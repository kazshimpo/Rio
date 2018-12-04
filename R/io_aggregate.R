io_aggregate <- function(io_data, book, sheet_conv, sheet_code, table, by_parts = TRUE, balance = FALSE, tol = 1e-5) {
  # Arguments:
  #
  #   io_data: data frame: An input-output data with long data format
  #     Each row of io_data consists of row_code(string) for row code,
  #     col_code(string) for column code and coresopnding input-output data.
  #     Japanese input-output data, for example, consists of producer's price, commercial margins, transportation costs and purchasers price.
  #   book: string: Name of Excel book which contains converters and new sector clasiffication.
  #     The Excel book contains row & column conveters and new sector classification of row & column on separate sheets.
  #   sheet_conv: list: A list of sheet's name of row & column converters, 
  #    $row for row converters and $col for column converter.
  #    Each record of row & colum conveter has original code(from), name(name) and destination(to).
  #   sheet_code: list: A list of sheet's name of row & column new sector classification, 
  #    $row for row converters and $col for column converter. 
  #    Each row of row & column sector classification consists of code and its name and also the type of sectors(sector). 
  #    For the type sectors, X means production sectors, F means final demand sectors, V means value added sectors, and CT shows contro totals.
  #    The codes coresopond to the destination codes(to) in row & column converters.
  #   table: a strings: name of IO data aggregated
  #   balance: logical: If true, test the balance of the IO tables.
  #     When the number of tables specified in tables is one, balance test is performed if balance is TRUE.
  #   tol: double: Tolerance. If difference between row and column control totals is less than tol,
  #    the sector is regarded as balanced.
  #
  # Values:
  #
  #    A list of sector classification tables(row_sector for row sectors and col_sectors for column sectors),
  #    and matrices of input-output tables with names specified in tables.
  
  require(tidyverse)
  require(readxl)
  
  # Read row & column converters in the Excel book specified as 'book'.
  # The sheet's names of conveters are  
  conv <- list()
  conv$row <- readxl::read_excel(book, sheet = sheet_conv$row) %>% dplyr::select(from, to_row = to)
  conv$col <- readxl::read_excel(book, sheet = sheet_conv$col) %>% dplyr::select(from, to_col = to)
  code <- list()
  code$row <- readxl::read_excel(book, sheet = sheet_code$row) %>% dplyr::mutate(loc = 1 : dplyr::n())
  code$col <- readxl::read_excel(book, sheet = sheet_code$col) %>% dplyr::mutate(loc = 1 : dplyr::n())
  
  # Find the column location of the specified tables
  table_loc <- match(table, colnames(io_data))
  if (any(is.na(table_loc))) {
    j <- which(is.na(table_loc))
    stop(paste('Following table names are not found:', table_loc[j]))
  }
  
  n_x_sector <- function(code) {
    n <- code %>%
      dplyr::select(sector) %>%
      dplyr::filter(sector == 'X') %>%
      dplyr::summarise(n = n()) %>%
      pull()
  }
  
  find_location <- function(code, sec) dplyr::filter(code, sector == sec) %>% dplyr::select(loc) %>% pull()
  
  long2matrix <- function(io_data, row_loc, col_loc) {
    m <- matrix(0, nrow = nrow(code$row), ncol = nrow(code$col))
    m[cbind(row_loc, col_loc)] <- dplyr::select(io_data, 1) %>% pull()
    dimnames(m) <- list(code$row$code, code$col$code)
    m
  }
  
  balance_test <- function(m) {
    # Find the number of X-sectors
    ixr <- find_location(code$row, 'X')
    ixc <- find_location(code$col, 'X')
    if (length(ixr) != length(ixc)) stop('Number of X-sectors must be same in row & column')
    # Calculate control totals of rows and columns
    i <- find_location(code$row, 'CT')
    j <- find_location(code$col, 'CT')
    m[i, ] <- 0
    m[, j] <- 0
    ctr <- colSums(m)
    ctc <- rowSums(m)
    # Create a data frame for testing the IO balance
    # Check ctr[i] = ctc[i], for all X-sector i
    ct <- data.frame(code = code$row$code[ixr], row = ctr[ixr], col = ctc[ixc]) %>%
      dplyr::mutate(dif = row - col) %>%
      dplyr::filter(abs(dif) > tol)
    # If the IO tables is balanced, the resulted ct data frame does not have any rows
    # Thus if it is inbalance, ct has some rows.
    if (nrow(ct) > 0) ok <- F
    else {
      ok <- T
      m[i, ] <- ctr
      m[, j] <- ctc
    }
    # Announce
    cat('----- Balance Test -----\n')
    if (ok) cat('IO table is balanced\n')
    else cat('IO table is inbalance. Check ct!\n')
    # Return
    return(list(ok = ok, iomat = m, ct = ct))
  }  

  ##----------------------------------------------------------------------
  ## Do sector aggregation with long data format
  ##----------------------------------------------------------------------
  aggregate <- io_data %>%
    # Merge the converters to the IO table long data
    dplyr::left_join(conv$col, by = c('col_code' = 'from')) %>%
    dplyr::left_join(conv$row, by = c('row_code' = 'from')) %>%
    # Omit the records in which the new destination is empty
    dplyr::filter(!is.na(to_col)) %>%
    dplyr::filter(!is.na(to_row)) %>%
    # The original row & column codes are not necessary from here onward
    dplyr::select(-col_code, -row_code) %>%
    # Rename the new destination as row_code & col_code
    dplyr::rename(col_code = to_col, row_code = to_row) %>%
    # Do aggregation!
    dplyr::group_by(col_code, row_code) %>%
    dplyr::summarise_all(sum) %>%
    dplyr::ungroup() %>%
    # Merge the new sector classification
    # Attaching the locations of row & col sectors is important to make matrix-format IO tables.
    dplyr::left_join(dplyr::select(code$col, col_code = code, col_loc = loc), by = 'col_code') %>%
    dplyr::left_join(dplyr::select(code$row, row_code = code, row_loc = loc), by = 'row_code')

  ##----------------------------------------------------------------------
  ## Make matrix-form IO table
  ##----------------------------------------------------------------------
  
  # Find the location of the row & col location columns  
  row_loc <- aggregate$row_loc
  col_loc <- aggregate$col_loc
  # Make a matrix form IO table
  iomat <- long2matrix(aggregate[, table_loc[[1]]], row_loc, col_loc)

  # Make a list for return  
  iot <- list(row_sector = code$row, col_sector = code$col)
  
  # Balance Test if necessary
  if (balance) {
    res_balance <- balance_test(iomat)
    if (!res_balance$ok) { # The IO table is inbalance
      iot$ct <- res_balance$ct
      iot$table <- res_balance$iomat
      return(iot)
    }
  }
  
  if (by_parts) {
    # Transaction matrix of intermediate inputs
    rows <- find_location(code$row, 'X')
    cols <- find_location(code$col, 'X')
    iot$X <- iomat[rows, cols]
    # Domestic output
    iot$xr <- iomat[nrow(iomat), cols]
    iot$xc <- iomat[rows, ncol(iomat)]
    # Domestic final demand
    cols <- find_location(code$col, 'F')
    iot$fd <- iomat[rows, cols]
    # Exports
    cols <- find_location(code$col, 'E')
    iot$ex <- iomat[rows, cols]
    # Imports
    cols <- find_location(code$col, 'M')
    iot$im <- iomat[rows, cols]
    # Value added
    rows <- find_location(code$row, 'V')
    cols <- find_location(code$col, 'X')
    iot$vad <- iomat[rows, cols]
  } else {
    iot$table <- iomat
  }
  return(iot)
}

iomat2long <- function(iomat, tbl_name) {
  require(tidyverse)
  require(magrittr)
  long <- data.frame(rownames(iomat), iomat)
  colnames(long) <- c('row_code', colnames(iomat))
  long %<>% tidyr::gather(key = col_code, value = val, -row_code)
  colnames(long) <- c('row_code', 'col_code', tbl_name)
  return(long)
}
