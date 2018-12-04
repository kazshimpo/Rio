\name{io_aggregate}
\alias{io_aggregate}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Sector aggregation of Input-Output tables
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Using conveters between the original IO sectors and a new aggregated IO sectors prepaired on sheets in an Excel book, this function performs sector aggregation of input-output tables. The format of the original input-output data must be a long format, in which each record consists of row and column codes and at least one corresponding input-output data. The program returns a list of input-output matrices which can be used for input-output analysis.
}
\usage{
ioaggregate(io_data, book, sheet_conv, sheet_code, table, by_parts = TRUE, balance = FALSE, tol = 1e-5)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{io_data}{
  A data frame of an input-output data with long data format. Each record of \code{io_data} consists of \code{row_code}(string) for row sector code, \code{col_code}(string) for column sector code and at least one coresopnding input-output data. Japanese input-output data published by Japanese government, for example, have input-output data such as producer's price, commercial margins, transportation costs and purchasers price in each record.
  }
  \item{book}{A string of Excel book name. The book has to contains four sheets on separate sheets: row & column converters and new row & column sector clasiffication.
  }
  \item{sheet_conv}{A list of sheet's name of row & column converters in the Excel book: \code{$row} for a row converter and \code{$col} for a column converter. Converters bridge the sectors from the original IO data to new aggregated IO tables. Each record of row & colum conveter has original IO code(\code{from}), its description(\code{name}) and its destination(\code{to}).
  }
  \item{sheet_code}{A list of sheet's name of row & column's new sector classification in the Excel book: \code{$row} for a row sector classfication and \code{$col} for column. Each row of sector classification sheet consists of \code{code} and its description as \code{name} and also the type of sectors as \code{sector}. The type of sector is: 'X' = production sectors, 'F' = domestic final demand sectors, 'E' = exports, 'M' = imports, 'V' = value added sectors, and 'CT' = contro totals. The \code{code} corresopond to the destination(\code{to}) in row & column converters.
  }
  \item{table}{A string of the IO table’s name aggregated. It is a column name in \code{io_data}.
  }
  \item{by_parts}{A logical. If \code{TRUE}, returns the aggregated IO table by parts: 1. Transaction matrix of intermediate inputs, 2. domestic output vectors of row and column, 3. Domestic final demand vector/matrix, 4. Exports vector/matrix, 5. Import vector/matrix, 6. Value added vector/matrix. If \code{FALSE}, returns a whole input-output matrix. This decomposition of a whole input-output matrix is done according to the sector type given in the new sector classificition table in the Excel book.
  }
  \item{balance}{A logical object: If is is \code{TRUE}, test the balance of the IO tables. Only when the number of tables specified in \code{tables} is one, balance test is performed if \code{balance} is \code{TRUE}.
  }
  \item{tol}{A double variable. Tolerance for balance test. If difference between row and column control totals is less than \code{tol},the sector is regarded as balanced.
  }
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A list which contains
\item{row_sector}{A data frame of row sector classification table,}
\item{col_sector}{A data frame of column sector classification table,}
and if \code{by_parts = TRUE}
\item{X}{Transaction matrix of intermediate inputs,}
\item{xr}{Domestic output vector(contro taotal of individual column sector),}
\item{xc}{Domestic output vector(contro taotal of individual row sector),}
\item{fd}{Domestic final demand vector/matrix,}
\item{ex}{Exports vector/matrix,}
\item{im}{Imports vector/matrix,}
\item{vad}{Value added vector/matrix,}
or if \code{by_parts = FALSE}
\code{table}{A whole input-output matrix.}
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
%%##---- Should be DIRECTLY executable !! ----
%%##-- ==>  Define data, use random,
%%##--	or do  help(data=index)  for the standard data sets.

%%## The function is currently defined as
%%function (x)
%%{
%%  }
%%}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
}
