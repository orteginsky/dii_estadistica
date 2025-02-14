% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/find_column.R
\name{find_column}
\alias{find_column}
\title{Find Column Containing a Specific Word in a Specific Row}
\usage{
find_column(dataframe, row, word = "MIXTA")
}
\arguments{
\item{dataframe}{A data frame in which to search for the word.}

\item{row}{An integer specifying the row index to search in.}

\item{word}{A character string specifying the word to search for. Defaults to "MIXTA".}
}
\value{
An integer representing the column index where the word is found, or 0 if not found.
}
\description{
This function searches for a specified word in a specific row of a data frame
and returns the index of the first column where the word is found.
}
\examples{
df <- data.frame(A = c("MIXTA", "b", "c"), B = c("x", "y", "MIXTA"))
col_index <- find_column(df, 1, "MIXTA")
}
