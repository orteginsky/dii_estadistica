% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/remove_rows_until_number.R
\name{remove_rows_until_number}
\alias{remove_rows_until_number}
\title{Remove Rows Until a Number is Found Exclusively}
\usage{
remove_rows_until_number(df, column)
}
\arguments{
\item{df}{A dataframe to clean.}

\item{column}{A character string indicating the column name to evaluate.}
}
\value{
A cleaned dataframe containing rows starting from the first occurrence of a number in the specified column.
}
\description{
This function removes rows from a dataframe based on a specified character column.
Rows are removed until the first occurrence of a row where the column contains only a number.
}
\examples{
data <- data.frame(
  text_column = c("No numbers", "Still text", "1234", "More text", "5678"),
  values = 1:5
)
clean_data <- remove_rows_until_number(data, "text_column")
}
