% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/remove_without_affecting.R
\name{remove_without_affecting}
\alias{remove_without_affecting}
\title{Remove Without Affecting Others}
\usage{
remove_without_affecting(datos, column_x, column_y = "Datos")
}
\arguments{
\item{datos}{A data frame containing the data to be processed.}

\item{column_x}{A character string representing the name of the column to exclude
from grouping.}

\item{column_y}{A character string representing the name of the column to be
summarized. Defaults to "Datos".}
}
\value{
A data frame with the grouped data and the summed values of the specified
        column.
}
\description{
This function groups data by all columns except the specified column and
summarizes the specified column by calculating the sum.
}
\examples{
df <- data.frame(A = c(1, 1, 2, 2), B = c(5, 5, 6, 6), Datos = c(10, 20, 30, 40))
remove_without_affecting(df, "A", "Datos")
}
