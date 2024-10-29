#' Convert Row to Column Headers
#'
#' This function assigns the values of a specified row as column headers in a data frame,
#' removing the original row afterward. It can be useful for converting data frames
#' where column headers are present as a row.
#'
#' @param data A data frame where the row specified will be used as the new column headers.
#' @param row An integer indicating the row number to be used as column headers.
#'            Defaults to the first row (1).
#' @return A data frame with the specified row values as column headers, with the row
#'         removed from the data frame. Column names are formatted to be unique, lowercase,
#'         and with spaces replaced by underscores.
#' @examples
#' df <- data.frame(
#'   X1 = c("Name", "Alice", "Bob"),
#'   X2 = c("Age", 25, 30)
#' )
#' row_to_header(df, row = 1)
#' @export
row_to_header <- function(data, row = 1) {
  # Extract the values from the specified row as column names
  header_names <- unlist(data[row, ])

  # Ensure unique, standardized column names
  header_names <- gsub(" ", "_", header_names)
  header_names <- make.names(header_names, unique = TRUE)
  header_names <- tolower(header_names)

  # Set the new column names and remove the row used as headers
  colnames(data) <- header_names
  data <- data[-row, ]

  return(data)
}
