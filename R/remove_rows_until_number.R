#' Remove Rows Until a Number is Found
#'
#' This function removes rows from a dataframe based on a specified character column.
#' Rows are removed until the first occurrence of a number is found in the column.
#'
#' @param df A dataframe to clean.
#' @param column A character string indicating the column name to evaluate.
#' @return A cleaned dataframe containing rows starting from the first occurrence of a number in the specified column.
#' @examples
#' data <- data.frame(
#'   text_column = c("No numbers", "Still text", "1234", "More text", "5678"),
#'   values = 1:5
#' )
#' clean_data <- remove_rows_until_number(data, 1)
#' @export
remove_rows_until_number <- function(df, column) {
  # Check if the column exists in the dataframe
  if (!column %in% colnames(df)) {
    stop("The specified column does not exist in the dataframe.")
  }

  # Ensure the column is treated as a character vector
  column_data <- as.character(df[[column]])

  # Find the index of the first row containing a number
  first_number_index <- which(grepl("\\d", column_data))[1]

  # If a number is found, keep rows from that index onward
  if (!is.na(first_number_index)) {
    df <- df[first_number_index:nrow(df), ]
  }

  # Return the cleaned dataframe
  return(df)
}
