#' Remove Columns by Keyword
#'
#' This function removes columns from a data frame based on the presence of a specific keyword in the column names.
#'
#' @param data A data frame from which columns will be removed.
#' @param keyword A character string representing the keyword to search for in column names.
#' @return A data frame with the specified columns removed. If no columns match the keyword, the original data frame is returned.
#' @examples
#' df <- data.frame(A_key = 1:3, B = 4:6, C_key = 7:9)
#' remove_columns(df, "key")
#' @export
remove_columns <- function(data, keyword) {
  # Get column names
  column_names <- names(data)

  # Find columns containing the keyword in their names
  columns_to_remove <- grep(keyword, column_names, ignore.case = TRUE)

  # Check if there are columns to remove
  if (length(columns_to_remove) == 0) {
    # Return the original data frame if no matches are found
    return(data)
  } else {
    # Remove the identified columns and return the modified data frame
    data <- data[, -columns_to_remove, drop = FALSE]
    return(data)
  }
}
