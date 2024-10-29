#' Remove Columns with Too Few Non-Null Values
#'
#' This function removes columns from a data frame that have fewer than a specified
#' number of non-null (non-NA) values.
#'
#' @param df A data frame from which columns will be removed.
#' @param max_non_nulls An integer specifying the minimum number of non-null values
#'                      required for a column to be retained.
#' @return A data frame containing only the columns that have at least the specified
#'         number of non-null values.
#' @examples
#' df <- data.frame(A = c(1, NA, 3), B = c(NA, NA, NA), C = c(4, 5, 6))
#' filtered_df <- remove_columns_with_too_few_non_nulls(df, max_non_nulls = 2)
#' @export
remove_columns_with_nulls <- function(df, max_non_nulls=0) {
  # Calculate the number of non-null (non-NA) values per column
  non_nulls_per_column <- colSums(!is.na(df))

  # Filter columns that have at least max_non_nulls non-null values
  columns_to_keep <-
    names(non_nulls_per_column)[non_nulls_per_column >= max_non_nulls]

  # Return the data frame with the columns that meet the condition
  filtered_df <- df[, columns_to_keep, drop = FALSE]

  return(filtered_df)
}
