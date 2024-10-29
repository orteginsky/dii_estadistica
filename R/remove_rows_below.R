#' Remove Rows Below a Specific Word
#'
#' This function removes all rows below the last occurrence of a specified word in
#' a given column of a data frame.
#'
#' @param dataframe A data frame from which rows will be removed.
#' @param word A character string specifying the word to search for.
#' @param column_index An integer representing the index of the column to search in.
#' @return A data frame with all rows below the specified word removed.
#' @examples
#' df <- data.frame(A = c("start", "data", "info", "end"), B = 1:4)
#' cleaned_df <- remove_rows_below(df, "data", 1)
#' @export
remove_rows_below <- function(dataframe, word, column_index) {
  row <- max(grep(word, dataframe[[column_index]]))
  last_row <- nrow(dataframe)
  dataframe <- dataframe %>% slice(-((row + 1):last_row))
  return(dataframe)
}
