#' Remove Rows Above a Specific Word
#'
#' This function removes all rows above the first occurrence of a specified word in
#' a given column of a data frame.
#'
#' @param dataframe A data frame from which rows will be removed.
#' @param word A character string specifying the word to search for.
#' @param column_index An integer representing the index of the column to search in.
#' @return A data frame with all rows above the specified word removed.
#' @examples
#' df <- data.frame(A = c("start", "data", "info", "end"), B = 1:4)
#' cleaned_df <- remove_rows_above(df, "info", 1)
#' @export
remove_rows_above <- function(dataframe, word, column_index) {
  row <- min(grep(word, dataframe[[column_index]]))
  dataframe <- dataframe %>% slice(-(1:(row - 1)))
  return(dataframe)
}
