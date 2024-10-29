#' Find Row Containing a Specific Word
#'
#' This function searches for a specified word within a range of columns in a data
#' frame and returns the index of the first row where the word is found.
#'
#' @param dataframe A data frame in which to search for the word.
#' @param word A character string specifying the word to search for. Defaults to "MIXTA".
#' @param start_col An integer indicating the starting column index for the search.
#' @param end_col An integer indicating the ending column index for the search.
#' @return An integer representing the row index where the word is found, or 0 if not found.
#' @examples
#' df <- data.frame(A = c("a", "b", "MIXTA"), B = c("x", "MIXTA", "z"))
#' row_index <- find_row(df, "MIXTA", 1, 2)
#' @export
find_row <- function(dataframe, word = "MIXTA", start_col = 4, end_col = 10) {
  if (is.na(start_col) || is.na(end_col)) {
    stop("Column indices do not exist in the dataframe.")
  }

  for (i in 1:nrow(dataframe)) {
    row <- dataframe[i, start_col:end_col]
    if (any(sapply(row, function(x) grepl(word, x)))) {
      return(i)  # Returns the row index
    }
  }

  return(0)  # Word not found
}
