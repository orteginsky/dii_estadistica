#' Find Column Containing a Specific Word in a Specific Row
#'
#' This function searches for a specified word in a specific row of a data frame
#' and returns the index of the first column where the word is found.
#'
#' @param dataframe A data frame in which to search for the word.
#' @param row An integer specifying the row index to search in.
#' @param word A character string specifying the word to search for. Defaults to "MIXTA".
#' @return An integer representing the column index where the word is found, or 0 if not found.
#' @examples
#' df <- data.frame(A = c("MIXTA", "b", "c"), B = c("x", "y", "MIXTA"))
#' col_index <- find_column(df, 1, "MIXTA")
#' @export
find_column <- function(dataframe, row, word = "MIXTA") {
  if (row > nrow(dataframe) || row < 1) {
    stop("The specified row number is out of the range of the dataframe.")
  }

  for (col in 1:ncol(dataframe)) {
    if (grepl(word, dataframe[row, col])) {
      return(col)  # Returns the column index
    }
  }

  return(0)  # Word not found
}
