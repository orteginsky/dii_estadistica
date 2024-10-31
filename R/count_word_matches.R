#' Count Word Matches Between Two Columns
#'
#' This function counts the number of matching words between two specified columns
#' in each row of a data frame.
#'
#' @param df A data frame containing the columns to be analyzed.
#' @param col1 A character string specifying the name of the first column.
#' @param col2 A character string specifying the name of the second column.
#' @return A numeric vector where each element represents the count of matching words
#'         for each row between \code{col1} and \code{col2}.
#' @examples
#' df <- data.frame(Text1 = c("apple orange", "banana pear"),
#'                  Text2 = c("apple banana", "pear mango"))
#' count_word_matches(df, "Text1", "Text2")
#' @export
count_word_matches <- function(df, col1, col2) {
  # Ensure specified columns exist in the dataframe
  if (!all(c(col1, col2) %in% colnames(df))) {
    stop("The specified columns do not exist in the dataframe.")
  }

  # Apply function row-wise to count matching words
  matches <- apply(df, 1, function(row) {
    # Split words in each column
    words_col1 <- strsplit(as.character(row[col1]), "\\s+")[[1]]
    words_col2 <- strsplit(as.character(row[col2]), "\\s+")[[1]]

    # Count how many words from col1 are in col2
    word_matches <- sum(words_col1 %in% words_col2)
    return(word_matches)
  })

  return(matches)
}
