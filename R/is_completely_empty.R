#' Check if a List or Subset is Completely Empty
#'
#' This function checks whether a given list or subset of a dataframe is completely empty.
#' It considers elements as empty if they are NULL or NA.
#'
#' @param lst A list or dataframe subset to check.
#' @return A logical value: TRUE if the list or subset is completely empty, FALSE otherwise.
#' @examples
#' empty_list <- list()
#' partially_empty_list <- list(NULL, 1)
#' completely_empty_list <- list(NULL, NULL)
#' dataframe_subset <- data.frame(X1 = NA, X2 = NA, X3 = NA)
#'
#' is_completely_empty(empty_list)           # TRUE
#' is_completely_empty(partially_empty_list) # FALSE
#' is_completely_empty(completely_empty_list) # TRUE
#' is_completely_empty(dataframe_subset)     # TRUE
#' @export
is_completely_empty <- function(lst) {
  if (!is.list(lst) && !is.data.frame(lst)) {
    stop("The input must be a list or dataframe.")
  }

  # Convert to a vector and check for NULL or NA
  all_empty <- all(is.na(lst) | sapply(lst, is.null))

  return(all_empty)
}
