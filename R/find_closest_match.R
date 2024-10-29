#' Find Closest Match in Comparison Column
#'
#' This function finds the closest match for each element in a specified search column
#' by comparing it to another column using the Levenshtein distance.
#'
#' @param search_column A character vector containing the elements to be searched for
#'                      matches.
#' @param compare_column A character vector containing the elements to be compared
#'                       against.
#' @return A data frame with three columns:
#'         \describe{
#'           \item{columna_a_buscar}{The original element from the search_column.}
#'           \item{coincidencia}{The closest matching element from the compare_column.}
#'           \item{distancia_minima}{The Levenshtein distance to the closest match.}
#'         }
#' @examples
#' find_closest_match(c("apple", "banana"), c("apples", "banana", "berry"))
#' @export
find_closest_match <- function(search_column, compare_column) {
  results <- sapply(search_column, function(text_to_search) {
    distances <- adist(text_to_search, compare_column)

    # Find the index of the element with the minimum distance
    index <- which.min(distances)

    # Return a list with the matching text and the minimum distance
    if (length(index) == 0) {
      return(c(NA, NA))
    } else {
      return(c(compare_column[index], distances[index]))
    }
  })

  results_df <- as.data.frame(t(results), stringsAsFactors = FALSE)
  colnames(results_df) <- c("coincidencia", "distancia_minima")
  results_df$distancia_minima <- as.numeric(results_df$distancia_minima)

  # Combine the results with the original search column
  final_table <- cbind(search_column, results_df)
  row.names(final_table) <- 1:nrow(final_table)

  return(final_table)
}
