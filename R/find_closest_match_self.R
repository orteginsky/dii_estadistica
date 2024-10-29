#' Find Closest Match
#'
#' This function finds the closest match for each element in a given character vector
#' based on the Levenshtein distance. It excludes the current element when searching for
#' the closest match.
#'
#' @param search_column A character vector containing the elements to be searched for
#'                      matches.
#' @return A data frame with three columns:
#'         \describe{
#'           \item{nombre}{The original element from the search_column.}
#'           \item{coincidencia}{The closest matching element.}
#'           \item{distancia_minima}{The Levenshtein distance to the closest match.}
#'         }
#' @examples
#' find_closest_match(c("apple", "apply", "apples", "banana"))
#' @export
find_closest_match_self <- function(search_column) {
  # Create a list to store the results
  results <- vector("list", length(search_column))

  for (i in seq_along(search_column)) {
    # Exclude the current element
    to_exclude <- search_column[-i]

    # Calculate Levenshtein distances
    distances <- adist(search_column[i], to_exclude)

    if (length(to_exclude) == 0) {
      # If there are no elements to compare, return NA
      results[[i]] <- c(coincidencia = NA, distancia_minima = NA)
    } else {
      # Find the index of the element with the minimum distance
      index_min <- which.min(distances)
      results[[i]] <- c(nombre = search_column[i],
                        coincidencia = to_exclude[index_min],
                        distancia_minima = distances[index_min])
    }
  }

  # Convert the list of results to a data frame
  results_df <- do.call(rbind, results)
  colnames(results_df) <- c("nombre", "coincidencia", "distancia_minima")

  # Convert the distance column to numeric
  results_df <- as.data.frame(results_df, stringsAsFactors = FALSE)
  results_df$distancia_minima <- as.numeric(results_df$distancia_minima)

  return(results_df)
}
