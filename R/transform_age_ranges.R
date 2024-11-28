#' Transform Age Ranges to Descriptive Text
#'
#' This function converts a column with age ranges such as "<=18", ">=40", etc.,
#' into descriptive text like "18 or less", "40 or more", and so on.
#'
#' @param column A character vector containing the age ranges (e.g., "<=18", ">=40").
#' @return A character vector with the transformed descriptive text.
#' @examples
#' ages <- c("<=18", ">=40", ">=35", "<=24", "<=14", ">=21")
#' transform_age_ranges(ages)
#' @export
transform_age_ranges <- function(column) {
  # Ensure the input is a character vector
  column <- as.character(column)

  # Use gsub to replace the patterns
  transformed_column <- gsub("^<=(\\d+)$", "\\1 o menos", column) %>%  # Replace "<=N" with "N or less"
    gsub("^>=(\\d+)$", "\\1 o más", .)                             # Replace ">=N" with "N or more"

  return(transformed_column)
}
