#' Transform Ordinal Numbers with Sorted Prefix
#'
#' This function transforms ordinal numbers like "1°", "2°", etc., into a format
#' with alphabetical prefixes assigned based on the natural order of the ordinal numbers.
#'
#' Duplicate ordinal values will receive the same prefix.
#'
#' @param column A character vector containing ordinal numbers (e.g., "1°", "2°").
#' @return A character vector with the transformed format.
#' @examples
#' ordinals <- c("2°", "5°", "3°", "4°", "1°", "1°")
#' transform_ordinals_with_sorted_prefix(ordinals)
#' @export
transform_ordinals <- function(column) {
  # Ensure the input is a character vector
  column <- as.character(column)

  # Extract numeric parts from ordinal numbers
  numeric_values <- as.numeric(gsub("°", "", column))

  # Get unique values in sorted order and map to letters
  unique_sorted <- sort(unique(numeric_values))
  prefix_map <- setNames(letters[seq_along(unique_sorted)], unique_sorted)

  # Map original column to its prefix and combine with the original value
  transformed_column <- paste0(prefix_map[as.character(numeric_values)], "_", column)

  return(transformed_column)
}
