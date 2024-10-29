#' Rename Column Based on Reference Data Frame
#'
#' This function renames a single column in a data frame based on the column names
#' in a reference data frame. If exactly one column name differs between the two
#' data frames, the differing column in the data frame to modify will be renamed
#' to match the name in the reference data frame.
#'
#' @param df_to_modify A data frame with column names to potentially modify.
#' @param reference_df A reference data frame with the correct column names.
#' @return A data frame with the column name updated to match the reference data frame.
#'         If there is not exactly one differing column, the function returns
#'         the data frame unmodified and outputs a message.
#' @examples
#' df1 <- data.frame(Name = c("A", "B"), Age = c(25, 30))
#' df2 <- data.frame(Nombre = c("A", "B"), Age = c(25, 30))
#' rename_based_on_reference(df2, df1)
#' @export
rename_column <- function(df_to_modify, reference_df) {
  # Extract column names from both data frames
  names_to_modify <- names(df_to_modify)
  reference_names <- names(reference_df)

  # Identify the differing column name
  differing_column <- setdiff(names_to_modify, reference_names)
  correct_column <- setdiff(reference_names, names_to_modify)

  # Check if there is exactly one differing column
  if (length(differing_column) == 1 && length(correct_column) == 1) {
    # Rename the differing column with the reference name
    names(df_to_modify)[names(df_to_modify) == differing_column] <- correct_column
  } else {
    message("No unique column mismatch found")
  }

  return(df_to_modify)
}
