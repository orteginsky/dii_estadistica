#' Change IDs in Data Frame
#'
#' This function replaces the `ID_programa` column in a data frame by performing
#' a left join with another data frame containing ID mappings. It fills in missing
#' values in `ID_programa` using specified columns from the second data frame.
#'
#' @param bad_data A data frame that contains the original data with the `ID_programa` column.
#' @param data_ids A data frame containing the new ID mappings.
#' @return A data frame with the updated `ID_programa` values.
#' @examples
#' bad_data <- data.frame(ID = c(1, 2, 3), ID_programa = c(NA, "A", NA))
#' data_ids <- data.frame(ID = c(1, 2), ID_programa_1 = c("X", "Y"), ID_programa_2 = c(NA, "Z"))
#' updated_data <- change_ID(bad_data, data_ids)
#' @export
change_ID <- function(bad_data, data_ids) {

  # Find common columns between both data frames
  common_columns <- intersect(names(bad_data), names(data_ids))

  # Reverse the order of columns in the second data frame (data_ids)
  columns_to_change <- rev(names(data_ids))

  # Perform a left join between bad_data and data_ids using common columns
  result <- bad_data %>%
    left_join(data_ids, by = common_columns) %>%

    # Replace NA in the ID_programa column with non-NA values from specified columns
    mutate(ID_programa = do.call(coalesce, across(all_of(columns_to_change)))) %>%

    # Remove the first column from data_ids (which is now redundant)
    select(-all_of(columns_to_change[1]))

  return(result)
}
