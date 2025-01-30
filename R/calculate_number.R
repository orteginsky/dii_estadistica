#' Compute a normalized row count based on unique values in selected columns
#'
#' This function calculates a number by dividing the total number of rows in a dataframe
#' by the product of the unique values in all columns except the specified default columns.
#'
#' @param df A dataframe containing the data.
#' @param default_columns A character vector specifying the columns to exclude from the calculation.
#'                        Default is `c("unidades", "datos")`.
#'
#' @return A numeric value representing the total rows divided by the product of unique values in the other columns.
#'         Returns `NA` if division by zero occurs.
#' @export
#'
#' @examples
#' df <- data.frame(unidades = rep(1:2, 4),
#'                  periodo = rep(1:2, each = 4),
#'                  sexo = rep(c("M", "F"), each = 2, times = 2),
#'                  turno = rep(c("M", "V"), times = 4),
#'                  datos = rnorm(8))
#' calculate_number(df)
#'
calculate_number <- function(df, default_columns) {
  if (!is.data.frame(df)) stop("'df' must be a dataframe.")
  if (!is.vector(default_columns) || !is.character(default_columns)) {
    stop("'created_columns' must be a character vector.")
  }
  total_rows <- nrow(df)

  # Exclude default columns from the calculation
  extra_columns <- setdiff(names(df), default_columns)

  # Compute the product of unique values in the remaining columns
  divisor <- prod(sapply(df[extra_columns], function(x) length(unique(x))))

  # Avoid division by zero
  if (divisor == 0) return(NA)

  # Compute the final value
  total_rows / divisor
}
