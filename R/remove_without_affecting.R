#' Remove Without Affecting Others
#'
#' This function groups data by all columns except the specified column and
#' summarizes the specified column by calculating the sum.
#'
#' @param datos A data frame containing the data to be processed.
#' @param column_x A character string representing the name of the column to exclude
#'                 from grouping.
#' @param column_y A character string representing the name of the column to be
#'                 summarized. Defaults to "Datos".
#' @return A data frame with the grouped data and the summed values of the specified
#'         column.
#' @examples
#' df <- data.frame(A = c(1, 1, 2, 2), B = c(5, 5, 6, 6), Datos = c(10, 20, 30, 40))
#' remove_without_affecting(df, "A", "Datos")
#' @export
remove_without_affecting <- function(datos, column_x, column_y = "Datos") {
  datos <- datos %>%
    group_by(across(-all_of(c(column_x,column_y)))) %>%
    summarise({{column_y}} := sum(!!sym(column_y)), .groups = 'drop')

  return(datos)
}

