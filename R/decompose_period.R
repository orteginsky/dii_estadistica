#' Decompose Period Column
#'
#' This function separates a period column into year and semester components.
#' The period is expected to be formatted as "YYYY-Semester", where "Semester"
#' may include a leading slash ("/"). The function extracts the year and semester
#' as numeric values.
#'
#' @param datos A data frame containing the period column to be decomposed.
#' @param column_index An integer indicating the index of the column to separate.
#' @return A data frame with the specified column decomposed into year and semester.
#' @examples
#' df <- data.frame(Periodo = c("2021-1", "2021-2", "2022-1"))
#' df_decomp <- decompose_period(df, 1)
#' @export
decompose_period <- function(datos, column_index) {
  datos <- datos %>%
    separate_wider_delim(
      cols = colnames(datos)[column_index],
      delim = "-",
      names = c("año_periodo", "Semestre_periodo")
    )

  # Extract and clean the semester information
  datos$Semestre_periodo <- str_extract(datos$Semestre_periodo, "/[12]$") %>%
    str_replace_all("/", "")

  # Convert to numeric
  datos$año_periodo <- as.numeric(datos$año_periodo)
  datos$Semestre_periodo <- as.numeric(datos$Semestre_periodo)

  return(datos)
}
