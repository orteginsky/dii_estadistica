#' Check for Graduate Program Keywords
#'
#' This function checks if the values in a specified column contain keywords
#' indicating graduate programs, such as "Maestría," "Doctorado," or "Especialidad."
#'
#' @param column A character vector representing the column to check for keywords.
#' @return A logical vector where each element is \code{TRUE} if the corresponding
#'         value in \code{column} starts with one of the specified keywords,
#'         indicating a graduate program, and \code{FALSE} otherwise.
#' @examples
#' column <- c("Maestría en Ciencias", "Doctorado en Filosofía", "Licenciatura en Derecho")
#' check_graduate_program(column)
#' @export
posgrado <- function(column) {
  # Identify rows that start with specified graduate program keywords
  is_maestria <- startsWith(column, "Maestría")
  is_doctorado <- startsWith(column, "Doctorado")
  is_especialidad <- startsWith(column, "Especialidad")

  # Return TRUE if any keyword is found in the row
  graduate_program <- (is_maestria | is_doctorado | is_especialidad)
  return(graduate_program)
}
