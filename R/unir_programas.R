#' Combine Degree and Programs
#'
#' This function combines a degree string with corresponding program strings.
#' If the degree is empty, it returns the program as is. If a vector of degrees
#' and a vector of programs are provided, it applies the combination element-wise.
#'
#' @param posgrado A character string representing the degree. Can also be a vector.
#' @param programas A character string or vector of program names to combine with the degree.
#' @return A character vector with the combined strings, or FALSE if the lengths do not match.
#' @examples
#' unir_programas("Master", "Computer Science")
#' unir_programas_vector(c("Master", ""), c("Computer Science", "Mathematics"))
#' @export
unir_programas <- function(posgrado, programas) {
  # Check if inputs are vectors
  if (is.vector(posgrado) && is.vector(programas)) {
    if (length(posgrado) != length(programas)) {
      return(FALSE)  # Return FALSE if lengths do not match
    }
    return(mapply(function(p, pr) {
      if (p == "") {
        return(pr)  # Return program as is if degree is empty
      } else {
        return(paste(p, pr, sep = " "))
      }
    }, posgrado, programas))
  } else {
    # Handle single values
    if (posgrado == "") {
      return(programas)  # Return program as is if degree is empty
    } else {
      return(paste(posgrado, programas, sep = " "))
    }
  }
}
