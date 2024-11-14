#' Capitalize Acronyms and Title Case Text
#'
#' This function capitalizes acronyms (uppercase sequences) and formats the remaining
#' text in title case. It can be useful for text formatting where acronyms need to
#' remain in uppercase while other parts of the text are capitalized.
#'
#' @param text A character string where the initial uppercase acronym will remain
#'             capitalized, and the rest of the text will be converted to title case.
#' @return A character string with the acronym in uppercase and the remaining text
#'         in title case.
#' @examples
#' capitalize_acronyms('ESIA ZACATENCO')
#' capitalize_acronyms('CICS MILPA ALTA')
#' @export
capitalize_acronyms <- function(text) {
  # Ensure 'text' is a single character string
  if (length(text) != 1) {
    stop("Input must be a single character string.")
  }

  # Separate the acronym (uppercase sequence) from the rest of the text
  acronym <- gsub("\\s.*", "", text, perl = TRUE)
  remaining_text <- gsub("^[^\\s]*\\s", "", text, perl = TRUE)

  # Check if the text contains only the acronym or has additional text
  if (remaining_text == "") {
    return(acronym)  # Only acronym is present
  } else {
    # Capitalize the remaining text in title case
    remaining_text <- stringr::str_to_title(tolower(remaining_text))

    # Combine the uppercase acronym with the title-cased remaining text
    result <- paste(acronym, remaining_text, sep = " ")

    return(result)
  }
}
