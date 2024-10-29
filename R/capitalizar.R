capitalizar <- function(columna) {
  columna <- str_to_title(tolower(columna))
  palabras <- c(" En ", " De ",
                " La ", " El ",
                " Y ", " E ",
                " Con ", " Por ",
                " Los ", " Para ",
                " Sus ", " Del ",
                " Más ", " Mas ")
  for (palabra in palabras) {
    columna <- gsub(palabra, tolower(palabra), columna)
  }
  columna <- gsub("  "," ", columna)
  return(columna)
}
