#' Process a dataframe by renaming, cleaning, and restructuring columns.
#'
#' @param data A dataframe to process.
#' @param created_columns A character vector with names of new columns to create.
#' @param n_column A numeric value indicating a specific column index for filtering.
#'
#' @return A processed dataframe.
#' @export

totals <- function(data, created_columns, n_column) {
  if (!is.data.frame(data)) stop("'data' must be a dataframe.")
  if (!is.vector(created_columns) || !is.character(created_columns)) {
    stop("'created_columns' must be a character vector.")
  }
  if (!is.numeric(n_column) || n_column <= 0 || n_column > ncol(data)) {
    stop("'n_column' must be a valid number within the column range.")
  }

  # Remove the first column if it contains only "No"
  if (!is.na(which(grepl("^No$", data[[1]]))[1])) {
    data <- data %>% select(-1)
  }

  # Rename the first two columns
  colnames(data)[1:2] <- c("Unidad_Academica", "Programa")

  # Remove unnecessary rows (based on NA values)
  data <- data %>%
    filter(!is.na(.data[[colnames(data)[n_column]]]))

  # Remove empty or specific value rows in first columns
  if (diiestadistica::is_completely_empty(data[1, (n_column + 1):ncol(data)])) {
    data <- data %>% slice(-1)
  }

  # Prepare column names using first rows
  transposed_length <- length(created_columns)
  transposed_data <- t(data[1:transposed_length, ]) %>%
    as_tibble() %>%
    fill(everything()) %>%
    mutate_all(diiestadistica::capitalizar)

  transposed_data <- transposed_data %>%
    unite("name", everything(), sep = "_", remove = FALSE) %>%
    slice(-1:-2)

  colnames(data)[3:ncol(data)] <- transposed_data$name

  # Fill missing values in main columns
  data <- data %>%
    fill(Unidad_Academica, Programa)

  # Remove irrelevant columns
  data <- data %>%
    diiestadistica::remove_columns("Total") %>%
    diiestadistica::remove_columns("Subtotal") %>%
    diiestadistica::remove_columns("Subt") %>%
    diiestadistica::remove_columns("\\+")

  # Filter out irrelevant rows
  data <- data %>%
    diiestadistica::remove_rows_until_number(colnames(data)[n_column])

  # Remove rows containing "TOTAL" or "Subtotal"
  filter_condition <- str_detect(data$Unidad_Academica,
                                 regex("TOTAL|Total|Subtotal|SUBTOTAL",
                                       ignore_case = FALSE)) |
    str_detect(data$Programa, regex("TOTAL|Subtotal", ignore_case = TRUE))
  filter_condition[is.na(filter_condition)] <- FALSE
  data <- data[filter_condition, ]
  data <- data %>%
    select(-Programa)

  # Restructure with pivot_longer and separate columns
  data <- data %>%
    pivot_longer(
      cols = names(data)[2]:names(data)[ncol(data)],
      names_to = "dividir",
      values_to = "Datos"
    ) %>%
    separate_wider_delim(
      dividir,
      delim = "_",
      names = created_columns
    ) %>%
    mutate(Datos = as.integer(Datos))

  if ("Sexo" %in% colnames(data)) {
    data <- data %>%
      filter(!str_detect("Subt",Sexo)) %>%
      mutate(Sexo = gsub("^H$","Hombres",Sexo)) %>%
      mutate(Sexo = gsub("^M$","Mujeres",Sexo)) %>%
      mutate(Sexo = gsub("^Hom$","Hombres",Sexo)) %>%
      mutate(Sexo = gsub("^Muj$","Mujeres",Sexo))
  }

  if ("Turno" %in% colnames(data)) {
    data <- data %>%
      filter(!str_detect("Subt",Turno)) %>%
      mutate(Turno = gsub("^V$","Vespertino",Turno)) %>%
      mutate(Turno = gsub("^M$","Matutino",Turno)) %>%
      mutate(Turno = gsub("^Ves$","Vespertino",Turno)) %>%
      mutate(Turno = gsub("^Mix$","Mixto",Turno)) %>%
      mutate(Turno = gsub("^Mat$","Matutino",Turno))
  }

  data <- data %>%
    filter(!is.na(Datos))

  return(data)
}
