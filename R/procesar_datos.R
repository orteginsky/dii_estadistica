#' Procesar Datos Tabulares
#'
#' Esta función procesa un dataframe eliminando columnas y filas irrelevantes,
#' renombrando columnas y reorganizando los datos en formato largo.
#'
#' @param datos Un dataframe con los datos iniciales a procesar.
#' @param columnas_creadas Un vector de nombres que se utilizarán al dividir las columnas.
#' @param n_columna Índice de la columna clave utilizada para filtrar valores NA y reorganizar los datos.
#' @return Un dataframe procesado y reorganizado.
#' @examples
#' datos <- data.frame(
#'   Col1 = c("No", "A", "B", "C"),
#'   Col2 = c("Unidad1", NA, "Unidad2", "Unidad3"),
#'   Col3 = c("Programa1", "Programa2", "Programa3", "Programa4"),
#'   Col4 = c(NA, 10, 20, 30)
#' )
#' columnas_creadas <- c("Concepto", "Sexo")
#' n_columna <- 3
#' datos_procesados <- procesar_datos(datos, columnas_creadas, n_columna)
#' @export
procesar_datos <- function(datos, columnas_creadas, n_columna) {
  # Verificar que los parámetros sean válidos
  if (!is.data.frame(datos)) stop("El argumento 'datos' debe ser un dataframe.")
  if (!is.vector(columnas_creadas) || !is.character(columnas_creadas)) {
    stop("El argumento 'columnas_creadas' debe ser un vector de caracteres.")
  }
  if (!is.numeric(n_columna) || n_columna <= 0 || n_columna > ncol(datos)) {
    stop("El argumento 'n_columna' debe ser un número válido dentro del rango de columnas.")
  }

  # 1. Eliminar la columna inicial si contiene solo "No"
  if (!is.na(which(grepl("^No$", datos[[1]]))[1])) {
    datos <- datos %>% select(-1)
  }

  # 2. Renombrar las dos primeras columnas
  colnames(datos)[1:2] <- c("Unidad_Academica", "Programa")

  # 3. Eliminar filas innecesarias (basadas en valores NA)
  datos <- datos %>%
    filter(!is.na(.data[[colnames(datos)[n_columna]]]))

  # 4. Eliminar filas con celdas vacías o específicas en las primeras columnas
  if (diiestadistica::is_completely_empty(datos[1, (n_columna + 1):ncol(datos)])) {
    datos <- datos %>% slice(-1)
  }
  if (sum(!is.na(datos[1, (n_columna + 1):ncol(datos)])) == 1) {
    datos <- datos %>%
      slice(-1) %>%
      select(-((ncol(datos) - 3):ncol(datos)))
  }

  # 5. Rellenar valores faltantes en columnas principales
  datos <- datos %>%
    fill(Unidad_Academica, Programa)

  # 6. Eliminar filas con "TOTAL" o "Subtotal" en las columnas clave
  filtro <- !str_detect(datos$Unidad_Academica, regex("TOTAL|Total", ignore_case = TRUE))
  filtro[is.na(filtro)] <- TRUE
  datos <- datos[filtro, ]

  filtro <- !str_detect(datos$Programa, regex("TOTAL|Subtotal", ignore_case = TRUE))
  filtro[is.na(filtro)] <- TRUE
  datos <- datos[filtro, ]

  # 7. Preparar nombres de columnas a partir de las primeras filas
  n_transpuesta <- length(columnas_creadas)
  datos_transpuesta <- t(datos[1:n_transpuesta, ]) %>%
    as_tibble() %>%
    fill(everything()) %>%
    mutate_all(diiestadistica::capitalizar)

  # Generar nombres concatenados
  datos_transpuesta <- datos_transpuesta %>%
    unite("nombre", everything(), sep = "_", remove = FALSE) %>%
    slice(-1:-2)

  # Asignar nuevos nombres de columnas
  colnames(datos)[3:ncol(datos)] <- datos_transpuesta$nombre

  # 8. Eliminar columnas irrelevantes
  datos <- datos %>%
    diiestadistica::remove_columns("Total") %>%
    diiestadistica::remove_columns("Subtotal") %>%
    diiestadistica::remove_columns("Subt") %>%
    diiestadistica::remove_columns("\\+")

  # 9. Filtrar filas que no son relevantes
  datos <- datos %>%
    diiestadistica::remove_rows_until_number(colnames(datos)[n_columna])

  # 10. Reorganizar datos con pivot_longer y separar columnas
  datos <- datos %>%
    pivot_longer(
      cols = names(datos)[3]:names(datos)[ncol(datos)],
      names_to = "dividir",
      values_to = "Datos"
    ) %>%
    separate_wider_delim(
      dividir,
      delim = "_",
      names = columnas_creadas
    ) %>%
    mutate(Datos = as.integer(Datos))

  # 11. Limpiar nombres de programas académicos
  datos$Programa <- datos$Programa %>%
    str_replace_all("^Ing\\. ", "Ingeniería ") %>%
    str_replace_all("^Lic\\. ", "Licenciatura ") %>%
    trimws() %>%
    diiestadistica::capitalizar() %>%
    str_replace("^Maestria", "Maestría") %>%
    str_replace("^M en C en Ing ", "Maestría en Ciencias en Ingeniería ") %>%
    str_replace("^Dr en C en Ing ", "Doctorado en Ciencias en Ingeniería ") %>%
    str_replace(" Mec$", " Mecánica") %>%
    str_replace(
      "^M en C y Tec de Vac y Bio$",
      "Maestría en Ciencias y Tecnología de Vacunas y Bioterapéuticos"
    ) %>%
    str_replace(
      "^Dr en C y Tec de Vacunas y Bioterapéuticos$",
      "Doctorado en Ciencias y Tecnología de Vacunas y Bioterapéuticos"
    )

  if ("Sexo" %in% colnames(datos)) {
    datos <- datos %>%
      filter(!str_detect("Subt",Sexo)) %>%
      mutate(Sexo = gsub("^H$","Hombres",Sexo)) %>%
      mutate(Sexo = gsub("^M$","Mujeres",Sexo)) %>%
      mutate(Sexo = gsub("^Hom$","Hombres",Sexo)) %>%
      mutate(Sexo = gsub("^Muj$","Mujeres",Sexo))
  }

  if ("Turno" %in% colnames(datos)) {
    datos <- datos %>%
      filter(!str_detect("Subt",Turno)) %>%
      mutate(Turno = gsub("^V$","Vespertino",Turno)) %>%
      mutate(Turno = gsub("^M$","Matutino",Turno)) %>%
      mutate(Turno = gsub("^Ves$","Vespertino",Turno)) %>%
      mutate(Turno = gsub("^Mix$","Mixto",Turno)) %>%
      mutate(Turno = gsub("^Mat$","Matutino",Turno))
  }

  datos <- datos %>%
    filter(!is.na(Datos))
  # Retornar el dataframe procesado
  return(datos)
}
