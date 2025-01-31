#' Agrega columnas a los dataframes de una lista basadas en su nombre
#'
#' Esta función toma una lista de dataframes cuyos nombres tienen el formato
#' `"palabra1_palabra2_numero1_numero2_numero3"`, y agrega cinco columnas nuevas
#' a cada dataframe: `nivel`, `indice`, `anio_inicio`, `anio_final` y `periodo`,
#' basadas en la estructura del nombre del dataframe.
#'
#' @param lista_dfs Una lista de dataframes con nombres en el formato
#' `"palabra1_palabra2_numero1_numero2_numero3"`.
#'
#' @return Una lista de dataframes con las nuevas columnas agregadas.
#'
#' @examples
#' lista_dfs <- list(
#'   "basico_educacion_2010_2020_1" = data.frame(a = 1:3, b = 4:6),
#'   "medio_salud_2005_2015_2" = data.frame(x = c("A", "B"), y = c(10, 20))
#' )
#' lista_procesada <- procesar_lista_dfs(lista_dfs)
#' print(lista_procesada)
#'
#' @export
add_name_in_df <- function(lista_dfs) {
  # Función para procesar cada dataframe
  agregar_columnas <- function(df, nombre) {
    # Dividir el nombre en partes
    partes <- unlist(strsplit(nombre, "_"))

    # Verificar que el nombre tenga al menos 5 elementos
    if (length(partes) < 5) {
      stop("El nombre del dataframe no tiene el formato esperado: palabra1_palabra2_numero1_numero2_numero3")
    }

    # Extraer los valores del nombre
    nivel <- partes[1]
    indice <- partes[2]
    anio_inicio <- as.numeric(partes[3])
    anio_final <- as.numeric(partes[4])
    semestre <- as.numeric(partes[5])

    # Agregar columnas al dataframe
    df$nivel <- nivel
    df$indice <- indice
    df$anio_inicio <- anio_inicio
    df$anio_final <- anio_final
    df$semestre <- semestre

    return(df)
  }

  # Aplicar la función a cada dataframe de la lista
  lista_dfs <- mapply(agregar_columnas, lista_dfs, names(lista_dfs), SIMPLIFY = FALSE)

  return(lista_dfs)
}
