#' Filter a list of dataframes to remove empty ones
#'
#' This function takes a list of dataframes and removes any dataframe
#' that has zero rows.
#'
#' @param df_list A list of dataframes.
#'
#' @return A list containing only the dataframes that have more than zero rows.
#'
#' @examples
#' df1 <- data.frame(a = 1:3, b = 4:6)
#' df2 <- data.frame()  # Empty dataframe
#' df3 <- data.frame(x = c("A", "B"), y = c(10, 20))
#'
#' df_list <- list(df1 = df1, df2 = df2, df3 = df3)
#' filtered_list <- filter_non_empty_dfs(df_list)
#' print(filtered_list)  # df2 is removed
#'
#' @export
filter_non_empty_dfs <- function(df_list) {
  Filter(function(df) nrow(df) > 0, df_list)
}
