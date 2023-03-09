#' Gathers info for each payment.
#'
#' Because of the way the pdf file is converted to csv, each record spans 2 lines. The second line contains info about the category and the city.
#'
#' @param df A tibble produced by [import_dir()].
#' @return A tibble.
#' @author Fernando Naufel
#' @importFrom dplyr filter mutate
#' @export
join_lines <- function(df) {

  df1 <- df %>%
    dplyr::filter(!is.na(data))

  df2 <- df %>%
    dplyr::filter(is.na(data))

  if (nrow(df1) != nrow(df2)) {
    stop(
      '\n# of item rows does not match # of category rows: ',
      nrow(df1), ' !- ', nrow(df2),
      '\n'
    )
  }

  df1 %>%
  dplyr::mutate(info = df2$item)

}
