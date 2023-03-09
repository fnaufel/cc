#' Import a csv file produced by [tabula](https://tabula.technology/).
#'
#' @description The user must manually select the areas of the pdf file to be converted to csv files.
#'
#' The csv file must not have column names in the first line.
#'
#' The csv file must contain 3 columns, otherwise, an error is thrown.
#'
#' @param filename Path and name of the csv file.
#' @return A tibble.
#' @author Fernando Naufel
#' @importFrom readr read_csv locale parse_number
#' @importFrom janitor clean_names remove_empty
#' @importFrom dplyr rename last_col mutate select starts_with distinct
#' @importFrom stringr str_remove_all
#' @export
import_csv <- function(filename) {

  df <- readr::read_csv(
    filename,
    col_names = FALSE,
    locale = readr::locale(
      decimal_mark = ",",
      grouping_mark = "."
    ),
    show_col_types = FALSE
  ) %>%
    janitor::clean_names() %>%
    janitor::remove_empty(c('rows', 'cols'), quiet = TRUE) %>%
    dplyr::rename(
      data = x1,
      item = x2,
      valor = dplyr::last_col()
    ) %>%
    dplyr::mutate(
      valor = readr::parse_number(
        stringr::str_remove_all(valor, ' '),
        locale = readr::locale(
          decimal_mark = ",",
          grouping_mark = "."
        )
      )
    )

  if (length(df) > 3) {

    extra_cols <-
      names(df)[startsWith(names(df), 'x')] %>%
      paste(collapse = ', ')

    extra_df <- df %>%
      dplyr::select(dplyr::starts_with('x')) %>%
      dplyr::distinct()

    stop(
      '\nExtra column(s) in ',
      filename,
      ':\n',
      extra_cols,
      '\nValues and counts:\n',
      extra_df,
      '\n',
      call. = FALSE
    )

  }

  df
}
