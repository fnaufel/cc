#' Extracts category and city from `info` field.
#'
#' Creates columns `categoria` and `cidade`.
#'
#' @param df A tibble created by [extract_monthly_payments()].
#' @return A tibble.
#' @author Fernando Naufel
#' @importFrom tidyr separate_wider_delim
#' @importFrom dplyr mutate if_else select
#' @importFrom stringr str_to_upper str_trim
#' @export
extract_cat_and_city <- function(df) {

  df %>%
    tidyr::separate_wider_delim(
      info,
      '.',
      names = c('categoria', 'cidade'),
      too_few = 'align_start'
    ) %>%
    dplyr::mutate(
      categoria = stringr::str_to_upper(stringr::str_trim(categoria)),
      cidade = stringr::str_to_upper(stringr::str_trim(cidade))
    ) %>%
    dplyr::mutate(
      categoria = dplyr::if_else(
        categoria == '', NA_character_, categoria
      ),
      cidade = dplyr::if_else(
        cidade == '', NA_character_, cidade
      )
    ) %>%
    dplyr::select(
      data, item, valor, parcela, n_parcelas,
      categoria, cidade
    )

}
