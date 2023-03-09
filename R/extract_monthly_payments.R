#' Extract installment info from `item` field.
#'
#' @description Creates columns `parcela` and `n_parcelas`.
#'
#' @param df A tibble created by [join_lines()].
#' @return A tibble.
#' @author Fernando Naufel
#' @importFrom tidyr separate_wider_regex separate_wider_delim
#' @importFrom dplyr mutate
#' @importFrom stringr str_trim str_remove
#' @export
extract_monthly_payments <- function(df) {

  df %>%
    tidyr::separate_wider_regex(
      item,
      c(
        'item' = '^.*',
        'parcela' = '\\d\\d/\\d\\d$'
      ),
      too_few = 'align_start'
    ) %>%
      tidyr::separate_wider_delim(
        parcela,
        '/',
        names = c('parcela', 'n_parcelas'),
        too_few = 'align_start'
      ) %>%
      dplyr::mutate(
        parcela = as.numeric(parcela),
        n_parcelas = as.numeric(n_parcelas),
        item = stringr::str_trim(stringr::str_remove(item, '-CT.*$'))
      )

}
