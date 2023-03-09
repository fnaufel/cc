#' Compute the year of each item's date.
#'
#' @description Originally, `data` only has day and month. We compare `data` with the due date (usually this month); if `data` is in the future, we conclude the item is an installment from last year.
#'
#' **Careful:** this only works *if no purchase has more than 12 installments*.
#'
#' @param due_date String containing date of current fatura in format 'dd/mm/yyyy'.
#' @param df Tibble produced by [extract_cat_and_city()].
#' @return A tibble.
#' @author Fernando Naufel
#' @importFrom dplyr mutate if_else arrange
#' @importFrom lubridate dmy year years
#' @importFrom stringr str_c
#' @export
guess_year <- function(df, due_date) {

  due_date <- lubridate::dmy(due_date)

  df %>%
    dplyr::mutate(
      data = lubridate::dmy(stringr::str_c(data, '/', lubridate::year(due_date)))
    ) %>%
    dplyr::mutate(
      data = dplyr::if_else(
        data >= due_date,
        data - lubridate::years(1),
        data
      )
    ) %>%
    dplyr::mutate(
      data_fatura = due_date
    ) %>%
    dplyr::arrange(data)

}
