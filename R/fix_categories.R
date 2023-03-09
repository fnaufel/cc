#' Uses more detailed categories and subcategories.
#'
#' It returns nothing, but writes files `df_auto_categorized.csv` and `df_to_categorize.csv` to the current dir.
#'
#' This function uses file `../../items_categories.csv`, which contains the association between items and categories.
#'
#' @param due_date Date of current fatura.
#' @param df Tibble produced by [guess_date()].
#' @return NULL
#' @author Fernando Naufel
#' @importFrom readr read_csv write_csv
#' @importFrom powerjoin power_left_join coalesce_yx
#' @importFrom stringr str_detect str_to_upper fixed
#' @importFrom dplyr filter select
#' @export
fix_categories <- function (df, due_date) {

  new_cats <- readr::read_csv(
    '../../items_categories.csv',
    comment = '#'
  )

  # TODO: first, examine last month's fatura to see if the same items appear with same n_parcelas and parcela one less than current parcela.

  # Assign categories based on items
  df <- df %>%
    powerjoin::power_left_join(
      new_cats,
      by = ~ stringr::str_detect(
        stringr::str_to_upper(.x$item),
        .y$item
      ),
      keep = 'left',
      na_matches = 'never',
      conflict = powerjoin::coalesce_yx
    )

  # Tibble containing items that matched
  df_ok <- df %>%
    dplyr::filter(stringr::str_detect(categoria, stringr::fixed(' | '))) %>%
    dplyr::select(
      data,
      item,
      valor,
      categoria,
      cidade,
      parcela,
      n_parcelas
    )

  # Write to file
  df_ok %>%
    readr::write_csv(
      'df_auto_categorized.csv'
    )

  # Tibble containing items that DID NOT MATCH.
  # These items still have the original MC categories.
  df_to_check <- df %>%
    dplyr::filter(!stringr::str_detect(categoria, stringr::fixed(' | '))) %>%
    dplyr::select(
      data,
      item,
      valor,
      categoria,
      cidade,
      parcela,
      n_parcelas
    )

  # Write to file
  df_ok %>%
    readr::write_csv(
      'df_to_categorize.csv'
    )

  NULL

}
