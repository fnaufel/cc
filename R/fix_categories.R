#' Match items to more detailed categories and subcategories.
#'
#' @description This function uses file `../../items_categories.csv`, which contains the association between items and categories.
#'
#' @param df Tibble produced by [guess_year()].
#' @param dirname Path passed by the user to [import_dir()].
#' @return A tibble.
#' @author Fernando Naufel
#' @importFrom readr read_csv write_csv
#' @importFrom powerjoin power_left_join coalesce_yx
#' @importFrom stringr str_detect str_to_upper fixed
#' @importFrom dplyr filter select
#' @importFrom fs path_norm path_join
#' @export
fix_categories <- function (df, dirname) {

  new_cats <- readr::read_csv(
    fs::path_norm(
      fs::path_join(
        c(
          dirname,
          '../../items_categories.csv'
        )
      )
    ),
    comment = '#'
  )

  # TODO: first, examine last month's fatura (final df) to see if the same items appear with same n_parcelas and parcela one less than current parcela. df already has `data_fatura` column. Import obs column too.

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

  df

}
