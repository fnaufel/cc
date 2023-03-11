#' Match items to more detailed categories and subcategories.
#'
#' @description This function uses two sources, in this order:
#' * Last month's fatura,
#' * The file in `mapping`, which contains the association between items and categories.
#'
#' @param df Tibble produced by [guess_year()].
#' @param dirname Path passed by the user to [import_dir()].
#' @param mapping Path and name of a csv file containing 2 columns: `item` and `category`.
#' @return A tibble.
#' @author Fernando Naufel
#' @importFrom readr read_csv write_csv
#' @importFrom powerjoin power_left_join coalesce_yx
#' @importFrom stringr str_detect str_to_upper fixed
#' @importFrom dplyr filter select
#' @importFrom fs path_norm path_join
#' @importFrom lubridate month
fix_categories <- function (df, dirname, mapping) {

  # First, try to get info from last month's fatura
  df <- df %>%
    use_history(dirname)

  # Read csv with mapping from items to categories
  new_cats <- readr::read_csv(
    fs::path_norm(
      fs::path_join(c(dirname, mapping))
    ),
    comment = '#'
  )

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
