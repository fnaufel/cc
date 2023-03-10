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
#' @importFrom lubridate month
fix_categories <- function (df, dirname) {

  # Get number of last month
  this_month <- df %>%
    dplyr::slice(1) %>%
    dplyr::pull(data_fatura)
  last_month <- (this_month - months(1)) %>%
    lubridate::month() %>%
    formatC(width = 2, flag = '0')

  # Assemble filename for last month's final tibble
  last_month_final_csv <- fs::path_norm(
    fs::path_join(
      dirname,
      '..',
      last_month,
      'df_final.csv'
    )
  )

  # If possible, get info from last month's tibble
  if (fs::file_exists(last_month_final_csv)) {
    df <- df %>%
      use_history(last_month_final_csv)
  }

  # Read csv with mapping from items to categories
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
