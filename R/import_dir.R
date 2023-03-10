#' Import and process all tabula csv files in directory `dirname`.
#'
#' @description This will read all csv files of the form 'tabula*.csv' in `dirname` and create 2 files (also in `dirname`):
#' * `df_matched.csv`, containing records whose categories were automatically determined (based on a table read from `dirname/../../categories.csv`), and
#' * `df_unmatched.csv`, containing records with the original categories from the fatura pdf.
#'
#' @param dirname Directory as string.
#' @param due_date Date of current fatura.
#' @author Fernando Naufel
#' @importFrom dplyr select filter
#' @importFrom stringr str_detect fixed
#' @importFrom readr write_csv
#' @importFrom fs path_norm path_join
#' @export
import_dir <- function(dirname, due_date) {

  # Get all csv filenames in dir
  files <- dir(
    dirname,
    '^tabula.*\\.csv$',
    full.names = TRUE
  )

  df <- files %>%
    purrr::map(import_csv) %>%
    purrr::list_rbind()

  df <- df %>%
    join_lines() %>%
    extract_monthly_payments() %>%
    extract_cat_and_city() %>%
    guess_year(due_date) %>%
    fix_categories(dirname) %>%
    mutate(obs = '') %>%
    dplyr::select(
      data,
      item,
      valor,
      categoria,
      obs,
      cidade,
      parcela,
      n_parcelas,
      data_fatura
    )

  # Items that matched
  df_matched <- df %>%
    dplyr::filter(
      stringr::str_detect(
        categoria,
        stringr::fixed(' | ')
      )
    )

  # Items that did not match
  df_unmatched <- df %>%
    dplyr::filter(
      !stringr::str_detect(
        categoria,
        stringr::fixed(' | ')
      )
    )

  # Write to files
  df_matched %>%
    write_df(
      fs::path_norm(
        fs::path_join(c(dirname, 'df_matched.csv'))
      )
    )

  df_unmatched %>%
    write_df(
      fs::path_norm(
        fs::path_join(c(dirname, 'df_unmatched.csv'))
      )
    )

}
