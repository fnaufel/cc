#' Import and process all tabula csv files in directory `dirname`.
#'
#' @description This will read all csv files of the form 'tabula*.csv' in `dirname` and create 2 files (also in `dirname`):
#' * `df_matched_(...).csv`, containing records whose categories were automatically determined (based on a table read from a csv file), and
#' * `df_unmatched_(...).csv`, containing records with the original categories from the fatura pdf.
#'
#' In both filenames above, `(...)` is the current time in the format `yy_mm_dd_hh_mm_ss`.
#'
#' @param dirname Directory as string.
#' @param due_date Date of current fatura. String format dd-mm-yyyy or dd-mm-yy
#' @param categorize If `TRUE`, assign new categories to items.
#' @param mapping Path and name of a csv file containing 2 columns: `item` and `category`. Used if `categorize == TRUE`.
#' @author Fernando Naufel
#' @importFrom dplyr select filter
#' @importFrom stringr str_detect fixed
#' @importFrom readr write_csv
#' @importFrom fs path_norm path_join
#' @importFrom purrr map list_rbind
#' @export
import_dir <- function(
    dirname,
    due_date,
    categorize = TRUE,
    mapping = '../../items_categories.csv'
) {

  # Get all tabula csv filenames in dir
  files <- dir(
    dirname,
    '^tabula.*\\.csv$',
    full.names = TRUE
  )

# Import and bind them into one df
  df <- files %>%
    purrr::map(import_csv) %>%
    purrr::list_rbind()

  # Process df
  df <- df %>%
    join_lines() %>%
    extract_monthly_payments() %>%
    extract_cat_and_city() %>%
    guess_year(due_date)

  # Assign new categories to items, if requested
  if (categorize) {
    df <- df %>% fix_categories(dirname, mapping)
  }

  # Add obs column and select columns in order
  df <- df %>%
    dplyr::mutate(obs = '') %>%
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
  # The way I detect this is not nice:
  # if categoria has a vertical bar "|", it means categoria comes
  # from the table; otherwise, it comes from fatura
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
  if (nrow(df_matched) > 0) {
    df_matched %>%
      write_df(
        fs::path_norm(
          fs::path_join(c(dirname, 'df_matched'))
        )
      )
  }

  if (nrow(df_unmatched) > 0) {
    df_unmatched %>%
      write_df(
        fs::path_norm(
          fs::path_join(c(dirname, 'df_unmatched'))
        )
      )
  }

}
