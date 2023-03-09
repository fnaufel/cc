#' Read `df_matched.csv` into a tibble and edit interactively.
#'
#' @param dirname Directory where csv files are.
#'
#' @importFrom readr read_csv locale
#' @importFrom fs path_norm path_join path_ext_remove path_ext_set
#' @importFrom dplyr pull
#' @importFrom DataEditR data_edit
#' @export
review_matched <- function(dirname) {

  review(dirname, 'df_matched.csv')

}

#' Read `df_unmatched.csv` into a tibble and edit interactively.
#'
#' @param dirname Directory where csv files are.
#'
#' @importFrom readr read_csv locale
#' @importFrom fs path_norm path_join path_ext_remove path_ext_set
#' @importFrom dplyr pull
#' @importFrom DataEditR data_edit
#' @export
review_unmatched <- function(dirname) {

  review(dirname, 'df_unmatched.csv')

}

review <- function(dirname, file) {

  # Read standard categories and subcategories
  categories <- readr::read_csv(
    fs::path_norm(
      fs::path_join(
        c(dirname, '../../categories.csv')
      )
    )
  ) %>%
    dplyr::pull(categoria)

  # Read matched df
  df <- readr::read_csv(
    fs::path_norm(
      fs::path_join(
        c(dirname, file)
      )
    ),
    col_types = 'DcdccciiD'
  )

  # Edit interactively and save
  df %>%
    DataEditR::data_edit(
      save_as = fs::path_norm(
        fs::path_join(
          c(
            dirname,
            file %>%
              fs::path_ext_remove() %>%
              paste0('_reviewed') %>%
              fs::path_ext_set('csv')
          )
        )
      ),
      viewer = 'browser',
      theme = 'paper',
      col_names = FALSE,
      col_edit = FALSE,
      row_edit = FALSE,
      col_readonly =
        names(df)[!(names(df) %in% c('categoria', 'obs'))],
      col_stretch = TRUE,
      col_options = list(
        'data' = 'date',
        'categoria' = categories,
        'data_fatura' = 'date'
      )
    )

}

