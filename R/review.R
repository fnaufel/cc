#' Read a csv file as produced by [import_dir()] into a tibble and edit interactively.
#'
#' @description This function uses a table of categories for autocompletion.
#'
#' @param filename Path and name of csv file.
#' @param categories Path and name of csv file containing categories.
#'
#' @importFrom readr read_csv locale
#' @importFrom fs path_norm path_join path_abs
#' @importFrom dplyr pull
#' @importFrom DataEditR data_edit
#' @export
review <- function(
  filename,
  categories = '../../categories.csv'
) {

  # Build path to categories csv file
  dirparts <- fs::path_split(fs::path_abs(filename))[[1]]
  dirname <- fs::path_join(dirparts[-length(dirparts)])

  # Read standard categories and subcategories
  categories <- readr::read_csv(
    fs::path_norm(fs::path_join(c(dirname, categories)))
  ) %>%
    dplyr::pull(categoria)

  # Read
  df <- read_df(filename)

  # Edit interactively and save
  df %>%
    DataEditR::data_edit(
      save_as = append_time(filename),
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

