#' Imports all csv files in directory `dirname`.
#'
#' Just calls [import_csv()] on all files with extension `.csv`.
#'
#' @param dirname Directory as string.
#' @return A tibble.
#' @author Fernando Naufel
#' @importFrom purrr map list_rbind
#' @export
import_dir <- function(dirname) {

  # Get all csv filenames in dir
  files <- dir(
    dirname,
    '\\.csv$',
    full.names = TRUE
  )

  files %>%
    purrr::map(import_csv) %>%
    purrr::list_rbind()

}
