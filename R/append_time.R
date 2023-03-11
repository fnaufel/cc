#' Add suffix of the form `_%Y_%m_%d_%H_%M_%S` to file name, keeping extension..
#'
#' @param filename File name as string.
#' @return New filename as string.
#' @author Fernando Naufel
#' @importFrom fs path_ext path_ext_remove
append_time <- function(filename) {

  # Save and remove extension
  extension <- fs::path_ext(filename)
  filename <- fs::path_ext_remove(filename)

  # Remove time suffix, if there
  filename <- stringr::str_remove(
    filename,
    '_\\d{4}_\\d{2}_\\d{2}_\\d{2}_\\d{2}_\\d{2}$'
  )

  # Add new time suffix
  suffix <- Sys.time() %>%
    format('_%Y_%m_%d_%H_%M_%S')

  # Add entension back and return
  paste0(filename, suffix) %>%
    fs::path_ext_set(extension)

}
