#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param dirname
#' @return
#' @author Fernando Naufel
#' @export
get_most_recent <- function(dirname, glob) {

  # Get filenames
  files <- fs::dir_ls(dirname, glob = glob)

  # If none, error
  if (length(files) == 0) {
    stop(
      '\nNo files matching glob ',
      glob,
      ' found in directory ',
      dirname,
      '\n'
    )
  }

  # Get times
  times <- files %>%
    str_match('_\\d{4}_\\d{2}_\\d{2}_\\d{2}_\\d{2}_\\d{2}')

  # Find index of most recent
  i <- which(times == max(times))

  # return filename
  files[i]

}
