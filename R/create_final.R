#' Merge the two reviewed tibbles, save and return the result.
#'
#' @param dirname Directory where csv files are.
#'
#' @return A tibble.
#'
#' @author fnaufel
#' @importFrom readr read_csv locale write_csv
#' @importFrom fs path_norm path_join
#' @importFrom dplyr arrange
#' @export
create_final <- function (dirname) {

  df_1 <- read_df(
    fs::path_norm(
      fs::path_join(
        c(dirname, 'df_matched_reviewed.csv')
      )
    )
  )

  df_2 <- read_df(
    fs::path_norm(
      fs::path_join(
        c(dirname, 'df_unmatched_reviewed.csv')
      )
    )
  )

  df_final <- df_1 %>%
    rbind(df_2) %>%
    dplyr::arrange(data)

  df_final %>%
    write_df(
      fs::path_norm(
        fs::path_join(
          c(dirname, 'df_final.csv')
        )
      )
    )

  df_final

}
