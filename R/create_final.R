#' Bind the 2 most recent reviewed dfs (matched and unmatched) in dirname.
#'
#' @description This function saves the merged df as df_merged_9999_99_99_99_99_99.csv and also returns the df.
#'
#' @param dirname Directory where csv files are.
#'
#' @return A tibble.
#'
#' @author fnaufel
#' @importFrom readr read_csv locale write_csv
#' @importFrom fs path_norm path_join
#' @importFrom dplyr arrange
#' @importFrom tidyr separate_wider_delim
create_final <- function (dirname) {

  # Read most recent reviewed csv files into dfs
  df_1 <- read_df(get_most_recent(dirname, '*_matched_reviewed_*.csv'))
  df_2 <- read_df(get_most_recent(dirname, '*_unmatched_reviewed_*.csv'))

  # Bind them and separate category into category and subcategory
  df_final <- df_1 %>%
    rbind(df_2) %>%
    tidyr::separate_wider_delim(
      categoria,
      ' | ',
      names = c('categoria', 'subcategoria')
    ) %>%
    dplyr::arrange(data)

  # Save df
  df_final %>%
    dplyr::select(
      data,
      item,
      valor,
      categoria,
      subcategoria,
      obs,
      cidade,
      parcela,
      n_parcelas,
      data_fatura
    ) %>%
    write_df(
      fs::path_norm(
        fs::path_join(
          c(dirname, 'df_merged.csv')
        )
      ) %>%
        append_time()
    )

  # Return df
  df_final

}
