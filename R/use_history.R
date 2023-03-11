#' Try to get info from last month's csv to fix categories.
#'
#' @param df This month's tibble.
#' @param dirname Path passed by the user to [import_dir()].
#' @return A tibble.
#' @author Fernando Naufel
use_history <- function(df, dirname) {

  # TODO: Check all this

  # Get number of last month
  this_month <- df %>%
    dplyr::slice(1) %>%
    dplyr::pull(data_fatura)
  last_month <- (this_month - months(1)) %>%
    lubridate::month() %>%
    formatC(width = 2, flag = '0')

  # Assemble filename for last month's final tibble
  # TODO: change this to get most recent df_merged file in last month's directory
  last_month_final_csv <- fs::path_norm(
    fs::path_join(
      dirname,
      '..',
      last_month,
      'df_merged'
    )
  )

  # If possible, get info from last month's tibble
  # if (fs::file_exists(last_month_final_csv)) {
  #   df <- df %>%
  #     ???(last_month_final_csv)
  # }


  # TODO: first, examine last month's fatura (final df) to see if the same items appear with same n_parcelas and parcela one less than current parcela. df already has `data_fatura` column. Import obs column too.

  # Read csv file into last_df

  # df_parcelado <- rows with parcela > 1

  # df_rest <- other rows

  # df_joined <- Left join df_parcelado with last_df by data, item, valor, n_parcelas

  # If df_joined$parcela.x != df_joined$parcela.y + 1, move row to df_rest

  # Eliminate redundant fields in df_joined

  # Concatenate categoria.y and subcategoria.y with ' | '

  # Bind df_joined with df_rest and arrange

  # Return bound df

}
