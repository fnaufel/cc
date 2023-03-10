#' Try to get info from last month's csv to fix categories.
#'
#' @param df This month's tibble.
#' @param last_month_final_csv Path to last month's csv file.
#' @return A tibble.
#' @author Fernando Naufel
use_history <- function(df, last_month_final_csv) {

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
