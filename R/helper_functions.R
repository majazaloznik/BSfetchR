#' Expanding from levels to series codes and titles
#'
#' These two helper functions take a set of non-time levels for a single table
#' and expand the grid to get all of their combinations and then either return
#' a dataframe with columns for each level code, or one where the level texts
#' have been concatenated into the series titles.
#' @param code_no code e.g. 0300230S
#' @return dataframe with expanded levels, one column per non-time dimension plus
#' unit_id for the level codes and sinle column with series titles for the other one.
#' @rdname expanding
#' @keywords internal
expand_to_level_codes <- function (tbl_id, con, schema = "platform") {
  levels <- UMARaccessR::sql_get_dimension_levels_from_table_id(tbl_id, con, schema )
  do.call(expand.grid,
          c(setNames(split(levels$level_value, levels$tab_dim_id),
                     paste0("Var", seq_along(unique(levels$tab_dim_id)))),
            stringsAsFactors = FALSE))
}

#' @rdname expanding
#' @keywords internal
expand_to_series_titles <- function (tbl_id, con, schema = "platform") {
  levels <- UMARaccessR::sql_get_dimension_levels_from_table_id(tbl_id, con, schema )
  do.call(expand.grid,
          c(setNames(split(levels$level_text, levels$tab_dim_id),
                     paste0("Var", seq_along(unique(levels$tab_dim_id)))),
            stringsAsFactors = FALSE)) |>
    tidyr::unite("name_long", dplyr::everything(), sep = " -- ")
}
