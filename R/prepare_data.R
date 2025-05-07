#' Prepare table to insert into `vintage` table
#'
#' Helper function that populates the vintage table with the new vintages. It gets
#' the series id's from the database and adds the publication date from the px.
#'
#' Returns table ready to insert into the `vintage`table with the
#' UMARimportr::insert family of functions.
#'
#' @param px_code the original BS code (e.g. i_36_6as)
#' @param con a connection to the database
#' @param schema the schema to use for the connection, default is "platform"
#'
#' @return a dataframe with the `series_id` and `published` columns
#' for all the series in this table.
#' @export
#'
prepare_vintage_table <- function(px_code, con, schema){
  tbl_id <- UMARaccessR::sql_get_table_id_from_table_code(con, px_code, schema)
  published <- get_px_metadata(px_code)$updated
  last_published <- UMARaccessR::sql_get_last_publication_date_from_table_id(tbl_id, con, schema)
  if(!is.null(last_published) && published == last_published) {
    stop(paste0("These vintages for table ", px_code,
                " are not new, they will not be inserted again."))
  } else {
    series_ids <- UMARaccessR::sql_get_series_ids_from_table_id(tbl_id, con, schema)
    data.frame(series_ids, published) |>
      dplyr::rename(series_id = id)
  }
}


#' Prepare BS data table for insertion
#'
#' Processes raw BS data into a format ready for database insertion,
#' handling BS-specific quirks like flags in time periods.
#'
#' @param px_code the original BS code (e.g. i_36_6as)
#' @param con Database connection
#' @param schema the schema to use for the connection, default is "platform"
#'
#' @return A list containing:
#'  - data: The processed data frame
#'  - table_id: The table ID
#'  - time_dimension: The name of the time dimension
#'  - interval_id: The interval ID
#'  - dimension_ids: The non-time dimension IDs
#'  - dimension_names: The names of the dimensions
#' @export
prepare_bs_data_for_insert <- function(px_code, con, schema = "platform") {
  # Get raw data
  tbl_id <- UMARaccessR::sql_get_table_id_from_table_code(con, px_code, schema)
  time_dim <- UMARaccessR::sql_get_time_dimension_from_table_code(px_code, con, schema)
  df <- get_px_data(px_code)
  dim_levels <- prepare_dimension_levels_table(px_code, con, schema)
  dims_to_recode <- unique(dim_levels$dimension)
  # Get non-dimension columns
  non_dim_cols <- setdiff(names(df), dims_to_recode)
  # Process all dimensions at once
  df <- dims_to_recode |>
    purrr::map_dfc(function(dim) {
      # Create lookup vector
      lookup <- dim_levels |>
        dplyr::filter(dimension == dim) |>
        dplyr::select(level_text, level_value) |>
        tibble::deframe()
      dplyr::tibble(!!dim := unname(lookup[as.character(df[[dim]])]))
    })  |>
    # Add back non-dimension columns
    dplyr::bind_cols(df[, non_dim_cols, drop = FALSE]) |>
    dplyr::mutate(
      across(-value, as.character),  # Convert all columns except "value" to character
      value = as.numeric(value),     # Ensure value is numeric
      time = Datum,
      flag = "") # empty flag


  # remove levels that we are not tracking
  dim_levels_in_db <- UMARaccessR::sql_get_dimension_levels_from_table_id(tbl_id, con, schema)
  dim_list <- split(dim_levels_in_db$level_value, dim_levels_in_db$dimension)
  names(dim_list)<- make.names(names(dim_list))
  dim_cols <- names(dim_list)
  keep_rows <- rep(TRUE, nrow(df))
  for (col in dim_cols) {
    keep_rows <- keep_rows & df[[col]] %in% dim_list[[col]]
  }
  df <- df[keep_rows, ]

  # Get metadata
  dim_ids <- UMARaccessR::sql_get_non_time_dimensions_from_table_id(tbl_id, con, schema)
  interval_id <- get_interval_id_from_px(px_code)

  df <- df |> dplyr::mutate(interval_id = !!interval_id)


  # Return structured result
  list(
    data = df,
    table_id = tbl_id,
    time_dimension = time_dim,
    interval_id = interval_id,
    dimension_ids = dim_ids$id,
    dimension_names = dim_ids$dimension
  )
}
