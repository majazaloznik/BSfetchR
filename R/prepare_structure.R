#' Prepare table to insert into `source` table

#' Helper function that manually prepares the new line for the source table.
#'
#' @param con connection to the database.
#' @param schema the schema to use for the connection, default is "platform"
#' @return a dataframe with the `name`, `name_long`, and `url`, columns.
#' for this table.
#' @export
prepare_source_table <- function(con, schema = "platform") {
  DBI::dbExecute(con, paste0("set search_path to ", schema))
  source_id <- UMARaccessR::sql_get_source_code_from_source_name(con, "BS", schema)
  if (is.null(source_id)){
  id <- dplyr::tbl(con, "source") |>
    dplyr::summarise(max = max(id, na.rm = TRUE)) |>
    dplyr::pull() + 1
  data.frame(id = id,
             name = "BS",
             name_long = "Banka Slovenije",
             url = "https://px.bsi.si/pxweb/sl/serije_slo/")} else {
             message("BS already listed in the source table.")}
}

#' Prepare table to insert into `table` table
#'
#' This one is really straightforward, and slightly superfluous, since it just
#' uses the \link[BSfetchR]{get_px_metadata} function and removes two columns.
#' Returns table ready to insert into the `table` table with the db_writing family
#' of functions.
#'
#' @param px_code the original BS code (e.g. i_36_6as)
#' @param con a connection to the database
#' @param schema the schema to use for the connection, default is "platform"
#' @param keep_vintage boolean whether to keep vintages
#'
#' @return a dataframe with the `code`, `name`, `source_id`, `url`, and `notes` columns
#' for this table.
#' @export
prepare_table_table <- function(px_code, keep_vintage = FALSE, con, schema = "platform") {
  source_id <- UMARaccessR::sql_get_source_code_from_source_name(con, "BS", schema)
  get_px_metadata(px_code, con, schema)  |>
    dplyr::select(-updated, -valuenotes, -units) |>
    dplyr::mutate(code = px_code,
                  keep_vintage = keep_vintage,
                  source_id = !!source_id) |>
    dplyr::relocate(source_id, .before = url)
}

#' Prepare table to insert into `category` table
#'
#' Helper function that extracts all the parent categories from the full
#' hierarchy data.frame, and prepares the category table with field ids and
#' their names. Returns table ready to insert into the `category` table with the db_writing family
#' of functions. Uses the full field hierarchy with parent_ids et al, output from
#' \link[BS fetchR]{get_all_bsi_tables}.
#'
#' @param px_code the original BS code (e.g. i_36_6as)
#' @param con a connection to the database
#' @param schema the schema to use for the connection, default is "platform"
#' @return a dataframe with the `id`, `name`, `source_id` for each category that
#' the table is a member of.
#' @export
#'
prepare_category_table <- function(px_code, con, schema = "platform") {
  full <- BSfetchR::full |>
    dplyr::rename(matrix_name = px_code)
  tmp <- full |> dplyr::filter(!is.na(matrix_name))
  id_no <- unique(tmp$id[tmp$matrix_name == px_code])
  source_id <- UMARaccessR::sql_get_source_code_from_source_name(con, "BS", schema)
  SURSfetchR::get_row(id_no, full) |>
    dplyr::mutate(source_id = !!source_id) |>
    dplyr::select(-parent_id)
}

#' Prepare table to insert into `category_relationship` table
#'
#' Helper function that extracts the field hierarchy from the full
#' hierarchy data.frame, and  prepares the category relationship table with field ids and
#' their parents' id. Returns table ready to insert into the `category_relationship`
#' table with the db_writing family of functions.
#' \link[BS fetchR]{get_all_bsi_tables}
#'
#' @param px_code the original BS table code (e.g. i_36_6as)
#' @param con a connection to the database
#' @param schema the schema to use for the connection, default is "platform"
#'
#' @return a dataframe with the `id`, `name`, `parent_id`, `source_id` for each relationship
#' betweeh categories
#' @export
#'
prepare_category_relationship_table <- function(px_code, con, schema = "platform") {
  full <- BSfetchR::full |>
    dplyr::rename(matrix_name = px_code)
  tmp <- full |> dplyr::filter(!is.na(matrix_name))
  id_no <- unique(tmp$id[tmp$matrix_name == px_code])
  source_id <- UMARaccessR::sql_get_source_code_from_source_name(con, "BS", schema)
  SURSfetchR::get_row(id_no, full) |>
    dplyr::mutate(parent_id = as.numeric(parent_id)) |>
    dplyr::arrange(parent_id) |>
    dplyr::select(-name) |>
    dplyr::mutate(source_id = !!source_id)
}



#' Prepare table to insert into `category_table` table
#'
#' Helper function that extracts the parent category for each table from the full
#' hierarchy data.frame, and fills up the category_table table with the table ids and
#' their categories (parents). A single table can have multiple parents - meaning
#' it is member of several categories (usually no more than two tho). Returns table
#' ready to insert into the `category_table`table with the db_writing family of functions.
#'
#' @param px_code the original BS table code (e.g. i_36_6as)
#' @param con connection to the database
#' @param schema database schema, defaults to "platform"
#' \link[SURSfetchR]{get_full_structure}
#' @return a dataframe with the `category_id` `table_id` and `source_id` columns for
#' each table-category relationship.
#' @export
#'
prepare_category_table_table <- function(px_code, con, schema = "platform") {
  tbl_id <- UMARaccessR::sql_get_table_id_from_table_code(con, px_code, schema)
  source_id <- UMARaccessR::sql_get_source_code_from_source_name(con, "BS", schema)

  BSfetchR::full %>%
    dplyr::filter(px_code == !!px_code) %>%
    dplyr::select(category_id = id) %>%
    dplyr::distinct() %>%
    dplyr::mutate(table_id = !!tbl_id,
                  source_id = !!source_id)
}


#' Prepare table to insert into `table_dimensions` table
#'
#' Helper function that extracts the dimensions for each table and their "time" status.
#' Returns table ready to insert into the `table_dimensions`table with the
#' db_writing family of functions.
#'
#' @param px_code the original BS table code (e.g. i_36_6as)
#' @param con connection to the database
#' @param schema database schema, defaults to "platform"
#' @return a dataframe with the `table_id`, `dimension_name`, `time` columns for
#' each dimension of this table.
#' @export
#'
prepare_table_dimensions_table <- function(px_code, con, schema = "platform") {
  tbl_id <- UMARaccessR::sql_get_table_id_from_table_code(con, px_code, schema)
  dim_list <- get_px_dim_levels(px_code, con, schema)
  data.frame(table_id = tbl_id,
             dimension = names(dim_list)) |>
  dplyr::mutate(is_time = ifelse(grepl("Datum", dimension), TRUE, FALSE))
}


#' Prepare table to insert into `dimension_levels` table
#'
#' Helper function that extracts the levels for all the dimensions for each
#' table and get their codes and text.
#' Returns table ready to insert into the `dimension_levels`table with the
#' db_writing family of functions.
#'
#' @param px_code the original BS table code (e.g. i_36_6as)
#' @param con connection to the database
#' @param schema database schema, defaults to "platform"
#' @return a dataframe with the `dimension_name`, `values`, `valueTexts`, and `id`
#' columns for this table.
#' @export
#'
prepare_dimension_levels_table <- function(px_code, con, schema = "platform") {

  tbl_id <- UMARaccessR::sql_get_table_id_from_table_code(con, px_code, schema)
  dim_ids <- UMARaccessR::sql_get_non_time_dimensions_from_table_id( tbl_id, con, schema)
  l <- get_px_list(px_code)

  names(l$VALUES)  |>
    purrr::map_df(function(dim_name) {
      tibble::tibble(
        dimension_name = dim_name,
        level_text = l$VALUES[[dim_name]],
        level_value = seq_along(l$VALUES[[dim_name]]) - 1 ) |>
        dplyr::filter(dimension_name != "Datum")}) |>
    dplyr::inner_join(dim_ids, by = c("dimension_name" = "dimension")) |>
    dplyr::rename(tab_dim_id = id, dimension = dimension_name)
}


#' Prepare table to insert into `units` table
#'
#' Helper function that extracts the units for each table from the px metadata.
#' Returns table ready to insert into the `units`table with the
#' db_writing family of functions.
#'
#' @param px_code the original BS table code (e.g. i_36_6as)
#' @return a dataframe with the single column containing the different units used
#' in this table.
#' @export
#'
prepare_unit_table <- function(px_code) {
  df <- data.frame(strsplit(get_px_metadata(px_code)$units, ", ")) %>%
    dplyr::mutate_all(tolower)
  names(df) <- "name"
  df
}




#' Prepare table to insert into `series` table
#'
#' This is a big one. Prepares the table to insert into the series table, which
#' along expanding the levels to get all the series and their codes as well, which
#' include also figuring out their time interval, this function also tries to
#' extract the unit for each series,
#' Returns table ready to insert into the `series`table with the
#' db_writing family of functions.
#'
#' @param px_code character object of the matrix code (e.g. 2300123S)
#' @param con connection to the database
#' @param schema database schema, defaults to "platform"
#'
#' @return a dataframe with the following columns: `series_title`, `series_code`,
#' `unit_id`, `table_id` and `interval_id`for each series in the table
#' well as the same number of rows as there are series
#' @export
prepare_series_table <- function(px_code, con, schema = "platform"){
  tbl_id <-  UMARaccessR::sql_get_table_id_from_table_code(con, px_code, schema)
  time_dimension <- UMARaccessR::sql_get_time_dimension_from_table_code(px_code, con, schema)
  interval_id <- get_interval_id_from_px(px_code)

  unit_id <- UMARaccessR::sql_get_unit_id_from_unit_name(tolower(get_px_metadata(px_code)$units), con, schema)

  expanded_level_codes <- expand_to_level_codes(tbl_id, con, schema) |>
    dplyr::mutate(unit_id = unit_id) |>
    tidyr::unite("code", dplyr::starts_with("Var"), sep = "--")  |>
    dplyr::mutate(code = paste0("BSS--", px_code, "--", code, "--",interval_id))  |>
    cbind(expand_to_series_titles(tbl_id, con, schema)) |>
    dplyr::mutate(table_id = tbl_id,
                  interval_id = interval_id)  |>
    dplyr::select(table_id, name_long, unit_id, code, interval_id)
}




#' Prepare table to insert into `series_levels` table
#'
#' Helper function that extracts the individual levels for each series and
#' gets the correct dimension id for each one and the correct series id to
#' keep with the constraints.
#' Returns table ready to insert into the `series_levels`table with the
#' db_writing family of functions.
#'
#'
#' @param px_code character object of the matrix code (e.g. 2300123S)
#' @param con connection to the database
#' @param schema database schema, defaults to "platform"
#' @return a dataframe with the `series_id`, `tab_dim_id`, `level_value` columns
#' all the series-level combinatins for this table.
#' @export
#'
prepare_series_levels_table <- function(px_code, con, schema = "platform") {
  tbl_id <-  UMARaccessR::sql_get_table_id_from_table_code(con, px_code, schema)
  dimz <- UMARaccessR::sql_get_dimensions_from_table_id(tbl_id, con, schema) |>
    dplyr::filter(is_time != TRUE) |>
    dplyr::pull(id)

  UMARaccessR::sql_get_series_from_table_id(tbl_id, con, schema) |>
    dplyr::filter(table_id == tbl_id) |>
    dplyr::select(table_id, id, code)  |>
    tidyr::separate(code, into = c("x1", "x2", paste0(dimz), "x3"), sep = "--") |>
    dplyr::select(series_id = id,  paste0(dimz)) |>
    tidyr::pivot_longer(-series_id, names_to = "tab_dim_id") |>
    dplyr::rename(level_value = value) |>
    as.data.frame()
}



