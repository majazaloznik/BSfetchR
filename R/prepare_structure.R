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
  get_px_metadata(px_code, con, schema)  |>
    dplyr::select(-updated, -valuenotes, -units) |>
    dplyr::mutate(code = px_code,
                  keep_vintage = keep_vintage)
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
    dplyr::mutate(source_id = !!source_id)
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


