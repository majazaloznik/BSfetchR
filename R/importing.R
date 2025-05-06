#' Import structural metadata for a BS table
#'
#' Umbrella function that prepares and import all the metadata tables into
#' the database. It uses the functions from the UMARimportR package to
#' insert the data into the database.
#'
#' @param code_no BS code name of the table
#' @param con connection to database
#' @param schema schema name, defaults to "platform"
#' @param all_levels logical whether to let user use dimension selector, default
#' is true, which automatically selects all levels in all dimensions
#' @param keep_vintage logical indicating whether to keep vintages, defaults to F
#'
#' @returns nothing
#' @export
#'
BS_import_structure <- function(px_code, con, schema = "platform", all_levels = TRUE,
                                  keep_vintage = FALSE) {
  message("Importing structure data: ", px_code, " into schema ", schema)
  # Create list to store all results
  insert_results <- list()
  # prepare and insert table
  table_table <- prepare_table_table(px_code, keep_vintage, con, schema)
  insert_results$table <- UMARimportR::insert_new_table_table(con, table_table, schema)
  message("Table insert: ", insert_results$table$count, " rows")
  # preapre and insert category table
  category_table <- prepare_category_table(px_code, con, schema)
  insert_results$category <- UMARimportR::insert_new_category(con, category_table, schema)
  message("Category insert: ", insert_results$category$count, " rows")
  # prepare and insert category relationship table
  category_relationship_table <- prepare_category_relationship_table(px_code, con, schema)
  insert_results$category_relantionship <- UMARimportR::insert_new_category_relationship(
    con, category_relationship_table, schema)
  message("Category relationship insert: ", insert_results$category_relantionship$count, " rows")
  # prepare and insert category table table
  category_table_table <- prepare_category_table_table(px_code,  con, schema)
  insert_results$category_table <- UMARimportR::insert_new_category_table(
    con, category_table_table, schema)
  message("Category table insert: ", insert_results$category_table$count, " rows")
  # prepare and insert table dimension table
  table_dimension_table <- prepare_table_dimensions_table(px_code, con, schema)
  insert_results$table_dimensions <- UMARimportR::insert_new_table_dimensions(
    con, table_dimension_table, schema)
  message("Table dimensions insert: ", insert_results$table_dimensions$count, " rows")
  # prepare and select dimension levels before inserting them
  dimension_levels_table_full <- prepare_dimension_levels_table(px_code, con, schema)
  if(all_levels){
    dimension_levels_table <- dimension_levels_table_full |>
      dplyr::select(-dimension)} else {
        dimension_levels_table <- UMARimportR::dimension_selector(dimension_levels_table_full) |>
          dplyr::select(-dimension)}
  insert_results$dimension_levels <- UMARimportR::insert_new_dimension_levels(
    con, dimension_levels_table, schema)
  message("Dimension levels insert: ", insert_results$dimension_levels$count, " rows")
  # prepare and insert unit table
  unit_table <-  prepare_unit_table(px_code)
  insert_results$units <- UMARimportR::insert_new_unit(con, unit_table, schema)
  message("Units insert: ", insert_results$units$count, " rows")
  # prepare and insert series table
  series_table <- prepare_series_table(px_code, con, schema)
  insert_results$series <- UMARimportR::insert_new_series(con, series_table, schema)
  message("Series insert: ", insert_results$series$count, " rows")
  # prepare and insert series levels table
  series_levels_table <- prepare_series_levels_table(px_code, con, schema)
  insert_results$series_levels <- UMARimportR::insert_new_series_levels(
    con, series_levels_table, schema)
  message("Series levels insert: ", insert_results$series_levels$count, " rows")
  invisible(insert_results)
}
