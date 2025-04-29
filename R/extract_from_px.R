#' Get PX file information
#'
#' Retrieves a PX file from the Bank of Slovenia data portal
#' based on the PX code.
#'
#' @param px_code Character string identifying the PX file (e.g., "F2_A1S")
#' @return A PX file object as returned by pxR::read.px
#' @export
get_px_list <- function(px_code) {
  url <- BSfetchR::full |>
    dplyr::filter(px_code == !!px_code) |>
    dplyr::pull(px_url)

  pxR::read.px(url, encoding = "utf-8")
}

#' Get metadata for a PX file
#'
#' Extracts metadata from a Bank of Slovenia PX file and
#' prepares it for database storage.
#'
#' @param px_code Character string identifying the PX file (e.g., "F2_A1S")
#' @param con Database connection object
#' @param schema Database schema name, defaults to "platform"
#' @return A data frame with metadata including name, update date, units, and notes
#' @export
get_px_metadata <- function(px_code, con, schema = "platform") {
  l <- get_px_list(px_code)
  source_id <- UMARaccessR::sql_get_source_code_from_source_name(con, "BS", schema)
  url <- BSfetchR::full |>
    dplyr::filter(px_code == !!px_code) |>
    dplyr::pull(px_url)
  data.frame(name = gsub('\"\n\"', "", unlist(l$DESCRIPTION$value)),
             updated = as.POSIXct(l$LAST.UPDATED[[1]],format="%Y%m%d %H:%M",tz=Sys.timezone()),
             units = l$UNITS[[1]],
             notes = I(list(c(l$NOTE, l$NOTEX))),
             valuenotes =I(list(l$VALUENOTE))) |>
    dplyr::mutate(notes = jsonlite::toJSON(notes),
                  source_id = !!source_id,
                  url = url)
}

#' Get dimension levels from a PX file
#'
#' Extracts the dimension levels (categories) from a Bank of Slovenia PX file.
#'
#' @param px_code Character string identifying the PX file (e.g., "F2_A1S")
#' @param con Database connection object
#' @param schema Database schema name, defaults to "platform"
#' @return A list of dimension levels
#' @export
get_px_dim_levels <- function(px_code, con, schema = "platform") {
  l <- get_px_list(px_code)
  l$VALUES
}

#' Get data values from a PX file
#'
#' Extracts the actual data values from a Bank of Slovenia PX file.
#'
#' @param px_code Character string identifying the PX file (e.g., "F2_A1S")
#' @param con Database connection object
#' @param schema Database schema name, defaults to "platform"
#' @return A vector of data values
#' @export
get_px_data <- function(px_code, con, schema = "platform") {
  l <- get_px_list(px_code)
  l$DATA$value
}
