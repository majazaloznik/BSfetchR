#' Read Bank of Slovenia PX files
#'
#' @param url URL to PX file
#' @param quiet Logical. If TRUE, suppresses warnings
#' @return Parsed PX object
#' @export
fetch_px <- function(url, quiet = FALSE) {
  # Download the file
  temp_file <- tempfile(fileext = ".px")
  download.file(url, temp_file, mode = "wb", quiet = quiet)

  # Check if file has BOM
  con <- file(temp_file, "rb")
  first_bytes <- readBin(con, "raw", n = 3)
  close(con)

  has_bom <- length(first_bytes) >= 3 &&
    first_bytes[1] == as.raw(0xEF) &&
    first_bytes[2] == as.raw(0xBB) &&
    first_bytes[3] == as.raw(0xBF)

  # Based on our diagnostic results, choose the right encoding strategy
  if (has_bom) {
    # Files with BOM work best with UTF-16LE on the server
    if (!quiet) message("File has BOM, using UTF-16LE")
    tryCatch({
      return(pxR::read.px(temp_file, encoding = "UTF-16LE"))
    }, error = function(e) {
      if (!quiet) message("UTF-16LE failed, trying windows-1250")
      return(pxR::read.px(temp_file, encoding = "windows-1250"))
    })
  } else {
    # Files without BOM work best with UTF-8 on the server
    if (!quiet) message("File has no BOM, using UTF-8")
    tryCatch({
      return(pxR::read.px(temp_file, encoding = "UTF-8"))
    }, error = function(e) {
      if (!quiet) message("UTF-8 failed, trying windows-1250")
      return(pxR::read.px(temp_file, encoding = "windows-1250"))
    })
  }
}

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

  fetch_px(url)
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
  url <- BSfetchR::full |>
    dplyr::filter(px_code == !!px_code) |>
    dplyr::pull(px_url)
  data.frame(name = gsub('\"\n\"', "", unlist(l$CONTENTS$value)),
             updated = as.POSIXct(ifelse(!is.null(l$LAST.UPDATED$value),
                                         l$LAST.UPDATED$value,
                                         "19000101 00:00" ),
                                  format = "%Y%m%d %H:%M", tz = Sys.timezone()),
             units = l$UNITS$value,
             notes = I(list(c(l$NOTE$value, l$NOTEX$value))),
             valuenotes =I(list(l$VALUENOTE$value))) |>
    dplyr::mutate(notes = jsonlite::toJSON(notes),
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

#' Get interval ID from a PX file
#'
#' Extracts the interval ID (e.g., "M", "Q", "A") from a PX file based on its time value.
#'
#' @param px_code Character string identifying the PX file (e.g., "F2_A1S")
#' @return A character string representing the interval ID ("M", "Q", "A", or NA)
#' @export
get_interval_id_from_px <- function(px_code){
  l <- get_px_list(px_code)
  time_value <- l$VALUES$Datum[1]
 interval_id <- ifelse(grepl("[0-9]{4}M[0-9]{2}", time_value), "M",
                       ifelse(grepl("[0-9]{4}Q[0-9]{1}", time_value), "Q",
                                     ifelse(grepl("[0-9]{4}", time_value), "A", NA)))
  interval_id
}
