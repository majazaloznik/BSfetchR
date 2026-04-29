#' Extract data tables from Bank of Slovenia website
#'
#' This function retrieves the top-level structure of data tables
#' from the Bank of Slovenia statistical data portal.
#'
#' @param base_url The base URL of the Bank of Slovenia data portal
#' @return A data frame containing information about top-level categories and subcategories
#' @export
#' @examples
#' \dontrun{
#' top_level <- extract_bsi_tables()
#' }
extract_bsi_tables <- function(base_url = "https://px.bsi.si/pxweb/sl/serije_slo/") {
  html_content <- httr::GET(base_url) |>
    httr::content("text", encoding = "UTF-8") |>
    xml2::read_html()

  root_nodes <- rvest::html_nodes(html_content, ".AspNet-TreeView-Root")

  purrr::map_df(root_nodes, function(node) {
    name <- node |>
      rvest::html_node("a") |>
      rvest::html_text() |>
      trimws()

    onclick <- node |>
      rvest::html_node("span.AspNet-TreeView-Expand") |>
      rvest::html_attr("onclick")

    href <- node |>
      rvest::html_node("a") |>
      rvest::html_attr("href")

    category_code <- dplyr::coalesce(
      stringr::str_match(onclick, "'p(serije_slo__[^']+)'")[, 2],
      stringr::str_match(href, "(serije_slo__[^/]+)")[, 2]
    )

    full_url <- if (!is.na(category_code)) {
      paste0("https://px.bsi.si/pxweb/sl/serije_slo/", category_code, "/?tablelist=true")
    } else NA_character_

    tibble::tibble(name = name, category_code = category_code, full_url = full_url, level = 1L)
  })
}
#' Extract tables from second level pages
#'
#' This function retrieves the detailed table information from second-level pages
#' of the Bank of Slovenia statistical data portal.
#'
#' @param url The URL of the second-level page to extract tables from
#' @return A data frame containing information about tables on the page
#' @export
#' @examples
#' \dontrun{
#' tables <- extract_second_level_tables("https://px.bsi.si/pxweb/sl/serije_slo/serije_slo__40_financni_racuni/")
#' }
extract_second_level_tables <- function(url) {
  message("Fetching tables from: ", url)

  # Fetch HTML content with proper encoding
  response <- httr::GET(url)
  html_content <- httr::content(response, "text", encoding = "UTF-8") %>%
    xml2::read_html()

  # Extract parent category from breadcrumb
  parent_name <- html_content %>%
    rvest::html_nodes(".breadcrumb_text_nolink") %>%
    rvest::html_text() %>%
    tail(1) %>%
    trimws()

  # Extract table items from the page
  table_items <- html_content %>%
    rvest::html_nodes(".tablelist_li")

  # Initialize results dataframe
  tables <- data.frame(
    type = character(),
    name = character(),
    select_url = character(),
    px_url = character(),
    parent = character(),
    modified_date = character(),
    stringsAsFactors = FALSE
  )

  # Process each table item
  for (item in table_items) {
    # Extract table name
    table_name_node <- item %>%
      rvest::html_node(".tablelist_linkHeading")

    if (!is.na(table_name_node)) {
      table_name <- table_name_node %>%
        rvest::html_text() %>%
        trimws()

      # Extract select URL
      select_url_node <- item %>%
        rvest::html_node("a.tablelist_link[href*='select']")

      select_url <- if (!is.na(select_url_node)) {
        href <- rvest::html_attr(select_url_node, "href")
        paste0("https://px.bsi.si", href)
      } else {
        NA_character_
      }

      px_url_node <- item |>
        rvest::html_node("a.tablelist_link[href*='Resources']")

      px_url <- if (!is.na(px_url_node)) {
        href <- rvest::html_attr(px_url_node, "href")
        paste0("https://px.bsi.si/", stringr::str_remove(href, "^\\.\\./\\.\\./\\.\\./\\.\\./"))
      } else if (!is.na(select_url)) {
        path <- stringr::str_match(select_url, "serije_slo/serije_slo__([^/]+)/([^/]+)/")[, 2:3]
        if (!any(is.na(path))) {
          paste0("https://px.bsi.si/Resources/PX/Databases/serije_slo/",
                 stringr::str_replace(path[1], "__", "/"), "/", path[2])
        } else NA_character_
      } else {
        NA_character_
      }

      # Extract modified date
      modified_date <- item %>%
        rvest::html_text() %>%
        stringr::str_extract("Spremenjeno:\\s*(\\d{1,2}\\.\\d{1,2}\\.\\d{4})")

      # Add to results
      tables <- rbind(tables, data.frame(
        type = "data_table",
        name = table_name,
        select_url = select_url,
        px_url = px_url,
        parent = parent_name,
        modified_date = modified_date,
        stringsAsFactors = FALSE
      ))
    }
  }

  return(tables)
}


#' Extract subcategory URLs from a Bank of Slovenia category page
#'
#' @param category_code Character string identifying the category
#'   (e.g. "serije_slo__10_denar_mfi")
#' @return A data frame with columns:
#'   \item{name}{name of the subcategory}
#'   \item{url}{full URL to the subcategory tablelist page}
#'   \item{subcategory_code}{subcategory code extracted from URL}
#' @export
#' @examples
#' \dontrun{
#' extract_subcategory_urls("serije_slo__10_denar_mfi")
#' }
extract_subcategory_urls <- function(category_code) {
  url <- paste0("https://px.bsi.si/pxweb/sl/serije_slo/", category_code, "/")
  html <- httr::GET(url) |>
    httr::content("text", encoding = "UTF-8") |>
    xml2::read_html()

  links <- rvest::html_nodes(html, "a")
  hrefs <- rvest::html_attr(links, "href")
  texts <- trimws(rvest::html_text(links))

  is_subcat <- !is.na(hrefs) &
    grepl(paste0(category_code, "__"), hrefs) &
    grepl("tablelist=true", hrefs)

  tibble::tibble(
    name = texts[is_subcat],
    url = paste0("https://px.bsi.si", hrefs[is_subcat]),
    subcategory_code = stringr::str_match(
      hrefs[is_subcat],
      paste0(category_code, "__([^/]+)")
    )[, 2]
  )
}
#' Get all tables from the Bank of Slovenia data portal
#'
#' Retrieves the complete hierarchy of data tables from the Bank of Slovenia
#' statistical data portal by navigating three levels: top-level categories,
#' subcategories, and actual data tables.
#'
#' @param base_url The base URL of the Bank of Slovenia data portal
#' @return A data frame with table metadata and hierarchical IDs
#' @export
#' @examples
#' \dontrun{
#' all_tables <- get_all_bsi_tables()
#' }
#'
get_all_bsi_tables <- function(base_url = "https://px.bsi.si/pxweb/sl/serije_slo/") {
  top_level <- extract_bsi_tables(base_url)

  # category rows
  category_rows <- tibble::tibble(
    type = "category",
    name = top_level$name,
    px_url = NA_character_,
    modified_date = NA_character_,
    category_code = top_level$category_code,
    category_name = top_level$name,
    subcategory_code = NA_character_,
    subcategory_name = NA_character_,
    px_code = NA_character_
  )

  all_subcats <- list()
  all_tables <- list()

  for (i in seq_len(nrow(top_level))) {
    cat_code <- top_level$category_code[i]
    cat_name <- top_level$name[i]
    message(sprintf("Processing category %d of %d: %s", i, nrow(top_level), cat_name))
    Sys.sleep(0.5)

    subcats <- tryCatch(
      extract_subcategory_urls(cat_code),
      error = function(e) { message("Error getting subcategories: ", e$message); NULL }
    )

    if (is.null(subcats) || nrow(subcats) == 0) {
      # no subcategories - fetch tables directly
      tables <- tryCatch(
        extract_second_level_tables(top_level$full_url[i]) |>
          dplyr::mutate(
            category_code = cat_code,
            category_name = cat_name,
            subcategory_code = NA_character_,
            subcategory_name = NA_character_
          ),
        error = function(e) { message("Error: ", e$message); NULL }
      )
      if (!is.null(tables)) all_tables[[length(all_tables) + 1]] <- tables
    } else {
      # subcategory rows for this category
      all_subcats[[length(all_subcats) + 1]] <- tibble::tibble(
        type = "subcategory",
        name = subcats$name,
        px_url = NA_character_,
        modified_date = NA_character_,
        category_code = cat_code,
        category_name = cat_name,
        subcategory_code = subcats$subcategory_code,
        subcategory_name = subcats$name,
        px_code = NA_character_
      )

      for (j in seq_len(nrow(subcats))) {
        message(sprintf("  Subcategory %d of %d: %s", j, nrow(subcats), subcats$name[j]))
        Sys.sleep(0.3)
        tables <- tryCatch(
          extract_second_level_tables(subcats$url[j]) |>
            dplyr::mutate(
              category_code = cat_code,
              category_name = cat_name,
              subcategory_code = subcats$subcategory_code[j],
              subcategory_name = subcats$name[j]
            ),
          error = function(e) { message("Error: ", e$message); NULL }
        )
        if (!is.null(tables)) all_tables[[length(all_tables) + 1]] <- tables
      }
    }
  }

  subcat_rows <- if (length(all_subcats) > 0) dplyr::bind_rows(all_subcats) else NULL
  table_rows <- if (length(all_tables) > 0) {
    dplyr::bind_rows(all_tables) |> dplyr::mutate(type = "data_table")
  } else NULL

  category_id_map <- c(
    "serije_slo__10_denar_mfi" = 1L,
    "serije_slo__20_obrestne_mere" = 2L,
    "serije_slo__30_EOT" = 3L,
    "serije_slo__40_financni_racuni" = 4L,
    "serije_slo__90_devizni_tecaji" = 5L)

  subcategory_id_map <- c(
    "10_denarni_agregati" = 10L, "20_kons_bilanca_mfi" = 11L,
    "30_bilanca_bs" = 12L, "40_bilanca_mfi" = 13L,
    "60_terj_mfi" = 14L, "70_obvez_mfi" = 15L,
    "50_izbrani_pod_bilanc" = 16L, "80_kakovost_terj_bank" = 17L,
    "85_lizing" = 18L, "90_zav" = 19L, "95_spi" = 20L,
    "10_OBR_MERE_BS" = 21L, "30_OBR_MERE_DEN_TRG" = 22L,
    "40_OBR_MERE_KONV" = 23L, "20_OBR_MERE_MFI" = 24L,
    "50_OBR_MERE_MFI_PG" = 25L, "10_PLACILNA_BILANCA" = 26L,
    "12_PLACILNA_BILANCA_DRZ" = 27L, "15_BLAG_STOR" = 28L,
    "18_POTOVANJA" = 29L, "19_STORITV_MENJAVA" = 30L,
    "20_MEDN_NALOZBE" = 31L, "25_MEDN_NALOZBE_DRZ" = 32L,
    "30_MEDN_REZERVE" = 33L, "35_MEDN_REZERVE_DEV_LIKV" = 34L,
    "40_ZUNANJI_DOLG" = 35L, "50_NEPOSREDNE_NALOZBE" = 36L,
    "60_KAZALCI_POSLOVANJA" = 37L, "10_devizni_tecaji_tolarja" = 38L,
    "20_term_devizni_trg" = 39L)

  result <- dplyr::bind_rows(category_rows, subcat_rows, table_rows) |>
    dplyr::mutate(
      px_code = dplyr::if_else(
        is.na(px_code),
        stringr::str_match(px_url, stringr::regex("/([^/]+)\\.px", ignore_case = TRUE))[, 2],
        px_code
      ),
      modified_date = stringr::str_extract(modified_date, "\\d{1,2}\\.\\d{1,2}\\.\\d{4}"),
      category_id = unname(category_id_map[category_code]),
      subcategory_id = unname(subcategory_id_map[subcategory_code]),
      # new subcategories not in map get IDs starting after 39
      subcategory_id = dplyr::if_else(
        is.na(subcategory_id) & !is.na(subcategory_code),
        40L + match(subcategory_code, unique(subcategory_code[is.na(subcategory_id) & !is.na(subcategory_code)])) - 1L,
        subcategory_id),
      id = dplyr::coalesce(subcategory_id, category_id),
      parent_id = dplyr::if_else(!is.na(subcategory_id), category_id, 0L)) |>
    dplyr::select(-select_url, -parent)

  unknown_cats <- unique(result$category_code[is.na(result$category_id)])
  if (length(unknown_cats) > 0) {
    stop("Unknown category codes with no ID mapping: ",
         paste(unknown_cats, collapse = ", "),
         ". Update category_id_map and insert into database manually.")
  }
  result |>
    dplyr::mutate(
      category_id = as.integer(unname(category_id)),
      subcategory_id = as.integer(unname(subcategory_id)),
      id = as.integer(unname(id)),
      parent_id = as.integer(unname(parent_id))
    ) |>
    as.data.frame()
}
