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
  # Fetch HTML content with proper encoding
  response <- httr::GET(base_url)
  html_content <- httr::content(response, "text", encoding = "UTF-8")  %>%
    xml2::read_html()

  # Extract all root category nodes
  root_nodes <- html_content %>%
    rvest::html_nodes(".AspNet-TreeView-Root")

  # Initialize results dataframe
  top_level_items <- data.frame(
    type = character(),
    name = character(),
    url = character(),
    parent = character(),
    full_url = character(),
    level = integer(),
    stringsAsFactors = FALSE
  )

  # Process each root node to extract its structure
  for (root_node in root_nodes) {
    # Check if this is a direct link or a category with children
    direct_link <- root_node %>% rvest::html_node("a")
    category_node <- root_node %>% rvest::html_node(".AspNet-TreeView-ClickableNonLink")

    if (!is.na(category_node)) {
      # This is a category with children
      category_name <- category_node %>%
        rvest::html_text() %>%
        trimws()

      # Add category to results
      top_level_items <- rbind(top_level_items, data.frame(
        type = "category",
        name = category_name,
        url = NA_character_,
        parent = NA_character_,
        full_url = NA_character_,
        level = 1,
        stringsAsFactors = FALSE
      ))

      # Get all child links
      child_links <- root_node %>%
        rvest::html_nodes("li.AspNet-TreeView-Leaf a")

      # Process each child link
      for (link in child_links) {
        child_name <- link %>%
          rvest::html_node("span.tableofcontent_tablelistlink") %>%
          rvest::html_text() %>%
          trimws()

        child_url <- link %>% rvest::html_attr("href")

        # Add child to results
        top_level_items <- rbind(top_level_items, data.frame(
          type = "subcategory",
          name = child_name,
          url = child_url,
          parent = category_name,
          full_url = paste0("https://px.bsi.si", child_url),
          level = 1,
          stringsAsFactors = FALSE
        ))
      }
    } else if (!is.na(direct_link)) {
      # This is a direct link (both category and table)
      link_name <- direct_link %>%
        rvest::html_node("span.tableofcontent_tablelistlink") %>%
        rvest::html_text() %>%
        trimws()

      link_url <- direct_link %>% rvest::html_attr("href")

      # Add direct link to results
      top_level_items <- rbind(top_level_items, data.frame(
        type = "both",  # Both category and link
        name = link_name,
        url = link_url,
        parent = NA_character_,
        full_url = paste0("https://px.bsi.si", link_url),
        level = 1,
        stringsAsFactors = FALSE
      ))
    }
  }

  return(top_level_items)
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
        select_url_node %>%
          rvest::html_attr("href") %>%
          paste0("https://px.bsi.si", .)
      } else {
        NA_character_
      }

      # Extract px file URL
      px_url_node <- item %>%
        rvest::html_node("a.tablelist_link[href*='.px']")

      px_url <- if (!is.na(px_url_node)) {
        px_url_node %>%
          rvest::html_attr("href") %>%
          paste0("https://px.bsi.si", .)

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

#' Get all tables from the Bank of Slovenia data portal
#'
#' This function retrieves the complete hierarchy of data tables
#' from the Bank of Slovenia statistical data portal, including top-level
#' categories, subcategories, and actual data tables. It also extracts
#' category codes and creates a hierarchical ID structure.
#'
#' @param base_url The base URL of the Bank of Slovenia data portal
#' @return A data frame containing information about all categories and tables,
#'         including category codes, IDs, and parent-child relationships
#' @export
#' @examples
#' \dontrun{
#' all_tables <- get_all_bsi_tables()
#' }
get_all_bsi_tables <- function(base_url = "https://px.bsi.si/pxweb/sl/serije_slo/") {
  # Get top level structure
  top_level <- extract_bsi_tables(base_url)

  # Initialize results with top level
  all_tables <- top_level

  # Follow links to second level
  second_level_tables <- data.frame()

  # Get all links to follow (subcategories and direct links)
  links_to_follow <- top_level[top_level$type %in% c("subcategory", "both"), ]

  # Process each link
  for (i in 1:nrow(links_to_follow)) {
    url <- links_to_follow$full_url[i]
    parent_name <- links_to_follow$name[i]

    # Handle the changing rxid by removing it from URL comparison
    url_without_rxid <- gsub("\\?rxid=[^&]+", "?", url)

    message(sprintf("Processing link %d of %d: %s", i, nrow(links_to_follow), parent_name))

    tryCatch({
      # Get tables from this URL
      tables <- extract_second_level_tables(url)

      if (nrow(tables) > 0) {
        # Add parent information
        tables$top_parent <- parent_name
        tables$level <- 2

        # Add to results
        second_level_tables <- rbind(second_level_tables, tables)
      }
    }, error = function(e) {
      message(sprintf("Error processing %s: %s", url, e$message))
    })

    # Add a small delay to be nice to the server
    Sys.sleep(0.5)
  }

  # Combine results if we got any second level tables
  if (nrow(second_level_tables) > 0) {
    all_tables <- dplyr::bind_rows(all_tables, second_level_tables)
  }

  # Process categories and create IDs
  all_tables <- all_tables %>%
    dplyr::mutate(full_url = dplyr::coalesce(full_url, select_url)) %>%
    dplyr::select(-select_url) %>%
    dplyr::mutate(
      category_code = stringr::str_match(full_url, "serije_slo__(.*?)(?:__|/)")[,2],
      subcategory_code = stringr::str_match(full_url, ".*?__.*?__(.*?)/")[,2],
      px_code = stringr::str_match(full_url, "([^/]+)(?:\\.px)")[,2],
      category_code = ifelse(name == "Finan\u010dne institucije in trgi",
                             "10_denar_mfi",
                             ifelse(name == "Obrestne mere",
                                    "20_obrestne_mere",
                                    ifelse(name == "Ekonomski odnosi s tujino",
                                           "30_EOT",
                                           ifelse(name == "Finan\u010dni ra\u010duni",
                                                  "40_financni_racuni",
                                                  ifelse(name == "Devizni te\u010daji tolarja 1991 - 2006, devizni trg v Sloveniji",
                                                         "90_devizni_tecaji",
                                                         category_code)))))
    ) %>%
    dplyr::mutate(category_id = match(category_code, unique(category_code))) %>%
    dplyr::mutate(
      max_id1 = max(category_id, na.rm = TRUE),
      subcategory_id = dplyr::if_else(
        !is.na(subcategory_code),
        # leaving room for up to four subcategories
        match(subcategory_code, unique(na.omit(subcategory_code))) + max_id1 + 4,
        NA_integer_
      )
    ) %>%
    dplyr::select(-max_id1) %>%
    dplyr::mutate(id = ifelse(is.na(subcategory_id),
                              category_id,
                              subcategory_id),
                  parent_id = dplyr::if_else(is.na(subcategory_id),
                                             0,
                                             category_id)) %>%
    # fix px url cuz it's wrong
    dplyr::mutate(px_url = ifelse(!is.na(px_url),
                                  ifelse(is.na(subcategory_code),
                                         paste("https://px.bsi.si/Resources/PX/Databases/serije_slo",
                                               category_code,
                                               paste0(px_code, ".px"), sep = "/"),
                                         paste("https://px.bsi.si/Resources/PX/Databases/serije_slo",
                                               category_code, subcategory_code,
                                               paste0(px_code, ".px"), sep = "/")),
                                         NA_character_))

  return(all_tables)
}


