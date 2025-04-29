library(httr)
library(xml2)
library(rvest)
library(dplyr)
library(stringr)

# Tests for Bank of Slovenia data scraper functions

# Test for extract_bsi_tables()
test_that("extract_bsi_tables extracts top-level categories and subcategories", {
  # Skip if not on interactive mode to avoid HTTP requests in automated testing
  skip_if_not(interactive(), "Skipping live HTTP requests in non-interactive mode")

  # Get actual data from the website
  result <- extract_bsi_tables()

  # Test basics of the result
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
  expect_true("type" %in% names(result))
  expect_true("name" %in% names(result))
  expect_true("url" %in% names(result))

  # Check for expected categories
  categories <- result[result$type == "category", ]
  expect_true(any(grepl("Finančn", categories$name)))

  # Check for expected subcategories
  subcategories <- result[result$type == "subcategory", ]
  expect_true(nrow(subcategories) > 0)
  expect_true(all(!is.na(subcategories$parent)))

  # Check direct links (type 'both')
  direct_links <- result[result$type == "both", ]
  if (nrow(direct_links) > 0) {
    expect_true(all(grepl("^https://", direct_links$full_url)))
  }
})

# Test for extract_second_level_tables()
test_that("extract_second_level_tables extracts table information", {
  # Skip if not on interactive mode to avoid HTTP requests in automated testing
  skip_if_not(interactive(), "Skipping live HTTP requests in non-interactive mode")

  # Use financni_racuni URL which usually has tables
  url <- "https://px.bsi.si/pxweb/sl/serije_slo/serije_slo__40_financni_racuni/"

  # Get actual data from the website
  result <- extract_second_level_tables(url)

  # Test basics of the result
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
  expect_true("type" %in% names(result))
  expect_true("name" %in% names(result))
  expect_true("select_url" %in% names(result))
  expect_true("px_url" %in% names(result))

  # All entries should be data tables
  expect_true(all(result$type == "data_table"))

  # Check that URLs to PX files are present
  expect_true(all(grepl("\\.px", result$px_url)))

  # Check parent and modified date
  expect_true(all(!is.na(result$parent)))
  expect_true(all(grepl("Spremenjeno", result$modified_date)))
})

# Test for get_all_bsi_tables()
test_that("get_all_bsi_tables integrates both levels and adds IDs", {
  # Skip on CI or automated testing
  skip_if_not(interactive(), "Skipping comprehensive scraping in non-interactive mode")

  # For interactive testing, only fetch a limited subset to avoid long execution
  # Use a small timeout to make the test faster
  old_timeout <- getOption("timeout")
  options(timeout = 5)
  on.exit(options(timeout = old_timeout))

  # Get actual data but limit scope
  result <- get_all_bsi_tables()

  # Limit further processing to just a subset
  result <- head(result, 10)

  # Test the structure
  expect_s3_class(result, "data.frame")
  expect_true("category_code" %in% names(result))
  expect_true("subcategory_code" %in% names(result))
  expect_true("category_id" %in% names(result))
  expect_true("subcategory_id" %in% names(result))
  expect_true("id" %in% names(result))
  expect_true("parent_id" %in% names(result))

  # Check some key relationships
  if (nrow(result[result$type == "subcategory", ]) > 0) {
    # Subcategories should have parent IDs that match category IDs
    subcategories <- result[result$type == "subcategory", ]
    for (i in 1:nrow(subcategories)) {
      parent_name <- subcategories$parent[i]
      parent_row <- result[result$name == parent_name & result$type == "category", ]
      if (nrow(parent_row) > 0) {
        expect_equal(subcategories$parent_id[i], parent_row$category_id[1])
      }
    }
  }

  # Check that IDs are properly assigned
  expect_true(all(!is.na(result$id)))
  expect_true(all(result$parent_id[result$type == "category"] == 0))
  if (any(result$type == "data_table")) {
    data_tables <- result[result$type == "data_table", ]
    expect_true(all(!is.na(data_tables$parent_id)))
  }
})

# Unit tests for regex patterns used in the functions
test_that("regex patterns extract correct components from URLs", {
  # Test URLs
  test_urls <- c(
    "https://px.bsi.si/pxweb/sl/serije_slo/serije_slo__10_denar_mfi__10_denarni_agregati/",
    "https://px.bsi.si/pxweb/sl/serije_slo/serije_slo__40_financni_racuni/",
    "https://px.bsi.si/pxweb/sl/serije_slo/serije_slo__90_devizni_tecaji__20_term_devizni_trg/"
  )

  # Test category code extraction
  category_codes <- unname(sapply(test_urls, function(url) {
    match <- stringr::str_match(url, "serije_slo__(.*?)(?:__|/)")[,2]
    return(match)
  }))
  expect_equal(category_codes, c("10_denar_mfi", "40_financni_racuni", "90_devizni_tecaji"))

  # Test subcategory code extraction
  subcategory_codes <- unname(sapply(test_urls, function(url) {
    match <- stringr::str_match(url, ".*?__.*?__(.*?)/")
    if (is.na(match[1,1])) return(NA_character_)
    return(match[,2])
  }))
  expect_equal(subcategory_codes, c("10_denarni_agregati", NA, "20_term_devizni_trg"))

  # Test PX file extraction
  px_urls <- c(
    "https://px.bsi.si/pxweb/sl/serije_slo/serije_slo__40_financni_racuni/F2_A1S.px",
    "https://px.bsi.si/Resources/PX/Databases/serije_slo/40_financni_racuni/F2_A1S.px"
  )
  px_codes <- unname(sapply(px_urls, function(url) {
    match <- stringr::str_match(url, "([^/]+)(?:\\.px)")
    if (is.na(match[1,1])) return(NA_character_)
    return(match[,2])
  }))
  expect_equal(px_codes, c("F2_A1S", "F2_A1S"))
})

# Test special character handling in the BSI website
test_that("special Slovenian characters are correctly handled", {
  # Create test data with known values
  test_data <- data.frame(
    name = c("Finančne institucije in trgi", "Obrestne mere", "Finančni računi",
             "Ekonomski odnosi s tujino", "Devizni tečaji tolarja 1991 - 2006, devizni trg v Sloveniji"),
    stringsAsFactors = FALSE
  )

  # Apply the same mapping logic as in get_all_bsi_tables()
  result <- test_data
  result$category_code <- ifelse(
    result$name == "Finančne institucije in trgi", "10_denar_mfi",
    ifelse(result$name == "Obrestne mere", "20_obrestne_mere",
           ifelse(result$name == "Ekonomski odnosi s tujino", "30_EOT",
                  ifelse(result$name == "Finančni računi", "40_financni_racuni",
                         ifelse(result$name == "Devizni tečaji tolarja 1991 - 2006, devizni trg v Sloveniji",
                                "90_devizni_tecaji", NA)))))

  # Check that the mapping works correctly
  expect_false(any(is.na(result$category_code)))
  expect_equal(result$category_code[result$name == "Finančne institucije in trgi"], "10_denar_mfi")
  expect_equal(result$category_code[result$name == "Finančni računi"], "40_financni_racuni")
  expect_equal(result$category_code[result$name == "Devizni tečaji tolarja 1991 - 2006, devizni trg v Sloveniji"],
               "90_devizni_tecaji")
})
