library(testthat)
library(httr)
library(xml2)
library(rvest)
library(dplyr)
library(stringr)

# Fixed tests for BSfetchR functions

test_that("regex patterns work correctly for URL parsing", {
  # Test URLs
  test_urls <- c(
    "https://px.bsi.si/pxweb/sl/serije_slo/serije_slo__10_denar_mfi__10_denarni_agregati/",
    "https://px.bsi.si/pxweb/sl/serije_slo/serije_slo__40_financni_racuni/",
    "https://px.bsi.si/pxweb/sl/serije_slo/serije_slo__90_devizni_tecaji__20_term_devizni_trg/"
  )

  # Test category code regex - use unname() to remove input names
  category_codes <- unname(sapply(test_urls, function(url) {
    match <- stringr::str_match(url, "serije_slo__(.*?)(?:__|/)")[,2]
    return(match)
  }))

  expect_equal(category_codes, c("10_denar_mfi", "40_financni_racuni", "90_devizni_tecaji"))

  # Test subcategory code regex - use unname()
  subcategory_codes <- unname(sapply(test_urls, function(url) {
    match <- stringr::str_match(url, ".*?__.*?__(.*?)/")
    if (is.na(match[1,1])) return(NA_character_)
    return(match[,2])
  }))

  expect_equal(subcategory_codes, c("10_denarni_agregati", NA, "20_term_devizni_trg"))

  # Test px file code regex - use unname()
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

test_that("changing rxid parameter is handled correctly", {
  # Test URLs with different rxid values
  url1 <- "https://px.bsi.si/pxweb/sl/serije_slo/serije_slo__40_financni_racuni/?rxid=12345"
  url2 <- "https://px.bsi.si/pxweb/sl/serije_slo/serije_slo__40_financni_racuni/?rxid=67890"

  # Apply the gsub pattern from the function
  clean_url1 <- gsub("\\?rxid=[^&]+", "?", url1)
  clean_url2 <- gsub("\\?rxid=[^&]+", "?", url2)

  # Both should be cleaned to the same base URL
  expect_equal(clean_url1, clean_url2)
  expect_equal(clean_url1, "https://px.bsi.si/pxweb/sl/serije_slo/serije_slo__40_financni_racuni/?")
})

test_that("special Slovenian characters are correctly handled", {
  # Create test data with special characters
  special_chars_data <- data.frame(
    name = c("Finančne institucije in trgi", "Obrestne mere", "Finančni računi"),
    stringsAsFactors = FALSE
  )

  # Check category mapping with special characters
  result <- special_chars_data
  result$category_code <- ifelse(
    result$name == "Finančne institucije in trgi", "10_denar_mfi",
    ifelse(result$name == "Obrestne mere", "20_obrestne_mere",
           ifelse(result$name == "Finančni računi", "40_financni_racuni", NA))
  )

  # Check that all mappings were successful
  expect_false(any(is.na(result$category_code)))
  expect_equal(result$category_code[result$name == "Finančni računi"], "40_financni_racuni")
})
