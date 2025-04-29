library(httr)
library(xml2)
library(rvest)
library(dplyr)
library(stringr)

# Tests for Bank of Slovenia data scraper functions

# Test for extract_bsi_tables()
test_that("extract_bsi_tables extracts top-level categories and subcategories", {
  # Skip if not on interactive mode to avoid HTTP requests in automated testing

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
  expect_true(any(grepl("Finan\u010dn", categories$name)))

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


# Test for get_all_bsi_tables()
test_that("get_all_bsi_tables integrates both levels and adds IDs", {

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
