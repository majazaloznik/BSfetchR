test_that("extract_bsi_tables returns expected structure and content", {
  #skip("Live scraping test - only run manually when regenerating full dataset")
  result <- extract_bsi_tables()

  expect_s3_class(result, "data.frame")
  expect_named(result, c("name", "category_code", "full_url", "level"))
  expect_equal(nrow(result), 5)
  expect_true(all(!is.na(result$name)))
  expect_true(all(!is.na(result$category_code)))
  expect_true(all(grepl("^https://px.bsi.si", result$full_url)))
  expect_true(any(grepl("Finan\u010dn", result$name)))
})

test_that("extract_subcategory_urls returns expected structure and content", {
  #skip("Live scraping test - only run manually when regenerating full dataset")
  result <- extract_subcategory_urls("serije_slo__10_denar_mfi")

  expect_s3_class(result, "data.frame")
  expect_named(result, c("name", "url", "subcategory_code"))
  expect_true(nrow(result) > 0)
  expect_true(all(!is.na(result$name)))
  expect_true(all(!is.na(result$subcategory_code)))
  expect_true(all(grepl("^https://px.bsi.si", result$url)))
  expect_true(all(grepl("tablelist=true", result$url)))
  # all subcategory codes should belong to the requested category
  expect_true(all(grepl("10_denar_mfi__", result$url)))
})

test_that("extract_second_level_tables returns expected structure", {
  #skip("Live scraping test - only run manually when regenerating full dataset")
  result <- extract_second_level_tables(
    "https://px.bsi.si/pxweb/sl/serije_slo/serije_slo__20_obrestne_mere__10_OBR_MERE_BS/?tablelist=true"
  )

  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
  expect_true(all(c("name", "px_url", "modified_date") %in% names(result)))
  expect_true(all(!is.na(result$name)))
  expect_true(all(grepl("\\.px", result$px_url), na.rm = TRUE))
})

test_that("get_all_bsi_tables returns expected structure with IDs", {
  #skip("Live scraping test - only run manually when regenerating full dataset")
  result <- get_all_bsi_tables()

  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
  expect_true(all(c("type", "name", "category_code", "subcategory_code",
                    "px_code", "category_id", "subcategory_id",
                    "id", "parent_id") %in% names(result)))

  # type values are as expected
  expect_true(all(result$type %in% c("category", "subcategory", "data_table")))

  # every category has type = "category" and parent_id = 0
  cats <- result[result$type == "category", ]
  expect_true(all(cats$parent_id == 0))
  expect_true(all(!is.na(cats$category_id)))

  # subcategories have parent_id matching a category_id
  subcats <- result[result$type == "subcategory", ]
  expect_true(all(subcats$parent_id > 0))
  expect_true(all(subcats$parent_id %in% cats$category_id))

  # data tables have px_code and px_url
  tables <- result[result$type == "data_table", ]
  expect_true(all(!is.na(tables$px_code)))
  expect_true(all(grepl("^https://", tables$px_url[!is.na(tables$px_url)])))

  # all rows have an id
  expect_true(all(!is.na(result$id)))
})
