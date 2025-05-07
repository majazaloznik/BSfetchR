test_that("BS_import_structure works correctly with mocked dimension_selector", {
  with_mock_db({
    con <- make_test_connection()
    result <- BS_import_structure("F2_Q1S", con, schema = "test_platform")
    expect_true("dimension_levels" %in% names(result))
    expect_true(is.list(result$dimension_levels))
    expect_true("count" %in% names(result$dimension_levels))
  })
})

test_that("BS_import_data works correctly", {
  with_mock_db({
    con <- make_test_connection()
    result <- BS_import_data_points("F2_Q1S", con, schema = "test_platform")
    expect_true(all(names(result) == c("vintages", "data")))
    expect_true(all(names(result$data) == c("periods_inserted", "datapoints_inserted",
                                              "flags_inserted")))
    expect_true(result$data$datapoints_inserted == 90720)
  })
})
