test_that("BS_import_structure works correctly with mocked dimension_selector", {
  with_mock_db({
    con <- make_test_connection()
    result <- BS_import_structure("I2_4_4AS", con, schema = "platform")
    expect_true("dimension_levels" %in% names(result))
    expect_true(is.list(result$dimension_levels))
    expect_true("count" %in% names(result$dimension_levels))
  })
})

test_that("BS_import_data works correctly", {
  withr::with_timezone("UTC", {
    with_mock_db({
      con <- make_test_connection()
      result <- BS_import_data_points("I2_4_4AS", con, schema = "platform")
      expect_true(all(names(result) == c("vintages", "data")))
      expect_true(all(names(result$data) == c("periods_inserted", "datapoints_inserted",
                                              "flags_inserted")))
      print(result$data$datapoints_inserted)
      expect_true(result$data$datapoints_inserted == 28356)
    })
  })
})
