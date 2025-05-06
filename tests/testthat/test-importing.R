test_that("BS_import_structure works correctly with mocked dimension_selector", {
  with_mock_db({
    con <- make_test_connection()
    result <- BS_import_structure("F2_Q1S", con, schema = "test_platform")
    expect_true("dimension_levels" %in% names(result))
    expect_true(is.list(result$dimension_levels))
    expect_true("count" %in% names(result$dimension_levels))
  })
})
