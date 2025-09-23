test_that("Debug SQL differences", {
  with_mock_db({
    con <- make_test_connection()

    # Capture what SQL is actually being generated
    dittodb::start_db_capturing()

    # This will fail but show us the actual SQL being generated
    tryCatch({
      result <- BS_import_structure("F2_Q1S", con, schema = "platform")
    }, error = function(e) {
      message("Error: ", e$message)
    })

    dittodb::stop_db_capturing()
  })
})

test_that("BS_import_structure works correctly with mocked dimension_selector", {
  with_mock_db({
    con <- make_test_connection()
    result <- BS_import_structure("F2_Q1S", con, schema = "platform")
    expect_true("dimension_levels" %in% names(result))
    expect_true(is.list(result$dimension_levels))
    expect_true("count" %in% names(result$dimension_levels))
  })
})

test_that("BS_import_data works correctly", {
  with_mock_db({
    con <- make_test_connection()
    result <- BS_import_data_points("F2_Q1S", con, schema = "platform")
    expect_true(all(names(result) == c("vintages", "data")))
    expect_true(all(names(result$data) == c("periods_inserted", "datapoints_inserted",
                                              "flags_inserted")))
    expect_true(result$data$datapoints_inserted == 91800)
  })
})
