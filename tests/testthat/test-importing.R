test_that("BS_import_structure works correctly with mocked dimension_selector", {
  with_mock_db({
    con <- make_test_connection()
    result <- BS_import_structure("I2_4_4AS", con, schema = "platform")
    expect_true("dimension_levels" %in% names(result))
    expect_true(is.list(result$dimension_levels))
    expect_true("count" %in% names(result$dimension_levels))
  })
})

test_that("BS_import_data_points imports new data and returns counts", {
  withr::with_timezone("UTC", {
    with_mock_db({
      con <- make_test_connection()
      result <- BS_import_data_points("I2_4_4AS", con, schema = "platform")
      expect_named(result, c("new_vintages", "new_datapoints"))
      expect_gt(result$new_vintages, 0)
      expect_equal(result$new_datapoints, 29070)
    })
  })
})

test_that("BS_import_data_points returns zero counts when nothing is new", {
  inserted <- FALSE
  local_mocked_bindings(
    prepare_vintage_table = function(...) stop(structure(
      class = c("bs_not_new", "error", "condition"),
      list(message = "not new", call = NULL))))
  local_mocked_bindings(
    insert_new_vintage = function(...) { inserted <<- TRUE; data.frame(count = 1L) },
    .package = "UMARimportR")
  result <- BS_import_data_points("X", con = NULL)
  expect_equal(result, list(new_vintages = 0L, new_datapoints = 0L))
  expect_false(inserted)
})

test_that("BS_import_data_points raises other vintage errors", {
  local_mocked_bindings(prepare_vintage_table = function(...) stop("BS is down"))
  expect_error(BS_import_data_points("X", con = NULL), "BS is down")
})

test_that("BS_import_data_points removes empty vintages if data import fails", {
  removed <- FALSE
  local_mocked_bindings(
    prepare_vintage_table = function(...) data.frame(series_id = 1:2, published = Sys.time()),
    prepare_bs_data_for_insert = function(...) stop("boom"))
  local_mocked_bindings(
    insert_new_vintage = function(...) data.frame(count = 2L),
    remove_empty_vintages = function(...) { removed <<- TRUE; invisible(NULL) },
    .package = "UMARimportR")
  expect_error(BS_import_data_points("X", con = NULL),
               "X: data import failed \\(boom\\); empty vintages removed")
  expect_true(removed)
})
