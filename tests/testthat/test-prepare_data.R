test_that("mock tests table prep without db access", {
  dittodb::with_mock_db({
    con <- make_test_connection()
    x <- prepare_vintage_table("F2_Q1S", con, schema = "platform")
    expect_true(all(dim(x) == c(1080,2)))
    expect_true(all(names(x) == c("series_id", "published")))
    x <- prepare_bs_data_for_insert("F2_Q1S", con, "platform")
    expect_true(is.list(x))
    expect_true(length(x) == 6)
    expect_true(all(names(x) == c("data", "table_id", "time_dimension",
                                  "interval_id", "dimension_ids", "dimension_names")))
    expect_true(all(names(x$data) == c("SEKTOR", "TIP_POD", "VRSTA", "FIN_INSTRUM",
                                       "Postavke", "Datum",
                                       "value", "time", "flag", "interval_id")))
    expect_true(is.character(x$data$SEKTOR))
    expect_true(all(x$data$flag == ""))
  })
})
test_that("prepare_vintage_table raises bs_not_new when publication date is unchanged", {
  ts <- as.POSIXct("2026-01-15 10:00:00", tz = "UTC")
  local_mocked_bindings(get_px_metadata = function(...) data.frame(updated = ts))
  local_mocked_bindings(
    sql_get_table_id_from_table_code = function(...) 1,
    sql_get_last_publication_date_from_table_id = function(...) ts,
    .package = "UMARaccessR")
  expect_error(prepare_vintage_table("X", con = NULL), class = "bs_not_new")
})
