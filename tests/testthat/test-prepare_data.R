test_that("mock tests table prep without db access", {
  dittodb::with_mock_db({
    con <- make_test_connection()
    x <- prepare_vintage_table("I1_1S", con, schema = "test_platform")
    expect_true(all(dim(x) == c(28,2)))
    expect_true(all(names(x) == c("series_id", "published")))
    # x <- prepare_surs_data_for_insert("H240S", con, "test_platform")
    # expect_true(is.list(x))
    # expect_true(length(x) == 6)
    # expect_true(all(names(x) == c("data", "table_id", "time_dimension",
    #                               "interval_id", "dimension_ids", "dimension_names")))
    # expect_true(all(names(x$data) == c("LETO", "TRANSAKCIJA", "value", "SEKTOR",
    #                                    "raw_time", "time", "flag", "interval_id")))

  })
})
