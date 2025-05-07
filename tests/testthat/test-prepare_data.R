test_that("mock tests table prep without db access", {
  dittodb::with_mock_db({
    con <- make_test_connection()
    x <- prepare_vintage_table("F2_Q1S", con, schema = "test_platform")
    expect_true(all(dim(x) == c(1080,2)))
    expect_true(all(names(x) == c("series_id", "published")))
    x <- prepare_bs_data_for_insert("F2_Q1S", con, "test_platform")
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
