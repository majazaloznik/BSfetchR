test_that("prepare source table", {
  dittodb::with_mock_db({
    con <- make_connection()
    expect_message(prepare_source_table(con = con))
    # con_test <- make_test_connection()
    # out <- prepare_source_table(con = con_test, schema = "platform")
    # expect_equal(nrow(out), 1)
    # expect_equal(out$id, 5)
  })
})

test_that("prepare table table", {
  dittodb::with_mock_db({
    con <- make_connection()
    out <- prepare_table_table("i_36_6as", con = con)
    expect_equal(nrow(out), 1)
    expect_equal(ncol(out), 6)
    expect_equal(names(out), c("name", "notes", "source_id", "url", "code", "keep_vintage"))
  })
})

test_that("prepare category table", {
  dittodb::with_mock_db({
    con <- make_connection()
    out <- prepare_category_table("i_36_6as", con = con)
    expect_equal(nrow(out), 2)
    expect_equal(ncol(out), 3)
    expect_equal(names(out), c("id", "name",  "source_id"))
    expect_equal(out$source_id[1], 5)
  })
})

test_that("prepare category relationship table", {
  dittodb::with_mock_db({
    con <- make_connection()
    out <- prepare_category_relationship_table("i_36_6as", con)
    expect_equal(nrow(out), 2)
    expect_equal(ncol(out), 3)
    expect_equal(names(out), c("id", "parent_id", "source_id"))
    expect_equal(out$source_id[1], 5)
  })
})

test_that("prepare category table table", {
  dittodb::with_mock_db({
    con <- make_connection()
    out <- prepare_category_table_table("i_32_6ms", con, "platform")
    expect_equal(nrow(out), 1)
    expect_equal(ncol(out), 3)
    expect_equal(names(out), c("category_id", "table_id", "source_id"))
    expect_equal(out$source_id[1], 5)
    expect_equal(out$category_id[1], 26)
    expect_equal(out$table_id[1], 257)
  })
})

test_that("prepare table dinemsions table", {
  dittodb::with_mock_db({
    con <- make_connection()
    out <- prepare_table_dimensions_table("F2_Q1S", con, "test_platform")
    expect_equal(nrow(out), 6)
    expect_equal(ncol(out), 3)
    expect_equal(names(out), c("table_id", "dimension", "is_time"))
    expect_equal(out$dimension[1], "Datum")
    expect_equal(out$is_time[1], TRUE )
    expect_equal(out$dimension[2], "SEKTOR")
    expect_equal(out$is_time[2], FALSE )
  })
})

test_that("prepare dimensions levels table", {
  dittodb::with_mock_db({
    con  <- make_connection()
    out <- prepare_dimension_levels_table("F2_Q1S", con, "test_platform")
    expect_equal(nrow(out), 30)
    expect_equal(ncol(out), 4)
    expect_equal(names(out), c("dimension", "level_text", "level_value", "tab_dim_id"))
    expect_true(any(is.na(out)) == FALSE)
  })
})

test_that("prepare unit table", {
  dittodb::with_mock_db({
    con_test <- make_test_connection()
    out <- prepare_unit_table("I1_1S")
    expect_equal(nrow(out), 1)
    expect_equal(ncol(out), 1)
    expect_equal(names(out), c("name"))
    expect_true(any(is.na(out)) == FALSE)
  })
})

test_that("prepare series table", {
  dittodb::with_mock_db({
    con  <- make_connection()
    out <- prepare_series_table("I1_4PS", con, "platform")
    expect_equal(nrow(out), 17)
    expect_equal(ncol(out), 5)
    expect_equal(names(out), c("table_id", "name_long", "unit_id", "code", "interval_id"))
    expect_true(any(is.na(out)) == FALSE)
  })
})


test_that("prepare series levels table", {
  dittodb::with_mock_db({
    con  <- make_connection()
    out <- prepare_series_levels_table("I1_4PS", con, "platform")
    expect_equal(nrow(out), 51)
    expect_equal(ncol(out), 3)
    expect_equal(names(out), c("series_id", "tab_dim_id", "level_value"))
    expect_true(any(is.na(out)) == FALSE)
  })
})











