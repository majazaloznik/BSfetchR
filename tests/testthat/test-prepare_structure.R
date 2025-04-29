test_that("prepare source table", {
  dittodb::with_mock_db({
    con <- make_connection()
    expect_message(prepare_source_table(con = con))
    con_test <- make_test_connection()

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
    expect_equal(ncol(out), 4)
    expect_equal(names(out), c("id", "name", "parent_id", "source_id"))
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

test_that("prepare category relationship table", {
  dittodb::with_mock_db({
    con_test <- make_test_connection()
    out <- prepare_category_table_table("i_36_6as", con_test, "test_platform")
    expect_equal(nrow(out), 1)
    expect_equal(ncol(out), 3)
    expect_equal(names(out), c("category_id", "table_id", "source_id"))
    expect_equal(out$source_id[1], 5)
    expect_equal(out$category_id[1], 31)
    expect_equal(out$table_id[1], 368)
  })
})

test_that("prepare table dinemsions table", {
  dittodb::with_mock_db({
    con_test <- make_test_connection()
    out <- prepare_table_dimensions_table("F2_Q1S", con_test, "test_platform")
    expect_equal(nrow(out), 6)
    expect_equal(ncol(out), 3)
    expect_equal(names(out), c("table_id", "dimension", "is_time"))
    expect_equal(out$dimension[1], "Datum")
    expect_equal(out$is_time[1], TRUE )
    expect_equal(out$dimension[2], "SEKTOR")
    expect_equal(out$is_time[2], FALSE )
  })
})

