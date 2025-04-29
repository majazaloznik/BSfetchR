test_that("prepare source table", {
  dittodb::with_mock_db({
    con <- make_connection()
    expect_message(prepare_source_table(con = con))
    con_test <- make_test_connection()
    out <- prepare_source_table(con = con_test, schema = "test_platform")
    expect_equal(nrow(out), 1)
    expect_equal(ncol(out), 4)
    expect_equal(names(out), c("id", "name", "name_long", "url"))
  })
})
test_that("prepare table table", {
  dittodb::with_mock_db({
    con <- make_connection()
    out <- prepare_table_table("i_36_6as", con = con)
    expect_equal(nrow(out), 1)
    expect_equal(ncol(out), 5)
    expect_equal(names(out), c("name", "notes", "source_id", "url", "keep_vintage"))
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
