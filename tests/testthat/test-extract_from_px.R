

test_that("prepare source table", {
  dittodb::with_mock_db({
    result <- get_px_list("i_36_6as")
    expect_s3_class(result, "px")
    expect_true(length(result) == 22)
    expect_true(all(c("VALUES", "DATA", "CONTENTS") %in% names(result)))
    expect_equal(result$UNITS$value, "mio EUR")
  })
})


# Test for get_px_metadata
test_that("get_px_metadata extracts metadata correctly", {
    result <- get_px_metadata("i_36_6as")
    # Check result structure
    expect_s3_class(result, "data.frame")
    expect_equal(ncol(result), 6)
    expect_equal(nrow(result), 1)
    # Check specific fields
    expect_equal(result$name, "Stanje mednarodnih nalo\u017eb BPM6 - mio EUR - letno")
    expect_s3_class(result$updated, "POSIXct")
    expect_equal(result$units, "mio EUR")
})

# Test for get_px_dim_levels
test_that("get_px_dim_levels returns dimension levels", {
  # Mock dependencies
  dittodb::with_mock_db({
    con <- make_connection()
      # Call the function
    result <- get_px_dim_levels("i_36_6as", con)

      # Check result
      expect_type(result, "list")
      expect_named(result, c("Datum", "Postavke"))
      expect_true(all(c("1994", "1995") %in% result$Datum))
      expect_true(all(c("N. NETO STANJE MEDNARODNIH NALOŽB (Imetja - Obveznosti)") %in% result$Postavke))
    })
})

# Test for get_px_data
test_that("get_px_data returns data values", {
  # Mock dependencies
  dittodb::with_mock_db({
    con <- make_connection()
      # Call the function
    result <- get_px_data("i_36_6as", con)
    expect_s3_class(result, "data.frame")
    expect_equal(ncol(result), 3)
    })
})


