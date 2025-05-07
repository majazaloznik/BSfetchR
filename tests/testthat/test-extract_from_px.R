

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

# tests/testthat/test-set_data_locale.R

test_that("set_data_locale changes locale and returns function to restore it", {
  # Save original locale
  original_locale <- Sys.getlocale("LC_CTYPE")

  # Set to UTF-8
  restore_fn <- UMARimportR::set_data_locale("UTF-8")

  # Confirm locale changed
  current_locale <- Sys.getlocale("LC_CTYPE")
  expect_false(identical(current_locale, original_locale),
               "Locale should change from original")

  # Restore original
  restore_fn()

  # Verify restoration
  expect_identical(Sys.getlocale("LC_CTYPE"), original_locale)
})

test_that("set_data_locale handles different locale categories", {
  # Test with LC_TIME
  original_time <- Sys.getlocale("LC_TIME")

  # Set only LC_TIME to C
  restore_time <- UMARimportR::set_data_locale("C", "LC_TIME")
  expect_equal(Sys.getlocale("LC_TIME"), "C")

  # Restore
  restore_time()
  expect_equal(Sys.getlocale("LC_TIME"), original_time)
})

# tests/testthat/test-read_bsi_px.R

test_that("read_bsi_px successfully reads valid BSI PX URL", {
  skip_if_offline()

  # Use a known working BSI URL
  url <- "https://px.bsi.si/Resources/PX/Databases/serije_slo/10_denar_mfi/60_terj_mfi/I1_5BBS.px"

  # Should return a valid px object
  result <- BSfetchR::read_bsi_px(url)

  # Check that we got a valid object
  expect_true(!is.null(result))
  expect_true(is.list(result))
  expect_true("HEADING" %in% names(result))
  expect_true("DATA" %in% names(result))
})

test_that("read_bsi_px handles non-existent URLs appropriately", {
  skip_if_offline()

  # Use a URL that doesn't exist
  url <- "https://px.bsi.si/Resources/PX/NonExistentFile.px"

  # Should throw an error for non-existent URL
  expect_error(BSfetchR::read_bsi_px(url))
})
