test_that("util_find_free_missing_code works", {
  skip_on_cran()
  skip_if_not_installed("hms")
  skip_if_not_installed("lubridate")

  # Numeric codes
  x <- c(777, 888, 999)
  expected_next_code <- 1000
  expect_equal(util_find_free_missing_code(x), as.character(expected_next_code))

  # Date-like missing codes reserve the day after the latest existing code.
  expect_equal(
    util_find_free_missing_code(c("2020-01-01", "2020-01-05")),
    "2020-01-06"
  )
  expect_equal(
    as.character(util_find_free_missing_code(c("2020-01-01", "abc"))),
    "2020-01-02"
  )

  # Time-like missing codes reserve one hour after the latest existing code.
  expect_equal(
    util_find_free_missing_code(c("09:00:00", "11:30:00")),
    "12:30:00"
  )

  # Mixed date/time inputs currently use the datetime fallback branch.
  fallback_code <- util_find_free_missing_code(c("2020-01-01", "11:30:00"))
  expect_s3_class(fallback_code, "POSIXct")
  expect_equal(
    format(fallback_code, "%Y-%m-%d", tz = "Europe/Berlin"),
    "2020-01-02"
  )

  # All values cannot be converted
  x <- c("abc", "def", NA, "ghi")
  expect_error(util_find_free_missing_code(x))
})
