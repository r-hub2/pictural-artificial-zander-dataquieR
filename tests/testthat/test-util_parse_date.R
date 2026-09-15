test_that("util_parse_date handles explicit formats and numeric input", {
  skip_on_cran()
  withr::local_timezone("UTC")

  numeric_date <- util_parse_date(0, tz = "UTC")
  expect_s3_class(numeric_date, "POSIXct")
  expect_equal(as.numeric(numeric_date), 0)

  formatted_date <- util_parse_date("02.01.2020",
    tryFormats = "%d.%m.%Y",
    tz = "UTC")
  expect_equal(format(formatted_date, "%Y-%m-%d %H:%M:%S %Z"),
    "2020-01-02 00:00:00 UTC")

  fallback_datetime <- util_parse_date("2020/01/02T03:04", tz = "UTC")
  expect_equal(format(fallback_datetime, "%Y-%m-%d %H:%M:%S %Z"),
    "2020-01-02 03:04:00 UTC")
})

test_that("util_parse_date rejects invalid required dates", {
  skip_on_cran()
  withr::local_timezone("UTC")

  expect_error(util_parse_date("not-a-date", optional = FALSE, tz = "UTC"))
})
