test_that("util_parse_time keeps existing time-like classes", {
  skip_on_cran()

  hms_value <- hms::hms(hours = 1, minutes = 2, seconds = 3)
  expect_identical(util_parse_time(hms_value), hms_value)

  difftime_value <- as.difftime(90, units = "secs")
  expect_s3_class(util_parse_time(difftime_value), "hms")
  expect_equal(as.numeric(util_parse_time(difftime_value)), 90)

  posix_value <- as.POSIXct("2020-01-01 01:02:03", tz = "UTC")
  expect_s3_class(util_parse_time(posix_value), "hms")
  expect_equal(as.numeric(util_parse_time(posix_value)), 3723)
})

test_that("util_parse_time parses numeric and list-like time values", {
  skip_on_cran()

  excel_fraction <- util_parse_time(c(0.5, 0.25))
  expect_s3_class(excel_fraction, "hms")
  expect_equal(as.numeric(excel_fraction), c(43200, 21600))

  hhmmss_value <- util_parse_time(123045)
  expect_s3_class(hhmmss_value, "hms")
  expect_equal(as.numeric(hhmmss_value), 45045)

  parsed_list <- util_parse_time(list("01:02:03", "02:03"))
  expect_length(parsed_list, 2)
  expect_true(all(vapply(parsed_list, inherits, logical(1), "hms")))
  expect_equal(vapply(parsed_list, as.numeric, numeric(1)), c(3723, 7380))
})

test_that("util_parse_time parses fallback numeric and factor values", {
  skip_on_cran()

  epoch_seconds <- util_parse_time(86400, tz = "UTC")
  expect_s3_class(epoch_seconds, "hms")
  expect_equal(as.numeric(epoch_seconds), 0)

  factor_time <- util_parse_time(factor("03:04"))
  expect_s3_class(factor_time, "hms")
  expect_equal(as.numeric(factor_time), 11040)
})

test_that("util_parse_time warns for date-time and invalid time strings", {
  skip_on_cran()

  expect_warning(
    full_datetime <- util_parse_time("2020-01-01 12:00:00"),
    "not pure times"
  )
  expect_true(is.na(as.numeric(full_datetime)))

  expect_warning(
    invalid_time <- util_parse_time(c("09:00", "not a time")),
    "Failed to parse"
  )
  expect_equal(as.numeric(invalid_time), c(32400, NA))

  expect_error(
    util_parse_time("not a time", optional = FALSE),
    "Failed to parse"
  )
})
