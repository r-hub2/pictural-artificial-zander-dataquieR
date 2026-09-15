skip_on_cran()

test_that("util_data_type_conversion maps basic dataquieR types", {
  expect_equal(
    util_data_type_conversion(c("1.9", "2.1", NA), DATA_TYPES$INTEGER),
    c(1, 2, NA)
  )
  expect_equal(
    util_data_type_conversion(c("1.5", "bad", NA), DATA_TYPES$FLOAT),
    c(1.5, NA, NA)
  )
  expect_identical(
    util_data_type_conversion(factor(c("a", "b")), DATA_TYPES$STRING),
    c("a", "b")
  )
})

test_that("util_data_type_conversion handles logical integer metadata", {
  integer_type <- structure(DATA_TYPES$INTEGER, orig_type = "logical")

  expect_identical(
    util_data_type_conversion(c("TRUE", "FALSE", NA), integer_type),
    c(1L, 0L, NA_integer_)
  )
})

test_that("util_data_type_conversion handles factor compatibility option", {
  x <- factor(c("10", "20"))

  withr::local_options(dataquieR.old_factor_handling = FALSE)
  expect_equal(
    util_data_type_conversion(x, DATA_TYPES$INTEGER),
    c(10, 20)
  )

  withr::local_options(dataquieR.old_factor_handling = TRUE)
  expect_equal(
    util_data_type_conversion(x, DATA_TYPES$INTEGER),
    c(1, 2)
  )
})

test_that("util_data_type_conversion maps date and time values", {
  date_value <- util_data_type_conversion("2020-01-02", DATA_TYPES$DATETIME)
  time_value <- util_data_type_conversion("12:34:56", DATA_TYPES$TIME)

  expect_s3_class(date_value, "POSIXct")
  expect_equal(format(date_value, "%Y-%m-%d"), "2020-01-02")
  expect_s3_class(time_value, "hms")
  expect_identical(as.character(time_value), "12:34:56")
})

test_that("util_data_type_conversion rejects unknown data types", {
  expect_error(
    util_data_type_conversion("x", "unknown"),
    "not a known data type",
    fixed = TRUE
  )
})
