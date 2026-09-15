test_that("util_as_valid_missing_codes classifies scalar missing-code inputs", {
  skip_on_cran()

  values <- c("1", "1.5", "2020-01-02", "12:13:14", "abc")

  converted <- util_as_valid_missing_codes(values)

  expect_equal(converted[[1]], 1)
  expect_equal(converted[[2]], 1.5)
  expect_s3_class(converted[[3]], "POSIXct")
  expect_equal(format(converted[[3]], "%Y-%m-%d"), "2020-01-02")
  expect_s3_class(converted[[4]], "hms")
  expect_equal(as.character(converted[[4]]), "12:13:14")
  expect_identical(converted[[5]], NA_character_)
})

test_that("util_as_valid_missing_codes keeps current missing-value branch", {
  skip_on_cran()

  converted <- util_as_valid_missing_codes(NA_character_)

  expect_s3_class(converted[[1]], "hms")
  expect_true(is.na(converted[[1]]))
})
