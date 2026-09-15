skip_on_cran()

test_that("descriptive helper environment computes local summary values", {
  helpers <- util_des_functions_env

  expect_identical(
    as.character(helpers$util_compute_median_cat(
      ordered(c("low", "high", "high"))
    )),
    "high"
  )
  expect_message(
    median_even <- helpers$util_compute_median_cat(ordered(c("low", "high"))),
    "Median is between two values"
  )
  expect_identical(as.character(median_even), "high")

  expect_identical(
    helpers$util_compute_mode_cat(factor(c("b", "a", "b", "a"))),
    "b a"
  )
  expect_identical(
    helpers$util_compute_mode_cat(factor(c("a", "b", "c", "d"))),
    "a b c and other 1 categories"
  )
  expect_identical(
    helpers$util_compute_mode_contin(c(1, 2, 1, 2, 3, 3, 4)),
    "1 2 and other 1 values"
  )

  expect_equal(helpers$util_compute_skewness(c(1, 2, 3, NA)), 0)
  expect_equal(helpers$util_compute_SE_skewness(1:5), 0.9128709,
    tolerance = 1e-6
  )
  expect_equal(helpers$util_compute_kurtosis(c(1, 2, 3, NA)), -2.5)
})

test_that("descriptive helper environment formats compact tables and time", {
  helpers <- util_des_functions_env

  expect_match(
    helpers$util_compute_frequency_table(c("b", "a", "b")),
    "2 = .b. . 1 = .a."
  )
  expect_identical(
    helpers$util_first_row_to_colnames(data.frame(
      first = c("x", "1"),
      second = c("y", "2")
    )),
    data.frame(x = "1", y = "2", row.names = 2L)
  )
  expect_identical(
    helpers$util_combine_cols_content(c(NA, "x", NA), "subject"),
    "x"
  )
  expect_identical(
    helpers$util_combine_cols_content(c(NA, NA), "subject"),
    ""
  )
  expect_warning(
    expect_true(is.na(helpers$util_combine_cols_content(
      c("x", "y"),
      "subject"
    ))),
    "more than one"
  )

  expect_identical(
    helpers$util_compute_difftime_auto(as.difftime(90, units = "secs")),
    "90 seconds"
  )
  expect_identical(
    helpers$util_compute_difftime_auto(as.difftime(3, units = "hours")),
    "3 hours"
  )
  expect_error(
    helpers$util_compute_difftime_auto(90),
    "Input must be a 'difftime' object"
  )
})
