test_that("util_condition_from_try_error works", {
  skip_on_cran()

  expect_true(util_is_try_error(try(stop("Test"), silent = TRUE)))
  expect_false(util_is_try_error("Test"))

  x <- util_condition_from_try_error(try(stop("Test"), silent = TRUE))
  expect_s3_class(x, "simpleError")
  expect_equal(conditionMessage(x), "Test")

  expect_error(
    util_condition_from_try_error("Test"),
    "Not a try-error"
  )
})
