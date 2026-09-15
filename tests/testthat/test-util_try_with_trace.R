skip_on_cran()

test_that("util_try_with_trace returns values or try-errors with conditions", {
  expect_equal(util_try_with_trace(1 + 1), 2)

  expect_message(
    err <- util_try_with_trace(stop("boom")),
    "Error: boom"
  )

  expect_s3_class(err, "try-error")
  expect_equal(as.character(err), "boom")
  expect_s3_class(attr(err, "condition"), "error")
  expect_s3_class(attr(err, "condition")$trace, "rlang_trace")
})

test_that("util_try_with_trace can suppress the printed message", {
  expect_silent(err <- util_try_with_trace(stop("quiet boom"), silent = TRUE))

  expect_s3_class(err, "try-error")
  expect_equal(as.character(err), "quiet boom")
})
