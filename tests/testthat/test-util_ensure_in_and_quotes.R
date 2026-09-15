test_that("util_ensure_in warns and returns allowed values", {
  skip_on_cran()

  expect_warning(
    result <- util_ensure_in(c("alfa", "beta"), c("alpha", "beta")),
    "did you mean"
  )

  expect_equal(result, "beta")
})

test_that("util_ensure_in can report custom errors", {
  skip_on_cran()

  expect_error(
    util_ensure_in(
      "alfa",
      c("alpha", "beta"),
      err_msg = "Unknown %s; closest %s",
      error = TRUE
    ),
    "Unknown"
  )
})

test_that("quote helpers use plain R quotes", {
  skip_on_cran()

  expect_equal(as.character(util_set_dQuoteString(c("a", "b"))),
    c('"a"', '"b"'))
  expect_equal(as.character(util_set_sQuoteString(c("a", "b"))),
    c("'a'", "'b'"))
})
