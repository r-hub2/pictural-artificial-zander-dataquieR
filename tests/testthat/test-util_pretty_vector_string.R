skip_on_cran()

test_that("util_pretty_vector_string formats vector values compactly", {
  expect_identical(util_pretty_vector_string(character()), "")
  expect_identical(
    util_pretty_vector_string(c("a", NA_character_, "c"), quote = sQuote),
    paste0(sQuote("a"), ", ", sQuote(""), ", ", sQuote("c"))
  )
  expect_identical(
    util_pretty_vector_string(c("a", "b", "c"), n_max = 2),
    paste0(dQuote("a"), ", ", dQuote("b"), ", ...")
  )
})

test_that("util_is_integer distinguishes missing from infinite values", {
  expect_identical(
    util_is_integer(c(1, 1.5, NA_real_, NaN, Inf, -Inf)),
    c(TRUE, FALSE, TRUE, FALSE, FALSE, FALSE)
  )
})

test_that("util_pretty_vector_string validates quote callbacks", {
  expect_error(
    util_pretty_vector_string("a", quote = function() "x"),
    "length\\(formals\\(quote\\)\\) > 0"
  )
  expect_error(
    util_pretty_vector_string("a", quote = "not a function"),
    "is.function\\(quote\\)"
  )
})
