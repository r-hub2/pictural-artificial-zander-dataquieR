test_that("util_is_numeric_in builds numeric predicates with diagnostics", {
  skip_on_cran()

  positive_integer <- util_is_numeric_in(min = 1, max = 5,
    whole_num = TRUE, finite = TRUE)
  expect_true(positive_integer(c(1, 5)))
  expect_false(positive_integer(0))
  expect_false(positive_integer(1.5))
  expect_false(positive_integer(Inf))
  expect_false(positive_integer("1"))
  expect_match(util_attr(positive_integer, "error_msg", exact = TRUE),
    "whole number")
  expect_match(util_attr(positive_integer, "error_msg", exact = TRUE),
    "larger/equal")
  expect_match(util_attr(positive_integer, "error_msg", exact = TRUE),
    "smaller/equal")
})

test_that("util_is_numeric_in can restrict values to an explicit set", {
  skip_on_cran()

  in_set <- util_is_numeric_in(set = c(1, 3, 5))

  expect_true(in_set(c(1, 5)))
  expect_false(in_set(c(1, 2)))
  expect_match(util_attr(in_set, "error_msg", exact = TRUE),
    '"1", "3", "5"', fixed = TRUE)
})
