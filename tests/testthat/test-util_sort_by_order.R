test_that("util_sort_by_order follows an external ordering vector", {
  skip_on_cran()

  x <- c("medium", "low", "high", "low")
  levels <- c("low", "medium", "high")

  expect_equal(util_sort_by_order(x, levels), c("low", "low", "medium", "high"))
  expect_equal(util_sort_by_order(x, levels, decreasing = TRUE),
    c("high", "medium", "low", "low"))
})

test_that("util_order_by_order returns positions in external order", {
  skip_on_cran()

  x <- c("medium", "low", "high", "low")
  levels <- c("low", "medium", "high")

  expect_equal(util_order_by_order(x, levels), c(2L, 4L, 1L, 3L))
})

test_that("dot substring helpers split at the first dot", {
  skip_on_cran()

  x <- c("call.var", "call.with.dot.var")

  expect_equal(`util_sub_string_left_from_.`(x), c("call", "call"))
  expect_equal(`util_sub_string_right_from_.`(x), c("var", "with.dot.var"))
  expect_error(`util_sub_string_left_from_.`("nodot"), "at least one dot")
  expect_error(`util_sub_string_right_from_.`("nodot"), "at least one dot")
})
