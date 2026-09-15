test_that("util_anti_join_common joins by shared columns when available", {
  skip_on_cran()

  x <- data.frame(
    id = c(1, 2, 3),
    value = c("a", "b", "c"),
    stringsAsFactors = FALSE
  )
  y <- data.frame(
    id = c(2, 4),
    other = c("drop", "ignore"),
    stringsAsFactors = FALSE
  )

  out <- util_anti_join_common(x, y)

  expect_equal(out$id, c(1, 3))
  expect_equal(out$value, c("a", "c"))
})

test_that("util_anti_join_common preserves legacy no-common-column semantics", {
  skip_on_cran()

  x <- data.frame(id = c(1, 2), stringsAsFactors = FALSE)
  y <- data.frame(other = "non-empty", stringsAsFactors = FALSE)

  expect_equal(util_anti_join_common(x, y), x[FALSE, , drop = FALSE])
  expect_equal(util_anti_join_common(x, y[FALSE, , drop = FALSE]), x)
})
