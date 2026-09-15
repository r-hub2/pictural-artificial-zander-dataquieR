test_that("util_table_rotator rotates one-row tables to key-value tables", {
  skip_on_cran()

  table <- data.frame(A = "x", B = 1, check.names = FALSE)
  attr(table, "is_html_escaped") <- TRUE

  rotated <- util_table_rotator(table)

  expect_equal(dim(rotated), c(2L, 2L))
  expect_equal(names(rotated), c(" ", " "))
  expect_equal(rotated[[1]], c("A", "B"))
  expect_equal(rotated[[2]], c("x", "1"))
  expect_true(util_attr(rotated, "is_html_escaped", exact = TRUE))
  expect_true(util_attr(rotated, "kv_table", exact = TRUE))
})

test_that("util_table_rotator leaves multi-row tables unchanged", {
  skip_on_cran()

  table <- data.frame(A = c("x", "y"), B = c(1, 2), check.names = FALSE)

  expect_identical(util_table_rotator(table), table)
})
