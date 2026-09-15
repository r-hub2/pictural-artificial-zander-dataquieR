skip_on_cran()

test_that("util_table_rotator converts one-row tables to key-value form", {
  table <- data.frame(
    alpha = 1,
    beta = TRUE,
    check.names = FALSE
  )
  attr(table, "is_html_escaped") <- TRUE

  rotated <- util_table_rotator(table)

  expect_s3_class(rotated, "data.frame")
  expect_identical(names(rotated), c(" ", " "))
  expect_identical(rotated[[1]], c("alpha", "beta"))
  expect_identical(rotated[[2]], c("1", "TRUE"))
  expect_true(util_attr(rotated, "is_html_escaped", exact = TRUE))
  expect_true(util_attr(rotated, "kv_table", exact = TRUE))
})

test_that("util_table_rotator leaves multi-row tables unchanged", {
  table <- data.frame(alpha = 1:2, beta = c("x", "y"))

  expect_identical(util_table_rotator(table), table)
})

test_that("util_table_of_vct returns stable table columns", {
  table <- util_table_of_vct(c("b", "a", "b", NA_character_))

  expect_s3_class(table, "data.frame")
  expect_identical(names(table), c("Var1", "Freq"))
  expect_identical(as.character(table$Var1), c("a", "b"))
  expect_identical(table$Freq, c(1L, 2L))

  expect_identical(util_table_of_vct(NULL), data.frame(
    Var1 = integer(0),
    Freq = integer(0)
  ))
})

test_that("util_table_of_vct keeps unused factor levels", {
  table <- util_table_of_vct(factor(c("b", "a", "b"), levels = c(
    "a", "b", "c"
  )))

  expect_identical(as.character(table$Var1), c("a", "b", "c"))
  expect_identical(table$Freq, c(1L, 2L, 0L))
})
