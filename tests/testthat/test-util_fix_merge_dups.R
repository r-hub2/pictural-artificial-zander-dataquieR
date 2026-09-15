skip_on_cran()

test_that("util_fix_merge_dups collapses compatible duplicate columns", {
  data <- data.frame(
    id = 1:3,
    left = c(1, NA, 3),
    right = c(1, 2, NA),
    check.names = FALSE
  )
  names(data) <- c("id", "value", "value")

  fixed <- util_fix_merge_dups(data)

  expect_identical(names(fixed), c("id", "value"))
  expect_identical(fixed$value, c(1, 2, 3))
})

test_that("util_fix_merge_dups leaves empty data frames unchanged", {
  data <- data.frame(id = integer(), left = numeric(), right = numeric(),
    check.names = FALSE)
  names(data) <- c("id", "value", "value")

  expect_identical(util_fix_merge_dups(data), data)
})

test_that("util_fix_merge_dups rejects incompatible duplicate columns", {
  data <- data.frame(
    id = 1:2,
    left = c(1, 2),
    right = c(1, 3),
    check.names = FALSE
  )
  names(data) <- c("id", "value", "value")

  expect_warning(
    expect_error(util_fix_merge_dups(data), "fix_merge_dups failed"),
    "could not fix merge result"
  )
})

test_that("util_fix_merge_dups can keep incompatible duplicate columns", {
  data <- data.frame(
    id = 1:2,
    left = c(1, 2),
    right = c(1, 3),
    check.names = FALSE
  )
  names(data) <- c("id", "value", "value")

  expect_warning(
    fixed <- util_fix_merge_dups(data, stop_if_incompatible = FALSE),
    "could not fix merge result"
  )

  expect_identical(names(fixed), c("id", "value", "value.1"))
  expect_identical(fixed$value, c(1, 2))
  expect_identical(fixed$value.1, c(1, 3))
})
