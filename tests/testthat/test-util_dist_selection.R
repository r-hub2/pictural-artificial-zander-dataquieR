test_that("util_dist_selection works", {
  skip_on_cran()
  study_data <- data.frame(
    integer = c(1, 2, 3, 4),
    factor = factor(c(1, 2, 2, 3),
      levels = 1:3,
      labels = c("male", "female", "undefined")
    ),
    ordered = ordered(c(3, 2, 1, 3),
      levels = 3:1,
      labels = c("low", "medium", "high")
    ),
    integer = as.integer(c(1, 2, 3, 4)),
    logical = c(TRUE, FALSE, TRUE, FALSE),
    float = 1:4 * pi
  )
  dist_info <- util_dist_selection(study_data)
  expect_equal(
    dist_info$IsInteger,
    c(rep(TRUE, 4), FALSE, FALSE)
  )
  expect_equal(
    dist_info$IsMultCat,
    c(rep(TRUE, 4), NA, NA)
  )
  expect_equal(
    dist_info$NCategory,
    c(4, 3, 3, 4, NA, NA)
  )
})

test_that("util_dist_selection reports distinct values and zero proportions", {
  skip_on_cran()

  study_data <- data.frame(
    signed = c(-1, 0, 0, 2, NA),
    category = c("a", "b", "b", "", NA),
    stringsAsFactors = FALSE
  )

  dist_info <- util_dist_selection(study_data)

  expect_true(dist_info$AnyNegative[[1]])
  expect_equal(dist_info$NDistinct, c(3, 2))
  expect_equal(dist_info$PropZeroes, c(2 / 4, 0))
  expect_equal(dist_info$NCategory, c(3, 2))
  expect_equal(dist_info$IsMultCat, c(TRUE, FALSE))
})

test_that("util_dist_selection warns for deprecated value-label argument", {
  skip_on_cran()

  expect_warning(
    util_dist_selection(data.frame(x = 1), val_lab = data.frame()),
    "deprecated"
  )
})
