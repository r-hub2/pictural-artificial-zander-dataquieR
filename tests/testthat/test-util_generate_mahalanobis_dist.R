test_that("util_generate_mahalanobis_dist adds Mahalanobis distances", {
  skip_on_cran()

  x <- data.frame(
    x1 = c(1, 2, 3, 5),
    x2 = c(2, 5, 4, 9)
  )

  result <- util_generate_mahalanobis_dist(x, c("x1", "x2"), "grp")

  expected <- mahalanobis(
    x,
    colMeans(x),
    cov(x)
  )
  expect_identical(result$df, 2L)
  expect_named(result$x_with_MD, c("x1", "x2", "MD_grp"))
  expect_equal(
    result$x_with_MD[order(result$x_with_MD$x1), "MD_grp"],
    expected
  )
})

test_that("util_generate_mahalanobis_dist keeps rows with incomplete data", {
  skip_on_cran()

  x <- data.frame(
    x1 = c(1, 2, NA, 5),
    x2 = c(2, 5, 4, 9)
  )

  expect_message(
    result <- util_generate_mahalanobis_dist(x, c("x1", "x2"), "grp"),
    "N=1 observational units were excluded",
    fixed = TRUE
  )

  expect_identical(result$df, 2L)
  expect_equal(nrow(result$x_with_MD), nrow(x))
  expect_true(is.na(result$x_with_MD$MD_grp[is.na(result$x_with_MD$x1)]))
  expect_false(any(is.na(result$x_with_MD$MD_grp[!is.na(result$x_with_MD$x1)])))
})

test_that("util_generate_mahalanobis_dist stops without complete cases", {
  skip_on_cran()

  x <- data.frame(
    x1 = c(NA_real_, 1),
    x2 = c(2, NA_real_)
  )

  expect_error(
    util_generate_mahalanobis_dist(x, c("x1", "x2"), "grp"),
    "No observational unit with complete cases"
  )
})
