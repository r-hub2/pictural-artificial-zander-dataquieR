test_that("simple outlier rule helpers flag values beyond their thresholds", {
  skip_on_cran()

  x <- c(rep(0, 20), 100, NA)
  expected <- c(rep(0, 20), 1, NA)

  expect_equal(util_3SD(x), expected)
  expect_equal(util_tukey(x), expected)
  expect_equal(util_hubert(x), expected)
})

test_that("simple outlier rule helpers keep ordinary values unflagged", {
  skip_on_cran()

  x <- c(1, 2, 3, 4, 5)

  expect_equal(util_3SD(x), rep(0, length(x)))
  expect_equal(util_tukey(x), rep(0, length(x)))
  expect_equal(util_hubert(x), rep(0, length(x)))
})
