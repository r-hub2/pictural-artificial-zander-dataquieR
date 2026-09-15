test_that("util_sigmagap marks a separated upper group", {
  skip_on_cran()

  expect_equal(
    util_sigmagap(c(0, 1, 2, 100, 101)),
    c(0, 0, 0, 1, 1)
  )
})

test_that("util_sigmagap leaves a continuous sequence unmarked", {
  skip_on_cran()

  expect_equal(util_sigmagap(1:5), rep(0, 5))
})

test_that("util_sigmagap marks a separated lower gap", {
  skip_on_cran()

  expect_equal(
    util_sigmagap(c(-100, -50, 0, 1, 2)),
    c(0, 1, 1, 0, 0)
  )
})

test_that("util_sigmagap expands a low break before the mean", {
  skip_on_cran()

  expect_equal(
    util_sigmagap(c(0, 100, 200:249)),
    c(1, 1, 1, rep(0, 49))
  )
})
