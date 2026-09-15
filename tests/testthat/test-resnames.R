# resnames is also tested implicitly by test-nres.R.

test_that("resnames dispatches for resultset2 list structures", {
  skip_on_cran()

  resultset <- structure(
    list(
      first = list(beta = 1, alpha = 2),
      second = list(beta = 3, gamma = 4)
    ),
    class = "dataquieR_resultset2"
  )

  expect_identical(resnames(resultset), c("alpha", "beta", "gamma"))

  attr(resultset, "resnames") <- c("stored", "order")

  expect_identical(resnames(resultset), c("stored", "order"))
})
