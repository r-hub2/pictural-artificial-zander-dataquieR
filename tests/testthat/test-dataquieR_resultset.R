skip_on_cran()

test_that("dataquieR_resultset works", {
  skip_on_cran()
  expect_error(dataquieR_resultset(list(a = 1, b = 2)))
})

test_that("deprecated dataquieR_resultset coercion methods stop clearly", {
  old_report <- structure(list(), class = "dataquieR_resultset")

  expect_error(
    as.data.frame(old_report),
    class = "lifecycle_error_deprecated"
  )
  expect_error(
    as.list(old_report),
    class = "lifecycle_error_deprecated"
  )
  expect_error(
    summary(old_report),
    class = "lifecycle_error_deprecated"
  )
})
