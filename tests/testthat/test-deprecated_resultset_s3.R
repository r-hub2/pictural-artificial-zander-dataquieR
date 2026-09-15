test_that("deprecated dataquieR_resultset S3 helpers stop", {
  skip_on_cran()

  old_report <- structure(list(), class = "dataquieR_resultset")

  expect_error(as.data.frame(old_report), "deprecated")
  expect_error(as.list(old_report), "deprecated")
  expect_error(print.dataquieR_resultset(old_report), "deprecated")
  expect_error(summary.dataquieR_resultset(old_report), "deprecated")
  expect_error(dataquieR_resultset_verify(old_report), "deprecated")
  expect_error(dataquieR_resultset(), "deprecated")
})
