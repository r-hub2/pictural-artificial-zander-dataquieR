test_that(
  "deprecated dataquieR_resultset coercions stop with dq_report2 hint",
  {
    skip_on_cran()
    skip_if_not_installed("lifecycle")

    old_report <- structure(list(), class = "dataquieR_resultset")

    expect_error(
      as.list(old_report),
      "Please use `dq_report2\\(\\)` instead"
    )
    expect_error(
      as.data.frame(old_report),
      "Please use `dq_report2\\(\\)` instead"
    )
  }
)
