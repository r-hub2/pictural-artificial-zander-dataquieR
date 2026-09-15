test_that("prep_acc_distributions_with_ecdf works", {
  skip_on_cran() # slow, errors obvious
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  skip_if_not_installed("colorspace")
  skip_if_not_installed("plotly")
  meta_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data.RData") # nolint: line_length_linter.
  study_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData", keep_types = TRUE) # nolint: line_length_linter.

  meta_data <-
    prep_scalelevel_from_data_and_metadata(
      study_data = study_data,
      meta_data = meta_data
    )

  withr::local_options(dataquieR.lazy_plots = FALSE)

  expect_silent(
    r1 <- prep_acc_distributions_with_ecdf(
      resp_vars = "SBP_0",
      group_vars = "USR_BP_0",
      study_data = study_data,
      meta_data = meta_data
    )
  )

  expect_false(
    inherits(try(ggplot_build(r1$SummaryPlot)), "try-error")
  )
  expect_s3_class(r1$SummaryPlot, "patchwork")

  seen <- new.env(parent = emptyenv())
  seen$count <- 0L
  testthat::local_mocked_bindings(
    util_ggplotly = function(...) {
      seen$count <- seen$count + 1L
      plotly::plot_ly(x = 1, y = 1, type = "scatter", mode = "markers")
    }
  )

  converted <- util_as_plotly_prep_acc_distributions_with_ecdf(r1)

  expect_s3_class(converted, "plotly")
  expect_identical(seen$count, 2L)
})
