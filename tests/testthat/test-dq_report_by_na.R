test_that("dq_report_by works with content na", {
  skip_if_not_installed("DT")
  skip_if_not_installed("markdown")
  skip_if_not_installed("stringdist")

  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  withr::local_options(
    dataquieR.CONDITIONS_WITH_STACKTRACE = TRUE,
    dataquieR.ERRORS_WITH_CALLER = TRUE,
    dataquieR.WARNINGS_WITH_CALLER = TRUE,
    dataquieR.MESSAGES_WITH_CALLER = TRUE
  )
  skip_on_cran() # slow test
  target <- withr::local_tempdir("testdqareportby")

  expect_message2(dq_report_by("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData", # nolint: line_length_linter.
      dimensions = "int",
      filter_indicator_functions = "int_datatype_matrix",
      cores = NULL,
      segment_column = NULL,
      meta_data_v2 = "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx", # nolint: line_length_linter.
      output_dir = !!file.path(target, "na"),
      also_print = FALSE
    ))

})
