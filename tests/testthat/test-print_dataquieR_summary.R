test_that("print.dataquieR_summary validates arguments before rendering", {
  skip_on_cran()

  summary <- structure(NA, class = "dataquieR_summary")
  captured <- new.env(parent = emptyenv())

  testthat::local_mocked_bindings(
    util_ensure_suggested = function(...) TRUE,
    util_reclassify_dataquieR_summary = function(x) x,
    util_render_table_dataquieR_summary = function(
      x, grouped_by,
      folder_of_report,
      vars_to_include
    ) {
      captured$summary <- x
      captured$grouped_by <- grouped_by
      captured$folder_of_report <- folder_of_report
      captured$vars_to_include <- vars_to_include
      "rendered-summary"
    }
  )

  expect_invisible(
    result <- print.dataquieR_summary(
      summary,
      grouped_by = c("indicator_metric", "indicator_metric"),
      dont_print = TRUE,
      folder_of_report = c(com_item_missingness = "report.html"),
      vars_to_include = c("study", "ssi")
    )
  )

  expect_identical(result, "rendered-summary")
  expect_identical(captured$summary, summary)
  expect_identical(captured$grouped_by, "indicator_metric")
  expect_identical(
    captured$folder_of_report,
    c(com_item_missingness = "report.html")
  )
  expect_identical(captured$vars_to_include, c("study", "ssi"))

  expect_error(
    print.dataquieR_summary(
      summary,
      dont_print = TRUE,
      vars_to_include = "invalid"
    ),
    "invalid"
  )
})
