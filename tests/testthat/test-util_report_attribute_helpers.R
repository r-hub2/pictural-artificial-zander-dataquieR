skip_on_cran()

test_that("quote helpers use plain ASCII quotes", {
  withr::local_options(useFancyQuotes = TRUE)

  expect_identical(
    as.character(util_set_dQuoteString(c("alpha", "beta"))),
    c("\"alpha\"", "\"beta\"")
  )
  expect_identical(
    as.character(util_set_sQuoteString(c("alpha", "beta"))),
    c("'alpha'", "'beta'")
  )
  expect_true(getOption("useFancyQuotes"))
})

test_that("report metadata helpers prefer cross-item metadata", {
  cross_item <- data.frame(VAR_NAMES = "x")
  legacy_cross <- data.frame(VAR_NAMES = "legacy")
  report <- list()
  attr(report, "meta_data_cross_item") <- cross_item
  attr(report, "meta_data_cross") <- legacy_cross
  attr(report, "meta_data") <- data.frame(VAR_NAMES = "item")

  expect_identical(util_report_meta_data_cross_item(report), cross_item)
  expect_identical(
    util_report_meta_data_frames(report),
    c("meta_data_cross_item", "meta_data")
  )

  attr(report, "meta_data_cross_item") <- NULL
  expect_identical(util_report_meta_data_cross_item(report), legacy_cross)

  attr(report, "meta_data_cross") <- NULL
  expect_equal(util_report_meta_data_cross_item(report), data.frame())
})

test_that("ReportSummaryTable attribute helpers round-trip values", {
  table <- data.frame(Variables = c("a", "b"))
  class(table) <- c("ReportSummaryTable", class(table))

  table <- util_set_report_summary_table_higher_means(table, TRUE)
  table <- util_set_report_summary_table_flip_mode(table, "vertical")
  table <- util_set_report_summary_table_continuous(table, FALSE)
  table <- util_set_report_summary_table_colscale(table, c("red", "green"))
  table <- util_set_report_summary_table_colcode(table, c("#ff0000", "#00ff00"))
  table <- util_set_report_summary_table_level_names(table, c("low", "high"))
  table <- util_set_report_summary_table_relative(table, TRUE)
  table <- util_set_report_summary_table_var_names(table, c("x", "y"))
  table <- util_set_report_summary_table_variables_label_col(table, LABEL)

  expect_true(util_report_summary_table_higher_means(table))
  expect_identical(util_report_summary_table_flip_mode(table), "vertical")
  expect_false(util_report_summary_table_continuous(table))
  expect_identical(
    util_report_summary_table_colscale(table),
    c("red", "green")
  )
  expect_identical(
    util_report_summary_table_colcode(table),
    c("#ff0000", "#00ff00")
  )
  expect_identical(
    util_report_summary_table_level_names(table),
    c("low", "high")
  )
  expect_true(util_report_summary_table_relative(table))
  expect_identical(
    util_report_summary_table_var_names(table),
    c("x", "y")
  )
  expect_identical(
    util_report_summary_table_variables_label_col(table),
    LABEL
  )
})
