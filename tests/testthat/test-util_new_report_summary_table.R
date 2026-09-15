test_that("util_new_report_summary_table constructs a data frame subclass", {
  x <- data.frame(Variables = "SBP_0", N = 1L)

  rst <- util_new_report_summary_table(x)

  expect_s3_class(rst, "ReportSummaryTable")
  expect_s3_class(rst, "data.frame")
  expect_true(util_is_report_summary_table(rst))
  expect_false(util_is_report_summary_table(x))
  expect_identical(unclass(rst), unclass(x))
})

test_that("util_new_report_summary_table rejects non-data frames", {
  expect_error(
    util_new_report_summary_table(list(Variables = "SBP_0", N = 1L)),
    "data.frame"
  )
})

test_that("util_new_report_summary_table normalizes empty inputs", {
  rst <- util_new_report_summary_table(data.frame())

  expect_s3_class(rst, "ReportSummaryTable")
  expect_named(rst, c("Variables", "N"))
  expect_equal(nrow(rst), 0L)
  expect_type(rst$Variables, "character")
  expect_type(rst$N, "integer")
})

test_that("util_new_report_summary_table rejects partial validation context", {
  table <- data.frame(Variables = "SBP_0", N = 1L)
  meta_data <- data.frame(VAR_NAMES = "SBP_0", LABEL = "Blood pressure")

  expect_error(
    util_new_report_summary_table(table, meta_data = meta_data),
    "either 'meta_data' and 'label_col' or neither"
  )
  expect_error(
    util_new_report_summary_table(table, label_col = LABEL),
    "either 'meta_data' and 'label_col' or neither"
  )
})

test_that("droplevels.ReportSummaryTable removes empty metric columns", {
  rst <- util_new_report_summary_table(data.frame(
    Variables = c("SBP_0", "DBP_0"),
    N = c(10L, 10L),
    keep = c(0, 1),
    drop_zero = c(0, 0),
    drop_na = c(NA_real_, NA_real_),
    check.names = FALSE
  ))

  reduced <- droplevels(rst)

  expect_s3_class(reduced, "ReportSummaryTable")
  expect_named(reduced, c("Variables", "N", "keep"))
  expect_equal(reduced$keep, c(0, 1))
})

test_that("droplevels.ReportSummaryTable rejects non-summary tables", {
  expect_error(
    droplevels.ReportSummaryTable(data.frame(Variables = "SBP_0", N = 1L))
  )
})
