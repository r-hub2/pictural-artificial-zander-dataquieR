skip_on_cran()

test_that("ReportSummaryTable accessors read and write attributes", {
  rst <- util_new_report_summary_table(
    data.frame(Variables = "SBP_0", N = 1L)
  )

  rst <- util_set_report_summary_table_higher_means(rst, "low")
  rst <- util_set_report_summary_table_flip_mode(rst, "noflip")
  rst <- util_set_report_summary_table_continuous(rst, TRUE)
  rst <- util_set_report_summary_table_colscale(rst, c("red", "blue"))
  rst <- util_set_report_summary_table_colcode(rst, c(ok = "blue"))
  rst <- util_set_report_summary_table_level_names(rst, c("ok"))
  rst <- util_set_report_summary_table_relative(rst, FALSE)
  rst <- util_set_report_summary_table_var_names(rst, "v00004")
  rst <- util_set_report_summary_table_variables_label_col(rst, LABEL)

  expect_identical(util_report_summary_table_higher_means(rst), "low")
  expect_identical(util_report_summary_table_flip_mode(rst), "noflip")
  expect_true(util_report_summary_table_continuous(rst))
  expect_identical(util_report_summary_table_colscale(rst), c("red", "blue"))
  expect_identical(util_report_summary_table_colcode(rst), c(ok = "blue"))
  expect_identical(util_report_summary_table_level_names(rst), c("ok"))
  expect_false(util_report_summary_table_relative(rst))
  expect_identical(util_report_summary_table_var_names(rst), "v00004")
  expect_identical(util_report_summary_table_variables_label_col(rst), LABEL)
})

test_that("util_init_respum_tab applies print defaults", {
  skip_on_cran()

  rst <- util_new_report_summary_table(
    data.frame(Variables = "SBP_0", N = 1L, Metric = 0.5))

  defaults <- util_init_respum_tab(rst)
  expect_identical(defaults$higher_means, "worse")
  expect_true(defaults$continuous)
  expect_true(defaults$relative)
  expect_length(defaults$colscale, 9L)
  expect_null(defaults$flip_mode)

  rst <- util_set_report_summary_table_higher_means(rst, "better")
  rst <- util_set_report_summary_table_continuous(rst, FALSE)
  rst <- util_set_report_summary_table_relative(rst, TRUE)
  rst <- util_set_report_summary_table_colscale(rst, c("low", "high"))
  rst <- util_set_report_summary_table_colcode(rst, c(ok = "green"))
  rst <- util_set_report_summary_table_level_names(rst, c("ok"))
  rst <- util_set_report_summary_table_flip_mode(rst, "flip")

  attrs <- util_init_respum_tab(rst)
  expect_identical(attrs$higher_means, "better")
  expect_false(attrs$continuous)
  expect_true(attrs$relative)
  expect_identical(attrs$colscale, c("high", "low"))
  expect_identical(attrs$colcode, c(ok = "green"))
  expect_identical(attrs$level_names, c("ok"))
  expect_identical(attrs$flip_mode, "flip")
})
