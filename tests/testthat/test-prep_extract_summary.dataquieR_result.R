skip_on_cran()

test_that("prep_extract_summary extracts percentage metrics from a result", {
  skip_on_cran()

  summary_table <- data.frame(
    PCT_com_qum_nonresp = "12.345",
    PCT_com_qum_refusal = "5",
    stringsAsFactors = FALSE
  )
  result <- list(SummaryTable = summary_table)
  class(result) <- c("dataquieR_result", "master_result", "list")

  call <- quote(dummy())
  attr(call, "entity_name") <- "Blood pressure"
  attr(call, VAR_NAMES) <- "bp"
  attr(call, STUDY_SEGMENT) <- "Segment A"
  attr(call, GRADING_RULESET) <- "custom"
  attr(call, "label_col") <- LABEL
  attr(result, "call") <- call
  attr(result, "cn") <- "com_test"
  attr(result, CHECK_ID) <- "questionnaire-check"

  summary <- suppressWarnings(prep_extract_summary.dataquieR_result(result))

  expect_s3_class(summary, "dq_report2_summary")
  expect_identical(summary$Data$VAR_NAMES, "bp")
  expect_identical(summary$Data$STUDY_SEGMENT, "Segment A")
  expect_identical(summary$Data$com_test.PCT_com_qum_nonresp, "12.34%")
  expect_identical(summary$Data$com_test.PCT_com_qum_refusal, "5.00%")
  expect_equal(summary$Table$com_test.PCT_com_qum_nonresp, 12.345)
  expect_equal(summary$Table$com_test.PCT_com_qum_refusal, 5)
  expect_identical(summary$meta_data[[LABEL]], "Blood pressure")
  expect_identical(summary$meta_data[[GRADING_RULESET]], "custom")
  expect_identical(summary$Data[[CHECK_ID]], "questionnaire-check")
  expect_identical(summary$Table[[CHECK_ID]], "questionnaire-check")
  expect_identical(summary$meta_data[[CHECK_ID]], "questionnaire-check")
})

test_that("prep_extract_summary uses default metadata attributes", {
  skip_on_cran()

  summary_table <- data.frame(
    PCT_com_qum_nonresp = "1",
    stringsAsFactors = FALSE
  )
  result <- list(SummaryTable = summary_table)
  class(result) <- c("dataquieR_result", "master_result", "list")

  call <- quote(dummy())
  attr(call, "entity_name") <- "Blood pressure"
  attr(result, "call") <- call
  attr(result, "cn") <- "com_test"

  summary <- suppressWarnings(prep_extract_summary.dataquieR_result(result))

  expect_identical(summary$Data$VAR_NAMES, "Blood pressure")
  expect_identical(summary$Data$STUDY_SEGMENT, "Study")
  expect_identical(summary$meta_data[[VAR_NAMES]], "Blood pressure")
  expect_identical(summary$meta_data[[GRADING_RULESET]], 0)
  expect_identical(summary$meta_data[[STUDY_SEGMENT]], "Study")
  expect_identical(summary$meta_data[[LABEL]], "Blood pressure")
})

test_that("prep_extract_summary uses CHECK_ID for unnamed group results", {
  skip_on_cran()

  result <- list(SummaryTable = data.frame(
    NUM_acc_ud_outlm = 14,
    PCT_acc_ud_outlm = 0.66
  ))
  class(result) <- c("dataquieR_result", "master_result", "list")

  call <- quote(dummy())
  group_label <- "Systolic blood pressure checks"
  attr(call, "entity_name") <- group_label
  attr(call, VAR_NAMES) <- stats::setNames(NA_character_, group_label)
  attr(call, "label_col") <- LABEL
  attr(result, "call") <- call
  attr(result, "cn") <- "acc_multivariate_outlier"
  attr(result, CHECK_ID) <- "8"

  summary <- suppressWarnings(prep_extract_summary.dataquieR_result(result))

  expect_identical(summary$Data[[VAR_NAMES]], "8")
  expect_identical(summary$Table[[VAR_NAMES]], "8")
  expect_identical(summary$meta_data[[VAR_NAMES]], "8")
  expect_identical(summary$meta_data[[LABEL]], group_label)
  expect_identical(summary$Table[[CHECK_ID]], "8")

  classes <- suppressWarnings(prep_summary_to_classes(summary))
  expect_setequal(
    classes$indicator_metric,
    c("NUM_acc_ud_outlm", "PCT_acc_ud_outlm")
  )
  expect_true(all(classes[[VAR_NAMES]] == "8"))
  expect_true(all(classes[[CHECK_ID]] == "8"))
})

test_that("prep_extract_summary retains variable-group result labels", {
  skip_on_cran()

  variable_group_table <- data.frame(
    VARIABLE_LIST = "bp_baseline | bp_follow_up",
    CHECK_ID = "blood_pressure_repeat",
    CHECK_LABEL = "Blood pressure repeat measures",
    NUM_acc_drm_inter = 0.4,
    GRADING_RULESET = "0",
    stringsAsFactors = FALSE
  )
  result <- list(VariableGroupTable = variable_group_table)
  class(result) <- c("dataquieR_result", "master_result", "list")

  call <- quote(dummy())
  attr(call, "entity_name") <- "Repeat-measurement assessment"
  attr(call, "label_col") <- LABEL
  attr(result, "call") <- call
  attr(result, "cn") <- "acc_repeated_measurements"

  summary <- suppressWarnings(prep_extract_summary.dataquieR_result(result))

  expect_s3_class(summary, "dq_report2_summary")
  expect_identical(
    summary$meta_data[[LABEL]],
    "Blood pressure repeat measures"
  )
  expect_identical(summary$meta_data[[VAR_NAMES]], "blood_pressure_repeat")
  expect_identical(summary$meta_data[[CHECK_ID]], "blood_pressure_repeat")
  expect_identical(summary$Data[[CHECK_ID]], "blood_pressure_repeat")
  expect_identical(summary$Table[[CHECK_ID]], "blood_pressure_repeat")
  expect_equal(summary$Table$acc_repeated_measurements.NUM_acc_drm_inter, 0.4)

  classes <- suppressWarnings(prep_summary_to_classes(summary))
  expect_identical(classes[[LABEL]], "Blood pressure repeat measures")
  expect_identical(classes[[GRADING_RULESET]], "0")
  expect_identical(classes[[CHECK_ID]], "blood_pressure_repeat")
})

test_that("prep_extract_summary retains ungraded variable-group results", {
  skip_on_cran()

  result <- list(VariableGroupTable = data.frame(
    VARIABLE_LIST = "SBP_0 | SBP_1",
    CHECK_ID = "blood_pressure",
    CHECK_LABEL = "Blood pressure",
    max_cor = 0.8,
    in_range = TRUE,
    stringsAsFactors = FALSE
  ))
  class(result) <- c("dataquieR_result", "master_result", "list")

  call <- quote(dummy())
  attr(call, "entity_name") <- "Association assessment"
  attr(call, "label_col") <- LABEL
  attr(result, "call") <- call
  attr(result, "cn") <- "des_scatterplot_matrix"

  summary <- suppressWarnings(prep_extract_summary.dataquieR_result(result))

  expect_equal(summary$Table$des_scatterplot_matrix.max_cor, 0.8)
  expect_equal(summary$Table$des_scatterplot_matrix.in_range, TRUE)
  expect_identical(summary$meta_data[[LABEL]], "Blood pressure")
})

test_that("prep_extract_summary rejects anonymous variable groups", {
  result <- structure(
    list(VariableGroupTable = data.frame(metric = 1)),
    class = c("dataquieR_result", "master_result", "list")
  )
  attr(result, "call") <- quote(dummy())
  attr(result, "cn") <- ""

  testthat::local_mocked_bindings(
    util_extract_indicator_metrics = function(x) x
  )
  expect_error(
    suppressWarnings(prep_extract_summary.dataquieR_result(result)),
    "VariableGroupTable must contain CHECK_ID"
  )
})

test_that("prep_extract_summary keeps unprefixed raw metrics", {
  result <- structure(
    list(SummaryTable = data.frame(metric = "raw")),
    class = c("dataquieR_result", "master_result", "list")
  )
  call <- quote(dummy())
  attr(call, "entity_name") <- "Variable"
  attr(result, "call") <- call
  attr(result, "cn") <- "call"

  testthat::local_mocked_bindings(
    util_extract_indicator_metrics = function(x) x
  )
  summary <- suppressWarnings(prep_extract_summary.dataquieR_result(result))

  expect_identical(as.character(summary$Data$call.metric), "raw")
  expect_identical(as.character(summary$Table$call.metric), "raw")
})
