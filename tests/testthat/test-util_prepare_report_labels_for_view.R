test_that("report display labels are adjusted only on a render copy", {
  skip_on_cran()

  meta_data <- data.frame(
    VAR_NAMES = c("scaleA1", "scaleA2", "v00018", "v01018"),
    LABEL = c(
      "scaleA1: scaleA1 Questionnaire item",
      "scaleA2 - Follow-up item",
      "v00018: EDUCATION",
      "v01018: EDUCATION"
    ),
    LONG_LABEL = c(
      "scaleA1: Long questionnaire item",
      "scaleA2 - Long follow-up item",
      "Education baseline",
      "Education follow-up"
    ),
    LABEL_DE = c(
      "scaleA1: Deutsches Item",
      "scaleA2 - Deutsches Follow-up-Item",
      "Bildung Baseline",
      "Bildung Follow-up"
    ),
    stringsAsFactors = FALSE
  )
  report <- structure(list(), class = "dataquieR_resultset2")
  attr(report, "meta_data") <- meta_data
  attr(report, "label_col") <- LABEL

  report_for_view <- util_prepare_report_labels_for_view(report)
  view_meta_data <- util_attr(report_for_view, "meta_data", exact = TRUE)

  expect_identical(util_attr(report, "meta_data", exact = TRUE), meta_data)
  expect_equal(
    view_meta_data[[LABEL]],
    c(
      "Questionnaire item",
      "Follow-up item",
      "v00018: EDUCATION",
      "v01018: EDUCATION"
    )
  )
  expect_equal(
    view_meta_data[[LONG_LABEL]],
    c(
      "Long questionnaire item",
      "Long follow-up item",
      "Education baseline",
      "Education follow-up"
    )
  )
  expect_equal(
    view_meta_data[["LABEL_DE"]],
    c(
      "Deutsches Item",
      "Deutsches Follow-up-Item",
      "Bildung Baseline",
      "Bildung Follow-up"
    )
  )
})

test_that("report display-label adjustment honors its option", {
  skip_on_cran()

  meta_data <- data.frame(
    VAR_NAMES = "scaleA1",
    LABEL = "scaleA1: Questionnaire item",
    stringsAsFactors = FALSE
  )
  report <- structure(list(), class = "dataquieR_resultset2")
  attr(report, "meta_data") <- meta_data
  attr(report, "label_col") <- LABEL

  report_for_view <- withr::with_options(
    list(dataquieR.fix_var_name_prefixes_label = FALSE),
    util_prepare_report_labels_for_view(report)
  )

  expect_identical(
    util_attr(report_for_view, "meta_data", exact = TRUE),
    meta_data
  )
})

test_that("report-by display labels use global collision decisions", {
  skip_on_cran()

  full_meta_data <- data.frame(
    VAR_NAMES = c("scaleA1", "v00018", "v01018"),
    LABEL = c(
      "scaleA1: Questionnaire item",
      "v00018: EDUCATION",
      "v01018: EDUCATION"
    ),
    stringsAsFactors = FALSE
  )
  make_subreport <- function(row) {
    report <- structure(list(), class = "dataquieR_resultset2")
    attr(report, "meta_data") <- full_meta_data[row, , drop = FALSE]
    attr(report, "label_col") <- LABEL
    report
  }

  first_subreport <- util_prepare_report_labels_for_view(
    make_subreport(c(1, 2)),
    reference_meta_data = full_meta_data
  )
  second_subreport <- util_prepare_report_labels_for_view(
    make_subreport(3),
    reference_meta_data = full_meta_data
  )
  first_subreport_after_print_prep <-
    util_prepare_report_labels_for_view(first_subreport)

  expect_equal(
    util_attr(first_subreport, "meta_data", exact = TRUE)[[LABEL]],
    c("Questionnaire item", "v00018: EDUCATION")
  )
  expect_equal(
    util_attr(second_subreport, "meta_data", exact = TRUE)[[LABEL]],
    "v01018: EDUCATION"
  )
  expect_identical(first_subreport_after_print_prep, first_subreport)
  expect_identical(
    full_meta_data[[LABEL]],
    c(
      "scaleA1: Questionnaire item",
      "v00018: EDUCATION",
      "v01018: EDUCATION"
    )
  )
})
