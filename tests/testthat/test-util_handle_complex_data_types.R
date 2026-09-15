test_that("complex_data_types", {
  skip_if_not_installed("DT")
  skip_if_not_installed("markdown")
  skip_if_not_installed("stringdist")

  skip_on_cran() # slow
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  study_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData", # nolint: line_length_linter.
    keep_types = TRUE
  )

  prep_load_workbook_like_file("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx") # nolint: line_length_linter.
  patch_legacy_fortests_repeated_measurements()
  meta_data <- prep_get_data_frame("item_level")

  meta_data[[DATA_TYPE]][meta_data[[LABEL]] == "N_CHILD_0"] <- "cOuNt"
  meta_data[[SCALE_LEVEL]][meta_data[[LABEL]] == "N_CHILD_0"] <- ""

  # dq_report2 ----

  r <- dq_report2(
    resp_vars = "N_CHILD_0",
    study_data = study_data, label_col = LABEL, meta_data = meta_data,
    dimensions = NULL,
    cores = NULL,
    meta_data_v2 =
      "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx" # nolint: line_length_linter.
  )

  repsum <- summary(r)
  report_meta_data <- util_attr(r, "meta_data", exact = TRUE)
  expect_snapshot(
    report_meta_data[report_meta_data[[LABEL]] == "N_CHILD_0", , FALSE]
  )

  expect_snapshot(repsum)

  # Call one function, only ----
  r <- acc_distributions(
    resp_vars = "N_CHILD_0",
    study_data = study_data, label_col = LABEL, meta_data = meta_data,
    meta_data_v2 =
      "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx" # nolint: line_length_linter.
  )

  skip_if_not_installed("vdiffr")
  expect_doppelganger2(
    "EXTENDED_DATA_TYPE cnt dist",
    r$SummaryPlotList$N_CHILD_0
  )

  # use pre-filled columns, such as SCALE_LEVEL, EXTENDED_DATA_TYPE, ... ----

  meta_data[[HARD_LIMITS]][meta_data[[LABEL]] == "N_CHILD_0"] <-
    "[0; 7]"
  r <- acc_distributions(
    resp_vars = "N_CHILD_0",
    study_data = study_data, label_col = LABEL, meta_data = meta_data,
    meta_data_v2 =
      "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx" # nolint: line_length_linter.
  )

  skip_if_not_installed("vdiffr")
  expect_doppelganger2(
    "EXTENDED_DATA_TYPE cnt dist2",
    r$SummaryPlotList$N_CHILD_0
  )
})

test_that("util_handle_complex_data_types normalizes count metadata locally", {
  skip_on_cran()

  meta_data <- data.frame(
    VAR_NAMES = c("n_children", "comment"),
    DATA_TYPE = c(" cOuNt ", DATA_TYPES$STRING),
    stringsAsFactors = FALSE
  )

  handled <- util_handle_complex_data_types(meta_data)

  expect_identical(handled[[DATA_TYPE]], c(
    DATA_TYPES$INTEGER,
    DATA_TYPES$STRING
  ))
  expect_identical(handled[[SCALE_LEVEL]], c(SCALE_LEVELS$RATIO, NA))
  expect_identical(handled[[HARD_LIMITS]], c("[0; Inf)", NA))
  expect_identical(handled[[EXTENDED_DATA_TYPE]], c("count", NA))
})

test_that("util_handle_complex_data_types keeps empty metadata empty", {
  skip_on_cran()

  meta_data <- data.frame(
    VAR_NAMES = character(),
    DATA_TYPE = character(),
    stringsAsFactors = FALSE
  )

  handled <- util_handle_complex_data_types(meta_data)

  expect_equal(handled, meta_data)
})

test_that("util_handle_complex_data_types keeps compatible count metadata", {
  skip_on_cran()

  meta_data <- data.frame(
    VAR_NAMES = "n_children",
    DATA_TYPE = "count",
    SCALE_LEVEL = SCALE_LEVELS$ORDINAL,
    HARD_LIMITS = "[1; 3]",
    EXTENDED_DATA_TYPE = "other",
    stringsAsFactors = FALSE
  )

  expect_warning(
    handled <- util_handle_complex_data_types(meta_data),
    "Overwriting some entries"
  )

  expect_identical(handled[[DATA_TYPE]], DATA_TYPES$INTEGER)
  expect_identical(handled[[SCALE_LEVEL]], SCALE_LEVELS$ORDINAL)
  expect_identical(handled[[HARD_LIMITS]], "[1; 3]")
  expect_identical(handled[[EXTENDED_DATA_TYPE]], "count")
})
