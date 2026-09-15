test_that("prep_compare_meta_with_study works", {
  skip_on_cran() # online, fragile
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  prep_load_workbook_like_file("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx") # nolint: line_length_linter.
  patch_legacy_fortests_repeated_measurements()
  sd0 <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData", keep_types = TRUE) # nolint: line_length_linter.

  my_warnings <- capture_warnings({
    res <- prep_compare_meta_with_study(sd0)
  })
  expect_snapshot_value(res, style = "deparse")
  expect_snapshot_value(my_warnings, style = "deparse")
})

test_that("prep_compare_meta_with_study reports local metadata mismatches", {
  skip_on_cran()

  study_data <- data.frame(
    x = 1:3,
    y = c("a", "b", "c"),
    stringsAsFactors = FALSE
  )
  meta_data <- data.frame(
    VAR_NAMES = c("x", "y"),
    DATA_TYPE = c(DATA_TYPES$STRING, DATA_TYPES$INTEGER),
    SCALE_LEVEL = c(SCALE_LEVELS$NOMINAL, SCALE_LEVELS$RATIO),
    MISSING_LIST = c(SPLIT_CHAR, SPLIT_CHAR),
    JUMP_LIST = c(SPLIT_CHAR, SPLIT_CHAR),
    stringsAsFactors = FALSE
  )

  my_warnings <- capture_warnings({
    res <- prep_compare_meta_with_study(
      study_data = study_data,
      meta_data = meta_data,
      label_col = VAR_NAMES
    )
  })

  expect_equal(res$dt_error, c("x", "y"))
  expect_equal(res$sl_error, "y")
  expect_equal(res$ml_error, character(0))
  expect_true(any(grepl(DATA_TYPE, my_warnings, fixed = TRUE)))
  expect_true(any(grepl(SCALE_LEVEL, my_warnings, fixed = TRUE)))
})
