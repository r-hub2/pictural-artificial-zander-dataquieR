test_that("nres works", {
  skip_on_cran() # slow, errors unlikely
  skip_if_not_installed("stringdist")

  study_data <- data.frame(
    id = 1:4,
    x = c(1L, 2L, NA_integer_, 4L),
    y = c("a", "b", "a", NA),
    stringsAsFactors = FALSE
  )
  meta_data <- data.frame(
    VAR_NAMES = c("id", "x", "y"),
    LABEL = c("id", "x", "y"),
    DATA_TYPE = c(DATA_TYPES$INTEGER, DATA_TYPES$INTEGER, DATA_TYPES$STRING),
    SCALE_LEVEL = c(
      SCALE_LEVELS$NOMINAL,
      SCALE_LEVELS$RATIO,
      SCALE_LEVELS$NOMINAL
    ),
    MISSING_LIST = c(SPLIT_CHAR, SPLIT_CHAR, SPLIT_CHAR),
    JUMP_LIST = c(SPLIT_CHAR, SPLIT_CHAR, SPLIT_CHAR),
    stringsAsFactors = FALSE
  )

  report <-
    suppressWarnings(suppressMessages(dq_report2(
      study_data = study_data,
      item_level = meta_data,
      dimensions = c("int"),
      label_col = LABEL,
      cores = NULL,
      filter_result_slots = NULL
    )))

  expect_equal(nres(report), 7)
})
