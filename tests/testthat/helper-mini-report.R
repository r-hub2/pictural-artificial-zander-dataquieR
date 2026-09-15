.dq_test_mini_study_data <- function() {
  data.frame(
    x = c(1L, 2L, NA_integer_, 4L, 5L, NA_integer_),
    y = c(1L, 1L, 2L, 2L, NA_integer_, 1L),
    z = c(2L, NA_integer_, 2L, 3L, 3L, 3L)
  )
}

.dq_test_mini_item_level <- function() {
  data.frame(
    VAR_NAMES = c("x", "y", "z"),
    LABEL = c("x", "y", "z"),
    DATA_TYPE = rep(DATA_TYPES$INTEGER, 3),
    SCALE_LEVEL = c(
      SCALE_LEVELS$RATIO,
      SCALE_LEVELS$NOMINAL,
      SCALE_LEVELS$NOMINAL
    ),
    MISSING_LIST = rep(SPLIT_CHAR, 3),
    JUMP_LIST = rep(SPLIT_CHAR, 3),
    stringsAsFactors = FALSE
  )
}

.dq_test_mini_report_summary <- function(resp_vars = c("x", "y", "z")) {
  study_data <- .dq_test_mini_study_data()
  meta_data <- .dq_test_mini_item_level()

  report <- suppressWarnings(suppressMessages(dq_report2(
    study_data = study_data,
    item_level = meta_data,
    resp_vars = resp_vars,
    filter_indicator_functions = "^com_item_missingness$",
    filter_result_slots = "^SummaryTable$",
    cores = NULL,
    dimensions = "Completeness"
  )))

  suppressWarnings(prep_extract_summary(report))
}
