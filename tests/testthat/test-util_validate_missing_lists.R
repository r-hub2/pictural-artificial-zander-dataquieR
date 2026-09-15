test_that("util_validate_missing_lists reports mixed assignment sources", {
  skip_on_cran()

  meta_data <- data.frame(
    VAR_NAMES = c("a", "b"),
    DATA_TYPE = c(DATA_TYPES$INTEGER, DATA_TYPES$INTEGER),
    MISSING_LIST = c("-1 = missing", "-2"),
    JUMP_LIST = c(NA_character_, NA_character_),
    stringsAsFactors = FALSE
  )
  cause_label_df <- data.frame(
    CODE_VALUE = c("-1", NA_character_, "-9"),
    CODE_LABEL = c("missing", "broken", "unused"),
    stringsAsFactors = FALSE
  )
  warning_log <- new.env(parent = emptyenv())
  warning_log$messages <- character()

  result <- withCallingHandlers(
    util_validate_missing_lists(
      meta_data = meta_data,
      cause_label_df = cause_label_df,
      label_col = VAR_NAMES
    ),
    warning = function(w) {
      warning_log$messages <- c(warning_log$messages, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  expect_true(any(grepl("Combining", warning_log$messages, fixed = TRUE)))
  expect_true(any(grepl("assignment notation", warning_log$messages,
        fixed = TRUE)))
  expect_true(any(grepl("Some code labels", warning_log$messages,
        fixed = TRUE)))
  expect_true(any(grepl("not mentioned", warning_log$messages, fixed = TRUE)))
  expect_s3_class(result$cause_label_df, "data.frame")
  expect_true("-2" %in% result$cause_label_df$CODE_VALUE)
})

test_that("util_validate_missing_lists requires minimal metadata columns", {
  skip_on_cran()

  expect_error(
    util_validate_missing_lists(data.frame(VAR_NAMES = "a"),
      label_col = VAR_NAMES),
    "Need at least"
  )
})

test_that("util_validate_missing_lists ignores malformed cause label frames", {
  skip_on_cran()

  meta_data <- data.frame(
    VAR_NAMES = "a",
    DATA_TYPE = DATA_TYPES$INTEGER,
    MISSING_LIST = "-1 = missing",
    stringsAsFactors = FALSE
  )

  expect_message(
    result <- util_validate_missing_lists(
      meta_data = meta_data,
      cause_label_df = data.frame(not_a_code = "-1"),
      label_col = VAR_NAMES,
      suppressWarnings = TRUE
    ),
    "Need columns"
  )

  expect_s3_class(result$cause_label_df, "data.frame")
  expect_true("-1" %in% result$cause_label_df$CODE_VALUE)
})

test_that("util_validate_missing_lists rejects invalid code classes", {
  skip_on_cran()

  meta_data <- data.frame(
    VAR_NAMES = "a",
    DATA_TYPE = DATA_TYPES$INTEGER,
    MISSING_LIST = "-1",
    stringsAsFactors = FALSE
  )
  cause_label_df <- data.frame(
    CODE_VALUE = "-1",
    CODE_LABEL = "missing",
    CODE_CLASS = "OTHER",
    stringsAsFactors = FALSE
  )

  expect_error(
    util_validate_missing_lists(
      meta_data = meta_data,
      cause_label_df = cause_label_df,
      label_col = VAR_NAMES
    ),
    "Only"
  )
})

test_that("util_validate_missing_lists warns for non numeric codes", {
  skip_on_cran()

  meta_data <- data.frame(
    VAR_NAMES = "a",
    DATA_TYPE = DATA_TYPES$INTEGER,
    MISSING_LIST = "abc = missing",
    stringsAsFactors = FALSE
  )

  expect_warning(
    result <- util_validate_missing_lists(
      meta_data = meta_data,
      label_col = VAR_NAMES
    ),
    "not numeric or date/time"
  )

  expect_s3_class(result$cause_label_df, "data.frame")
})

test_that("util_validate_missing_lists skips date parsing for numeric codes", {
  skip_on_cran()

  meta_data <- data.frame(
    VAR_NAMES = "a",
    DATA_TYPE = DATA_TYPES$INTEGER,
    MISSING_LIST = "-999 = missing",
    stringsAsFactors = FALSE
  )

  testthat::local_mocked_bindings(
    util_parse_date = function(...) stop("date parsing should be skipped"),
    util_parse_time = function(...) stop("time parsing should be skipped")
  )

  expect_no_error(
    util_validate_missing_lists(meta_data = meta_data, label_col = VAR_NAMES)
  )
})

test_that("util_validate_missing_lists accepts date-like missing codes", {
  skip_on_cran()

  meta_data <- data.frame(
    VAR_NAMES = "a",
    DATA_TYPE = DATA_TYPES$DATE,
    MISSING_LIST = "2020-01-01 = missing",
    stringsAsFactors = FALSE
  )

  expect_no_warning(
    util_validate_missing_lists(meta_data = meta_data, label_col = VAR_NAMES)
  )
})

test_that("util_validate_missing_lists warns for duplicate code meanings", {
  skip_on_cran()

  meta_data <- data.frame(
    VAR_NAMES = c("a", "b"),
    DATA_TYPE = c(DATA_TYPES$INTEGER, DATA_TYPES$INTEGER),
    MISSING_LIST = c("-1 = missing a", "-1 = missing b"),
    stringsAsFactors = FALSE
  )

  warnings <- new.env(parent = emptyenv())
  warnings$messages <- character()
  result <- withCallingHandlers(
    util_validate_missing_lists(
      meta_data = meta_data,
      assume_consistent_codes = TRUE,
      label_col = VAR_NAMES
    ),
    warning = function(w) {
      warnings$messages <- c(warnings$messages, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  expect_true(any(grepl("more than one meaning", warnings$messages,
        fixed = TRUE)))
  expect_s3_class(result$cause_label_df, "data.frame")
})

test_that("util_validate_missing_lists can announce code expansion", {
  skip_on_cran()

  meta_data <- data.frame(
    VAR_NAMES = c("a", "b"),
    DATA_TYPE = c(DATA_TYPES$INTEGER, DATA_TYPES$INTEGER),
    MISSING_LIST = c("99 = same missing reason", "99 = same missing reason"),
    stringsAsFactors = FALSE
  )

  expect_message(
    result <- withCallingHandlers(
      util_validate_missing_lists(
        meta_data = meta_data,
        expand_codes = TRUE,
        label_col = VAR_NAMES
      ),
      warning = function(w) invokeRestart("muffleWarning")
    ),
    "Would use label"
  )

  expect_s3_class(result$cause_label_df, "data.frame")
})
