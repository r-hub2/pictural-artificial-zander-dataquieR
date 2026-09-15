test_that("prep_extract_cause_label_df normalizes typed cause labels", {
  skip_on_cran()

  meta_data <- data.frame(
    VAR_NAMES = c("num", "date", "time"),
    DATA_TYPE = c(
      DATA_TYPES$INTEGER,
      DATA_TYPES$DATETIME,
      DATA_TYPES$TIME
    ),
    MISSING_LIST = c(
      "999 = Not provided",
      "2020-01-01 = Date missing",
      "12:34:56 = Time missing"
    ),
    JUMP_LIST = c("888", "2020-02-02", "01:02:03"),
    stringsAsFactors = FALSE
  )

  extracted <- prep_extract_cause_label_df(
    meta_data = meta_data,
    label_col = VAR_NAMES
  )

  expect_equal(
    extracted$meta_data[[MISSING_LIST]],
    c("999", "2020-01-01", "12:34:56")
  )
  expect_equal(
    extracted$meta_data[[JUMP_LIST]],
    c("888", "2020-02-02", "01:02:03")
  )

  cause_label_df <- extracted$cause_label_df
  expect_named(
    cause_label_df,
    c(CODE_VALUE, CODE_LABEL, CODE_CLASS, "resp_vars")
  )
  expect_equal(nrow(cause_label_df), 6)
  expect_equal(
    cause_label_df[[CODE_VALUE]],
    c("888", "2020-02-02", "01:02:03", "999", "2020-01-01", "12:34:56")
  )
  expect_equal(
    cause_label_df[[CODE_CLASS]],
    c(rep("JUMP", 3), rep("MISSING", 3))
  )
  expect_equal(
    cause_label_df[[CODE_LABEL]],
    c(
      "JUMP 888",
      "JUMP 2020-02-02",
      "JUMP 01:02:03",
      "Not provided",
      "Date missing",
      "Time missing"
    )
  )
})

test_that("prep_extract_cause_label_df preserves explicit empty lists", {
  skip_on_cran()

  meta_data <- data.frame(
    VAR_NAMES = "num",
    DATA_TYPE = DATA_TYPES$INTEGER,
    MISSING_LIST = SPLIT_CHAR,
    JUMP_LIST = SPLIT_CHAR,
    stringsAsFactors = FALSE
  )

  extracted <- prep_extract_cause_label_df(
    meta_data = meta_data,
    label_col = VAR_NAMES
  )

  expect_equal(extracted$meta_data[[MISSING_LIST]], SPLIT_CHAR)
  expect_equal(extracted$meta_data[[JUMP_LIST]], SPLIT_CHAR)
  expect_equal(nrow(extracted$cause_label_df), 0)
})

test_that("prep_extract_cause_label_df preserves row alignment for NAs", {
  skip_on_cran()

  meta_data <- data.frame(
    VAR_NAMES = c("empty", "coded"),
    DATA_TYPE = DATA_TYPES$INTEGER,
    MISSING_LIST = c(NA_character_, "999"),
    JUMP_LIST = NA_character_,
    stringsAsFactors = FALSE
  )

  extracted <- expect_silent(prep_extract_cause_label_df(meta_data = meta_data))

  expect_equal(extracted$cause_label_df$CODE_VALUE, "999")
  expect_equal(extracted$cause_label_df$CODE_LABEL, "MISSING 999")
})
