test_that("util_app_iac works", {
  skip_on_cran() # deprecated
  md <- prep_create_meta(
    VAR_NAMES = letters,
    DATA_TYPE = c(
      rep(DATA_TYPES$FLOAT, 13), rep(DATA_TYPES$INTEGER, 10),
      DATA_TYPES$STRING, DATA_TYPES$DATETIME, DATA_TYPES$STRING
    ),
    MISSING_LIST = "",
    DETECTION_LIMITS =
      c(rep("[0;9)", 10), rep(NA, 6), rep("", 10))
  )
  expect_equal(
    util_app_iac(md, as.factor(rep(1, nrow(md)))),
    as.factor(c(rep(4, 13), rep(2, 11), 4, 2))
  )
  md <- prep_create_meta(
    VAR_NAMES = letters,
    DATA_TYPE = c(
      rep(DATA_TYPES$FLOAT, 13), rep(DATA_TYPES$INTEGER, 10),
      DATA_TYPES$STRING, DATA_TYPES$DATETIME, DATA_TYPES$STRING
    ),
    MISSING_LIST = "",
    VALUE_LABELS =
      head(c(rep(
        c(NA_character_, "", "12 = x | 14 = z", "|"),
        ceiling(26 / 4)
      )), 26),
    DETECTION_LIMITS =
      c(rep("[0;9)", 10), rep(NA, 6), rep("", 10))
  )
  testthat::local_edition(3)
  expect_snapshot_value(
    style = "deparse",
    util_app_iac(md, as.factor(c(
      rep(rep(1, nrow(md)), 52),
      rep(rep(0, nrow(md)), 52)
    )))
  )
})

test_that("util_app_iac uses table-based categorical metadata", {
  skip_on_cran()

  meta_data <- data.frame(
    DATA_TYPE = c(DATA_TYPES$INTEGER, DATA_TYPES$STRING, DATA_TYPES$FLOAT),
    VALUE_LABEL_TABLE = c(NA_character_, "labels", NA_character_),
    STANDARDIZED_VOCABULARY_TABLE = c("vocab", NA_character_, "vocab"),
    stringsAsFactors = FALSE
  )

  expect_equal(
    util_app_iac(meta_data, c(1, 0, 1)),
    factor(c(3, 1, 4))
  )

  without_type <- meta_data[, VALUE_LABEL_TABLE, drop = FALSE]
  expect_equal(util_app_iac(without_type, c(1, 1, 1)), factor(c(4, 4, 4)))
})

test_that("util_app_iac scores table-backed categorical metadata", {
  skip_on_cran()
  prep_purge_data_frame_cache()
  withr::defer(prep_purge_data_frame_cache())
  prep_add_data_frames(
    yes_no = data.frame(CODE_VALUE = c(0, 1), CODE_LABEL = c("no", "yes")),
    float_labels = data.frame(CODE_VALUE = c(0, 1), CODE_LABEL = c("no", "yes")), # nolint: line_length_linter.
    append = FALSE
  )

  md <- prep_create_meta(
    VAR_NAMES = c("int_with_table", "string_without_table", "float_table"),
    DATA_TYPE = c(
      DATA_TYPES$INTEGER,
      DATA_TYPES$STRING,
      DATA_TYPES$FLOAT
    ),
    MISSING_LIST = SPLIT_CHAR,
    VALUE_LABEL_TABLE = c("yes_no", NA_character_, "float_labels")
  )

  result <- util_app_iac(md, as.factor(c(1, 1, 1)))

  expect_equal(as.character(result), c("3", "2", "4"))
})
