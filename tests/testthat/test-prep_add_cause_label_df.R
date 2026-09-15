test_that("prep_add_cause_label_df labels existing missing and jump lists", {
  skip_on_cran()

  meta_data <- data.frame(
    VAR_NAMES = c("a", "b"),
    DATA_TYPE = c(DATA_TYPES$INTEGER, DATA_TYPES$INTEGER),
    MISSING_LIST = c("-1 = old", "-1 = old"),
    JUMP_LIST = c("-2 = oldjump", "-2 = oldjump"),
    stringsAsFactors = FALSE
  )
  cause_label_df <- data.frame(
    CODE_VALUE = c("-1", "-2", "-3"),
    CODE_LABEL = c("missing a", "jump all", "missing b"),
    CODE_CLASS = c("MISSING", "JUMP", "MISSING"),
    resp_vars = c("a", "", "b"),
    stringsAsFactors = FALSE
  )

  labeled <- prep_add_cause_label_df(
    meta_data,
    cause_label_df,
    replace_meta_data = FALSE,
    assume_consistent_codes = FALSE
  )

  expect_equal(labeled[[MISSING_LIST]], c(
    "-1 = missing a",
    "-1 = MISSING b -1"
  ))
  expect_equal(labeled[[JUMP_LIST]], c("-2 = jump all", "-2 = jump all"))
})

test_that("prep_add_cause_label_df replaces metadata from scoped code rows", {
  skip_on_cran()

  meta_data <- data.frame(
    VAR_NAMES = c("a", "b"),
    DATA_TYPE = c(DATA_TYPES$INTEGER, DATA_TYPES$INTEGER),
    MISSING_LIST = SPLIT_CHAR,
    JUMP_LIST = SPLIT_CHAR,
    stringsAsFactors = FALSE
  )
  cause_label_df <- data.frame(
    CODE_VALUE = c("-1", "-2", "-3", "-4"),
    CODE_LABEL = c("missing a", "global jump", "missing b", "ignored"),
    CODE_CLASS = c("MISSING", "JUMP", "MISSING", NA),
    resp_vars = c("a", "", "b", "a"),
    stringsAsFactors = FALSE
  )

  replaced <- prep_add_cause_label_df(meta_data, cause_label_df)

  expect_equal(replaced[[MISSING_LIST]], c(
    "-1 = missing a",
    "-3 = missing b"
  ))
  expect_equal(replaced[[JUMP_LIST]], c("-2 = global jump", "-2 = global jump"))
})

test_that("prep_add_cause_label_df validates scalar control arguments", {
  skip_on_cran()

  meta_data <- data.frame(
    VAR_NAMES = "a",
    DATA_TYPE = DATA_TYPES$INTEGER,
    stringsAsFactors = FALSE
  )
  cause_label_df <- data.frame(
    CODE_VALUE = "-1",
    CODE_LABEL = "missing",
    stringsAsFactors = FALSE
  )

  expect_error(
    prep_add_cause_label_df(meta_data, cause_label_df, label_col = c("a", "b")),
    "Need one character value"
  )
  expect_error(
    prep_add_cause_label_df(
      meta_data,
      cause_label_df,
      label_col = NA_character_
    ),
    "Need one character value"
  )
  expect_error(
    prep_add_cause_label_df(meta_data, cause_label_df, label_col = LABEL),
    "No column"
  )
  expect_error(
    prep_add_cause_label_df(
      meta_data,
      cause_label_df,
      assume_consistent_codes = c(TRUE, FALSE)
    ),
    "Need one logical value"
  )
  expect_error(
    prep_add_cause_label_df(
      meta_data,
      cause_label_df,
      assume_consistent_codes = NA
    ),
    "Need one logical value"
  )
  expect_error(
    prep_add_cause_label_df(
      meta_data,
      cause_label_df,
      replace_meta_data = NA
    ),
    "Need one logical value"
  )
  expect_error(
    prep_add_cause_label_df(
      meta_data,
      cause_label_df,
      replace_meta_data = c(TRUE, FALSE)
    ),
    "Need one logical value"
  )
})

test_that("prep_add_cause_label_df keeps missing absent lists absent", {
  skip_on_cran()

  meta_data <- data.frame(
    VAR_NAMES = c("a", "b"),
    DATA_TYPE = c(DATA_TYPES$INTEGER, DATA_TYPES$INTEGER),
    stringsAsFactors = FALSE
  )
  cause_label_df <- data.frame(
    CODE_VALUE = "-1",
    CODE_LABEL = "missing",
    stringsAsFactors = FALSE
  )

  labeled <- prep_add_cause_label_df(
    meta_data,
    cause_label_df,
    replace_meta_data = FALSE
  )

  expect_equal(labeled[[MISSING_LIST]], c(NA_character_, NA_character_))
  expect_equal(labeled[[JUMP_LIST]], c(NA_character_, NA_character_))
})
