test_that("util_get_code_list works", {
  skip_on_cran()

  withr::local_options(
    dataquieR.CONDITIONS_WITH_STACKTRACE = TRUE,
    dataquieR.ERRORS_WITH_CALLER = TRUE,
    dataquieR.WARNINGS_WITH_CALLER = TRUE,
    dataquieR.MESSAGES_WITH_CALLER = TRUE
  )
  mdf <- prep_create_meta(
    VAR_NAMES = c("age", "sex"),
    DATA_TYPE = c(DATA_TYPES$INTEGER, DATA_TYPES$INTEGER),
    MISSING_LIST = c(NA, "-1 = -|-2 = -|-5 = -"),
    JUMP_LIST = c("999 = -", "")
  )
  expect_warning(
    expect_equal(
      util_get_code_list(c("age"), "MISSING_LIST",
        split_char = SPLIT_CHAR,
        mdf = mdf,
        label_col = VAR_NAMES,
        warning_if_no_list = TRUE
      ),
      numeric(0)
    ),
    perl = TRUE,
    regexp = paste0(
      "Could not find .MISSING_LIST. for",
      " .age. in the meta_data for replacing codes with NAs."
    )
  )
  expect_equal(
    util_get_code_list(c("sex"), "MISSING_LIST",
      split_char = SPLIT_CHAR,
      mdf = mdf,
      label_col = VAR_NAMES,
      warning_if_no_list = TRUE
    ),
    c(`-` = -1, `-` = -2, `-` = -5)
  )
  expect_equal(
    util_get_code_list(c("sex"), "JUMP_LIST",
      split_char = SPLIT_CHAR,
      mdf = mdf,
      label_col = VAR_NAMES,
      warning_if_no_list = TRUE
    ),
    setNames(numeric(0), character(0))
  )
  expect_equal(
    util_get_code_list(c("age"), "JUMP_LIST",
      split_char = SPLIT_CHAR,
      mdf = mdf,
      label_col = VAR_NAMES,
      warning_if_no_list = TRUE
    ),
    c(`-` = 999)
  )
  expect_warning(
    util_get_code_list(c("age"), "XJUMP_LIST",
      split_char = SPLIT_CHAR,
      mdf = mdf,
      label_col = VAR_NAMES,
      warning_if_no_list = TRUE
    ),
    regexp = paste(
      "Metadata does not provide a column called .*XJUMP_LIST.*",
      "for replacing codes with NAs."
    ),
    perl = TRUE
  )
  expect_warning(
    util_get_code_list(c("age"), "JUMP_LIST",
      split_char = SPLIT_CHAR,
      mdf = mdf,
      label_col = "xx",
      warning_if_no_list = TRUE
    ),
    regexp = paste(
      "Metadata does not provide a column called .+xx.+",
      "for the labels."
    ),
    perl = TRUE
  )
  # Use util_get_code_list() locally to inspect combined age/sex JUMP_LISTs.
})

test_that("util_get_code_list warns about non-numeric codes", {
  skip_on_cran()

  withr::local_options(
    dataquieR.CONDITIONS_WITH_STACKTRACE = TRUE,
    dataquieR.ERRORS_WITH_CALLER = TRUE,
    dataquieR.WARNINGS_WITH_CALLER = TRUE,
    dataquieR.MESSAGES_WITH_CALLER = TRUE
  )
  mdf <- prep_create_meta(
    VAR_NAMES = c("age", "sex"),
    DATA_TYPE = c(DATA_TYPES$INTEGER, DATA_TYPES$INTEGER),
    MISSING_LIST = c(NA, NA),
    JUMP_LIST = c("999=-", "")
  )
  mdf$MISSING_LIST[2] <- "-1|-2|-5|x"
  expect_warning(
    expect_equal(
      util_get_code_list(c("sex"), "MISSING_LIST",
        split_char = SPLIT_CHAR,
        mdf = mdf,
        label_col = VAR_NAMES,
        warning_if_no_list = TRUE
      ),
      c(`-1` = -1, `-2` = -2, `-5` = -5)
    ),
    perl = TRUE,
    regexp = paste(
      "Some codes ..MISSING_LIST.. were",
      "not numeric/assignment for .sex.: .x., these will be ignored"
    )
  )
})

test_that("util_get_code_list recognizes no code on purpose", {
  skip_on_cran()
  mdf <- prep_create_meta(
    VAR_NAMES = c("age", "sex"),
    DATA_TYPE = c(DATA_TYPES$INTEGER, DATA_TYPES$INTEGER),
    MISSING_LIST = c(SPLIT_CHAR, SPLIT_CHAR),
    JUMP_LIST = c("999 = ", "")
  )
  expect_silent(expect_equal(
    util_get_code_list(c("sex"), "MISSING_LIST",
      split_char = SPLIT_CHAR,
      mdf = mdf,
      label_col = VAR_NAMES,
      warning_if_no_list = TRUE
    ),
    setNames(numeric(0), character(0))
  ))
})

test_that("util_get_code_list parses date, time, and legacy value labels", {
  skip_on_cran()

  mdf <- data.frame(
    VAR_NAMES = c("date_var", "time_var", "cat_var"),
    DATA_TYPE = c(DATA_TYPES$DATETIME, DATA_TYPES$TIME, DATA_TYPES$INTEGER),
    MISSING_LIST = c(
      "2020-01-01 = old | bad = bad",
      "12:34:56 = noon | bad = bad",
      ""
    ),
    VALUE_LABELS = c("", "", "1 = One < 2 = Two"),
    stringsAsFactors = FALSE
  )

  expect_warning(
    date_codes <- util_get_code_list("date_var", MISSING_LIST, mdf = mdf),
    "not datetime/assignment"
  )
  expect_equal(format(unname(date_codes), "%Y-%m-%d"), "2020-01-01")
  expect_equal(names(date_codes), "old")

  expect_warning(
    time_codes <- util_get_code_list("time_var", MISSING_LIST, mdf = mdf),
    "not time/assignment"
  )
  expect_equal(as.numeric(unname(time_codes)), 12 * 3600 + 34 * 60 + 56)
  expect_equal(names(time_codes), "noon")

  value_labels <- util_get_code_list("cat_var", VALUE_LABELS, mdf = mdf)
  expect_equal(value_labels, c(One = 1, Two = 2))
})

test_that("util_get_combined_code_lists labels bare codes by source", {
  skip_on_cran()
  mdf <- prep_create_meta(
    VAR_NAMES = "sex",
    DATA_TYPE = DATA_TYPES$INTEGER,
    MISSING_LIST = "-1|-2",
    JUMP_LIST = "999"
  )

  expect_equal(
    util_get_combined_code_lists(
      "sex",
      c(MISSING_LIST, JUMP_LIST),
      mdf = mdf,
      label_col = VAR_NAMES
    ),
    c(`MISSING -1` = "-1", `MISSING -2` = "-2", `JUMP 999` = "999")
  )
  expect_equal(
    util_get_combined_code_lists(
      "sex",
      MISSING_LIST,
      mdf = mdf,
      label_col = VAR_NAMES,
      assume_consistent_codes = FALSE
    ),
    c(`MISSING sex -1` = "-1", `MISSING sex -2` = "-2")
  )
  expect_equal(
    util_get_combined_code_lists(
      "sex",
      MISSING_LIST,
      mdf = mdf,
      label_col = VAR_NAMES,
      have_cause_label_df = TRUE
    ),
    c(`-1` = "-1", `-2` = "-2")
  )
})
