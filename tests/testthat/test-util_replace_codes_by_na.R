test_that("util_replace_codes_by_na works", {
  skip_on_cran()
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  local({
    meta_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data.RData") # nolint: line_length_linter.
    study_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData", keep_types = TRUE) # nolint: line_length_linter.
    sd0 <- study_data[1:30, 30:35, FALSE]
    md0 <- meta_data[meta_data$VAR_NAMES %in% colnames(sd0), , FALSE]
    expected <- structure(list(v00024 = c(
      0, 1, 0, NA, NA,
      1, 0, 0, 1, 0, NA, 1, 1, 0, 0, 1, NA, 0, 1, 0, NA, 0, 0, 0, 1,
      0, NA, 1, 1, 0
    ), v00025 = c(
      NA, 1, NA, NA, NA, NA, NA, NA, 4,
      NA, NA, 3, 0, NA, NA, 0, NA, NA, NA, NA, NA, NA, NA, NA, 1, NA,
      NA, 3, 3, NA
    ), v00026 = c(
      3, 7, 4, 6, NA, 11, 2, 7, 3, 8, NA,
      6, 7, 7, 4, 2, NA, NA, 3, 5, NA, 3, NA, 7, 8, 11, NA, 4, 7, 8
    ), v00027 = c(
      NA, 4, 3, NA, NA, NA, 4, NA, 3, 6, NA, NA, 2, 4,
      4, 2, NA, NA, NA, NA, NA, 2, NA, 2, NA, NA, NA, 4, NA, 2
    ), v00028 = c(
      1,
      2, 0, 2, NA, 2, 1, 3, 1, 1, NA, 2, 0, 2, 0, NA, NA, 1, 4, 1,
      NA, 1, 3, 5, 3, 3, NA, NA, 3, NA
    ), v00029 = c(
      0, 0, NA, 0, NA,
      NA, NA, NA, 0, 0, NA, NA, 0, 0, 1, 0, NA, NA, NA, NA, NA, 0,
      NA, 0, 1, NA, NA, 0, NA, 0
    )), row.names = c(NA, 30L), class = "data.frame", Codes_to_NA = TRUE)
    got <- dataquieR:::util_replace_codes_by_NA(
      study_data = sd0, meta_data = md0
    )
    expect_equal(got, expected)
  })
})

test_that("util_replace_codes_by_NA handles local missing and jump codes", {
  skip_on_cran()

  study_data <- data.frame(
    age = c(20, 999, 30, 888),
    score = c(1, -1, -2, 3)
  )
  attr(study_data, "MAPPED") <- TRUE
  meta_data <- data.frame(
    VAR_NAMES = c("age", "score"),
    DATA_TYPE = c(DATA_TYPES$INTEGER, DATA_TYPES$INTEGER),
    MISSING_LIST = c("999 = missing", "-1 = missing"),
    JUMP_LIST = c("888 = jump", "-2 = jump"),
    stringsAsFactors = FALSE
  )

  replaced <- util_replace_codes_by_NA(study_data, meta_data)

  expect_equal(replaced$age, c(20, NA, 30, NA))
  expect_equal(replaced$score, c(1, NA, NA, 3))
  expect_true(util_attr(replaced, "Codes_to_NA", exact = TRUE))
  expect_true(util_attr(replaced, "MAPPED", exact = TRUE))
})

test_that("util_replace_codes_by_NA applies shared missing code fallback", {
  skip_on_cran()

  study_data <- data.frame(age = c(20, 777, 30))
  meta_data <- data.frame(
    VAR_NAMES = "age",
    DATA_TYPE = DATA_TYPES$INTEGER,
    MISSING_LIST = "999 = missing",
    JUMP_LIST = "888 = jump",
    stringsAsFactors = FALSE
  )

  replaced <- util_replace_codes_by_NA(
    study_data,
    meta_data,
    sm_code = 777
  )

  expect_equal(replaced$age, c(20, NA, 30))
  expect_true(util_attr(replaced, "Codes_to_NA", exact = TRUE))
})

test_that("util_replace_codes_by_NA respects study-data label columns", {
  skip_on_cran()

  study_data <- data.frame(age = c(20, 999, 30))
  attr(study_data, "label_col") <- LABEL
  meta_data <- data.frame(
    VAR_NAMES = "age_source",
    LABEL = "age",
    DATA_TYPE = DATA_TYPES$INTEGER,
    MISSING_LIST = "999 = missing",
    JUMP_LIST = SPLIT_CHAR,
    stringsAsFactors = FALSE
  )

  replaced <- util_replace_codes_by_NA(study_data, meta_data)

  expect_equal(replaced$age, c(20, NA, 30))
  expect_true(util_attr(replaced, "Codes_to_NA", exact = TRUE))
})

test_that("util_replace_codes_by_NA warns for missing code-list columns", {
  skip_on_cran()

  study_data <- data.frame(age = c(20, 30))
  meta_data <- data.frame(
    VAR_NAMES = "age",
    DATA_TYPE = DATA_TYPES$INTEGER,
    stringsAsFactors = FALSE
  )

  expect_warning(
    expect_warning(
      replaced <- util_replace_codes_by_NA(study_data, meta_data),
      regexp = "MISSING_LIST"
    ),
    regexp = "JUMP_LIST"
  )

  expect_equal(replaced$age, study_data$age)
  expect_true(util_attr(replaced, "Codes_to_NA", exact = TRUE))
})
