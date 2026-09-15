test_that("util_app_iav works", {
  skip_on_cran() # deprecated

  # Create a test metadata set
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

  md_hl <- prep_create_meta(
    VAR_NAMES = letters,
    DATA_TYPE = c(
      rep(DATA_TYPES$FLOAT, 13), rep(DATA_TYPES$INTEGER, 10),
      DATA_TYPES$STRING, DATA_TYPES$DATETIME, DATA_TYPES$STRING
    ),
    MISSING_LIST = "",
    HARD_LIMITS = c(rep("[0;8]", 8), rep(NA, 6), rep("", 12))
  )

  md_sl <- prep_create_meta(
    VAR_NAMES = letters,
    DATA_TYPE = c(
      rep(DATA_TYPES$FLOAT, 13), rep(DATA_TYPES$INTEGER, 10),
      DATA_TYPES$STRING, DATA_TYPES$DATETIME, DATA_TYPES$STRING
    ),
    MISSING_LIST = "",
    SOFT_LIMITS = c(rep("[1;5]", 8), rep(NA, 6), rep("", 12))
  )

  result1 <- util_app_iav(md, as.factor(rep(1, nrow(md))))
  result2 <- util_app_iav(md_hl, as.factor(rep(1, nrow(md_hl))))
  result3 <- util_app_iav(md_sl, as.factor(rep(1, nrow(md_sl))))

  # Check that the result is a factor
  expect_s3_class(result1, "factor")

  # Check that the result is as expected
  expect_equal(
    result1,
    as.factor(c(rep(2, 23), 4, 2, 4))
  )
  expect_equal(
    result2,
    as.factor(c(rep(3, 8), rep(2, 6), rep(3, 9), 4, 3, 4))
  )
  expect_equal(
    result3,
    as.factor(c(rep(3, 8), rep(2, 6), rep(3, 9), 4, 3, 4))
  )
})

test_that("util_app_iav scores limits and non-numeric data types", {
  skip_on_cran() # deprecated

  md <- data.frame(
    DATA_TYPE = c(
      DATA_TYPES$FLOAT,
      DATA_TYPES$INTEGER,
      DATA_TYPES$DATETIME,
      DATA_TYPES$STRING
    ),
    HARD_LIMITS = c(NA, "[0;1]", NA, "[0;1]"),
    SOFT_LIMITS = c(NA, NA, "[2020;2021]", NA),
    stringsAsFactors = FALSE
  )

  expect_identical(
    as.character(util_app_iav(md, c(0, 0, 1, 1))),
    c("0", "1", "3", "4")
  )
})

test_that("util_app_iav handles metadata without limits columns", {
  skip_on_cran() # deprecated

  md <- data.frame(
    DATA_TYPE = c(DATA_TYPES$FLOAT, DATA_TYPES$INTEGER),
    stringsAsFactors = FALSE
  )

  expect_identical(
    as.character(util_app_iav(md, c(0, 1))),
    c("0", "2")
  )
})
