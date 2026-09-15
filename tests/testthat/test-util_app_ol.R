test_that("util_app_ol works", {
  skip_on_cran() # deprecated
  md <- prep_create_meta(
    VAR_NAMES = letters,
    DATA_TYPE = c(
      rep(DATA_TYPES$FLOAT, 13), rep(DATA_TYPES$INTEGER, 10),
      DATA_TYPES$STRING, DATA_TYPES$DATETIME, DATA_TYPES$STRING
    ),
    MISSING_LIST = "",
    GROUP_VAR_OBSERVER = "a",
    DETECTION_LIMITS =
      c(rep("[0;9)", 10), rep(NA, 6), rep("", 10))
  )
  expect_equal(
    util_app_ol(md, as.factor(rep(1, nrow(md)))),
    as.factor(c(rep(3, 23), rep(4, 3)))
  )
})

test_that("util_app_ol handles minimal metadata and labelled integers", {
  skip_on_cran() # deprecated

  md <- data.frame(
    DATA_TYPE = c(
      DATA_TYPES$FLOAT,
      DATA_TYPES$INTEGER,
      DATA_TYPES$INTEGER,
      DATA_TYPES$STRING
    ),
    VALUE_LABELS = c(NA, NA, "1 = Yes", NA),
    VALUE_LABEL_TABLE = c(NA, NA, NA, NA),
    STANDARDIZED_VOCABULARY_TABLE = c(NA, NA, NA, NA),
    stringsAsFactors = FALSE
  )

  expect_identical(
    as.character(util_app_ol(md, c(0, 1, 1, 1))),
    c("1", "3", "4", "4")
  )

  md_minimal <- data.frame(
    DATA_TYPE = DATA_TYPES$INTEGER,
    stringsAsFactors = FALSE
  )

  expect_identical(as.character(util_app_ol(md_minimal, 0)), "1")
})
