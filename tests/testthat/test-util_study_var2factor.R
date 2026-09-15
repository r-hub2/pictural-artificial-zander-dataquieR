test_that("util_study_var2factor maps missing codes and system missings", {
  skip_on_cran()

  study_data <- data.frame(x = c(1, 2, NA))
  meta_data <- data.frame(
    VAR_NAMES = "x",
    LABEL = "x",
    DATA_TYPE = DATA_TYPES$INTEGER,
    SCALE_LEVEL = SCALE_LEVELS$NOMINAL,
    VALUE_LABELS = NA_character_,
    MISSING_LIST = "1 = missing-code",
    JUMP_LIST = NA_character_,
    stringsAsFactors = FALSE
  )

  converted <- suppressWarnings(util_study_var2factor(
    resp_vars = "x",
    study_data = study_data,
    meta_data = meta_data,
    label_col = VAR_NAMES,
    code_name = MISSING_LIST
  ))

  expect_s3_class(converted$x, "factor")
  expect_identical(as.character(converted$x), c(
    "missing-code",
    NA_character_,
    .SM_LAB
  ))
  expect_true(.SM_LAB %in% levels(converted$x))
})

test_that("util_study_var2factor defaults to all study-data columns", {
  skip_on_cran()

  study_data <- data.frame(
    x = c(1, 3),
    y = c(2, NA)
  )
  meta_data <- data.frame(
    VAR_NAMES = c("x", "y"),
    LABEL = c("x", "y"),
    DATA_TYPE = DATA_TYPES$INTEGER,
    SCALE_LEVEL = SCALE_LEVELS$NOMINAL,
    VALUE_LABELS = NA_character_,
    MISSING_LIST = c("1 = missing-x", "2 = missing-y"),
    JUMP_LIST = NA_character_,
    stringsAsFactors = FALSE
  )

  converted <- suppressWarnings(util_study_var2factor(
    study_data = study_data,
    meta_data = meta_data,
    label_col = VAR_NAMES,
    code_name = MISSING_LIST,
    include_sysmiss = FALSE
  ))

  expect_named(converted, c("x", "y"))
  expect_s3_class(converted$x, "factor")
  expect_s3_class(converted$y, "factor")
  expect_identical(as.character(converted$x), c("missing-x", NA_character_))
  expect_identical(as.character(converted$y), c("missing-y", NA_character_))
  expect_false(.SM_LAB %in% levels(converted$y))
})
