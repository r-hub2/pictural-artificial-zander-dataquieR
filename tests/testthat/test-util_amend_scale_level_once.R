test_that("util_amend_scale_level_once fills missing scale levels", {
  study_data <- data.frame(a = c(1L, 2L, 3L))
  meta_data <- data.frame(
    VAR_NAMES = "a",
    LABEL = "a",
    DATA_TYPE = DATA_TYPES$INTEGER,
    SCALE_LEVEL = NA_character_,
    MISSING_LIST = "",
    JUMP_LIST = "",
    HARD_LIMITS = "",
    VARIABLE_ROLE = VARIABLE_ROLES$PRIMARY,
    stringsAsFactors = FALSE
  )

  amended <- util_amend_scale_level_once(
    study_data = study_data,
    meta_data = meta_data,
    label_col = LABEL,
    verbose = FALSE
  )

  expect_true(amended$predicted)
  expect_false(util_empty(amended$meta_data[[SCALE_LEVEL]]))
  expect_true(isTRUE(util_attr(
    amended$meta_data,
    "dataquieR_scale_level_predicted",
    exact = TRUE
  )))
})

test_that("util_amend_scale_level_once preserves prediction marker", {
  study_data <- data.frame(a = c(1L, 2L, 3L))
  meta_data <- data.frame(
    VAR_NAMES = "a",
    LABEL = "a",
    DATA_TYPE = DATA_TYPES$INTEGER,
    SCALE_LEVEL = SCALE_LEVELS$RATIO,
    stringsAsFactors = FALSE
  )
  attr(meta_data, "dataquieR_scale_level_predicted") <- TRUE

  testthat::local_mocked_bindings(
    prep_prepare_dataframes = function(...) {
      stop("complete scale-level metadata should not be amended again")
    }
  )

  amended <- util_amend_scale_level_once(
    study_data = study_data,
    meta_data = meta_data,
    label_col = LABEL,
    verbose = FALSE
  )

  expect_true(amended$predicted)
  expect_equal(amended$meta_data[[SCALE_LEVEL]], SCALE_LEVELS$RATIO)
})

test_that("util_amend_scale_level_once does not estimate complete scale levels", { # nolint: line_length_linter.
  study_data <- data.frame(a = c(1L, 2L, 3L))
  meta_data <- data.frame(
    VAR_NAMES = "a",
    LABEL = "a",
    DATA_TYPE = DATA_TYPES$INTEGER,
    SCALE_LEVEL = SCALE_LEVELS$RATIO,
    stringsAsFactors = FALSE
  )

  testthat::local_mocked_bindings(
    prep_prepare_dataframes = function(...) {
      stop("complete scale-level metadata should not be amended")
    }
  )

  amended <- util_amend_scale_level_once(
    study_data = study_data,
    meta_data = meta_data,
    label_col = LABEL,
    verbose = FALSE
  )

  expect_false(amended$predicted)
  expect_equal(amended$meta_data[[SCALE_LEVEL]], SCALE_LEVELS$RATIO)
})
