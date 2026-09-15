test_that("int_part_vars_structure reports damaged participation hierarchy", {
  skip_on_cran()

  study_data <- data.frame(
    part_exam = c(1, 0, 1),
    part_lab = c(1, 1, 0),
    value = c(10, 20, 30)
  )
  meta_data <- data.frame(
    VAR_NAMES = c("part_exam", "part_lab", "value"),
    LABEL = c("Exam participation", "Lab participation", "Value"),
    DATA_TYPE = c(
      DATA_TYPES$INTEGER,
      DATA_TYPES$INTEGER,
      DATA_TYPES$INTEGER
    ),
    SCALE_LEVEL = c(
      SCALE_LEVELS$NOMINAL,
      SCALE_LEVELS$NOMINAL,
      SCALE_LEVELS$RATIO
    ),
    PART_VAR = c(NA_character_, "part_exam", "part_lab"),
    STUDY_SEGMENT = c("Exam", "Lab", "Lab"),
    VARIABLE_ROLE = "primary",
    MISSING_LIST = SPLIT_CHAR,
    JUMP_LIST = SPLIT_CHAR,
    stringsAsFactors = FALSE
  )

  result <- suppressWarnings(int_part_vars_structure(
    study_data = study_data,
    meta_data = meta_data,
    label_col = VAR_NAMES,
    disclose_problem_paprt_var_data = TRUE
  ))

  expect_type(result, "list")
  expect_length(result, 0)
})

test_that("int_part_vars_structure reports missing participation metadata", {
  skip_on_cran()

  study_data <- data.frame(
    part_exam = c(1, 1),
    value = c(10, 20)
  )
  meta_without_part_var <- data.frame(
    VAR_NAMES = c("part_exam", "value"),
    LABEL = c("Exam participation", "Value"),
    DATA_TYPE = c(DATA_TYPES$INTEGER, DATA_TYPES$INTEGER),
    SCALE_LEVEL = c(SCALE_LEVELS$NOMINAL, SCALE_LEVELS$RATIO),
    STUDY_SEGMENT = c("Exam", "Exam"),
    VARIABLE_ROLE = "primary",
    MISSING_LIST = SPLIT_CHAR,
    JUMP_LIST = SPLIT_CHAR,
    stringsAsFactors = FALSE
  )

  expect_warning(
    int_part_vars_structure(
      study_data = study_data,
      meta_data = meta_without_part_var,
      label_col = VAR_NAMES
    ),
    "variables w/o 'PART_VARS'",
    fixed = TRUE
  )

  meta_with_unknown_part_var <- meta_without_part_var
  meta_with_unknown_part_var[[PART_VAR]] <- c(NA_character_, "part_missing")

  warnings <- character()
  withCallingHandlers(
    int_part_vars_structure(
      study_data = study_data,
      meta_data = meta_with_unknown_part_var,
      label_col = VAR_NAMES
    ),
    warning = function(cond) {
      warnings <<- c(warnings, conditionMessage(cond))
      invokeRestart("muffleWarning")
    }
  )

  expect_true(any(grepl(
    "Missing 1 'PART_VARS' from the 'meta_data'",
    warnings,
    fixed = TRUE
  )))
  expect_true(any(grepl(
    "Missing 1 'PART_VARS' from the 'study_data'",
    warnings,
    fixed = TRUE
  )))
})
