test_that("prep_get_labels handles omitted variables and search guardrails", {
  skip_on_cran()

  meta_data <- prep_create_meta(
    VAR_NAMES = c("AGE", "SEX"),
    LABEL = c("Age", "Sex"),
    DATA_TYPE = c(DATA_TYPES$INTEGER, DATA_TYPES$INTEGER),
    MISSING_LIST = ""
  )

  labels <- prep_get_labels(item_level = meta_data)

  expect_equal(labels, c(AGE = "Age", SEX = "Sex"), ignore_attr = TRUE)
  expect_identical(
    util_attr(labels, "label_col", exact = TRUE),
    as.character(LABEL)
  )

  expect_error(
    prep_get_labels(
      "AGE",
      item_level = meta_data,
      meta_data = meta_data[2, , drop = FALSE]
    ),
    "You cannot provide both"
  )

  expect_error(
    prep_get_labels(
      "AGE",
      item_level = meta_data,
      resp_vars_are_var_names_only = TRUE,
      resp_vars_match_label_col_only = TRUE
    ),
    "Call with either"
  )
})

test_that("prep_get_labels falls back to variable names without labels", {
  skip_on_cran()

  meta_data <- data.frame(
    VAR_NAMES = c("AGE", "SEX"),
    DATA_TYPE = c(DATA_TYPES$INTEGER, DATA_TYPES$INTEGER),
    stringsAsFactors = FALSE
  )

  labels <- suppressWarnings(prep_get_labels(
    c("AGE", "SEX"),
    item_level = meta_data,
    force_label_col = "FALSE"
  ))

  expect_equal(labels, c(AGE = "AGE", SEX = "SEX"), ignore_attr = TRUE)
  expect_identical(
    util_attr(labels, "label_col", exact = TRUE),
    as.character(LABEL)
  )
})

test_that("prep_get_labels keeps metadata labels unchanged", {
  skip_on_cran()
  expect_true(dataquieR.fix_var_name_prefixes_label_default)

  meta_data <- data.frame(
    VAR_NAMES = c("scaleA1", "scaleA2", "scaleA3"),
    LABEL = c(
      "scaleA1: scaleA1 Questionnaire item",
      "scaleA2 - Follow-up item",
      "scaleA30 remains a distinct label"
    ),
    DATA_TYPE = rep(DATA_TYPES$INTEGER, 3),
    stringsAsFactors = FALSE
  )

  labels <- prep_get_labels(
    c("scaleA1", "scaleA2", "scaleA3"),
    item_level = meta_data,
    force_label_col = "FALSE"
  )

  expect_equal(
    labels,
    c(
      scaleA1 = "scaleA1: scaleA1 Questionnaire item",
      scaleA2 = "scaleA2 - Follow-up item",
      scaleA3 = "scaleA30 remains a distinct label"
    ),
    ignore_attr = TRUE
  )
})

test_that("prep_get_labels keeps prefixes needed for unique display labels", {
  skip_on_cran()

  meta_data <- data.frame(
    VAR_NAMES = c("v00018", "v01018"),
    LABEL = c("v00018: EDUCATION", "v01018: EDUCATION"),
    DATA_TYPE = rep(DATA_TYPES$INTEGER, 2),
    stringsAsFactors = FALSE
  )

  labels <- prep_get_labels(
    c("v00018", "v01018"),
    item_level = meta_data,
    force_label_col = "FALSE"
  )

  expect_equal(
    labels,
    c(v00018 = "v00018: EDUCATION", v01018 = "v01018: EDUCATION"),
    ignore_attr = TRUE
  )
})
