test_that(
  "util_referred_vars collects metadata references and marks them suppressed",
  {
    skip_on_cran()

    vars <- c(
      "main",
      "lookup",
      "covar",
      "cross_ref",
      "limit_ref",
      "computed_ref",
      "manual_id",
      "subgroup_var",
      "strata_var",
      "df_id",
      "shared_id",
      "seg_id"
    )
    meta_data <- data.frame(
      VAR_NAMES = vars,
      LABEL = vars,
      LONG_LABEL = vars,
      ORIGINAL_VAR_NAMES = vars,
      ORIGINAL_LABEL = vars,
      VARIABLE_ROLE = "process",
      KEY_LINK = NA_character_,
      CO_VARS = NA_character_,
      DATAFRAMES = "df1",
      STUDY_SEGMENT = "seg1",
      stringsAsFactors = FALSE
    )
    meta_data$KEY_LINK[meta_data[[VAR_NAMES]] == "main"] <- "lookup"
    meta_data$CO_VARS[meta_data[[VAR_NAMES]] == "main"] <- "covar"

    meta_data_cross_item <- data.frame(
      VARIABLE_LIST = "cross_ref",
      HARD_LIMITS = "[limit_ref] > 0",
      SOFT_LIMITS = NA_character_,
      DETECTION_LIMITS = NA_character_,
      stringsAsFactors = FALSE
    )
    meta_data_item_computation <- data.frame(
      VARIABLE_LIST = "computed_ref",
      stringsAsFactors = FALSE
    )
    meta_data_dataframe <- data.frame(
      DF_CODE = "df1",
      DF_ID_VARS = "df_id | shared_id",
      stringsAsFactors = FALSE
    )
    meta_data_segment <- data.frame(
      STUDY_SEGMENT = "seg1",
      SEGMENT_ID_VARS = "seg_id",
      stringsAsFactors = FALSE
    )

    out <- util_referred_vars(
      resp_vars = "main",
      id_vars = "manual_id",
      vars_in_subgroup = "subgroup_var",
      meta_data = meta_data,
      label_col = LABEL,
      meta_data_segment = meta_data_segment,
      meta_data_dataframe = meta_data_dataframe,
      meta_data_cross_item = meta_data_cross_item,
      meta_data_item_computation = meta_data_item_computation,
      strata_column = "strata_var"
    )

    expect_identical(out$vars_complete, c(
      "main",
      "lookup",
      "covar",
      "cross_ref",
      "limit_ref",
      "computed_ref",
      "strata_var",
      "manual_id",
      "subgroup_var",
      "df_id",
      "shared_id",
      "seg_id"
    ))
    expect_equal(out$md_complete[[VAR_NAMES]], vars)
    expect_equal(
      out$md_complete[[VARIABLE_ROLE]],
      c("process", rep("suppress", length(vars) - 1L))
    )
  }
)

test_that("util_referred_vars reports missing response metadata", {
  skip_on_cran()

  vars <- c("known", "df_id")
  meta_data <- data.frame(
    VAR_NAMES = vars,
    LABEL = vars,
    LONG_LABEL = vars,
    ORIGINAL_VAR_NAMES = vars,
    ORIGINAL_LABEL = vars,
    VARIABLE_ROLE = "process",
    KEY_LINK = NA_character_,
    CO_VARS = NA_character_,
    stringsAsFactors = FALSE
  )
  meta_data_cross_item <- data.frame(
    VARIABLE_LIST = character(),
    HARD_LIMITS = character(),
    SOFT_LIMITS = character(),
    DETECTION_LIMITS = character()
  )
  meta_data_item_computation <- data.frame(VARIABLE_LIST = character())
  meta_data_dataframe <- data.frame(
    DF_ID_VARS = "df_id",
    stringsAsFactors = FALSE
  )

  expect_warning(
    out <- util_referred_vars(
      resp_vars = c("known", "missing_resp"),
      meta_data = meta_data,
      label_col = LABEL,
      meta_data_cross_item = meta_data_cross_item,
      meta_data_item_computation = meta_data_item_computation,
      meta_data_dataframe = meta_data_dataframe
    ),
    "missing_resp"
  )

  expect_equal(out$vars_complete, c("known", "missing_resp", "df_id"))
  expect_equal(out$md_complete[[VAR_NAMES]], vars)
  expect_equal(out$md_complete[[VARIABLE_ROLE]], c("process", "suppress"))
})

test_that("util_variable_references identifies item-level reference columns", {
  skip_on_cran()

  meta_data <- data.frame(
    VAR_NAMES = "main",
    KEY_DEVICE = "device",
    GROUP_VAR_OBSERVER = "observer",
    TIME_VAR = "visit",
    PART_VAR = "part",
    CO_VARS = "age",
    LABEL = "Main",
    OTHER = "ignored",
    stringsAsFactors = FALSE
  )

  expect_identical(
    util_variable_references(meta_data),
    c("KEY_DEVICE", "GROUP_VAR_OBSERVER", "TIME_VAR", "PART_VAR", "CO_VARS")
  )
})
