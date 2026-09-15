skip_on_cran()

test_that("util_filter_repsum keeps study and SSI variables consistently", {
  repsum <- data.frame(
    VAR_NAMES = c("age", "miss_age", "sex"),
    value = c(1, 2, 3),
    stringsAsFactors = FALSE
  )
  meta_data <- data.frame(
    VAR_NAMES = c("age", "miss_age", "sex"),
    LABEL = c("Age", "Missing age", "Sex"),
    COMPUTED_VARIABLE_ROLE = c(NA_character_, "MISS_RESP", ""),
    stringsAsFactors = FALSE
  )
  rownames_of_report <- c("Age", "Missing age", "Sex")

  study_only <- util_filter_repsum(
    repsumtab = repsum,
    vars_to_include = "study",
    meta_data = meta_data,
    rownames_of_report = rownames_of_report,
    label_col = LABEL
  )
  expect_identical(study_only$VAR_NAMES, c("age", "sex"))
  expect_identical(
    attr(study_only, "rownames_of_report"),
    c("Age", "Sex")
  )

  ssi_only <- util_filter_repsum(
    repsumtab = repsum,
    vars_to_include = "ssi",
    meta_data = meta_data,
    rownames_of_report = rownames_of_report,
    label_col = LABEL
  )
  expect_identical(ssi_only$VAR_NAMES, "miss_age")
  expect_identical(attr(ssi_only, "rownames_of_report"), "Missing age")

  both <- util_filter_repsum(
    repsumtab = repsum,
    vars_to_include = c("study", "ssi"),
    meta_data = meta_data,
    rownames_of_report = rownames_of_report,
    label_col = LABEL
  )
  expect_identical(both$VAR_NAMES, repsum$VAR_NAMES)
  expect_identical(attr(both, "rownames_of_report"), rownames_of_report)
})

test_that("util_filter_repsum validates include modes", {
  expect_error(
    util_filter_repsum(
      repsumtab = data.frame(VAR_NAMES = "age"),
      vars_to_include = "invalid",
      meta_data = data.frame(
        VAR_NAMES = "age",
        LABEL = "Age",
        COMPUTED_VARIABLE_ROLE = NA_character_
      ),
      rownames_of_report = "Age",
      label_col = LABEL
    ),
    "invalid"
  )
})

test_that("variable-group results are separated by output type", {
  repsum <- data.frame(
    VAR_NAMES = c(
      "age", "age", "repeated_group", "contradiction_group",
      "scale_missing"
    ),
    call_names = c(
      "com_item_missingness", "acc_repeated_measurements",
      "acc_repeated_measurements", "con_contradictions_redcap",
      "con_ssi_range_check"
    ),
    value = 1:5,
    .variable_group_result_label = c(
      NA, NA, "Repeated group", "Contradiction group", "Scale group"
    ),
    stringsAsFactors = FALSE
  )
  meta_data <- data.frame(
    VAR_NAMES = c("age", "sex", "scale_missing"),
    LABEL = c("Age", "Sex", "Scale missingness"),
    COMPUTED_VARIABLE_ROLE = c(NA_character_, NA_character_, "MISS_RESP"),
    stringsAsFactors = FALSE
  )

  variable_groups <- util_filter_repsum(
    repsumtab = repsum,
    vars_to_include = "variable_group",
    meta_data = meta_data,
    rownames_of_report = meta_data$LABEL,
    label_col = LABEL,
    variable_group_call_names = c(
      "acc_repeated_measurements",
      "con_contradictions_redcap",
      "con_ssi_range_check"
    )
  )

  expect_identical(
    variable_groups$call_names,
    c("acc_repeated_measurements", "con_contradictions_redcap",
      "con_ssi_range_check")
  )
  study_results <- util_filter_repsum(
    repsumtab = repsum,
    vars_to_include = "study",
    meta_data = meta_data,
    rownames_of_report = meta_data$LABEL,
    label_col = LABEL,
    variable_group_call_names = unique(variable_groups$call_names)
  )
  expect_identical(
    study_results$call_names,
    c("com_item_missingness", "acc_repeated_measurements")
  )
})

test_that("group-only function diagnostics do not become item results", {
  repsum <- data.frame(
    VAR_NAMES = c("age", "age", "group_a"),
    call_names = c(
      "com_item_missingness", "con_ssi_range_check", "con_ssi_range_check"
    ),
    function_name = c(
      "com_item_missingness", "con_ssi_range_check", "con_ssi_range_check"
    ),
    indicator_metric = c(
      "PCT_miss", "CAT_applicability", "PCT_scc_miss"
    ),
    .variable_group_result_label = c(NA, NA, "Missing responses"),
    stringsAsFactors = FALSE
  )
  meta_data <- data.frame(
    VAR_NAMES = "age",
    LABEL = "Age",
    COMPUTED_VARIABLE_ROLE = NA_character_,
    stringsAsFactors = FALSE
  )

  local_mocked_bindings(
    util_report_scope_target_functions = function(target_entity) {
      switch(target_entity,
        item = "com_item_missingness",
        variable_group = "con_ssi_range_check"
      )
    }
  )

  item_results <- util_filter_repsum(
    repsumtab = repsum,
    vars_to_include = "study",
    meta_data = meta_data,
    rownames_of_report = "Age",
    label_col = LABEL,
    variable_group_call_names = "con_ssi_range_check"
  )
  expect_identical(item_results$call_names, "com_item_missingness")

  group_results <- util_filter_repsum(
    repsumtab = repsum,
    vars_to_include = "variable_group",
    meta_data = meta_data,
    rownames_of_report = "Age",
    label_col = LABEL,
    variable_group_call_names = "con_ssi_range_check"
  )
  expect_identical(group_results$VAR_NAMES, "group_a")
})

test_that("util_filter_repsum keeps unregistered group results out of study", {
  repsum <- data.frame(
    VAR_NAMES = c("age", "Questionnaire item A | Questionnaire item B"),
    call_names = c("com_item_missingness", "des_scatterplot_matrix"),
    value = c(1, 2),
    stringsAsFactors = FALSE
  )
  meta_data <- data.frame(
    VAR_NAMES = "age",
    LABEL = "Age",
    COMPUTED_VARIABLE_ROLE = NA_character_,
    stringsAsFactors = FALSE
  )

  study_results <- util_filter_repsum(
    repsumtab = repsum,
    vars_to_include = "study",
    meta_data = meta_data,
    rownames_of_report = "Age",
    label_col = LABEL,
    study_var_names = "age"
  )
  group_results <- util_filter_repsum(
    repsumtab = repsum,
    vars_to_include = "variable_group",
    meta_data = meta_data,
    rownames_of_report = "Age",
    label_col = LABEL,
    study_var_names = "age"
  )

  expect_identical(study_results$VAR_NAMES, "age")
  expect_identical(
    group_results$VAR_NAMES,
    "Questionnaire item A | Questionnaire item B"
  )
})

test_that("variable-group descriptors without known metrics are excluded", {
  repsum <- data.frame(
    VAR_NAMES = rep("group_a", 3),
    call_names = rep("des_scatterplot_matrix", 3),
    indicator_metric = c("NUM_acc_drm_gold", "max_cor", "in_range"),
    stringsAsFactors = FALSE
  )
  meta_data <- data.frame(
    VAR_NAMES = "item_a",
    LABEL = "Item A",
    COMPUTED_VARIABLE_ROLE = NA_character_,
    stringsAsFactors = FALSE
  )

  filtered <- util_filter_repsum(
    repsumtab = repsum,
    vars_to_include = "variable_group",
    meta_data = meta_data,
    rownames_of_report = "Group A",
    label_col = LABEL,
    study_var_names = "item_a"
  )

  expect_identical(filtered$indicator_metric, "NUM_acc_drm_gold")
})

test_that("variable-group diagnostics without regular output are retained", {
  repsum <- data.frame(
    VAR_NAMES = rep("group_a", 5),
    call_names = rep("con_ssi_range_check", 5),
    indicator_metric = c(
      "",
      "EMPTY_OUTPUT",
      "CAT_applicability",
      "MSG_applicability",
      "unknown_descriptor"
    ),
    stringsAsFactors = FALSE
  )
  meta_data <- data.frame(
    VAR_NAMES = "item_a",
    LABEL = "Item A",
    COMPUTED_VARIABLE_ROLE = NA_character_,
    stringsAsFactors = FALSE
  )

  filtered <- util_filter_repsum(
    repsumtab = repsum,
    vars_to_include = "variable_group",
    meta_data = meta_data,
    rownames_of_report = "Group A",
    label_col = LABEL,
    study_var_names = "item_a"
  )

  expect_identical(
    filtered$indicator_metric,
    c("", "EMPTY_OUTPUT", "CAT_applicability", "MSG_applicability")
  )
})
