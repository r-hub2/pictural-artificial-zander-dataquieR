skip_on_cran()

test_that("acc_repeated_measurements returns results per method and comparison", { # nolint: line_length_linter.
  study_data <- data.frame(
    SBP_0 = c(120, 121, 119, 122),
    SBP_1 = c(121, 120, 118, 123),
    SBP_reference = c(120, 121, 119, 122)
  )

  meta_data <- data.frame(
    VAR_NAMES = names(study_data),
    LABEL = names(study_data),
    DATA_TYPE = DATA_TYPES$FLOAT,
    SCALE_LEVEL = SCALE_LEVELS$RATIO,
    MISSING_LIST = SPLIT_CHAR,
    JUMP_LIST = SPLIT_CHAR,
    stringsAsFactors = FALSE
  )

  res <- suppressWarnings(acc_repeated_measurements(
    variable_group = names(study_data),
    repeated_measures_metric = "rmse|mean_absolute_difference",
    repeated_measures_reference = "SBP_reference",
    study_data = study_data,
    meta_data = meta_data,
    label_col = VAR_NAMES
  ))

  expect_true(all(c(
    "VariableGroupTable", "VariableGroupData",
    "OtherTable"
  ) %in%
    names(res)))
  expect_false("SummaryTable" %in% names(res))
  expect_false("SummaryPlot" %in% names(res))
  expect_equal(
    names(res$VariableGroupTable),
    c(
      VARIABLE_LIST, "ICC_acc_drm_inter", "NUM_acc_drm_inter",
      "NUM_acc_drm_gold"
    )
  )
  expect_equal(
    res$VariableGroupTable[[VARIABLE_LIST]],
    c(
      "SBP_0 | SBP_reference",
      "SBP_1 | SBP_reference",
      "SBP_0 | SBP_reference",
      "SBP_1 | SBP_reference"
    )
  )
  expect_equal(res$VariableGroupTable$NUM_acc_drm_gold, c(0, 1, 0, 1))
  expect_true(VARIABLE_LIST %in% names(res$OtherTable))
  expect_equal(
    res$VariableGroupData$`Repeated-measurement metric`,
    c(
      "rmse", "rmse",
      "mean_absolute_difference",
      "mean_absolute_difference"
    )
  )
  expect_equal(
    res$VariableGroupData$`Comparison variables`,
    c(
      "SBP_0 | SBP_reference",
      "SBP_1 | SBP_reference",
      "SBP_0 | SBP_reference",
      "SBP_1 | SBP_reference"
    )
  )
  expect_equal(
    res$VariableGroupData$`Repeated-measurement reference`,
    rep("SBP_reference", 4)
  )
  expect_equal(
    res$VariableGroupData$`Repeated-measurement value`,
    c(0, 1, 0, 1)
  )
  expect_equal(
    unique(res$VariableGroupData$`Repeated-measurement status`),
    "ok"
  )
})

test_that("repeated-measurement setting ids link to filtered settings table", {
  tb <- data.frame(
    `Repeated-measurement setting` = c("rm_rmse_default", NA_character_),
    value = c("<unsafe>", "plain"),
    check.names = FALSE
  )

  linked <- util_link_result_references(tb)

  expect_identical(util_attr(linked, "is_html_escaped", exact = TRUE), TRUE)
  expect_match(linked$`Repeated-measurement setting`[[1]],
    "statisticalsettings.html\\?dq_filter_col=SETTING_ID",
    perl = TRUE
  )
  expect_match(linked$`Repeated-measurement setting`[[1]],
    "dq_filter_value=rm_rmse_default",
    fixed = TRUE
  )
  expect_match(linked$value[[1]], "&lt;unsafe&gt;", fixed = TRUE)
})

test_that("acc_repeated_measurements has stable metric results", {
  study_data <- data.frame(
    sbp_a = c(120, 122, 124, 126),
    sbp_b = c(121, 121, 125, 128),
    sbp_ref = c(120, 123, 123, 127),
    rating_a = c("yes", "yes", "no", "no"),
    rating_b = c("yes", "no", "no", "no"),
    rating_ref = c("yes", "yes", "no", "yes")
  )
  meta_data <- data.frame(
    VAR_NAMES = names(study_data),
    LABEL = names(study_data),
    DATA_TYPE = c(rep(DATA_TYPES$FLOAT, 3), rep(DATA_TYPES$STRING, 3)),
    SCALE_LEVEL = c(rep(SCALE_LEVELS$RATIO, 3), rep(SCALE_LEVELS$NOMINAL, 3)),
    MISSING_LIST = SPLIT_CHAR,
    JUMP_LIST = SPLIT_CHAR,
    RECODE_CASES = c(rep(NA_character_, 3), rep("yes", 3)),
    stringsAsFactors = FALSE
  )

  numeric_ref <- suppressWarnings(acc_repeated_measurements(
    variable_group = c("sbp_a", "sbp_b", "sbp_ref"),
    repeated_measures_metric = paste(
      c("rmse", "mean_absolute_difference", "mean_difference"),
      collapse = SPLIT_CHAR
    ),
    repeated_measures_reference = "sbp_ref",
    study_data = study_data,
    meta_data = meta_data,
    label_col = VAR_NAMES
  ))
  numeric_global <- suppressWarnings(acc_repeated_measurements(
    variable_group = c("sbp_a", "sbp_b", "sbp_ref"),
    repeated_measures_metric = "within_subject_sd",
    study_data = study_data,
    meta_data = meta_data,
    label_col = VAR_NAMES
  ))
  categorical_ref <- suppressWarnings(acc_repeated_measurements(
    variable_group = c("rating_a", "rating_b", "rating_ref"),
    repeated_measures_metric = paste(
      c("percent_agreement", "weighted_kappa", "sensitivity"),
      collapse = SPLIT_CHAR
    ),
    repeated_measures_reference = "rating_ref",
    study_data = study_data,
    meta_data = meta_data,
    label_col = VAR_NAMES,
    repeated_measurement_settings = data.frame(
      SETTING_ID = c("rm_weighted_kappa_default", "rm_sensitivity_default"),
      MIN_N = c(1L, 1L),
      stringsAsFactors = FALSE
    )
  ))

  stable <- rbind(
    numeric_ref$VariableGroupData,
    numeric_global$VariableGroupData,
    categorical_ref$VariableGroupData
  )
  stable <- stable[, c(
    "Repeated-measurement metric",
    "Comparison variables",
    "Repeated-measurement reference",
    "Repeated-measurement value",
    "Number of complete observations",
    "Repeated-measurement status"
  )]
  stable$`Repeated-measurement value` <-
    round(stable$`Repeated-measurement value`, digits = 6)

  expect_snapshot_value(stable, style = "deparse")
  expect_equal(
    names(numeric_ref$VariableGroupTable),
    c(
      VARIABLE_LIST, "ICC_acc_drm_inter", "NUM_acc_drm_inter",
      "NUM_acc_drm_gold"
    )
  )
  expect_equal(
    colnames(numeric_ref$VariableGroupData),
    c(
      "Variable list",
      "Repeated-measurement metric",
      "Comparison variables",
      "Repeated-measurement reference",
      "Repeated-measurement setting",
      "Repeated-measurement value",
      "Confidence interval low",
      "Confidence interval high",
      "Number of observations",
      "Number of complete observations",
      "Number of observations with missing repeats",
      "Repeated-measurement status",
      "Repeated-measurement method",
      "Repeated-measurement warning"
    )
  )
  expect_equal(
    unname(numeric_ref$VariableGroupData$`Repeated-measurement value`),
    numeric_ref$VariableGroupTable$NUM_acc_drm_gold
  )
})

test_that("acc_repeated_measurements rejects missing metrics", {
  study_data <- data.frame(SBP_0 = 1:3, SBP_1 = 2:4)
  meta_data <- data.frame(
    VAR_NAMES = names(study_data),
    LABEL = names(study_data),
    DATA_TYPE = DATA_TYPES$INTEGER,
    SCALE_LEVEL = SCALE_LEVELS$RATIO,
    MISSING_LIST = SPLIT_CHAR,
    JUMP_LIST = SPLIT_CHAR,
    stringsAsFactors = FALSE
  )

  expect_error(
    acc_repeated_measurements(
      variable_group = names(study_data),
      repeated_measures_metric = "",
      study_data = study_data,
      meta_data = meta_data,
      label_col = VAR_NAMES
    ),
    regexp = "No repeated-measurements metric requested"
  )
})

test_that("acc_repeated_measurements computes pairwise results without reference", { # nolint: line_length_linter.
  study_data <- data.frame(
    A = c(1, 2, 3),
    B = c(1, 4, 3),
    C = c(1, 2, 5)
  )
  meta_data <- data.frame(
    VAR_NAMES = names(study_data),
    LABEL = names(study_data),
    DATA_TYPE = DATA_TYPES$INTEGER,
    SCALE_LEVEL = SCALE_LEVELS$RATIO,
    MISSING_LIST = SPLIT_CHAR,
    JUMP_LIST = SPLIT_CHAR,
    stringsAsFactors = FALSE
  )

  res <- suppressWarnings(acc_repeated_measurements(
    variable_group = names(study_data),
    repeated_measures_metric = "rmse",
    study_data = study_data,
    meta_data = meta_data,
    label_col = VAR_NAMES
  ))

  expect_equal(
    res$VariableGroupTable[[VARIABLE_LIST]],
    c("A | B", "A | C", "B | C")
  )
  expect_equal(
    res$VariableGroupTable$NUM_acc_drm_inter,
    sqrt(c(4 / 3, 4 / 3, 8 / 3))
  )
})

test_that("acc_repeated_measurements computes global results once per group", {
  study_data <- data.frame(
    A = c(1, 2, 3),
    B = c(2, 2, 4),
    C = c(3, 2, 5)
  )
  meta_data <- data.frame(
    VAR_NAMES = names(study_data),
    LABEL = names(study_data),
    DATA_TYPE = DATA_TYPES$INTEGER,
    SCALE_LEVEL = SCALE_LEVELS$RATIO,
    MISSING_LIST = SPLIT_CHAR,
    JUMP_LIST = SPLIT_CHAR,
    stringsAsFactors = FALSE
  )

  res <- suppressWarnings(acc_repeated_measurements(
    variable_group = names(study_data),
    repeated_measures_metric = "within_subject_sd",
    study_data = study_data,
    meta_data = meta_data,
    label_col = VAR_NAMES
  ))

  expect_equal(nrow(res$VariableGroupTable), 1)
  expect_equal(res$VariableGroupTable[[VARIABLE_LIST]], "A | B | C")
  expect_equal(res$VariableGroupTable$NUM_acc_drm_inter, sqrt(2 / 3))
})

test_that("acc_repeated_measurements applies setting overrides by setting_id", {
  study_data <- data.frame(
    A = c(1, 2, 3),
    B = c(1, 4, 3)
  )
  meta_data <- data.frame(
    VAR_NAMES = names(study_data),
    LABEL = names(study_data),
    DATA_TYPE = DATA_TYPES$INTEGER,
    SCALE_LEVEL = SCALE_LEVELS$RATIO,
    MISSING_LIST = SPLIT_CHAR,
    JUMP_LIST = SPLIT_CHAR,
    stringsAsFactors = FALSE
  )
  settings <- data.frame(
    SETTING_ID = "rm_rmse_default",
    MIN_N = 4,
    stringsAsFactors = FALSE
  )

  res <- suppressWarnings(acc_repeated_measurements(
    variable_group = names(study_data),
    repeated_measures_metric = "rmse",
    study_data = study_data,
    meta_data = meta_data,
    label_col = VAR_NAMES,
    repeated_measurement_settings = settings
  ))

  expect_equal(
    res$OtherTable$repeated_measures_status,
    "not_computable"
  )
  expect_match(
    res$OtherTable$repeated_measures_warning,
    "Insufficient complete observations"
  )
})

test_that("acc_repeated_measurements uses RECODE_CASES as event-level fallback", { # nolint: line_length_linter.
  study_data <- data.frame(
    test = c("yes", "yes", "no", "no"),
    reference = c("yes", "no", "yes", "no")
  )
  meta_data <- data.frame(
    VAR_NAMES = names(study_data),
    LABEL = names(study_data),
    DATA_TYPE = DATA_TYPES$STRING,
    SCALE_LEVEL = SCALE_LEVELS$NOMINAL,
    MISSING_LIST = SPLIT_CHAR,
    JUMP_LIST = SPLIT_CHAR,
    RECODE_CASES = c("yes", "yes"),
    stringsAsFactors = FALSE
  )
  settings <- data.frame(
    SETTING_ID = "rm_sensitivity_default",
    MIN_N = 1,
    stringsAsFactors = FALSE
  )

  res <- suppressWarnings(acc_repeated_measurements(
    variable_group = names(study_data),
    repeated_measures_metric = "sensitivity",
    repeated_measures_reference = "reference",
    study_data = study_data,
    meta_data = meta_data,
    label_col = VAR_NAMES,
    repeated_measurement_settings = settings
  ))

  expect_equal(res$VariableGroupTable$NUM_acc_drm_gold, 0.5)
  expect_equal(res$OtherTable$repeated_measures_status, "ok")
})

test_that("acc_repeated_measurements normalizes EVENT_LEVELS via dichotomization", { # nolint: line_length_linter.
  study_data <- data.frame(
    test = c(0, 1, 1, 3),
    reference = c(0, 0, 2, 3)
  )
  meta_data <- data.frame(
    VAR_NAMES = names(study_data),
    LABEL = names(study_data),
    DATA_TYPE = DATA_TYPES$INTEGER,
    SCALE_LEVEL = SCALE_LEVELS$ORDINAL,
    MISSING_LIST = SPLIT_CHAR,
    JUMP_LIST = SPLIT_CHAR,
    EVENT_LEVELS = "[2;3]",
    stringsAsFactors = FALSE
  )
  settings <- data.frame(
    SETTING_ID = "rm_sensitivity_default",
    MIN_N = 1,
    stringsAsFactors = FALSE
  )

  res <- acc_repeated_measurements(
    variable_group = names(study_data),
    repeated_measures_metric = "sensitivity",
    repeated_measures_reference = "reference",
    study_data = study_data,
    meta_data = meta_data,
    label_col = VAR_NAMES,
    repeated_measurement_settings = settings
  )

  expect_equal(res$VariableGroupTable$NUM_acc_drm_gold, 0.5)
  expect_equal(res$OtherTable$repeated_measures_status, "ok")
})

test_that("acc_repeated_measurements validates reference variables", {
  study_data <- data.frame(
    SBP_0 = 1:3,
    SBP_1 = 2:4,
    SBP_reference = 1:3
  )
  meta_data <- data.frame(
    VAR_NAMES = names(study_data),
    LABEL = names(study_data),
    DATA_TYPE = DATA_TYPES$INTEGER,
    SCALE_LEVEL = SCALE_LEVELS$RATIO,
    MISSING_LIST = SPLIT_CHAR,
    JUMP_LIST = SPLIT_CHAR,
    stringsAsFactors = FALSE
  )

  expect_error(
    suppressWarnings(acc_repeated_measurements(
      variable_group = c("SBP_0", "SBP_1"),
      repeated_measures_metric = "rmse",
      repeated_measures_reference = "SBP_reference",
      study_data = study_data,
      meta_data = meta_data,
      label_col = VAR_NAMES
    )),
    regexp = "reference variable"
  )
})

test_that("acc_repeated_measurements maps reference variables through metadata", { # nolint: line_length_linter.
  study_data <- data.frame(
    SBP_0 = c(120, 121, 119),
    SBP_1 = c(121, 120, 118),
    SBP_reference = c(120, 121, 119)
  )

  meta_data <- data.frame(
    VAR_NAMES = names(study_data),
    LABEL = c(
      "Baseline systolic BP",
      "Follow-up systolic BP",
      "Reference systolic BP"
    ),
    DATA_TYPE = DATA_TYPES$FLOAT,
    SCALE_LEVEL = SCALE_LEVELS$RATIO,
    MISSING_LIST = SPLIT_CHAR,
    JUMP_LIST = SPLIT_CHAR,
    stringsAsFactors = FALSE
  )

  res <- suppressWarnings(acc_repeated_measurements(
    variable_group = names(study_data),
    repeated_measures_metric = "rmse",
    repeated_measures_reference_vars = "Reference systolic BP",
    study_data = study_data,
    meta_data = meta_data,
    label_col = VAR_NAMES
  ))

  expect_equal(
    res$OtherTable$repeated_measures_reference,
    rep("SBP_reference", 2)
  )
})

test_that("acc_repeated_measurements rejects conflicting reference aliases", {
  study_data <- data.frame(
    SBP_0 = 1:3,
    SBP_1 = 2:4,
    SBP_reference = 1:3
  )
  meta_data <- data.frame(
    VAR_NAMES = names(study_data),
    LABEL = names(study_data),
    DATA_TYPE = DATA_TYPES$INTEGER,
    SCALE_LEVEL = SCALE_LEVELS$RATIO,
    MISSING_LIST = SPLIT_CHAR,
    JUMP_LIST = SPLIT_CHAR,
    stringsAsFactors = FALSE
  )

  expect_error(
    suppressWarnings(acc_repeated_measurements(
      variable_group = names(study_data),
      repeated_measures_metric = "rmse",
      repeated_measures_reference = "SBP_reference",
      repeated_measures_reference_vars = "SBP_0",
      study_data = study_data,
      meta_data = meta_data,
      label_col = VAR_NAMES
    )),
    regexp = "not both with different values"
  )
})

test_that("acc_repeated_measurements accepts only one reference variable", {
  study_data <- data.frame(
    SBP_0 = 1:3,
    SBP_1 = 2:4,
    SBP_reference = 1:3
  )
  meta_data <- data.frame(
    VAR_NAMES = names(study_data),
    LABEL = names(study_data),
    DATA_TYPE = DATA_TYPES$INTEGER,
    SCALE_LEVEL = SCALE_LEVELS$RATIO,
    MISSING_LIST = SPLIT_CHAR,
    JUMP_LIST = SPLIT_CHAR,
    stringsAsFactors = FALSE
  )

  expect_error(
    suppressWarnings(acc_repeated_measurements(
      variable_group = names(study_data),
      repeated_measures_metric = "rmse",
      repeated_measures_reference_vars = c("SBP_reference", "SBP_0"),
      study_data = study_data,
      meta_data = meta_data,
      label_col = VAR_NAMES
    )),
    regexp = "Need exactly one element"
  )
})

test_that("acc_repeated_measurements warns about unsupported metrics", {
  study_data <- data.frame(SBP_0 = 1:3, SBP_1 = 2:4)
  meta_data <- data.frame(
    VAR_NAMES = names(study_data),
    LABEL = names(study_data),
    DATA_TYPE = DATA_TYPES$INTEGER,
    SCALE_LEVEL = SCALE_LEVELS$RATIO,
    MISSING_LIST = SPLIT_CHAR,
    JUMP_LIST = SPLIT_CHAR,
    stringsAsFactors = FALSE
  )

  expect_warning(
    res <- withCallingHandlers(
      acc_repeated_measurements(
        variable_group = names(study_data),
        repeated_measures_metric = "rmse|unsupported_metric",
        study_data = study_data,
        meta_data = meta_data,
        label_col = VAR_NAMES
      ),
      warning = function(w) {
        if (grepl("Changing language has no effect",
            conditionMessage(w),
            fixed = TRUE
          )) {
          invokeRestart("muffleWarning")
        }
      }
    ),
    regexp = "Unsupported repeated-measurement metric"
  )
  expect_equal(res$OtherTable$repeated_measures_metric, "rmse")

  expect_error(
    suppressWarnings(acc_repeated_measurements(
      variable_group = names(study_data),
      repeated_measures_metric = "unsupported_metric",
      study_data = study_data,
      meta_data = meta_data,
      label_col = VAR_NAMES
    )),
    regexp = "No supported repeated-measurement metric requested"
  )
})

test_that("acc_repeated_measurements computes all eligible cross-item groups", {
  prep_purge_data_frame_cache()
  withr::defer(prep_purge_data_frame_cache())

  study_data <- data.frame(
    SBP_0 = c(120, 121, 119),
    SBP_1 = c(121, 120, 118),
    SBP_2 = c(119, 122, 120),
    OTHER_0 = c(1, 2, 3),
    OTHER_1 = c(1, 2, 4)
  )
  meta_data <- data.frame(
    VAR_NAMES = names(study_data),
    LABEL = c(
      "SBP baseline", "SBP repeat", "SBP reference",
      "Other baseline", "Other repeat"
    ),
    DATA_TYPE = DATA_TYPES$FLOAT,
    SCALE_LEVEL = SCALE_LEVELS$RATIO,
    MISSING_LIST = SPLIT_CHAR,
    JUMP_LIST = SPLIT_CHAR,
    stringsAsFactors = FALSE
  )
  meta_data_cross_item <- data.frame(
    CHECK_ID = c("bp", "unsupported"),
    CHECK_LABEL = c("blood_pressure_repeats", "unsupported_repeats"),
    VARIABLE_LIST = c(
      "SBP baseline|SBP repeat|SBP reference",
      "Other baseline|Other repeat"
    ),
    REPEATED_MEASURES_METRIC = c("rmse", "unsupported_metric"),
    stringsAsFactors = FALSE
  )
  prep_add_data_frames(
    data_frame_list = list("cross-item_level" = meta_data_cross_item)
  )

  res <- suppressWarnings(acc_repeated_measurements(
    study_data = study_data,
    meta_data = meta_data,
    label_col = LABEL,
    repeated_measurement_settings = data.frame(
      SETTING_ID = "rm_rmse_default",
      MIN_N = 1,
      stringsAsFactors = FALSE
    )
  ))

  expect_true(all(c("VariableGroupTable", "VariableGroupData", "OtherTable") %in% # nolint: line_length_linter.
        names(res)))
  expect_equal(nrow(res$OtherTable), 3)
  expect_true(all(res$OtherTable$repeated_measures_metric == "rmse"))
  expect_false(any(grepl("OTHER", res$OtherTable[[VARIABLE_LIST]])))
  for (component in c(
    "VariableGroupTable", "VariableGroupData", "OtherTable"
  )) {
    expect_identical(unique(res[[component]][[CHECK_ID]]), "bp")
    expect_identical(
      unique(res[[component]][[CHECK_LABEL]]),
      "blood_pressure_repeats"
    )
  }
})

test_that("acc_repeated_measurements is generated from cross-item metadata", {
  meta_data <- data.frame(
    VAR_NAMES = c("SBP_0", "SBP_1", "SBP_reference"),
    LABEL = c("SBP_0", "SBP_1", "SBP_reference"),
    DATA_TYPE = DATA_TYPES$FLOAT,
    SCALE_LEVEL = SCALE_LEVELS$RATIO,
    MISSING_LIST = SPLIT_CHAR,
    JUMP_LIST = SPLIT_CHAR,
    stringsAsFactors = FALSE
  )
  meta_data_cross_item <- data.frame(
    CHECK_ID = "bp",
    CHECK_LABEL = "blood_pressure_repeats",
    VARIABLE_LIST = "SBP_0|SBP_1|SBP_reference",
    REPEATED_MEASURES_METRIC = "rmse|mean_absolute_difference",
    REPEATED_MEASURES_REFERENCE = "SBP_reference",
    stringsAsFactors = FALSE
  )

  calls <- util_generate_calls(
    dimensions = "Accuracy",
    meta_data = meta_data,
    label_col = VAR_NAMES,
    meta_data_segment = NULL,
    meta_data_dataframe = NULL,
    meta_data_cross_item = meta_data_cross_item,
    specific_args = list(),
    arg_overrides = list(),
    resp_vars = NULL,
    filter_indicator_functions = "^acc_repeated_measurements$",
    exclude_indicator_functions = character(0)
  )

  expect_length(calls, 1)
  expect_identical(as.character(calls[[1]][[1]]), "acc_repeated_measurements")
  expect_equal(
    eval(calls[[1]]$variable_group),
    c("SBP_0", "SBP_1", "SBP_reference")
  )
  expect_equal(
    as.character(eval(calls[[1]]$repeated_measures_metric)),
    c("rmse", "mean_absolute_difference")
  )
  expect_equal(
    as.character(eval(calls[[1]]$repeated_measures_reference)),
    "SBP_reference"
  )
  expect_identical(util_attr(calls[[1]], CHECK_ID, exact = TRUE), "bp")
  expect_identical(
    util_attr(calls[[1]], CHECK_LABEL, exact = TRUE),
    "blood_pressure_repeats"
  )
})

test_that("repeated-measurement metric helpers expose guardrail results", {
  skip_on_cran()

  expect_true(is.na(util_repeated_measurement_concordance(
    c(1, 1),
    c(1, 2)
  )))
  expect_true(is.na(util_repeated_measurement_cohen_kappa("same", "same")))
  expect_true(is.na(util_repeated_measurement_weighted_kappa("same", "same")))
  expect_true(is.na(util_repeated_measurement_fleiss_kappa(
    data.frame(a = "yes", b = "yes")
  )))
  expect_true(is.na(util_repeated_measurement_fleiss_kappa(
    data.frame(
      a = c("yes", "yes"), b = c("yes", "yes"),
      c = c("yes", "yes")
    )
  )))

  expect_equal(
    util_repeated_measurement_percent_agreement(
      data.frame(
        a = c("yes", "yes"), b = c("yes", "no"),
        c = c("yes", "yes")
      )
    ),
    0.5
  )
  expect_true(is.na(util_repeated_measurement_within_subject_sd(
    data.frame(a = numeric(0), b = numeric(0))
  )))

  cv_setting <- data.frame(cv_scale = "percent", stringsAsFactors = FALSE)
  expect_true(is.na(util_repeated_measurement_coefficient_of_variation(
    data.frame(a = c(0, 0), b = c(0, 0)),
    cv_setting
  )))
  expect_gt(util_repeated_measurement_coefficient_of_variation(
    data.frame(a = c(1, 2), b = c(2, 4)),
    cv_setting
  ), 0)

  meta_data <- data.frame(
    VAR_NAMES = c("test", "ref"),
    DATA_TYPE = DATA_TYPES$INTEGER,
    SCALE_LEVEL = SCALE_LEVELS$NOMINAL,
    RECODE_CASES = c("", ""),
    RECODE_CONTROL = c("", ""),
    stringsAsFactors = FALSE
  )
  expect_null(util_repeated_measurement_event_meta(
    meta_data, VAR_NAMES,
    "test"
  ))
  sens <- util_repeated_measurement_sens_spec(
    test = c(1, 0),
    ref = c(1, 1),
    test_var = "test",
    ref_var = "ref",
    meta_data = meta_data,
    label_col = VAR_NAMES,
    metric = "sensitivity"
  )
  expect_true(is.na(sens$value))
  expect_match(sens$warning, "EVENT_LEVELS or RECODE_CASES")

  icc_setting <- data.frame(
    icc_model = "twoway",
    icc_unit = "single",
    icc_type = "agreement",
    stringsAsFactors = FALSE
  )
  expect_true(is.na(util_repeated_measurement_icc(
    data.frame(a = 1, b = 1),
    icc_setting,
    type = ""
  )))
  expect_true(is.na(util_repeated_measurement_icc(
    data.frame(a = c(1, 2), b = c(2, 3)),
    transform(icc_setting, icc_model = "oneway"),
    type = ""
  )))
  expect_true(is.na(util_repeated_measurement_icc(
    data.frame(a = c(1, 2), b = c(2, 3)),
    transform(icc_setting, icc_unit = "average"),
    type = ""
  )))
  expect_true(is.na(util_repeated_measurement_icc(
    data.frame(a = c(1, 2), b = c(2, 3)),
    icc_setting,
    type = "unsupported"
  )))
  expect_true(is.finite(util_repeated_measurement_icc(
    data.frame(a = c(1, 2, 3), b = c(1, 2, 4)),
    icc_setting,
    type = "consistency"
  )))
})

test_that("repeated-measurement comparisons follow the configured mode", {
  skip_on_cran()

  variables <- c("baseline", "followup", "reference")

  expect_identical(
    util_repeated_measurement_comparisons(
      variables,
      setting = list(comparison_mode_default = "global")
    ),
    list(variables)
  )
  expect_identical(
    util_repeated_measurement_comparisons(
      variables,
      setting = list(comparison_mode_default = "reference_pairwise"),
      repeated_measures_reference_vars = "reference"
    ),
    list(c("baseline", "reference"), c("followup", "reference"))
  )
  expect_identical(
    util_repeated_measurement_comparisons(
      variables[1:2],
      setting = list(comparison_mode_default = "pairwise")
    ),
    list(c("baseline", "followup"))
  )
})

test_that("repeated-measurement group table assigns successful metric values", {
  skip_on_cran()

  details <- data.frame(
    Variables = c("group_icc", "group_reference", "group_warning"),
    comparison_variables = c("", "measure|reference", ""),
    repeated_measures_metric = c("icc_agreement", "rmse", "rmse"),
    repeated_measures_reference = c(NA_character_, "reference", ""),
    repeated_measures_status = c("ok", "ok", "warning"),
    repeated_measures_value = c(0.8, 1.2, 9.9),
    stringsAsFactors = FALSE
  )

  table <- util_repeated_measurement_variable_group_table(details)

  expect_identical(table$VARIABLE_LIST, c(
    "group_icc", "measure|reference", "group_warning"
  ))
  expect_equal(table$ICC_acc_drm_inter, c(0.8, NA_real_, NA_real_))
  expect_equal(table$NUM_acc_drm_gold, c(NA_real_, 1.2, NA_real_))
  expect_equal(table$NUM_acc_drm_inter, rep(NA_real_, 3))
})

test_that("repeated-measurement kappa helpers calculate nominal agreement", {
  skip_on_cran()

  expect_equal(
    util_repeated_measurement_cohen_kappa(
      c("a", "a", "b", "b"),
      c("a", "b", "b", "b")
    ),
    0.5
  )

  expect_equal(
    util_repeated_measurement_fleiss_kappa(data.frame(
      rater_1 = c("a", "b", "a"),
      rater_2 = c("a", "b", "a"),
      rater_3 = c("a", "b", "b")
    )),
    0.55
  )
})

test_that(
  "repeated-measurement agreement helpers reject undefined statistics",
  {
    skip_on_cran()

    expect_true(is.na(util_repeated_measurement_fleiss_kappa(data.frame(
      rater_1 = "a",
      rater_2 = "a"
    ))))
    expect_true(is.na(util_repeated_measurement_fleiss_kappa(data.frame(
      rater_1 = c("a", "a"),
      rater_2 = c("a", "a"),
      rater_3 = c("a", "a")
    ))))

    measurements <- data.frame(first = c(1, 2, 3), second = c(1, 2, 4))
    setting <- data.frame(
      icc_model = "twoway",
      icc_unit = "single",
      icc_type = "agreement",
      stringsAsFactors = FALSE
    )
    expect_true(is.finite(util_repeated_measurement_icc(
      measurements, setting,
      type = ""
    )))

    setting$icc_model <- "oneway"
    expect_true(is.na(util_repeated_measurement_icc(
      measurements, setting,
      type = "agreement"
    )))
    setting$icc_model <- "twoway"
    setting$icc_unit <- "average"
    expect_true(is.na(util_repeated_measurement_icc(
      measurements, setting,
      type = "agreement"
    )))
    setting$icc_unit <- "single"
    expect_true(is.na(util_repeated_measurement_icc(
      measurements, setting,
      type = "unsupported"
    )))
  }
)

test_that(
  "repeated-measurement helpers retain structured inapplicability details",
  {
    skip_on_cran()

    row <- util_repeated_measurement_warning_row(
      variable_group = c("first", "second"),
      metric = "rmse",
      repeated_measures_reference_vars = "",
      setting_id = "rm_rmse_default",
      warning = "No applicable comparison",
      method_details = "configured method"
    )
    expect_identical(row$Variables, "first | second")
    expect_identical(row$repeated_measures_status, "not_computable")
    expect_identical(row$comparison_variables, NA_character_)
    expect_identical(row$repeated_measures_reference, "")
    expect_identical(row$repeated_measures_metric_setting, "rm_rmse_default")
    expect_true(is.na(row$repeated_measures_value))
    expect_true(is.na(row$repeated_measures_ci_low))
    expect_true(is.na(row$repeated_measures_ci_high))
    expect_true(is.na(row$repeated_measures_n))
    expect_true(is.na(row$repeated_measures_n_complete))
    expect_true(is.na(row$repeated_measures_n_missing))
    expect_identical(row$repeated_measures_method, "configured method")
    expect_identical(row$repeated_measures_warning, "No applicable comparison")

    default_row <- util_repeated_measurement_warning_row(
      variable_group = "first",
      metric = "rmse",
      repeated_measures_reference_vars = "reference",
      setting_id = NA_character_,
      warning = "No configured setting"
    )
    expect_identical(default_row$repeated_measures_method, NA_character_)

    meta_data <- data.frame(
      VAR_NAMES = c("test", "reference"),
      LABEL = c("test", "reference"),
      stringsAsFactors = FALSE
    )
    result <- util_repeated_measurement_sens_spec(
      test = c("yes", "no"),
      ref = c("yes", "no"),
      test_var = "test",
      ref_var = "reference",
      meta_data = meta_data,
      label_col = VAR_NAMES,
      metric = "sensitivity"
    )
    expect_true(is.na(result$value))
    expect_match(
      result$warning,
      "missing for test variable and reference variable"
    )
  }
)

test_that("repeated-measurement settings helpers normalize overrides", {
  skip_on_cran()

  defaults <- util_repeated_measurement_default_settings()

  expect_identical(util_repeated_measurement_settings(NULL), defaults)
  expect_identical(
    util_repeated_measurement_settings("missing_registered_settings_table"),
    defaults
  )
  expect_error(
    util_repeated_measurement_settings(list(setting_id = "x")),
    "must be a data frame"
  )

  expect_error(
    util_repeated_measurement_apply_setting_overrides(
      defaults,
      data.frame(metric = "rmse")
    ),
    "SETTING_ID"
  )

  overrides <- data.frame(
    SETTING_ID = c("custom_rmse", "", "rm_rmse_default"),
    METRIC = c("rmse", "ignored", NA_character_),
    ENABLED = c("yes", "no", "0"),
    IS_DEFAULT = c("Y", "N", ""),
    MIN_N = c("5", "9", "not an integer"),
    UNKNOWN_COLUMN = "ignored",
    stringsAsFactors = FALSE
  )

  expect_message(
    settings <- util_repeated_measurement_apply_setting_overrides(
      defaults,
      overrides
    ),
    "Ignoring unknown repeated-measurement setting column"
  )

  custom <- settings[settings$setting_id == "custom_rmse", , drop = FALSE]
  rmse_default <- settings[settings$setting_id == "rm_rmse_default", ,
    drop = FALSE
  ]

  expect_equal(custom$metric, "rmse")
  expect_true(custom$enabled)
  expect_true(custom$is_default)
  expect_equal(custom$min_n, 5L)
  expect_false(rmse_default$enabled)
  expect_equal(rmse_default$min_n, 1L)

  expect_identical(
    unname(util_repeated_measurement_logical_column(
      c("", TRUE, "false", "yes", "unexpected"),
      default = TRUE
    )),
    c(TRUE, TRUE, FALSE, TRUE, TRUE)
  )
})

test_that("repeated-measurement setting selection reports ambiguous cases", {
  skip_on_cran()

  settings <- util_repeated_measurement_standardize_settings(data.frame(
    setting_id = c("rmse_one", "rmse_two", "kap"),
    metric = c("rmse", "rmse", "cohen_kappa"),
    enabled = c(TRUE, TRUE, FALSE),
    is_default = c(FALSE, FALSE, TRUE),
    stringsAsFactors = FALSE
  ))

  ambiguous <- util_repeated_measurement_select_setting(
    settings = settings,
    metric = "rmse",
    metric_index = 1L,
    setting_ids = c("rmse_one", "rmse_two"),
    n_metrics = 1L
  )
  expect_null(ambiguous$setting)
  expect_match(ambiguous$warning, "Ambiguous")

  disabled <- util_repeated_measurement_select_setting(
    settings = settings,
    metric = "cohen_kappa",
    metric_index = 1L,
    setting_ids = "kap",
    n_metrics = 1L
  )
  expect_null(disabled$setting)
  expect_match(disabled$warning, "was not found as an enabled setting")

  wrong_metric <- util_repeated_measurement_select_setting(
    settings = settings,
    metric = "cohen_kappa",
    metric_index = 1L,
    setting_ids = "rmse_one",
    n_metrics = 1L
  )
  expect_null(wrong_metric$setting)
  expect_match(wrong_metric$warning, "is defined for metric")

  fallback <- util_repeated_measurement_select_setting(
    settings = settings,
    metric = "rmse",
    metric_index = 1L,
    setting_ids = character(0),
    n_metrics = 1L
  )
  expect_equal(fallback$setting$setting_id, "rmse_one")
  expect_match(fallback$warning, "no explicit default setting")

  missing <- util_repeated_measurement_select_setting(
    settings = settings,
    metric = "icc_agreement",
    metric_index = 1L,
    setting_ids = character(0),
    n_metrics = 1L
  )
  expect_null(missing$setting)
  expect_match(missing$warning, "no enabled repeated-measurement setting")
})

test_that(
  paste0(
    "repeated-measurement metric parser and core guardrails ",
    "are explicit"
  ),
  {
    skip_on_cran()

    expect_error(
      util_parse_repeated_measurement_metrics(""),
      "No repeated-measurements metric requested"
    )
    expect_identical(
      util_parse_repeated_measurement_metrics("RMSE | rmse | mean_difference"),
      c("rmse", "mean_difference")
    )
    expect_identical(
      util_parse_repeated_measurement_metric_settings(" first | second "),
      c("first", "second")
    )
    expect_identical(
      util_parse_repeated_measurement_metric_settings(NA_character_),
      character(0)
    )

    meta_data <- data.frame(
      VAR_NAMES = c("x", "y"),
      DATA_TYPE = DATA_TYPES$FLOAT,
      stringsAsFactors = FALSE
    )
    setting <- data.frame(
      min_n = 3L,
      min_repeats = 2L,
      stringsAsFactors = FALSE
    )

    no_complete <- util_compute_repeated_measurement_metric(
      study_data = data.frame(x = c(NA_real_, 1), y = c(1, NA_real_)),
      meta_data = meta_data,
      label_col = VAR_NAMES,
      vars = c("x", "y"),
      metric = "rmse",
      setting = setting
    )
    expect_identical(no_complete$status, "not_computable")
    expect_equal(no_complete$n_complete, 0L)
    expect_true(is.na(no_complete$warning))

    too_few_complete <- util_compute_repeated_measurement_metric(
      study_data = data.frame(x = c(1, 2), y = c(1, 2)),
      meta_data = meta_data,
      label_col = VAR_NAMES,
      vars = c("x", "y"),
      metric = "rmse",
      setting = setting
    )
    expect_identical(too_few_complete$status, "not_computable")
    expect_match(too_few_complete$warning, "Insufficient complete observations")

    too_few_repeats <- util_compute_repeated_measurement_metric(
      study_data = data.frame(x = c(1, 2, 3)),
      meta_data = meta_data,
      label_col = VAR_NAMES,
      vars = "x",
      metric = "rmse",
      setting = transform(setting, min_n = 1L, min_repeats = 2L)
    )
    expect_identical(too_few_repeats$status, "not_computable")
    expect_match(too_few_repeats$warning, "Insufficient repeated measurements")
  }
)

test_that("repeated-measurement ICC rejects unsupported setting variants", {
  skip_on_cran()

  dat <- data.frame(
    r1 = c(1, 2, 3),
    r2 = c(1.1, 2.2, 2.9)
  )
  base_setting <- data.frame(
    icc_model = "twoway",
    icc_unit = "single",
    icc_type = "",
    stringsAsFactors = FALSE
  )

  expect_true(is.finite(util_repeated_measurement_icc(
    dat,
    setting = base_setting,
    type = ""
  )))
  expect_true(is.na(util_repeated_measurement_icc(
    dat,
    setting = transform(base_setting, icc_model = "oneway"),
    type = "agreement"
  )))
  expect_true(is.na(util_repeated_measurement_icc(
    dat,
    setting = transform(base_setting, icc_unit = "average"),
    type = "agreement"
  )))
  expect_true(is.na(util_repeated_measurement_icc(
    dat,
    setting = base_setting,
    type = "unsupported"
  )))
})
