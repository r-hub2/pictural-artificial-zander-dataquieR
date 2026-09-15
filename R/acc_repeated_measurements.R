# nolint start: line_length_linter.
#' Check repeated measurements
#'
#' @description
#' Computes repeated-measurement checks for one cross-item group.
#'
#' [Indicator]
#'
#' @inheritParams .template_function_indicator
#'
#' @param variable_group [variable list] the names of the repeated-measurement
#'                                     variables. If empty, all eligible
#'                                     repeated-measurement groups from
#'                                     cross-item metadata are computed.
#' @param repeated_measures_metric [character] requested repeated-measurement
#'                                           methods. Direct calls may use a
#'                                           pipe-separated list.
#' @param repeated_measures_reference [variable] optional reference variable
#'                                             from `variable_group`. Alias for
#'                                             `repeated_measures_reference_vars`.
#' @param repeated_measures_reference_vars [variable] optional reference variable
#'                                                  from `variable_group`.
#' @param repeated_measures_metric_setting [character] optional setting id(s)
#'                                                  from
#'                                                  `REPEATED_MEASURES_METRIC_SETTING`.
#' @param repeated_measurement_settings [data.frame] optional statistical
#'                                                settings table or a registered
#'                                                data frame name.
#'
#' @return a list with:
#'   - `VariableGroupTable`: [data.frame] with indicator-metric columns
#'   - `VariableGroupData`: [data.frame] with report-facing labels
#'   - `OtherTable`: [data.frame] with one row per comparison and method
#'
#' @export
# nolint end
acc_repeated_measurements <- function(variable_group = NULL,
  study_data,
  label_col = VAR_NAMES,
  item_level = "item_level",
  meta_data = item_level,
  meta_data_v2,
  repeated_measures_metric = "",
  repeated_measures_reference = "",
  repeated_measures_reference_vars =
    repeated_measures_reference,
  repeated_measures_metric_setting = "",
  repeated_measurement_settings =
    c(
      "statistical_settings",
      "repeated_measurement_settings"
    )) {
  util_maybe_load_meta_data_v2()

  if (is.null(variable_group) || all(util_empty(variable_group))) {
    return(util_repeated_measurements_all_groups(
      study_data = study_data,
      label_col = label_col,
      meta_data = meta_data,
      repeated_measurement_settings = repeated_measurement_settings
    ))
  }

  if (all(util_empty(repeated_measures_metric))) {
    util_error(
      "No repeated-measurements metric requested",
      applicability_problem = TRUE,
      intrinsic_applicability_problem = TRUE
    )
  }

  repeated_measures_metric <-
    util_parse_repeated_measurement_metrics(repeated_measures_metric)
  repeated_measures_metric_setting <-
    util_parse_repeated_measurement_metric_settings(
      repeated_measures_metric_setting
    )

  # Add or remove supported repeated-measurement methods in
  # REPEATED_MEASURES_METRICS, defined in R/000_globs.R.
  unsupported_metrics <- setdiff(
    repeated_measures_metric,
    REPEATED_MEASURES_METRICS
  )
  if (length(unsupported_metrics) > 0) {
    util_warning(
      paste(
        "Unsupported repeated-measurement metric(s): %s.",
        "Ignoring unsupported metric(s). Supported metric(s): %s."
      ),
      prep_deparse_assignments(
        codes = unsupported_metrics,
        mode = "string_codes"
      ),
      prep_deparse_assignments(
        codes = REPEATED_MEASURES_METRICS,
        mode = "string_codes"
      ),
      applicability_problem = TRUE
    )
  }
  repeated_measures_metric <- intersect(
    repeated_measures_metric,
    REPEATED_MEASURES_METRICS
  )
  if (length(repeated_measures_metric) == 0) {
    util_error(
      "No supported repeated-measurement metric requested.",
      applicability_problem = TRUE
    )
  }

  prep_prepare_dataframes(.replace_hard_limits = TRUE)

  util_correct_variable_use(
    variable_group,
    allow_more_than_one = TRUE,
    allow_any_obs_na = TRUE
  )

  variable_group <- variable_group[!is.na(variable_group)]

  if (length(variable_group) < 2) {
    util_error(
      "Need at least two variables for repeated-measurement checks.",
      applicability_problem = TRUE,
      intrinsic_applicability_problem = .called_in_pipeline
    )
  }

  # Naming convention bridge:
  # `repeated_measures_reference` mirrors the metadata attribute
  # REPEATED_MEASURES_REFERENCE and is kept as the pipeline-facing alias.
  # `repeated_measures_reference_vars` is the canonical function argument
  # because dataquieR variable-name arguments should end in `_vars`.
  if (!missing(repeated_measures_reference_vars) &&
    !util_empty(repeated_measures_reference) &&
    !identical(
      trimws(repeated_measures_reference),
      trimws(repeated_measures_reference_vars)
    )) {
    util_error(
      paste(
        "Use either %s or %s,",
        "not both with different values."
      ),
      dQuote("repeated_measures_reference"),
      dQuote("repeated_measures_reference_vars"),
      applicability_problem = TRUE
    )
  }

  repeated_measures_reference_vars <- trimws(repeated_measures_reference_vars)

  if (!all(util_empty(repeated_measures_reference_vars))) {
    # Conceptually, this is one reference/gold-standard variable, not a group.
    util_correct_variable_use(
      "repeated_measures_reference_vars",
      allow_more_than_one = FALSE,
      allow_any_obs_na = TRUE
    )

    if (!repeated_measures_reference_vars %in% variable_group) {
      util_error(
        "The repeated-measurement reference variable %s is not part of %s.",
        dQuote(repeated_measures_reference_vars),
        sQuote("variable_group"),
        applicability_problem = TRUE
      )
    }
  }

  reference <- if (util_empty(repeated_measures_reference_vars)) {
    NA_character_
  } else {
    repeated_measures_reference_vars
  }

  effective_settings <-
    util_repeated_measurement_settings(repeated_measurement_settings)

  RepeatedMeasurementDetails <- util_compute_repeated_measurements(
    study_data = ds1,
    meta_data = meta_data,
    label_col = label_col,
    variable_group = variable_group,
    repeated_measures_metric = repeated_measures_metric,
    repeated_measures_reference_vars = reference,
    repeated_measures_metric_setting = repeated_measures_metric_setting,
    repeated_measurement_settings = effective_settings
  )

  VariableGroupTable <- util_repeated_measurement_variable_group_table(
    RepeatedMeasurementDetails
  )

  OtherTable <- RepeatedMeasurementDetails
  names(OtherTable)[names(OtherTable) == "Variables"] <- VARIABLE_LIST

  VariableGroupData <- RepeatedMeasurementDetails
  colnames(VariableGroupData) <- c(
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

  util_attach_attr(
    list(
      VariableGroupTable = VariableGroupTable,
      VariableGroupData = VariableGroupData,
      OtherTable = OtherTable
    ),
    sizing_hints = list(
      number_of_vars = length(variable_group)
    ),
    referred_tables = list(
      statistical_settings = effective_settings
    )
  )
}

#' Internal helper: repeated measurements all groups
#'
#' @noRd
util_repeated_measurements_all_groups <- function(study_data,
  label_col,
  meta_data,
  repeated_measurement_settings) {
  prep_prepare_dataframes(.replace_hard_limits = TRUE)
  meta_data_cross_item <- try(prep_get_data_frame("cross-item_level"),
    silent = TRUE
  )
  if (util_is_try_error(meta_data_cross_item) ||
      !is.data.frame(meta_data_cross_item) ||
      nrow(meta_data_cross_item) == 0 ||
      !(REPEATED_MEASURES_METRIC %in% names(meta_data_cross_item))) {
    util_error(
      "No repeated-measurements metric requested",
      applicability_problem = TRUE,
      intrinsic_applicability_problem = TRUE
    )
  }

  meta_data_cross_item <- util_normalize_cross_item(
    meta_data = meta_data,
    meta_data_cross_item = meta_data_cross_item,
    label_col = label_col
  )

  has_metric <- !util_empty(meta_data_cross_item[[REPEATED_MEASURES_METRIC]])
  meta_data_cross_item <- meta_data_cross_item[has_metric, , drop = FALSE]
  settings <- util_repeated_measurement_settings(repeated_measurement_settings)

  results <- lapply(seq_len(nrow(meta_data_cross_item)), function(i) {
    row <- meta_data_cross_item[i, , drop = FALSE]
    metrics <- try(util_parse_repeated_measurement_metrics(
      row[[REPEATED_MEASURES_METRIC]]
    ), silent = TRUE)
    if (util_is_try_error(metrics)) {
      return(NULL)
    }
    metrics <- intersect(metrics, REPEATED_MEASURES_METRICS)
    if (length(metrics) == 0) {
      return(NULL)
    }
    variable_group <- names(util_parse_assignments(
      row[[VARIABLE_LIST]],
      multi_variate_text = TRUE
    )[[1]])
    variable_group <- variable_group[!util_empty(variable_group)]
    if (length(variable_group) < 2) {
      return(NULL)
    }
    metric_setting <- if (REPEATED_MEASURES_METRIC_SETTING %in% names(row)) {
      row[[REPEATED_MEASURES_METRIC_SETTING]]
    } else {
      ""
    }
    reference <- if (REPEATED_MEASURES_REFERENCE %in% names(row)) {
      row[[REPEATED_MEASURES_REFERENCE]]
    } else {
      ""
    }

    result <- try(
      acc_repeated_measurements(
        variable_group = variable_group,
        study_data = ds1,
        label_col = label_col,
        meta_data = meta_data,
        repeated_measures_metric = metrics,
        repeated_measures_reference = reference,
        repeated_measures_metric_setting = metric_setting,
        repeated_measurement_settings = settings
      ),
      silent = TRUE
    )
    if (util_is_try_error(result)) {
      return(result)
    }
    util_add_variable_group_identity(
      result,
      check_id = row[[CHECK_ID]],
      check_label = row[[CHECK_LABEL]]
    )
  })

  results <- Filter(function(x) {
    !is.null(x) && !util_is_try_error(x)
  }, results)
  if (length(results) == 0) {
    util_error(
      "No supported repeated-measurement metric requested.",
      applicability_problem = TRUE
    )
  }

  VariableGroupTable <- do.call(
    rbind,
    lapply(results, `[[`, "VariableGroupTable")
  )
  VariableGroupData <- do.call(
    rbind,
    lapply(results, `[[`, "VariableGroupData")
  )
  OtherTable <- do.call(
    rbind,
    lapply(results, `[[`, "OtherTable")
  )
  rownames(VariableGroupTable) <- NULL
  rownames(VariableGroupData) <- NULL
  rownames(OtherTable) <- NULL

  util_attach_attr(
    list(
      VariableGroupTable = VariableGroupTable,
      VariableGroupData = VariableGroupData,
      OtherTable = OtherTable
    ),
    sizing_hints = list(
      number_of_vars = length(unique(unlist(strsplit(
        VariableGroupTable[[VARIABLE_LIST]],
        SPLIT_CHAR,
        fixed = TRUE
      ))))
    ),
    referred_tables = list(
      statistical_settings = settings
    )
  )
}

#' Internal helper: parse repeated measurement metrics
#'
#' @noRd
util_parse_repeated_measurement_metrics <- function(x) {
  metrics <- unname(unlist(
    util_parse_assignments(x, multi_variate_text = TRUE),
    use.names = FALSE
  ))
  metrics <- trimws(tolower(metrics))
  metrics <- metrics[!util_empty(metrics)]

  if (length(metrics) == 0) {
    util_error(
      "No repeated-measurements metric requested",
      applicability_problem = TRUE,
      intrinsic_applicability_problem = TRUE
    )
  }

  unique(metrics)
}

#' Internal helper: parse repeated measurement metric settings
#'
#' @noRd
util_parse_repeated_measurement_metric_settings <- function(x) {
  if (all(util_empty(x))) {
    return(character(0))
  }
  settings <- unname(unlist(
    util_parse_assignments(x, multi_variate_text = TRUE),
    use.names = FALSE
  ))
  settings <- trimws(settings)
  settings[!util_empty(settings)]
}

## Metric computation core -----------------------------------------------------

#' Internal helper: compute repeated measurement metric
#'
#' @noRd
util_compute_repeated_measurement_metric <- function(study_data,
  meta_data,
  label_col,
  vars,
  metric,
  setting) {
  dat <- study_data[, vars, drop = FALSE]
  n <- nrow(dat)
  complete <- stats::complete.cases(dat)
  dat <- dat[complete, , drop = FALSE]
  n_complete <- nrow(dat)
  n_missing <- n - n_complete

  if (n_complete == 0) {
    return(util_repeated_measurement_result(
      status = "not_computable",
      n = n,
      n_complete = n_complete,
      n_missing = n_missing
    ))
  }
  if (n_complete < setting$min_n) {
    return(util_repeated_measurement_result(
      status = "not_computable",
      n = n,
      n_complete = n_complete,
      n_missing = n_missing,
      warning = sprintf(
        "Insufficient complete observations: n_complete = %s, min_n = %s.",
        n_complete,
        setting$min_n
      )
    ))
  }
  if (length(vars) < setting$min_repeats) {
    return(util_repeated_measurement_result(
      status = "not_computable",
      n = n,
      n_complete = n_complete,
      n_missing = n_missing,
      warning = sprintf(
        "Insufficient repeated measurements: repeats = %s, min_repeats = %s.",
        length(vars),
        setting$min_repeats
      )
    ))
  }

  metric_result <- switch(metric,
    concordance_correlation = util_repeated_measurement_result_value(
      util_repeated_measurement_concordance(dat[[vars[1]]], dat[[vars[2]]])
    ),
    cohen_kappa = util_repeated_measurement_result_value(
      util_repeated_measurement_cohen_kappa(dat[[vars[1]]], dat[[vars[2]]])
    ),
    weighted_kappa = util_repeated_measurement_result_value(
      util_repeated_measurement_weighted_kappa(
        dat[[vars[1]]], dat[[vars[2]]],
        weights = setting$kappa_weights
      )
    ),
    fleiss_kappa = util_repeated_measurement_result_value(
      util_repeated_measurement_fleiss_kappa(dat)
    ),
    percent_agreement = util_repeated_measurement_result_value(
      util_repeated_measurement_percent_agreement(dat)
    ),
    mean_difference = util_repeated_measurement_result_value(
      mean(
        util_as_numeric(dat[[vars[1]]]) -
          util_as_numeric(dat[[vars[2]]])
      )
    ),
    mean_absolute_difference = util_repeated_measurement_result_value(
      mean(abs(
        util_as_numeric(dat[[vars[1]]]) -
          util_as_numeric(dat[[vars[2]]])
      ))
    ),
    rmse = util_repeated_measurement_result_value(
      sqrt(mean((
        util_as_numeric(dat[[vars[1]]]) -
          util_as_numeric(dat[[vars[2]]])
      )^2))
    ),
    within_subject_sd = util_repeated_measurement_result_value(
      util_repeated_measurement_within_subject_sd(dat)
    ),
    coefficient_of_variation = util_repeated_measurement_result_value(
      util_repeated_measurement_coefficient_of_variation(dat, setting = setting)
    ),
    sensitivity = util_repeated_measurement_sens_spec(
      dat[[vars[1]]], dat[[vars[2]]],
      test_var = vars[1],
      ref_var = vars[2],
      meta_data = meta_data,
      label_col = label_col,
      metric = metric
    ),
    specificity = util_repeated_measurement_sens_spec(
      dat[[vars[1]]], dat[[vars[2]]],
      test_var = vars[1],
      ref_var = vars[2],
      meta_data = meta_data,
      label_col = label_col,
      metric = metric
    ),
    icc_agreement = util_repeated_measurement_result_value(
      util_repeated_measurement_icc(dat, setting = setting, type = "agreement")
    ),
    icc_consistency = util_repeated_measurement_result_value(
      util_repeated_measurement_icc(dat, setting = setting, type = "consistency") # nolint: line_length_linter.
    ),
    util_repeated_measurement_result_value(NA_real_)
  )

  value <- metric_result$value
  status <- if (is.na(value)) "not_computable" else "ok"

  util_repeated_measurement_result(
    value = value,
    status = status,
    n = n,
    n_complete = n_complete,
    n_missing = n_missing,
    warning = metric_result$warning
  )
}

#' Internal helper: repeated measurement settings
#'
#' @noRd
util_repeated_measurement_settings <- function(settings =
    c(
      "statistical_settings",
      "repeated_measurement_settings"
    )) {
  defaults <- util_repeated_measurement_default_settings()
  if (is.null(settings) || all(util_empty(settings))) {
    return(defaults)
  }
  if (is.character(settings)) {
    loaded_settings <- NULL
    for (setting_name in settings) {
      loaded_settings <- tryCatch(
        prep_get_data_frame(setting_name),
        error = function(e) NULL
      )
      if (!is.null(loaded_settings)) {
        break
      }
    }
    settings <- loaded_settings
    if (is.null(loaded_settings)) {
      return(defaults)
    }
  }
  if (!is.data.frame(settings)) {
    util_error(
      "%s must be a data frame or a registered data frame name.",
      sQuote("repeated_measurement_settings"),
      applicability_problem = TRUE
    )
  }
  util_repeated_measurement_apply_setting_overrides(defaults, settings)
}

#' Internal helper: repeated measurement apply setting overrides
#'
#' @noRd
util_repeated_measurement_apply_setting_overrides <- function(defaults,
  overrides) {
  defaults <- util_repeated_measurement_normalize_setting_names(defaults)
  overrides <- util_repeated_measurement_normalize_setting_names(overrides)
  if (!("setting_id" %in% names(overrides))) {
    util_error(
      "The repeated-measurement settings table needs a %s column.",
      sQuote("SETTING_ID"),
      applicability_problem = TRUE
    )
  }

  unknown_cols <- setdiff(names(overrides), names(defaults))
  if (length(unknown_cols) > 0) {
    util_message(
      "Ignoring unknown repeated-measurement setting column(s): %s.",
      prep_deparse_assignments(codes = unknown_cols, mode = "string_codes")
    )
  }
  overrides <- overrides[, intersect(names(overrides), names(defaults)),
    drop = FALSE
  ]
  result <- Reduce(function(result, i) {
    setting_id <- trimws(as.character(overrides$setting_id[i]))
    if (util_empty(setting_id)) {
      return(result)
    }
    target <- match(setting_id, result$setting_id)
    if (is.na(target)) {
      new_row <- defaults[1, , drop = FALSE]
      new_row[] <- NA
      new_row$setting_id <- setting_id
      result <- rbind(result, new_row)
      target <- nrow(result)
    }
    value_cols <- setdiff(names(overrides), "setting_id")
    has_value <- !vapply(
      overrides[i, value_cols, drop = FALSE],
      util_empty,
      logical(1)
    )
    if (any(has_value)) {
      result[target, value_cols[has_value]] <-
        overrides[i, value_cols[has_value], drop = FALSE]
    }
    result
  }, seq_len(nrow(overrides)), init = defaults)

  util_repeated_measurement_standardize_settings(result)
}

#' Internal helper: repeated measurement default settings
#'
#' @noRd
util_repeated_measurement_default_settings <- function() {
  f <- system.file("repeatedMeasurementSettings.rds", package = "dataquieR")
  if (!nzchar(f)) {
    f <- file.path("inst", "repeatedMeasurementSettings.rds")
  }
  util_repeated_measurement_normalize_setting_names(readRDS(f))
}

#' Internal helper: repeated measurement normalize setting names
#'
#' @noRd
util_repeated_measurement_normalize_setting_names <- function(settings) {
  names(settings) <- tolower(names(settings))
  settings
}

#' Internal helper: repeated measurement standardize settings
#'
#' @noRd
util_repeated_measurement_standardize_settings <- function(settings) {
  defaults <- util_repeated_measurement_default_settings()
  missing_cols <- setdiff(names(defaults), names(settings))
  settings[missing_cols] <- lapply(defaults[missing_cols], function(x) {
    rep(NA, nrow(settings))
  })
  settings <- settings[, names(defaults), drop = FALSE]
  settings$setting_id <- trimws(as.character(settings$setting_id))
  settings$metric <- trimws(tolower(as.character(settings$metric)))
  logical_defaults <- c(
    is_default = FALSE,
    enabled = TRUE,
    requires_reference = FALSE,
    allows_reference = TRUE,
    higher_is_better = NA
  )
  settings[names(logical_defaults)] <- Map(
    util_repeated_measurement_logical_column,
    settings[names(logical_defaults)],
    logical_defaults
  )
  integer_defaults <- c(
    min_n = 1L,
    min_repeats = 2L,
    max_repeats = NA_integer_,
    bootstrap_n = 0L
  )
  settings[names(integer_defaults)] <- Map(function(x, default) {
    out <- suppressWarnings(as.integer(x))
    out[is.na(out)] <- default
    out
  }, settings[names(integer_defaults)], integer_defaults)
  settings
}

#' Internal helper: repeated measurement logical column
#'
#' @noRd
util_repeated_measurement_logical_column <- function(x, default = FALSE) {
  vapply(x, function(value) {
    if (util_empty(value)) {
      return(default)
    }
    if (is.logical(value)) {
      return(value[1])
    }
    value <- tolower(trimws(as.character(value[1])))
    if (value %in% c("true", "t", "1", "yes", "y")) {
      TRUE
    } else if (value %in% c("false", "f", "0", "no", "n")) {
      FALSE
    } else {
      default
    }
  }, logical(1))
}

#' Internal helper: repeated measurement select setting
#'
#' @noRd
util_repeated_measurement_select_setting <- function(settings,
  metric,
  metric_index,
  setting_ids,
  n_metrics) {
  setting_id <- NA_character_
  if (length(setting_ids) > 0) {
    if (length(setting_ids) == 1 && n_metrics == 1) {
      setting_id <- setting_ids[1]
    } else if (length(setting_ids) == n_metrics) {
      setting_id <- setting_ids[metric_index]
    } else {
      return(list(
        setting = NULL,
        warning = paste(
          "Ambiguous REPEATED_MEASURES_METRIC_SETTING:",
          "provide no value, one setting for one metric, or one setting per",
          "metric in the same order."
        )
      ))
    }
    hit <- settings[settings$setting_id == setting_id & settings$enabled, ,
      drop = FALSE
    ]
    if (nrow(hit) == 0) {
      return(list(
        setting = NULL,
        warning = sprintf(
          "REPEATED_MEASURES_METRIC_SETTING %s was not found as an enabled setting.", # nolint: line_length_linter.
          dQuote(setting_id)
        )
      ))
    }
    if (!identical(hit$metric[1], metric)) {
      return(list(
        setting = NULL,
        warning = sprintf(
          "REPEATED_MEASURES_METRIC_SETTING %s is defined for metric %s but %s was requested.", # nolint: line_length_linter.
          dQuote(setting_id),
          dQuote(hit$metric[1]),
          dQuote(metric)
        )
      ))
    }
    return(list(setting = hit[1, , drop = FALSE], warning = NA_character_))
  }

  candidates <- settings[settings$metric == metric & settings$enabled, ,
    drop = FALSE
  ]
  if (nrow(candidates) == 0) {
    return(list(
      setting = NULL,
      warning = sprintf(
        "Metric %s has no enabled repeated-measurement setting.",
        dQuote(metric)
      )
    ))
  }
  defaults <- candidates[candidates$is_default, , drop = FALSE]
  if (nrow(defaults) == 1) {
    return(list(setting = defaults[1, , drop = FALSE], warning = NA_character_))
  }
  if (nrow(defaults) > 1) {
    return(list(
      setting = NULL,
      warning = sprintf(
        "Metric %s has more than one default setting.",
        dQuote(metric)
      )
    ))
  }
  list(
    setting = candidates[1, , drop = FALSE],
    warning = sprintf(
      "Metric %s has no explicit default setting; using first enabled setting.",
      dQuote(metric)
    )
  )
}

#' Internal helper: repeated measurement warning row
#'
#' @noRd
util_repeated_measurement_warning_row <- function(variable_group,
  metric,
  repeated_measures_reference_vars,
  setting_id,
  warning,
  method_details =
    NA_character_) {
  data.frame(
    Variables = paste(variable_group, collapse = sprintf(" %s ", SPLIT_CHAR)),
    repeated_measures_metric = metric,
    comparison_variables = NA_character_,
    repeated_measures_reference = repeated_measures_reference_vars,
    repeated_measures_metric_setting = setting_id,
    repeated_measures_value = NA_real_,
    repeated_measures_ci_low = NA_real_,
    repeated_measures_ci_high = NA_real_,
    repeated_measures_n = NA_integer_,
    repeated_measures_n_complete = NA_integer_,
    repeated_measures_n_missing = NA_integer_,
    repeated_measures_status = "not_computable",
    repeated_measures_method = method_details,
    repeated_measures_warning = warning,
    stringsAsFactors = FALSE
  )
}

#' Internal helper: repeated measurement method details
#'
#' @noRd
util_repeated_measurement_method_details <- function(setting) {
  paste(na.omit(c(setting$method_label, setting$method_note)),
    collapse = sprintf(" %s ", SPLIT_CHAR)
  )
}

#' Internal helper: compute repeated measurements
#'
#' @noRd
util_compute_repeated_measurements <- function(study_data,
  meta_data,
  label_col,
  variable_group,
  repeated_measures_metric,
  repeated_measures_reference_vars =
    NA_character_,
  repeated_measures_metric_setting =
    character(0),
  repeated_measurement_settings) {
  rows <- lapply(seq_along(repeated_measures_metric), function(metric_index) {
    metric <- repeated_measures_metric[metric_index]
    selected_setting <- util_repeated_measurement_select_setting(
      settings = repeated_measurement_settings,
      metric = metric,
      metric_index = metric_index,
      setting_ids = repeated_measures_metric_setting,
      n_metrics = length(repeated_measures_metric)
    )
    setting <- selected_setting$setting
    setting_warning <- selected_setting$warning
    if (is.null(setting)) {
      return(util_repeated_measurement_warning_row(
        variable_group = variable_group,
        metric = metric,
        repeated_measures_reference_vars = repeated_measures_reference_vars,
        setting_id = NA_character_,
        warning = setting_warning
      ))
    }
    if (isTRUE(setting$requires_reference) &&
        all(util_empty(repeated_measures_reference_vars))) {
      return(util_repeated_measurement_warning_row(
        variable_group = variable_group,
        metric = metric,
        repeated_measures_reference_vars = repeated_measures_reference_vars,
        setting_id = setting$setting_id,
        method_details = util_repeated_measurement_method_details(setting),
        warning = sprintf(
          "Metric %s requires a repeated-measurement reference.",
          dQuote(metric)
        )
      ))
    }
    comparisons <- util_repeated_measurement_comparisons(
      variable_group = variable_group,
      setting = setting,
      repeated_measures_reference_vars = repeated_measures_reference_vars
    )
    do.call(rbind, lapply(comparisons, function(vars) {
      res <- util_compute_repeated_measurement_metric(
        study_data = study_data,
        meta_data = meta_data,
        label_col = label_col,
        vars = vars,
        metric = metric,
        setting = setting
      )
      data.frame(
        Variables = paste(variable_group,
          collapse = sprintf(" %s ", SPLIT_CHAR)
        ),
        repeated_measures_metric = metric,
        comparison_variables = paste(vars,
          collapse = sprintf(" %s ", SPLIT_CHAR)
        ),
        repeated_measures_reference = repeated_measures_reference_vars,
        repeated_measures_metric_setting = setting$setting_id,
        repeated_measures_value = res$value,
        repeated_measures_ci_low = res$ci_low,
        repeated_measures_ci_high = res$ci_high,
        repeated_measures_n = res$n,
        repeated_measures_n_complete = res$n_complete,
        repeated_measures_n_missing = res$n_missing,
        repeated_measures_status = res$status,
        repeated_measures_method = util_repeated_measurement_method_details(
          setting
        ),
        repeated_measures_warning = paste(
          na.omit(c(setting_warning, res$warning)),
          collapse = sprintf(" %s ", SPLIT_CHAR)
        ),
        stringsAsFactors = FALSE
      )
    }))
  })

  SummaryTable <- do.call(rbind, rows)
  rownames(SummaryTable) <- NULL
  SummaryTable
}

#' Internal helper: repeated measurement variable group table
#'
#' @noRd
util_repeated_measurement_variable_group_table <- function(details) {
  metric_cols <- c(
    "ICC_acc_drm_inter",
    "NUM_acc_drm_inter",
    "NUM_acc_drm_gold"
  )
  variables <- details$comparison_variables
  use_group_label <- vapply(variables, util_empty, logical(1))
  variables[use_group_label] <- details$Variables[use_group_label]
  VariableGroupTable <- data.frame(
    VARIABLE_LIST = variables,
    stringsAsFactors = FALSE
  )
  VariableGroupTable[metric_cols] <- NA_real_

  has_reference <- !vapply(
    details$repeated_measures_reference,
    util_empty,
    logical(1)
  )
  metric_col <- ifelse(
    grepl("^icc_", details$repeated_measures_metric),
    "ICC_acc_drm_inter",
    ifelse(
      has_reference,
      "NUM_acc_drm_gold",
      "NUM_acc_drm_inter"
    )
  )
  ok <- length(metric_col) == nrow(details) &
    details$repeated_measures_status == "ok"
  if (any(ok)) {
    VariableGroupTable[cbind(
      which(ok),
      match(
        metric_col[ok],
        names(VariableGroupTable)
      )
    )] <-
      details$repeated_measures_value[ok]
  }
  VariableGroupTable
}

#' Internal helper: repeated measurement comparisons
#'
#' @noRd
util_repeated_measurement_comparisons <- function(variable_group,
  setting,
  repeated_measures_reference_vars =
    NA_character_) {
  mode <- tolower(trimws(setting$comparison_mode_default))
  if (util_empty(mode)) {
    mode <- "pairwise"
  }
  if (mode == "global") {
    return(list(variable_group))
  }

  if (mode == "reference_pairwise" ||
      !all(util_empty(repeated_measures_reference_vars))) {
    if (all(util_empty(repeated_measures_reference_vars))) {
      return(list())
    }
    comparison_vars <- setdiff(variable_group, repeated_measures_reference_vars)
    return(lapply(comparison_vars, c, repeated_measures_reference_vars))
  }

  utils::combn(variable_group, 2, simplify = FALSE)
}

#' Internal helper: repeated measurement result value
#'
#' @noRd
util_repeated_measurement_result_value <- function(value,
  warning = NA_character_) {
  list(value = value, warning = warning)
}

#' Internal helper: repeated measurement result
#'
#' @noRd
util_repeated_measurement_result <- function(value = NA_real_,
  ci_low = NA_real_,
  ci_high = NA_real_,
  status,
  n,
  n_complete,
  n_missing,
  warning = NA_character_) {
  list(
    value = value,
    ci_low = ci_low,
    ci_high = ci_high,
    status = status,
    n = n,
    n_complete = n_complete,
    n_missing = n_missing,
    warning = warning
  )
}

#' Internal helper: repeated measurement concordance
#'
#' @noRd
util_repeated_measurement_concordance <- function(x, y) {
  x <- util_as_numeric(x)
  y <- util_as_numeric(y)
  if (length(x) < 2 || stats::var(x) == 0 || stats::var(y) == 0) {
    return(NA_real_)
  }
  rho <- suppressWarnings(stats::cor(x, y))
  (2 * rho * stats::sd(x) * stats::sd(y)) /
    (stats::var(x) + stats::var(y) + (mean(x) - mean(y))^2)
}

#' Internal helper: repeated measurement cohen kappa
#'
#' @noRd
util_repeated_measurement_cohen_kappa <- function(x, y) {
  x <- as.character(x)
  y <- as.character(y)
  lv <- sort(unique(c(x, y)))
  if (length(lv) < 2) {
    return(NA_real_)
  }
  tab <- table(factor(x, levels = lv), factor(y, levels = lv))
  n <- sum(tab)
  if (n == 0) {
    return(NA_real_)
  }
  po <- sum(diag(tab)) / n
  pe <- sum(rowSums(tab) * colSums(tab)) / (n^2)
  if (isTRUE(all.equal(1, pe))) {
    return(NA_real_)
  }
  (po - pe) / (1 - pe)
}

#' Internal helper: repeated measurement weighted kappa
#'
#' @noRd
util_repeated_measurement_weighted_kappa <- function(x, y, weights = "squared") { # nolint: line_length_linter.
  x <- as.character(x)
  y <- as.character(y)
  lv <- sort(unique(c(x, y)))
  k <- length(lv)
  if (k < 2) {
    return(NA_real_)
  }
  tab <- table(factor(x, levels = lv), factor(y, levels = lv))
  n <- sum(tab)
  if (n == 0) {
    return(NA_real_)
  }
  obs <- tab / n
  exp <- outer(rowSums(obs), colSums(obs))
  idx <- seq_len(k)
  distance <- abs(outer(idx, idx, "-"))
  weights <- tolower(trimws(as.character(weights[1])))
  weights <- switch(weights,
    linear = distance / (k - 1),
    unweighted = 1 - diag(k),
    squared = (distance / (k - 1))^2,
    (distance / (k - 1))^2
  )
  denominator <- sum(weights * exp)
  if (denominator == 0) {
    return(NA_real_)
  }
  1 - sum(weights * obs) / denominator
}

#' Internal helper: repeated measurement fleiss kappa
#'
#' @noRd
util_repeated_measurement_fleiss_kappa <- function(dat) {
  if (ncol(dat) < 3 || nrow(dat) < 1) {
    return(NA_real_)
  }
  cats <- sort(unique(unlist(
    lapply(dat, as.character),
    use.names = FALSE
  )))
  if (length(cats) < 2) {
    return(NA_real_)
  }
  counts <- t(apply(dat, 1, function(row) {
    tabulate(match(as.character(row), cats), nbins = length(cats))
  }))
  n_raters <- ncol(dat)
  p_i <- (rowSums(counts^2) - n_raters) / (n_raters * (n_raters - 1))
  p_j <- colSums(counts) / (nrow(dat) * n_raters)
  p_bar <- mean(p_i)
  p_e <- sum(p_j^2)
  if (isTRUE(all.equal(1, p_e))) {
    return(NA_real_)
  }
  (p_bar - p_e) / (1 - p_e)
}

#' Internal helper: repeated measurement percent agreement
#'
#' @noRd
util_repeated_measurement_percent_agreement <- function(dat) {
  if (ncol(dat) == 2) {
    return(mean(
      as.character(dat[[1]]) == as.character(dat[[2]])
    ))
  }
  mean(apply(dat, 1, function(row) {
    length(unique(as.character(row))) == 1
  }))
}

#' Internal helper: repeated measurement within subject sd
#'
#' @noRd
util_repeated_measurement_within_subject_sd <- function(dat) {
  mat <- as.matrix(as.data.frame(
    lapply(dat, util_as_numeric)
  ))
  if (ncol(mat) < 2 || nrow(mat) < 1) {
    return(NA_real_)
  }
  subject_means <- rowMeans(mat)
  sqrt(sum((mat - subject_means)^2) / (nrow(mat) * (ncol(mat) - 1)))
}

#' Internal helper: repeated measurement coefficient of variation
#'
#' @noRd
util_repeated_measurement_coefficient_of_variation <- function(dat, setting) {
  wssd <- util_repeated_measurement_within_subject_sd(dat)
  numeric_dat <- as.data.frame(lapply(dat, util_as_numeric))
  denominator <- mean(as.matrix(numeric_dat), na.rm = TRUE)
  if (is.na(wssd) || is.na(denominator) || denominator == 0) {
    return(NA_real_)
  }
  value <- wssd / denominator
  if (identical(tolower(trimws(as.character(setting$cv_scale[1]))), "percent")) { # nolint: line_length_linter.
    value <- value * 100
  }
  value
}

#' Internal helper: repeated measurement event meta
#'
#' @noRd
util_repeated_measurement_event_meta <- function(meta_data, label_col, var) {
  meta_data <- util_normalize_dichotomization_metadata(meta_data, label_col)
  meta_row <- meta_data[meta_data[[label_col]] == var, , drop = FALSE]
  if (nrow(meta_row) == 0 ||
      (util_empty(meta_row[[RECODE_CASES]]) &&
          util_empty(meta_row[[RECODE_CONTROL]]))) {
    return(NULL)
  }
  meta_row <- meta_row[1, , drop = FALSE]
  meta_row
}

#' Internal helper: repeated measurement event vector
#'
#' @noRd
util_repeated_measurement_event_vector <- function(x, var, meta_data,
  label_col) {
  event_meta <- util_repeated_measurement_event_meta(meta_data, label_col, var)
  if (is.null(event_meta)) {
    return(NULL)
  }
  study_data <- data.frame(x, stringsAsFactors = FALSE, check.names = FALSE)
  names(study_data) <- var
  event_data <- suppressWarnings(util_dichotomize(
    study_data = study_data,
    meta_data = event_meta,
    label_col = label_col
  ))
  if (is.null(util_attr(event_data, "Dichotomization", exact = TRUE)[[var]])) {
    return(NULL)
  }
  event_data[[var]] == 1
}

#' Internal helper: repeated measurement sens spec
#'
#' @noRd
util_repeated_measurement_sens_spec <- function(test,
  ref,
  test_var,
  ref_var,
  meta_data,
  label_col,
  metric) {
  test_event <- util_repeated_measurement_event_vector(
    test, test_var, meta_data, label_col
  )
  ref_event <- util_repeated_measurement_event_vector(
    ref, ref_var, meta_data, label_col
  )
  missing_event_meta <- character(0)
  if (is.null(test_event)) {
    missing_event_meta <- c(missing_event_meta, "test variable")
  }
  if (is.null(ref_event)) {
    missing_event_meta <- c(missing_event_meta, "reference variable")
  }
  if (length(missing_event_meta) > 0) {
    return(util_repeated_measurement_result_value(
      NA_real_,
      warning = paste(
        "EVENT_LEVELS or RECODE_CASES is required for",
        sprintf(
          "%s but missing for %s.", metric,
          paste(missing_event_meta, collapse = " and ")
        )
      )
    ))
  }
  tp <- sum(test_event & ref_event)
  tn <- sum(!test_event & !ref_event)
  fp <- sum(test_event & !ref_event)
  fn <- sum(!test_event & ref_event)
  value <- if (metric == "sensitivity") {
    if ((tp + fn) > 0) tp / (tp + fn) else NA_real_
  } else {
    if ((tn + fp) > 0) tn / (tn + fp) else NA_real_
  }
  util_repeated_measurement_result_value(value)
}

#' Internal helper: repeated measurement icc
#'
#' @noRd
util_repeated_measurement_icc <- function(dat, setting, type) {
  mat <- as.matrix(as.data.frame(lapply(dat, util_as_numeric)))
  if (ncol(mat) < 2 || nrow(mat) < 2 || any(!is.finite(mat))) {
    return(NA_real_)
  }

  n_subjects <- nrow(mat)
  n_repeats <- ncol(mat)
  grand_mean <- mean(mat)
  subject_mean <- rowMeans(mat)
  repeat_mean <- colMeans(mat)

  ss_subject <- n_repeats * sum((subject_mean - grand_mean)^2)
  ss_repeat <- n_subjects * sum((repeat_mean - grand_mean)^2)
  residual <- sweep(sweep(mat, 1, subject_mean, "-"), 2, repeat_mean, "-") +
    grand_mean
  ss_error <- sum(residual^2)

  ms_subject <- ss_subject / (n_subjects - 1)
  ms_repeat <- ss_repeat / (n_repeats - 1)
  ms_error <- ss_error / ((n_subjects - 1) * (n_repeats - 1))

  if (!is.finite(ms_subject) || !is.finite(ms_error)) {
    return(NA_real_)
  }

  model <- trimws(as.character(setting$icc_model[1]))
  unit <- trimws(as.character(setting$icc_unit[1]))
  if (util_empty(type)) {
    type <- trimws(as.character(setting$icc_type[1]))
  }
  model <- tolower(model)
  unit <- tolower(unit)
  type <- tolower(type)
  if (!util_empty(model) && model != "twoway") {
    return(NA_real_)
  }
  if (!util_empty(unit) && unit != "single") {
    return(NA_real_)
  }
  if (util_empty(type)) {
    type <- "agreement"
  }
  denominator <- if (type == "consistency") {
    ms_subject + (n_repeats - 1) * ms_error
  } else if (type == "agreement") {
    ms_subject + (n_repeats - 1) * ms_error +
      n_repeats * (ms_repeat - ms_error) / n_subjects
  } else {
    NA_real_
  }

  if (!is.finite(denominator) || denominator == 0) {
    return(NA_real_)
  }

  (ms_subject - ms_error) / denominator
}
