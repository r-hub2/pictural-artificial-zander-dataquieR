# nolint start: line_length_linter.
#' Delete rows from a summary table by result scope
#'
#' @inheritParams .template_function_indicator
#' @param repsumtab [data.frame] the report summary table
#' @param vars_to_include `"study"`, `"ssi"`, `"variable_group"`, or a
#'   combination of these scopes.
#' @param variable_group_call_names call names that emit a
#'   `VariableGroupTable` result.
#' @param study_var_names names of the original study variables. Results for
#'   other entities are retained for the variable-group scope, even if their
#'   producing call does not expose a `VariableGroupTable`.
#'
#' @returns [data.frame] the filtered `repsumtab` with attribute
#'   `rownames_of_report`, also filtered
#' @keywords internal
#' @noRd
# nolint end
util_filter_repsum <- function(repsumtab, vars_to_include, meta_data,
  rownames_of_report, label_col, variable_group_call_names = character(0),
  study_var_names = meta_data[[VAR_NAMES]]) {
  util_expect_scalar(vars_to_include,
    allow_more_than_one = TRUE,
    min_length = 1,
    max_length = 3,
    check_type = is.character
  )
  util_match_arg(vars_to_include,
    c("study", "ssi", "variable_group"),
    several_ok = TRUE
  )

  if (!(COMPUTED_VARIABLE_ROLE %in% names(meta_data))) {
    meta_data[[COMPUTED_VARIABLE_ROLE]] <- NA_character_
  }
  ssi_vars <-
    meta_data[[VAR_NAMES]][!util_empty(meta_data[[COMPUTED_VARIABLE_ROLE]])]
  ssi_vars_lb <-
    meta_data[[label_col]][!util_empty(meta_data[[COMPUTED_VARIABLE_ROLE]])]

  is_ssi <- repsumtab[[VAR_NAMES]] %in% ssi_vars
  detail_label_column <- ".variable_group_result_label"
  is_variable_group_output <- if (
    "call_names" %in% names(repsumtab) &&
      detail_label_column %in% names(repsumtab)
  ) {
    repsumtab$call_names %in% variable_group_call_names &
      !util_empty(repsumtab[[detail_label_column]])
  } else {
    rep(FALSE, nrow(repsumtab))
  }
  is_known_study_variable <- repsumtab[[VAR_NAMES]] %in% study_var_names
  group_only_functions <- setdiff(
    util_report_scope_target_functions("variable_group"),
    util_report_scope_target_functions("item")
  )
  is_group_only_function <- if ("function_name" %in% names(repsumtab)) {
    repsumtab$function_name %in% group_only_functions
  } else {
    rep(FALSE, nrow(repsumtab))
  }
  is_mismatched_item_scope <- is_known_study_variable & !is_ssi &
    is_group_only_function & !is_variable_group_output
  is_study <- is_known_study_variable & !is_ssi &
    !is_mismatched_item_scope &
    !is_variable_group_output
  is_variable_group <- !is_known_study_variable |
    is_variable_group_output

  keep <- rep(FALSE, nrow(repsumtab))
  if ("study" %in% vars_to_include) keep <- keep | is_study
  if ("ssi" %in% vars_to_include) keep <- keep | is_ssi
  if ("variable_group" %in% vars_to_include) keep <- keep | is_variable_group
  repsumtab <- repsumtab[keep, , drop = FALSE]

  if (identical(vars_to_include, "variable_group") &&
      "indicator_metric" %in% names(repsumtab)) {
    translated_metrics <- util_translate_indicator_metrics(
      as.character(repsumtab$indicator_metric)
    )
    known_metric <- !is.na(translated_metrics) &
      !util_empty(translated_metrics)
    diagnostic_metric <- startsWith(
      as.character(repsumtab$indicator_metric),
      "CAT_"
    ) | startsWith(
      as.character(repsumtab$indicator_metric),
      "MSG_"
    )
    known_metric <- known_metric |
      diagnostic_metric |
      util_empty(repsumtab$indicator_metric) |
      as.character(repsumtab$indicator_metric) == "EMPTY_OUTPUT"
    repsumtab <- repsumtab[known_metric, , drop = FALSE]
  }

  if (identical(vars_to_include, "ssi")) {
    rownames_of_report <- rownames_of_report[
      rownames_of_report %in% ssi_vars_lb
    ]
  } else if (identical(vars_to_include, "study")) {
    study_vars_lb <- meta_data[[label_col]][meta_data[[VAR_NAMES]] %in%
        study_var_names & util_empty(meta_data[[COMPUTED_VARIABLE_ROLE]])]
    rownames_of_report <- rownames_of_report[
      rownames_of_report %in% study_vars_lb
    ]
  }

  util_attach_attr(repsumtab, rownames_of_report = rownames_of_report)
}
