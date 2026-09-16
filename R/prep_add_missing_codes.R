# nolint start: line_length_linter.
#' Insert missing codes for `NA`s based on rules
#'
#' @inheritParams .template_function_indicator
#'
#' @param rules [data.frame] with the columns:
#'   - `resp_vars` or `VAR_NAMES`:
#'                  Variable, whose `NA`-values should be replaced by jump codes
#'   - `CODE_CLASS`: Either `MISSING` or `JUMP`: Is the currently described case
#'                   an expected missing value (`JUMP`) or not (`MISSING`)
#'   - `CODE_VALUE`: The jump code or missing code
#'   - `CODE_LABEL`: A label describing the reason for the missing value
#'   - `RULE`: A rule in `REDcap` style (see, e.g.,
#'   [`REDcap` help](https://help.redcap.ualberta.ca/help-and-faq/project-best-practices/data-quality/example-data-quality-rules) and
#'   [`REDcap` how-to](https://docs.google.com/document/d/1l3nGBgqqPKi5PtMe75g7q0dny8QzGMd_/edit?tab=t.0))
#'   that describes cases for the missing
#'   - `DATA_PREPARATION`: optional. if available, and `use_value_labels`` is
#'                                   either missing or `NA`, this columns
#'                                   controls the rule handling.
#' @param overwrite [logical] Also insert missing codes, if the values are not
#'                            `NA`
#' @param use_value_labels [logical] In rules for factors, use the value labels,
#'                                   not the codes. Defaults to `TRUE`, if any
#'                                   `VALUE_LABELS` are given in the metadata,
#'                                   and no `DATA_PREPARATION` exists for the
#'                                   `rules`. `NA` means to use
#'                                   `DATA_PREPARATION`, if available.
#'
#' @return a `list` with the entries:
#'   - `ModifiedStudyData`: Study data with `NA`s replaced by the `CODE_VALUE`
#'   - `ModifiedMetaData`: Metadata having the new codes amended in the columns
#'                         `JUMP_LIST` or `MISSING_LIST`, respectively
#'
#' @author Thomas J. Musholt contributed the performance improvement for simple
#'   rule evaluation.
#' @export
#'
# nolint end
prep_add_missing_codes <- function(resp_vars,
  study_data,
  meta_data_v2,
  item_level = "item_level",
  label_col,
  rules,
  use_value_labels = NA,
  overwrite = FALSE,
  meta_data = item_level) {
  util_maybe_load_meta_data_v2()
  util_expect_scalar(overwrite, check_type = is.logical)

  prep_prepare_dataframes(
    .replace_missings = FALSE,
    .replace_hard_limits = FALSE,
    .adjust_data_type = FALSE,
    .amend_scale_level = FALSE
  )

  return_missing_code_result <- function(ds1, meta_data) {
    ModifiedStudyData <- ds1
    ModifiedStudyData[] <- lapply(ModifiedStudyData, unlist)

    colnames(ModifiedStudyData) <-
      prep_map_labels(colnames(ds1),
        to = VAR_NAMES,
        from = label_col,
        meta_data = meta_data
      )

    modified_study_data_for_attr <- ModifiedStudyData
    attr(modified_study_data_for_attr, "study_data") <- NULL
    attr(ModifiedStudyData, "study_data") <-
      util_cast_off(modified_study_data_for_attr, "ModifiedStudyData", TRUE)

    list(
      ModifiedStudyData = ModifiedStudyData,
      ModifiedMetaData = meta_data
    )
  }

  util_expect_scalar(use_value_labels, check_type = is.logical, allow_na = TRUE)

  util_expect_data_frame(rules, c(
    CODE_CLASS,
    CODE_LABEL, CODE_VALUE, RULE
  ))

  if (all(c("resp_vars", VAR_NAMES) %in% colnames(rules))) {
    util_error(
      c(
        "Have %s as well as %s in %s. This is not supported,",
        "give only one of these columns, please."
      ),
      dQuote("resp_vars"),
      dQuote(VAR_NAMES),
      sQuote("rules"),
      applicability_problem = TRUE
    )
  }

  colnames(rules)[colnames(rules) == "resp_vars"] <-
    VAR_NAMES

  util_expect_data_frame(rules, c(
    VAR_NAMES, CODE_CLASS,
    CODE_LABEL, CODE_VALUE, RULE
  ))

  rules <- util_explode_var_names_lists(
    var_names_list =
      util_parse_assignments(rules[[VAR_NAMES]], multi_variate_text = TRUE),
    dframe = rules,
    col_name = VAR_NAMES
  )

  rules[[VAR_NAMES]] <- util_find_var_by_meta(
    rules[[VAR_NAMES]],
    meta_data = meta_data,
    label_col = label_col,
    target = label_col,
    ifnotfound = rules[[VAR_NAMES]]
  )

  rules_var_in_meta <- rules[[VAR_NAMES]] %in% meta_data[[label_col]]
  if (!all(rules_var_in_meta)) {
    util_message(
      c(
        "Found the following %s/%s in %s,",
        "which are not in the %s: %s.",
        "Ignoring these rules."
      ),
      sQuote(VAR_NAMES),
      sQuote("resp_vars"),
      sQuote("rules"),
      sQuote("meta_data"),
      util_pretty_vector_string(rules[[VAR_NAMES]][!rules_var_in_meta])
    )
    rules <- rules[rules_var_in_meta, , drop = FALSE]
  }

  if (!nrow(rules)) {
    return(return_missing_code_result(ds1, meta_data))
  }

  util_expect_data_frame(rules, list(
    VAR_NAMES = function(x) {
      all(x %in% colnames(ds1))
    },
    CODE_CLASS = function(x) {
      all(x %in% c("MISSING", "JUMP"))
    },
    CODE_LABEL = is.character,
    CODE_VALUE = util_is_valid_missing_codes,
    RULE = is.character
  ))

  if (missing(resp_vars) || identical(resp_vars, NA)) {
    resp_vars <- rules[[VAR_NAMES]]
  }

  util_correct_variable_use(resp_vars, allow_more_than_one = TRUE)

  rules <- rules[rules[[VAR_NAMES]] %in% resp_vars, , drop = FALSE]

  rules[[CODE_VALUE]] <- util_as_valid_missing_codes(rules[[CODE_VALUE]])

  compiled_rules <- lapply(setNames(nm = rules[[RULE]]), util_parse_redcap_rule)

  if (!DATA_PREPARATION %in% colnames(rules)) {
    if (is.na(use_value_labels)) {
      if (
        (VALUE_LABELS %in% colnames(meta_data) &&
            any(!util_empty(meta_data[[VALUE_LABELS]]))) ||
          (VALUE_LABEL_TABLE %in% colnames(meta_data) &&
              any(!util_empty(meta_data[[VALUE_LABEL_TABLE]]))) ||
          (STANDARDIZED_VOCABULARY_TABLE %in% colnames(meta_data) &&
              any(!util_empty(meta_data[[STANDARDIZED_VOCABULARY_TABLE]])))) {
        rules[[DATA_PREPARATION]] <- "LABEL"
      } else {
        rules[[DATA_PREPARATION]] <- ""
      }
    } else {
      if (use_value_labels) {
        rules[[DATA_PREPARATION]] <- "LABEL"
      } else {
        rules[[DATA_PREPARATION]] <- ""
      }
    }
  }

  rule_match <- mapply(
    SIMPLIFY = FALSE,
    rule = compiled_rules,
    data_preparation = rules[[DATA_PREPARATION]],
    FUN = function(rule, data_preparation) {
      to_apply <- util_parse_missing_code_data_preparation(data_preparation)
      if (util_can_eval_missing_code_rule_prepared(to_apply)) {
        return(util_eval_prepared_redcap_rule(
          rule = rule,
          ds1 = ds1,
          meta_data = meta_data,
          label_col = label_col
        ))
      }

      util_eval_rule(
        rule = rule,
        ds1 = ds1,
        meta_data = meta_data,
        replace_missing_by =
          util_missing_code_replacement_mode(to_apply),
        use_value_labels = ("LABEL" %in% to_apply),
        replace_limits = ("LIMITS" %in% to_apply)
      )
    }
  )

  na <- lapply(setNames(rules[[VAR_NAMES]], nm = rules[[RULE]]), function(rv) {
    r <- util_empty(ds1[[rv]])
    if (overwrite) {
      r[] <- TRUE
    }
    r
  })

  util_stop_if_not(length(rule_match) == length(na))

  add_miss <- lapply(
    setNames(nm = rules[[RULE]]),
    function(rl) {
      util_stop_if_not(length(na[[rl]]) ==
          length(rule_match[[rl]]))
      rm <- as.logical(rule_match[[rl]])
      rm[is.na(rm)] <- FALSE
      na[[rl]] & rm
    }
  )

  workload <- as.data.frame(add_miss,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  target_variable <- function(nm) {
    rules[rules[[RULE]] == nm, VAR_NAMES, drop = TRUE]
  }

  target_code <- function(nm) {
    rules[rules[[RULE]] == nm, CODE_VALUE, drop = TRUE]
  }

  for (rulenm in rules[[RULE]]) {
    tv <- target_variable(rulenm)
    for (tv0 in tv) {
      ds1[[tv0]][workload[[rulenm]]] <-
        rep(target_code(rulenm), sum(workload[[rulenm]]))
    }
  }

  cause_label_df_list <- prep_extract_cause_label_df(
    meta_data = meta_data,
    label_col = label_col
  )

  meta_data <- cause_label_df_list$meta_data
  cause_label_df <- cause_label_df_list$cause_label_df

  colnames(cause_label_df)[colnames(cause_label_df) == "resp_vars"] <-
    VAR_NAMES

  cause_label_df <- rbind(
    cause_label_df,
    rules[, colnames(cause_label_df), drop = FALSE]
  )

  if (!CODE_LABEL %in% colnames(cause_label_df)) {
    cause_label_df[[CODE_LABEL]] <-
      paste(cause_label_df[[CODE_CLASS]], cause_label_df[[CODE_VALUE]])
  }

  colnames(cause_label_df)[colnames(cause_label_df) == VAR_NAMES] <-
    "resp_vars"

  meta_data <-
    prep_add_cause_label_df(
      meta_data = meta_data,
      cause_label_df = cause_label_df,
      label_col = label_col
    )

  # The modified codes are now the raw basis for later rebuilds.
  # Keep attr(., "study_data") in sync so prep_prepare_dataframes() does not
  # fall back to the pre-amendment input values.
  return(return_missing_code_result(ds1, meta_data))
}

#' Parse DATA_PREPARATION for prep_add_missing_codes()
#'
#' @param data_preparation value from the DATA_PREPARATION column
#'
#' @return normalized DATA_PREPARATION tokens
#'
#' @noRd
util_parse_missing_code_data_preparation <- function(data_preparation) {
  to_apply <- util_parse_assignments(data_preparation)
  to_apply <- toupper(trimws(to_apply))
  to_apply <- to_apply[!is.na(to_apply)]
  to_apply <- to_apply[nzchar(to_apply)]
  if (sum(
    "MISSING_INTERPRET" %in% to_apply,
    "MISSING_LABEL" %in% to_apply,
    "MISSING_NA" %in% to_apply
  ) > 1) {
    util_warning(
      "Invalid %s in computation rules. Falling back to %s",
      sQuote(DATA_PREPARATION),
      dQuote("MISSING_NA")
    )
    to_apply <- to_apply[!startsWith(to_apply, "MISSING_")]
    to_apply <- c(to_apply, "MISSING_NA")
  }
  to_apply
}

#' Check if a missing-code rule can be evaluated without preparing data again
#'
#' @param to_apply normalized DATA_PREPARATION tokens
#'
#' @return TRUE for rules that can use the already prepared study data
#'
#' @noRd
util_can_eval_missing_code_rule_prepared <- function(to_apply) {
  !any(c(
    to_apply %in% c("LABEL", "LIMITS"),
    startsWith(to_apply, "MISSING_")
  ))
}

#' Translate DATA_PREPARATION tokens to util_eval_rule() missing-code mode
#'
#' @param to_apply normalized DATA_PREPARATION tokens
#'
#' @return replacement mode for util_eval_rule()
#'
#' @noRd
util_missing_code_replacement_mode <- function(to_apply) {
  if ("MISSING_NA" %in% to_apply) {
    return("NA")
  }
  if ("MISSING_INTERPRET" %in% to_apply) {
    return("INTERPRET")
  }
  if ("MISSING_LABEL" %in% to_apply) {
    return("LABEL")
  }
  ""
}

#' Explode list-columns of variable names to row-wise representation
#'
#' Expands a data frame by repeating each row according to the length of the
#' corresponding element in `var_names_list`. Each element of the list must be a
#' character vector. The resulting data frame contains one row per entry of all
#' character vectors, with the original columns preserved and an additional
#' column holding the exploded values.
#'
#' @param var_names_list A list of character vectors. Must have the same length
#'   as the number of rows in `dframe`.
#' @param dframe A `data.frame` whose rows are to be repeated.
#' @param col_name A single character string giving the name of the new column
#'   that will contain the unlisted values from `var_names_list`. Defaults to
#'   `"var_names"`.
#'
#' @returns A `data.frame` with repeated rows from `dframe` and one additional
#'   column named according to `col_name`, containing the values from
#'   `var_names_list`. Rows corresponding to empty elements (`character(0)`) in
#'   `var_names_list` are omitted.
#' @noRd
util_explode_var_names_lists <- function(var_names_list, dframe,
  col_name = "var_names") {
  util_stop_if_not(
    "'var_names_list' must be a list" = is.list(var_names_list)
  )

  util_stop_if_not(
    "'dframe' must be a data.frame" = is.data.frame(dframe)
  )

  util_stop_if_not(
    "'col_name' must be a single character value" =
      is.character(col_name) && length(col_name) == 1L && !is.na(col_name)
  )

  util_stop_if_not(
    "length(var_names_list) must match nrow(dframe)" =
      length(var_names_list) == nrow(dframe)
  )

  lens <- lengths(var_names_list)

  idx <- rep(seq_len(nrow(dframe)), lens)

  out <- dframe[idx, , drop = FALSE]

  out[[col_name]] <- unlist(var_names_list, use.names = FALSE)

  row.names(out) <- NULL

  out
}
