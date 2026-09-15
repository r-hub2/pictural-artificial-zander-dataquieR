# nolint start: line_length_linter.
#' Compute questionnaire or SSI results
#'
#' @description
#' Computes only the Social Science Item (SSI) result objects that are implied
#' by the cross-item metadata. This is a lightweight computation entry point for
#' users who want the questionnaire-specific results directly in R without
#' generating a full `dq_report2()` or `dq_report_by()` HTML report.
#'
#' @inheritParams dq_report2
#'
#' @return If one SSI result is computed, a [dataquieR_result]. If several SSI
#'   results are computed, a combined `master_result` object. If the metadata
#'   does not define SSI computations, an empty named [list].
#'
#' @details
#' `dq_questionnaire()` prepares the same metadata inputs as [dq_report2()], but
#' then generates and evaluates only the SSI calls listed in the package SSI
#' concept metadata. Consequently, consistency and accuracy functions are only
#' run when the SSI metadata mapping marks them as questionnaire-relevant.
#'
#' @examples
#' \dontrun{
#' dq_questionnaire(meta_data_v2 = "meta_data_v2.xlsx")
#' }
#'
#' @export
# nolint end
dq_questionnaire <- function(study_data,
  item_level = "item_level",
  label_col = LABEL,
  meta_data_segment = "segment_level",
  meta_data_dataframe = "dataframe_level",
  meta_data_cross_item = "cross-item_level",
  meta_data_item_computation =
    "item_computation_level",
  meta_data = item_level,
  meta_data_v2,
  ...,
  cores = NULL,
  ignore_empty_vars =
    getOption(
      "dataquieR.ignore_empty_vars",
      dataquieR.ignore_empty_vars_default
    ),
  specific_args = list(),
  advanced_options = list(),
  debug_parallel = FALSE,
  resp_vars = character(0),
  filter_result_slots = c(
    "^Summary",
    "^Segment",
    "^DataTypePlotList",
    "^ReportSummaryTable",
    "^Dataframe",
    "^Result",
    "^VariableGroup"
  ),
  mode = c("default", "futures", "queue", "parallel"),
  mode_args = list(),
  cross_item_level,
  `cross-item_level`,
  segment_level,
  dataframe_level,
  item_computation_level,
  .internal =
    rlang::env_inherits(
      rlang::caller_env(),
      parent.env(environment())
    ),
  name_of_study_data,
  dt_adjust = as.logical(getOption(
    "dataquieR.dt_adjust",
    dataquieR.dt_adjust_default
  ))) {
  mode <- util_match_arg(mode)
  util_guard_rstudio_user_cluster(cores, advanced_options)

  withr::local_options(
    c(
      list(
        dataquieR.CONDITIONS_WITH_STACKTRACE = FALSE,
        dataquieR.ERRORS_WITH_CALLER = FALSE,
        dataquieR.MESSAGES_WITH_CALLER = FALSE,
        dataquieR.WARNINGS_WITH_CALLER = FALSE
      ),
      advanced_options
    )
  )

  if (!missing(meta_data_v2)) {
    util_message(
      "Have %s set, so I'll remove all loaded data frames",
      sQuote("meta_data_v2")
    )
    prep_purge_data_frame_cache()
    prep_load_workbook_like_file(meta_data_v2)
  }

  util_ck_arg_aliases()

  if (missing(study_data)) {
    df_study_data <- util_find_study_data_from_dataframe_level(
      meta_data_dataframe = meta_data_dataframe
    )
    if (!is.null(df_study_data)) {
      util_message(
        "Using %s from your dataframe level metadata",
        dQuote(df_study_data$name_of_study_data)
      )
      study_data <- df_study_data$study_data
      if (missing(name_of_study_data)) {
        name_of_study_data <- df_study_data$name_of_study_data
      }
    } else if ("study_data" %in% prep_list_dataframes()) {
      util_message("Using %s from the dataframe cache.", dQuote("study_data"))
      study_data <- prep_get_data_frame("study_data", keep_types = TRUE)
      if (missing(name_of_study_data)) {
        name_of_study_data <- "study_data"
      }
    } else {
      util_error(
        c(
          "Missing %s. Please pass it explicitly, load it into the",
          "data-frame cache, or provide dataframe-level metadata with",
          "loadable study-data references."
        ),
        sQuote("study_data")
      )
    }
  }

  if (missing(name_of_study_data)) {
    if (is.data.frame(study_data)) {
      name_of_study_data <- substr(
        head(as.character(substitute(study_data)), 1),
        1, 500
      )
    } else if (length(study_data) == 1 && is.character(study_data)) {
      name_of_study_data <- study_data
    } else {
      name_of_study_data <- "study_data"
    }
  } else {
    util_expect_scalar(name_of_study_data, check_type = is.character)
    name_of_study_data <- substr(name_of_study_data, 1, 500)
  }

  util_expect_data_frame(study_data, keep_types = TRUE)
  util_register_primary_study_data(
    study_data = study_data,
    name_of_study_data = name_of_study_data
  )
  util_handle_val_tab()

  if (!.internal) {
    util_verify_names(name_of_study_data = name_of_study_data)
  }

  prepared <- util_prepare_questionnaire_inputs(
    study_data = study_data,
    meta_data = meta_data,
    label_col = label_col,
    meta_data_segment = meta_data_segment,
    meta_data_dataframe = meta_data_dataframe,
    meta_data_cross_item = meta_data_cross_item,
    meta_data_item_computation = meta_data_item_computation,
    resp_vars = resp_vars,
    ignore_empty_vars = ignore_empty_vars,
    name_of_study_data = name_of_study_data
  )

  if (dt_adjust) {
    prepared$study_data <- util_adjust_data_type(
      study_data = prepared$study_data,
      meta_data = prepared$meta_data,
      relevant_vars_for_warnings = NULL
    )
  }

  all_calls <- util_generate_questionnaire_calls(
    meta_data = prepared$meta_data,
    label_col = prepared$label_col,
    meta_data_segment = prepared$meta_data_segment,
    meta_data_dataframe = prepared$meta_data_dataframe,
    meta_data_cross_item = prepared$meta_data_cross_item,
    specific_args = specific_args,
    arg_overrides = list(...),
    resp_vars = prepared$resp_vars
  )

  results <- util_evaluate_questionnaire_calls(
    all_calls = all_calls,
    study_data = prepared$study_data,
    meta_data = prepared$meta_data,
    label_col = prepared$label_col,
    meta_data_segment = prepared$meta_data_segment,
    meta_data_dataframe = prepared$meta_data_dataframe,
    meta_data_cross_item = prepared$meta_data_cross_item,
    filter_result_slots = filter_result_slots
  )
  results <- util_combine_questionnaire_results(
    results = results,
    all_calls = all_calls,
    meta_data = prepared$meta_data,
    meta_data_cross_item = prepared$meta_data_cross_item
  )

  util_questionnaire_results(results)
}

#' Internal helper: prepare questionnaire inputs
#'
#' @noRd
util_prepare_questionnaire_inputs <- function(study_data,
  meta_data,
  label_col,
  meta_data_segment,
  meta_data_dataframe,
  meta_data_cross_item,
  meta_data_item_computation,
  resp_vars,
  ignore_empty_vars,
  name_of_study_data) {
  try(util_expect_data_frame(meta_data), silent = TRUE)
  case_insens <- util_is_na_0_empty_or_false(
    getOption(
      "dataquieR.study_data_colnames_case_sensitive",
      dataquieR.study_data_colnames_case_sensitive_default
    )
  )

  if (case_insens) {
    colnames(study_data) <- util_align_colnames_case(
      .colnames = colnames(study_data),
      .var_names = meta_data[[VAR_NAMES]]
    )
  }

  in_study <- FALSE
  ci_in_study <- FALSE
  if (is.data.frame(meta_data)) {
    in_study <- meta_data[[VAR_NAMES]] %in% colnames(study_data)
    if (!case_insens) {
      ci_in_study <- tolower(meta_data[[VAR_NAMES]]) %in%
        tolower(colnames(study_data))
    }
  }
  if (identical(getOption(
    "dataquieR.ELEMENT_MISSMATCH_CHECKTYPE",
    dataquieR.ELEMENT_MISSMATCH_CHECKTYPE_default
  ), "subset_u")) {
    if (is.data.frame(meta_data)) {
      meta_data <- meta_data[in_study, , drop = FALSE]
    }
  }

  try(
    meta_data <- util_prepare_item_level_metadata(
      meta_data = meta_data,
      label_col = label_col
    ),
    silent = TRUE
  )
  if (!is.data.frame(meta_data) || !prod(dim(meta_data))) {
    try_ci <- ""
    if (!case_insens && any(ci_in_study)) {
      try_ci <- sprintf(
        paste(
          "But maybe, if you enable case-insensitive mapping",
          "of meta_data on study data using %s, it could work?"
        ),
        sQuote("options(dataquieR.study_data_colnames_case_sensitive = FALSE)")
      )
    }
    util_warning(
      paste(
        "No item level metadata matching study data found. Will guess",
        "some from the study data. This will not be very helpful, please",
        "consider passing an item level metadata file.",
        try_ci
      ),
      immediate = TRUE
    )
    predicted <- prep_study2meta(study_data, convert_factors = TRUE)
    meta_data <- predicted$MetaData
    study_data <- predicted$ModifiedStudyData
  } else {
    rownames(meta_data) <- NULL
  }

  meta_data_segment <- util_ensure_segment_metadata(
    meta_data_segment = meta_data_segment,
    meta_data = meta_data
  )
  try(util_expect_data_frame(meta_data_dataframe), silent = TRUE)
  if (!is.data.frame(meta_data_dataframe)) {
    util_message(
      "No dataframe level metadata %s found.",
      dQuote(meta_data_dataframe)
    )
    meta_data_dataframe <- util_dataframe_metadata_for_names(
      name_of_study_data,
      include_df_code = FALSE,
      include_df_id_vars = FALSE
    )
  } else {
    rownames(meta_data_dataframe) <- NULL
  }
  meta_data_cross_item <- util_ensure_cross_item_metadata(
    meta_data_cross_item
  )

  suppressWarnings(util_ensure_in(VAR_NAMES, names(meta_data),
    error = TRUE,
    err_msg = sprintf(
      "Did not find the mandatory column %%s in the %s.",
      sQuote("meta_data")
    )
  ))

  util_expect_scalar(label_col, check_type = is.character)
  util_ensure_in(label_col, names(meta_data),
    error = TRUE,
    err_msg = sprintf(
      "Did not find a label column (%s) named %%s in the %s. Did you mean %%s?",
      sQuote("label_col"),
      sQuote("meta_data")
    )
  )

  try(util_expect_data_frame(meta_data_item_computation), silent = TRUE)
  if (!is.data.frame(meta_data_item_computation)) {
    meta_data_item_computation <- data.frame()
  }

  cross_item_already_normalized <- identical(
    util_attr(meta_data_cross_item, "normalized", exact = TRUE),
    TRUE
  )

  prepared_label_modification_text <- NULL
  prepared_label_modification_table <- NULL
  if (!util_dataquieR_inputs_prepared(study_data, meta_data)) {
    prepared_inputs <- util_prepare_dataquieR_inputs(
      study_data = study_data,
      meta_data = meta_data,
      label_col = label_col,
      meta_data_cross_item = meta_data_cross_item,
      meta_data_item_computation = meta_data_item_computation,
      name_of_study_data = name_of_study_data
    )
    study_data <- prepared_inputs$study_data
    meta_data <- prepared_inputs$meta_data
    prepared_label_modification_text <-
      prepared_inputs$label_modification_text
    prepared_label_modification_table <-
      prepared_inputs$label_modification_table
  }

  mod_label <- util_ensure_label(
    meta_data = meta_data,
    label_col = label_col
  )
  if (!is.null(mod_label$label_modification_text)) {
    meta_data <- mod_label$meta_data
    label_col <- mod_label$label_col
  }
  mod_label$label_modification_text <- trimws(paste(
    prepared_label_modification_text,
    mod_label$label_modification_text
  ))
  mod_label$label_modification_table <- rbind(
    prepared_label_modification_table,
    mod_label$label_modification_table
  )
  meta_data <- util_fill_empty_label_columns(meta_data, label_col = label_col)

  if (!cross_item_already_normalized) {
    meta_data_cross_item <- util_normalize_cross_item(
      meta_data = meta_data,
      meta_data_cross_item = meta_data_cross_item,
      label_col = label_col
    )
  }

  all_vars <- (length(resp_vars) == 0)
  if (!all_vars && nrow(meta_data_cross_item) > 0) {
    meta_data_cross_item <- util_filter_cross_item_metadata(
      meta_data_cross_item = meta_data_cross_item,
      resp_vars = resp_vars,
      meta_data = meta_data,
      label_col = label_col
    )
  }

  util_expect_scalar(resp_vars,
    allow_more_than_one = TRUE,
    allow_null = TRUE,
    check_type = is.character
  )

  md100 <- meta_data
  miss_from_study <- !(md100[[VAR_NAMES]] %in% colnames(study_data))
  only_nas <- vapply(setNames(md100[[VAR_NAMES]], nm = md100[[label_col]]),
    function(vn) {
      all(util_empty(study_data[[vn]]))
    },
    FUN.VALUE = logical(1)
  )

  if (any(miss_from_study)) {
    vars_not_found <- paste0(
      dQuote(paste0(
        md100[miss_from_study, label_col, drop = TRUE], " (",
        md100[miss_from_study, VAR_NAMES, drop = TRUE], ")"
      )),
      collapse = ", "
    )
    util_message(
      c(
        "Could not find the following variables in %s:",
        "%s.\nThese will be preliminarily removed from the %s."
      ),
      sQuote("study_data"),
      vars_not_found,
      sQuote("meta_data")
    )
    md100 <- md100[!miss_from_study, , drop = FALSE]
  }

  if (all_vars) {
    resp_vars <- md100[[label_col]]
  } else {
    resp_vars_m <- util_find_var_by_meta(
      resp_vars = resp_vars,
      meta_data = md100,
      label_col = label_col,
      target = label_col,
      ifnotfound = NA_character_
    )
    if (any(is.na(resp_vars_m))) {
      util_warning(
        c(
          "Could not find the following variables in %s:",
          "%s.\nThese will be removed."
        ),
        sQuote("meta_data"),
        paste0(dQuote(resp_vars[is.na(resp_vars_m)]), collapse = ", ")
      )
    }
    resp_vars <- resp_vars_m[!is.na(resp_vars_m)]
  }

  to_remove <- intersect(resp_vars, names(which(only_nas)))
  orig_ignore_empty_vars <- ignore_empty_vars

  if (length(resp_vars) == 0) {
    util_error("No response variables left.")
  }

  if (ignore_empty_vars == "auto") {
    ignore_empty_vars <- length(to_remove) / length(resp_vars) > .2
  } else {
    ignore_empty_vars <- as.logical(ignore_empty_vars)
  }

  if (ignore_empty_vars) {
    util_warning(
      c(
        "%s was %s, so removing the following variables,",
        "because they only feature empty values: %s"
      ),
      sQuote("ignore_empty_vars"),
      sQuote(orig_ignore_empty_vars),
      util_pretty_vector_string(to_remove, n_max = 6),
      immediate = TRUE
    )
    resp_vars <- setdiff(resp_vars, to_remove)
  } else if (length(to_remove) > 0) {
    util_message("Have variables only featuring empty data values: %s",
      util_pretty_vector_string(to_remove, n_max = 6),
      immediate = TRUE
    )
  }

  util_reset_cache()
  if (getOption("dataquieR.precomputeStudyData",
      default = dataquieR.precomputeStudyData_default
    )) {
    util_populate_study_data_cache(study_data, meta_data, label_col = LABEL)
  } else {
    util_purge_study_data_cache()
  }

  scale_level <- util_amend_scale_level_once(
    study_data = study_data,
    meta_data = meta_data,
    label_col = label_col
  )
  meta_data <- scale_level$meta_data

  list(
    study_data = study_data,
    meta_data = meta_data,
    label_col = label_col,
    meta_data_segment = meta_data_segment,
    meta_data_dataframe = meta_data_dataframe,
    meta_data_cross_item = meta_data_cross_item,
    resp_vars = resp_vars
  )
}

#' Internal helper: generate questionnaire calls
#'
#' @noRd
util_generate_questionnaire_calls <- function(meta_data,
  label_col,
  meta_data_segment,
  meta_data_dataframe,
  meta_data_cross_item,
  specific_args,
  arg_overrides,
  resp_vars) {
  ssi_functions <- util_questionnaire_ssi_functions(meta_data_cross_item)

  if (length(ssi_functions) == 0) {
    return(util_attach_attr(
      list(),
      rn = character(0),
      cn = character(0),
      multivariatcol = character(0)
    ))
  }

  all_calls <- lapply(seq_along(ssi_functions), function(idx) {
    util_generate_calls_for_function(
      fkt = ssi_functions[idx],
      meta_data = meta_data,
      label_col = label_col,
      meta_data_segment = meta_data_segment,
      meta_data_dataframe = meta_data_dataframe,
      meta_data_cross_item = meta_data_cross_item,
      specific_args = specific_args,
      arg_overrides = arg_overrides,
      resp_vars = resp_vars,
      ssi_functions = ssi_functions,
      non_ssi_functions = character()
    )
  })
  names(all_calls) <- unname(ssi_functions)

  util_finalize_generated_calls(
    all_calls = all_calls,
    meta_data = meta_data,
    label_col = label_col
  )
}

#' Internal helper: evaluate questionnaire calls
#'
#' @noRd
util_evaluate_questionnaire_calls <- function(all_calls,
  study_data,
  meta_data,
  label_col,
  meta_data_segment,
  meta_data_dataframe,
  meta_data_cross_item,
  filter_result_slots) {
  if (length(all_calls) == 0) {
    return(list())
  }

  env <- list2env(
    list(
      study_data = study_data,
      meta_data = meta_data,
      label_col = label_col,
      meta_data_segment = meta_data_segment,
      meta_data_dataframe = meta_data_dataframe,
      meta_data_cross_item = meta_data_cross_item
    ),
    parent = asNamespace(utils::packageName())
  )

  result_order <- util_questionnaire_call_order(
    all_calls = all_calls,
    meta_data = meta_data,
    meta_data_cross_item = meta_data_cross_item
  )
  result_names <- names(all_calls)[result_order]
  results <- lapply(result_names, function(result_name) {
    call <- all_calls[[result_name]]
    result <- util_eval_to_dataquieR_result(
      expression = call,
      env = env,
      filter_result_slots = filter_result_slots,
      nm = result_name,
      function_name = as.character(call[[1]]),
      my_call = call,
      called_in_pipeline = TRUE,
      checkpoint_resumed = FALSE
    )
    if (inherits(result, "compressed")) {
      result <- util_decompress(result)
    }
    attr(result, "dq_result_title") <- util_questionnaire_result_title(
      call = call,
      meta_data = meta_data,
      label_col = label_col,
      meta_data_cross_item = meta_data_cross_item
    )
    result
  })
  names(results) <- result_names
  results
}

#' Internal helper: questionnaire results
#'
#' @noRd
util_questionnaire_results <- function(results) {
  questionnaire_grouped <- identical(
    util_attr(results, "dq_questionnaire_grouped", exact = TRUE),
    TRUE
  )
  names(results) <- names(results)

  if (length(results) == 0) {
    return(results)
  }

  results <- util_questionnaire_add_section_anchors(results)

  if (length(results) == 1) {
    result <- results[[1]]
    attr(result, "dq_questionnaire_result") <- TRUE
    return(result)
  }

  combined <- if (questionnaire_grouped) {
    util_sectioned_master_result_from_result_list(results, title_mode = "ssi")
  } else {
    util_master_result_from_result_list(results, title_mode = "ssi")
  }
  if (is.null(combined)) {
    combined <- util_sectioned_master_result_from_result_list(results,
      title_mode = "ssi"
    )
  }
  attr(combined, "dq_questionnaire_result") <- TRUE
  combined
}

#' Internal helper: questionnaire result title
#'
#' @noRd
util_questionnaire_result_title <- function(call,
  meta_data,
  label_col,
  meta_data_cross_item) {
  resp_vars <- try(eval(call[["resp_vars"]]), silent = TRUE)
  if (util_is_try_error(resp_vars) ||
      length(resp_vars) != 1 ||
      is.na(resp_vars)) {
    return(NA_character_)
  }

  var_name <- util_attr(call, VAR_NAMES, exact = TRUE)
  if (length(var_name) != 1 || is.na(var_name)) {
    var_name <- util_map_labels(resp_vars,
      meta_data,
      from = label_col,
      to = VAR_NAMES,
      ifnotfound = resp_vars,
      warn_ambiguous = FALSE
    )
  }
  if (length(var_name) != 1 || is.na(var_name)) {
    return(NA_character_)
  }

  check_id <- util_map_labels(var_name,
    meta_data,
    from = VAR_NAMES,
    to = CHECK_ID,
    ifnotfound = NA_character_,
    warn_ambiguous = FALSE
  )
  group_title <- util_questionnaire_group_title(
    check_id = check_id,
    meta_data_cross_item = meta_data_cross_item
  )

  computed_role <- util_map_labels(var_name,
    meta_data,
    from = VAR_NAMES,
    to = COMPUTED_VARIABLE_ROLE,
    ifnotfound = NA_character_,
    warn_ambiguous = FALSE
  )
  metric_title <- util_questionnaire_metric_title(computed_role)

  if (!util_empty(group_title) && !util_empty(metric_title)) {
    return(paste(group_title, metric_title, sep = ": "))
  }
  if (!util_empty(group_title)) {
    return(group_title)
  }
  if (!util_empty(metric_title)) {
    return(metric_title)
  }
  NA_character_
}

#' Internal helper: questionnaire group title
#'
#' @noRd
util_questionnaire_group_title <- function(check_id, meta_data_cross_item) {
  if (length(check_id) != 1 ||
      is.na(check_id) ||
      util_empty(check_id) ||
      !CHECK_ID %in% colnames(meta_data_cross_item)) {
    return(NA_character_)
  }

  row <- match(check_id, meta_data_cross_item[[CHECK_ID]])
  if (is.na(row)) {
    return(NA_character_)
  }

  titles <- util_generate_pages_ssi_cross_item_titles(
    meta_data_cross_item[row, , drop = FALSE]
  )
  unname(titles[["long_title"]])
}

#' Internal helper: questionnaire metric title
#'
#' @noRd
util_questionnaire_metric_title <- function(computed_role) {
  if (length(computed_role) != 1 ||
      is.na(computed_role) ||
      util_empty(computed_role)) {
    return(NA_character_)
  }
  computed_role <- unname(computed_role)

  ssi_metric <- names(COMPUTED_VARIABLE_ROLES)[
    vapply(COMPUTED_VARIABLE_ROLES, function(role) {
      identical(role, computed_role)
    }, FUN.VALUE = logical(1))
  ]
  ssi_metric <- setdiff(ssi_metric, "NA")
  if (length(ssi_metric) != 1) {
    return(NA_character_)
  }

  title <- util_get_concept_info("ssi",
    get("SSI_METRICS") == ssi_metric,
    "result_caption",
    drop = TRUE
  )
  if (length(title) != 1 || is.na(title) || util_empty(title)) {
    title <- util_get_concept_info("ssi",
      get("SSI_METRICS") == ssi_metric,
      "menu_label",
      drop = TRUE
    )
  }
  if (length(title) != 1 || is.na(title) || util_empty(title)) {
    return(NA_character_)
  }

  as.character(title)
}
