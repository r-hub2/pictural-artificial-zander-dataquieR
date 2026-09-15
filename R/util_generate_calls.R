#' Generate an execution/calling plan for computing a report from the metadata
#'
#' @inheritParams .template_function_report_plan
#'
#' @param dimensions [dimensions] Vector of dimensions to address in the report.
#'                   Allowed values in the vector are Completeness, Consistency,
#'                   and Accuracy. The generated report will only cover the
#'                   listed data quality dimensions. Accuracy is computational
#'                   expensive, so this dimension is not enabled by default.
#'                   Completeness should be included, if Consistency is
#'                   included, and Consistency should be included, if Accuracy
#'                   is included to avoid misleading detections of e.g. missing
#'                   codes as outliers, please refer to the data quality concept
#'                   for more details. Integrity is always included.
#' @param filter_indicator_functions [character] regular expressions, only
#'                                               if an indicator function's name
#'                                               matches one of these, it'll
#'                                               be used for the report. If
#'                                               of length zero, no filtering
#'                                               is performed.
#' @param exclude_indicator_functions [character] regular expressions,
#'                                               if an indicator function's name
#'                                               matches one of these, it'll
#'                                               be excluded from the report. If
#'                                               of length zero, no filtering
#'                                               is performed.
#'
#' @return a list of calls
#'
#' @family reporting_functions
#' @concept process
#' @noRd
util_generate_calls <- function(dimensions,
  meta_data,
  label_col,
  meta_data_segment,
  meta_data_dataframe,
  meta_data_cross_item,
  specific_args,
  arg_overrides,
  resp_vars,
  filter_indicator_functions,
  exclude_indicator_functions) {
  .dimensions <- unique(c("Descriptors", "Integrity", dimensions))
  .dimensions <- substr(tolower(.dimensions), 1, 3)
  .dimensions[.dimensions == "int"] <- "int_all" # use the wrappers, only
  .dimensions <- paste(paste0(.dimensions, "_"), collapse = "|")
  .dimensions <- sprintf("^(%s)", .dimensions)
  exports <- getNamespaceExports(utils::packageName())
  ind_functions <- grep(.dimensions, exports, value = TRUE)
  ind_functions <- c(ind_functions, "int_encoding_errors")
  ind_functions <- ind_functions[
    vapply(ind_functions,
      exists,
      envir = getNamespace(utils::packageName()),
      mode = "function", FUN.VALUE = logical(1)
    )
  ]
  ind_functions <- c(setdiff(ind_functions, c(
    "con_contradictions", # we use the new cross-item-level now
    "acc_robust_univariate_outlier", # is just a synonym for acc_univariate_outlier # nolint: line_length_linter.
    "acc_distributions", # there are specific functions for location and proportion checks # nolint: line_length_linter.
    "com_unit_missingness", # function does not really create something reasonable, currently # nolint: line_length_linter.
    "des_summary" # function just meant to be called directly by the user, more specific functions are available in the pipeline # nolint: line_length_linter.
  )), "int_datatype_matrix") # the data type matrix function does not start with int_all, but integrity should always run. However, it is still run before the pipeline, see util_evalute_calls and int_data_type_matrix # nolint: line_length_linter.

  ind_functions <- setNames(nm = ind_functions)

  ind_functions <- util_filter_names_by_regexps(
    ind_functions,
    filter_indicator_functions
  )

  ind_functions <- util_filter_names_by_regexps(ind_functions,
    exclude_indicator_functions,
    negate = TRUE
  )

  ssi_functions <- util_questionnaire_ssi_functions(meta_data_cross_item)

  all_calls <- lapply(setNames(nm = union(ind_functions, ssi_functions)),
    util_generate_calls_for_function,
    meta_data = meta_data,
    label_col = label_col,
    meta_data_segment = meta_data_segment,
    meta_data_dataframe = meta_data_dataframe,
    meta_data_cross_item = meta_data_cross_item,
    specific_args = specific_args,
    arg_overrides = arg_overrides,
    resp_vars = resp_vars,
    ssi_functions = ssi_functions,
    non_ssi_functions = ind_functions
  )


  util_finalize_generated_calls(
    all_calls = all_calls,
    meta_data = meta_data,
    label_col = label_col
  )
}

#' Internal helper: questionnaire ssi functions
#'
#' @noRd
util_questionnaire_ssi_functions <- function(meta_data_cross_item) {
  ssi_functions <- character(0)
  try(
    {
      ssi_functions <- unlist(util_parse_assignments(util_get_concept_info(
        "ssi",
        get("SSI_METRICS") %in% colnames(meta_data_cross_item),
        "functions",
        drop = TRUE
      )))
    },
    silent = TRUE
  )
  aliases <- unique(gsub("\\..*$", "", ssi_functions))
  function_names <- aliases
  missing_functions <- !vapply(function_names, exists, logical(1),
    mode = "function",
    envir = asNamespace(utils::packageName())
  )
  function_names[missing_functions] <- sub("_ssi$", "",
    function_names[missing_functions]
  )
  setNames(function_names, aliases)
}

#' Internal helper: finalize generated calls
#'
#' @noRd
util_finalize_generated_calls <- function(all_calls, meta_data, label_col) {
  explode_splinter <- function(entity_name, splinter, fkt) {
    check_id <- util_attr(splinter, CHECK_ID, exact = TRUE)
    check_label <- util_attr(splinter, CHECK_LABEL, exact = TRUE)
    to_explode <- vapply(lapply(splinter, util_attr, "explode"),
      identical,
      FUN.VALUE = logical(1),
      y = TRUE
    )
    if (all(!to_explode)) {
      res <- do.call(
        what = call,
        args = c(list(name = fkt), splinter),
        quote = TRUE
      )
      attr(res, "entity_name") <- entity_name
      attr(res, CHECK_ID) <- check_id
      attr(res, CHECK_LABEL) <- check_label
      attr(res, VAR_NAMES) <- util_map_labels(
        x = entity_name,
        to = VAR_NAMES,
        from = label_col,
        ifnotfound = NA_character_,
        meta_data = meta_data
      )
      if (!(STUDY_SEGMENT %in% colnames(meta_data))) {
        meta_data[[STUDY_SEGMENT]] <- rep(NA_character_, nrow(meta_data))
      }
      attr(res, STUDY_SEGMENT) <- util_map_labels(
        x = entity_name,
        to = STUDY_SEGMENT,
        from = label_col,
        ifnotfound = NA_character_,
        meta_data = meta_data
      )
      if (!(GRADING_RULESET %in% colnames(meta_data))) {
        meta_data[[GRADING_RULESET]] <- rep(0, nrow(meta_data))
      }
      attr(res, GRADING_RULESET) <- util_map_labels(
        x = entity_name,
        to = GRADING_RULESET,
        from = label_col,
        ifnotfound = 0,
        meta_data = meta_data
      )
      attr(res, "label_col") <- label_col
      return(res)
    }
    my_splinter <- head(splinter[to_explode], 1)
    remainder <- tail(splinter[to_explode], -1)
    not_a_bomb <- splinter[!to_explode]
    lapply(my_splinter[[1]], function(my_splinter_part) {
      my_arg <- setNames(
        list(my_splinter_part),
        nm = names(my_splinter)
      )
      arg_list <- c(my_arg, remainder, not_a_bomb)
      attr(arg_list, CHECK_ID) <- check_id
      attr(arg_list, CHECK_LABEL) <- check_label
      explode_splinter(
        entity_name = entity_name,
        splinter = arg_list,
        fkt = fkt
      )
    })
  }

  explode <- function(fkt, bomb) {
    mapply(
      SIMPLIFY = FALSE,
      names(bomb),
      bomb,
      FUN = explode_splinter,
      MoreArgs = list(fkt = fkt)
    )
  }

  all_calls <- unlist(mapply(
    SIMPLIFY = FALSE,
    names(all_calls), all_calls, FUN = explode
  ))


  has_resp_vars <- vapply(lapply(all_calls, names),
    `%in%`,
    x = "resp_vars", FUN.VALUE = logical(1)
  )
  suppress <- has_resp_vars &
    vapply(lapply(all_calls, `[[`, "resp_vars"), length,
      FUN.VALUE = integer(1)
    ) == 0

  all_calls <- all_calls[!suppress]

  make_name <- function(nm, call) {
    entity_name <- util_attr(call, "entity_name", exact = TRUE)
    r <- gsub(".", "_", fixed = TRUE, sub( # the alias names must not contain dots ".", the first dot separates the call alias from the entity name (mostly VAR_NAMES) # nolint: line_length_linter.
      paste0(
        "\\.\\Q",
        entity_name,
        "\\E"
      ),
      "",
      nm,
      perl = TRUE
    ))
    r <- sub("_TIME_VAR", "", r) # all with explode = TRUE
    r <- sub("_GROUP_VAR", "", r)
    r <- paste0(tolower(r), ".", entity_name)
    r
  }

  names(all_calls) <- unlist(mapply(
    SIMPLIFY = FALSE,
    names(all_calls), all_calls, FUN = make_name
  ))

  util_stop_if_not(all(grepl(fixed = TRUE, ".", names(all_calls)))) # there must be at least one dot to separated the col. name from the row name (wolog this is the first dots) # nolint: line_length_linter.

  if (length(all_calls)) {
    rn <- util_sub_string_right_from_.(names(all_calls))
    cn <- util_sub_string_left_from_.(names(all_calls))
  } else {
    rn <- character(0)
    cn <- character(0)
    all_calls <- list()
  }

  attr(all_calls, "rn") <- rn
  attr(all_calls, "cn") <- cn


  multivariatecol <- names(all_calls)[vapply(
    lapply(
      all_calls,
      `[[`, "resp_vars"
    ), length,
    FUN.VALUE = integer(1)
  ) != 1] # all calls that passed more ore fewer than one variable in resp_vars

  multivariatecol <- gsub("\\..*$", "", multivariatecol)

  attr(all_calls, "multivariatcol") <- multivariatecol # SQ2 compatibility, everything, that does not address item_level, i.e., that is not called once for each variable; CAVE: Strange name in SQ2 standard w/o "e" # nolint: line_length_linter.

  all_calls
}
