#' Internal helper: questionnaire call order
#'
#' @noRd
util_questionnaire_call_order <- function(all_calls,
  meta_data,
  meta_data_cross_item) {
  context <- util_questionnaire_call_context(
    all_calls = all_calls,
    meta_data = meta_data,
    meta_data_cross_item = meta_data_cross_item
  )
  order(
    context[["group_order"]],
    context[["metric_order"]],
    context[["original_order"]]
  )
}

#' Internal helper: questionnaire call context
#'
#' @noRd
util_questionnaire_call_context <- function(all_calls,
  meta_data,
  meta_data_cross_item) {
  original_order <- seq_along(all_calls)
  context <- data.frame(
    result_name = names(all_calls),
    check_id = rep(NA_character_, length(all_calls)),
    metric_role = rep(NA_character_, length(all_calls)),
    group_order = rep(.Machine$integer.max, length(all_calls)),
    metric_order = rep(.Machine$integer.max, length(all_calls)),
    original_order = original_order,
    stringsAsFactors = FALSE
  )
  required_item_columns <- c(VAR_NAMES, CHECK_ID, COMPUTED_VARIABLE_ROLE)
  if (!length(all_calls) ||
      !is.data.frame(meta_data) ||
      !all(required_item_columns %in% colnames(meta_data)) ||
      !is.data.frame(meta_data_cross_item) ||
      !(CHECK_ID %in% colnames(meta_data_cross_item))) {
    return(context)
  }

  variable_names <- vapply(all_calls, function(call) {
    variable_name <- util_attr(call, VAR_NAMES, exact = TRUE)
    if (length(variable_name) == 1 &&
        !is.na(variable_name) &&
        !util_empty(variable_name)) {
      as.character(variable_name)
    } else {
      NA_character_
    }
  }, FUN.VALUE = character(1))
  check_ids <- util_map_labels(
    variable_names,
    meta_data = meta_data,
    from = VAR_NAMES,
    to = CHECK_ID,
    ifnotfound = NA_character_,
    warn_ambiguous = FALSE
  )
  metric_roles <- util_map_labels(
    variable_names,
    meta_data = meta_data,
    from = VAR_NAMES,
    to = COMPUTED_VARIABLE_ROLE,
    ifnotfound = NA_character_,
    warn_ambiguous = FALSE
  )

  group_ids <- unique(meta_data_cross_item[[CHECK_ID]])
  group_ids <- group_ids[!util_empty(group_ids)]
  ssi_info <- util_get_concept_info("ssi")
  metric_ids <- if ("SSI_METRICS" %in% colnames(ssi_info)) {
    ssi_info[["SSI_METRICS"]]
  } else {
    character(0)
  }
  group_order <- match(check_ids, group_ids)
  metric_order <- match(metric_roles, metric_ids)
  group_order[is.na(group_order)] <- length(group_ids) + 1L
  metric_order[is.na(metric_order)] <- length(metric_ids) + 1L

  context[["check_id"]] <- check_ids
  context[["metric_role"]] <- metric_roles
  context[["group_order"]] <- group_order
  context[["metric_order"]] <- metric_order
  context
}

#' Internal helper: combine questionnaire results
#'
#' @noRd
util_combine_questionnaire_results <- function(results,
  all_calls,
  meta_data,
  meta_data_cross_item) {
  if (length(results) < 2) {
    return(results)
  }

  context <- util_questionnaire_call_context(
    all_calls = all_calls,
    meta_data = meta_data,
    meta_data_cross_item = meta_data_cross_item
  )
  context <- context[match(names(results), context[["result_name"]]),
    , drop = FALSE
  ]
  function_names <- vapply(results, function(result) {
    function_name <- util_attr(result, "function_name", exact = TRUE)
    if (length(function_name) == 1 && !is.na(function_name)) {
      function_name
    } else {
      NA_character_
    }
  }, FUN.VALUE = character(1))
  has_error <- vapply(results, function(result) {
    length(util_attr(result, "error", exact = TRUE)) > 0
  }, FUN.VALUE = logical(1))
  same_run <- rep(FALSE, length(results) - 1L)
  if (length(same_run)) {
    previous <- seq_len(length(results) - 1L)
    current <- previous + 1L
    same_run <- !has_error[previous] & !has_error[current] &
      !is.na(context[["check_id"]][previous]) &
      !is.na(context[["check_id"]][current]) &
      !is.na(function_names[previous]) &
      !is.na(function_names[current]) &
      context[["check_id"]][previous] == context[["check_id"]][current] &
      function_names[previous] == function_names[current]
  }
  run_ids <- cumsum(c(TRUE, !same_run))

  result_blocks <- lapply(unique(run_ids), function(run_id) {
    indices <- which(run_ids == run_id)
    run_results <- results[indices]
    if (length(run_results) < 2) {
      return(run_results)
    }
    combined <- util_master_result_from_result_list(
      run_results,
      title_mode = "ssi"
    )
    if (is.null(combined) ||
        length(util_attr(combined, "dq_result_list", exact = TRUE))) {
      return(run_results)
    }

    summaries <- lapply(
      run_results,
      util_attr,
      which = "r_summary",
      exact = TRUE
    )
    summaries <- Filter(is.data.frame, summaries)
    if (length(summaries)) {
      attr(combined, "r_summary") <- util_rbind(
        data_frames_list = summaries
      )
    }
    grading_meta_data <- lapply(
      run_results,
      util_questionnaire_result_grading_meta_data
    )
    grading_meta_data <- Filter(is.data.frame, grading_meta_data)
    if (length(grading_meta_data)) {
      attr(combined, "dq_questionnaire_grading_meta_data") <- unique(
        util_rbind(data_frames_list = grading_meta_data)
      )
    }

    check_id <- context[["check_id"]][indices[[1]]]
    group_title <- util_questionnaire_group_title(
      check_id,
      meta_data_cross_item
    )
    if (is.na(group_title) || util_empty(group_title)) {
      group_title <- check_id
    }
    metric_titles <- vapply(
      context[["metric_role"]][indices],
      util_questionnaire_metric_title,
      FUN.VALUE = character(1)
    )
    metric_titles <- unique(metric_titles[!is.na(metric_titles) &
          !util_empty(metric_titles)])
    combined_title <- if (length(metric_titles)) {
      paste0(group_title, ": ", paste(metric_titles, collapse = ", "))
    } else {
      group_title
    }
    attr(combined, "dq_result_title") <- combined_title
    combined_name <- paste(function_names[indices[[1]]], group_title, sep = ".")
    setNames(list(combined), combined_name)
  })
  combined_results <- do.call(c, result_blocks)
  names(combined_results) <- make.unique(names(combined_results))
  attr(combined_results, "dq_questionnaire_grouped") <- TRUE
  combined_results
}
