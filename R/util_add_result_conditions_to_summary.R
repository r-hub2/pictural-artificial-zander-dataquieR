#' Add result conditions to summary rows
#'
#' Conditions belong to a result object. A result can contribute multiple
#' item- or variable-group summary rows, so attach the same conditions to every
#' extracted row identifier rather than only to the variables of its call.
#'
#' @param result A `dataquieR_result`.
#' @param summary Summary rows extracted from `result`.
#' @param function_name Name of the producing function.
#'
#' @return `summary` enriched with condition rows.
#' @noRd
util_add_result_conditions_to_summary <- function(result, summary,
  function_name) {
  aspects <- c("applicability", "error", "anamat", "indicator_or_descriptor")
  categories <- vapply(aspects, function(aspect) {
    as.character(as.numeric(util_as_cat(
      util_get_category_for_result(result, aspect = aspect)
    )))
  }, FUN.VALUE = character(1))
  messages <- vapply(aspects, function(aspect) {
    util_get_message_for_result(result, aspect = aspect)
  }, FUN.VALUE = character(1))
  names(categories) <- paste0("CAT_", aspects)
  names(messages) <- paste0("MSG_", aspects)
  conditions <- c(categories, messages)

  var_names <- character(0)
  if (is.data.frame(summary) && VAR_NAMES %in% names(summary)) {
    var_names <- unique(as.character(summary[[VAR_NAMES]]))
    var_names <- var_names[!util_empty(var_names)]
  }
  my_call <- util_attr(result, "call", exact = TRUE)
  if (!length(var_names)) {
    var_names <- unname(util_attr(my_call, VAR_NAMES, exact = TRUE))
  }
  if (!length(var_names)) {
    return(summary)
  }

  study_segments <- rep(NA_character_, length(var_names))
  call_study_segments <- unname(util_attr(my_call, STUDY_SEGMENT, exact = TRUE))
  if (length(call_study_segments)) {
    study_segments <- rep(call_study_segments, length.out = length(var_names))
  }
  if (is.data.frame(summary) &&
      all(c(VAR_NAMES, STUDY_SEGMENT) %in% names(summary))) {
    summary_segments <- summary[[STUDY_SEGMENT]][
      match(var_names, summary[[VAR_NAMES]])
    ]
    study_segments[!util_empty(summary_segments)] <-
      summary_segments[!util_empty(summary_segments)]
  }

  call_name <- unname(util_attr(result, "cn", exact = TRUE))
  if (!length(call_name)) {
    call_name <- NA_character_
  }
  condition_summary <- data.frame(
    VAR_NAMES = rep(var_names, each = length(conditions)),
    STUDY_SEGMENT = rep(study_segments, each = length(conditions)),
    call_names = rep(call_name, length(var_names) * length(conditions)),
    value = rep(unname(conditions), length(var_names)),
    values_raw = rep(unname(conditions), length(var_names)),
    function_name = function_name,
    indicator_metric = rep(names(conditions), length(var_names)),
    stringsAsFactors = FALSE
  )
  if (is.data.frame(summary) && CHECK_ID %in% names(summary)) {
    check_ids <- summary[[CHECK_ID]][match(var_names, summary[[VAR_NAMES]])]
    condition_summary[[CHECK_ID]] <- rep(
      as.character(check_ids),
      each = length(conditions)
    )
  }
  summary <- util_rbind(condition_summary, summary)
  summary$function_name <- function_name
  summary
}
