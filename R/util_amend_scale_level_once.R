#' Amend missing scale-level metadata once
#'
#' Fill missing `SCALE_LEVEL` entries using the same preparation path that
#' `dq_report2()` historically used. If scale levels were already predicted by
#' an outer caller, the marker is preserved so `dq_report2()` can still emit the
#' report hint without repeating the expensive prediction.
#'
#' @param study_data Study data visible for the current report or segment.
#' @param meta_data Item-level metadata for the same variables.
#' @param label_col Metadata label column.
#' @param verbose If `TRUE`, print the existing progress messages.
#'
#' @return A list with amended `meta_data` and logical `predicted`.
#'
#' @noRd
util_amend_scale_level_once <- function(study_data,
  meta_data,
  label_col,
  verbose = TRUE) {
  scale_level_predicted <- isTRUE(util_attr(
    meta_data,
    "dataquieR_scale_level_predicted",
    exact = TRUE
  ))

  needs_scale_level <-
    !(SCALE_LEVEL %in% colnames(meta_data)) ||
    any(util_empty(meta_data[[SCALE_LEVEL]][
      meta_data[[VAR_NAMES]] %in% colnames(study_data)
    ]))

  if (!needs_scale_level) {
    return(list(
      meta_data = meta_data,
      predicted = scale_level_predicted
    ))
  }

  if (verbose) {
    util_message("Estimating %s...", sQuote(SCALE_LEVEL))
  }

  suppressWarnings(suppressMessages({
    prep_prepare_dataframes(
      .study_data = study_data,
      .meta_data = meta_data,
      .label_col = label_col,
      .replace_hard_limits = FALSE,
      .replace_missings = FALSE,
      .adjust_data_type = TRUE,
      .amend_scale_level = TRUE,
      .internal = TRUE
    )
  }))

  attr(meta_data, "dataquieR_scale_level_predicted") <- TRUE
  scale_level_predicted <- TRUE

  if (verbose) {
    util_message("Estimating %s... done", sQuote(SCALE_LEVEL))
  }

  list(
    meta_data = meta_data,
    predicted = scale_level_predicted
  )
}
