#' Normalize labels for cross-item checks
#'
#' Explicit `CHECK_LABEL` values remain the primary user-facing group names.
#' Missing labels use scale metadata when available and otherwise combine the
#' stable `CHECK_ID` with short member labels.
#'
#' @param meta_data_cross_item Normalized cross-item metadata.
#' @param variable_list Parsed variable lists using item labels.
#' @param meta_data Normalized item-level metadata.
#'
#' @return `meta_data_cross_item` with non-empty, unique `CHECK_LABEL` values.
#' @noRd
util_normalize_cross_item_labels <- function(
  meta_data_cross_item,
  variable_list,
  meta_data
) {
  if (!CHECK_LABEL %in% colnames(meta_data_cross_item)) {
    meta_data_cross_item[[CHECK_LABEL]] <- NA_character_
  }
  check_labels <- trimws(as.character(meta_data_cross_item[[CHECK_LABEL]]))

  for (column in c(SCALE_NAME, SCALE_ACRONYM)) {
    if (column %in% colnames(meta_data_cross_item)) {
      missing_labels <- vapply(
        check_labels,
        util_result_caption_empty,
        FUN.VALUE = logical(1)
      )
      candidate_labels <- trimws(as.character(meta_data_cross_item[[column]]))
      use_candidate <- missing_labels & !vapply(
        candidate_labels,
        util_result_caption_empty,
        FUN.VALUE = logical(1)
      )
      check_labels[use_candidate] <- candidate_labels[use_candidate]
    }
  }

  missing_labels <- vapply(
    check_labels,
    util_result_caption_empty,
    FUN.VALUE = logical(1)
  )
  short_label_target <- if (LABEL %in% colnames(meta_data)) {
    LABEL
  } else {
    VAR_NAMES
  }
  short_variable_list <- lapply(variable_list, function(members) {
    util_find_var_by_meta(
      resp_vars = members,
      meta_data = meta_data,
      target = short_label_target,
      ifnotfound = members
    )
  })
  fallback_labels <- mapply(
    meta_data_cross_item[[CHECK_ID]],
    short_variable_list,
    SIMPLIFY = TRUE,
    USE.NAMES = FALSE,
    FUN = function(check_id, members) {
      members <- members[!util_empty(members)]
      if (!length(members)) {
        return(as.character(check_id))
      }
      sprintf(
        "%s: %s",
        check_id,
        util_pretty_vector_string(members, quote = identity, n_max = 3)
      )
    }
  )
  check_labels[missing_labels] <- fallback_labels[missing_labels]

  if (any(duplicated(check_labels))) {
    util_message(
      c(
        "Check labels cannot have duplicates in",
        "cross-item_level metadata. I'll fix that"
      ),
      applicability_problem = TRUE
    )
    while (any(duplicated(check_labels))) {
      duplicate_labels <- duplicated(check_labels)
      check_labels[duplicate_labels] <- paste0(
        "Check #",
        seq_along(check_labels)[duplicate_labels]
      )
    }
  }

  meta_data_cross_item[[CHECK_LABEL]] <- check_labels
  meta_data_cross_item
}
