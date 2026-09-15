#' Prepare display-only labels on a local report copy
#'
#' @param report a `dataquieR_resultset2` report
#' @param reference_meta_data optional full item-level metadata used to make
#'   display-label collision decisions consistently across subreports
#'
#' @return `report` with display labels adjusted in its item-level metadata
#'
#' @noRd
util_prepare_report_labels_for_view <- function(report,
  reference_meta_data = NULL) {
  if (isTRUE(util_attr(
    report,
    "dataquieR_view_labels_prepared",
    exact = TRUE
  ))) {
    return(report)
  }
  if (!isTRUE(getOption(
    "dataquieR.fix_var_name_prefixes_label",
    dataquieR.fix_var_name_prefixes_label_default
  ))) {
    return(report)
  }

  meta_data <- util_attr(report, "meta_data", exact = TRUE)
  if (!is.data.frame(meta_data) ||
      !VAR_NAMES %in% colnames(meta_data)) {
    return(report)
  }

  label_col <- util_attr(report, "label_col", exact = TRUE)
  if (is.data.frame(reference_meta_data) &&
      VAR_NAMES %in% colnames(reference_meta_data)) {
    display_meta_data <- reference_meta_data
  } else {
    display_meta_data <- meta_data
  }
  display_meta_data <- util_prepare_meta_data_labels_for_view(
    meta_data = display_meta_data,
    label_col = label_col
  )

  display_rows <- match(
    meta_data[[VAR_NAMES]],
    display_meta_data[[VAR_NAMES]]
  )
  matched_rows <- !is.na(display_rows)
  label_columns <- intersect(
    util_report_view_label_columns(meta_data, label_col),
    colnames(display_meta_data)
  )
  for (current_label_col in label_columns) {
    meta_data[[current_label_col]][matched_rows] <-
      display_meta_data[[current_label_col]][display_rows[matched_rows]]
  }

  attr(report, "meta_data") <- meta_data
  attr(report, "dataquieR_view_labels_prepared") <- TRUE
  report
}

#' Prepare metadata labels for report rendering
#'
#' @noRd
util_prepare_meta_data_labels_for_view <- function(meta_data, label_col) {
  label_columns <- util_report_view_label_columns(meta_data, label_col)
  for (current_label_col in label_columns) {
    if (is.character(meta_data[[current_label_col]])) {
      meta_data[[current_label_col]] <- util_fix_var_name_prefixes_label(
        labels = meta_data[[current_label_col]],
        var_names = meta_data[[VAR_NAMES]]
      )
    }
  }
  meta_data
}

#' Identify metadata label columns used for report rendering
#'
#' @noRd
util_report_view_label_columns <- function(meta_data, label_col) {
  label_columns <- colnames(meta_data)[
    colnames(meta_data) %in% c(LABEL, LONG_LABEL, label_col) |
      startsWith(colnames(meta_data), paste0(LABEL, "_")) |
      startsWith(colnames(meta_data), paste0(LONG_LABEL, "_"))
  ]
  setdiff(label_columns, VAR_NAMES)
}

#' Remove repeated variable-name prefixes from display labels
#'
#' @noRd
util_fix_var_name_prefixes_label <- function(labels, var_names) {
  if (!isTRUE(getOption(
    "dataquieR.fix_var_name_prefixes_label",
    dataquieR.fix_var_name_prefixes_label_default
  ))) {
    return(labels)
  }

  util_stop_if_not(length(labels) == length(var_names))
  original_labels <- labels

  fix_one <- function(label, var_name) {
    if (util_empty(label) || util_empty(var_name)) {
      return(label)
    }

    fixed_label <- trimws(label)
    var_name <- trimws(var_name)
    escaped_var_name <- gsub(
      "([][{}()+*^$|\\\\?.])",
      "\\\\\\1",
      var_name,
      perl = TRUE
    )
    prefix_pattern <- paste0(
      "^",
      escaped_var_name,
      "(?:\\s*[:;,_/|\\-]+\\s*|\\s+)"
    )

    repeat {
      shortened <- trimws(sub(prefix_pattern, "", fixed_label, perl = TRUE))
      if (identical(shortened, fixed_label) || util_empty(shortened)) {
        break
      }
      fixed_label <- shortened
    }

    fixed_label
  }

  labels[] <- mapply(
    fix_one,
    label = labels,
    var_name = var_names,
    USE.NAMES = FALSE
  )
  duplicate_shortened <- duplicated(labels) | duplicated(labels,
    fromLast = TRUE)
  labels[duplicate_shortened] <- original_labels[duplicate_shortened]
  labels
}
