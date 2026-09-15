#' Identify missing display values in summaries
#'
#' @noRd
util_summary_display_value_missing <- function(x) {
  normalized <- trimws(as.character(x))
  util_empty(x) | toupper(normalized) %in% c("NA", "NAN")
}

#' Build the JavaScript handler for a summary-result popup
#'
#' @param url URL loaded in the result popup.
#' @param link_url URL used when the popup opens the result in the report.
#' @param title Popup title.
#' @param escape Whether to escape the JavaScript for a raw HTML attribute.
#'
#' @noRd
util_summary_popup_handler <- function(url, link_url, title, escape = TRUE) {
  handler <- htmltools::htmlTemplate(
    text_ = "(function(e) {
        e.preventDefault();
        showDataquieRResult({{url}}, {{link_url}}, {{title}});
      })(event);",
    url = jsonlite::toJSON(url, auto_unbox = TRUE),
    link_url = jsonlite::toJSON(link_url, auto_unbox = TRUE),
    title = jsonlite::toJSON(title, auto_unbox = TRUE)
  )
  if (!escape) {
    return(as.character(handler))
  }
  htmltools::htmlEscape(handler, attribute = TRUE)
}

#' Check whether a summary result has a usable navigation target
#'
#' @noRd
util_summary_link_available <- function(href, popup_href) {
  usable_target <- function(target) {
    target <- trimws(as.character(target))
    !util_empty(target) &
      !grepl("(^|/)NA(?:$|[?#])", target, perl = TRUE)
  }
  usable_target(href) & usable_target(popup_href)
}

#' Check whether a summary row has displayable output
#'
#' @noRd
util_summary_display_row_available <- function(value, indicator_metric,
  messages, classification) {
  !util_summary_display_value_missing(value) |
    as.character(indicator_metric) == "EMPTY_OUTPUT" |
    !util_empty(messages) |
    !util_empty(classification)
}

#' Map summary metrics to data-quality dimensions
#'
#' @noRd
util_summary_metric_dimensions <- function(indicator_metrics) {
  metrics <- util_report_scope_normalize_indicator_metrics(indicator_metrics)
  dqi <- util_get_concept_info("dqi")
  dqi <- dqi[
    dqi[["Level"]] == 3L &
      !is.na(dqi[["abbreviation"]]) &
      !util_empty(dqi[["abbreviation"]]),
    c("abbreviation", "Dimension"),
    drop = FALSE
  ]
  abbreviations <- as.character(dqi[["abbreviation"]])
  dimensions <- vapply(metrics, function(metric) {
    matches <- which(
      metric == abbreviations |
        startsWith(metric, paste0(abbreviations, "_"))
    )
    if (!length(matches)) {
      return(NA_character_)
    }
    dqi[["Dimension"]][matches[[which.max(nchar(abbreviations[matches]))]]]
  }, character(1), USE.NAMES = FALSE)
  unresolved <- is.na(dimensions)
  legacy_prefix_dimensions <- c(
    int_ = "Integrity",
    com_ = "Completeness",
    con_ = "Consistency",
    acc_ = "Accuracy"
  )
  for (prefix in names(legacy_prefix_dimensions)) {
    use_prefix <- unresolved & startsWith(
      as.character(indicator_metrics),
      prefix
    )
    dimensions[use_prefix] <- legacy_prefix_dimensions[[prefix]]
  }
  setNames(dimensions, as.character(indicator_metrics))
}

#' Replace the visible text of summary links
#'
#' @noRd
util_summary_replace_link_text <- function(link, text) {
  escaped_text <- htmltools::htmlEscape(as.character(text))
  has_anchor <- grepl("<a[^>]*>", link)
  link[has_anchor] <- mapply(
    link[has_anchor],
    escaped_text[has_anchor],
    FUN = function(current_link, current_text) {
      sub(
        "(<a[^>]*>).*?(</a>)",
        paste0("\\1", current_text, "\\2"),
        current_link,
        perl = TRUE
      )
    },
    USE.NAMES = FALSE
  )
  link[!has_anchor] <- escaped_text[!has_anchor]
  link
}

#' Describe the display state of grouped results
#'
#' @noRd
util_summary_group_result_state <- function(code, class_num, has_result) {
  numeric_class <- suppressWarnings(as.numeric(as.character(class_num)))
  split_rows <- split(seq_along(code), code)
  state <- lapply(split_rows, function(rows) {
    result_rows <- rows[has_result[rows]]
    if (!length(result_rows)) {
      return(c(
        result_count = 0L,
        worst_count = 0L,
        has_classification = 0L
      ))
    }
    available_classes <- numeric_class[result_rows]
    available_classes <- available_classes[!is.na(available_classes)]
    if (!length(available_classes)) {
      return(c(
        result_count = length(result_rows),
        worst_count = length(result_rows),
        has_classification = 0L
      ))
    }
    c(
      result_count = length(result_rows),
      worst_count = sum(numeric_class[result_rows] == max(available_classes),
        na.rm = TRUE
      ),
      has_classification = 1L
    )
  })
  state <- do.call(rbind, state)
  state[match(code, rownames(state)), , drop = FALSE]
}

#' Select primary rows from repeated group results
#'
#' @noRd
util_summary_primary_result_rows <- function(code, has_result) {
  vapply(unique(code), function(current_code) {
    rows <- which(code == current_code)
    result_rows <- rows[has_result[rows]]
    if (length(result_rows)) result_rows[[1]] else rows[[1]]
  }, integer(1), USE.NAMES = FALSE)
}

#' Select the most specific metrics for grouped results
#'
#' @noRd
util_summary_most_specific_group_metrics <- function(result) {
  if (!is.data.frame(result) || !nrow(result) ||
      !"indicator_metric" %in% colnames(result)) {
    return(result)
  }

  metrics <- as.character(result[["indicator_metric"]])
  abbreviations <- util_report_scope_normalize_indicator_metrics(metrics)
  dqi <- util_get_concept_info("dqi")
  dqi <- dqi[
    !is.na(dqi[["IndicatorID"]]) &
      !util_empty(dqi[["IndicatorID"]]),
    c("IndicatorID", "Parent_Element_ID", "abbreviation"),
    drop = FALSE
  ]
  dqi <- dqi[!duplicated(dqi[["IndicatorID"]]), , drop = FALSE]
  metric_ids <- dqi[["IndicatorID"]][match(
    abbreviations,
    dqi[["abbreviation"]]
  )]
  if (all(is.na(metric_ids))) {
    return(result)
  }

  parent_ids <- setNames(
    as.character(dqi[["Parent_Element_ID"]]),
    as.character(dqi[["IndicatorID"]])
  )
  is_ancestor <- function(ancestor, descendant) {
    current <- descendant
    visited <- character()
    while (!is.na(current) && nzchar(current) &&
        !current %in% visited) {
      visited <- c(visited, current)
      current <- unname(parent_ids[current])
      if (identical(current, ancestor)) {
        return(TRUE)
      }
    }
    FALSE
  }

  has_result <- rep(FALSE, nrow(result))
  for (column in intersect(
    c("values_raw", "value"),
    colnames(result)
  )) {
    has_result <- has_result | !util_empty(result[[column]])
  }
  metric_kind <- ifelse(
    grepl("_", metrics, fixed = TRUE),
    sub("_.*$", "", metrics),
    ""
  )
  group_columns <- intersect(
    c(
      VAR_NAMES,
      STUDY_SEGMENT,
      "call_names",
      ".variable_group_result_label"
    ),
    colnames(result)
  )
  group_values <- c(
    lapply(result[group_columns], function(x) addNA(factor(x))),
    list(addNA(factor(metric_kind)))
  )
  group_code <- do.call(
    interaction,
    c(group_values, list(drop = TRUE, lex.order = TRUE))
  )

  remove_metric <- rep(FALSE, nrow(result))
  for (rows in split(seq_len(nrow(result)), group_code)) {
    available_ids <- unique(metric_ids[
      rows[has_result[rows] & !is.na(metric_ids[rows])]
    ])
    if (!length(available_ids)) {
      next
    }
    metric_rows <- rows[!is.na(metric_ids[rows])]
    ancestor_rows <- metric_rows[vapply(
      metric_rows,
      function(row) {
        any(vapply(
          setdiff(available_ids, metric_ids[[row]]),
          function(available_id) {
            is_ancestor(metric_ids[[row]], available_id)
          },
          logical(1)
        ))
      },
      logical(1)
    )]
    if (length(ancestor_rows)) {
      remove_metric[rows] <- abbreviations[rows] %in%
        abbreviations[ancestor_rows]
    }
  }

  result[!remove_metric, , drop = FALSE]
}
