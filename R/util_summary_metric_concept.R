#' Match summary metrics below a DQ_OBS concept
#'
#' @param indicator_metrics Result metric names.
#' @param concept_abbreviation Abbreviation of a DQ_OBS concept.
#'
#' @return A logical vector parallel to `indicator_metrics`.
#' @noRd
util_summary_metrics_in_concept <- function(
  indicator_metrics,
  concept_abbreviation
) {
  util_expect_scalar(concept_abbreviation, check_type = is.character)

  dqi <- util_get_concept_info("dqi")
  dqi <- unique(dqi[, c(
    "IndicatorID", "Parent_Element_ID", "abbreviation"
  ), drop = FALSE])
  dqi <- dqi[!duplicated(dqi[["IndicatorID"]]), , drop = FALSE]
  root_ids <- dqi[["IndicatorID"]][
    dqi[["abbreviation"]] == concept_abbreviation
  ]
  if (!length(root_ids)) {
    return(rep(FALSE, length(indicator_metrics)))
  }

  concept_ids <- root_ids
  repeat {
    children <- dqi[["IndicatorID"]][
      !is.na(dqi[["Parent_Element_ID"]]) &
        dqi[["Parent_Element_ID"]] %in% concept_ids
    ]
    new_ids <- setdiff(children, concept_ids)
    if (!length(new_ids)) {
      break
    }
    concept_ids <- c(concept_ids, new_ids)
  }

  concept_metrics <- dqi[["abbreviation"]][
    dqi[["IndicatorID"]] %in% concept_ids
  ]
  concept_metrics <- concept_metrics[
    !is.na(concept_metrics) & !util_empty(concept_metrics)
  ]
  util_report_scope_normalize_indicator_metrics(indicator_metrics) %in%
    concept_metrics
}

#' Restrict a report summary to one DQ_OBS concept branch
#'
#' @param summary A `dataquieR_summary` object.
#' @param concept_abbreviation Abbreviation of a DQ_OBS concept.
#' @param include Whether to retain or exclude matching metrics.
#'
#' @return A cloned `dataquieR_summary`; the input is not modified.
#' @noRd
util_summary_subset_metric_concept <- function(
  summary,
  concept_abbreviation,
  include = TRUE
) {
  util_expect_scalar(include, check_type = is.logical)
  this <- util_attr(summary, "this", exact = TRUE)
  if (!is.environment(this) || !is.data.frame(this$result) ||
      !"indicator_metric" %in% colnames(this$result)) {
    return(summary)
  }

  matches <- util_summary_metrics_in_concept(
    this$result[["indicator_metric"]],
    concept_abbreviation
  )
  if (!include) {
    matches <- !matches
  }
  result <- summary
  cloned_this <- rlang::env_clone(this)
  cloned_this$result <- this$result[matches, , drop = FALSE]
  attr(result, "this") <- cloned_this
  result
}

#' Render one switchable variable-group sunburst
#'
#' @param other_chart Rendered sunburst for group checks outside contradictions.
#' @param contradiction_chart Rendered contradiction-check sunburst.
#' @param all_chart Rendered sunburst with all group-level result leaves.
#' @param contradiction_count Number of evaluated contradiction checks.
#' @param contradiction_notice Short count shown when both modes are available.
#'
#' @return HTML containing one visible sunburst and, where applicable, a mode
#'   switch.
#' @noRd
util_render_variable_group_sunburst_switch <- function(
  other_chart,
  contradiction_chart,
  all_chart = htmltools::HTML(""),
  contradiction_count = 0L,
  contradiction_notice = NULL
) {
  have_other <- !util_is_empty_html(other_chart)
  have_contradiction <- !util_is_empty_html(contradiction_chart)
  have_all <- !util_is_empty_html(all_chart)
  if (!have_other && !have_contradiction && !have_all) {
    return(htmltools::HTML(""))
  }

  mode_count <- sum(c(have_other, have_contradiction, have_all))
  have_controls <- mode_count > 1L
  if (have_controls && have_contradiction) {
    util_expect_scalar(contradiction_count, check_type = is.numeric)
    util_expect_scalar(contradiction_notice, check_type = is.character)
  }
  other_id <- "dq-variable-group-sunburst-other"
  contradiction_id <- "dq-variable-group-sunburst-contradictions"
  all_id <- "dq-variable-group-sunburst-all"
  default_mode <- if (have_other) {
    "other"
  } else if (have_contradiction) {
    "contradiction"
  } else {
    "all"
  }

  render_panel <- function(id, mode, title, description, chart, visible) {
    htmltools::tags$section(
      id = id,
      class = "dq-sunburst-mode-panel",
      role = if (have_controls) "tabpanel",
      `data-dq-sunburst-mode-panel` = mode,
      `aria-label` = title,
      `aria-hidden` = if (visible) "false" else "true",
      hidden = if (!visible) "hidden",
      htmltools::h2(title),
      htmltools::p(description),
      chart
    )
  }

  render_button <- function(mode, label, panel_id) {
    selected <- identical(mode, default_mode)
    htmltools::tags$button(
      type = "button",
      class = paste(
        c("dq-sunburst-mode-button", if (selected) "is-active"),
        collapse = " "
      ),
      role = "tab",
      `data-dq-sunburst-mode-button` = mode,
      `aria-controls` = panel_id,
      `aria-selected` = if (selected) "true" else "false",
      tabindex = if (selected) "0" else "-1",
      label
    )
  }

  htmltools::div(
    id = "dq-variable-group-sunburst-mode-switch",
    class = "dq-sunburst-mode-switch",
    `data-dq-default-mode` = default_mode,
    if (have_controls) {
      htmltools::tagList(
        if (have_contradiction) {
          htmltools::p(
            class = "dq-sunburst-mode-notice",
            htmltools::strong(paste0(contradiction_notice, " available.")),
            " Select which group-result category the chart displays."
          )
        },
        htmltools::div(
          class = "dq-sunburst-mode-tabs",
          role = "tablist",
          `aria-label` = "Variable-group chart category",
          if (have_other) {
            render_button("other", "Other group checks", other_id)
          },
          if (have_contradiction) {
            render_button(
              "contradiction",
              sprintf("Contradiction checks (%d)", contradiction_count),
              contradiction_id
            )
          },
          if (have_all) {
            render_button("all", "All group-level results", all_id)
          }
        ),
        htmltools::p(
          class = "dq-sunburst-mode-status dq-visually-hidden",
          `aria-live` = "polite",
          `aria-atomic` = "true",
          paste("Showing", switch(default_mode,
              other = "Other group checks.",
              contradiction = "Contradiction checks.",
              all = "All group-level results."
            ))
        )
      )
    },
    if (have_other) {
      render_panel(
        other_id,
        "other",
        "Other group checks",
        paste(
          "Sector area, label size, and color emphasize more critical results.",
          "Contradiction checks are excluded from this view."
        ),
        other_chart,
        visible = identical(default_mode, "other")
      )
    },
    if (have_contradiction) {
      render_panel(
        contradiction_id,
        "contradiction",
        "Contradiction checks",
        paste(
          "Sector area, label size, and color emphasize more critical checks.",
          "The count in the selector and notice is the number of evaluated",
          "checks; the chart does not show the share of affected observations."
        ),
        contradiction_chart,
        visible = identical(default_mode, "contradiction")
      )
    },
    if (have_all) {
      render_panel(
        all_id,
        "all",
        "All group-level results",
        paste(
          "Sector area, label size, and color emphasize more critical results.",
          "The number and classifications of contradiction checks can dominate",
          "this combined view; their count is shown separately and the chart",
          "does not show the share of affected observations."
        ),
        all_chart,
        visible = identical(default_mode, "all")
      )
    }
  )
}

#' Count evaluated checks represented in a grouped summary
#'
#' @param results Long-format summary rows for one concept branch.
#'
#' @return Number of unique evaluated check keys.
#' @noRd
util_summary_evaluated_check_count <- function(results) {
  results <- util_summary_most_specific_group_metrics(results)
  if (!is.data.frame(results) || !nrow(results) ||
      !VAR_NAMES %in% colnames(results)) {
    return(0L)
  }
  has_value <- if ("value" %in% colnames(results)) {
    !util_empty(results[["value"]])
  } else {
    rep(FALSE, nrow(results))
  }
  check_keys <- unique(as.character(results[[VAR_NAMES]][has_value]))
  as.integer(sum(!util_empty(check_keys)))
}

#' Describe evaluated summary checks and their variable groups
#'
#' @param results Long-format summary rows for one concept branch.
#' @param meta_data_cross_item Normalized cross-item metadata.
#' @param check_label Singular label for one evaluated check.
#'
#' @return A scalar chart title.
#' @noRd
util_summary_check_count_title <- function(
  results,
  meta_data_cross_item,
  check_label
) {
  util_expect_scalar(check_label, check_type = is.character)
  results <- util_summary_most_specific_group_metrics(results)
  if (!is.data.frame(results) || !nrow(results) ||
      !VAR_NAMES %in% colnames(results)) {
    return(sprintf("0 %s checks across 0 variable groups", check_label))
  }
  has_value <- if ("value" %in% colnames(results)) {
    !util_empty(results[["value"]])
  } else {
    rep(FALSE, nrow(results))
  }
  evaluated <- results[has_value, , drop = FALSE]
  check_keys <- unique(as.character(evaluated[[VAR_NAMES]]))
  check_keys <- check_keys[!util_empty(check_keys)]

  group_keys <- check_keys
  variable_list_column <- if (
    is.data.frame(meta_data_cross_item) &&
      VARIABLE_LIST_ORDER %in% colnames(meta_data_cross_item)
  ) {
    VARIABLE_LIST_ORDER
  } else {
    VARIABLE_LIST
  }
  if (is.data.frame(meta_data_cross_item) &&
      variable_list_column %in% colnames(meta_data_cross_item)) {
    group_label_column <- ".variable_group_result_label"
    metadata_rows <- if (
      CHECK_ID %in% colnames(meta_data_cross_item) &&
        CHECK_ID %in% colnames(evaluated)
    ) {
      match(
        unique(as.character(evaluated[[CHECK_ID]])),
        as.character(meta_data_cross_item[[CHECK_ID]])
      )
    } else if (
      CHECK_LABEL %in% colnames(meta_data_cross_item) &&
        group_label_column %in% colnames(evaluated)
    ) {
      match(
        unique(as.character(evaluated[[group_label_column]])),
        as.character(meta_data_cross_item[[CHECK_LABEL]])
      )
    } else {
      integer()
    }
    variable_lists <- meta_data_cross_item[
      metadata_rows, variable_list_column, drop = TRUE
    ]
    variable_lists <- trimws(as.character(variable_lists))
    variable_lists <- variable_lists[!util_empty(variable_lists)]
    group_keys <- vapply(variable_lists, function(variable_list) {
      variables <- names(util_parse_assignments(variable_list))
      if (length(variables)) {
        paste(sort(variables), collapse = SPLIT_CHAR)
      } else {
        variable_list
      }
    }, FUN.VALUE = character(1))
    if (!length(group_keys)) {
      group_keys <- check_keys
    }
  }

  check_count <- length(check_keys)
  group_count <- length(unique(group_keys))
  check_noun <- paste(
    check_label,
    if (check_count == 1L) "check" else "checks"
  )
  group_noun <- if (group_count == 1L) {
    "variable group"
  } else {
    "variable groups"
  }
  sprintf("%d %s across %d %s",
    check_count, check_noun, group_count, group_noun
  )
}
