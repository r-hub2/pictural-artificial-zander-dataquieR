#' Internal helper: questionnaire add section anchors
#'
#' @noRd
util_questionnaire_add_section_anchors <- function(results) {
  if (!length(results)) {
    return(results)
  }
  anchors <- sprintf("questionnaire-section-%02d", seq_along(results))
  mapply(
    SIMPLIFY = FALSE,
    USE.NAMES = TRUE,
    result = results,
    anchor = anchors,
    FUN = function(result, anchor) {
      attr(result, "dq_result_anchor") <- anchor
      result
    }
  )
}

#' Internal helper: questionnaire result grading meta data
#'
#' @noRd
util_questionnaire_result_grading_meta_data <- function(result) {
  stored <- util_attr(
    result,
    "dq_questionnaire_grading_meta_data",
    exact = TRUE
  )
  if (is.data.frame(stored) &&
      all(c(VAR_NAMES, GRADING_RULESET) %in% colnames(stored))) {
    return(stored[, c(VAR_NAMES, GRADING_RULESET), drop = FALSE])
  }

  call <- util_attr(result, "call", exact = TRUE)
  variable_names <- util_attr(call, VAR_NAMES, exact = TRUE)
  rule_sets <- util_attr(call, GRADING_RULESET, exact = TRUE)
  if (!length(variable_names)) {
    summary_data <- util_attr(result, "r_summary", exact = TRUE)
    if (is.data.frame(summary_data) && VAR_NAMES %in% colnames(summary_data)) {
      variable_names <- unique(summary_data[[VAR_NAMES]])
    }
  }
  variable_names <- as.character(variable_names)
  variable_names <- variable_names[!is.na(variable_names) &
      !util_empty(variable_names)]
  if (!length(variable_names)) {
    return(data.frame())
  }

  if (!length(rule_sets)) {
    rule_sets <- rep("0", length(variable_names))
  } else if (!is.null(names(rule_sets)) && !is.null(names(variable_names))) {
    rule_sets <- rule_sets[match(names(variable_names), names(rule_sets))]
  } else {
    rule_sets <- rep(
      as.character(rule_sets),
      length.out = length(variable_names)
    )
  }
  rule_sets <- as.character(rule_sets)
  rule_sets[is.na(rule_sets) | util_empty(rule_sets)] <- "0"

  data.frame(
    VAR_NAMES = unname(variable_names),
    GRADING_RULESET = unname(rule_sets),
    stringsAsFactors = FALSE
  )
}

#' Internal helper: questionnaire html sections
#'
#' @noRd
util_questionnaire_html_sections <- function(x) {
  results <- util_attr(x, "dq_result_list", exact = TRUE)
  if (!length(results)) {
    result_name <- util_attr(x, "cn", exact = TRUE)
    if (!length(result_name) || is.na(result_name) || util_empty(result_name)) {
      result_name <- "dq_questionnaire"
    }
    results <- setNames(list(x), result_name)
  }
  results <- util_questionnaire_add_missing_section_anchors(results)

  titles <- util_attr(x, "dq_result_titles", exact = TRUE)
  if (length(titles) != length(results)) {
    titles <- util_result_list_display_titles(results, title_mode = "ssi")
  }
  titles <- as.character(titles)
  missing_titles <- is.na(titles) | util_empty(titles)
  titles[missing_titles] <- names(results)[missing_titles]

  list(
    results = results,
    titles = unname(titles),
    anchors = vapply(
      results,
      util_attr,
      which = "dq_result_anchor",
      exact = TRUE,
      FUN.VALUE = character(1)
    )
  )
}

#' Internal helper: questionnaire add missing section anchors
#'
#' @noRd
util_questionnaire_add_missing_section_anchors <- function(results) {
  existing <- vapply(results, function(result) {
    anchor <- util_attr(result, "dq_result_anchor", exact = TRUE)
    length(anchor) == 1 && is.character(anchor) && !is.na(anchor) &&
      !util_empty(anchor)
  }, FUN.VALUE = logical(1))
  anchors <- vapply(results, function(result) {
    anchor <- util_attr(result, "dq_result_anchor", exact = TRUE)
    if (length(anchor) == 1 && is.character(anchor) && !is.na(anchor)) {
      anchor
    } else {
      ""
    }
  }, FUN.VALUE = character(1))
  if (all(existing) && !anyDuplicated(anchors)) {
    return(results)
  }
  util_questionnaire_add_section_anchors(results)
}

#' Internal helper: questionnaire summary data
#'
#' @noRd
util_questionnaire_summary_data <- function(sections) {
  summaries <- mapply(
    SIMPLIFY = FALSE,
    USE.NAMES = FALSE,
    result = sections$results,
    section_index = seq_along(sections$results),
    FUN = function(result, section_index) {
      summary_data <- util_attr(result, "r_summary", exact = TRUE)
      if (!is.data.frame(summary_data) || !nrow(summary_data)) {
        return(NULL)
      }
      summary_data[[".questionnaire_section"]] <- section_index
      summary_data
    }
  )
  summaries <- Filter(is.data.frame, summaries)
  if (!length(summaries)) {
    return(data.frame())
  }
  summary_data <- util_rbind(data_frames_list = summaries)
  required_columns <- c(
    VAR_NAMES,
    "indicator_metric",
    "value",
    "values_raw",
    "call_names",
    "function_name"
  )
  if (!all(required_columns %in% colnames(summary_data))) {
    return(data.frame())
  }
  summary_data[[".questionnaire_order"]] <- seq_len(nrow(summary_data))

  grading_meta_data <- lapply(
    sections$results,
    util_questionnaire_result_grading_meta_data
  )
  grading_meta_data <- Filter(is.data.frame, grading_meta_data)
  if (length(grading_meta_data)) {
    grading_meta_data <- unique(util_rbind(
      data_frames_list = grading_meta_data
    ))
  } else {
    grading_meta_data <- data.frame(
      VAR_NAMES = unique(summary_data[[VAR_NAMES]]),
      GRADING_RULESET = "0",
      stringsAsFactors = FALSE
    )
  }

  classified <- suppressWarnings(util_metrics_to_classes(
    summary_data,
    meta_data = grading_meta_data
  ))
  is_metric <- !is.na(classified[["indicator_metric"]]) &
    !startsWith(classified[["indicator_metric"]], "CAT_") &
    !startsWith(classified[["indicator_metric"]], "MSG_")
  classified <- classified[
    is_metric & !is.na(classified[["class"]]),
    , drop = FALSE
  ]
  if (!nrow(classified)) {
    return(data.frame())
  }
  classified <- classified[order(
    classified[[".questionnaire_section"]],
    classified[[".questionnaire_order"]]
  ), , drop = FALSE]

  section_index <- classified[[".questionnaire_section"]]
  classes <- as.character(classified[["class"]])
  colors <- util_get_colors()
  labels <- util_get_labels_grading_class()
  metric_labels <- util_translate_indicator_metrics(
    classified[["indicator_metric"]],
    ignore_unknown = TRUE
  )
  data.frame(
    section = sections$titles[section_index],
    anchor = sections$anchors[section_index],
    metric = metric_labels,
    value = as.character(classified[["value"]]),
    class = classes,
    color = unname(colors[classes]),
    class_label = unname(labels[classes]),
    stringsAsFactors = FALSE
  )
}

#' Internal helper: questionnaire summary html
#'
#' @noRd
util_questionnaire_summary_html <- function(sections) {
  summary_data <- util_questionnaire_summary_data(sections)
  if (!nrow(summary_data)) {
    return(NULL)
  }

  rows <- lapply(seq_len(nrow(summary_data)), function(row) {
    color <- summary_data[["color"]][row]
    if (is.na(color) || util_empty(color)) {
      color <- "#ffffff"
    }
    foreground <- util_get_fg_color(color)
    assessment <- summary_data[["class_label"]][row]
    if (is.na(assessment) || util_empty(assessment)) {
      assessment <- summary_data[["class"]][row]
    }
    value <- summary_data[["value"]][row]
    cell_text <- if (is.na(value) || util_empty(value)) {
      assessment
    } else {
      sprintf("%s: %s", assessment, value)
    }
    target <- paste0("#", summary_data[["anchor"]][row])
    htmltools::tags$tr(
      htmltools::tags$td(
        htmltools::a(
          href = target,
          summary_data[["section"]][row]
        )
      ),
      htmltools::tags$td(summary_data[["metric"]][row]),
      htmltools::tags$td(
        style = sprintf("background:%s;padding:0;", color),
        htmltools::a(
          href = target,
          title = sprintf(
            "%s: %s",
            summary_data[["metric"]][row],
            cell_text
          ),
          style = paste0(
            "color:", foreground, ";display:block;padding:0.55em 0.7em;",
            "text-decoration:none;"
          ),
          cell_text
        )
      )
    )
  })

  htmltools::div(
    class = "dq-questionnaire-summary",
    htmltools::h2(id = "questionnaire-summary", "Summary"),
    htmltools::tags$table(
      class = "table table-striped",
      style = "width:100%;max-width:70rem;",
      htmltools::tags$thead(
        htmltools::tags$tr(
          htmltools::tags$th("Result"),
          htmltools::tags$th("Metric"),
          htmltools::tags$th("Assessment")
        )
      ),
      do.call(htmltools::tags$tbody, rows)
    )
  )
}

#' Internal helper: questionnaire navigation menu
#'
#' @noRd
util_questionnaire_navigation_menu <- function(sections, have_summary) {
  links <- if (have_summary) {
    list(htmltools::a(
      href = "#questionnaire-summary",
      title = "Jump to summary",
      "Summary"
    ))
  } else {
    list()
  }
  links <- c(links, mapply(
    SIMPLIFY = FALSE,
    USE.NAMES = FALSE,
    anchor = sections$anchors,
    title = sections$titles,
    FUN = function(anchor, title) {
      htmltools::a(
        href = paste0("#", anchor),
        title = paste("Jump to", title),
        title
      )
    }
  ))
  links <- lapply(links, htmltools::tags$li)
  util_float_index_menu(object = do.call(htmltools::tagList, links))
}

#' Internal helper: questionnaire html content
#'
#' @noRd
util_questionnaire_html_content <- function(x, dir, use_plot_ly, ...) {
  sections <- util_questionnaire_html_sections(x)
  summary <- util_questionnaire_summary_html(sections)
  content <- mapply(
    SIMPLIFY = FALSE,
    USE.NAMES = FALSE,
    dqr = sections$results,
    result_name = names(sections$results),
    result_title = sections$titles,
    anchor = sections$anchors,
    FUN = function(dqr, result_name, result_title, anchor) {
      attr(dqr, "dq_result_title") <- NULL
      attr(dqr, "dq_result_anchor") <- NULL
      htmltools::tagList(
        htmltools::h2(id = anchor, result_title),
        util_pretty_print(
          dqr = dqr,
          nm = result_name,
          is_single_var = FALSE,
          meta_data = data.frame(),
          label_col = VAR_NAMES,
          use_plot_ly = use_plot_ly,
          dir = dir,
          is_ssi = TRUE,
          link_variables = FALSE,
          ...
        )
      )
    }
  )
  list(
    content = htmltools::tagList(summary, content),
    menu = util_questionnaire_navigation_menu(
      sections,
      have_summary = !is.null(summary)
    )
  )
}
