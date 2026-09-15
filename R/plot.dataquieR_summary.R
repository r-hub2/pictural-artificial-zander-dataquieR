# nolint start: line_length_linter.
#' Plot a `dataquieR` summary
#'
#' @param x the `dataquieR` summary, see [summary()] and [dq_report2()]
#' @param y not yet used
#' @param ... not yet used
#' @param filter if given, this filters the summary, e.g.,
#'         `filter = call_names == "com_qualified_item_missingness"`
#' @param dont_plot suppress the actual plotting, just return a printable
#'                   object derived from `x`
#' @param stratify_by column to stratify the summary, may be one string.
#' @param disable_plotly [logical] do not use `plotly`, even if installed
#' @param vars_to_include [character] `"study"`, `"ssi"`, or
#'   `"variable_group"`. The latter includes all results emitted as variable
#'   group tables, including non-scale assessments.
#' @param hierarchy not yet defined, but if an argument is given, a
#'                  sunburst chart is displayed, currently, only `DQ_OBS`
#'                  can be used a the hierarchy.
#' @param folder_of_report a named vector with the location of variable and
#'                         `call_names`
#' @param var_uniquenames a data frame with the original variable names and the
#'                        unique names in case of reports created with dq_report_by
#'                        containing the same variable in several reports
#'                        (e.g., creation of reports by sex)
#' @param summary_title optional scalar title for the summary chart. The default
#'   is derived from the displayed assessment units.
#' @param summary_subtitle optional scalar subtitle for the summary chart.
#' @param summary_unit_label optional plural label for counted chart units.
#'
#' @return invisible html object
#' @export
#'
# nolint end
plot.dataquieR_summary <- function(x, y, ..., filter, dont_plot = FALSE,
  stratify_by,
  vars_to_include = "study",
  disable_plotly = FALSE,
  hierarchy,
  folder_of_report = NULL,
  var_uniquenames = NULL,
  summary_title = NULL,
  summary_subtitle = NULL,
  summary_unit_label = NULL) {
  if (!disable_plotly) {
    util_ensure_suggested(
      pkg = c("plotly"),
      goal = "generate interactive HTML-summaries."
    )
  }

  util_stop_if_not("y is not used for plotting summaries" = missing(y))
  util_ensure_suggested(
    pkg = c(
      "htmltools",
      "DT", "rmarkdown",
      "markdown"
    ),
    goal = "generate plain HTML-summaries."
  )

  x <- util_reclassify_dataquieR_summary(x)

  repsum <- x

  indicator_metric <- NULL
  function_name <- NULL
  util_expect_scalar(dont_plot, check_type = is.logical)
  if (!is.null(summary_title)) {
    util_expect_scalar(summary_title, check_type = is.character)
  }
  if (!is.null(summary_subtitle)) {
    util_expect_scalar(summary_subtitle, check_type = is.character)
  }
  if (!is.null(summary_unit_label)) {
    util_expect_scalar(summary_unit_label, check_type = is.character)
  }
  if (missing(stratify_by)) {
    stratify_by <- character(0)
  } else {
    util_expect_scalar(stratify_by, check_type = is.character)
  }
  this <- util_attr(repsum, "this", exact = TRUE)
  summary_meta_data <- this$summary_meta_data
  if (is.null(summary_meta_data)) summary_meta_data <- this$meta_data

  suitable_vars_sum <- util_filter_repsum(
    this$result,
    vars_to_include,
    summary_meta_data,
    this$rownames_of_report,
    this$label_col,
    this$variable_group_call_names,
    study_var_names = this$meta_data[[VAR_NAMES]]
  )
  if (identical(vars_to_include, "variable_group")) {
    suitable_vars_sum <- util_summary_most_specific_group_metrics(
      suitable_vars_sum
    )
  }
  rownames_of_report <- util_attr(suitable_vars_sum, "rownames_of_report",
    exact = TRUE
  )

  if (!disable_plotly && !missing(hierarchy)) {
    rs <- repsum
    attr(rs, "this") <- rlang::env_clone(this)
    rs_this <- util_attr(rs, "this", exact = TRUE)
    rs_this$result <- suitable_vars_sum
    if (is.null(rs_this$result) ||
        !prod(dim(rs_this$result))) {
      return(htmltools::HTML(""))
    }
    return(util_render_sunburst_from_summary_classes(rs,
        vars_to_include = vars_to_include,
        folder_of_report =
          folder_of_report,
        var_uniquenames =
          var_uniquenames,
        ...
      ))
  }

  # Historical summary-per-function prototype removed here. Inspect with
  # `git show a9ec647d12 -- R/plot.dataquieR_summary.R`.

  suppressMessages(suitable_vars_sum %>%
      dplyr::filter(!startsWith(as.character(indicator_metric), "CAT_")) %>%
      dplyr::filter(!startsWith(as.character(indicator_metric), "MSG_")) ->
      all_per_variable_all_issue_classes_except_errors)

  if (!missing(filter)) {
    cl <-
      rlang::call2(dplyr::filter,
        .data = all_per_variable_all_issue_classes_except_errors,
        substitute(filter)
      )
    all_per_variable_all_issue_classes_except_errors <- eval(cl,
      envir = parent.frame()
    )
  }

  if (nrow(all_per_variable_all_issue_classes_except_errors) == 0) {
    return(htmltools::HTML(""))
  }

  if (!all(stratify_by %in%
        colnames(all_per_variable_all_issue_classes_except_errors))) {
    util_error(
      "Cannot stratify summary by %s, I don't know, what %s are.",
      util_pretty_vector_string(setdiff(
        stratify_by,
        colnames(all_per_variable_all_issue_classes_except_errors)
      )),
      util_pretty_vector_string(setdiff(
        stratify_by,
        colnames(all_per_variable_all_issue_classes_except_errors)
      ))
    )
  }

  summary_grouping <- c(VAR_NAMES, stratify_by)
  all_per_variable_all_issue_classes_except_errors <-
    all_per_variable_all_issue_classes_except_errors %>%
    dplyr::mutate(
      result_detail_label = util_summary_plot_note_labels(
        VAR_NAMES,
        meta_data = summary_meta_data,
        vars_to_include = vars_to_include,
        indicator_metrics = .data$indicator_metric,
        call_names = .data$call_names
      ),
      group_display_label = util_summary_plot_group_labels(
        VAR_NAMES,
        meta_data = summary_meta_data,
        vars_to_include = vars_to_include
      )
    )

  worst_per_variable <- all_per_variable_all_issue_classes_except_errors %>%
    dplyr::filter(!is.na(value)) %>%
    # Keep `stratify_by` in the grouping when a stratified summary is plotted.
    dplyr::group_by(dplyr::across(dplyr::all_of(summary_grouping))) %>%
    dplyr::summarise(
      class =
        suppressWarnings(
          util_as_cat(max(util_as_cat(class), na.rm = TRUE))
        ),
      summary_label = dplyr::first(.data$group_display_label),
      note = paste(
        unique(.data$result_detail_label[
          !is.na(.data$result_detail_label) &
            !util_empty(.data$result_detail_label)
        ]),
        collapse = "<br>"
      )
    )
  plot_tab <- worst_per_variable

  if (length(stratify_by)) {
    plot_tab <- plot_tab %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(stratify_by))) %>%
      dplyr::filter(any(!is.na(.data$class))) %>%
      dplyr::ungroup()
  }

  # An all-unclassified pie conveys no distribution and obscures the more
  # useful detailed diagnostics below it.
  if (!any(!is.na(plot_tab$class))) {
    return(htmltools::HTML(""))
  }

  sum_plot_tab <- plot_tab %>%
    # Keep `stratify_by` in the class counts.
    dplyr::group_by(dplyr::across(dplyr::all_of(c("class", stratify_by)))) %>%
    dplyr::summarise(
      value = length(VAR_NAMES),
      note = paste(
        ifelse(
          is.na(.data$summary_label) | util_empty(.data$summary_label),
          .data$note,
          paste0("<b>", .data$summary_label, "</b><br>", .data$note)
        ),
        collapse = "<br><br>"
      )
    )

  if ("note" %in% colnames(sum_plot_tab)) {
    sum_plot_tab$note <- util_summary_plot_compact_hover_notes(
      sum_plot_tab$note
    )
  }

  if (length(stratify_by) == 0) {
    sum_plot_tab <- sum_plot_tab %>%
      dplyr::mutate(
        X = if (is.null(summary_title)) {
          util_summary_plot_title(
            vars_to_include = vars_to_include,
            rownames_of_report = rownames_of_report,
            suitable_vars_sum = suitable_vars_sum,
            all_per_variable_all_issue_classes_except_errors =
              all_per_variable_all_issue_classes_except_errors,
            plot_tab = plot_tab
          )
        } else {
          summary_title
        },
        class = as.integer(class)
      )
  }
  # Historical summary-per-dimension prototype removed here. Inspect with
  # `git show fc26df7e2c -- R/plot.dataquieR_summary.R`.

  attr(sum_plot_tab, "vars_to_include") <- vars_to_include
  attr(sum_plot_tab, "summary_subtitle") <- summary_subtitle
  attr(sum_plot_tab, "summary_unit_label") <- summary_unit_label
  # if (length(unique(sum_plot_tab$X)) > 0) {
  if (!disable_plotly) {
    summaryplots <-
      prep_render_pie_chart_from_summaryclasses_plotly(
        sum_plot_tab,
        meta_data = this$meta_data
      )
  } else {
    summaryplots <-
      prep_render_pie_chart_from_summaryclasses_ggplot2(
        sum_plot_tab,
        meta_data = this$meta_data
      )
  }
  # Empty summary plots return above before renderer selection.

  if (!inherits(summaryplots, "htmlwidget") &&
      !inherits(summaryplots, "html") &&
      !inherits(summaryplots, "shiny.tag") &&
      !inherits(summaryplots, "shiny.tag.list")) {
    if (!all(vapply(summaryplots, inherits, "htmlwidget",
          FUN.VALUE = logical(1)
        ) |
          vapply(summaryplots, inherits, "html",
            FUN.VALUE = logical(1)
          ) |
          vapply(summaryplots, inherits, "shiny.tag",
            FUN.VALUE = logical(1)
          ) |
          vapply(summaryplots, inherits, "shiny.tag.list",
            FUN.VALUE = logical(1)
          ))) {
      util_error(c(
        "Internal error: Not all summaryplots are html htmlwidgets",
        "or shiny.tags / shiny.tag.lists. Sorry, and please report",
        "this bug. Thank you."
      ))
    }
    summaryplots <- do.call(htmltools::tagList, summaryplots)
  }

  r <- htmltools::browsable(summaryplots)

  if (!dont_plot) {
    print(r)
  } else {
    invisible(r)
  }
}

#' Compact hover notes for summary plots
#'
#' @noRd
util_summary_plot_compact_hover_notes <- function(
  notes,
  max_entries = 3L,
  max_characters = 15L
) {
  vapply(notes, function(note) {
    entries <- unique(strsplit(note, "<br><br>", fixed = TRUE)[[1]])
    entries <- entries[!is.na(entries) & nzchar(entries)]
    total <- length(entries)
    entries <- entries[seq_len(min(total, max_entries))]
    entries <- gsub("<[^>]+>", "", entries)
    entries <- trimws(entries)
    entries <- ifelse(
      nchar(entries) > max_characters,
      paste0(substr(entries, 1L, max_characters), "..."),
      entries
    )
    suffix <- if (total > max_entries) {
      paste0("<br>... and ", total - max_entries, " more")
    } else {
      ""
    }
    paste0(paste(entries, collapse = "<br>"), suffix)
  }, character(1))
}

#' Resolve variable-group labels for summary plots
#'
#' @noRd
util_summary_plot_group_labels <- function(var_names, meta_data,
  vars_to_include) {
  if (!identical(vars_to_include, "variable_group")) {
    return(rep(NA_character_, length(var_names)))
  }
  util_expect_data_frame(meta_data)
  if (!LABEL %in% colnames(meta_data)) {
    return(var_names)
  }
  labels <- meta_data[[LABEL]][match(var_names, meta_data[[VAR_NAMES]])]
  labels[util_empty(labels)] <- var_names[util_empty(labels)]
  as.character(labels)
}

#' Resolve note labels for summary plots
#'
#' @noRd
util_summary_plot_note_labels <- function(var_names, meta_data,
  vars_to_include, indicator_metrics = NULL, call_names = NULL) {
  if (!vars_to_include %in% c("ssi", "variable_group")) {
    labels <- suppressWarnings(try(prep_get_labels(var_names,
          max_len = 80,
          label_class = "SHORT",
          meta_data = meta_data
        ), silent = TRUE))
    if (inherits(labels, "try-error")) {
      labels <- var_names
    }
    return(labels)
  }

  util_expect_data_frame(meta_data)
  if (!COMPUTED_VARIABLE_ROLE %in% colnames(meta_data)) {
    meta_data[[COMPUTED_VARIABLE_ROLE]] <- NA_character_
  }
  if (!CHECK_ID %in% colnames(meta_data)) {
    meta_data[[CHECK_ID]] <- NA_character_
  }
  if (!LABEL %in% colnames(meta_data)) {
    meta_data[[LABEL]] <- meta_data[[VAR_NAMES]]
  }

  ssi_info <- util_get_concept_info("ssi")
  metric_labels <- setNames(
    ssi_info[["menu_label"]],
    ssi_info[["SSI_METRICS"]]
  )
  idx <- match(var_names, meta_data[[VAR_NAMES]])
  roles <- meta_data[[COMPUTED_VARIABLE_ROLE]][idx]
  labels <- meta_data[[LABEL]][idx]

  vapply(seq_along(var_names), function(i) {
    role <- roles[[i]]
    if (identical(vars_to_include, "variable_group") && util_empty(role)) {
      metric <- util_summary_plot_variable_group_metric_label(
        indicator_metric = indicator_metrics[[i]],
        call_name = call_names[[i]]
      )
      group <- util_first_non_empty_character(labels[[i]], var_names[[i]])
      return(sprintf("%s: %s", metric, group))
    }
    metric <- util_first_non_empty_character(
      unname(metric_labels[role]),
      role,
      labels[[i]],
      var_names[[i]]
    )
    group <- util_first_non_empty_character(labels[[i]], var_names[[i]])
    sprintf(
      "%s: %s",
      metric,
      group
    )
  }, FUN.VALUE = character(1))
}

#' Resolve metric labels for variable-group summary plots
#'
#' @noRd
util_summary_plot_variable_group_metric_label <- function(indicator_metric,
  call_name) {
  metric <- indicator_metric
  ssi_info <- util_get_concept_info("ssi")
  metric_labels <- setNames(
    ssi_info[["menu_label"]],
    ssi_info[["SSI_METRICS"]]
  )
  mapped_metric_label <- unname(metric_labels[indicator_metric])
  if (!util_empty(mapped_metric_label)) {
    return(mapped_metric_label)
  }
  if (!util_empty(call_name)) {
    metric <- sub(
      paste0("^", gsub(".", "\\\\.", call_name, fixed = TRUE), "\\\\."),
      "",
      metric
    )
  }
  metric <- switch(metric,
    "max_cor" = "Maximum correlation",
    "in_range" = "Within requested range",
    metric
  )
  if (!identical(metric, indicator_metric) &&
      metric %in% c("Maximum correlation", "Within requested range")) {
    return(metric)
  }
  if (!util_empty(call_name)) {
    return(util_alias2caption(call_name, long = TRUE))
  }
  gsub("_", " ", metric, fixed = TRUE)
}

#' Build a summary-plot title
#'
#' @noRd
util_summary_plot_title <- function(vars_to_include, rownames_of_report,
  suitable_vars_sum, all_per_variable_all_issue_classes_except_errors,
  plot_tab) {
  if (identical(vars_to_include, "variable_group")) {
    requested_group_count <- suitable_vars_sum %>%
      dplyr::filter(
        !startsWith(as.character(.data$indicator_metric), "CAT_") &
          !startsWith(as.character(.data$indicator_metric), "MSG_")
      ) %>%
      dplyr::select(VAR_NAMES) %>%
      unique() %>%
      nrow()
    available_group_count <-
      all_per_variable_all_issue_classes_except_errors %>%
      dplyr::filter(!is.na(value)) %>%
      dplyr::select(VAR_NAMES) %>%
      unique() %>%
      nrow()
    classified_group_count <- plot_tab %>%
      dplyr::filter(!is.na(class)) %>%
      dplyr::select(VAR_NAMES) %>%
      unique() %>%
      nrow()
    return(sprintf(
      "%d of %d available variable groups classified (%d requested)",
      classified_group_count, available_group_count, requested_group_count
    ))
  }
  if (!identical(vars_to_include, "ssi")) {
    return(sprintf(
      "%d variables: %d classified by indicators",
      length(rownames_of_report),
      length(unique(plot_tab[[VAR_NAMES]]))
    ))
  }

  computed_result_count <- suitable_vars_sum %>%
    dplyr::select(VAR_NAMES) %>%
    unique() %>%
    nrow()
  classified_result_count <-
    all_per_variable_all_issue_classes_except_errors %>%
    dplyr::filter(!is.na(value) & !is.na(class)) %>%
    dplyr::select(VAR_NAMES) %>%
    unique() %>%
    nrow()

  sprintf(
    "%d of %d computed variable-group results classified",
    classified_result_count,
    computed_result_count
  )
}
