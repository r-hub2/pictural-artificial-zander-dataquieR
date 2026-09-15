#' Create a `ggplot2` pie chart
#'
#' needs `htmltools`
#'
#' @param data data as returned by `prep_summary_to_classes` but
#'             summarized by one column (currently, we support
#'             `indicator_metric`, `STUDY_SEGMENT`, and `VAR_NAMES`)
#' @param meta_data [meta_data]
#'
#' @return a `htmltools` compatible object or `NULL`, if package is missing
#'
#' @family summary_functions
#' @export
prep_render_pie_chart_from_summaryclasses_ggplot2 <- function(data,
  meta_data = "item_level") {
  vars_to_include <- util_attr(data, "vars_to_include", exact = TRUE)
  summary_subtitle <- util_attr(data, "summary_subtitle", exact = TRUE)
  ssi <- (identical(vars_to_include, "ssi"))
  variable_group <- identical(vars_to_include, "variable_group")
  te <- topenv(parent.frame(1)) # see https://stackoverflow.com/a/27870803
  if (!(isNamespace(te) && getNamespaceName(te) == "dataquieR")) {
    lifecycle::deprecate_soft(
      "2.1.0.9007",
      "prep_render_pie_chart_from_summaryclasses_ggplot2()",
      "plot.dataquieR_summary()"
    )
  }

  if (nrow(data) == 0) {
    return(htmltools::browsable(htmltools::HTML("")))
  }

  grouped_by <- setdiff(colnames(data), c("class", "value", "percent", "note"))

  util_stop_if_not(length(grouped_by) == 1)

  groups <- unique(data[[grouped_by]])

  util_stop_if_not(length(groups) > 0)

  if (length(groups) > 1) {
    all_pys <- lapply(setNames(nm = groups), function(g) {
      prep_render_pie_chart_from_summaryclasses_ggplot2(
        data[data[[grouped_by]] == g, , drop = FALSE],
        meta_data = meta_data
      )
    })
    ncols <- min(2, ceiling(sqrt(length(all_pys))))
    nrows <- ceiling(length(all_pys) / ncols)

    pys_matrix <- htmltools::tags$table(lapply(
      seq_len(nrows),
      function(rw) {
        if ((rw - 1) * ncols + 1 <= length(all_pys)) { # current row needed?
          cur_tr_as_list <- lapply(seq_len(ncols), function(rw, cl) {
            wch <- (rw - 1) * ncols + cl
            if (wch <= length(all_pys)) {
              o <- htmltools::tags$td(all_pys[wch])
            } else {
              o <- htmltools::tags$td()
            }
            o
          }, rw = rw)
          do.call(htmltools::tags$tr, cur_tr_as_list)
        } else { # should never be reached
          NULL
        }
      }
    ))

    py <- pys_matrix

    py <- htmltools::tags$table(py)
    py <- htmltools::browsable(py)
    return(py)
  }

  labels <- util_get_labels_grading_class()
  labels["NA"] <- "Not classified"
  gg_colors <- util_get_colors()
  gg_colors["NA"] <- "lightgrey"
  names(gg_colors) <- labels[names(gg_colors)]

  if (is.factor(data$class)) {
    data$class <- as.integer(gsub("^cat", "", data$class))
  }
  class_values <- as.character(data$class)
  class_values[is.na(class_values)] <- "NA"
  data$class <- factor(class_values,
    levels = names(labels),
    labels = labels,
    ordered = TRUE
  )

  data <- data[order(data$class, -data$value, decreasing = TRUE), , drop = FALSE] # nolint: line_length_linter.
  data$display_percent <- util_pie_display_percentages(data$value)


  p <- ggplot(data, aes(
    x = 1,
    y = value,
    fill = class
  )) +
    geom_bar(stat = "identity", width = 1) +
    geom_text(
      aes(
        x = 1.1,
        label = .data$display_percent
      ),
      position = ggplot2::position_stack(vjust = 0.5),
      check_overlap = TRUE
    ) +
    ggplot2::coord_polar("y", start = 0, clip = "off") +
    scale_fill_manual(values = gg_colors) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.line = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      # Historical legend hiding removed here.
      legend.title = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      plot.background = ggplot2::element_blank()
    )

  if (identical(grouped_by, "indicator_metric")) {
    title <- util_translate_indicator_metrics(groups,
      short = FALSE,
      long = FALSE
    )
    subtitle <- "percentage of QA classes"
  } else if (identical(grouped_by, "call_names")) {
    fnms <- util_cll_nm2fkt_nm(groups)
    if (nchar(fnms) < nchar(groups)) {
      suff <- gsub("^.*_", ": ", groups)
      substr(suff, 3, 3) <- toupper(substr(suff, 3, 3))
    } else {
      suff <- ""
    }
    title <- paste0(util_map_labels(fnms,
        util_get_concept_info("implementations"),
        to = "dq_report2_short_title",
        from = "function_R",
        ifnotfound = NA_character_
      ), suff)
    subtitle <- "percentage of QA classes"
  } else if (identical(grouped_by, as.character(STUDY_SEGMENT))) {
    title <- groups
    subtitle <- "percentage of QA classes"
  } else if (identical(grouped_by, as.character(VAR_NAMES))) {
    util_expect_data_frame(meta_data)
    title <- prep_get_labels(groups,
      meta_data = meta_data,
      label_class = "LONG"
    )
    subtitle <- "percentage of QA classes"
  } else if (identical(grouped_by, as.character("function_name"))) {
    title <- vapply(groups, util_alias2caption,
      long = TRUE,
      FUN.VALUE = character(1)
    )
    subtitle <- groups
  } else {
    title <- groups
    subtitle <- "percentage of QA classes"
    # Historical empty-subtitle and unknown-grouping error variants removed.
  }

  if (!is.null(summary_subtitle)) {
    subtitle <- summary_subtitle
  }

  if (!ssi && !variable_group) {
    subtitle <- paste(subtitle, sprintf(
      " -- %d of %d %s classified",
      sum(data$value, na.rm = TRUE),
      nrow(meta_data),
      "variables"
    ))
  }

  p <- p + ggtitle(
    title, subtitle
  )

  htmltools::plotTag(
    width = 400, height = 400,
    p, alt = "A summary of data quality assessments"
  )
}
