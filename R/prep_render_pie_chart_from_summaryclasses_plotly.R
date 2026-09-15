#' Create a `plotly` pie chart
#'
#' @param data data as returned by `prep_summary_to_classes` but
#'             summarized by one column (currently, we support
#'             `indicator_metric`, `call_names`,
#'              `STUDY_SEGMENT`, and `VAR_NAMES`)
#' @param meta_data [meta_data]
#'
#' @return a `htmltools` compatible object
#'
#' @family summary_functions
#' @export
prep_render_pie_chart_from_summaryclasses_plotly <- function(data,
  meta_data = "item_level") {
  vars_to_include <- util_attr(data, "vars_to_include", exact = TRUE)
  summary_subtitle <- util_attr(data, "summary_subtitle", exact = TRUE)
  summary_unit_label <- util_attr(data, "summary_unit_label", exact = TRUE)
  ssi <- (identical(vars_to_include, "ssi"))
  variable_group <- identical(vars_to_include, "variable_group")
  te <- topenv(parent.frame(1)) # see https://stackoverflow.com/a/27870803
  if (!(isNamespace(te) && getNamespaceName(te) == "dataquieR")) {
    lifecycle::deprecate_soft(
      "2.1.0.9007",
      "prep_render_pie_chart_from_summaryclasses_plotly()",
      "plot.dataquieR_summary()"
    )
  }

  if (!util_ensure_suggested(c("plotly", "htmltools"), err = FALSE)) {
    return(NULL)
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
      prep_render_pie_chart_from_summaryclasses_plotly(
        data[data[[grouped_by]] == g, , drop = FALSE],
        meta_data = meta_data
      )
    })
    # Historical subplot layout prototype removed here. Inspect with
    # `git show 184d467443 --`
    # `R/prep_render_pie_chart_from_summaryclasses_plotly.R`.
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

  py_colors <- util_get_colors()
  py_colors["NA"] <- "lightgrey"
  labs <- util_get_labels_grading_class()
  labs["NA"] <- "Not classified"

  # wrap the text of the labs if too long. Max no. characters = 20
  no_char_labs <- vapply(labs, FUN = function(x) {
    no_char <- nchar(x)
  }, FUN.VALUE = numeric(1))

  if (any(no_char_labs > 10)) {
    labs <- vapply(labs, FUN = function(x) {
      # remove white spaces at beginning or end
      x <- trimws(x)

      # If labs >20 characters, cut it at 20
      if (nchar(x) > 20) {
        x <- substr(x, start = 1, stop = 20)
      }

      sst <- strsplit(x, "")[[1]] # from https://stackoverflow.com/questions/11619616/how-to-split-a-string-into-substrings-of-a-given-length # nolint: line_length_linter.
      m <- matrix("",
        nrow = 10,
        ncol = (length(sst) + 10 - 1) %/% 10
      )
      m[seq_along(sst)] <- sst
      x <- apply(m, 2, paste, collapse = "")
      rm(sst, m)
      # remove white spaces at beginning or end
      x <- trimws(x)
      x <- paste0(x, collapse = "<br>")
    }, FUN.VALUE = character(1))
  }


  if (is.factor(data$class)) {
    data$class <- as.integer(gsub("^cat", "", data$class))
  }
  class_values <- as.character(data$class)
  class_values[is.na(class_values)] <- "NA"
  data$class <- factor(class_values,
    levels = names(py_colors),
    ordered = TRUE
  )

  data <- data[order(data$class, -data$value, decreasing = TRUE), , drop = FALSE] # nolint: line_length_linter.
  hoverinfo <- "label+percent+value"

  if ("note" %in% colnames(data)) {
    hoverinfo <- paste0(hoverinfo, "+text")
  } else {
    data$note <- ""
  }
  if (variable_group) {
    if (is.null(summary_unit_label)) {
      summary_unit_label <- "group-metric results"
    }
    note_heading <- paste0(
      toupper(substr(summary_unit_label, 1L, 1L)),
      substring(summary_unit_label, 2L)
    )
    data$note <- paste0("<b>", note_heading, "</b><br>", data$note)
  }

  if (length(groups) > 1) {
    # Historical sunburst prototype removed here. Inspect with
    # `git show 184d467443 --`
    # `R/prep_render_pie_chart_from_summaryclasses_plotly.R`.
  } else {
    rotation_value <- 90
    # order data frame "data"
    data <- data[order(data$class), , drop = FALSE]
    data$display_percent <- util_pie_display_percentages(data$value)


    py <- plotly::add_pie(
      plotly::plot_ly(data,
        height = 400,
        width = 400
      ),
      sort = FALSE,
      direction = "clockwise",
      rotation = rotation_value,
      labels = labs[
        paste(data$class)
      ],
      values = data$value,
      hovertext = data$note,
      customdata = data$note,
      # Segment names are carried through labels and hover text.
      hoverinfo = hoverinfo,
      hovertemplate = if (variable_group) {
        paste0(
          "<b>%{label}</b><br>%{value} ", summary_unit_label, " ",
          "(%{percent})<br>%{customdata}<extra></extra>"
        )
      } else {
        paste0(
          "<b>%{label}</b><br>%{value} (%{percent})",
          "<br>%{customdata}<extra></extra>"
        )
      },
      text = data$display_percent,
      textinfo = "text",
      textposition = "inside",
      insidetextorientation = "horizontal",
      showlegend = TRUE,
      marker = list(
        colors = py_colors[paste(data$class)]
      )
    )
  }

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

  wrapped_title <- util_plotly_wrap_text(title)
  wrapped_subtitle <- util_plotly_wrap_text(subtitle)
  title_line_count <- util_attr(wrapped_title, "line_count", exact = TRUE) +
    util_attr(wrapped_subtitle, "line_count", exact = TRUE)
  extra_title_height <- 18L * max(0L, title_line_count - 2L)


  value_on_top_conditional <- 60 + extra_title_height
  value_bottom_conditional <- 85

  py <- plotly::layout(py,
    title =
      list(
        text =
        as.character(htmltools::tagList(
          wrapped_title,
          htmltools::tags$sup(wrapped_subtitle)
        )),
        y = 0.95,
        yref = "container"
      ),
    autosize = FALSE, # maybe can cause trouble
    legend = list(
      orientation = "h",
      x = 0.5,
      xanchor = "center",
      y = -0.05,
      yanchor = "top",
      font = list(size = 11)
    ),
    margin = list(
      t = value_on_top_conditional,
      r = 30,
      l = 30,
      b = value_bottom_conditional
    )
  )
  py$height <- 400 + extra_title_height

  py <- plotly::config(py, displaylogo = FALSE)

  py <- htmltools::span(
    title = groups,
    `data-tippy-always-on` = "true",
    py
  )

  py <- htmltools::browsable(py)
  return(py)


  # Historical post-return plotly layout prototype removed here. Inspect with
  # `git show 214dd76a7d --`
  # `R/prep_render_pie_chart_from_summaryclasses_plotly.R`.
}

#' Wrap Plotly text without losing long unbroken labels
#'
#' @param text Scalar text to wrap.
#' @param width Maximum number of characters per line.
#'
#' @return Escaped HTML text with `<br>` line breaks and a `line_count`
#'   attribute.
#' @noRd
util_plotly_wrap_text <- function(text, width = 38L) {
  text <- as.character(text)
  util_stop_if_not(length(text) == 1L, length(width) == 1L, width > 3L)
  text <- gsub("[[:space:]]+", " ", trimws(text))
  lines <- strwrap(text, width = width, simplify = FALSE)[[1]]
  lines <- unlist(lapply(lines, function(line) {
    starts <- seq.int(1L, max(1L, nchar(line)), by = width)
    substring(line, starts, starts + width - 1L)
  }), use.names = FALSE)
  if (!length(lines)) {
    lines <- ""
  }
  result <- htmltools::HTML(paste(
    htmltools::htmlEscape(lines),
    collapse = "<br>"
  ))
  attr(result, "line_count") <- length(lines)
  result
}
