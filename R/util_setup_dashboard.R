#' make an html dashboard for a report
#'
#' @param report [dataquieR report v2][dq_report2]
#' @param make_links [logical] add links to variables
#' @param return_table_only [logical] if `TRUE` returns the table only,
#'                                    otherwise and `htmlwidget`
#' @param repsum [data.frame] optional precomputed [summary()] of `report`
#' @inheritParams util_filter_repsum
#'
#' @return a `htmltools` compatible dashboard of report results
#'         or `NULL`, if package is missing
#' @noRd
util_setup_dashboard <- function(report,
  make_links = FALSE,
  return_table_only = FALSE,
  repsum = NULL,
  vars_to_include = "study") {
  if (!util_ensure_suggested(c("jsonlite"), err = FALSE)) {
    return(NULL)
  }
  title <- util_attr(report, "title", exact = TRUE)
  label_col <- util_attr(report, "label_col", exact = TRUE)
  if (is.null(title)) {
    title <- "Data Quality Report"
  }

  if (is.null(repsum)) {
    repsum <- summary(report)
  }
  if (inherits(repsum, "dataquieR_summary")) {
    repsum <- util_reclassify_dataquieR_summary(repsum)
  }
  table <- if (identical(vars_to_include, "study")) {
    util_dashboard_table(repsum)
  } else {
    util_dashboard_table(repsum, vars_to_include = vars_to_include)
  }

  #
  # return early if table is empty anyway
  #
  if (is.null(table) || nrow(table) == 0 || ncol(table) == 0) {
    return()
  }

  # Retain the original VAR_NAMES for later mapping.
  table[["..VAR_NAMES"]] <- table[[VAR_NAMES]]

  margin <- 2


  #
  # If SummaryPlot or SummaryPlotList are there and ggplot objects,
  # add them for each result
  #
  if (label_col ==
      util_attr(table, "label_col", exact = TRUE)) {
    plot_names <- c("SummaryPlot", "SummaryPlotList")
    table$Figure <- mapply(
      SIMPLIFY = FALSE,
      var_label = table[[util_attr(table, "label_col", exact = TRUE)]],
      call_name = table$call_names,
      function(var_label, call_name) {
        fn_result_name <- paste0(call_name, ".", var_label)
        if (fn_result_name %in% names(report) &&
            any(plot_names %in% names(report[[fn_result_name]]))) {
          result_plot_name <- head(
            intersect(
              plot_names,
              names(report[[fn_result_name]])
            ),
            2
          )
          if (util_is_gg(report[[fn_result_name]][[result_plot_name]])) {
            result <- htmltools::plotTag(
              {
                withr::local_par(list(
                  mar = rep(margin, 4),
                  oma = rep(0, 4)
                ))
                suppressWarnings(suppressMessages(
                  print(report[[fn_result_name]][[result_plot_name]])
                ))
              },
              width = 250,
              height = 200,
              alt = paste("Figure")
            )
            as.character(result)
          } else {
            NA_character_
          }
        } else {
          NA_character_
        }
      }
    )
  }

  #
  # if a row has a figure and it's value is "T" or "F", replace the value with
  # the figure
  #
  logicals_with_figure <- !is.na(table$Figure) & table$value %in% c("T", "F")
  table$value[logicals_with_figure] <- table$Figure[logicals_with_figure]

  # Item-level descriptive summaries are not defined for variable groups.
  add_summary_graphs <- function(table, summary_name, graph_name) {
    if (summary_name %in% colnames(report)) {
      des <- report[, summary_name, "SummaryTable", drop = TRUE]
      des[[label_col]] <- des$Variables
      des[[graph_name]] <- des$Graph
      des$Graph <- NULL
      if (!is.null(dim(des)) && nrow(des) > 0) {
        table <- suppressWarnings(merge(table, des,
            all.x = TRUE,
            by = label_col,
            suffixes = c("", "")
          ))
        table <- util_fix_merge_dups(table)
      }
    }
    table
  }

  if (identical(vars_to_include, "study")) {
    table <- add_summary_graphs(table, "des_summary_continuous", "GraphCon")
    table <- add_summary_graphs(table, "des_summary_categorical", "GraphCat")
  }

  no_lb <- is.na(table$Variable_names) # happens for variables w/o any descriptive statistics (SCALE_LEVEL is "na"), since the column comes from des_summary() # nolint: line_length_linter.

  table$Variable_names[no_lb] <-
    vapply(
      lapply(apply(table[no_lb, intersect(unique(rev(c(
        VAR_NAMES, LABEL, label_col, LONG_LABEL
      ))), colnames(table)), drop = FALSE], 1, unique, simplify = FALSE), unique), # nolint: line_length_linter.
      paste0,
      collapse = "<br />",
      FUN.VALUE = character(1)
    )

  # GraphCon
  # GraphCat
  # Variable Label
  # Variable Names
  # Figure

  if (identical(vars_to_include, "study")) {
    if (!"GraphCat" %in% colnames(table)) {
      table$GraphCat <- NA
    }
    if (!"GraphCon" %in% colnames(table)) {
      table$GraphCon <- NA
    }

    # If both graphs are available, continuous is preferred and categorical is
    # used as the fallback.
    if ("des_summary_continuous" %in% colnames(report) &&
        "des_summary_categorical" %in% colnames(report)) {
      table$Graph <- table$GraphCon
      table$GraphCon <- NULL
      table$Graph[is.na(table$Graph)] <- table$GraphCat[is.na(table$Graph)]
      table$GraphCat <- NULL
    } else if ("des_summary_continuous" %in% colnames(report)) {
      table$Graph <- table$GraphCon
      table$GraphCon <- NULL
    } else if ("des_summary_categorical" %in% colnames(report)) {
      table$Graph <- table$GraphCat
      table$GraphCat <- NULL
    }
  }

  # Keep the variable label as the fixed left-most column. A descriptive plot,
  # when available, follows it instead of claiming the fixed position itself.
  table <- table[, intersect(unique(c(
    label_col,
    "Graph",
    colnames(table)
  )), colnames(table)), drop = FALSE]

  if (make_links) {
    table0 <- table
    #
    # for each of label_col, "Graph" and VAR_NAMES that is in colnames, make a
    # link from the content
    #
    for (cn in intersect(colnames(table), c(
      label_col,
      "Graph",
      VAR_NAMES
    ))) {
      cnt <- table0[[cn]]
      if (any(.no_cnt <- is.na(cnt))) {
        cnt[.no_cnt] <- "&nbsp;"
      }
      lb <- table0[[label_col]]
      cl <- table0[["call_names"]]
      href <- table[["href"]]
      popup_href <- table[["popup_href"]]
      title <- table[["title"]]
      table[[cn]] <- mapply(
        SIMPLIFY = FALSE,
        cnt = cnt,
        lb = lb,
        cl = cl,
        href = href,
        popup_href = popup_href,
        title = title,
        function(cnt, lb, cl, href, popup_href, title) {
          # Historical NA-content debug breakpoint removed in commit 214dd76a7d.
          if (!util_summary_link_available(href, popup_href)) {
            return(as.character(htmltools::HTML(cnt)))
          }
          as.character(htmltools::a(htmltools::HTML(cnt),
            href = href,
            onclick = util_summary_popup_handler(
              url = popup_href,
              link_url = href,
              title = title,
              escape = FALSE
            )
          ))
        }
      )
    }
    table$href <- table$popup_href <- table$title <- NULL
    rm("table0")
  }

  # get indices of colnames and subtract 1 so they make sense for javascript
  var_class_col_js_idx <- which(colnames(table) == "var_class") - 1
  class_col_js_idx <- which(colnames(table) == "Class") - 1
  class_raw_col_js_idx <- which(colnames(table) == "class") - 1
  name_col_js_idx <- which(colnames(table) == label_col) - 1

  table$Class <- table$class # we can handle factors, now

  # translate all colnames
  translated_colnames <- util_translate(
    colnames(table),
    ns = "dashboard_table"
  )
  if (identical(vars_to_include, "variable_group")) {
    translated_colnames[colnames(table) == label_col] <- "Variable group"
    translated_colnames[colnames(table) == VAR_NAMES] <- "Group ID"
  }
  util_translated_colnames(table) <- translated_colnames

  #
  # unlist all columns that are lists
  #
  list_columns <- vapply(table, is.list, FUN.VALUE = logical(1))

  if (any(list_columns)) {
    table[list_columns] <-
      lapply(table[list_columns], unlist)
  }

  attr(table, "indexes") <- list(
    var_class_col_js_idx = var_class_col_js_idx,
    class_col_js_idx = class_col_js_idx,
    class_raw_col_js_idx = class_raw_col_js_idx,
    name_col_js_idx = name_col_js_idx
  )

  #
  if (return_table_only) {
    return(table)
  }

  my_dashboard <- util_dashboard_table2widget(
    table,
    label_col,
    vars_to_include = vars_to_include
  )

  my_dashboard
}

#' Select dashboard columns that may be displayed
#'
#' @noRd
util_dashboard_display_columns <- function(table, internal_columns) {
  displayed_columns <- colnames(table)
  if (prep_is_translated(displayed_columns)) {
    source_columns <- util_untranslated_colnames(table)
    column_positions <- match(internal_columns, source_columns, nomatch = 0L)
    column_positions <- column_positions[column_positions > 0L]
    return(as.character(displayed_columns[column_positions]))
  }
  intersect(internal_columns, displayed_columns)
}

#' Defer decoding dashboard preview images until they approach the viewport
#'
#' @param table Dashboard data frame containing rendered HTML cells.
#'
#' @return `table` with image sources moved to `data-dq-lazy-src`.
#' @noRd
util_defer_dashboard_images <- function(table) {
  placeholder <- paste0(
    "data:image/gif;base64,",
    "R0lGODlhAQABAIAAAAAAAP///ywAAAAAAQABAAACAUwAOw=="
  )
  image_pattern <- paste0(
    "(<img\\b[^>]*?)\\bsrc\\s*=\\s*",
    "([\"'])([^\"']+)\\2"
  )
  image_replacement <- paste0(
    "\\1src=\"", placeholder, "\" ",
    "loading=\"lazy\" decoding=\"async\" ",
    "data-dq-lazy-src=\\2\\3\\2"
  )

  for (column in seq_along(table)) {
    if (!is.character(table[[column]])) {
      next
    }
    table[[column]] <- gsub(
      image_pattern,
      image_replacement,
      table[[column]],
      perl = TRUE,
      ignore.case = TRUE
    )
  }
  table
}

#' Convert a dashboard table into its interactive widget
#'
#' @noRd
util_dashboard_table2widget <- function(table, label_col,
  vars_to_include = "study") {
  table <- util_defer_dashboard_images(table)
  indexes <- util_attr(table, "indexes", exact = TRUE)
  var_class_col_js_idx <- indexes$var_class_col_js_idx
  class_col_js_idx <- indexes$class_col_js_idx
  class_raw_col_js_idx <- indexes$class_raw_col_js_idx
  name_col_js_idx <- indexes$name_col_js_idx

  #
  # make actual dashboard from the data
  #
  source_columns <- if (prep_is_translated(colnames(table))) {
    util_untranslated_colnames(table)
  } else {
    colnames(table)
  }
  initial_columns <- unique(c(
    label_col,
    if (!identical(vars_to_include, "variable_group")) VAR_NAMES,
    if ("Stratum" %in% source_columns) "Stratum",
    "Metric",
    "value",
    "Graph",
    "Figure",
    "Class",
    "Call",
    "var_class"
  ))
  all_columns <- if (identical(vars_to_include, "variable_group")) {
    setdiff(source_columns, c(VAR_NAMES, STUDY_SEGMENT, "..VAR_NAMES"))
  } else {
    source_columns
  }
  my_dashboard <-
    util_html_table(
      table,
      filter = "none",
      descs = setNames(
        rep("", ncol(table)),
        colnames(table)
      ),
      searchBuilder = TRUE, # -- var_class not ok and var_class no unclear
      col_tags = list(
        init = util_dashboard_display_columns(table, initial_columns),
        all = util_dashboard_display_columns(table, all_columns)
      ),
      initial_col_tag = "init",

      # Historical SearchBuilder JSON debug recipe removed here.
      # initial search criteria: var_class != grading_class[[1]] && Class !=
      # Null
      #
      init_search =
      list(criteria = list(
        list(
          condition = "!=",
          data = as.character(util_translate(
            "var_class", as_this_translation = colnames(table)
          )),
          type = "string",
          value = list(util_get_labels_grading_class()[["1"]])
        ),
        list(
          condition = "!null",
          data = as.character(util_translate(
            "Class", as_this_translation = colnames(table)
          )),
          type = "string", value = list()
        )
      ), logic = "AND"),
      #
      # arguments that are passed directly to javascript
      #
      additional_init_args = list(
        grading_cols = as.character(util_translate(unique(c(
          "Class",
          "class",
          "var_class"
        )), as_this_translation = colnames(table))),
        secondary_order = setNames(list(
          c(var_class_col_js_idx, name_col_js_idx, class_raw_col_js_idx)
        ), nm = as.character(util_translate(
          unique(c("var_class")),
          as_this_translation = colnames(table)
        ))),
        grading_order = util_get_labels_grading_class(),
        grading_colors = util_get_colors(),
        fg_colors = util_get_fg_color(util_get_colors()),
        deferred_column_filters = TRUE
      ), # sort handling for this is in report_dt.js:sort_vert_dt().
      # Historical class-column orderData override removed here.
      additional_columnDefs = list(list(
        className = "dt-right",
        # Value is mixed, but should nevertheless be right-aligned.
        targets = which(colnames(table) %in% c("Value")) - 1
      )),
      options = list(
        scroller = TRUE,
        paging = TRUE,
        deferRender = TRUE,
        scrollCollapse = FALSE,
        autoFill = FALSE,
        responsive = FALSE
      )
    )
  htmltools::tagList(
    html_dependency_tippy(),
    html_dependency_clipboard(),
    my_dashboard
  )
}
