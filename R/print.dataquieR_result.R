#' Print a [dataquieR] result returned by [dq_report2]
#' @aliases dataquieR_result
#' @param x [list] a dataquieR result from [dq_report2] or
#'                 `util_eval_to_dataquieR_result`
#' @param ... passed to print. Additionally, the argument `slot` may be passed
#'            to print only specific sub-results.
#' @seealso `util_pretty_print()`
#' @return see print
#' @export
print.dataquieR_result <- function(x, ...) {
  withr::local_options(list(
    dataquieR.CONDITIONS_WITH_STACKTRACE = FALSE,
    dataquieR.ERRORS_WITH_CALLER = FALSE,
    dataquieR.WARNINGS_WITH_CALLER = FALSE,
    dataquieR.MESSAGES_WITH_CALLER = FALSE
  ))
  messages <- util_attr(x, "message", exact = TRUE)
  warnings <- util_attr(x, "warning", exact = TRUE)
  errors <- util_attr(x, "error", exact = TRUE)
  if (length(messages) > 0) {
    for (m in messages) {
      util_message("%s", m)
    }
  }
  if (length(warnings) > 0) {
    for (w in warnings) {
      util_warning("%s", w)
    }
  }
  error_shown <- FALSE
  if (length(errors) > 0) {
    e <- errors[[1]]
    try(util_error("%s", e))
    error_shown <- TRUE
  }
  attr(x, "message") <- NULL
  attr(x, "warning") <- NULL
  attr(x, "error") <- NULL
  if (inherits(x, "empty")) {
    return()
  }
  class(x) <- setdiff(class(x), c("dataquieR_result", "square_result_list"))
  if (inherits(x, "dataquieR_NULL")) {
    x <- NULL
  }
  opts <- list(...)
  if ("slot" %in% names(opts)) {
    if (opts$slot %in% names(x)) {
      if (!("view" %in% names(list(...))) || (!identical(
        list(...)[["view"]],
        FALSE
      ))) {
        print(x[[opts$slot]])
      } else {
        invisible(x[[opts$slot]])
      }
    } else {
      if (!error_shown) util_error("Cannot find %s in result", opts$slot)
    }
  } else {
    if (error_shown && is.null(x)) {
      return()
    }
    if (!("view" %in% names(list(...))) || (!identical(
      list(...)[["view"]],
      FALSE
    ))) {
      print(x, ...)
    } else {
      invisible(x)
    }
  }
}

ALLOWED_DATAQUIER_RESULT_NAMES <- character(0)

(function() {
  prefixes <-
    c("Result", "Dataframe", "Segment", "Summary", "VariableGroup")
  pre_prefixes <-
    c("Modified", "Flagged", "")
  suffixes <-
    c("Table", "Data")
  singleton <-
    c(
      "ReportSummaryTable",
      "SummaryPlot",
      "SummaryPlotList",
      "PlotlyPlot",
      "DataTypePlotList",
      "DataframeDataList",
      "SegmentDataList",
      "VariableGroupPlotList",
      "ModifiedStudyData",
      "FlaggedStudyData",
      "OtherData",
      "OtherTable",
      "Other"
    )
  all_names <- expand.grid(
    pre_prefixes, prefixes, suffixes
  )
  assign(
    "ALLOWED_DATAQUIER_RESULT_NAMES",
    c(apply(all_names, 1, paste0, collapse = ""), singleton),
    parent.frame()
  )
})()

#' Internal helper: is gg
#'
#' @noRd
util_is_gg <- function(x) {
  # Handle S7-wrapped dq_lazy_ggplot
  if (dq_lazy_register_s7() &&
      inherits(x, "S7_object") &&
      !is.null(.dq_lazy_state$s7_class) &&
      S7::S7_inherits(x, .dq_lazy_state$s7_class)) {
    return(TRUE)
  }
  return(inherits(x, "gg") || inherits(x, "dq_lazy_ggplot") ||
      inherits(x, "") ||
      inherits(x, "util_pairs_ggplot_panels") ||
      inherits(x, "svg_plot_proxy") ||
      util_is_gg_plot(x))
}

#' Internal helper: is gg plot
#'
#' @noRd
util_is_gg_plot <- function(x) {
  util_error(
    paste(
      "Internal error, sorry. Please report! Will be availble later. As",
      "a dataquieR developer: util_is_gg_plot cannot be used during package",
      "load."
    )
  )
}

# Historical debug recipe removed here. Inspect commit c48791d1e3 for
# the old dq_report2 probes and the referenced revised metadata workbook.
#' Internal helper: dataquieR result
#'
#' @noRd
util_dataquieR_result <- function(r) {
  if (inherits(r, "dataquieR_NULL") ||
      length(util_attr(r, "error", exact = TRUE)) == 1) {
    return(r)
  }
  util_stop_if_not(is.list(r))
  # Historical null-slot repair removed here. Inspect commit 7a1b7fdf3e
  # before restoring the old fallback behavior.
  util_stop_if_not(all(trimws(names(r)) != ""))
  which_not <- names(r)[
    !startsWith(names(r), "ScalarValue_") &
      !names(r) %in% ALLOWED_DATAQUIER_RESULT_NAMES
  ]
  if (length(which_not) > 0) {
    util_error(
      c(
        "Internal error, sorry. Found an unexpected",
        "result %s, please report."
      ),
      util_pretty_vector_string(which_not)
    )
  }
  .util_is_data_frame_or_length0 <- function(x) {
    length(x) == 0 || is.data.frame(x)
  }
  util_stop_if_not(all(vapply(r[endsWith(names(r), "Table")],
        .util_is_data_frame_or_length0,
        FUN.VALUE = logical(1)
      )))
  TableSlots <- endsWith(names(r), "Table") &
    vapply(r, .util_is_data_frame_or_length0, FUN.VALUE = logical(1))
  if (any(TableSlots)) {
    for (TableSlot in names(r)[TableSlots]) {
      class(r[[TableSlot]]) <- union("TableSlot", class(r[[TableSlot]]))
      if (inherits(r[[TableSlot]], "ReportSummaryTable")) {
        class(r[[TableSlot]]) <- union(
          "ReportSummaryTable",
          class(r[[TableSlot]])
        )
      }
    }
  }
  DataSlots <- endsWith(names(r), "Data") &
    !endsWith(names(r), "StudyData") &
    vapply(r, .util_is_data_frame_or_length0, FUN.VALUE = logical(1))
  if (any(DataSlots)) {
    for (DataSlot in names(r)[DataSlots]) {
      class(r[[DataSlot]]) <- union("DataSlot", class(r[[DataSlot]]))
    }
  }
  StudyDataSlots <- endsWith(names(r), "StudyData") &
    vapply(r, .util_is_data_frame_or_length0, FUN.VALUE = logical(1))
  if (any(StudyDataSlots)) {
    for (StudyDataSlot in names(r)[StudyDataSlots]) {
      class(r[[StudyDataSlot]]) <- union(
        "StudyDataSlot",
        class(r[[StudyDataSlot]])
      )
    }
  }
  if ("Other" %in% names(r)) {
    class(r[["Other"]]) <- union("Other", class(r[["Other"]]))
  }
  if ("ReportSummaryTable" %in% names(r)) {
    util_stop_if_not(inherits(r$ReportSummaryTable, "ReportSummaryTable"))
  }
  if ("PlotlyPlot" %in% names(r)) {
    util_stop_if_not(inherits(r$PlotlyPlot, "plotly"))
  }
  if ("SummaryPlot" %in% names(r)) {
    util_stop_if_not(util_is_gg(r$SummaryPlot))
  }
  if ("SummaryPlotList" %in% names(r)) {
    util_stop_if_not(is.list(r$SummaryPlotList))
    util_stop_if_not(all(vapply(r$SummaryPlotList,
          util_is_gg,
          FUN.VALUE = logical(1)
        )))
  }
  if ("DataTypePlotList" %in% names(r)) {
    util_stop_if_not(is.list(r$DataTypePlotList))
    util_stop_if_not(all(vapply(r$DataTypePlotList,
          util_is_gg,
          FUN.VALUE = logical(1)
        )))
  }
  if ("DataframeDataList" %in% names(r)) {
    util_stop_if_not(is.list(r$DataframeDataList) &&
        !is.data.frame(r$DataframeDataList))
  }
  if ("SegmentDataList" %in% names(r)) {
    util_stop_if_not(is.list(r$SegmentDataList) &&
        !is.data.frame(r$SegmentDataList))
  }
  if ("VariableGroupPlotList" %in% names(r)) {
    util_stop_if_not(is.list(r$VariableGroupPlotList))
    util_stop_if_not(all(vapply(r$VariableGroupPlotList,
          util_is_gg,
          FUN.VALUE = logical(1)
        )))
  }
  if ("ResultTable" %in% names(r)) {
    util_stop_if_not(.util_is_data_frame_or_length0(r$ResultTable))
    util_stop_if_not(length(r$ResultTable) == 0 ||
        ncol(r$ResultTable) == 0 ||
        "ResultName" %in% colnames(r$ResultTable))
  }
  if ("ResultData" %in% names(r)) {
    util_stop_if_not(.util_is_data_frame_or_length0(r$ResultData))
  }
  if ("OtherData" %in% names(r)) {
    util_stop_if_not(.util_is_data_frame_or_length0(r$OtherData))
  }
  if ("OtherTable" %in% names(r)) {
    util_stop_if_not(.util_is_data_frame_or_length0(r$OtherTable))
  }
  if ("DataframeTable" %in% names(r)) {
    util_stop_if_not(.util_is_data_frame_or_length0(r$DataframeTable))
    util_stop_if_not(length(r$DataframeTable) == 0 ||
        ncol(r$DataframeTable) == 0 ||
        "DF_NAME" %in% colnames(r$DataframeTable))
  }
  if ("SegmentTable" %in% names(r)) {
    util_stop_if_not(.util_is_data_frame_or_length0(r$SegmentTable))
    util_stop_if_not(length(r$SegmentTable) == 0 ||
        ncol(r$SegmentTable) == 0 ||
        "Segment" %in% colnames(r$SegmentTable))
  }
  if ("SummaryTable" %in% names(r)) {
    util_stop_if_not(.util_is_data_frame_or_length0(r$SummaryTable))
    util_stop_if_not(length(r$SummaryTable) == 0 ||
        ncol(r$SummaryTable) == 0 ||
        "Variables" %in% colnames(r$SummaryTable))
  }
  if ("ReportSummaryTable" %in% names(r)) {
    util_stop_if_not(.util_is_data_frame_or_length0(r$ReportSummaryTable))
    util_stop_if_not(length(r$ReportSummaryTable) == 0 ||
        ncol(r$ReportSummaryTable) == 0 ||
        "Variables" %in% colnames(r$ReportSummaryTable))
    util_stop_if_not(length(r$ReportSummaryTable) == 0 ||
        ncol(r$ReportSummaryTable) == 0 ||
        "N" %in% colnames(r$ReportSummaryTable))
  }
  if ("VariableGroupTable" %in% names(r)) {
    util_stop_if_not(.util_is_data_frame_or_length0(r$VariableGroupTable))
    util_stop_if_not(length(r$VariableGroupTable) == 0 ||
        ncol(r$VariableGroupTable) == 0 ||
        "VARIABLE_LIST" %in% colnames(r$VariableGroupTable))
  }
  class(r) <- union(c("dataquieR_result", "master_result"), class(r))
  r
}

#' Print a `StudyDataSlot` object
#'
#' @param x the object
#' @param ... not used
#'
#' @return see print
#' @export
print.StudyDataSlot <- function(x, ...) {
  util_ensure_suggested("tibble")
  r <- tibble::as_tibble(x)
  if (!("view" %in% names(list(...))) || (!identical(
    list(...)[["view"]],
    FALSE
  ))) {
    print(r)
  } else {
    invisible(r)
  }
}

#' Print a `DataSlot` object
#'
#' @param x the object
#' @param ... not used
#'
#' @return see print
#' @export
print.DataSlot <- function(x, ...) {
  x <- util_apply_entity_grading_for_render(x)
  entity_grading_args <- util_attr(x, "entity_grading_args", exact = TRUE)
  r <- util_html_table(x, additional_init_args = entity_grading_args)
  if (!is.null(r)) r <- htmltools::browsable(r)
  if (!("view" %in% names(list(...))) || (!identical(
    list(...)[["view"]],
    FALSE
  ))) {
    if (isTRUE(getOption("knitr.in.progress"))) {
      util_ensure_suggested("knitr", "knit-print")
      class(x) <- setdiff(class(x), "DataSlot")
      knitr::knit_print(x)
    } else {
      print(r)
    }
  } else {
    invisible(r)
  }
}

#' Print a `TableSlot` object
#'
#' @param x the object
#' @param ... not used
#'
#' @return see print
#' @export
print.TableSlot <- function(x, ...) {
  r <- util_make_data_slot_from_table_slot(x)
  r <- util_apply_entity_grading_for_render(r)
  entity_grading_args <- util_attr(r, "entity_grading_args", exact = TRUE)
  r <- util_html_table(r, additional_init_args = entity_grading_args)
  if (!is.null(r)) r <- htmltools::browsable(r)
  if (!("view" %in% names(list(...))) || (!identical(
    list(...)[["view"]],
    FALSE
  ))) {
    print(r)
  } else {
    invisible(r)
  }
}

#' Print a `master_result` object
#'
#' @param x the object
#' @param template the template for the `iframes`, not used, so far.
#' @param ... not used
#'
#' @return `invisible(NULL)`
#' @export
print.master_result <- function(x, template = "default", ...) {
  if (is.list(x) &&
    is.null(util_attr(x, "cn", exact = TRUE)) &&
    identical(
      try(
        all(
          vapply(x, function(y) {
            inherits(y, "dataquieR_result") &&
              inherits(y, "master_result")
          },
          FUN.VALUE = logical(1)
          ),
          na.rm = TRUE
        ),
        silent = TRUE
      ),
      TRUE
    )) {
    class(x) <- unique(c("list", class(x)))
    return(print.list(x, ...))
  }
  template <- "default"
  if (isTRUE(getOption("knitr.in.progress"))) {
    f <- withr::local_tempdir(.local_envir = knitr::knit_global())
  } else {
    f <- withr::local_tempdir(.local_envir = rlang::global_env())
  }
  withr::local_dir(f)
  doc <- util_save_master_result_html(x, dir = f, template = template, ...)

  if (!("view" %in% names(list(...))) || (!identical(
    list(...)[["view"]],
    FALSE
  ))) {
    if (isTRUE(getOption("knitr.in.progress"))) {
      util_ensure_suggested("htmlwidgets", "Render results in RMarkdown")
      # A custom HTML widget sizing policy was considered here.

      util_ensure_suggested("knitr", "knit-print")
      if (knitr::is_latex_output()) {
        util_warning(
          c(
            "%s in R markdown not yet supported by %s for printing",
            "full results"
          ),
          sQuote(knitr::pandoc_to()), sQuote(packageName())
        )
        return("")
      } else if (knitr::is_html_output()) {
        return(knitr::knit_print(
          statichtmlWidget(doc,
            js =
              '$(function(){$("body").css("overflow", ""); })'
          )
        ))
      } else {
        # Chunk height handling was considered here.
        util_warning(
          c(
            "%s in R markdown not yet supported by %s for printing",
            "full results"
          ),
          sQuote(knitr::pandoc_to()), sQuote(packageName())
        )
        return("")
      }
    } else {
      viewer <- getOption("viewer", utils::browseURL)
      viewer("index.html")
    }
  }
  invisible(NULL)
}

#' Internal helper: save master result html
#'
#' @noRd
util_save_master_result_html <- function(x, dir, template = "default", ...) {
  util_ensure_suggested("rmarkdown", goal = "render dataquieR results")
  old_called_in_pipeline <- .dq2_globs$.called_in_pipeline
  .dq2_globs$.called_in_pipeline <- TRUE
  withr::defer({
    .dq2_globs$.called_in_pipeline <- old_called_in_pipeline
  })
  template <- "default"
  package_name <- packageName()
  logo <- "logo.png"
  logo_src <- system.file(
    "logos",
    "dataquieR_48x48.png",
    package = package_name
  )
  if (nzchar(logo_src)) {
    file.copy(logo_src, file.path(dir, logo), overwrite = TRUE)
  }
  jqui <- rmarkdown::html_dependency_jqueryui()
  jqui$stylesheet <- "jquery-ui.min.css"
  function_name <- util_attr(x, "function_name", exact = TRUE)
  if (!is.null(function_name) &&
    function_name %in%
      c(
        "con_limit_deviations",
        "con_hard_limits",
        "con_soft_limits",
        "con_detection_limits"
      )) {
    x$ReportSummaryTable <- NULL
  }
  result_list <- util_attr(x, "dq_result_list", exact = TRUE)
  is_questionnaire <- identical(
    util_attr(x, "dq_questionnaire_result", exact = TRUE),
    TRUE
  )
  use_plot_ly <- util_ensure_suggested(
    "plotly",
    "plot interactive figures",
    err = FALSE
  )
  page_menu <- NULL
  if (is_questionnaire) {
    questionnaire_html <- util_questionnaire_html_content(
      x = x,
      dir = dir,
      use_plot_ly = use_plot_ly,
      ...
    )
    cnt <- questionnaire_html$content
    page_menu <- questionnaire_html$menu
  } else if (length(result_list)) {
    result_titles <- util_attr(x, "dq_result_titles", exact = TRUE)
    if (length(result_titles) != length(result_list)) {
      result_titles <- names(result_list)
    }
    names(result_titles) <- names(result_list)
    cnt <- htmltools::tagList(lapply(names(result_list), function(result_name) {
      dqr <- result_list[[result_name]]
      result_title <- result_titles[[result_name]]
      attr(dqr, "dq_result_title") <- NULL
      htmltools::tagList(
        htmltools::h2(result_title),
        util_pretty_print(
          dqr = dqr,
          nm = result_name,
          is_single_var = FALSE,
          meta_data = data.frame(),
          label_col = VAR_NAMES,
          use_plot_ly = use_plot_ly,
          dir = dir,
          is_ssi = is_questionnaire,
          ...
        )
      )
    }))
  } else {
    cnt <- util_pretty_print(
      dqr = x, nm = util_attr(x, "cn", exact = TRUE),
      is_single_var = FALSE,
      use_plot_ly = use_plot_ly,
      dir = dir,
      is_ssi = is_questionnaire,
      ...
    )
  }
  util_write_iframe_results(
    pages = cnt,
    progress_msg = function(...) {},
    progress = function(...) {},
    template_file = system.file("templates",
      template,
      "iframe.html",
      package =
        package_name
    ),
    dir = dir
  )
  doc <- htmltools::tagList(
    rmarkdown::html_dependency_jquery(),
    html_dependency_tippy(),
    html_dependency_clipboard(),
    html_dependency_dataquieR(iframe = FALSE),
    html_dependency_jspdf(),
    jqui,
    htmltools::div(class = "navbar"),
    page_menu,
    cnt,
    htmltools::tags$script(paste0(
      "window.dataquieR_single_result = true ; ",
      "$(function(){$(\"body\").css(\"overflow\", \"\"); ",
      "$(\".navbar\").hide(); $(\".default-target\").height(\"1em\");})"
    )),
    # htmltools::tags$script('    setTimeout(function() {
    # debugger
    #                        window.dispatchEvent(new Event("resize")) }, 500)')
  )

  deps_prepro <- util_copy_all_deps(
    dir = dir,
    doc,
    rmarkdown::html_dependency_jquery(),
    jqui,
    html_dependency_clipboard(),
    html_dependency_tippy(),
    rmarkdown::html_dependency_font_awesome(),
    html_dependency_dataquieR(),
    html_dependency_jspdf()
  )

  html_result <- htmltools::htmlTemplate(
    system.file("templates",
      template,
      "report.html",
      package =
        package_name
    ),
    document_ = TRUE,
    by_report = FALSE,
    spage = doc,
    logo = logo,
    menu = NULL,
    loading = NULL,
    deps = deps_prepro$deps,
    title = util_attr(x, "nm", exact = TRUE),
    backlink = NULL,
    header = NULL
  )

  writeLines(
    as.character(html_result),
    con = file.path(dir, "index.html"),
    useBytes = TRUE
  )

  doc
}


# Historical knit_print prototype removed here. Inspect commit 048305c5c1
# before restoring the experimental Slot method.

#' Print a `Slot` object
#'
#' displays all warnings and stuff. then it prints `x`.
#'
#' @param x the object
#' @param ... not used
#'
#' @return calls the next print method
#' @export
print.Slot <- function(x, ...) {
  if (any(inherits(x, c("Other", "ReportSummaryTable")))) {
    return(NextMethod())
  }
  messages <- util_attr(x, "message", exact = TRUE)
  warnings <- util_attr(x, "warning", exact = TRUE)
  errors <- util_attr(x, "error", exact = TRUE)
  if (length(messages) > 0) {
    for (m in messages) {
      util_message("%s", m)
    }
  }
  if (length(warnings) > 0) {
    for (w in warnings) {
      util_warning("%s", w)
    }
  }
  error_shown <- FALSE
  if (length(errors) > 0) {
    e <- errors[[1]]
    try(util_error("%s", e))
    error_shown <- TRUE
  }
  attr(x, "message") <- NULL
  attr(x, "warning") <- NULL
  attr(x, "error") <- NULL
  withr::with_pdf(
    NULL,
    o <- capture.output(rr <- withVisible(NextMethod()))
  )
  r <- rr$value
  v <- rr$visible
  if (!is.null(r)) {
    class(r) <- setdiff(class(r), "Slot")
  }
  if ((v || !("view" %in% names(list(...))) || (!identical(
    list(...)[["view"]],
    FALSE
  )))) {
    print(r)
    # Captured output is intentionally not forwarded here.
  } else {
    invisible(r)
  }
}
