#' Convert a [dataquieR report v2][dq_report2] to a named list of web pages
#'
#' @param report [dataquieR report v2][dq_report2].
#' @param template [character] template to use, only the name, not the path
#' @param disable_plotly [logical] do not use `plotly`, even if installed
#' @param progress [`function`] lambda for progress in percent -- 1-100
#' @param progress_msg [`function`] lambda for progress messages
#' @param block_load_factor [numeric] multiply size of parallel compute blocks
#'                                    by this factor.
#' @param dir [character] output directory for potential `iframes`.
#' @param my_dashboard [list] of class `shiny.tag.list` featuring a dashboard or
#'                            missing or `NULL`
#' @param repsum [data.frame] optional precomputed [summary()] of `report`
#'
#' @return named list, each entry becomes a file with the name of the entry.
#'         the contents are `HTML` objects as used by `htmltools`.
#' @examples
#' \dontrun{
#' devtools::load_all()
#' prep_load_workbook_like_file("meta_data_v2")
#' report <- dq_report2("study_data", dimensions = NULL, label_col = "LABEL")
#' save(report, file = "report_v2.RData")
#' report <- dq_report2("study_data", label_col = "LABEL")
#' save(report, file = "report_v2_short.RData")
#' }
#'
#' @family html
#' @concept process
#' @keywords internal
#' @noRd
util_generate_pages_from_report <- function(
  report, template,
  disable_plotly,
  progress = progress,
  progress_msg = progress_msg,
  block_load_factor,
  dir,
  my_dashboard,
  repsum = NULL
) {
  indicator_metric <- NULL
  function_name <- NULL

  util_ensure_suggested(
    pkg = c("htmltools"),
    goal = "generate interactive HTML-reports."
  )
  have_plot_ly <- util_ensure_suggested(
    pkg = c("plotly"),
    goal = "generate interactive figures in plain HTML-reports.",
    err = FALSE
  )
  if (disable_plotly) have_plot_ly <- FALSE
  if (have_plot_ly) {
    plot_figure <- util_plot_figure_plotly
  } else {
    plot_figure <- util_plot_figure_no_plotly
  }

  vars_in_rep <- rownames(report)

  meta_data <- util_attr(report, "meta_data", exact = TRUE)
  label_col <- util_attr(report, "label_col", exact = TRUE)
  warn_pred_meta <- util_attr(report, "warning_pred_meta", exact = TRUE)
  title <- util_attr(report, "title", exact = TRUE)
  subtitle <- util_attr(report, "subtitle", exact = TRUE)
  label_meta_data_hints <- util_attr(report, "label_meta_data_hints",
    exact = TRUE
  )
  meta_data_segment <- util_attr(report, "meta_data_segment", exact = TRUE)
  report_matrix_list <- util_attr(report, "matrix_list", exact = TRUE)

  pages <- list()
  call_env <- environment()

  append_single_page <- function(
    drop_down_to_attach, # main menu entry (first-level) # nolint: line_length_linter.
    div_name, # sub-menu entry (second-level)
    file_name, # name where the page is stored, it is possible to add more than one page to a file # nolint: line_length_linter.
    ...,
    menu_separator_before = FALSE
  ) { # ... are the contents in htmltools compatible objects
    alternative_names <- util_attr(div_name, "alternative_names", exact = TRUE)
    # Setup ####
    if (file_name %in% names(call_env$pages)) {
      fil <- call_env$pages[[file_name]]
    } else {
      fil <- list()
    }
    all_ids <- unlist(lapply(call_env$pages, names))
    if (div_name %in% all_ids) {
      pgidx <- vapply(
        FUN = `%in%`, X = lapply(call_env$pages, names),
        x = div_name, FUN.VALUE = logical(1)
      )
      existing_page <- call_env$pages[pgidx]
      if (length(existing_page) == 1) {
        file_name <- names(existing_page)
        if (length(file_name) == 1 && is.character(file_name)) {
          existing_page <- existing_page[[file_name]][[div_name]]
          dd_men <- util_attr(existing_page, "dropdown", exact = TRUE)
          dropdown <- existing_page$dropdown
          existing_page$dropdown <- NULL
          pg <- htmltools::tagList(
            existing_page,
            ...,
            dropdown = dropdown
          )
          attr(pg, "dropdown") <- dd_men
          call_env$pages[[file_name]][[div_name]] <- pg
          return(invisible(NULL))
        }
      }
      util_error(
        "Cannot create report, single page with ID %s already exists",
        dQuote(div_name)
      )
    }

    curr_url <- htmltools::tagGetAttribute(.menu_env$menu_entry(
      sprintf("%s#%s", file_name, div_name),
      alternative_names = alternative_names
    ), "href")

    # https://matthewjamestaylor.com/custom-tags?utm_content=cmp-true
    # e.g. <r-1 curr_url=></r-1> // needs to have a hyphen, attributes in
    # custom tags don't need the prefix data-
    curr_url <- htmltools::tag("dataquier-data", varArgs = list(
      `curr-url` =
        gsub("\"", "\\\"",
          curr_url,
          fixed = TRUE
        )
    ))

    sp <- util_attach_attr(htmltools::div(
      ...,
      curr_url,
      class = "singlePage",
      id = div_name
    ),
    dropdown = drop_down_to_attach,
    menu_separator_before = isTRUE(menu_separator_before))
    fil[[div_name]] <- sp
    call_env$pages[[file_name]] <- fil
    invisible(NULL)
  }

  progress_msg("Page generation", "Creating Page 1")
  # Note on changed labels
  label_modification_text <- util_attr(report, "label_modification_text",
    exact = TRUE
  )
  if (nchar(label_modification_text) > 0) {
    label_modification_table <- util_attr(report, "label_modification_table",
      exact = TRUE
    )
    notes_labels <- htmltools::div(
      htmltools::h3("Label modifications"),
      util_html_table(util_df_escape(label_modification_table),
        dl_fn = "Label_modifications"
      )
    )
  } else {
    notes_labels <- htmltools::div()
  }

  # Page 1 ####
  # Technical information about the R session and the report and first Overview
  if (is.null(repsum)) {
    repsum <- summary(report)
  }
  repsum_this <- util_attr(repsum, "this", exact = TRUE)
  report_meta_data_cross_item <- util_report_meta_data_cross_item(report)
  meta_data_cross_item <- util_normalize_cross_item(
    meta_data = meta_data,
    meta_data_cross_item = report_meta_data_cross_item,
    label_col = label_col
  )
  meta_data_cross_item <- util_mark_contradiction_only_groups(
    meta_data_cross_item,
    if (is.null(repsum_this)) data.frame() else repsum_this$result
  )
  if (!is.null(repsum_this)) {
    repsum_this[["meta_data_cross_item"]] <- meta_data_cross_item
  }
  summaryplots <- plot(repsum,
    dont_plot = TRUE, disable_plotly =
      disable_plotly
  )
  comat <- print(repsum, dont_print = TRUE)
  this_repsum <- util_attr(repsum, "this", exact = TRUE)
  summary_metric_rows <- function(results) {
    if (!is.data.frame(results) || !nrow(results) ||
        !"indicator_metric" %in% colnames(results)) {
      return(data.frame())
    }
    metrics <- as.character(results[["indicator_metric"]])
    results[
      !startsWith(metrics, "CAT_") & !startsWith(metrics, "MSG_"),
      ,
      drop = FALSE
    ]
  }
  item_level_results <- util_filter_repsum(
    this_repsum$result,
    vars_to_include = "study",
    meta_data = this_repsum$meta_data,
    rownames_of_report = this_repsum$rownames_of_report,
    label_col = this_repsum$label_col,
    variable_group_call_names = this_repsum$variable_group_call_names
  )
  item_level_metric_results <- summary_metric_rows(item_level_results)
  have_item_level_dq <- nrow(item_level_metric_results) > 0
  variable_group_results <- util_filter_repsum(
    this_repsum$result,
    vars_to_include = "variable_group",
    meta_data = this_repsum$meta_data,
    rownames_of_report = this_repsum$rownames_of_report,
    label_col = this_repsum$label_col,
    variable_group_call_names = this_repsum$variable_group_call_names
  )
  variable_group_metric_results <- summary_metric_rows(
    variable_group_results
  )
  have_variable_group_dq <- nrow(variable_group_metric_results) > 0
  contradiction_rows <- util_summary_metrics_in_concept(
    variable_group_metric_results[["indicator_metric"]],
    "con_con"
  )
  contradiction_metric_results <- variable_group_metric_results[
    contradiction_rows, , drop = FALSE
  ]
  other_variable_group_metric_results <- variable_group_metric_results[
    !contradiction_rows, , drop = FALSE
  ]
  have_contradiction_dq <- nrow(contradiction_metric_results) > 0
  have_other_variable_group_dq <-
    nrow(other_variable_group_metric_results) > 0
  have_contradiction_classifications <-
    "class" %in% colnames(contradiction_metric_results) &&
    any(!util_empty(contradiction_metric_results[["class"]]))
  have_other_variable_group_classifications <-
    "class" %in% colnames(other_variable_group_metric_results) &&
    any(!util_empty(other_variable_group_metric_results[["class"]]))
  have_combined_variable_group_sunburst <-
    have_contradiction_classifications &&
    have_other_variable_group_classifications
  other_variable_group_repsum <- util_summary_subset_metric_concept(
    repsum,
    "con_con",
    include = FALSE
  )
  contradiction_repsum <- util_summary_subset_metric_concept(
    repsum,
    "con_con",
    include = TRUE
  )
  other_variable_group_sunburst <- if (
    have_other_variable_group_classifications && have_plot_ly
  ) {
    plot(
      other_variable_group_repsum,
      hierarchy = "DQ_OBS",
      vars_to_include = "variable_group"
    )
  }
  contradiction_sunburst <- if (
    have_contradiction_classifications && have_plot_ly
  ) {
    plot(
      contradiction_repsum,
      hierarchy = "DQ_OBS",
      vars_to_include = "variable_group"
    )
  }
  all_variable_group_sunburst <- if (
    have_combined_variable_group_sunburst && have_plot_ly
  ) {
    plot(
      repsum,
      hierarchy = "DQ_OBS",
      vars_to_include = "variable_group"
    )
  }
  have_other_variable_group_sunburst <-
    !util_is_empty_html(other_variable_group_sunburst)
  have_contradiction_sunburst <-
    !util_is_empty_html(contradiction_sunburst)
  contradiction_sunburst_notice <- if (have_contradiction_sunburst) {
    util_summary_check_count_title(
      contradiction_metric_results,
      report_meta_data_cross_item,
      "contradiction"
    )
  }
  contradiction_sunburst_count <- util_summary_evaluated_check_count(
    contradiction_metric_results
  )
  variable_group_summaryplots <- if (have_other_variable_group_dq) {
    plot(other_variable_group_repsum,
      vars_to_include = "variable_group",
      dont_plot = TRUE,
      disable_plotly = disable_plotly
    )
  }
  contradiction_summaryplots <- if (have_contradiction_dq) {
    plot(contradiction_repsum,
      vars_to_include = "variable_group",
      dont_plot = TRUE,
      disable_plotly = disable_plotly,
      summary_title = util_summary_check_count_title(
        contradiction_metric_results,
        report_meta_data_cross_item,
        "contradiction"
      ),
      summary_subtitle = paste(
        "grading classes of checks -- not affected",
        "observations"
      ),
      summary_unit_label = "contradiction checks"
    )
  }
  variable_group_comat <- if (have_variable_group_dq) {
    print(repsum,
      vars_to_include = "variable_group",
      grouped_by = "indicator_metric",
      dont_print = TRUE
    )
  }

  integrity_issues_before_pipeline <-
    htmltools::span("")

  if (!is.null(integrity_issues_before_pipeline_conds <-
        util_attr(report, "integrity_issues_before_pipeline",
          exact = TRUE
        )) &&
      length(integrity_issues_before_pipeline_conds) > 0) {
    iss_df <-
      lapply(integrity_issues_before_pipeline_conds, function(cnd) {
        msg <- paste(cli::ansi_strip(conditionMessage(cnd)), collapse = "\n")
        var <- util_attr(cnd, "varname", exact = TRUE)
        if (length(var) == 0) {
          var <- ""
          # Historical empty-table fallback removed here. Inspect with
          # `git show eec77ab7c0 -- R/util_generate_pages_from_report.R`.
        }
        indi <- util_attr(cnd, "integrity_indicator", exact = TRUE)
        if (length(indi) != 1) {
          return(
            data.frame(
              Indicator = character(0),
              `Study Variable` = character(0),
              Issue = character(0),
              check.names = FALSE, stringsAsFactors = FALSE,
              check.rows = FALSE, fix.empty.names = FALSE,
              row.names = NULL
            )
          )
        }
        Indicator <- "Integrity issue"
        abbreviation <- NULL # to make check happy.
        try(
          Indicator <-
            head(util_get_concept_info("dqi", abbreviation == indi, "Name",
                drop = TRUE
              ), 1),
          silent = TRUE
        )
        if (length(Indicator) != 1) {
          Indicator <- "Integrity issue"
        }

        data.frame(
          Indicator = Indicator, `Study Variable` = var, Issue = msg,
          check.names = FALSE, stringsAsFactors = FALSE,
          check.rows = FALSE, fix.empty.names = FALSE,
          row.names = NULL
        )
      })
    iss_df <- util_rbind(data_frames_list = iss_df)
    iss_df <- iss_df[!duplicated(iss_df), , drop = FALSE]
    integrity_issues_before_pipeline <-
      htmltools::tagList(
        htmltools::h2("Integrity Issues"),
        htmltools::p(
          "There where some technical issues with the study data frame."
        ),
        util_html_table(util_df_escape(iss_df), dl_fn = "Integrity_Issues")
      )
  }

  properties <- util_attr(report, "properties", exact = TRUE)
  if (!is.list(properties)) {
    properties <- list(error = "No report properties found, report file corrupted?") # nolint: line_length_linter.
  }

  if (is.list(properties)) {
    p <- properties
    p[vapply(p, is.call, FUN.VALUE = logical(1))] <-
      vapply(lapply(p[vapply(p, is.call, FUN.VALUE = logical(1))], deparse),
        paste,
        collapse = "\n", FUN.VALUE = character(1)
      )
    p[vapply(p, inherits, "POSIXt", FUN.VALUE = logical(1))] <-
      vapply(p[vapply(p, inherits, "POSIXt", FUN.VALUE = logical(1))],
        paste,
        collapse = " ", FUN.VALUE = character(1)
      )
    p <- p[vapply(p, is.vector, FUN.VALUE = logical(1))]
    p <- p[vapply(p, length, FUN.VALUE = integer(1)) == 1]
    p <- data.frame(
      `  ` = names(p),
      ` ` = unlist(unname(p)),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  }

  # Information on dataset sizes
  study_data_dimnames <- util_attr(report, "study_data_dimnames", exact = TRUE)
  study_variables <- util_generate_pages_partition_study_variables(
    study_variables = study_data_dimnames[[2]],
    meta_data = meta_data
  )
  vinsd <- study_variables[["original"]]
  socs_cnt <- length(study_variables[["computed"]])
  vinmd <- meta_data[[VAR_NAMES]]
  meta_data_variables <- util_generate_pages_partition_study_variables(
    study_variables = vinmd,
    meta_data = meta_data
  )
  with_md <- intersect(vinsd, vinmd)
  wo_md <- setdiff(vinsd, vinmd)
  if (length(wo_md) > 0) {
    mark <- function(x) {
      paste0("<strong>", x, "</strong>")
    }
  } else {
    mark <- identity
  }
  meta_data_item_computation <-
    util_attr(report, "meta_data_item_computation", exact = TRUE)
  if (!is.data.frame(meta_data_item_computation)) {
    meta_data_item_computation <- data.frame(
      VAR_NAMES = character(0),
      CHECK_ID = character(0)
    )
  }
  metadata_coverage_pct <- round(
    length(with_md) / length(vinsd) * 100,
    digits = 0
  )
  computed_group_results_pct <- round(
    socs_cnt / length(vinsd) * 100,
    digits = 0
  )
  info_sd <-
    data.frame(
      " " = c(
        "Sample size",
        mark("Number of variables"),
        mark("Study variables with item-level metadata"),
        ifelse(socs_cnt > 0, mark("Computed variable-group results"), "")
      ),
      " " = c(
        length(study_data_dimnames[[1]]),
        mark(as.character(length(vinsd))),
        mark(as.character(length(with_md))),
        ifelse(socs_cnt > 0, mark(as.character(socs_cnt)), "")
      ),
      check.names = FALSE
    )
  info_sd_hover <- info_sd
  info_sd_hover[] <- ""
  if (length(wo_md) > 0) {
    info_sd_hover[3, ] <- paste0(
      "<span style=\"color:#ffffbb\">Variables without metadata: ",
      util_pretty_vector_string(wo_md, n_max = 20),
      "</span>; Variables in study data: ",
      util_pretty_vector_string(vinsd, n_max = 20)
    )
  } else {
    info_sd_hover[3, ] <- paste0(
      metadata_coverage_pct,
      "% metadata coverage; Variables in study data: ",
      util_pretty_vector_string(vinsd, n_max = 20)
    )
  }
  if (socs_cnt > 0) {
    info_sd_hover[4, ] <- paste0(
      computed_group_results_pct,
      "% of study variables"
    )
  }
  info_md <-
    data.frame(
      " " = "Study variables (item-level metadata)",
      " " = length(meta_data_variables[["original"]]),
      check.names = FALSE
    )
  if (is.data.frame(meta_data_segment) &&
      nrow(meta_data_segment) > 0) {
    info_md <-
      rbind(info_md, list("Number of segments", nrow(meta_data_segment)))
  }

  ## create a table with the summary of indicators and descriptors
  info_dim_dq <- util_generate_table_indicators_descriptors(report,
    repsum = repsum
  )
  # Historical descriptor-row prototype removed here. Inspect with
  # `git show 471bad8023 -- R/util_generate_pages_from_report.R`.

  info_dim_dq[info_dim_dq$Dimension == "Accuracy", "No. DQ descriptors"] <-
    info_dim_dq[info_dim_dq$Dimension == "Accuracy", "No. DQ descriptors", drop =
      TRUE] + 5
  # The 5 descriptors are Location Parameters, Spread, Skewness, Kurtosis, and
  # the graph
  info_dim_dq[info_dim_dq$Dimension == "Completeness", "No. DQ descriptors"] <-
    info_dim_dq[info_dim_dq$Dimension == "Completeness", "No. DQ descriptors", drop =
      TRUE] + 1
  # The descriptor is the columns Missing/Valid

  info_dim_dq[["No. DQ descriptors"]] <- NULL

  ## create a table with the summary of scale
  info_scale_dq <- util_generate_table_scale(report,
    repsum = repsum)

  summary_has_value <- function(results, column) {
    is.data.frame(results) && column %in% colnames(results) &&
      any(!util_empty(results[[column]]))
  }
  summary_plot_or_status <- function(plot, results, level) {
    if (!util_is_empty_html(plot)) {
      return(plot)
    }
    calculated <- summary_has_value(results, "value")
    classified <- summary_has_value(results, "class")
    if (calculated && !classified) {
      message <- paste(
        sprintf("%s results were calculated, but no grading rule", level),
        paste(
          "produced a classification. The numeric results remain available",
          "in the summary matrix or dashboard. This is not a data-quality",
          "rating."
        )
      )
    } else if (!calculated) {
      message <- paste(
        sprintf("No %s result could be calculated for this overview.",
          tolower(level)
        ),
        paste(
          "Required data or metadata may be missing, or the selected data",
          "may not be suitable. Review the available check pages and",
          "messages. This is not a data-quality rating."
        )
      )
    } else {
      message <- paste(
        sprintf("No %s classification chart can be displayed.",
          tolower(level)
        ),
        paste(
          "The numeric results remain available in the summary matrix or",
          "dashboard. This is not a data-quality rating."
        )
      )
    }
    htmltools::div(
      class = "dq-report-empty-state",
      role = "note",
      htmltools::strong("No classification chart available"),
      htmltools::p(message)
    )
  }

  item_summaryplots <- summary_plot_or_status(
    summaryplots,
    item_level_metric_results,
    "Item-level"
  )
  variable_group_summaryplots <- if (have_other_variable_group_dq) {
    summary_plot_or_status(
      variable_group_summaryplots,
      other_variable_group_metric_results,
      "Variable-group"
    )
  }
  contradiction_summaryplots <- if (have_contradiction_dq) {
    summary_plot_or_status(
      contradiction_summaryplots,
      contradiction_metric_results,
      "Contradiction-check"
    )
  }
  have_item_level_classifications <- summary_has_value(
    item_level_metric_results,
    "class"
  )
  selected_functions <- unique(vapply(
    colnames(report),
    util_cll_nm2fkt_nm,
    character(1),
    function_alias_map = util_attr(
      report_matrix_list,
      "function_alias_map",
      exact = TRUE
    )
  ))
  target_level_labels <- c(
    item = "individual variables",
    variable_group = "variable groups",
    observational_unit = "observational units",
    segment = "segments",
    dataframe = "data frames"
  )
  selected_target_levels <- names(target_level_labels)[vapply(
    names(target_level_labels),
    function(target_entity) {
      any(selected_functions %in%
          util_report_scope_target_functions(target_entity))
    },
    logical(1)
  )]
  selected_target_text <- if (length(selected_target_levels)) {
    paste0(
      "The selected checks target ",
      paste(target_level_labels[selected_target_levels], collapse = ", "),
      "."
    )
  }

  general_result_grid_style <- paste(
    "display:grid;",
    "grid-template-columns:repeat(auto-fit,minmax(min(100%,28em),1fr));",
    "gap:1.5em;",
    "align-items:start;"
  )
  report_info_results <- htmltools::div(
    class = "dq-general-result-grid",
    style = general_result_grid_style,
    htmltools::div(
      htmltools::h3("Study data summary"),
      # Keep the two-column summary compact and readable.
      htmltools::tags$style(
        paste(
          ".dq-left-col3 td:nth-child(2) {",
          "padding-left: 0.35em !important; }"
        )
      ),
      htmltools::div(
        class = "dq-left-col3",
        htmltools::browsable(util_formattable(
          escape_all_content = FALSE,
          info_sd,
          min_color = c(255, 255, 255),
          max_color = c(255, 255, 255),
          style_header = c(
            "font-weight: bold;width: 18em;text-align: left;",
            "font-weight: bold;width: 6em;text-align: left;"
          ),
          hover_texts = info_sd_hover
        ))
      )
    ),
    htmltools::div(
      htmltools::h3("Metadata summary"),
      htmltools::p(
        style = "color:red;font-size:110%;",
        htmltools::strong(
          htmltools::em(warn_pred_meta)
        )
      ),
      htmltools::browsable(util_formattable(
        info_md,
        min_color = c(255, 255, 255),
        max_color = c(255, 255, 255),
        style_header = c(
          "font-weight: bold;width: 18em;text-align: left;",
          "font-weight: bold;width: 6em;text-align: left;"
        )
      ))
    )
  )
  scope_tree <- util_render_report_scope_tree(
    info_dim_dq,
    info_scale_dq,
    report = report,
    repsum = repsum
  )
  scope_results <- if (!util_is_empty_html(scope_tree)) {
    htmltools::div(
      class = "dq-assessment-scope",
      htmltools::h2("Assessment scope"),
      htmltools::p(
        class = "dq-assessment-scope-references",
        "Assessment scope follows the DQ_OBS concept (Schmidt et al., 2021)."
      ),
      scope_tree
    )
  }
  item_summary_result <- if (have_item_level_dq) {
    htmltools::div(
      htmltools::h2("Data Quality Summary"),
      item_summaryplots,
      htmltools::div(
        style = "margin-top: 0.75em; text-align: left;",
        htmltools::a(
          href = "#Item-level data quality summary",
          "Display Detailed View"
        )
      )
    )
  }
  variable_group_detail_link <- htmltools::div(
    style = "margin-top: 0.75em; text-align: left;",
    htmltools::a(
      href = "variable_group_summary.html",
      "Display Detailed View"
    )
  )
  variable_group_summary_result <- if (have_other_variable_group_dq) {
    htmltools::div(
      htmltools::h2("Data Quality Summary for variable groups"),
      htmltools::p(
        "This chart summarizes evaluated variable-group results other than ",
        "contradiction checks."
      ),
      variable_group_summaryplots,
      if (!have_contradiction_dq) variable_group_detail_link
    )
  }
  contradiction_summary_result <- if (have_contradiction_dq) {
    htmltools::div(
      htmltools::h2("Contradiction checks"),
      htmltools::p(
        "Each slice represents one evaluated contradiction check. ",
        "Percentages show the share of checks in each grading class, not ",
        "the share of observations that violated a rule."
      ),
      contradiction_summaryplots,
      variable_group_detail_link
    )
  }
  summary_results <- if (have_item_level_dq || have_variable_group_dq) {
    htmltools::div(
      class = "dq-general-result-grid",
      style = general_result_grid_style,
      item_summary_result,
      variable_group_summary_result,
      contradiction_summary_result
    )
  } else {
    htmltools::div(
      class = "dq-report-empty-state",
      role = "note",
      htmltools::h2("Data quality summary"),
      if (length(selected_target_text)) htmltools::p(selected_target_text),
      htmltools::p(
        paste(
          "No item-level or variable-group results can be summarized here.",
          "This can happen when checks were not requested at these levels,",
          "could not be calculated, or return results at another assessment",
          "level. Open the available analysis pages and messages for details.",
          "The absence of an overview is not a data-quality rating."
        )
      )
    )
  }

  if (!isTRUE(util_attr(report, "dt_adjust", exact = TRUE))) {
    dt_adjust_info <- htmltools::tagList(
      htmltools::hr(),
      htmltools::p(htmltools::em(htmltools::strong(
        paste(
          "Data types are assumed to be all matching, because this report",
          "was computed with the dt_adjust-option set to FALSE."
        )
      )))
    )
  } else {
    dt_adjust_info <- htmltools::HTML("")
  }

  append_single_page(
    "General",
    util_attach_attr("Report information",
      alternative_names = c("index", "home")
    ),
    "report.html",
    htmltools::tagList(
      htmltools::h1(title),
      htmltools::h2(subtitle),
      report_info_results,
      scope_results,
      summary_results,
      integrity_issues_before_pipeline,
      htmltools::h2("Technical information"),
      htmltools::p("The table below summarizes technical information about this report, the R session and the operating system."), # nolint: line_length_linter.
      util_html_table(util_df_escape(p),
        dl_fn = "Report_Metadata",
        kv_table = TRUE
      ),
      htmltools::p(htmltools::tags$i(
        id = "render-time",
        sprintf(
          "Rendered using %s %s at %s",
          utils::packageName(),
          util_dataquieR_version(),
          as.character(Sys.time())
        )
      )),
      htmltools::tags$script( # https://stackoverflow.com/a/34579496
        '$(function() {
                              var xx = $("#render-time").html()
                              var data = window.renderingData
                              if (data.hasOwnProperty("renderingTime")) {
                                $("#render-time").html(xx + " in " + data.renderingTime)
                              } else {
                                $("#render-time").html(xx + " -- no rendering time available.")
                              }
                              // console.log(data);
                          });'
      ),
      if (!util_is_empty_html(notes_labels)) {
        list(
          htmltools::hr(),
          htmltools::a(id = "notes_labels", notes_labels)
        )
      },
      if (!util_is_empty_html(dt_adjust_info)) {
        list(
          dt_adjust_info,
          htmltools::hr()
        )
      },
      htmltools::h2("Related literature"),
      do.call(htmltools::tagList, lapply(
        format(
          utils::readCitationFile(
            system.file("CITATION",
              package = utils::packageName()
            ),
            list(Encoding = "UTF-8")
          ),
          style = "HTML"
        ),
        htmltools::HTML
      )),
      htmltools::tags$button(
        class = "clipbtn",
        `data-clipboard-text` = paste(
          format(
            utils::readCitationFile(
              system.file("CITATION",
                package =
                  utils::packageName()
              ),
              list(Encoding = "UTF-8")
            ),
            style = "bibtex"
          ),
          collapse = "\n\n"
        ),
        "\U1F4CB BibTeX",
        `aria-label` = "Copy BibTeX to clipboard",
        `data-clipboard-success` = "BibTeX copied to clipboard."
      )
    )
  )

  progress_msg("Page generation", "Creating Page 2")

  # Page 2 ####
  if (have_item_level_dq) {
    append_single_page(
      "General",
      util_attach_attr("Item-level data quality summary",
        alternative_names = c("summary table")
      ),
      "report.html",
      htmltools::htmlTemplate(
        text_ = readLines(system.file("templates", template, "overview.html",
            package = utils::packageName()
          )),
        comat = util_apply_full_page_table_class(comat),
        summaryplots = item_summaryplots,
        # Historical overview matrices and label notes are no longer passed.
        util_float_index_menu = util_float_index_menu,
        util_map_labels = util_map_labels,
        util_get_concept_info = util_get_concept_info,
        util_abbreviate = util_abbreviate
      )
    )
  }

  if (have_item_level_classifications && have_plot_ly) {
    append_single_page(
      "General",
      util_attach_attr("Item-level data quality chart",
        alternative_names = c("summary chart", "sunburst")
      ),
      "sunburst.html",
      plot(repsum, hierarchy = "DQ_OBS")
    )
  }

  if (have_item_level_dq &&
      !missing(my_dashboard) && inherits(my_dashboard, "shiny.tag.list") &&
      !util_is_empty_html(my_dashboard)) {
    append_single_page(
      "General",
      util_attach_attr("Item-level data quality dashboard",
        alternative_names = c("summary dashboard")
      ),
      "dashboard.html",
      util_apply_full_page_table_class(my_dashboard)
    )
  }

  if (have_variable_group_dq) {
    append_single_page(
      "General",
      util_attach_attr("Variable-group data quality summary",
        alternative_names = c("variable-group summary table")
      ),
      "variable_group_summary.html",
      htmltools::tagList(
        htmltools::h1("Overview of variable-group quality checks"),
        htmltools::p(
          "This overview summarizes quality checks that assess groups of ",
          "variables, including scale, association, repeated-measurement, ",
          "and contradiction results."
        ),
        htmltools::h2("Summary charts"),
        if (have_other_variable_group_dq) {
          htmltools::tagList(
            htmltools::h3("Variable-group checks"),
            htmltools::p(
              "This chart summarizes evaluated variable-group results other ",
              "than contradiction checks."
            ),
            variable_group_summaryplots
          )
        },
        if (have_contradiction_dq) {
          htmltools::tagList(
            htmltools::h3("Contradiction checks"),
            htmltools::p(
              "Each slice represents one evaluated contradiction check. ",
              "Percentages show the share of checks in each grading class, ",
              "not the share of observations that violated a rule."
            ),
            contradiction_summaryplots
          )
        },
        htmltools::h2("Summary matrix"),
        variable_group_comat
      )
    )

    if (have_other_variable_group_sunburst || have_contradiction_sunburst) {
      append_single_page(
        "General",
        util_attach_attr("Variable-group data quality chart",
          alternative_names = c("variable-group summary chart")
        ),
        "variable_group_chart.html",
        htmltools::tagList(
          htmltools::h1("Variable-group data quality chart"),
          htmltools::p(
            "The chart arranges variable-group results in the DQ_OBS concept ",
            "hierarchy. Results without a current DQ_OBS mapping are shown ",
            "separately."
          ),
          util_render_variable_group_sunburst_switch(
            other_chart = other_variable_group_sunburst,
            contradiction_chart = contradiction_sunburst,
            all_chart = all_variable_group_sunburst,
            contradiction_count = contradiction_sunburst_count,
            contradiction_notice = contradiction_sunburst_notice
          )
        )
      )
    }

    variable_group_dashboard <- util_setup_dashboard(
      report,
      make_links = TRUE,
      repsum = repsum,
      vars_to_include = "variable_group"
    )
    if (inherits(variable_group_dashboard, "shiny.tag.list") &&
        !util_is_empty_html(variable_group_dashboard)) {
      append_single_page(
        "General",
        util_attach_attr("Variable-group data quality dashboard",
          alternative_names = c("variable-group summary dashboard")
        ),
        "variable_group_dashboard.html",
        util_apply_full_page_table_class(variable_group_dashboard)
      )
    }
  }

  if (!util_parallel_get_options()$settings$mode %in%
      c("local", "multicore", "socket")) {
    util_error(
      c(
        "On a non-local/multicore cluster,",
        "parallel rendering of reports is not supported.",
        "Please call %s before",
        "rendering the report."
      ),
      dQuote("util_parallel_stop()")
    )
    # Historical SharedObject rendering prototype removed here. Inspect with
    # `git show b6a44e4fa3 -- R/util_generate_pages_from_report.R`.
  }

  cores <- util_get_cores_safe()
  if (!is.numeric(cores) || !util_is_integer(cores) || !is.finite(cores)) {
    cores <- 1
  }

  if (!is.null(parallel::getDefaultCluster())) {
    # Historical cluster-export prototype removed here. Inspect with
    # `git show 84111e9319 -- R/util_generate_pages_from_report.R`.
    if (suppressWarnings(util_ensure_suggested("pkgload",
          err = FALSE,
          goal =
            "not really needed"
        ))) {
      dev_package <- pkgload::is_dev_package(utils::packageName())
    } else {
      dev_package <- FALSE
    }

    if (dev_package && !is.null(parallel::getDefaultCluster())) {
      .d <- getNamespaceInfo(asNamespace(utils::packageName()), "path")
      .exp <- substitute({
        pkgload::load_all(path = .d)
        invisible(NULL)
      })
      parallel::clusterExport(envir = environment(), varlist = ".exp")
      parallel::clusterEvalQ(expr = eval(.exp))
    } else {
      util_parallel_library(utils::packageName(), show.info = FALSE)
    }

    ## --- export stuff
    #----------------------------------------------------------
    .options <- options() # options to be copied to the children (child process)
    .options <- .options[startsWith(names(.options), "dataquieR.")] # only dataquieR options selected

    progress_msg("Cluster setup: initializing parallel mode, if applicable", "exporting options") # nolint: line_length_linter.

    suppressMessages(suppressWarnings(util_parallel_export(
      objnames = ".options", show.info = FALSE
    )))

    progress_msg("Cluster setup: initializing parallel mode, if applicable", "exporting data frame cache") # nolint: line_length_linter.

    rule_sets <- getOption(
      "dataquieR.grading_rulesets",
      dataquieR.grading_rulesets_default
    )

    rule_formats <- getOption(
      "dataquieR.grading_formats",
      dataquieR.grading_formats_default
    )

    dataframes_list <- setNames(
      list(
        prep_get_data_frame(rule_sets),
        prep_get_data_frame(rule_formats)
      ),
      nm = c(
        rule_sets,
        rule_formats
      )
    )

    suppressMessages(suppressWarnings(util_parallel_export(
      objnames = "dataframes_list", show.info = FALSE
    )))

    parallel::clusterEvalQ(cl = NULL, options(.options))
    parallel::clusterEvalQ(cl = NULL, prep_add_data_frames(
      data_frame_list = dataframes_list
    ))

    parallel::clusterEvalQ(cl = NULL, {
      ..glbs <- get(".dq2_globs", envir = asNamespace("dataquieR"))
      ..glbs$.called_in_pipeline <- TRUE
    })
  }

  progress_msg("Page generation", "Creating indicator pages in memory")

  dim_pages <- util_html_for_dims(
    report,
    use_plot_ly = have_plot_ly,
    template = template,
    block_load_factor = block_load_factor,
    repsum = repsum,
    dir = dir
  )

  rendered_repsum <- comat

  dim_pages <- dim_pages[vapply(dim_pages, length, FUN.VALUE = integer(1))
    != 0]

  progress_msg("Page generation", "Mounting indicator pages")
  i <- 0
  n <- length(dim_pages)
  for (args in dim_pages) {
    do.call(append_single_page, args) # creates the indicator related pages because cur_var has not yet been set # nolint: line_length_linter.
    i <- i + 1L
    progress(i / n * 100)
  }

  vars_to_create_pages4 <- intersect(
    rownames(report),
    util_map_labels(
      ifnotfound = NA_character_,
      study_data_dimnames[[2]],
      meta_data,
      to = label_col
    )
  )

  vars_to_create_pages4 <-
    vars_to_create_pages4[vapply(vars_to_create_pages4, function(rn) {
      length(report[rn, , drop = FALSE]) > 0
    }, FUN.VALUE = logical(1))]

  meta_data_with_computed_role <- meta_data
  if (!COMPUTED_VARIABLE_ROLE %in%
      colnames(meta_data_with_computed_role)) {
    meta_data_with_computed_role[[COMPUTED_VARIABLE_ROLE]] <- NA_character_
  }
  vars_to_create_pages4 <- vars_to_create_pages4[util_empty(util_map_labels(
    ifnotfound = NA_character_, # remove SSI vars from standard report.
    vars_to_create_pages4,
    meta_data_with_computed_role,
    to = COMPUTED_VARIABLE_ROLE,
    from = label_col
  ))]

  i <- 0
  n <- length(vars_to_create_pages4)
  util_setup_rstudio_job("Page generation: Single Variable View", n = n)

  progress_msg("Page generation", "Generating single variable pages")

  cores <- util_get_cores_safe()
  if (!is.numeric(cores) || !util_is_integer(cores) || !is.finite(cores)) {
    cores <- 1
  }
  block_size <- util_get_render_block_size(
    total = n,
    workers = cores,
    block_load_factor = block_load_factor
  )
  nblocks <- ceiling(n / block_size)
  for (cur_block in seq_len(nblocks) - 1) { # create the single variable pages
    block_indices <- seq(
      1 + cur_block * block_size,
      min(
        cur_block * block_size + block_size,
        nrow(report)
      )
    )
    vars_in_chunk <- vars_to_create_pages4[block_indices]
    vars_in_chunk <- intersect(
      vars_in_chunk,
      util_map_labels(
        ifnotfound = NA_character_,
        study_data_dimnames[[2]],
        meta_data,
        to = label_col
      )
    )
    vars_in_chunk <- vars_in_chunk[!is.na(vars_in_chunk)]
    progress(i / n * 100)
    progress_msg("Page generation", sprintf(
      "Single Variables %s",
      paste(sQuote(vars_in_chunk),
        collapse = ", "
      )
    ))

    # results, cll, repsum, function_alias_map,
    # meta_data, label_col, use_plot_ly, dir,
    # template, wd

    results <-
      lapply(setNames(nm = vars_in_chunk), function(nm) {
        report[nm, , as_raw = TRUE]
      })

    # find which dimensions are included in the report
    dims_in_rep <-
      unique(vapply(strsplit(colnames(report), "_"), `[[`, 1,
          FUN.VALUE = character(1)
        ))

    dims_in_rep <- util_sort_by_order(dims_in_rep, names(dims))

    # match the functions to the dimensions
    clls_in_rep <- lapply(
      setNames(nm = dims_in_rep),
      function(prefix) {
        colnames(report)[startsWith(
          colnames(report),
          prefix
        )]
      }
    )

    combined_list <- Map(function(results, cur_vars) {
      list(
        results = results,
        cur_vars = cur_vars
      )
    }, results = results, cur_vars = vars_in_chunk)

    use_plot_ly <- have_plot_ly
    note_meta <- warn_pred_meta
    function_alias_map <-
      util_attr(report_matrix_list, "function_alias_map", exact = TRUE)

    f <-
      function(cml) {
        cml$results[vapply(cml$results, is.raw, FUN.VALUE = logical(1))] <-
          lapply(cml$results[vapply(cml$results, is.raw, FUN.VALUE = logical(1))], util_decompress) # nolint: line_length_linter.
        # Rendered summaries are passed directly, not loaded from a file.
        util_html_for_var(
          results = cml$results, cur_var = cml$cur_vars,
          dir = dir,
          use_plot_ly = have_plot_ly,
          template = template,
          note_meta = warn_pred_meta,
          rendered_repsum = rendered_repsum,
          meta_data = meta_data,
          label_col = label_col,
          dims_in_rep = dims_in_rep,
          clls_in_rep = clls_in_rep,
          function_alias_map = function_alias_map
        )
      }
    use_plot_ly <- have_plot_ly
    f <- util_isolate_function(
      f,
      c( # "rrs",
        "rendered_repsum",
        "dir",
        "have_plot_ly",
        "template",
        "warn_pred_meta",
        "meta_data",
        "label_col",
        "dims_in_rep",
        "clls_in_rep",
        "function_alias_map"
      )
    )
    chunk_of_pages <-
      util_par_lapply_lb(
        # Default chunk sizing is used for page generation.
        X = combined_list,
        fun = f
      )

    progress_msg("Computed current chunk")
    chunk_of_pages <- do.call(`c`, chunk_of_pages)
    for (args in chunk_of_pages) {
      do.call(append_single_page, args)
    }
    i <- i + length(vars_in_chunk)
    progress(i / n * 100)
  }
  progress_msg("Computed all chunks")

  # SSI by indicator (COMPUTED_VARIABLE_ROLE) #####
  vars_to_create_pages4 <- intersect(
    rownames(report),
    util_map_labels(
      ifnotfound = NA_character_,
      vars_to_create_pages4,
      meta_data,
      to = label_col,
      from = label_col
    )
  )

  vars_to_create_pages4 <-
    vars_to_create_pages4[vapply(vars_to_create_pages4, function(rn) {
      length(report[rn, , drop = FALSE]) > 0
    }, FUN.VALUE = logical(1))]

  variable_group_page_results <- util_setup_dashboard(
    report,
    make_links = FALSE,
    return_table_only = TRUE,
    repsum = repsum,
    vars_to_include = "variable_group"
  )
  variable_group_linked_results <- util_setup_dashboard(
    report,
    make_links = TRUE,
    return_table_only = TRUE,
    repsum = repsum,
    vars_to_include = "variable_group"
  )

  ssi_roles <- # also used later in the other SSI output block
    util_map_labels(
      ifnotfound = NA_character_, # select SSI vars
      rownames(report),
      meta_data_with_computed_role,
      to = COMPUTED_VARIABLE_ROLE,
      from = label_col
    )

  ssi_roles_only <- unique(ssi_roles)
  ssi_roles_only <- ssi_roles_only[ssi_roles_only %in% COMPUTED_VARIABLE_ROLES]
  requested_ssi_roles <- ssi_roles_only
  group_result_roles <- util_generate_pages_variable_group_roles(
    variable_group_page_results
  )
  ssi_roles_only <- unique(c(ssi_roles_only, group_result_roles))

  vars_for_each_role <- lapply(
    setNames(nm = ssi_roles_only),
    function(curr_role) {
      names(which(ssi_roles == curr_role))
    }
  )

  i <- 0
  n <- length(ssi_roles_only) * 2
  util_setup_rstudio_job("Page generation: SSI Views", n = n)

  progress_msg("Page generation", "Generating SSI pages")

  fam <- util_attr(report_matrix_list, "function_alias_map", exact = TRUE) # also used later in the next ssi section

  util_ssi_tag_list <- function(...) {
    htmltools::tagList(Filter(Negate(util_is_empty_html), list(...)))
  }

  util_ssi_render_results <- function(dqr, nm_fun,
    ssi_link_target = c("role", "cross_item"),
    rotate_for_one_row = TRUE,
    popup_nm = NULL) {
    ssi_link_target <- util_match_arg(ssi_link_target)
    util_expect_scalar(rotate_for_one_row, check_type = is.logical)

    rendered <- mapply(
      SIMPLIFY = FALSE,
      dqr = dqr,
      nm = names(dqr),
      FUN = function(dqr, nm) {
        out <- util_pretty_print(
          dqr = dqr,
          nm = nm_fun(nm),
          is_single_var = TRUE,
          use_plot_ly = have_plot_ly,
          meta_data = meta_data_with_computed_role,
          meta_data_cross_item = meta_data_cross_item,
          label_col = label_col,
          dir = dir,
          is_ssi = TRUE,
          ssi_link_target = ssi_link_target,
          rotate_for_one_row = rotate_for_one_row,
          popup_nm = popup_nm
        )
        if (util_is_empty_html(out)) {
          return(NULL)
        }
        out
      }
    )
    rendered <- Filter(Negate(is.null), rendered)
    if (!length(rendered)) {
      return(NULL)
    }
    rendered
  }

  util_ssi_applicable_aliases_slots <- function(cvr) {
    .applicable_functions <-
      unlist(util_parse_assignments(
        util_get_concept_info("ssi",
          get("SSI_METRICS") %in% cvr,
          "functions",
          drop = TRUE
        ),
        multi_variate_text = TRUE
      ))
    applicable_aliases_slots <- lapply(
      .applicable_functions,
      function(af) {
        as <- gsub("^.*\\.", "", af)
        af <- gsub("\\..*$", "", af)
        r <- subset(fam, get("name") == af,
          "alias",
          drop = TRUE
        )
        if (length(r) == 0) {
          r <- ""
        }
        res <- util_recycle(r, as)
        list(alias = res[[1]], slot = res[[2]])
      }
    )
    applicable_slots <- vapply(applicable_aliases_slots, `[[`, "slot",
      FUN.VALUE = character(1)
    )
    applicable_aliases <- vapply(applicable_aliases_slots, `[[`, "alias",
      FUN.VALUE = character(1)
    )
    uniq_al_slo <- unique(cbind(applicable_aliases, applicable_slots))
    data.frame(
      alias = unlist(uniq_al_slo[, "applicable_aliases", drop = TRUE]),
      slot = unlist(uniq_al_slo[, "applicable_slots", drop = TRUE]),
      stringsAsFactors = FALSE
    )
  }

  util_generate_pages_ssi_metric_page <- function(cvr) {
    if (!util_empty(cvr)) {
      ssi_men_lab <- util_get_concept_info("ssi",
        get("SSI_METRICS") == cvr,
        "menu_label",
        drop = TRUE
      )
      ssi_title <- util_get_concept_info("ssi",
        get("SSI_METRICS") == cvr,
        "result_caption",
        drop = TRUE
      )
      ssi_description <- htmltools::HTML(util_ssi_metric_description(cvr))
      generic_metric_results <- util_generate_pages_variable_group_results(
        variable_group_page_results,
        role = cvr,
        label_col = label_col
      )
      applicable_pairs <- util_ssi_applicable_aliases_slots(cvr)
      summary_pairs <- applicable_pairs[
        applicable_pairs[["slot"]] %in% c("SummaryData", "SummaryTable"),
        ,
        drop = FALSE
      ]
      summary_tables <- do.call(c, lapply(vars_for_each_role[[cvr]],
        function(variable) {
          mapply(
            SIMPLIFY = FALSE,
            alias = summary_pairs[["alias"]],
            slot = summary_pairs[["slot"]],
            FUN = function(alias, slot) {
              if (alias == "") {
                return(NULL)
              }
              dqr <- lapply(
                report[variable, alias, drop = FALSE],
                util_subset_result_slot_for_combine,
                slot = slot
              )
              dqr <- util_combine_res(dqr)
              tb <- dqr[[alias]][[slot]]
              if (!is.data.frame(tb) ||
                  !("Variables" %in% colnames(tb)) ||
                  nrow(tb) == 0) {
                return(NULL)
              }
              rownames(tb) <- NULL
              tb
            }
          )
        }
      ))
      summary_tables <- Filter(Negate(is.null), summary_tables)
      summary_groups <- unique(unlist(lapply(summary_tables, function(table) {
        if (!is.data.frame(table) || !"Variables" %in% colnames(table)) {
          return(character())
        }
        as.character(table[["Variables"]])
      }), use.names = FALSE))
      summary_groups <- summary_groups[
        !is.na(summary_groups) & !util_empty(summary_groups)
      ]
      if (length(summary_tables)) {
        summary_table <- do.call(util_rbind, summary_tables)
        summary_table <- util_generate_pages_restore_data_types(
          summary_table,
          summary_tables
        )
        summary_table <- util_generate_pages_compact_ssi_metric_summary(
          summary_table
        )
        summary_result <- list(ResultData = summary_table)
        attr(summary_result, "call") <- "Variable-group metric summary"
        attr(summary_result, "error") <- list()
        attr(summary_result, "warning") <- list()
        attr(summary_result, "message") <- list()
        summary_content <- util_ssi_render_results(
          setNames(list(summary_result), "ssi_metric_summary"),
          identity,
          ssi_link_target = "cross_item",
          rotate_for_one_row = FALSE
        )
      } else {
        summary_content <- NULL
      }
      if (!is.null(generic_metric_results)) {
        generic_metric_table <- util_html_table(
          generic_metric_results,
          meta_data = meta_data,
          link_variables = FALSE,
          fillContainer = FALSE
        )
        summary_content <- htmltools::tagList(
          summary_content,
          generic_metric_table
        )
      }
      pg_list <- lapply(vars_for_each_role[[cvr]], function(variable) {
        render_pairs <- applicable_pairs[
          !applicable_pairs[["slot"]] %in% c("SummaryData", "SummaryTable"),
          ,
          drop = FALSE
        ]

        # SSI pages start from computed variables: resolve the item variable
        # to its CHECK_ID via item-computation metadata, then use the
        # cross-item metadata to display the scale/check label.
        vg_title <- util_map_labels(
          util_map_labels(
            util_map_labels(
              variable,
              meta_data = meta_data_with_computed_role,
              to = VAR_NAMES,
              from = label_col,
              ifnotfound = NA_character_
            ),
            meta_data = meta_data_item_computation,
            from = VAR_NAMES,
            to = CHECK_ID,
            ifnotfound = NA_character_
          ),
          meta_data = meta_data_cross_item,
          from = CHECK_ID,
          to = CHECK_LABEL,
          ifnotfound = NA_character_
        )
        if (length(vg_title) != 1 || is.na(vg_title) || !nzchar(vg_title)) {
          vg_title <- variable
        }
        variable_content <- mapply(
          SIMPLIFY = FALSE,
          alias = render_pairs[["alias"]],
          slot = render_pairs[["slot"]],
          FUN = function(alias, slot) {
            if (alias == "") {
              return(NULL)
            }
            dqr <- lapply(
              report[variable, alias, drop = FALSE],
              util_subset_result_slot_for_combine,
              slot = slot
            )
            dqr <- util_combine_res(dqr)
            r <- util_ssi_render_results(dqr, identity,
              ssi_link_target = "cross_item")
            # care about memory usage
            rm(dqr)
            gc()
            htmltools::tagList(r)
          }
        )
        variable_content <- Filter(Negate(util_is_empty_html), variable_content)
        if (!length(variable_content)) {
          return(NULL)
        }
        util_generate_pages_ssi_section(
          id = paste0(cvr, ".", vg_title),
          title = vg_title,
          content = htmltools::tagList(variable_content)
        )
      })
      pg_list <- Filter(Negate(is.null), pg_list)
      metric_abbreviations <- unique(
        util_report_scope_variable_group_dqi_metrics(
          ssi_men_lab
        )[["abbreviation"]]
      )
      metric_abbreviations <- metric_abbreviations[
        !is.na(metric_abbreviations) & !util_empty(metric_abbreviations)
      ]
      summary_plot <- if (length(metric_abbreviations)) {
        plot(repsum,
          vars_to_include = "variable_group",
          filter = util_report_scope_normalize_indicator_metrics(
            indicator_metric
          ) %in% metric_abbreviations,
          dont_plot = TRUE,
          disable_plotly = disable_plotly
        )
      } else {
        NULL
      }
      if (util_is_empty_html(summary_plot)) {
        summary_plot <- util_generate_pages_unclassified_group_summary(
          summary_groups,
          use_plotly = have_plot_ly,
          meta_data = meta_data
        )
      }
      if (!length(pg_list) &&
          util_is_empty_html(summary_content) &&
          util_is_empty_html(summary_plot)) {
        return(NULL)
      }
      summary_section <- if (!util_is_empty_html(summary_plot) ||
          !util_is_empty_html(summary_content)) {
        util_generate_pages_ssi_section(
          id = paste0(cvr, ".Summary"),
          title = "Summary",
          content = htmltools::tagList(summary_plot, summary_content)
        )
      }
      page_sections <- util_generate_pages_ssi_sections(c(
        list(summary_section),
        pg_list
      ))
      list(
        VARIABLE_GROUP_REPORT_MENU,
        prep_title_escape(ssi_men_lab, html = TRUE), # Modified title in the Menu drop-down # nolint: line_length_linter.
        paste0("SSIGROUP_", htmltools::urlEncodePath(cvr), ".html"),
        util_ssi_tag_list(
          htmltools::a(id = htmltools::urlEncodePath(as.character(cvr))),
          htmltools::h2(ssi_title),
          htmltools::div(
            class = "infobutton",
            ssi_description),
          page_sections
        )
      )
    } else {
      NULL
    }
  }
  ssi_pages <- lapply(
    setNames(nm = ssi_roles_only),
    util_generate_pages_ssi_metric_page
  )
  # nocov end

  ssi_pages <- ssi_pages[vapply(ssi_pages, length, FUN.VALUE = integer(1))
    != 0]
  ssi_page_div_names <- vapply(ssi_pages, function(args) {
    as.character(args[[2]])
  }, character(1))
  ssi_page_files <- setNames(vapply(ssi_pages, function(args) {
    as.character(args[[3]])
  }, character(1)), ssi_page_div_names)

  progress_msg("Page generation", "Mounting SSI pages by indicators")
  i <- 0
  n <- length(ssi_pages)
  for (args in ssi_pages) {
    do.call(append_single_page, args) # creates the indicator related pages because cur_var has not yet been set # nolint: line_length_linter.
    progress(i / n * 100)
  }

  variable_group_calls <- util_generate_pages_variable_group_calls(
    variable_group_linked_results
  )
  variable_group_call_pages <- lapply(variable_group_calls, function(call) {
    call_results <- util_generate_pages_variable_group_call_results(
      variable_group_linked_results,
      call = call,
      label_col = label_col
    )
    if (is.null(call_results) || !nrow(call_results)) {
      return(NULL)
    }
    call_div_name <- prep_title_escape(call, html = TRUE)
    appends_to_metric_page <- as.character(call_div_name) %in%
      ssi_page_div_names
    if (!appends_to_metric_page) {
      return(NULL)
    }
    list(
      VARIABLE_GROUP_REPORT_MENU,
      call_div_name,
      unname(ssi_page_files[[as.character(call_div_name)]]),
      htmltools::tagList(
        htmltools::h3("Results"),
        util_html_table(
          call_results,
          meta_data = meta_data,
          link_variables = FALSE,
          fillContainer = FALSE
        )
      )
    )
  })
  variable_group_call_pages <- Filter(Negate(is.null),
    variable_group_call_pages)
  for (args in variable_group_call_pages) {
    do.call(append_single_page, args)
  }

  # SSI by variable group #####
  about_scale <- c(
    CHECK_ID,
    CHECK_LABEL,
    SCALE_NAME,
    SCALE_ACRONYM
  )

  missing_cols <- setdiff(about_scale, colnames(meta_data_cross_item))
  if (!!nrow(meta_data_cross_item)) {
    meta_data_cross_item[missing_cols] <- lapply(missing_cols, function(x) NA)
  }

  result_groups_for_report <- util_generate_pages_variable_group_inventory(
    variable_group_page_results,
    meta_data_cross_item,
    label_col = label_col,
    columns = unique(c(about_scale, ssi_roles_only))
  )
  actual_result_groups <-
    util_generate_pages_variable_group_result_inventory(
      report,
      meta_data_cross_item,
      columns = unique(c(about_scale, ssi_roles_only))
    )
  groups_for_report <- util_generate_pages_requested_ssi_groups(
    meta_data_cross_item,
    requested_ssi_roles,
    columns = about_scale
  )
  if (nrow(result_groups_for_report)) {
    groups_for_report <- util_rbind(
      groups_for_report,
      result_groups_for_report
    )
  }
  if (nrow(actual_result_groups)) {
    groups_for_report <- util_rbind(
      groups_for_report,
      actual_result_groups
    )
  }
  if (nrow(groups_for_report)) {
    group_ids <- as.character(groups_for_report[[CHECK_ID]])
    groups_for_report <- groups_for_report[
      !util_empty(group_ids) &
        !duplicated(group_ids),
      ,
      drop = FALSE
    ]
  }

  ssi_concept_info <- util_get_concept_info("ssi")
  mapping_names_labels_ssi <- setNames(
    ssi_concept_info$result_caption,
    ssi_concept_info$SSI_METRICS
  )
  if (!nrow(groups_for_report)) {
    ssi_pages2 <- list()
  } else {
    util_generate_pages_ssi_group_page <- function(rw) {
      check_id <- rw[[CHECK_ID]]
      titles <- util_generate_pages_ssi_cross_item_titles(rw)
      short_title <- titles[["short_title"]]
      long_title <- titles[["long_title"]]
      section_prefix <- short_title
      popup_id <- util_variable_group_popup_id(check_id)
      generic_group_results <- NULL
      generic_group_results_without_roles <- NULL
      all_group_functions <- character()
      generic_group_functions <- character()
      generic_group_roles <- character()
      if (is.data.frame(variable_group_page_results) &&
          nrow(variable_group_page_results)) {
        group_result_sources <- if (prep_is_translated(
          colnames(variable_group_page_results)
        )) {
          util_untranslated_colnames(variable_group_page_results)
        } else {
          colnames(variable_group_page_results)
        }
        group_result_columns <- setNames(
          as.character(colnames(variable_group_page_results)),
          group_result_sources
        )
        if (CHECK_ID %in% group_result_sources && !util_empty(check_id)) {
          result_check_ids <- as.character(variable_group_page_results[[
            group_result_columns[[CHECK_ID]]
          ]])
          group_match <- !util_empty(result_check_ids) &
            result_check_ids == as.character(check_id)
        } else {
          group_match <- rep(FALSE, nrow(variable_group_page_results))
        }
        if ("function_name" %in% group_result_sources) {
          result_functions <- as.character(variable_group_page_results[[
            group_result_columns[["function_name"]]
          ]])
          all_group_functions <- unique(result_functions[group_match])
          all_group_functions <- all_group_functions[
            !util_empty(all_group_functions)
          ]
          group_match <- group_match &
            !result_functions %in%
              util_generate_pages_shared_variable_group_functions()
        }
        generic_result_sources <- c(
          Comparison = ".variable_group_result_label",
          Call = "Call",
          `Indicator Metric` = "Metric",
          Value = "value",
          Classification = "Class",
          .computed_role = COMPUTED_VARIABLE_ROLE
        )
        generic_result_sources <- generic_result_sources[
          generic_result_sources %in% group_result_sources
        ]
        if (any(group_match) && length(generic_result_sources)) {
          if ("function_name" %in% group_result_sources) {
            generic_group_functions <- unique(result_functions[group_match])
            generic_group_functions <- generic_group_functions[
              !util_empty(generic_group_functions)
            ]
          }
          if (COMPUTED_VARIABLE_ROLE %in% group_result_sources) {
            generic_group_roles <- unique(as.character(
              variable_group_page_results[[
                group_result_columns[[COMPUTED_VARIABLE_ROLE]]
              ]][group_match]
            ))
            generic_group_roles <- generic_group_roles[
              !util_empty(generic_group_roles)
            ]
          }
          generic_group_results <- variable_group_page_results[
            group_match,
            unname(group_result_columns[generic_result_sources]),
            drop = FALSE
          ]
          colnames(generic_group_results) <- names(generic_result_sources)
          if (".computed_role" %in% names(generic_group_results)) {
            generic_group_results_without_roles <- generic_group_results[
              util_empty(generic_group_results[[".computed_role"]]),
              setdiff(names(generic_group_results), ".computed_role"),
              drop = FALSE
            ]
            generic_group_results <- generic_group_results[
              , setdiff(names(generic_group_results), ".computed_role"),
              drop = FALSE
            ]
          } else {
            generic_group_results_without_roles <- generic_group_results
          }
          generic_group_results <- unique(generic_group_results)
          generic_group_results_without_roles <- unique(
            generic_group_results_without_roles
          )
        }
        if (is.null(generic_group_results) || !nrow(generic_group_results)) {
          generic_group_results <- NULL
        }
      }
      generic_group_section <- function(results = generic_group_results) {
        if (is.null(results) || !nrow(results)) {
          return(NULL)
        }
        result_table <- util_html_table(
          results,
          meta_data = meta_data,
          link_variables = FALSE,
          fillContainer = FALSE
        )
        result_table <- util_wrap_dqr_result(
          inner = result_table,
          nm = popup_id,
          dqr = NULL,
          errors = character(),
          warnings = character(),
          messages = character()
        )
        util_generate_pages_ssi_section(
          id = paste0(section_prefix, ".Results"),
          title = "Results",
          content = htmltools::tagList(result_table)
        )
      }
      actual_group_results <-
        util_generate_pages_variable_group_actual_results(
          report,
          as.data.frame(rw, stringsAsFactors = FALSE)
        )
      all_group_results <- actual_group_results
      actual_group_results <- Filter(function(result) {
        function_name <- util_attr(
          result,
          "dq_report_function_name",
          exact = TRUE
        )
        !function_name %in% c(
          generic_group_functions,
          util_generate_pages_shared_variable_group_functions()
        )
      }, actual_group_results)
      actual_group_sections <- Map(function(result, result_name) {
        function_name <- util_attr(
          result,
          "dq_report_function_name",
          exact = TRUE
        )
        title <- util_result_function_caption(function_name)
        rendered <- util_pretty_print(
          dqr = result,
          nm = paste0(result_name, ".", popup_id),
          is_single_var = FALSE,
          use_plot_ly = have_plot_ly,
          meta_data = meta_data,
          meta_data_cross_item = meta_data_cross_item,
          label_col = label_col,
          dir = dir,
          popup_nm = popup_id
        )
        if (util_is_empty_html(rendered)) {
          return(NULL)
        }
        util_generate_pages_ssi_section(
          id = paste0(section_prefix, ".", prep_link_escape(function_name)),
          title = title,
          content = htmltools::tagList(rendered)
        )
      }, actual_group_results, names(actual_group_results))
      actual_group_sections <- Filter(Negate(is.null), actual_group_sections)
      computed_vars_for_this_scale_vn <- subset(
        meta_data_item_computation,
        trimws(CHECK_ID) == check_id,
        VAR_NAMES,
        drop = TRUE
      )
      computed_vars_for_this_scale_vn <- unique(
        computed_vars_for_this_scale_vn
      )
      computed_vars_for_this_scale <- util_map_labels(
        computed_vars_for_this_scale_vn,
        meta_data = meta_data,
        to = label_col,
        from = VAR_NAMES,
        ifnotfound = NA,
        warn_ambiguous = FALSE
      )
      computed_vars_for_this_scale <- unique(computed_vars_for_this_scale)
      computed_vars_for_this_scale <- computed_vars_for_this_scale[
        !is.na(computed_vars_for_this_scale)
      ]
      curr_roles <- ssi_roles[computed_vars_for_this_scale]
      keep_ssi_role <- curr_roles %in% ssi_concept_info$SSI_METRICS
      curr_roles <- curr_roles[keep_ssi_role]
      computed_vars_for_this_scale <- computed_vars_for_this_scale[
        keep_ssi_role
      ]
      variable_group_menu <-
        util_generate_pages_variable_group_menu(
          generic_group_functions = all_group_functions,
          actual_group_results = all_group_results,
          has_ssi_results = length(computed_vars_for_this_scale) > 0L ||
            any(generic_group_roles %in% ssi_concept_info$SSI_METRICS)
        )
      group_summary_vars <- unique(as.character(c(
        check_id,
        short_title,
        long_title,
        rw[[CHECK_LABEL]],
        computed_vars_for_this_scale_vn
      )))
      group_summary_vars <- group_summary_vars[!util_empty(group_summary_vars)]
      group_summary_plot <- if (length(group_summary_vars)) {
        plot(repsum,
          vars_to_include = "variable_group",
          filter = VAR_NAMES %in% group_summary_vars,
          dont_plot = TRUE,
          disable_plotly = disable_plotly
        )
      } else {
        NULL
      }
      group_summary_section <- if (util_is_empty_html(group_summary_plot)) {
        NULL
      } else {
        util_generate_pages_ssi_section(
          id = paste0(section_prefix, ".Summary"),
          title = "Summary",
          content = htmltools::tagList(group_summary_plot)
        )
      }
      metadata_content <- util_generate_pages_variable_group_metadata(
        meta_data_cross_item,
        check_id
      )
      metadata_section <- if (util_is_empty_html(metadata_content)) {
        NULL
      } else {
        util_generate_pages_ssi_section(
          id = paste0(section_prefix, ".Metadata"),
          title = "Variable-group metadata",
          content = metadata_content
        )
      }
      generic_group_page <- function() {
        if (is.null(variable_group_menu)) {
          return(NULL)
        }
        result_section <- generic_group_section()
        if (is.null(result_section) && !length(actual_group_sections)) {
          return(NULL)
        }
        page <- util_generate_pages_ssi_sections(c(
          list(group_summary_section),
          list(result_section),
          actual_group_sections,
          list(metadata_section)
        ))
        list(
          variable_group_menu,
          prep_title_escape(short_title, html = TRUE),
          paste0(prep_link_escape(short_title), ".html"),
          util_ssi_tag_list(
            htmltools::h2(long_title),
            page
          )
        )
      }
      if (length(computed_vars_for_this_scale) > 0) {
        applicable_pairs <- util_ssi_applicable_aliases_slots(curr_roles)
        applicable_aliases <- applicable_pairs[["alias"]]
        applicable_slots <- applicable_pairs[["slot"]]
        summary_slots <- applicable_slots %in% c("SummaryData", "SummaryTable")
        summary_pairs <- unique(data.frame(
          alias = applicable_aliases[summary_slots],
          slot = applicable_slots[summary_slots],
          stringsAsFactors = FALSE
        ))
        if (nrow(summary_pairs)) {
          summary_pairs <- summary_pairs[order(match(
            summary_pairs[["slot"]],
            c("SummaryData", "SummaryTable")
          )), , drop = FALSE]
          summary_pairs <- summary_pairs[!duplicated(summary_pairs[["alias"]]),
            , drop = FALSE
          ]
        }

        summary_tables <- mapply(
          SIMPLIFY = FALSE,
          alias = summary_pairs[["alias"]],
          slot = summary_pairs[["slot"]],
          FUN = function(alias, slot) {
            if (alias == "") {
              return(NULL)
            }
            dqr <- report[computed_vars_for_this_scale, alias, drop = FALSE]
            dqr <- lapply(
              dqr,
              util_subset_result_slot_for_combine,
              slot = slot
            )
            dqr <- util_combine_res(dqr)
            tb <- dqr[[alias]][[slot]]
            tb <- util_generate_pages_ssi_result_data(
              tb = tb,
              curr_roles = curr_roles,
              mapping_names_labels_ssi = mapping_names_labels_ssi
            )
            if (is.null(tb)) {
              return(NULL)
            }
            tb
          }
        )
        summary_tables <- Filter(Negate(is.null), summary_tables)
        if (length(summary_tables)) {
          summary_table <- do.call(util_rbind, summary_tables)
          summary_table <- util_generate_pages_restore_data_types(
            summary_table,
            summary_tables
          )
          summary_table <- util_generate_pages_compact_ssi_summary(
            summary_table
          )
          summary_metric_roles <- names(mapping_names_labels_ssi)[
            match(
              summary_table[["Metrics"]],
              mapping_names_labels_ssi
            )
          ]
          summary_table <- util_generate_pages_link_ssi_summary_metrics(
            tb = summary_table,
            metric_roles = summary_metric_roles,
            section_prefix = section_prefix
          )
          summary_result <- list(ResultData = summary_table)
          attr(summary_result, "call") <- "SSI group summary"
          attr(summary_result, "error") <- list()
          attr(summary_result, "warning") <- list()
          attr(summary_result, "message") <- list()
          summary_content <- htmltools::tagList(util_ssi_render_results(
            setNames(list(summary_result), "con_ssi_range_check"),
            identity
          ))
        } else {
          summary_content <- htmltools::tagList()
        }

        detail_sections <- do.call(c, mapply(
          SIMPLIFY = FALSE, # parallel?
          alias = applicable_aliases[!summary_slots],
          slot = applicable_slots[!summary_slots],
          FUN = function(alias, slot) {
            if (alias == "") {
              return(htmltools::HTML(""))
            }
            dqr <- report[computed_vars_for_this_scale, alias, drop = FALSE]
            result_roles <- unique(curr_roles[
              intersect(names(dqr), names(curr_roles))
            ])
            result_roles <- result_roles[result_roles %in%
                names(mapping_names_labels_ssi)]
            result_title <- if (length(result_roles) == 1) {
              mapping_names_labels_ssi[[result_roles]]
            } else {
              util_alias2caption(alias, long = TRUE)
            }
            dqr <- lapply(
              dqr,
              util_subset_result_slot_for_combine,
              slot = slot
            )
            dqr <- util_combine_res(dqr)
            if (slot %in% c("SummaryTable", "SummaryData")) {
              old_slot <- slot
              tb <- dqr[[alias]][[slot]]
              tb <- util_generate_pages_ssi_result_data(
                tb = tb,
                curr_roles = curr_roles,
                mapping_names_labels_ssi = mapping_names_labels_ssi
              )
              if (is.null(tb)) {
                return(htmltools::HTML(""))
              }
              #-> correct names
              slot <- "ResultData"
              dqr[[alias]][[slot]] <- tb
              dqr[[alias]][[old_slot]] <- NULL
            }
            dqr <- lapply(dqr, function(result) {
              dqr_call <- util_attr(result, "call", exact = TRUE)
              entity_name <- util_attr(
                dqr_call,
                "entity_name",
                exact = TRUE
              )
              entity_role <- if (length(entity_name) == 1 &&
                  !is.na(entity_name) &&
                  nzchar(entity_name) &&
                  entity_name %in% names(curr_roles)) {
                curr_roles[[entity_name]]
              } else {
                NA_character_
              }
              has_ssi_role <- !is.na(entity_role) &&
                entity_role %in% names(mapping_names_labels_ssi)
              section_title <- if (has_ssi_role) {
                mapping_names_labels_ssi[[entity_role]]
              } else {
                result_title
              }
              attr(result, "dq_result_title") <- section_title
              attr(result, "dq_result_anchor") <- paste0(
                section_prefix,
                ".",
                if (has_ssi_role) entity_role else section_title
              )
              if (has_ssi_role) {
                attr(result, "dq_result_description") <-
                  util_ssi_metric_description(entity_role)
              }
              result
            })
            r <- util_ssi_render_results(
              dqr,
              function(nm) paste0(nm, ".", popup_id),
              popup_nm = popup_id
            )
            # care about memory usage
            rm(dqr)
            gc()
            r
          }
        ))

        has_rich_content <- !util_is_empty_html(group_summary_plot) ||
          !util_is_empty_html(summary_content) ||
          length(Filter(Negate(util_is_empty_html), detail_sections))
        generic_results <- if (has_rich_content) {
          generic_group_results_without_roles
        } else {
          generic_group_results
        }
        if (!has_rich_content &&
            (is.null(generic_results) || !nrow(generic_results)) &&
            !length(actual_group_sections)) {
          return(NULL)
        }
        detailed_summary <- htmltools::tagList(
          group_summary_plot,
          summary_content
        )
        detailed_summary_section <- if (util_is_empty_html(detailed_summary)) {
          NULL
        } else {
          util_generate_pages_ssi_section(
            id = paste0(section_prefix, ".Summary"),
            title = "Summary",
            content = detailed_summary
          )
        }
        page <- util_generate_pages_ssi_sections(c(
          list(detailed_summary_section),
          detail_sections,
          list(generic_group_section(generic_results)),
          actual_group_sections,
          list(metadata_section)
        ))

        list(
          VARIABLE_GROUP_REPORT_MENU,
          prep_title_escape(short_title, html = TRUE),
          paste0(prep_link_escape(short_title), ".html"),
          util_ssi_tag_list(
            htmltools::h2(long_title),
            page
          )
        )
      } else {
        generic_group_page()
      }
    }
    ssi_pages2 <- lapply(seq_len(nrow(groups_for_report)), function(i) {
      util_generate_pages_ssi_group_page(
        groups_for_report[i, , drop = FALSE]
      )
    })
  }
  ssi_pages2 <- ssi_pages2[vapply(ssi_pages2, length, FUN.VALUE = integer(1))
    != 0]
  if (length(ssi_pages) && length(ssi_pages2)) {
    first_scale_group <- which(vapply(
      ssi_pages2,
      function(page) identical(page[[1]], VARIABLE_GROUP_REPORT_MENU),
      logical(1)
    ))
    if (length(first_scale_group)) {
      ssi_pages2[[first_scale_group[[1]]]][["menu_separator_before"]] <- TRUE
    }
  }

  progress_msg("Page generation", "Mounting SSI pages by variable groups")
  i <- 0
  n <- length(ssi_pages2)
  for (args in ssi_pages2) {
    do.call(append_single_page, args) # creates the indicator related pages because cur_var has not yet been set # nolint: line_length_linter.
    progress(i / n * 100)
  }


  # meta_data ----
  meta_data_frames <- util_report_meta_data_frames(report)
  internal_computed_var_names <-
    util_generate_pages_internal_computed_var_names(
      util_attr(report, "meta_data", exact = TRUE)
    )

  # These alternative names are search aliases for the rendered report and its
  # JavaScript search. They deliberately match the public R argument aliases for
  # usability, but this is not part of the argument-resolution contract.
  meta_data_titles <- list(
    meta_data_segment = util_attach_attr("Segment-level metadata",
      alternative_names =
        util_metadata_level_alternative_names(
          "meta_data_segment"
        )
    ),
    meta_data_dataframe = util_attach_attr("Dataframe-level metadata",
      alternative_names =
        util_metadata_level_alternative_names(
          "meta_data_dataframe"
        )
    ),
    meta_data_cross_item = util_attach_attr("Cross-item-level metadata",
      alternative_names =
        util_metadata_level_alternative_names(
          "meta_data_cross_item"
        )
    ),
    meta_data_cross = util_attach_attr("Cross-item-level metadata",
      alternative_names =
        util_metadata_level_alternative_names(
          "meta_data_cross"
        )
    ),
    meta_data = util_attach_attr("Item-level metadata",
      alternative_names =
        util_metadata_level_alternative_names(
          "meta_data"
        )
    ),
    meta_data_item_computation = util_attach_attr("Item-computation-level metadata", # nolint: line_length_linter.
      alternative_names =
        util_metadata_level_alternative_names(
          "meta_data_item_computation"
        )
    )
  )

  for (mdn in meta_data_frames) {
    xlmd <- util_attr(report, mdn, exact = TRUE) # x level metadata

    if (mdn == "meta_data") {
      # these two columns should have been replaced by the v1->v2 conversion of
      # the metadata in
      # prep_meta_data_v1_to_item_level_meta_data
      # (.util_internal_normalize_meta_data)
      xlmd[[MISSING_LIST]] <- NULL
      xlmd[[JUMP_LIST]] <- NULL

      # there can be specific notes for item-level metadata attributes
      if (length(label_meta_data_hints)) {
        meta_data_h <- force(htmltools::tagList(
          htmltools::div(
            # Keep the metadata hints visually restrained inside the report.
            htmltools::tags$em(htmltools::pre(
              style = htmltools::css(
                `white-space` = "pre-wrap"
              ),
              do.call(
                paste,
                c(
                  list(collapse = "\n"),
                  lapply(lapply(label_meta_data_hints, conditionMessage),
                    paste,
                    collapse = "\n"
                  )
                )
              )
            ))
          )
        ))
        label_meta_data_hints <- meta_data_h
      } else {
        label_meta_data_hints <- NULL
      }
    } else {
      label_meta_data_hints <- NULL
    }

    if (identical(mdn, "meta_data_cross_item") &&
        VARIABLE_LIST %in% colnames(xlmd)) {
      display_col <- "Variable-group items"
      if (display_col %in% colnames(xlmd)) {
        display_col <- "Variable-group items (summary)"
      }
      compact_source <- if (VARIABLE_LIST_ORDER %in% colnames(xlmd)) {
        VARIABLE_LIST_ORDER
      } else {
        VARIABLE_LIST
      }
      xlmd <- data.frame(
        xlmd[, VARIABLE_LIST, drop = FALSE],
        setNames(data.frame(
          util_generate_pages_compact_variable_group_items(
            xlmd[[compact_source]]
          ),
          check.names = FALSE
        ), display_col),
        xlmd[, setdiff(colnames(xlmd), VARIABLE_LIST), drop = FALSE],
        check.names = FALSE
      )
    } else {
      display_col <- character(0)
    }

    xlmd[, !grepl("_TABLE$", colnames(xlmd))] <-
      util_df_escape(xlmd[, !grepl("_TABLE$", colnames(xlmd)),
          drop = FALSE
        ])

    for (cn in grep("_TABLE$", colnames(xlmd), value = TRUE)) {
      xlmd[!util_empty(xlmd[[cn]]), cn] <- vapply(
        FUN.VALUE = character(1),
        xlmd[!util_empty(xlmd[[cn]]), cn, drop = TRUE],
        FUN = function(tn) {
          paste(
            as.character(htmltools::a(href = paste0(
              prep_link_escape(tn, html = TRUE), ".html"
            ), tn)),
            collapse = ""
          )
        }
      )
    }

    # Hover text for headers of metadata tables
    text_to_display <- util_get_hovertext(mdn)

    # filter text_to_display only for headers actually present in the metadata
    text_to_display <- text_to_display[names(text_to_display) %in% names(xlmd)]
    if (length(display_col)) {
      text_to_display[display_col] <-
        "Compact display of the item variables listed in VARIABLE_LIST."
    }

    attr(xlmd, "description") <- text_to_display

    if (mdn == "meta_data") {
      hideCols <- setdiff(
        colnames(xlmd),
        util_get_var_att_names_of_level(
          VARATT_REQUIRE_LEVELS$TECHNICAL
        )
      )
      col_tags <- c(
        list(
          "dataquieR metadata model" = intersect(
            colnames(xlmd),
            util_get_var_att_names_of_level(VARATT_REQUIRE_LEVELS$TECHNICAL)
          ),
          "all" = colnames(xlmd)
        ),
        lapply(
          lapply(setNames(nm = VARATT_REQUIRE_LEVELS_ORDER),
            util_get_var_att_names_of_level,
            cumulative = FALSE
          ),
          function(x) {
            union(VAR_NAMES, x)
          }
        )
      )
      col_tags[VARATT_REQUIRE_LEVELS_ORDER] <-
        lapply(col_tags[VARATT_REQUIRE_LEVELS_ORDER], function(x) {
          util_attach_attr(x, cssClass = "dq-hidden-col")
        })
      col_tags
    } else {
      known_cols <- colnames(xlmd)[vapply(colnames(xlmd),
          exists,
          FUN.VALUE = logical(1)
        )]
      hideCols <- setdiff(colnames(xlmd), known_cols)
      if (identical(mdn, "meta_data_cross_item")) {
        hideCols <- setdiff(
          union(hideCols, c(VARIABLE_LIST, "Variables", "Labels")),
          display_col
        )
        known_cols <- union(
          display_col,
          setdiff(known_cols, c(VARIABLE_LIST, "Variables", "Labels"))
        )
      }
      col_tags <- list(
        "dataquieR metadata model" = known_cols,
        "all" = colnames(xlmd)
      )
    }

    internal_computed_metadata <-
      mdn %in% c("meta_data", "meta_data_item_computation") &&
      length(internal_computed_var_names) > 0 &&
      VAR_NAMES %in% colnames(xlmd)
    fixed_metadata_table <-
      mdn %in% c("meta_data", "meta_data_item_computation")
    meta_data_table <- util_html_table(
      xlmd, # generate links in VAR_NAMES/Variables, only possible if meta_data and label_col are available # nolint: line_length_linter.
      meta_data = meta_data,
      meta_data_cross_item = meta_data_cross_item,
      label_col = label_col,
      dl_fn = mdn,
      hideCols = hideCols,
      col_tags = col_tags,
      fixed_header = fixed_metadata_table,
      options = if (fixed_metadata_table) {
        list(
          scrollX = FALSE,
          scrollY = "",
          scrollCollapse = FALSE,
          fixedColumns = FALSE
        )
      } else {
        list()
      }
    )
    if (internal_computed_metadata) {
      meta_data_table <- util_generate_pages_internal_computed_toggle(
        meta_data_table,
        internal_computed_var_names
      )
    }

    tmpl <- system.file("templates", template, paste0(mdn, ".html"),
      package = utils::packageName()
    )
    if (!file.exists(tmpl)) {
      tmpl <- system.file("templates", template,
        paste0("generic_meta_data.html"),
        package = utils::packageName()
      )
    }

    meta_data_title <- meta_data_titles[[mdn]]
    if (length(meta_data_title) != 1 ||
        !is.character(meta_data_title) ||
        util_empty(meta_data_title)) {
      meta_data_title <- mdn
    }

    if (length(meta_data_table) == 0) {
      meta_data_table <- htmltools::p(paste(
        meta_data_title,
        "were not provided."
      ))
    }

    append_single_page(
      "Metadata",
      meta_data_title,
      paste0(mdn, ".html"),
      htmltools::htmlTemplate(
        tmpl,
        meta_data_name = mdn,
        meta_data_title =
          # Use upper case for all words, not only the first one
          gsub("(^|[[:space:]]|[[:punct:]])([[:alpha:]])",
            "\\1\\U\\2",
            meta_data_title,
            perl = TRUE
          ),
        label_meta_data_hints = label_meta_data_hints,
        meta_data_table = util_apply_full_page_table_class(meta_data_table)
      )
    )
  }

  referred_tables <- util_attr(report, "referred_tables", exact = TRUE)
  settings_table_names <- c(
    "statistical_settings",
    "repeated_measurement_settings"
  )
  settings_table_name <- intersect(settings_table_names, names(referred_tables))
  statistical_settings <- NULL
  if (length(settings_table_name)) {
    stored_settings <- referred_tables[[settings_table_name[[1]]]]
    stored_names <- tolower(names(stored_settings))
    if (is.data.frame(stored_settings) && "setting_id" %in% stored_names) {
      statistical_settings <-
        util_repeated_measurement_settings(stored_settings)
    }
  }
  referred_tables[c(
    "statistical_settings",
    "repeated_measurement_settings"
  )] <- NULL
  if (length(referred_tables)) { # show all tables referred to by the report
    for (reftab in names(referred_tables)) {
      ref_table_fixed <- referred_tables[[reftab]]
      ref_table_fixed <- util_fix_datatype_for_table(ref_table_fixed) # to add the columns' data types # nolint: line_length_linter.
      if (nrow(ref_table_fixed) > 1000) {
        append_single_page(
          "Metadata",
          util_attach_attr(paste("Table", sQuote(reftab)),
            alternative_names = c(reftab)
          ),
          paste0(prep_link_escape(reftab, html = TRUE), ".html"),
          htmltools::h1(dQuote(reftab)),
          util_html_table(
            util_df_escape(head(ref_table_fixed, 1000)), # generate links in VAR_NAMES/Variables, only possible if meta_data and label_col are available # nolint: line_length_linter.
            meta_data = meta_data,
            label_col = label_col,
            dl_fn = reftab
          ),
          htmltools::tags$hr(),
          htmltools::p(
            sprintf(
              "Showing only the first 1000 rows of a table with %d rows",
              nrow(ref_table_fixed)
            )
          )
        )
      } else {
        append_single_page(
          "Metadata",
          util_attach_attr(paste("Table", sQuote(reftab)),
            alternative_names = c(reftab)
          ),
          paste0(prep_link_escape(reftab, html = TRUE), ".html"),
          htmltools::h1(dQuote(reftab)),
          util_html_table(
            util_df_escape(ref_table_fixed), # generate links in VAR_NAMES/Variables, only possible if meta_data and label_col are available # nolint: line_length_linter.
            meta_data = meta_data,
            label_col = label_col,
            dl_fn = reftab
          )
        )
      }
    }
  }

  if (is.data.frame(statistical_settings)) {
    names(statistical_settings) <- toupper(names(statistical_settings))
    statistical_settings <- util_df_escape(statistical_settings)
    attr(statistical_settings, "description") <-
      util_get_hovertext("statistical_settings")
    append_single_page(
      "Metadata",
      util_attach_attr("Statistical settings",
        alternative_names = settings_table_names
      ),
      "statisticalsettings.html",
      htmltools::htmlTemplate(
        system.file("templates", template, "statistical_settings.html",
          package = utils::packageName()
        ),
        meta_data_name = "statistical_settings",
        meta_data_title = "Statistical Settings",
        label_meta_data_hints = NULL,
        meta_data_table = util_apply_full_page_table_class(util_html_table(
          statistical_settings,
          dl_fn = "statistical_settings"
        ))
      )
    )
  }


  rsts <- util_get_rule_sets()
  for (rstsn in names(rsts)) {
    ..tb <- util_df_escape(rsts[[rstsn]])
    ..tb[[GRADING_RULESET]] <-
      ifelse(..tb[[GRADING_RULESET]] == "0",
        "Default (0)",
        paste0("(", ..tb[[GRADING_RULESET]], ")")
      )
    if ("indicator_metric" %in% colnames(..tb)) {
      ..tb[["indicator_metric"]] <- lapply(
        ..tb[["indicator_metric"]],
        function(im) {
          as.character(
            htmltools::span(
              title = im,
              util_translate_indicator_metrics(im)
            )
          )
        }
      )
    }

    # add hover text to headers of table Grading ruleset
    text_to_display <- util_get_hovertext("grading_rulesets")
    attr(..tb, "description") <- text_to_display

    append_single_page(
      "Metadata",
      util_attach_attr(paste("Grading Ruleset", dQuote(rstsn)),
        alternative_names = c(rstsn)
      ),
      paste0("rulesets.html"),
      htmltools::htmlTemplate(
        system.file("templates", template,
          paste0("grading_ruleset_text.html"),
          package = utils::packageName()
        ),
        meta_data_name = paste0("grading_rulesets_", rstsn),
        meta_data_title =
          paste("Grading Rulesets", rstsn),
        meta_data_table = util_apply_full_page_table_class(util_html_table(..tb,
            dl_fn = paste0("grading_rulesets_", rstsn)
          ))
      )
    )
  }

  ..tb <- util_df_escape(util_get_ruleset_formats())
  if ("color" %in% colnames(..tb)) {
    ..tb[["color"]] <- lapply(
      ..tb[["color"]],
      function(cl) {
        as.character(
          htmltools::span(
            style = htmltools::css(
              background_color =
                util_col2rgb(cl),
              color =
                util_get_fg_color(
                  util_col2rgb(cl)
                )
            ),
            cl
          )
        )
      }
    )
  }

  # add hover text to headers of table Ruleset formats
  text_to_display <- util_get_hovertext("grading_formats")
  attr(..tb, "description") <- text_to_display


  append_single_page(
    "Metadata",
    util_attach_attr(paste("Ruleset Formats"),
      alternative_names = c("grading_formats")
    ),
    paste0("ruleset_formats.html"),
    htmltools::htmlTemplate(
      system.file("templates", template,
        paste0("ruleset_formats_text.html"),
        package = utils::packageName()
      ),
      meta_data_name = "grading_formats",
      meta_data_title =
        "Ruleset Formats",
      meta_data_table =
        util_apply_full_page_table_class(util_html_table(..tb,
            dl_fn = "grading_formats"
          ))
    )
  )


  pages
}

#' Render an explicitly unclassified variable-group summary
#'
#' @param groups Character vector of variable-group labels.
#' @param use_plotly Whether to render the interactive Plotly variant.
#' @param meta_data Metadata passed to the summary renderer.
#'
#' @return An HTML-compatible pie chart or `NULL` for no groups.
#' @noRd
util_generate_pages_unclassified_group_summary <- function(
  groups,
  use_plotly,
  meta_data
) {
  groups <- unique(as.character(groups))
  groups <- groups[!is.na(groups) & !util_empty(groups)]
  if (!length(groups)) {
    return(NULL)
  }
  plot_data <- data.frame(
    X = sprintf(
      "0 of %d available variable groups classified",
      length(groups)
    ),
    class = NA_integer_,
    value = length(groups),
    note = paste(groups, collapse = "<br>"),
    stringsAsFactors = FALSE
  )
  attr(plot_data, "vars_to_include") <- "variable_group"
  if (use_plotly) {
    prep_render_pie_chart_from_summaryclasses_plotly(
      plot_data,
      meta_data = meta_data
    )
  } else {
    prep_render_pie_chart_from_summaryclasses_ggplot2(
      plot_data,
      meta_data = meta_data
    )
  }
}

#' Resolve displayed and source columns of a variable-group result table
#'
#' @param result_table A dashboard-style variable-group result table.
#'
#' @return A named character vector mapping source to displayed columns.
#' @noRd
util_generate_pages_variable_group_columns <- function(result_table) {
  if (!is.data.frame(result_table)) {
    return(character())
  }
  result_columns <- as.character(colnames(result_table))
  source_columns <- if (prep_is_translated(colnames(result_table))) {
    util_untranslated_colnames(result_table)
  } else {
    result_columns
  }
  setNames(result_columns, as.character(source_columns))
}

#' Find computed-variable roles represented in variable-group results
#'
#' @param result_table A dashboard-style variable-group result table.
#'
#' @return A character vector of known computed-variable roles.
#' @noRd
util_generate_pages_variable_group_roles <- function(result_table) {
  columns <- util_generate_pages_variable_group_columns(result_table)
  if (!COMPUTED_VARIABLE_ROLE %in% names(columns)) {
    return(character())
  }
  roles <- as.character(result_table[[columns[[COMPUTED_VARIABLE_ROLE]]]])
  unique(roles[roles %in% COMPUTED_VARIABLE_ROLES])
}

#' Find report calls represented by non-SSI variable-group results
#'
#' @param result_table A dashboard-style variable-group result table.
#'
#' @return A character vector of displayed report-call names.
#' @noRd
util_generate_pages_variable_group_calls <- function(result_table) {
  columns <- util_generate_pages_variable_group_columns(result_table)
  if (!"Call" %in% names(columns)) {
    return(character())
  }
  calls <- as.character(result_table[[columns[["Call"]]]])
  keep <- !util_empty(calls)
  if ("function_name" %in% names(columns)) {
    functions <- as.character(result_table[[columns[["function_name"]]]])
    keep <- keep & !functions %in% c(
      "con_ssi_range_check",
      "con_contradictions_redcap"
    )
  }
  unique(calls[keep])
}

#' Find implementations represented by one variable-group report call
#'
#' @param result_table A dashboard-style variable-group result table.
#' @param call A displayed report-call name.
#'
#' @return A character vector of implementation names.
#' @noRd
util_generate_pages_variable_group_call_functions <- function(
  result_table,
  call
) {
  columns <- util_generate_pages_variable_group_columns(result_table)
  if (!all(c("Call", "function_name") %in% names(columns))) {
    return(character())
  }
  call_rows <- as.character(result_table[[columns[["Call"]]]]) == call
  functions <- unique(as.character(result_table[[
    columns[["function_name"]]
  ]][call_rows]))
  functions[!util_empty(functions)]
}

#' Select variable-group results for one report call
#'
#' @param result_table A dashboard-style variable-group result table.
#' @param call A displayed report-call name.
#' @param label_col The metadata label-column name.
#'
#' @return A display-ready data frame or `NULL`.
#' @noRd
util_generate_pages_variable_group_call_results <- function(
  result_table,
  call,
  label_col
) {
  util_generate_pages_variable_group_result_rows(
    result_table = result_table,
    selector_column = "Call",
    selector_value = call,
    label_col = label_col,
    excluded_functions = c(
      "con_ssi_range_check",
      "con_contradictions_redcap"
    )
  )
}

#' Select variable-group results for one computed-variable role
#'
#' @param result_table A dashboard-style variable-group result table.
#' @param role A computed-variable role.
#' @param label_col The metadata label-column name.
#'
#' @return A display-ready data frame or `NULL`.
#' @noRd
util_generate_pages_variable_group_results <- function(
  result_table,
  role,
  label_col
) {
  util_generate_pages_variable_group_result_rows(
    result_table = result_table,
    selector_column = COMPUTED_VARIABLE_ROLE,
    selector_value = role,
    label_col = label_col
  )
}

#' Select display-ready rows from variable-group results
#'
#' @param result_table A dashboard-style variable-group result table.
#' @param selector_column The source column used to select rows.
#' @param selector_value The value to select.
#' @param label_col The metadata label-column name.
#' @param excluded_functions Implementations to omit from the selection.
#'
#' @return A display-ready data frame or `NULL`.
#' @noRd
util_generate_pages_variable_group_result_rows <- function(
  result_table,
  selector_column,
  selector_value,
  label_col,
  excluded_functions = character()
) {
  if (!is.data.frame(result_table) || !nrow(result_table)) {
    return(NULL)
  }
  columns <- util_generate_pages_variable_group_columns(result_table)
  if (!selector_column %in% names(columns)) {
    return(NULL)
  }
  selector <- as.character(result_table[[columns[[selector_column]]]])
  keep <- selector == selector_value & !is.na(selector)
  if (length(excluded_functions) && "function_name" %in% names(columns)) {
    functions <- as.character(result_table[[columns[["function_name"]]]])
    keep <- keep & !functions %in% excluded_functions
  }
  if (!any(keep)) {
    return(NULL)
  }
  requested <- c(
    `Variable group` = label_col,
    Comparison = ".variable_group_result_label",
    Call = "Call",
    `Indicator Metric` = "Metric",
    Value = "value",
    Classification = "Class"
  )
  requested <- requested[requested %in% names(columns)]
  if (!length(requested)) {
    return(NULL)
  }
  result <- result_table[
    keep,
    unname(columns[requested]),
    drop = FALSE
  ]
  colnames(result) <- names(requested)
  unique(result)
}

#' Build the inventory of variable groups represented in report results
#'
#' @param result_table A dashboard-style variable-group result table.
#' @param meta_data_cross_item Normalized cross-item metadata.
#' @param label_col The metadata label-column name.
#' @param columns Cross-item metadata columns to retain.
#'
#' @return One row per stable variable-group identifier.
#' @noRd
util_generate_pages_variable_group_inventory <- function(
  result_table,
  meta_data_cross_item,
  label_col,
  columns = c(CHECK_ID, CHECK_LABEL, SCALE_NAME, SCALE_ACRONYM)
) {
  columns <- unique(c(CHECK_ID, CHECK_LABEL, columns))
  empty_inventory <- function() {
    as.data.frame(
      setNames(rep(list(character()), length(columns)), columns),
      stringsAsFactors = FALSE
    )
  }
  if (!is.data.frame(result_table) || !nrow(result_table)) {
    return(empty_inventory())
  }

  display_columns <- util_generate_pages_variable_group_columns(result_table)
  if (!CHECK_ID %in% names(display_columns)) {
    return(empty_inventory())
  }
  id_column <- display_columns[[CHECK_ID]]
  label_column <- if (label_col %in% names(display_columns)) {
    display_columns[[label_col]]
  } else {
    NULL
  }
  group_ids <- as.character(result_table[[id_column]])
  group_labels <- if (is.null(label_column)) {
    rep(NA_character_, length(group_ids))
  } else {
    as.character(result_table[[label_column]])
  }
  keep <- !util_empty(group_ids)
  if (!any(keep)) {
    return(empty_inventory())
  }
  group_ids <- group_ids[keep]
  group_labels <- group_labels[keep]
  groups <- do.call(util_rbind, lapply(unique(group_ids), function(group_id) {
    labels <- group_labels[group_ids == group_id]
    labels <- labels[!util_empty(labels)]
    data.frame(
      id = group_id,
      label = if (length(labels)) labels[[1]] else NA_character_,
      stringsAsFactors = FALSE
    )
  }))

  cross_item <- meta_data_cross_item
  if (!is.data.frame(cross_item)) {
    cross_item <- empty_inventory()
  }
  missing_columns <- setdiff(columns, colnames(cross_item))
  cross_item[missing_columns] <- lapply(
    missing_columns,
    function(...) rep(NA_character_, nrow(cross_item))
  )

  rows <- lapply(seq_len(nrow(groups)), function(i) {
    group_id <- groups[["id"]][[i]]
    group_label <- groups[["label"]][[i]]
    match_id <- which(as.character(cross_item[[CHECK_ID]]) == group_id)
    if (length(match_id)) {
      row <- cross_item[match_id[[1]], columns, drop = FALSE]
    } else {
      row <- as.data.frame(
        setNames(rep(list(NA_character_), length(columns)), columns),
        stringsAsFactors = FALSE
      )
    }
    row[[CHECK_ID]] <- group_id
    if (util_empty(row[[CHECK_LABEL]]) && !util_empty(group_label)) {
      row[[CHECK_LABEL]] <- group_label
    }
    row
  })
  do.call(util_rbind, rows)
}

#' Extract result objects for one report function
#'
#' @param report A report result set.
#' @param function_name Name of a function represented by a report column.
#'
#' @return A list of non-empty [dataquieR_result] objects.
#' @noRd
util_generate_pages_function_results <- function(report, function_name) {
  if (is.null(report) || !function_name %in% colnames(report)) {
    return(list())
  }
  results <- report[, function_name, drop = TRUE]
  if (inherits(results, "dataquieR_result")) {
    results <- list(results)
  }
  Filter(function(result) {
    inherits(result, "dataquieR_result") &&
      !inherits(result, "dataquieR_NULL") &&
      length(result) > 0L
  }, results)
}

#' Match one result object to normalized variable-group metadata
#'
#' @param result A [dataquieR_result] object.
#' @param meta_data_cross_item Normalized cross-item metadata.
#'
#' @return Stable `CHECK_ID` values represented by `result`.
#' @noRd
util_generate_pages_result_group_ids <- function(
  result,
  meta_data_cross_item
) {
  if (!inherits(result, "dataquieR_result") ||
      !is.data.frame(meta_data_cross_item) ||
      !nrow(meta_data_cross_item)) {
    return(character())
  }
  if (!CHECK_ID %in% colnames(meta_data_cross_item)) {
    return(character())
  }
  known_ids <- as.character(meta_data_cross_item[[CHECK_ID]])
  candidates <- character()
  result_call <- util_attr(result, "call", exact = TRUE)
  candidates <- c(
    candidates,
    util_attr(result_call, CHECK_ID, exact = TRUE),
    util_attr(result, CHECK_ID, exact = TRUE)
  )
  for (slot in names(result)) {
    value <- result[[slot]]
    if (is.data.frame(value) && CHECK_ID %in% colnames(value)) {
      candidates <- c(candidates, as.character(value[[CHECK_ID]]))
    } else if (is.list(value) && endsWith(slot, "VariableGroupPlotList")) {
      plot_names <- names(value) %||% rep("", length(value))
      candidates <- c(
        candidates,
        plot_names[plot_names %in% known_ids],
        vapply(value, function(plot) {
          util_attr(plot, CHECK_ID, exact = TRUE) %||% NA_character_
        }, character(1))
      )
    }
  }
  candidates <- unique(trimws(as.character(candidates)))
  candidates <- candidates[!util_empty(candidates)]
  known_ids[known_ids %in% candidates]
}

#' Build a variable-group inventory from actual report results
#'
#' @param report A report result set.
#' @param meta_data_cross_item Normalized cross-item metadata.
#' @param columns Cross-item metadata columns to retain.
#'
#' @return One metadata row per represented variable group.
#' @noRd
util_generate_pages_variable_group_result_inventory <- function(
  report,
  meta_data_cross_item,
  columns = c(CHECK_ID, CHECK_LABEL, SCALE_NAME, SCALE_ACRONYM)
) {
  columns <- intersect(unique(c(CHECK_ID, CHECK_LABEL, columns)),
    colnames(meta_data_cross_item)
  )
  if (!length(columns) || !nrow(meta_data_cross_item)) {
    return(meta_data_cross_item[FALSE, columns, drop = FALSE])
  }
  functions <- intersect(
    util_report_scope_target_functions("variable_group"),
    colnames(report)
  )
  functions <- functions[vapply(
    functions,
    util_report_scope_function_is_applicable,
    logical(1),
    report = report
  )]
  group_ids <- unique(unlist(lapply(functions, function(function_name) {
    results <- util_generate_pages_function_results(report, function_name)
    unlist(lapply(
      results,
      util_generate_pages_result_group_ids,
      meta_data_cross_item = meta_data_cross_item
    ), use.names = FALSE)
  }), use.names = FALSE))
  group_ids <- group_ids[!util_empty(group_ids)]
  meta_data_cross_item[
    as.character(meta_data_cross_item[[CHECK_ID]]) %in% group_ids,
    columns,
    drop = FALSE
  ]
}

#' Restrict a result object to one variable group
#'
#' @param result A [dataquieR_result] object.
#' @param group_row One normalized cross-item metadata row.
#'
#' @return The restricted result, or `NULL` if it does not represent the group.
#' @noRd
util_generate_pages_variable_group_result_subset <- function(
  result,
  group_row
) {
  if (!is.data.frame(group_row) || nrow(group_row) != 1L ||
      !CHECK_ID %in% colnames(group_row) || util_empty(group_row[[CHECK_ID]])) {
    return(NULL)
  }
  group_id <- as.character(group_row[[CHECK_ID]])
  result_call <- util_attr(result, "call", exact = TRUE)
  result_ids <- trimws(as.character(c(
    util_attr(result_call, CHECK_ID, exact = TRUE),
    util_attr(result, CHECK_ID, exact = TRUE)
  )))
  if (group_id %in% result_ids) {
    attr(result, CHECK_ID) <- group_id
    if (CHECK_LABEL %in% colnames(group_row) &&
        !util_empty(group_row[[CHECK_LABEL]])) {
      attr(result, CHECK_LABEL) <- as.character(group_row[[CHECK_LABEL]])
    }
    return(result)
  }

  restricted <- result
  matched <- FALSE
  for (slot in names(result)) {
    value <- result[[slot]]
    if (is.data.frame(value)) {
      keep <- if (CHECK_ID %in% colnames(value)) {
        trimws(as.character(value[[CHECK_ID]])) %in% group_id
      } else {
        rep(FALSE, nrow(value))
      }
      if (any(keep)) {
        restricted[[slot]] <- value[keep, , drop = FALSE]
        matched <- TRUE
      } else {
        restricted[[slot]] <- NULL
      }
    } else if (is.list(value) && endsWith(slot, "VariableGroupPlotList")) {
      plot_names <- names(value) %||% rep("", length(value))
      plot_ids <- vapply(value, function(plot) {
        util_attr(plot, CHECK_ID, exact = TRUE) %||% NA_character_
      }, character(1))
      keep <- trimws(plot_names) %in% group_id | plot_ids %in% group_id
      if (any(keep)) {
        restricted[[slot]] <- value[keep]
        matched <- TRUE
      } else {
        restricted[[slot]] <- NULL
      }
    } else {
      restricted[[slot]] <- NULL
    }
  }
  if (!matched || !length(restricted)) {
    return(NULL)
  }
  attr(restricted, CHECK_ID) <- group_id
  if (CHECK_LABEL %in% colnames(group_row)) {
    attr(restricted, CHECK_LABEL) <- as.character(group_row[[CHECK_LABEL]])
  }
  restricted
}

#' Collect actual report results for one variable group
#'
#' @param report A report result set.
#' @param group_row One normalized cross-item metadata row.
#'
#' @return Named list of group-specific result objects.
#' @noRd
util_generate_pages_variable_group_actual_results <- function(
  report,
  group_row
) {
  functions <- intersect(
    util_report_scope_target_functions("variable_group"),
    colnames(report)
  )
  results <- unlist(lapply(functions, function(function_name) {
    function_results <- util_generate_pages_function_results(
      report,
      function_name
    )
    function_results <- Filter(Negate(is.null), lapply(
      function_results,
      util_generate_pages_variable_group_result_subset,
      group_row = group_row
    ))
    lapply(function_results, function(result) {
      attr(result, "dq_report_function_name") <- function_name
      result
    })
  }), recursive = FALSE)
  if (!length(results)) {
    return(list())
  }
  names(results) <- make.unique(vapply(results, function(result) {
    util_attr(result, "dq_report_function_name", exact = TRUE)
  }, character(1)))
  results
}

#' List variable-group functions rendered on shared dimension pages
#'
#' @return Function names that must not be repeated on concrete group pages.
#' @noRd
util_generate_pages_shared_variable_group_functions <- function() {
  c(
    "con_contradictions",
    "con_contradictions_redcap",
    "des_scatterplot_matrix"
  )
}

#' Select the report menu for a variable-group page
#'
#' SSI results and other variable-group analyses share one report menu. A page
#' is omitted when all its functions are already represented on a shared
#' dimension page.
#'
#' @param generic_group_functions Function names represented by dashboard rows.
#' @param actual_group_results Group-specific result objects.
#' @param has_ssi_results Whether the group contributes SSI results.
#'
#' @return A menu label or `NULL` for a group represented elsewhere.
#' @noRd
util_generate_pages_variable_group_menu <- function(
  generic_group_functions,
  actual_group_results,
  has_ssi_results
) {
  if (isTRUE(has_ssi_results)) {
    return(VARIABLE_GROUP_REPORT_MENU)
  }
  actual_functions <- vapply(actual_group_results, function(result) {
    util_attr(result, "dq_report_function_name", exact = TRUE) %||% ""
  }, character(1))
  functions <- unique(c(generic_group_functions, actual_functions))
  functions <- functions[!util_empty(functions)]
  shared_page_functions <-
    util_generate_pages_shared_variable_group_functions()
  if (length(functions) && all(functions %in% shared_page_functions)) {
    return(NULL)
  }
  VARIABLE_GROUP_REPORT_MENU
}

#' Select variable groups with requested SSI calculations
#'
#' @param meta_data_cross_item Normalized cross-item metadata.
#' @param requested_ssi_roles Computed roles requested by the report.
#' @param columns Additional metadata columns to retain.
#'
#' @return Cross-item rows that request at least one selected role.
#'
#' @noRd
util_generate_pages_requested_ssi_groups <- function(
  meta_data_cross_item,
  requested_ssi_roles,
  columns = c(CHECK_ID, CHECK_LABEL, SCALE_NAME, SCALE_ACRONYM)
) {
  requested_ssi_roles <- intersect(
    requested_ssi_roles,
    colnames(meta_data_cross_item)
  )
  columns <- intersect(
    unique(c(columns, requested_ssi_roles)),
    colnames(meta_data_cross_item)
  )
  if (!nrow(meta_data_cross_item) || !length(requested_ssi_roles)) {
    return(meta_data_cross_item[FALSE, columns, drop = FALSE])
  }
  requested <- vapply(seq_len(nrow(meta_data_cross_item)), function(i) {
    any(vapply(requested_ssi_roles, function(role) {
      any(!util_empty(meta_data_cross_item[[role]][[i]]))
    }, logical(1)))
  }, logical(1))
  meta_data_cross_item[requested, columns, drop = FALSE]
}

#' Format compact item lists for variable-group metadata
#'
#' @param x A vector of normalized variable-list strings.
#' @param n_max Maximum number of item names to display.
#'
#' @return A character vector of compact item lists.
#' @noRd
util_generate_pages_compact_variable_group_items <- function(x,
  n_max = 12) {
  vapply(x, function(variable_list) {
    items <- util_generate_pages_variable_group_items(variable_list)
    if (length(items) > n_max) {
      items <- c(head(items, n_max), "...")
    }
    if (!length(items)) {
      return("")
    }
    prep_deparse_assignments(
      codes = items,
      labels = character(),
      split_char = SPLIT_CHAR,
      mode = "string_codes"
    )
  }, FUN.VALUE = character(1))
}

#' Normalize serialized assignment lists for report display
#'
#' @param x Serialized metadata lists.
#'
#' @return A character vector with one space around each [SPLIT_CHAR].
#' @noRd
util_generate_pages_normalize_assignment_lists <- function(x) {
  vapply(as.character(x), function(value) {
    if (is.na(value) || !grepl(SPLIT_CHAR, value, fixed = TRUE)) {
      return(value)
    }
    if (util_empty(gsub(SPLIT_CHAR, "", value, fixed = TRUE))) {
      return(SPLIT_CHAR)
    }
    items <- trimws(strsplit(value, SPLIT_CHAR, fixed = TRUE)[[1]])
    prep_deparse_assignments(
      codes = items,
      labels = character(),
      split_char = SPLIT_CHAR,
      mode = "string_codes"
    )
  }, character(1), USE.NAMES = FALSE)
}

#' Extract item names from one normalized variable-list value
#'
#' @param variable_list A normalized cross-item variable-list value.
#'
#' @return A character vector of item names.
#' @noRd
util_generate_pages_variable_group_items <- function(variable_list) {
  if (length(variable_list) != 1 || is.na(variable_list) ||
      util_empty(variable_list)) {
    return(character(0))
  }
  parts <- strsplit(
    as.character(variable_list),
    SPLIT_CHAR,
    fixed = TRUE
  )[[1]]
  parts <- trimws(gsub("<br\\s*/?>", "\n", parts, perl = TRUE))
  parts <- sub(":.*$", "", parts, perl = TRUE)
  parts <- sub("[\r\n].*$", "", parts, perl = TRUE)
  parts <- trimws(parts)
  parts <- parts[nzchar(parts)]
  unique(parts)
}

#' Render metadata for one variable group
#'
#' @param meta_data_cross_item Normalized cross-item metadata.
#' @param check_id Stable variable-group identifier.
#'
#' @return An HTML metadata table or `NULL`.
#'
#' @noRd
util_generate_pages_variable_group_metadata <- function(
  meta_data_cross_item,
  check_id
) {
  if (!is.data.frame(meta_data_cross_item) ||
      util_empty(check_id)) {
    return(NULL)
  }
  rows <- meta_data_cross_item[
    as.character(meta_data_cross_item[[CHECK_ID]]) == as.character(check_id),
    , drop = FALSE
  ]
  if (!nrow(rows)) {
    return(NULL)
  }
  rows <- rows[1, !startsWith(colnames(rows), "."), drop = FALSE]
  if (VARIABLE_LIST %in% colnames(rows)) {
    rows[[VARIABLE_LIST]] <- util_generate_pages_compact_variable_group_items(
      rows[[VARIABLE_LIST]]
    )
  }
  list_columns <- intersect(colnames(rows), c(
    VARIABLE_LIST_ORDER,
    DATA_PREPARATION,
    CONTRADICTION_TYPE,
    MULTIVARIATE_OUTLIER_CHECKTYPE
  ))
  rows[list_columns] <- lapply(
    rows[list_columns],
    util_generate_pages_normalize_assignment_lists
  )
  values <- vapply(rows, function(value) {
    paste(as.character(value), collapse = ", ")
  }, character(1))
  util_html_table(
    util_df_escape(data.frame(
      Name = colnames(rows),
      Value = values,
      stringsAsFactors = FALSE
    )),
    copy_row_names_to_column = FALSE,
    dl_fn = paste0("Variable-group_Metadata_", check_id),
    fillContainer = FALSE
  )
}

#' Find automatically generated variables in item-level metadata
#'
#' @param meta_data Item-level metadata.
#'
#' @return A character vector of internal computed-variable names.
#' @noRd
util_generate_pages_internal_computed_var_names <- function(meta_data) {
  if (!is.data.frame(meta_data) ||
      !all(c(VAR_NAMES, STUDY_SEGMENT) %in% colnames(meta_data))) {
    return(character(0))
  }
  meta_data[[VAR_NAMES]][
    !is.na(meta_data[[STUDY_SEGMENT]]) &
      meta_data[[STUDY_SEGMENT]] == ".COMPUTED__ssi"
  ]
}

#' Add the internal-computed-variable toggle to a metadata table
#'
#' @param table A data-table htmlwidget or a nested object containing one.
#' @param internal_computed_var_names Names hidden until the toggle is enabled.
#'
#' @return The wrapped metadata table.
#' @noRd
util_generate_pages_internal_computed_toggle <- function(
  table,
  internal_computed_var_names
) {
  wrapper_id <- "dq-internal-computed-metadata"
  toggle_id <- "dq-internal-computed-toggle"
  toggle_button <- list(
    text = paste0(
      "<label class=\"dq-internal-computed-toggle\" for=\"",
      toggle_id,
      "\"><input type=\"checkbox\" id=\"",
      toggle_id,
      "\"> Show automatically generated variables</label>"
    ),
    className = "dq-internal-computed-toggle-control",
    action = util_html_table_js(paste0(
      "function(e, dt, node, config) {",
      "dataquieRInternalComputedToggleAction(e, dt, node, config);",
      "}"
    )),
    init = util_html_table_js(paste0(
      "function(dt, node, config) {",
      "dataquieRInternalComputedToggleInit(dt, node, config);",
      "}"
    )),
    internalNames = unname(as.character(internal_computed_var_names)),
    rootId = wrapper_id
  )

  add_button <- function(x) {
    if (inherits(x, "htmlwidget") && !is.null(x$x$options)) {
      x$x$options$buttons <- c(list(toggle_button), x$x$options$buttons)
      return(list(value = x, added = TRUE))
    }
    if (is.list(x)) {
      for (i in seq_along(x)) {
        updated <- add_button(x[[i]])
        x[[i]] <- updated$value
        if (updated$added) {
          return(list(value = x, added = TRUE))
        }
      }
    }
    list(value = x, added = FALSE)
  }
  updated <- add_button(table)
  util_stop_if_not(
    "Could not add the metadata toggle to the data table." = updated$added
  )

  htmltools::div(
    id = wrapper_id,
    class = "dq-internal-computed-metadata-table",
    updated$value
  )
}

#' Internal helper: apply full page table class
#'
#' @noRd
util_apply_full_page_table_class <- function(x) {
  util_ensure_suggested(
    pkg = c("htmltools"),
    goal = "generate interactive HTML-reports."
  )

  htmltools::div(class = "fullpage-table", x)
}

#' Internal helper: is empty html
#'
#' @noRd
util_is_empty_html <- function(x) {
  if (is.null(x)) {
    return(TRUE)
  }

  # convert to character representation
  txt <- paste0(as.character(x), collapse = "")
  !nzchar(trimws(txt))
}
