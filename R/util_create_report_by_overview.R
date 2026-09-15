#' Check whether a report-by overview needs an SD legend
#'
#' @param strata_column [character] name of the study variable used for strata,
#'   or `NULL`.
#'
#' @return A scalar logical.
#' @noRd
util_report_by_overview_has_legend <- function(strata_column) {
  !is.null(strata_column) &&
    exists("..INFO_SD_NAME_FOR_REPORT", .dataframe_environment())
}

#' Resolve display labels used by the report-by overview
#'
#' Result summaries can contain cross-item rows whose `VAR_NAMES` value is a
#' group identifier rather than an item-level variable name. Prefer metadata
#' labels for study variables and retain the label already stored on all other
#' result rows.
#'
#' @param result result-summary data frame.
#' @param meta_data item-level metadata.
#' @param label_col display-label column.
#'
#' @return A character vector with one display label per result row.
#' @noRd
util_report_by_overview_labels <- function(result, meta_data, label_col) {
  var_names <- as.character(result[[VAR_NAMES]])
  if (identical(label_col, VAR_NAMES)) {
    return(var_names)
  }

  fallback <- var_names
  if (label_col %in% colnames(result)) {
    result_labels <- as.character(result[[label_col]])
    has_result_label <- !is.na(result_labels) & nzchar(trimws(result_labels))
    fallback[has_result_label] <- result_labels[has_result_label]
  }

  required_columns <- c(VAR_NAMES, label_col)
  if (!all(required_columns %in% colnames(meta_data))) {
    return(fallback)
  }
  mapping_data <- meta_data[, required_columns, drop = FALSE]
  unname(util_map_labels(
    var_names,
    meta_data = mapping_data,
    from = VAR_NAMES,
    to = label_col,
    ifnotfound = as.list(fallback)
  ))
}

#' Select report-by summary rows suitable for dashboard links
#'
#' @noRd
util_report_by_overview_dashboard_link_rows <- function(full_sum) {
  if (!is.data.frame(full_sum) || !nrow(full_sum)) {
    return(full_sum)
  }
  link_keys <- c(VAR_NAMES, "call_names", "indicator_metric")
  if (!all(link_keys %in% colnames(full_sum))) {
    return(full_sum)
  }
  has_result <- rep(FALSE, nrow(full_sum))
  for (column in intersect(c("values_raw", "value"), colnames(full_sum))) {
    has_result <- has_result | !util_empty(full_sum[[column]])
  }
  full_sum <- full_sum[order(!has_result), , drop = FALSE]
  full_sum[!duplicated(full_sum[link_keys]), , drop = FALSE]
}

#' Collect item-level assessment coverage for a report bundle
#'
#' @param reports stored reports corresponding to `summaries`.
#' @param summaries stored report summaries corresponding to `reports`.
#' @param repeated_items whether the same item in different subreports must be
#'   treated as a separate assessment target.
#'
#' @return A data frame as returned by `util_report_scope_item_coverage()`.
#' @noRd
util_report_by_overview_item_coverage <- function(
  reports,
  summaries,
  repeated_items = FALSE
) {
  coverage <- lapply(seq_along(reports), function(index) {
    report <- reports[[index]]
    if (is.null(report)) {
      return(NULL)
    }
    current <- util_report_scope_item_coverage(
      report,
      repsum = summaries[[index]]
    )
    if (!nrow(current) || !repeated_items) {
      return(current)
    }
    this <- util_attr(summaries[[index]], "this", exact = TRUE)
    prefix <- if (is.environment(this) || is.list(this)) this$sdn else NULL
    if (is.null(prefix) || !length(prefix) || util_empty(prefix)) {
      prefix <- paste0("report_", index)
    }
    current$variable <- paste0(prefix, "-", current$variable)
    current$variable_label <- paste0(prefix, " - ", current$variable_label)
    current
  })
  coverage <- Filter(is.data.frame, coverage)
  if (!length(coverage)) {
    return(util_report_scope_item_coverage())
  }
  unique(util_rbind(data_frames_list = coverage))
}

#' Render item-level assessment scope for a report bundle
#'
#' @inheritParams util_report_by_overview_item_coverage
#'
#' @return An [htmltools::tag()] or `NULL`.
#' @noRd
util_report_by_overview_item_scope <- function(
  reports,
  summaries,
  repeated_items = FALSE
) {
  coverage <- util_report_by_overview_item_coverage(
    reports = reports,
    summaries = summaries,
    repeated_items = repeated_items
  )
  if (!nrow(coverage)) {
    return(NULL)
  }
  dimensions <- c("Integrity", "Completeness", "Consistency", "Accuracy")
  indicator_counts <- vapply(dimensions, function(dimension) {
    length(unique(coverage$indicator_id[coverage$dimension == dimension]))
  }, integer(1))
  info_dim_dq <- data.frame(
    Dimension = dimensions,
    `No. DQ indicators` = indicator_counts,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  possible_indicator_ids <- unique(unlist(lapply(reports, function(report) {
    util_report_scope_target_indicator_ids("item", report = report)
  }), use.names = FALSE))
  util_render_report_scope_tree(
    info_dim_dq = info_dim_dq,
    info_scale_dq = data.frame(),
    item_coverage = coverage,
    item_possible_indicator_ids = possible_indicator_ids
  )
}

#' Prefix variable-group scope details for one report stratum
#'
#' @param node A variable-group scope-tree node.
#' @param prefix Stratum label used to distinguish repeated group assessments.
#'
#' @return The prefixed scope-tree node.
#' @noRd
util_report_by_overview_prefix_group_scope <- function(node, prefix) {
  items <- node$classification_items
  if (is.list(items)) {
    for (name in c("possible", "computed", "classified", "unresolved")) {
      values <- items[[name]]
      if (length(values)) {
        value_names <- paste(prefix, names(values), sep = "\f")
        values <- paste(prefix, values, sep = " - ")
        names(values) <- value_names
        items[[name]] <- values
      }
    }
    if (is.data.frame(items$matrix) && nrow(items$matrix)) {
      items$matrix$unit <- paste(prefix, items$matrix$unit, sep = "\f")
      items$matrix$unit_label <- paste(
        prefix,
        items$matrix$unit_label,
        sep = " - "
      )
    }
    node$classification_items <- items
  }
  node$children <- lapply(
    node$children,
    util_report_by_overview_prefix_group_scope,
    prefix = prefix
  )
  node
}

#' Merge variable-group scope trees from report-by subreports
#'
#' @param nodes Variable-group scope-tree nodes.
#'
#' @return One combined scope-tree node, or `NULL`.
#' @noRd
util_report_by_overview_merge_group_scopes <- function(nodes) {
  nodes <- Filter(Negate(is.null), nodes)
  if (!length(nodes)) {
    return(NULL)
  }
  items <- util_report_scope_tree_merge_classification_items(lapply(
    nodes,
    `[[`,
    "classification_items"
  ))
  concepts <- util_report_scope_tree_merge_concept_items(lapply(
    nodes,
    `[[`,
    "concept_items"
  ))
  child_labels <- unique(unlist(lapply(nodes, function(node) {
    vapply(node$children, `[[`, character(1), "label")
  }), use.names = FALSE))
  children <- lapply(child_labels, function(label) {
    util_report_by_overview_merge_group_scopes(unlist(lapply(
      nodes,
      function(node) {
        Filter(
          function(child) identical(child$label, label),
          node$children
        )
      }
    ), recursive = FALSE))
  })
  matrix <- items$matrix
  measures <- if (is.data.frame(matrix) && nrow(matrix)) {
    length(unique(matrix$analysis))
  } else {
    max(vapply(nodes, `[[`, integer(1), "measures"))
  }
  assessed <- if (is.data.frame(matrix) && nrow(matrix)) {
    length(unique(matrix$unit))
  } else {
    max(vapply(nodes, `[[`, integer(1), "assessed"))
  }
  coverage <- util_report_scope_tree_coverage(
    length(items$classified),
    length(items$possible),
    length(items$computed)
  )
  concept_coverage <- util_report_scope_tree_concept_coverage(
    length(concepts$assessed),
    length(concepts$possible),
    length(concepts$computed),
    length(concepts$requested)
  )
  label <- nodes[[1]]$label
  util_report_scope_tree_node(
    sprintf(
      "%s: %s; %s; %s",
      label,
      util_count_label(measures, "variable-group metric"),
      util_count_label(assessed, "group"),
      util_report_scope_tree_group_coverage_label(coverage)
    ),
    coverage = coverage,
    classification_items = items,
    concept_coverage = concept_coverage,
    concept_items = concepts,
    label = label,
    measures = measures,
    assessed = assessed,
    children = children
  )
}

#' Render variable-group assessment scope for a report bundle
#'
#' @inheritParams util_report_by_overview_item_coverage
#'
#' @return An [htmltools::tag()] or `NULL`.
#' @noRd
util_report_by_overview_group_scope <- function(
  reports,
  summaries,
  repeated_groups = FALSE
) {
  nodes <- lapply(seq_along(reports), function(index) {
    report <- reports[[index]]
    if (is.null(report)) {
      return(NULL)
    }
    info <- util_generate_table_scale(report, repsum = summaries[[index]])
    node <- util_report_scope_variable_group_tree_nodes(
      info,
      report = report,
      repsum = summaries[[index]]
    )
    if (is.null(node$coverage) || node$coverage$possible < 1L) {
      return(NULL)
    }
    if (repeated_groups) {
      this <- util_attr(summaries[[index]], "this", exact = TRUE)
      prefix <- if (is.environment(this) || is.list(this)) {
        this$stratum
      }
      if (is.null(prefix) || !length(prefix) || util_empty(prefix)) {
        prefix <- paste0("report_", index)
      }
      node <- util_report_by_overview_prefix_group_scope(node, prefix)
    }
    node
  })
  node <- util_report_by_overview_merge_group_scopes(nodes)
  if (is.null(node)) {
    return(NULL)
  }
  htmltools::div(
    class = "dq-report-scope-tree-grid",
    util_render_report_scope_tree_panel(node)
  )
}

#' Load reports belonging to stored report-by summaries
#'
#' @inheritParams util_report_by_overview_item_coverage
#'
#' @return A list parallel to `summary_names`; unavailable reports are `NULL`.
#' @noRd
util_report_by_overview_reports <- function(output_dir, summary_names) {
  lapply(summary_names, function(summary_name) {
    report_name <- sub(
      "^report_summary_(.*)[.]RDS$",
      "report_\\1.dq2",
      summary_name
    )
    report_path <- file.path(output_dir, report_name)
    if (!file.exists(report_path)) {
      return(NULL)
    }
    report <- try(prep_load_report(report_path), silent = TRUE)
    if (util_is_try_error(report)) NULL else report
  })
}

#' Create an overview of the reports created with `dq_report_by`
#'
#' writes to the files
#' `index.html`, `dashboard.html`, `tables.html` in `output_dir`.
#'
#' @param output_dir [character] the directory in which all reports are searched
#'                               and the overview is saved
#'
#' @return `invisible(NULL)`
#' @noRd
util_create_report_by_overview <- function(output_dir) {
  util_ensure_suggested("jsonlite",
    goal = "overall-overviews",
    err = TRUE
  )

  # prepare the path of the output_dir with the / at the end
  out_dir <- output_dir
  if (!endsWith(out_dir, .Platform$file.sep)) {
    out_dir <- paste0(out_dir, .Platform$file.sep)
  }

  strata_column <- NULL
  segment_column <- NULL
  strata_column_label <- NULL
  subgroup <- NULL
  mod_label <- NULL
  title <- NULL
  disable_plotly <- NULL
  rep_id <- NULL
  start_time <- NULL
  subtitle <- NULL
  author <- NULL
  user_info <- NULL
  by_call <- NULL
  call_report_by <- NULL
  call_report_by_overview <- NULL

  list2env(readRDS(file.path(out_dir, "report_by_meta.RDS")),
    envir = environment()
  )
  # this contains as list of
  # @param strata_column [character] name of a study variable to stratify the
  #                                    report by. It can be null
  # @param segment_column [character] name of a metadata attribute
  #                                    usable to split the report in
  #                                    sections of variables. It can be null
  # @param strata_column_label [character] the label of the variable used as
  #                                           strata_column
  # @param subgroup [character] optional, to define subgroups of cases
  # @param mod_label [list] `util_ensure_label()` info
  # @param title [character] a title for the overview `HTML`-file
  # @param disable_plotly [logical] do not use `plotly`, even if installed
  # @param rep_id [character] unique ID for the report to detect changes
  # @param start_time [as.POSIXct] time point when this computation has started

  if (nzchar(trimws(subtitle))) {
    subtitle <- paste0(subtitle, ": ")
  }

  util_expect_scalar(title, check_type = is.character)

  packageName <- utils::packageName()

  doc_title <- title

  # create a summary of summaries
  # import all the R objects containing a report summary and create a list
  sum_names <- list.files(path = output_dir, pattern = "^report_summary_")
  sum_names2 <- vapply(sum_names, function(x) {
    paste0(out_dir, x)
  }, FUN.VALUE = character(1))
  list_summaries2 <- lapply(sum_names2, FUN = readRDS)
  reports <- util_report_by_overview_reports(output_dir, sum_names)

  # create a toc
  toc <- lapply(sum_names, function(x) {
    path_name <- paste0(out_dir, x)
    if (!"report.html" %in%
      list.files(path = file.path(
        sub("/$", "", out_dir),
        sub(
          "^report_summary", "report",
          sub(".RDS$", "", x)
        ),
        ".report"
      ))) {
      return(NULL)
    }
    temp_file <- readRDS(path_name)
    this1 <- util_attr(temp_file, "this", exact = TRUE)
    sdn <- this1$sdn
    safe_name <- this1$safe_name
    if (is.null(safe_name) || !length(safe_name)) {
      safe_name <- gsub("[^a-zA-Z0-9_\\.]", "", sdn)
    }
    path_name <- file.path(output_dir, sprintf("report_%s", safe_name))
    location <- sprintf("report_%s", safe_name)
    toc <- list()
    toc[[this1$stratum]] <- list()
    toc[[this1$stratum]][[this1$segment]] <- location
    return(toc)
  })
  toc <- toc[!vapply(toc, is.null, FUN.VALUE = logical(1))]

  # create a vector containing all variables and their location in folder
  vars_reportlocation <- lapply(sum_names, function(x) {
    name_folder <- gsub("(report_summary_)(.*)\\.RDS", "\\2", x)
    name_folder <- paste0("report_", name_folder)
    path_name <- paste0(out_dir, x)
    temp_file <- readRDS(path_name)
    this1 <- util_attr(temp_file, "this", exact = TRUE)
    used_data_file <- this1$used_data_file
    # leave only file name and extension, if the vector contains a path
    if (grepl(.Platform$file.sep, used_data_file, fixed = TRUE)) {
      used_data_file <- gsub(".+\\/(.+\\..+$)", "\\1", used_data_file)
    }
    stratum <- this1$stratum
    segm <- this1$segment
    # the complete name changes depending of the split selected
    if (is.null(strata_column)) {
      origin_name <- paste0(used_data_file, "-", segm)
    } else if (!is.null(strata_column) && is.null(segment_column)) {
      origin_name <- paste0(used_data_file, "-", stratum)
    } else if (!is.null(strata_column) && !is.null(segment_column)) {
      origin_name <- paste0(used_data_file, "-", stratum, "-", segm)
    }

    temp_res <- this1$result[, colnames(this1$result) %in%
        c(VAR_NAMES, "call_names", this1$label_col), drop = FALSE]

    if (this1$label_col != VAR_NAMES) {
      temp_res[[VAR_NAMES]] <- util_report_by_overview_labels(
        result = temp_res,
        meta_data = this1$meta_data,
        label_col = this1$label_col
      )
    }

    temp_res$md_names <- paste0(
      temp_res[[VAR_NAMES]],
      rep(".", nrow(temp_res)),
      temp_res$call_names
    )
    if (nrow(temp_res) == 0) {
      return(setNames(list(), nm = character(0)))
    }
    md_names <- temp_res$md_names
    md_names <- paste0(origin_name, "-", md_names)
    if (is.null(strata_column) || (!is.null(strata_column) &&
          length(list_summaries2) == 1)) {
      origin_name <- paste0(origin_name, "-")
      md_names <- sub(origin_name, "", md_names, fixed = TRUE)
    }

    md_names <- unique(md_names)
    label_col_and_folder <- list(setNames(
      rep(name_folder,
        times = length(md_names)
      ),
      nm = md_names
    ))
    return(label_col_and_folder)
  })

  vars_reportlocation <- unlist(vars_reportlocation)

  # In case of creation of report by strata (more than one strata) the VAR_NAMES
  # changes so a conversion dataframe is needed to create the links to the
  # folders
  if (!is.null(strata_column) && length(list_summaries2) > 1) {
    # create a vector with the variable_names and the unique variable names
    # created when there is a strata selection
    vars_originalnames <- mapply(
      x = sum_names,
      SIMPLIFY = FALSE,
      FUN = function(x) {
        name_folder <- gsub(
          "(report_summary_)(.*)\\.RDS",
          "\\2", x
        )
        name_folder <- paste0("report_", name_folder)
        path_name <- paste0(out_dir, x)
        temp_file <- readRDS(path_name)
        this1 <- util_attr(temp_file, "this",
          exact = TRUE
        )
        used_data_file <- this1$used_data_file
        if (grepl(.Platform$file.sep,
            used_data_file,
            fixed = TRUE
          )) {
          used_data_file <- gsub(
            ".+\\/(.+\\..+$)",
            "\\1",
            used_data_file
          )
        }
        stratum <- this1$stratum
        segm <- this1$segment
        # the complete name changes depending of the split selected
        if (is.null(strata_column)) {
          origin_name <- paste0(
            used_data_file,
            "-",
            segm
          )
        } else if (!is.null(strata_column) &&
            is.null(segment_column)) {
          origin_name <- paste0(
            used_data_file,
            "-",
            stratum
          )
        } else if (!is.null(strata_column) &&
            !is.null(segment_column)) {
          origin_name <- paste0(
            used_data_file,
            "-",
            stratum,
            "-",
            segm
          )
        }

        temp_res <- this1$result[, colnames(this1$result) %in%
            c(VAR_NAMES, this1$label_col),
          drop = FALSE
        ]
        temp_res[[LABEL]] <- util_report_by_overview_labels(
          result = temp_res,
          meta_data = this1$meta_data,
          label_col = this1$label_col
        )
        temp_res <- temp_res[!duplicated(
          temp_res[, c(VAR_NAMES, LABEL), drop = FALSE]
        ), , drop = FALSE]
        temp_res$md_names <- rep(NA, nrow(temp_res))
        md_names <- temp_res[[VAR_NAMES]]
        md_names <- paste0(origin_name, "-", md_names)
        if (is.null(strata_column) ||
            (!is.null(strata_column) &&
                length(list_summaries2) == 1)) {
          origin_name <- paste0(origin_name, "-")
          md_names <- sub(origin_name,
            "",
            md_names,
            fixed = TRUE
          )
        }
        if (nrow(temp_res) > 0) {
          temp_res$md_names <- md_names
        }

        temp_res$new_name_with_label <- paste0(temp_res[[LABEL]])
        new_name_with_label <- temp_res$new_name_with_label
        new_name_with_label <- paste0(
          origin_name,
          "-",
          new_name_with_label
        )
        if (is.null(strata_column) ||
            (!is.null(strata_column) &&
                length(list_summaries2) == 1)) {
          origin_name <- paste0(origin_name, "-")
          new_name_with_label <- sub(origin_name,
            "",
            new_name_with_label,
            fixed = TRUE
          )
        }
        if (nrow(temp_res) > 0) {
          temp_res$new_name_with_label <- new_name_with_label
        }

        temp_res$name_matching_result2_Variables <-
          paste0(temp_res[[LABEL]])
        name_matching_result2_variables <-
          temp_res$name_matching_result2_Variables
        name_matching_result2_variables <-
          paste0(
            origin_name,
            " - ",
            name_matching_result2_variables
          )
        if (is.null(strata_column) ||
            (!is.null(strata_column) &&
                length(list_summaries2) == 1)) {
          origin_name <- paste0(origin_name, "-")
          name_matching_result2_variables <-
            sub(origin_name,
              "",
              name_matching_result2_variables,
              fixed = TRUE
            )
        }
        if (nrow(temp_res) > 0) {
          temp_res$name_matching_result2_Variables <-
            name_matching_result2_variables
        }
        label_col_and_folder <-
          data.frame(
            new_names_with_varnames = temp_res$md_names,
            new_names_with_label = temp_res$new_name_with_label,
            result2_Variables_match =
            temp_res$name_matching_result2_Variables,
            var_names = temp_res[[VAR_NAMES]],
            name_label = temp_res[[LABEL]]
          )
        return(label_col_and_folder)
      }
    )
  } else {
    vars_originalnames <- NULL
  }

  # create a summary of summaries to be included in the overview page
  if (is.null(strata_column)) {
    summary_all <- util_combine_list_report_summaries(list_summaries2,
      type = "unique_vars"
    )
  } else {
    summary_all <- util_combine_list_report_summaries(list_summaries2,
      type = "repeated_vars"
    )
  }
  summary_all_this <- util_attr(summary_all, "this", exact = TRUE)
  variable_group_results <- util_filter_repsum(
    summary_all_this$result,
    vars_to_include = "variable_group",
    meta_data = summary_all_this$meta_data,
    rownames_of_report = summary_all_this$rownames_of_report,
    label_col = summary_all_this$label_col,
    variable_group_call_names =
      summary_all_this$variable_group_call_names
  )
  have_variable_group_dq <- nrow(variable_group_results) > 0 && any(
    !startsWith(as.character(variable_group_results$indicator_metric), "CAT_") &
      !startsWith(as.character(variable_group_results$indicator_metric), "MSG_")
  )
  contradiction_rows <- util_summary_metrics_in_concept(
    variable_group_results[["indicator_metric"]],
    "con_con"
  )
  contradiction_variable_group_results <- variable_group_results[
    contradiction_rows, , drop = FALSE
  ]
  other_variable_group_results <- variable_group_results[
    !contradiction_rows, , drop = FALSE
  ]
  have_contradiction_classifications <-
    "class" %in% colnames(contradiction_variable_group_results) &&
    any(!util_empty(contradiction_variable_group_results[["class"]]))
  have_other_variable_group_classifications <-
    "class" %in% colnames(other_variable_group_results) &&
    any(!util_empty(other_variable_group_results[["class"]]))
  have_combined_variable_group_sunburst <-
    have_contradiction_classifications &&
    have_other_variable_group_classifications
  contradiction_summary_all <- util_summary_subset_metric_concept(
    summary_all,
    "con_con",
    include = TRUE
  )
  other_variable_group_summary_all <- util_summary_subset_metric_concept(
    summary_all,
    "con_con",
    include = FALSE
  )

  item_scope <- util_report_by_overview_item_scope(
    reports = reports,
    summaries = list_summaries2,
    repeated_items = !is.null(strata_column)
  )
  group_scope <- util_report_by_overview_group_scope(
    reports = reports,
    summaries = list_summaries2,
    repeated_groups = !is.null(strata_column)
  )

  # render the table and the pie chart of the overview summary
  tb_summaries <-
    util_render_table_dataquieR_summary(summary_all,
      folder_of_report = vars_reportlocation,
      var_uniquenames = vars_originalnames
    )
  plot_summaries <- plot(summary_all,
    dont_plot = TRUE,
    disable_plotly = disable_plotly,
    folder_of_report = vars_reportlocation,
    var_uniquenames = vars_originalnames
  )
  variable_group_tb_summaries <- if (have_variable_group_dq) {
    util_render_table_dataquieR_summary(summary_all,
      vars_to_include = "variable_group",
      grouped_by = "indicator_metric",
      folder_of_report = vars_reportlocation,
      var_uniquenames = vars_originalnames
    )
  }
  variable_group_plot_summaries <- if (have_variable_group_dq) {
    plot(summary_all,
      vars_to_include = "variable_group",
      dont_plot = TRUE,
      disable_plotly = disable_plotly,
      folder_of_report = vars_reportlocation,
      var_uniquenames = vars_originalnames
    )
  }

  dashboard_names <- list.files(
    path = output_dir,
    pattern = "^report_dashboard_",
    full.names = TRUE
  )

  all_dashboards <- lapply(setNames(nm = dashboard_names), readRDS)
  variable_group_dashboards <- lapply(seq_along(reports), function(index) {
    report <- reports[[index]]
    if (is.null(report)) {
      return(NULL)
    }
    db <- util_setup_dashboard(
      report,
      make_links = FALSE,
      return_table_only = TRUE,
      repsum = list_summaries2[[index]],
      vars_to_include = "variable_group"
    )
    if (is.data.frame(db) && nrow(db) > 0) {
      this <- util_attr(list_summaries2[[index]], "this", exact = TRUE)
      used_data_file <- this$used_data_file
      if (grepl(.Platform$file.sep, used_data_file, fixed = TRUE)) {
        used_data_file <- basename(used_data_file)
      }
      attr(db, "name_of_study_data") <- used_data_file
      attr(db, "level_name") <- this$stratum
    }
    db
  })

  render_dashboard <- function(dashboards, image_dir,
    vars_to_include = "study") {
    link_results <- summary_all_this$result
    if (identical(vars_to_include, "variable_group")) {
      link_results <- util_summary_most_specific_group_metrics(link_results)
    }
    full_sum <- util_add_links_to_summary_table(
      link_results,
      summary_all_this,
      folder_of_report = vars_reportlocation,
      vars_to_include = vars_to_include
    )
    full_sum <- util_report_by_overview_dashboard_link_rows(full_sum)
    dashboards <- Filter(is.data.frame, dashboards)
    dashboards <- lapply(dashboards, function(db) {
      if (!nrow(db)) {
        return(db)
      }
      translated_colnames <- colnames(db)
      if (is.null(strata_column)) {
        db$fq_VARNAME <- db$..VAR_NAMES
      } else {
        stratum <- util_attr(db, "level_name", exact = TRUE)
        stratum_prefix <- paste0(as.character(strata_column_label), "_")
        stratum_display <- if (startsWith(stratum, stratum_prefix)) {
          paste0(
            as.character(strata_column_label),
            " = ",
            substring(stratum, nchar(stratum_prefix) + 1L)
          )
        } else {
          stratum
        }
        db$Stratum <- stratum_display
        db$fq_VARNAME <- paste0(
          util_attr(db, "name_of_study_data", exact = TRUE),
          "-",
          stratum,
          "-",
          ifelse(!is.null(segment_column),
            paste0(util_with_orig_names(db)[[STUDY_SEGMENT]], "-"),
            ""
          ),
          db$..VAR_NAMES
        )
      }
      source_colnames <- c(
        util_attr(translated_colnames, "names", exact = TRUE),
        if (!is.null(strata_column)) "Stratum",
        "fq_VARNAME"
      )
      attr(db, "names") <- as.character(colnames(db))
      util_translated_colnames(db) <- util_translate(
        source_colnames,
        as_this_translation = translated_colnames
      )
      db
    })
    all_dashboards_df <- util_rbind(data_frames_list = dashboards)
    rownames(all_dashboards_df) <- NULL
    if (!prod(dim(all_dashboards_df)) || !prod(dim(full_sum))) {
      return(htmltools::HTML(""))
    }
    orig_cn <- colnames(all_dashboards_df)
    to_rm <- util_untranslated_colnames(all_dashboards_df) %in%
      c("title", "href", "popup_href")
    ns <- util_attr(orig_cn, "ns", exact = TRUE)
    lang <- util_attr(orig_cn, "lang", exact = TRUE)
    class <- util_attr(orig_cn, "class", exact = TRUE)
    nms <- util_attr(orig_cn, "names", exact = TRUE)
    all_dashboards_df[to_rm] <- NULL
    orig_cn <- orig_cn[!to_rm]
    nms <- nms[!to_rm]
    attr(orig_cn, "lang") <- lang
    attr(orig_cn, "ns") <- ns
    attr(orig_cn, "names") <- nms
    attr(orig_cn, "class") <- class
    util_translated_colnames(all_dashboards_df) <- orig_cn

    dashboard_table <-
      merge(
        x = all_dashboards_df,
        y = full_sum[, -which(colnames(full_sum) == "value"), drop = FALSE],
        by.x = util_translate(
          c(
            "fq_VARNAME",
            "call_names",
            "indicator_metric"
          ),
          as_this_translation = colnames(all_dashboards_df)
        ),
        by.y = c("VAR_NAMES", "call_names", "indicator_metric"),
        all = FALSE
      )


    util_translated_colnames(dashboard_table) <-
      util_translate(
        util_translate(colnames(dashboard_table),
          as_this_translation = colnames(all_dashboards_df),
          reverse = TRUE
        ),
        as_this_translation = colnames(all_dashboards_df)
      )

    dashboard_table <- util_fix_columns_in_dashboard_for_overview(
      dashboard_table,
      image_dir = image_dir
    )

    summary_attrs <- lapply(list_summaries2, util_attr, "this")
    label_cols <- lapply(summary_attrs, `[[`, "label_col")
    unique_label_col <- unique(label_cols)[[1]]
    util_dashboard_table2widget(
      dashboard_table,
      unique_label_col,
      vars_to_include = vars_to_include
    )
  }
  dashboard <- render_dashboard(
    all_dashboards,
    image_dir = file.path(output_dir, "dashboard_images")
  )
  variable_group_dashboard <- if (have_variable_group_dq) {
    render_dashboard(
      variable_group_dashboards,
      image_dir = file.path(
        output_dir,
        "variable_group_dashboard_images"
      ),
      vars_to_include = "variable_group"
    )
  }

  if (!disable_plotly) {
    sunburst_summaries <- plot(summary_all,
      dont_plot = TRUE,
      disable_plotly = disable_plotly,
      hierarchy = TRUE,
      folder_of_report = vars_reportlocation,
      var_uniquenames = vars_originalnames
    )
  } else {
    sunburst_summaries <- htmltools::HTML("")
  }
  variable_group_sunburst_summaries <- if (
    have_other_variable_group_classifications && !disable_plotly
  ) {
    plot(other_variable_group_summary_all,
      dont_plot = TRUE,
      disable_plotly = disable_plotly,
      hierarchy = TRUE,
      vars_to_include = "variable_group",
      folder_of_report = vars_reportlocation,
      var_uniquenames = vars_originalnames
    )
  } else {
    htmltools::HTML("")
  }
  contradiction_sunburst_summaries <- if (
    have_contradiction_classifications && !disable_plotly
  ) {
    plot(contradiction_summary_all,
      dont_plot = TRUE,
      disable_plotly = disable_plotly,
      hierarchy = TRUE,
      vars_to_include = "variable_group",
      folder_of_report = vars_reportlocation,
      var_uniquenames = vars_originalnames
    )
  } else {
    htmltools::HTML("")
  }
  all_variable_group_sunburst_summaries <- if (
    have_combined_variable_group_sunburst && !disable_plotly
  ) {
    plot(summary_all,
      dont_plot = TRUE,
      disable_plotly = disable_plotly,
      hierarchy = TRUE,
      vars_to_include = "variable_group",
      folder_of_report = vars_reportlocation,
      var_uniquenames = vars_originalnames
    )
  } else {
    htmltools::HTML("")
  }
  have_variable_group_sunburst <-
    !util_is_empty_html(variable_group_sunburst_summaries) ||
    !util_is_empty_html(contradiction_sunburst_summaries)
  contradiction_sunburst_notice <- if (
    !util_is_empty_html(contradiction_sunburst_summaries)
  ) {
    util_summary_check_count_title(
      contradiction_variable_group_results,
      summary_all_this$meta_data_cross_item,
      "contradiction"
    )
  }
  contradiction_sunburst_count <- util_summary_evaluated_check_count(
    contradiction_variable_group_results
  )

  # Create an all_ids file
  all_ids <- lapply(sum_names, function(x) {
    name_folder <- gsub("(report_summary_)(.*)\\.RDS", "\\2", x)

    path_all_ids_file <- file.path(
      out_dir,
      paste0("report_", name_folder),
      ".report",
      "anchor_list.RDS"
    )
    if (!file.exists(path_all_ids_file)) {
      util_warning(
        "Cannot read %s for %s -- Internal error, sorry. Please report",
        dQuote(path_all_ids_file),
        dQuote(x)
      )
      return(character(0))
    }
    temp_file <- readRDS(path_all_ids_file)
    temp_file <- file.path(
      fsep = "/",
      paste0("report_", name_folder),
      ".report",
      temp_file
    )
    return(temp_file)
  })

  # create an overall anchor list with all created results, useful not to
  # create links to non-existing results
  all_ids <- c(
    unlist(all_ids),
    "index.html",
    "sunburst.html",
    "dashboard.html",
    "tables.html",
    "variable-group-sunburst.html",
    "variable-group-dashboard.html",
    "variable-group-tables.html"
  )

  cat(
    sep = "",
    "window.all_ids = {\"all_ids\": ",
    paste0(
      "[",
      paste0('"', all_ids, '"', collapse = ", "),
      "]"
    ),
    "}",
    file = file.path(out_dir, "anchor_list.js")
  )

  # creates the html overview page that links all sub-reports created
  if (!is.null(strata_column) && !is.null(segment_column)) {
    level_seg <- names(unlist(toc))
    get_level <- function(str) {
      sub("\\..*", "", str)
    }
    sdlevel <- vapply(level_seg, get_level, FUN.VALUE = character(1))
    get_segm <- function(str) {
      sub(".*\\.", "", str)
    }
    segs <- vapply(level_seg, get_segm, FUN.VALUE = character(1))
    sdlevel <- substr(sdlevel, nchar(strata_column_label) + 2, 10000)
    title <- paste0(
      "Report: ", segment_column, " = ", segs, ", ",
      strata_column_label, " = ", sdlevel
    )
  } else if (is.null(strata_column) && !is.null(segment_column)) {
    level_seg <- names(unlist(toc))
    get_level <- function(str) {
      sub("\\..*", "", str)
    }
    sdlevel <- vapply(level_seg, get_level, FUN.VALUE = character(1))
    get_segm <- function(str) {
      sub(".*\\.", "", str)
    }
    segs <- vapply(level_seg, get_segm, FUN.VALUE = character(1))
    title <- paste0("Report: ", segment_column, " = ", segs, ".", sdlevel)
  } else if (!is.null(strata_column) && is.null(segment_column)) {
    sdlevel <- names(unlist(toc))
    sdlevel <- substr(sdlevel, nchar(strata_column_label) + 2, 10000)
    title <- paste("Report: ", strata_column_label, " = ", sdlevel)
  } else if (is.null(strata_column) && is.null(segment_column) &&
      !is.null(subgroup)) {
    if (is.character(title)) {
      title <- paste0(title, ". Subgroup: ", subgroup)
    } else {
      title <- paste0("Report on subgroup: ", subgroup)
    }
  } else {
    title <- paste0("Report on all variables and observations")
  }

  # create the link for the created reports
  if (length(toc) == 0) {
    href <- character(0)
  } else {
    href <- paste0(unlist(toc), "/.report/report.html")
  }

  has_legend <- util_report_by_overview_has_legend(strata_column)

  overview_css_dep <- htmltools::htmlDependency(
    name = "dataquieR.dq_report_by_overview",
    version = "0.0.1",
    src = system.file("menu", package = "dataquieR"),
    stylesheet = c("dataquieR_overview.css")
  )


  build_report_list <- function(href, title) {
    htmltools::tags$div(
      class = "dq-overview-card dq-overview-card-links",
      htmltools::tags$div(
        class = "dq-overview-card-title",
        "List of created reports"
      ),
      htmltools::tags$ul(
        class = "dq-overview-report-list",
        lapply(seq_along(href), function(i) {
          htmltools::tags$li(
            class = "dq-overview-report-item",
            htmltools::tags$a(
              class = "dq-overview-report-link",
              href = href[[i]],
              title[[i]]
            )
          )
        })
      )
    )
  }

  build_legend_block <- function() {
    info_sd <- as.list(prep_get_data_frame("..INFO_SD_NAME_FOR_REPORT"))

    htmltools::tags$div(
      class = "dq-overview-card dq-overview-card-legend",
      htmltools::tags$div(
        class = "dq-overview-card-title",
        "Legend"
      ),
      htmltools::tags$div(
        class = "dq-overview-legend-grid",
        lapply(names(info_sd), function(x) {
          htmltools::tags$div(
            class = "dq-overview-legend-item",
            htmltools::tags$div(
              class = "dq-overview-legend-key",
              x
            ),
            htmltools::tags$div(
              class = "dq-overview-legend-value",
              as.character(info_sd[[x]])
            )
          )
        })
      )
    )
  }

  overview_page <- function(file, label, entity, content,
    include_plots = FALSE) {
    list(
      file = file,
      label = label,
      entity = entity,
      content = content,
      include_plots = include_plots
    )
  }

  item_pages <- if (disable_plotly) {
    list(
      overview_page("tables.html", "Items: Summary table", "item", "table",
        include_plots = TRUE
      ),
      overview_page("dashboard.html", "Items: Dashboard", "item", "dashboard",
        include_plots = TRUE
      )
    )
  } else {
    list(
      overview_page("sunburst.html", "Items: Sunburst", "item", "sunburst"),
      overview_page("dashboard.html", "Items: Dashboard", "item", "dashboard",
        include_plots = TRUE
      ),
      overview_page("tables.html", "Items: Summary table", "item", "table",
        include_plots = TRUE
      )
    )
  }
  group_pages <- if (disable_plotly || !have_variable_group_sunburst) {
    list(
      overview_page("variable-group-tables.html",
        "Variable groups: Summary table", "variable_group", "table",
        include_plots = TRUE
      ),
      overview_page("variable-group-dashboard.html",
        "Variable groups: Dashboard", "variable_group", "dashboard",
        include_plots = TRUE
      )
    )
  } else {
    list(
      overview_page("variable-group-sunburst.html",
        "Variable groups: Sunburst", "variable_group", "sunburst"
      ),
      overview_page("variable-group-dashboard.html",
        "Variable groups: Dashboard", "variable_group", "dashboard",
        include_plots = TRUE
      ),
      overview_page("variable-group-tables.html",
        "Variable groups: Summary table", "variable_group", "table",
        include_plots = TRUE
      )
    )
  }
  scope_page <- overview_page(
    "index.html",
    "Assessment scope",
    "item",
    "scope"
  )
  overview_pages <- c(
    list(scope_page),
    item_pages,
    if (have_variable_group_dq) group_pages else list()
  )

  build_tab_nav <- function(active_file) {

    htmltools::tags$div(
      class = "dq-overview-tabs",
      lapply(overview_pages, function(tab) {
        classes <- "dq-overview-tab"
        if (identical(tab$file, active_file)) {
          classes <- paste(classes, "active")
        }
        htmltools::tags$a(
          class = classes,
          href = tab$file,
          `data-no-existance-check` =
            jsonlite::toJSON(TRUE, auto_unbox = TRUE),
          tab$label
        )
      })
    )
  }

  build_footer <- function(start_time, end_time) {
    p <- NULL
    if (is.list(user_info)) {
      p <- user_info
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


    htmltools::tagList(
      util_html_table(
        title = prep_title_escape(doc_title, TRUE),
        rotate_for_one_row = TRUE,
        util_rbind(data.frame(
          check.names = FALSE,
          fix.empty.names = FALSE,
          `Overall computation time` =
            as.character(htmltools::span(`data-content` = "renderingTime")),
          `dataquieR version` = util_dataquieR_version(),
          `Timestamp` = format(start_time),
          `Author` = author,
          `Call` =
            as.character(htmltools::div(
              class = "dq-overview-call-cell",
              htmltools::pre(util_report_by_overview_call_text(
                by_call = by_call,
                call_report_by = call_report_by,
                call_report_by_overview = call_report_by_overview
              ))
            ))
        ), p)
      )
    )
  }

  build_section_card <- function(title_text, content, body_class = NULL,
    id = NULL) {
    htmltools::tags$div(
      id = id,
      class = "dq-overview-section-card",
      htmltools::tags$div(
        class = "dq-overview-section-heading",
        title_text
      ),
      htmltools::tags$div(
        class = paste(
          c("dq-overview-section-body", body_class),
          collapse = " "
        ),
        content
      )
    )
  }

  build_overview_toc <- function(active_file,
    entity = c("item", "variable_group"),
    include_plot_summaries = FALSE,
    include_sunburst_summaries = FALSE,
    include_dashboard = FALSE,
    include_tb_summaries = FALSE,
    end_time) {
    entity <- match.arg(entity)
    summary_sections <- list()

    if (identical(active_file, "index.html") &&
        (!is.null(item_scope) || !is.null(group_scope))) {
      summary_sections <- c(
        summary_sections,
        list(build_section_card(
          "Assessment scope",
          htmltools::tagList(
            htmltools::p(
              "Assessment scope follows the DQ_OBS concept ",
              "(Schmidt et al., 2021)."
            ),
            item_scope,
            group_scope
          )
        ))
      )
    }

    if (include_plot_summaries) {
      plot_content <- if (identical(entity, "item")) {
        plot_summaries
      } else {
        variable_group_plot_summaries
      }
      summary_sections <- c(
        summary_sections,
        list(build_section_card("Plots", plot_content))
      )
    }
    if (include_sunburst_summaries) {
      if (identical(entity, "item")) {
        summary_sections <- c(
          summary_sections,
          list(build_section_card("Item-level sunburst", sunburst_summaries))
        )
      } else {
        summary_sections <- c(
          summary_sections,
          list(build_section_card(
            "Variable-group sunburst",
            util_render_variable_group_sunburst_switch(
              other_chart = variable_group_sunburst_summaries,
              contradiction_chart = contradiction_sunburst_summaries,
              all_chart = all_variable_group_sunburst_summaries,
              contradiction_count = contradiction_sunburst_count,
              contradiction_notice = contradiction_sunburst_notice
            )
          ))
        )
      }
    }
    if (include_dashboard) {
      dashboard_content <- if (identical(entity, "item")) {
        dashboard
      } else {
        variable_group_dashboard
      }
      dashboard_title <- if (identical(entity, "item")) {
        "Item-level dashboard"
      } else {
        "Variable-group dashboard"
      }
      summary_sections <- c(
        summary_sections,
        list(build_section_card(
          dashboard_title,
          dashboard_content,
          body_class = "dq-overview-dashboard-container"
        ))
      )
    }
    if (include_tb_summaries) {
      table_content <- if (identical(entity, "item")) {
        tb_summaries
      } else {
        variable_group_tb_summaries
      }
      table_title <- if (identical(entity, "item")) {
        "Item-level summary table"
      } else {
        "Variable-group summary table"
      }
      summary_sections <- c(
        summary_sections,
        list(build_section_card(
          table_title,
          table_content,
          body_class = "dq-overview-table-container"
        ))
      )
    }

    header_subtitle <- paste0(
      util_by_header_from_args(by_call),
      #      "\nInteractive overview of created reports",
      if (is.character(doc_title) && length(doc_title) == 1L &&
          nzchar(doc_title)) {
        paste0(" \u2014 ", doc_title)
      } else {
        ""
      }
    )

    do.call(
      htmltools::tagList,
      list(
        htmltools::tags$script(
          src = ".report/renderinfo.js",
          type = "text/javascript"
        ),
        htmltools::div(class = "navbar"),
        htmltools::tags$script(
          type = "text/javascript",
          "window.dq_report_by_overview = true"
        ),
        htmltools::tags$script(
          type = "text/javascript",
          src = "anchor_list.js"
        ),
        htmltools::tags$div(
          class = "dq-overview-app",
          htmltools::tags$div(
            class = "dq-overview-shell",
            htmltools::tags$div(
              class = "dq-overview-topbar",
              htmltools::tags$div(
                class = "dq-overview-eyebrow",
                "dataquieR overview"
              ),
              htmltools::tags$h1(
                class = "dq-overview-title",
                doc_title
              ),
              htmltools::tags$p(
                class = "dq-overview-subtitle",
                paste0(
                  author, ": ", header_subtitle,
                  " (", format(start_time), ")"
                )
              ),
              build_tab_nav(active_file)
            ),
            htmltools::tags$div(
              class = "dq-overview-main",
              summary_sections,
              build_report_list(href = href, title = title),
              if (has_legend) build_legend_block()
            ),
            htmltools::tags$div(
              class = "dq-overview-footer",
              build_footer(
                start_time = start_time,
                end_time = end_time
              )
            )
          )
        )
      )
    )
  }

  if (nchar(mod_label$label_modification_text) > 0) {
    notes_labels <- htmltools::div(
      htmltools::h3("Label modifications"),
      util_html_table(util_df_escape(mod_label$label_modification_table),
        dl_fn = "Label_modifications"
      )
    )
  } else {
    notes_labels <- htmltools::div()
  }

  logo_rel <- "logo.png"

  file.copy(
    system.file("logos",
      "dataquieR_48x48.png",
      package = packageName
    ),
    file.path(output_dir, logo_rel)
  )

  save_overview_html <- function(toc, file_name) {
    htmltools::save_html(
      htmltools::tagList(
        overview_css_dep,
        rmarkdown::html_dependency_jquery(),
        html_dependency_tippy(),
        html_dependency_dataquieR(iframe = FALSE),
        htmltools::tags$head(
          htmltools::tags$script(
            type = "text/javascript",
            "window.dq_report_by_overview = true"
          ),
          htmltools::tags$title(doc_title),
          htmltools::tags$link(
            rel = "icon",
            type = "image/png",
            href = logo_rel
          ),
          htmltools::tags$meta(
            name = "viewport",
            content = "width=device-width, initial-scale=1"
          )
        ),
        htmltools::HTML("<!-- done -->"),
        toc,
        htmltools::tags$div(
          class = "dq-overview-shell dq-overview-notes",
          notes_labels
        )
      ),
      file = file.path(output_dir, file_name)
    )
  }

  end_time <- Sys.time()

  lapply(overview_pages, function(page) {
    toc <- build_overview_toc(
      active_file = page$file,
      entity = page$entity,
      include_plot_summaries = page$include_plots,
      include_sunburst_summaries = identical(page$content, "sunburst"),
      include_dashboard = identical(page$content, "dashboard"),
      include_tb_summaries = identical(page$content, "table"),
      end_time = end_time
    )
    save_overview_html(toc, page$file)
  })

  util_write_renderinfo_js_json(
    output_dir,
    rep_id,
    start_time = start_time,
    end_time = end_time
  )

  invisible(NULL)
}

#' Create a Short Subtitle From a dq_report_by Call
#'
#' @param by_call a call to `dq_report_by()` (possibly unevaluated)
#'
#' @return character(1)
#' @noRd
util_by_header_from_args <- function(by_call) {
  util_stop_if_not("`by_call` must be supplied" = !missing(by_call))

  bcl <- rlang::call_match(
    call = by_call,
    fn = dq_report_by
  )

  args <- rlang::call_args(bcl)

  deparse1_safe <- function(x) {
    if (is.null(x)) {
      return(NULL)
    }
    paste(deparse(x), collapse = "")
  }

  clean_expr <- function(x) {
    if (is.null(x)) {
      return(NULL)
    }
    gsub('^"|"$', "", x)
  }

  as_label <- function(x) {
    if (is.null(x)) {
      return(NULL)
    }

    if (is.character(x)) {
      return(x)
    }

    if (is.symbol(x)) {
      return(as.character(x))
    }

    if (is.call(x) && identical(x[[1]], as.name("::")) && length(x) == 3) {
      return(as.character(x[[3]]))
    }

    clean_expr(deparse1_safe(x))
  }

  normalize_chr <- function(x) {
    if (is.null(x)) {
      return(NULL)
    }

    if (is.character(x)) {
      return(x)
    }

    if (is.symbol(x)) {
      return(as.character(x))
    }

    if (is.call(x) && identical(x[[1]], as.name("c"))) {
      vals <- unlist(lapply(as.list(x)[-1], function(xx) {
        if (is.character(xx)) {
          xx
        } else if (is.symbol(xx)) {
          as.character(xx)
        } else if (is.call(xx) &&
            identical(xx[[1]], as.name("::")) &&
            length(xx) == 3) {
          as.character(xx[[3]])
        } else {
          clean_expr(deparse1_safe(xx))
        }
      }), use.names = FALSE)
      return(vals)
    }

    clean_expr(deparse1_safe(x))
  }

  collapse_labels <- function(x) {
    if (!length(x)) {
      return("")
    }
    if (length(x) == 1) {
      return(x)
    }
    if (length(x) == 2) {
      return(paste(x, collapse = " & "))
    }

    paste0(
      paste(x[-length(x)], collapse = ", "),
      ", & ",
      x[length(x)]
    )
  }

  strata <- if ("strata_column" %in% names(args)) {
    as_label(args$strata_column)
  } else {
    NULL
  }

  segment <- if ("segment_column" %in% names(args)) {
    if (is.null(args$segment_column)) NULL else as_label(args$segment_column)
  } else {
    NULL
  }

  dimensions <- if ("dimensions" %in% names(args)) {
    normalize_chr(args$dimensions)
  } else {
    NULL
  }

  default_dims <- c("des", "int", "com", "con")
  full_dims <- c("des", "int", "com", "con", "acc")
  optional_dims <- c("com", "con", "acc")

  if (is.null(dimensions) || !length(dimensions)) {
    dimensions <- default_dims
  }

  dimensions <- unique(tolower(dimensions))

  if (identical(dimensions, "all")) {
    dimensions <- full_dims
  }

  dim_label <- function(x) {
    if (x %in% names(dims)) {
      unname(dims[[x]])
    } else {
      x
    }
  }

  parts <- character()

  if (!is.null(strata)) {
    parts <- c(parts, sprintf("Stratified by %s", strata))
  }

  if (!is.null(segment)) {
    parts <- c(
      parts,
      sprintf(
        "Split by Segments as Defined in %s in the Metadata",
        segment
      )
    )
  }

  present_opt <- intersect(optional_dims, dimensions)
  added_opt <- setdiff(present_opt, intersect(optional_dims, default_dims))
  missing_opt <- setdiff(optional_dims, present_opt)

  if (length(added_opt)) {
    parts <- c(
      parts,
      sprintf(
        "Including %s Checks",
        collapse_labels(vapply(added_opt, dim_label, character(1)))
      )
    )
  }

  if (length(missing_opt) && !identical(sort(missing_opt), "acc")) {
    parts <- c(
      parts,
      sprintf(
        "Without %s Checks",
        collapse_labels(vapply(missing_opt, dim_label, character(1)))
      )
    )
  }

  if (!length(parts)) {
    return("Data Quality Report Bundle")
  }

  paste("Data Quality Report Bundle", paste(parts, collapse = " and "))
}

#' Build a Compact Call Text for dq_report_by Overview Pages
#'
#' @param env environment containing `dq_report_by()` argument values
#'
#' @return character(1)
#' @noRd
util_compact_dq_report_by_call_from_env <- function(env = parent.frame()) {
  safe_get <- function(name) {
    if (!exists(name, envir = env, inherits = FALSE)) {
      return(NULL)
    }
    value <- tryCatch(get(name, envir = env, inherits = FALSE),
      error = function(e) NULL
    )
    if (rlang::is_missing(value)) {
      return(NULL)
    }
    value
  }

  quote_chr <- function(x) {
    paste0('"', gsub('(["\\\\])', "\\\\\\1", x), '"')
  }

  compact_path <- function(x) {
    if (!is.character(x) || length(x) != 1 || is.na(x)) {
      return(NULL)
    }
    base <- basename(x)
    if (!nzchar(base)) {
      base <- x
    }
    quote_chr(base)
  }

  compact_value <- function(x) {
    if (is.null(x)) {
      return("NULL")
    }
    if (is.character(x)) {
      if (length(x) == 0) {
        return("character(0)")
      }
      if (length(x) == 1) {
        return(quote_chr(x))
      }
      vals <- vapply(x, quote_chr, FUN.VALUE = character(1))
      return(paste0("c(", paste(vals, collapse = ", "), ")"))
    }
    if (is.logical(x) && length(x) == 1) {
      return(if (is.na(x)) "NA" else toupper(as.character(x)))
    }
    if (is.numeric(x) && length(x) == 1) {
      return(as.character(x))
    }
    if (is.symbol(x)) {
      return(as.character(x))
    }
    if (is.data.frame(x)) {
      return("study_data")
    }
    paste(deparse(x, nlines = 1), collapse = "")
  }

  args <- list(study_data = NULL)
  study_data <- safe_get("study_data")
  args[["study_data"]] <- if (is.character(study_data) &&
      length(study_data) == 1) {
    compact_path(study_data)
  } else {
    "study_data"
  }

  named_args <- c(
    "meta_data_v2",
    "dimensions",
    "segment_column",
    "strata_column",
    "segment_select",
    "strata_select",
    "selection_type",
    "subgroup",
    "resp_vars",
    "html_table_backend"
  )

  for (name in named_args) {
    value <- safe_get(name)
    if (is.null(value) && !name %in% c("segment_column", "strata_column")) {
      next
    }
    if (identical(name, "meta_data_v2")) {
      formatted <- compact_path(value)
      if (is.null(formatted)) {
        formatted <- compact_value(value)
      }
    } else {
      formatted <- compact_value(value)
    }
    args[[name]] <- formatted
  }

  rendered <- vapply(names(args), function(name) {
    if (identical(name, "study_data")) {
      args[[name]]
    } else {
      paste0(name, " = ", args[[name]])
    }
  }, FUN.VALUE = character(1))

  paste0("dq_report_by(", paste(rendered, collapse = ", "), ")")
}

#' Pick a Human-Readable dq_report_by Call for Overview Pages
#'
#' @return character(1)
#' @noRd
util_report_by_overview_call_text <- function(by_call = NULL,
  call_report_by = NULL,
  call_report_by_overview = NULL) {
  if (is.character(call_report_by_overview) &&
      length(call_report_by_overview) == 1 &&
      nzchar(trimws(call_report_by_overview))) {
    return(call_report_by_overview)
  }

  if (is.call(by_call)) {
    return(paste(deparse(by_call), collapse = "\n"))
  }

  if (is.character(call_report_by) &&
      length(call_report_by) == 1 &&
      nzchar(trimws(call_report_by))) {
    return(call_report_by)
  }

  "dq_report_by(...)"
}
