#' Create a dashboard-table from a report summary
#'
#' @param repsum [data.frame] a report summary from `summary(report)`
#' @param folder_of_report a named vector with the location of variable and
#'                         `call_names`
#' @inheritParams util_filter_repsum
#'
#' @family html
#' @concept process
#' @noRd
util_dashboard_table <- function(repsum, folder_of_report = NULL,
  vars_to_include = "study") {
  this <- util_attr(repsum, "this", exact = TRUE)
  summary_meta_data <- this$summary_meta_data
  if (is.null(summary_meta_data)) summary_meta_data <- this$meta_data
  big_table <- util_filter_repsum(
    this$result,
    vars_to_include = vars_to_include,
    meta_data = summary_meta_data,
    rownames_of_report = this$rownames_of_report,
    label_col = this$label_col,
    variable_group_call_names = this$variable_group_call_names,
    study_var_names = this$meta_data[[VAR_NAMES]]
  )
  if (identical(vars_to_include, "variable_group")) {
    big_table <- util_summary_most_specific_group_metrics(big_table)
  }
  tb <- suppressWarnings(
    big_table[!is.na(as.numeric(big_table$values_raw)), , drop = FALSE]
  )
  label_col <- this$label_col
  meta_data <- summary_meta_data

  rowmaxes <- this$rowmaxes
  if (identical(vars_to_include, "variable_group")) {
    rowmaxes <- util_compute_rowmaxes(
      result = big_table,
      labels = this$labels,
      colors = this$colors,
      order_of = this$order_of,
      filter_of = this$filter_of,
      labels_of_var_names_in_report = this$labels_of_var_names_in_report
    )
  }
  var_classes <- setNames(util_as_integer_cat(
    rowmaxes$rowmax
  ), nm = rowmaxes[[VAR_NAMES]])
  var_classes <- ordered(var_classes,
    levels = names(util_get_labels_grading_class()),
    labels = util_get_labels_grading_class()
  )

  tb[[label_col]] <-
    prep_map_labels(
      tb[[VAR_NAMES]],
      meta_data = meta_data,
      to = label_col,
      ifnotfound = tb[[VAR_NAMES]]
    )

  indicator_metric <- NULL # to make R CMD check happy

  if (length(tb) > 0 && prod(dim(tb)) > 0) {
    tb <- subset(
      tb,
      !startsWith(indicator_metric, "CAT_") &
        !startsWith(indicator_metric, "MSG_")
    )
  }

  tb$var_class <- var_classes[tb[[VAR_NAMES]]]

  cols <- util_get_colors()[tb$class]
  labs <- util_get_labels_grading_class()[tb$class]
  fg_cols <- util_get_fg_color(cols)

  tb$`Class` <- ifelse(is.na(labs), "", paste0(
    labs
  ))

  tb$`Metric` <- util_translate_indicator_metrics(tb$indicator_metric)
  tb$`Call` <- vapply(tb$call, util_alias2caption,
    long = TRUE,
    FUN.VALUE = character(1)
  )
  tb$n_classes <- NULL

  meta_data[[STUDY_SEGMENT]] <- NULL # delete, it is already in tb

  merge_keys <- c(VAR_NAMES, label_col)
  if (identical(vars_to_include, "variable_group")) {
    merge_keys <- c(merge_keys, CHECK_ID)
  }
  tb <- merge(tb, meta_data, by = intersect(
    intersect(
      colnames(tb),
      colnames(meta_data)
    ),
    merge_keys
  ))

  tb$class <- ordered(tb$class,
    levels = names(util_get_labels_grading_class()),
    labels = util_get_labels_grading_class()
  )

  my_order1 <- c(
    label_col,
    VAR_NAMES,
    STUDY_SEGMENT,
    "Call",
    "Metric",
    "value",
    "Class",
    "call_names",
    "values_raw",
    "function_name",
    "indicator_metric",
    "class",
    "var_class",
    LABEL,
    DATA_TYPE,
    SCALE_LEVEL
  )

  my_order2 <- c(
    STANDARDIZED_VOCABULARY_TABLE,
    MISSING_LIST_TABLE,
    HARD_LIMITS,
    DETECTION_LIMITS,
    SOFT_LIMITS,
    DISTRIBUTION,
    DECIMALS,
    GROUP_VAR_OBSERVER,
    GROUP_VAR_DEVICE,
    TIME_VAR,
    PART_VAR,
    VARIABLE_ROLE,
    VARIABLE_ORDER,
    LONG_LABEL,
    "ELEMENT_HOMOGENITY_CHECKTYPE",
    UNIVARIATE_OUTLIER_CHECKTYPE,
    N_RULES,
    LOCATION_METRIC,
    LOCATION_RANGE,
    PROPORTION_RANGE,
    "REPEATED_MEASURES_VARS",
    CO_VARS,
    END_DIGIT_CHECK,
    VALUE_LABEL_TABLE,
    MISSING_LIST,
    JUMP_LIST
  )

  my_order <- unique(c(
    intersect(my_order1, colnames(tb)),
    sort(setdiff(colnames(tb), union(my_order1, my_order2))),
    intersect(my_order2, colnames(tb))
  ))

  tb <- tb[, my_order, drop = FALSE]

  tb <- util_add_links_to_summary_table(tb,
    this,
    folder_of_report = folder_of_report,
    vars_to_include = vars_to_include
  )

  return(util_attach_attr(tb,
      label_col = label_col,
      vars_to_include = vars_to_include
    ))
}

#' Internal helper: add links to summary table
#'
#' @noRd
util_add_links_to_summary_table <- function(tb, this, folder_of_report = NULL,
  vars_to_include = c("study")) {
  label_col <- this$label_col
  while (!is.null(util_attr(label_col, "orig_label_col", exact = TRUE))) {
    label_col <- util_attr(label_col, "orig_label_col", exact = TRUE)
  }
  old_label <- if (label_col %in% names(tb)) {
    tb[[label_col]]
  } else {
    rep(NA_character_, nrow(tb))
  }
  new_label <- if (this$label_col %in% names(tb)) {
    tb[[this$label_col]]
  } else {
    rep(NA_character_, nrow(tb))
  }
  map_labels_or_na <- function(to) {
    tryCatch(
      prep_map_labels(tb[[VAR_NAMES]],
        meta_data = this$meta_data,
        to = to,
        ifnotfound = as.character(tb[[VAR_NAMES]])
      ),
      error = function(e) rep(NA_character_, nrow(tb))
    )
  }
  mapped_label <- map_labels_or_na(label_col)
  mapped_display_label <- map_labels_or_na(this$label_col)
  is_study_variable <- as.character(tb[[VAR_NAMES]]) %in%
    as.character(this$meta_data[[VAR_NAMES]])
  tb[[label_col]] <- ifelse(
    is_study_variable & !is.na(mapped_label),
    mapped_label,
    old_label
  )
  tb[[this$label_col]] <- ifelse(
    is_study_variable & !is.na(mapped_display_label),
    mapped_display_label,
    new_label
  )
  missing_link_label <- util_empty(tb[[label_col]])
  tb[[label_col]][missing_link_label] <-
    as.character(tb[[this$label_col]][missing_link_label])
  missing_link_label <- util_empty(tb[[label_col]])
  tb[[label_col]][missing_link_label] <-
    as.character(tb[[VAR_NAMES]][missing_link_label])
  missing_display_label <- util_empty(tb[[this$label_col]])
  tb[[this$label_col]][missing_display_label] <-
    as.character(tb[[label_col]][missing_display_label])
  if (nrow(tb) == 0) {
    tb$href <- character(0)
    tb$popup_href <- character(0)
    tb$title <- character(0)
  } else {
    tb$href <-
      paste0(
        "VAR_", prep_link_escape(tb[[label_col]],
          html = TRUE
        ),
        ".html#",
        prep_link_escape(as.character(tb[[label_col]])),
        ".", tb[["call_names"]]
      )

    # showDataquieRResult("VAR_AGE0.html#nm=acc_distributions_only.AGE_0",
    # "VAR_AGE0.html#v00003: Age B/L", "Gemüsesuppe")
    tb$popup_href <-
      paste0(
        "VAR_", prep_link_escape(tb[[label_col]],
          html = TRUE
        ),
        ".html#nm=", tb$call_names, ".",
        as.character(tb[[label_col]])
      )

    tb$title <- paste0(
      tb[[this$label_col]],
      ": ",
      vapply(tb$call_names, util_alias2caption, long = TRUE, FUN.VALUE = character(1)), # nolint: line_length_linter.
      ifelse(!is.na(tb$title_ind), paste0(
        " (",
        tb$title_ind,
        ")"
      ), "")
    )
  }

  if (identical(vars_to_include, "variable_group") &&
      VAR_NAMES %in% colnames(tb)) {
    tb$href[] <- NA_character_
    tb$popup_href[] <- NA_character_
    group_ids <- if (CHECK_ID %in% colnames(tb)) {
      as.character(tb[[CHECK_ID]])
    } else {
      rep(NA_character_, nrow(tb))
    }
    group_href <- util_cross_item_hrefs(
      group_ids,
      this$meta_data_cross_item
    )
    has_group_href <- !is.na(group_href) & nzchar(group_href)
    tb$href[has_group_href] <- group_href[has_group_href]
    tb$popup_href[has_group_href] <- paste0(
      sub("#.*$", "", group_href[has_group_href]),
      "#nm=",
      util_variable_group_popup_id(group_ids[has_group_href])
    )
    overview_href <- util_contradictions_overview_href()
    links_to_overview <- has_group_href & group_href == overview_href
    tb$popup_href[links_to_overview] <-
      util_contradictions_overview_popup_href()
  }

  if (!is.null(folder_of_report)) {
    resolve_report_folder <- function(label, display_label, var_name,
      origin, original_var_name,
      variable_label, new_label,
      call_name) {
      variable_candidates <- unique(c(
        label,
        display_label,
        var_name,
        original_var_name,
        variable_label,
        new_label
      ))
      variable_candidates <- variable_candidates[
        !is.na(variable_candidates) & nzchar(variable_candidates)
      ]
      origin_candidates <- unique(c(origin, NA_character_))
      full_candidates <- unique(unlist(lapply(origin_candidates, function(o) {
        if (is.na(o) || !nzchar(o)) {
          variable_candidates
        } else {
          c(
            paste0(o, "-", variable_candidates),
            paste0(o, " - ", variable_candidates)
          )
        }
      }), use.names = FALSE))
      keys <- unique(gsub("\\s+", "", paste0(
        full_candidates,
        ".",
        call_name
      )))
      folder_names <- gsub("\\s+", "", names(folder_of_report))
      names(folder_names) <- names(folder_of_report)
      keys <- keys[!is.na(keys) & nzchar(keys)]
      match <- folder_of_report[names(folder_names)[folder_names %in% keys]]
      if (!length(match)) {
        match <- folder_of_report[names(folder_of_report) %in% keys]
      }
      if (length(match)) {
        return(unname(match[[1]]))
      }
      NA_character_
    }
    optional_col <- function(x) {
      if (x %in% names(tb)) {
        as.character(tb[[x]])
      } else {
        rep(NA_character_, nrow(tb))
      }
    }
    report_folders <- mapply(
      resolve_report_folder,
      label = tb[[label_col]],
      display_label = tb[[this$label_col]],
      var_name = tb[[VAR_NAMES]],
      origin = optional_col("..Origin"),
      original_var_name = optional_col("Original_var_name"),
      variable_label = optional_col("Variable Label.1"),
      new_label = optional_col("new_label"),
      call_name = tb[["call_names"]],
      USE.NAMES = FALSE
    )
    has_report_folder <- !is.na(report_folders) & nzchar(report_folders)

    unresolved_empty_output <-
      !has_report_folder &
      "indicator_metric" %in% colnames(tb) &
      as.character(tb[["indicator_metric"]]) == "EMPTY_OUTPUT"
    tb$href[unresolved_empty_output] <- ""
    tb$popup_href[unresolved_empty_output] <- ""

    tb$href <-
      ifelse(has_report_folder,
        paste0(
          report_folders,
          "/.report/",
          tb$href
        ),
        tb$href
      )
    tb$popup_href <-
      ifelse(has_report_folder,
        paste0(
          report_folders,
          "/.report/",
          tb$popup_href
        ),
        tb$popup_href
      )
  }

  has_ssi_rows <- COMPUTED_VARIABLE_ROLE %in% colnames(tb) &&
    any(!util_empty(tb[[COMPUTED_VARIABLE_ROLE]]))

  if (!identical(vars_to_include, "variable_group") &&
      (identical(vars_to_include, "ssi") || has_ssi_rows)) {
    scale_href <- tryCatch(
      util_ssi_computed_variable_hrefs(
        tb[[VAR_NAMES]],
        meta_data = this$meta_data,
        meta_data_cross_item = this$meta_data_cross_item
      ),
      error = function(e) rep(NA_character_, nrow(tb))
    )
    has_scale_href <- !is.na(scale_href) & nzchar(scale_href)
    tb$href[has_scale_href] <- scale_href[has_scale_href]
    tb$popup_href[has_scale_href] <- scale_href[has_scale_href]
  }

  util_attach_attr(tb, orig_label_col = tb[[this$label_col]])
}
