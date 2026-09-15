#' Generate a report summary table
#'
#' @param object a square result set
#' @param aspect an aspect/problem category of results
#' @param FUN function to apply to the cells of the result table
#' @param ... not used
#' @param collapse passed to `FUN`
#'
#' @return a summary of a `dataquieR` report
#' @export
#' @examples
#' \dontrun{
#' util_html_table(summary(report),
#'   filter = "top", options = list(scrollCollapse = TRUE, scrollY = "75vh"),
#'   is_matrix_table = TRUE, rotate_headers = TRUE
#' )
#' }
summary.dataquieR_resultset2 <- function(object, aspect = c("applicability", "error", "anamat", "indicator_or_descriptor"), # nolint: line_length_linter.
  FUN,
  collapse = "\n<br />\n",
  ...) {
  my_storr_object <- util_get_storr_object_from_report(object)
  repsum <- util_attr(object, "repsum", exact = TRUE)
  if (!is.null(repsum) &&
    identical(rlang::call_args_names(rlang::call_match()), "object") &&
    identical(
      util_attr(repsum, "rule_digest", exact = TRUE),
      rlang::hash(list(
        util_get_rule_sets(),
        util_get_ruleset_formats()
      ))
    )
  ) {
    return(repsum)
  }
  if (missing(FUN)) {
    util_stop_if_not(
      "aspect is only supported for specific FUN values" =
        missing(aspect)
    )

    meta_data <- util_attr(object, "meta_data", exact = TRUE)
    label_col <- util_attr(object, "label_col", exact = TRUE)
    labels_of_var_names_in_report <- setNames(
      meta_data[[label_col]],
      nm = meta_data[[VAR_NAMES]]
    )

    var_names_of_labels_in_report <- setNames(
      nm = meta_data[[label_col]],
      meta_data[[VAR_NAMES]]
    )

    labels <- util_get_labels_grading_class()
    colors <- util_get_colors()
    names(labels) <- paste0("cat", names(labels))
    names(colors) <- paste0("cat", names(colors))
    if (!identical(names(labels), names(colors)) ||
        !all(paste0("cat", seq_len(5)) %in% names(colors))) {
      util_warning(
        c(
          "Could not find enough categories in custom format file,",
          "falling back to default"
        ),
        applicability_problem = TRUE,
        intrinsic_applicability_problem = FALSE
      )
      withr::local_options(list(
        dataquieR.grading_formats =
          system.file("grading_formats.xlsx",
            package = "dataquieR"
          ),
        dataquieR.grading_rulesets =
          system.file("grading_rulesets.xlsx",
            package = "dataquieR"
          )
      ))
      labels <- util_get_labels_grading_class()
      colors <- util_get_colors()
      names(labels) <- paste0("cat", names(labels))
      names(colors) <- paste0("cat", names(colors))
    }

    labels <- as.list(labels)
    colors <- as.list(colors)

    labels[["catNA"]] <- htmltools::HTML("\u00A0") # this is nbsp, but it reads " ", even in the HTML source code # nolint: line_length_linter.
    labels[["NA"]] <- htmltools::HTML("\u00A0")
    colors[["catNA"]] <- "#ffffff00" # Transparent fallback color.
    colors[["NA"]] <- "#ffffff00" # Transparent fallback color.

    icons <- lapply(labels, function(x) htmltools::HTML("\u00A0")) # not available, yet # nolint: line_length_linter.

    order_of <- suppressWarnings(setNames(as.integer(gsub(
      "^cat", "",
      util_as_cat(names(colors))
    )), nm = names(colors)))

    filter_of <- labels

    if (!is.null(my_storr_object)) {
      namespace <- util_get_storr_summ_namespace(my_storr_object)
      all_sums <- my_storr_object$mget(
        my_storr_object$list(
          namespace = namespace
        ),
        namespace = namespace
      )
    } else {
      all_sums <- lapply(object, util_attr, "r_summary", exact = TRUE)
    }

    result <- util_rbind(data_frames_list = all_sums)

    variable_group_call_names <- character()
    if (all(c("call_names", CHECK_ID) %in% colnames(result))) {
      variable_group_call_names <- unique(as.character(
        result$call_names[!util_empty(result[[CHECK_ID]])]
      ))
    }
    # Keep reading matrix_list: external integrations may still use its alias
    # map even though current summaries are built from normalized long rows.
    matrix_list <- util_attr(object, "matrix_list", exact = TRUE)

    meta_data_cross_item <- util_normalize_cross_item(
      meta_data = meta_data,
      meta_data_cross_item = util_report_meta_data_cross_item(object),
      label_col = label_col
    )
    result <- util_summary_normalize_variable_group_rows(
      result = result,
      meta_data_cross_item = meta_data_cross_item,
      label_col = label_col
    )

    group_meta_columns <- intersect(
      c(VAR_NAMES, GRADING_RULESET, STUDY_SEGMENT, label_col, CHECK_ID),
      names(result)
    )
    variable_group_meta_data <- if (
      all(c(VAR_NAMES, label_col) %in% names(result))) {
      result[
        !(as.character(result[[VAR_NAMES]]) %in%
            as.character(meta_data[[VAR_NAMES]])) &
          !util_empty(result[[label_col]]),
        group_meta_columns,
        drop = FALSE
      ]
    } else {
      data.frame()
    }
    variable_group_meta_data <- variable_group_meta_data[
      !duplicated(as.character(variable_group_meta_data[[VAR_NAMES]])),
      ,
      drop = FALSE
    ]
    if (GRADING_RULESET %in% names(variable_group_meta_data)) {
      variable_group_meta_data[[GRADING_RULESET]][
        util_empty(variable_group_meta_data[[GRADING_RULESET]])
      ] <- "0"
    }
    summary_meta_data <- util_rbind(
      data_frames_list = list(meta_data, variable_group_meta_data)
    )
    labels_of_var_names_in_report <- setNames(
      summary_meta_data[[label_col]],
      nm = summary_meta_data[[VAR_NAMES]]
    )
    var_names_of_labels_in_report <- setNames(
      nm = summary_meta_data[[label_col]],
      summary_meta_data[[VAR_NAMES]]
    )

    if (any(is.na(result$n_classes))) {
      result[is.na(result$n_classes), "n_classes"] <- 5
    }

    ordered_call_names <- colnames(object)
    ordered_var_names <- var_names_of_labels_in_report[rownames(object)]

    # Historical wide summary-table construction removed here. Inspect with
    # `git show 8fb28fb01b -- R/summary.dataquieR_resultset2.R`.

    indicator_metric <- NULL

    if (!prod(dim(result))) {
      cls <-
        c(
          "VAR_NAMES", "class", "indicator_metric", "value", "values_raw",
          "n_classes", "STUDY_SEGMENT", "call_names", "function_name"
        )
      result <-
        as.data.frame(setNames(rep(list(character(0)), length(cls)), nm = cls))
    }

    if (!"class" %in% colnames(result)) {
      result$class <- rep(NA, nrow(result))
    }

    # Class handling now comes from `prep_summary_to_classes()`.
    result$class <- util_as_cat(result$class)

    result <- result[!is.na(result[[VAR_NAMES]]), , drop = FALSE] # remove all non-variable-related stuff, not yet supported, here. # nolint: line_length_linter.

    # compute stopped_functions
    stopped_functions <- vapply(object,
      inherits,
      what = "dataquieR_NULL",
      FUN.VALUE = logical(1)
    )

    # Historical Total-column handling removed with the wide-table prototype.

    spreaded_result_df <- NA
    class(spreaded_result_df) <- c("dataquieR_summary")


    function_alias_map <- util_attr(matrix_list, "function_alias_map",
      exact = TRUE
    )
    function_alias_map <- unique(function_alias_map[, c("name", "alias"), drop = FALSE]) # nolint: line_length_linter.
    alias_names <- setNames(function_alias_map$name, nm = function_alias_map$alias) # nolint: line_length_linter.

    result <- util_metrics_to_classes(
      result,
      summary_meta_data
    ) # re-classify
    result <- util_summary_normalize_variable_group_rows(
      result = result,
      meta_data_cross_item = meta_data_cross_item,
      label_col = label_col
    )

    if (all(c(VAR_NAMES, label_col) %in% names(result))) {
      is_group_result <- !as.character(result[[VAR_NAMES]]) %in%
        as.character(meta_data[[VAR_NAMES]])
      variable_group_meta_data <- result[
        is_group_result & !util_empty(result[[label_col]]),
        group_meta_columns,
        drop = FALSE
      ]
      variable_group_meta_data <- variable_group_meta_data[
        !duplicated(as.character(variable_group_meta_data[[VAR_NAMES]])),
        ,
        drop = FALSE
      ]
      summary_meta_data <- util_rbind(
        data_frames_list = list(meta_data, variable_group_meta_data)
      )
      labels_of_var_names_in_report <- setNames(
        summary_meta_data[[label_col]],
        nm = summary_meta_data[[VAR_NAMES]]
      )
    }

    rowmaxes <- util_compute_rowmaxes(
      result = result,
      labels = labels,
      colors = colors,
      order_of = order_of,
      filter_of = filter_of,
      labels_of_var_names_in_report =
        labels_of_var_names_in_report
    )


    this <- new.env(parent = emptyenv())

    for (prop in c(
      "result", # long format of the summary, column indicator_metric contains CAT_ and MSG_, which are special process classes # nolint: line_length_linter.
      "rowmaxes", # maximum category for each row in the summary table
      # and all the ellipsis_arguments from
      # util_get_html_cell_for_result, above:
      "labels_of_var_names_in_report",
      "alias_names",
      "stopped_functions",
      "colors",
      "labels",
      "order_of",
      "filter_of",
      "ordered_call_names",
      "ordered_var_names",
      "variable_group_call_names"
    )) {
      this[[prop]] <- get(prop)
    }

    # needed for the summary downlaod button
    this[["title"]] <- util_attr(object, "title", exact = TRUE)
    this[["subtitle"]] <- util_attr(object, "subtitle", exact = TRUE)

    # needed to pretty print
    this[["meta_data"]] <- meta_data
    this[["summary_meta_data"]] <- summary_meta_data
    this[["meta_data_cross_item"]] <- meta_data_cross_item

    # "label_col", # needed to pretty print
    this[["label_col"]] <- label_col

    # needed for pretty plot
    this[["rownames_of_report"]] <- rownames(object)
    this[["colnames_of_report"]] <- colnames(object)

    attr(spreaded_result_df, "this") <- this
    attr(spreaded_result_df, "rule_digest") <-
      rlang::hash(list(
        util_get_rule_sets(),
        util_get_ruleset_formats()
      ))
    spreaded_result_df
  } else {
    f <- substitute(FUN)
    FUN <- force(eval(f, enclos = parent.frame(), envir = environment()))
    util_stop_if_not(inherits(object, "dataquieR_resultset2"))
    aspect <- util_match_arg(aspect, several_ok = FALSE)

    rn_obj <- rownames(object)
    cn_obj <- colnames(object)

    rn_to_use <- vapply(rn_obj, function(rn) {
      any(vapply(object[rn, , drop = TRUE],
          inherits,
          "dataquieR_result",
          FUN.VALUE = logical(1)
        ))
    }, FUN.VALUE = logical(1))

    cn_to_use <-
      vapply(cn_obj, function(cn) {
        x <- object[, cn, drop = FALSE]
        length(x) > 0 &&
          any(!endsWith(names(x), ".[ALL]")) &&
          all(vapply(names(x), function(listname) {
            any(endsWith(listname, paste0(".", rownames(object))))
          }, FUN.VALUE = logical(1)))
      }, FUN.VALUE = logical(1))

    rn_obj <- rn_obj[rn_to_use]
    cn_obj <- cn_obj[cn_to_use]

    if (!any(rn_to_use) || !any(cn_to_use)) {
      return(
        matrix(
          nrow = 0, ncol = 2,
          dimnames = list(c(), c(VAR_NAMES, STUDY_SEGMENT))
        )
      )
    }

    do.call(rbind, lapply(setNames(nm = rn_obj),
      FUN = function(rn) {
        vapply(setNames(nm = cn_obj),
          FUN.VALUE = character(1),
          FUN = function(cn, aspect, collapse) {
            r <- FUN(object[rn, cn, drop = TRUE],
              aspect = aspect,
              collapse = collapse,
              rn = rn,
              cn = cn
            )
            # Historical debug retry removed here. Inspect commit 214dd76a7d
            # before restoring interactive FUN debugging.
            r
          },
          aspect = aspect,
          collapse = collapse
        )
      }
    )) # TOOD: "Any-Issue" Column
  }
}


#' Internal helper: reclassify dataquieR summary
#'
#' @noRd
util_reclassify_dataquieR_summary <- function(x) {
  util_stop_if_not(inherits(x, "dataquieR_summary"))

  if (identical(
    util_attr(x, "rule_digest", exact = TRUE),
    rlang::hash(list(
      util_get_rule_sets(),
      util_get_ruleset_formats()
    ))
  )) {
    return(x)
  }

  original_this <- util_attr(x, "this", exact = TRUE)
  this <- rlang::env_clone(original_this)
  rs_table_long <- this$result
  meta_data <- this$summary_meta_data
  if (is.null(meta_data)) meta_data <- this$meta_data

  this$result <-
    util_metrics_to_classes(rs_table_long, meta_data)

  this$rowmaxes <- util_compute_rowmaxes(
    result = this$result,
    labels = this$labels,
    colors = this$colors,
    order_of = this$order_of,
    filter_of = this$filter_of,
    labels_of_var_names_in_report =
      this$labels_of_var_names_in_report
  )

  attr(x, "this") <- this

  attr(x, "rule_digest") <-
    rlang::hash(list(
      util_get_rule_sets(),
      util_get_ruleset_formats()
    ))

  x
}

#' Internal helper: compute rowmaxes
#'
#' @noRd
util_compute_rowmaxes <- function(result,
  labels,
  colors,
  order_of,
  filter_of,
  labels_of_var_names_in_report) {
  indicator_metric <- NULL # make R CMD check happy
  if (!VAR_NAMES %in% colnames(result)) {
    result[[VAR_NAMES]] <- rep(NA_character_, nrow(result))
  }
  if (!"class" %in% colnames(result)) {
    result[["class"]] <- rep(NA_character_, nrow(result))
  }
  rowmaxes <- result %>%
    dplyr::filter(!startsWith(as.character(indicator_metric), "CAT_") &
        !startsWith(as.character(indicator_metric), "MSG_")) %>%
    dplyr::group_by(VAR_NAMES) %>%
    dplyr::summarise(rowmax = suppressWarnings( ######
      max(util_as_cat(class), na.rm = TRUE)
    ))

  rowmax <- paste(rowmaxes$rowmax) # also make NA -> "NA"

  rowmaxes$label <- vapply(labels[rowmax], identity,
    FUN.VALUE = character(1)
  )

  rowmaxes$color <- vapply(colors[rowmax], identity,
    FUN.VALUE = character(1)
  )

  rowmaxes$order <- vapply(order_of[rowmax], identity,
    FUN.VALUE = integer(1)
  )

  rowmaxes$filter <- vapply(filter_of[rowmax], identity,
    FUN.VALUE = character(1)
  )

  report_label <- function(vn) {
    if (length(vn) != 1 || is.na(vn)) {
      return("NA")
    }
    if (!(vn %in% names(labels_of_var_names_in_report))) {
      return(vn)
    }
    label <- labels_of_var_names_in_report[[vn]]
    if (length(label) != 1 || is.na(label)) {
      return(vn)
    }
    label
  }

  get_cell_text <- function(lb, cl, o, f, vn) {
    # util_message("lb = %s, cl = %s, o = %s, f = %s, vn = %s",
    #              sQuote(lb), sQuote(cl), sQuote(o), sQuote(f), sQuote(vn))
    link <- util_generate_anchor_link(report_label(vn),
      "",
      title =
        htmltools::HTML(as.character(lb))
    )
    link$attribs$style <- c(
      link$attribs$style,
      "text-decoration:none;display:block;"
    )
    fg_color <- util_get_fg_color(cl)
    link$attribs$style <- c(link$attribs$style, sprintf("color:%s;", fg_color))

    paste0(
      htmltools::tagList(
        # Historical row and column labels are not rendered inside the cell.
        htmltools::pre(
          onclick = "",
          style = sprintf("height: 100%%; min-height: 2em; margin: 0em; padding: 0em; background: %s; cursor: pointer; text-align: center;", cl), # nolint: line_length_linter.
          sort = o,
          filter = f,
          title = "",
          link
        )
      ),
      collapse = "\n"
    )
  }

  rowmaxes$cell_text <- setNames(
    vapply(
      mapply(
        SIMPLIFY = FALSE,
        FUN = get_cell_text,
        lb = rowmaxes$label,
        cl = rowmaxes$color,
        o = rowmaxes$order,
        f = rowmaxes$filter,
        vn = rowmaxes$VAR_NAMES
      ),
      FUN = identity,
      FUN.VALUE = character(1)
    ),
    nm = vapply(rowmaxes$VAR_NAMES, report_label, FUN.VALUE = character(1))
  )

  return(rowmaxes)
}

#' Normalize variable-group rows in result summaries
#'
#' @noRd
util_summary_normalize_variable_group_rows <- function(
  result,
  meta_data_cross_item,
  label_col
) {
  detail_label_column <- ".variable_group_result_label"
  if (!is.data.frame(result) ||
      !all(c(VAR_NAMES, CHECK_ID) %in% colnames(result))) {
    return(result)
  }

  group_rows <- !util_empty(result[[CHECK_ID]])
  if (!any(group_rows)) {
    return(result)
  }

  if (!detail_label_column %in% colnames(result)) {
    result[[detail_label_column]] <- rep(NA_character_, nrow(result))
  }
  if (!label_col %in% colnames(result)) {
    result[[label_col]] <- as.character(result[[VAR_NAMES]])
  }
  missing_detail <- group_rows & util_empty(result[[detail_label_column]])
  result[[detail_label_column]][missing_detail] <-
    as.character(result[[label_col]][missing_detail])

  group_ids <- as.character(result[[CHECK_ID]][group_rows])
  cross_item_rows <- if (is.data.frame(meta_data_cross_item) &&
      all(c(CHECK_ID, CHECK_LABEL) %in% colnames(meta_data_cross_item))) {
    match(group_ids, as.character(meta_data_cross_item[[CHECK_ID]]))
  } else {
    rep(NA_integer_, length(group_ids))
  }
  group_labels <- as.character(result[[label_col]][group_rows])
  group_labels[util_empty(group_labels)] <- group_ids[util_empty(group_labels)]
  known_groups <- !is.na(cross_item_rows)
  known_labels <- as.character(
    meta_data_cross_item[[CHECK_LABEL]][cross_item_rows[known_groups]]
  )
  use_known_label <- !util_empty(known_labels)
  group_labels[which(known_groups)[use_known_label]] <-
    known_labels[use_known_label]
  group_labels[util_empty(group_labels)] <- group_ids[util_empty(group_labels)]

  result[[VAR_NAMES]][group_rows] <- group_ids
  result[[label_col]][group_rows] <- group_labels
  result
}
