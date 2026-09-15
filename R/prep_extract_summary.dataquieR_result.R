#' Extract report summary from reports
#'
#' @param r [dataquieR_result] a result from a[dq_report2] report
#' @param ... not used
#'
#' @return [list] with two slots `Data` and `Table` with [data.frame]s
#'                featuring all metrics columns
#'                from the report `r`, the [STUDY_SEGMENT] and the [VAR_NAMES].
#'                In case of `Data`, the columns are formatted nicely but still
#'                with the standardized column names -- use
#'                `util_translate_indicator_metrics()` to rename them nicely. In
#'                case of `Table`, just as they are.
#' @family summary_functions
#' @seealso [prep_combine_report_summaries()]
#' @export
prep_extract_summary.dataquieR_result <- function(
  r,
  ...
) {
  te <- topenv(parent.frame(1)) # see https://stackoverflow.com/a/27870803
  if (!(isNamespace(te) && getNamespaceName(te) == "dataquieR")) {
    lifecycle::deprecate_soft(
      "2.1.0.9007",
      "prep_extract_summary.dataquieR_result()"
    )
  }
  util_stop_if_not(
    "Can only be called for results objects from a dq_report2 of class dataquieR_result" = # nolint: line_length_linter.
      inherits(r, "dataquieR_result")
  )

  call <- util_attr(r, "call", exact = TRUE)
  lb <- util_attr(call, "entity_name", exact = TRUE) # this exists for long
  result_check_id <- util_attr(r, CHECK_ID, exact = TRUE)

  try(
    rule_set <- util_attr(call, GRADING_RULESET, exact = TRUE),
    silent = TRUE
  )
  if (is.null(rule_set)) rule_set <- 0

  try(
    v0 <- util_attr(call, VAR_NAMES, exact = TRUE),
    silent = TRUE
  )
  if (!is.null(v0) &&
      length(result_check_id) == 1L &&
      !util_empty(result_check_id) &&
      all(util_empty(v0))) {
    v0 <- as.character(result_check_id)
  }
  if (is.null(v0)) v0 <- lb

  try(
    label_col <- util_attr(call, "label_col", exact = TRUE),
    silent = TRUE
  )
  if (is.null(label_col)) label_col <- LABEL

  # VariableGroupTable has one row per stable CHECK_ID rather than one row per
  # item. Convert its metric columns to the regular summary contract while
  # preserving that explicit group identity for classification and rendering.
  variable_group_table <- r[["VariableGroupTable"]]
  variable_group_metrics <- if (is.data.frame(variable_group_table)) {
    util_extract_indicator_metrics(variable_group_table)
  } else {
    data.frame()
  }
  if (ncol(variable_group_metrics) == 0 &&
      is.data.frame(variable_group_table)) {
    group_result_columns <- vapply(
      variable_group_table,
      function(column) is.numeric(column) || is.logical(column),
      FUN.VALUE = logical(1)
    )
    variable_group_metrics <- variable_group_table[, group_result_columns,
      drop = FALSE
    ]
  }

  if (nrow(variable_group_metrics) > 0 && ncol(variable_group_metrics) > 0) {
    util_stop_if_not(
      "VariableGroupTable must contain CHECK_ID" =
        CHECK_ID %in% colnames(variable_group_table),
      "VariableGroupTable CHECK_ID values must be non-empty" =
        !any(util_empty(variable_group_table[[CHECK_ID]]))
    )
    call_name <- util_attr(r, "cn", exact = TRUE)
    if (util_empty(call_name)) call_name <- "variable_group"

    group_ids <- as.character(variable_group_table[[CHECK_ID]])
    group_labels <- group_ids
    if (CHECK_LABEL %in% names(variable_group_table)) {
      group_labels <- as.character(variable_group_table[[CHECK_LABEL]])
      group_labels[util_empty(group_labels)] <- group_ids[
        util_empty(group_labels)
      ]
    }
    group_names <- group_ids
    colnames(variable_group_metrics) <- paste(
      call_name, colnames(variable_group_metrics), sep = "."
    )

    sseg <- util_attr(call, STUDY_SEGMENT, exact = TRUE)
    if (is.null(sseg)) sseg <- "Study"

    group_rulesets <- rep(rule_set, nrow(variable_group_table))
    if (GRADING_RULESET %in% names(variable_group_table)) {
      group_rulesets <- variable_group_table[[GRADING_RULESET]]
      group_rulesets[util_empty(group_rulesets)] <- rule_set
    }

    group_meta_data <- data.frame(
      VAR_NAMES = group_names,
      GRADING_RULESET = group_rulesets,
      STUDY_SEGMENT = rep(sseg, length(group_names)),
      stringsAsFactors = FALSE
    )
    group_meta_data[[label_col]] <- group_labels
    group_meta_data[[CHECK_ID]] <- group_ids

    variable_group_data <- lapply(variable_group_metrics, function(metric) {
      if (is.numeric(metric)) {
        return(util_round_to_decimal_places(metric))
      }
      metric
    })
    variable_group_data <- as.data.frame(variable_group_data,
      stringsAsFactors = FALSE
    )

    result <- list(
      Data = variable_group_data,
      Table = variable_group_metrics,
      meta_data = group_meta_data
    )
    result$Data[[STUDY_SEGMENT]] <- sseg
    result$Data[[VAR_NAMES]] <- group_names
    result$Data[[GRADING_RULESET]] <- group_rulesets
    result$Data[[label_col]] <- group_labels
    result$Data[[CHECK_ID]] <- group_ids
    result$Table[[STUDY_SEGMENT]] <- sseg
    result$Table[[VAR_NAMES]] <- group_names
    result$Table[[GRADING_RULESET]] <- group_rulesets
    result$Table[[label_col]] <- group_labels
    result$Table[[CHECK_ID]] <- group_ids
    class(result) <- "dq_report2_summary"
    return(result)
  }

  sts <-
    lapply(setNames(nm = v0), function(v) {
      all_cll <- lapply(setNames(nm = util_attr(r, "cn", exact = TRUE)), function(cll) { # nolint: line_length_linter.
        st <- r[["SummaryTable"]]
        if (is.data.frame(st)) {
          st <- util_extract_indicator_metrics(st)
          if (nrow(st) == 1 && ncol(st) > 0) {
            return(st)
          }
        }
        NULL
      })
      all_cll[vapply(all_cll, is.null, FUN.VALUE = logical(1))] <- NULL
      all_cll
    })

  sts[vapply(sts, length, FUN.VALUE = integer(1)) == 0] <- NULL
  sts <- lapply(sts, function(st) {
    st <- lapply(names(st), function(stnm) {
      colnames(st[[stnm]]) <-
        paste(stnm, colnames(st[[stnm]]), sep = ".")
      st[[stnm]]
    })
    r <- do.call(cbind, st)
    r
  })
  res <- util_rbind(data_frames_list = sts)

  sseg <- util_attr(call, STUDY_SEGMENT, exact = TRUE)

  if (is.null(sseg)) sseg <- "Study"

  res[[STUDY_SEGMENT]] <- rep(sseg, nrow(res))
  res[[VAR_NAMES]] <- rep(v0, nrow(res))
  res_raw <- res

  counts <- vapply(colnames(res),
    FUN.VALUE = logical(1),
    FUN = function(x) {
      util_stop_if_not(length(x) == 1)
      x <- sub("^[^\\.]+\\.", "", x)
      nm <- strsplit(x, "_", fixed = TRUE)[[1]]
      if (length(nm) >= 2) {
        identical(nm[[1]], "NUM")
      } else {
        FALSE
      }
    }
  )
  res_raw[, counts] <- lapply(res[, counts, drop = FALSE], as.numeric)
  res[, counts] <- lapply(lapply(res[, counts, drop = FALSE], as.numeric),
    scales::number,
    accuracy = 1
  )

  percentages <- vapply(colnames(res),
    FUN.VALUE = logical(1),
    FUN = function(x) {
      util_stop_if_not(length(x) == 1)
      x <- sub("^[^\\.]+\\.", "", x)
      nm <- strsplit(x, "_", fixed = TRUE)[[1]]
      if (length(nm) >= 2) {
        identical(nm[[1]], "PCT")
      } else {
        FALSE
      }
    }
  )
  res_raw[, percentages] <- lapply(res_raw[, percentages, drop = FALSE], as.numeric) # nolint: line_length_linter.
  res[, percentages] <- lapply(lapply(res[, percentages, drop = FALSE], as.numeric), # nolint: line_length_linter.
    scales::percent, ,
    scale = 1, accuracy = 0.01
  )

  flags <- vapply(colnames(res),
    FUN.VALUE = logical(1),
    FUN = function(x) {
      util_stop_if_not(length(x) == 1)
      x <- sub("^[^\\.]+\\.", "", x)
      nm <- strsplit(x, "_", fixed = TRUE)[[1]]
      if (length(nm) >= 2) {
        identical(nm[[1]], "FLG")
      } else {
        FALSE
      }
    }
  )
  res_raw[, flags] <- lapply(res_raw[, flags, drop = FALSE], as.logical)
  res[, flags] <- lapply(
    lapply(res[, flags, drop = FALSE], as.logical),
    ifelse, "T", "F"
  )

  res <- res[, sort(colnames(res)), drop = FALSE]
  res_raw <- res_raw[, sort(colnames(res_raw)), drop = FALSE]

  meta_data <- data.frame(
    VAR_NAMES = v0,
    GRADING_RULESET = rule_set,
    STUDY_SEGMENT = sseg
  )

  meta_data[[label_col]] <- lb

  if (length(result_check_id) == 1L && !util_empty(result_check_id)) {
    result_check_id <- as.character(result_check_id)
    res[[CHECK_ID]] <- result_check_id
    res_raw[[CHECK_ID]] <- result_check_id
    meta_data[[CHECK_ID]] <- result_check_id
  }

  r <- list(Data = res, Table = res_raw, meta_data = meta_data)
  class(r) <- "dq_report2_summary"
  r
}
