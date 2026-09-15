#' Combine `ReportSummaryTable` outputs
#'
#' @param ... `ReportSummaryTable` objects to combine.
#'
#' @description
#' Using this `rbind` implementation, you can combine different
#' heatmap-like results of the class `ReportSummaryTable`.
#'
#' @seealso [base::rbind.data.frame]
#'
#' @export
#'
rbind.ReportSummaryTable <- function(...) {
  a <- list(...)
  if (!all(vapply(a, is.data.frame, FUN.VALUE = logical(1)))) {
    util_error("Can only bind ReportSummaryTables")
  }
  if (!all(vapply(a, inherits,
        what = "ReportSummaryTable",
        FUN.VALUE = logical(1)
      ))) {
    util_error("Can only bind ReportSummaryTables")
  }
  a <- a[!!vapply(a, nrow, FUN.VALUE = integer(1))]
  a <- a[!!vapply(a, ncol, FUN.VALUE = integer(1))]
  if (length(a) <= 2) {
    if (length(a) == 0) {
      x <- rbind.data.frame(
        deparse.level = 1,
        make.row.names = TRUE,
        stringsAsFactors = FALSE,
        factor.exclude = TRUE
      )
      return(util_new_report_summary_table(x))
    } else if (length(a) == 1) {
      x <- a[[1]]
      y <- data.frame(Variables = character(0), N = integer(0))
      y <- util_new_report_summary_table(y)
    } else if (length(a) == 2) {
      x <- a[[1]]
      y <- a[[2]]
    }

    if (nrow(x) == 0) {
      return(y)
    }

    if (nrow(y) == 0) {
      return(x)
    }

    cols <- union(colnames(x), colnames(y))
    x[setdiff(cols, colnames(x))] <- numeric(nrow(x))
    y[setdiff(cols, colnames(y))] <- numeric(nrow(y))
    r <- rbind.data.frame(x[, cols, drop = FALSE], y[, cols, drop = FALSE],
      deparse.level = 1,
      make.row.names = TRUE,
      stringsAsFactors = FALSE,
      factor.exclude = TRUE
    )
    r <- util_new_report_summary_table(r)
    r <- util_set_report_summary_table_higher_means(
      r, util_report_summary_table_higher_means(x)
    )
    r <- util_set_report_summary_table_flip_mode(
      r, util_report_summary_table_flip_mode(x)
    )
    r <- util_set_report_summary_table_continuous(
      r, util_report_summary_table_continuous(x)
    )
    r <- util_set_report_summary_table_colscale(
      r, util_report_summary_table_colscale(x)
    )
    r <- util_set_report_summary_table_colcode(
      r, util_report_summary_table_colcode(x)
    )
    r <- util_set_report_summary_table_level_names(
      r, util_report_summary_table_level_names(x)
    )
    r <- util_set_report_summary_table_relative(
      r, util_report_summary_table_relative(x)
    )
    r <- util_set_report_summary_table_var_names(
      r,
      c(
        util_report_summary_table_var_names(x),
        util_report_summary_table_var_names(y)
      )
    )
    r
  } else { # recursive call to bind ReportSummaryTables for more than two variables # nolint: line_length_linter.
    x <-
      do.call(
        Recall,
        c(
          list(
            a[[1]],
            a[[2]]
          ),
          list(
            # Base rbind options are intentionally not forwarded recursively.
          )
        )
      )
    y <- a[3:length(a)]
    do.call(
      Recall,
      c(
        list(
          x
        ),
        y,
        list(
          # Base rbind options are intentionally not forwarded recursively.
        )
      )
    )
  }
}
