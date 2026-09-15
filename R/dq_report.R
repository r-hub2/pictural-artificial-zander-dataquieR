#' Generate a full DQ report
#'
#' Deprecated
#'
#' @param ... Deprecated. Use [dq_report2()]. Its `cores` argument controls
#'   parallel report computation and rendering.
#' @return Deprecated
#' @export
#' @importFrom stats setNames
dq_report <- function(...) { # nocov start
  lifecycle::deprecate_stop("2.1.0", what = "dq_report()", with = "dq_report2()") # nolint: line_length_linter.
} # nocov end
