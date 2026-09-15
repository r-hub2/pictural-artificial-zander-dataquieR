#' Mark JavaScript code for htmlwidgets-based table options
#'
#' @param ... JavaScript code passed to [htmlwidgets::JS()].
#'
#' @return a JavaScript expression object for htmlwidgets.
#' @keywords internal
#' @noRd
util_html_table_js <- function(...) {
  util_ensure_suggested("htmlwidgets", "Generating nice tables")
  htmlwidgets::JS(...)
}
