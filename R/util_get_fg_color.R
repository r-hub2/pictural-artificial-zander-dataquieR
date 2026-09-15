#' Find a foreground color for a background
#'
#' black or white
#'
#' @param cl colors
#'
#' @return black or white for each cl
#' @seealso [`stackoverflow.com`](https://stackoverflow.com/a/24810681)
#' @noRd
util_get_fg_color <- function(cl) {
  cl <- col2rgb(util_col2rgb(cl), alpha = TRUE)
  brightness <- cl["red", , drop = FALSE] * 0.299 + cl["green", , drop = FALSE] * 0.587 + # nolint: line_length_linter.
    cl["blue", , drop = FALSE] * 0.114
  as.vector(ifelse(brightness > 160, "#000000", "#ffffff"))
}
