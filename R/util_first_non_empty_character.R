#' Select the first non-empty character value
#'
#' @param ... Character candidates in preference order.
#'
#' @return One character value or `NA_character_`.
#' @noRd
util_first_non_empty_character <- function(...) {
  candidates <- as.character(unlist(list(...), use.names = FALSE))
  candidates <- candidates[!vapply(
    candidates,
    util_result_caption_empty,
    FUN.VALUE = logical(1)
  )]
  if (length(candidates)) {
    candidates[[1]]
  } else {
    NA_character_
  }
}
