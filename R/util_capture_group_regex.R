#' Match the last regular expression occurrence
#'
#' Base-R replacement for the subset of
#' [stringi::stri_match_last_regex()] needed internally by dataquieR.
#'
#' For each element of `x`, the function returns the last match of `pattern`.
#' The first result column contains the complete match, followed by one column
#' per capture group.
#'
#' @param x [`character`] vector to match against.
#' @param pattern [`character`] scalar regular expression.
#' @param perl [`logical`] scalar. Passed to [gregexec()]. Defaults to `TRUE`.
#' @param as_data_frame [`logical`] scalar. If `TRUE`, return a `data.frame`
#'   instead of a character matrix.
#'
#' @return A [`character`] matrix with one row per element of `x`. The first
#'   column is named `"match"` and contains the full match. Capture groups are
#'   named `"group1"`, `"group2"`, and so on. If `as_data_frame` is `TRUE`, the
#'   result is returned as a `data.frame`.
#'
#' @details
#' This function uses base R regular expressions via [gregexec()] and
#' [regmatches()]. With `perl = TRUE`, this is PCRE/PCRE2-based and therefore
#' not fully identical to `stringi`'s ICU regular expression engine. It is meant
#' as a lightweight internal helper for simple path and capture-group matching.
#'
#' @examples
#' x <- c("abc/foo_123/bar_456.txt", "abc/no-match.txt")
#' util_capture_group_regex(x, "([a-z]+)_([0-9]+)")
#'
#' @keywords internal
#' @noRd
util_capture_group_regex <- function(x, pattern, perl = TRUE,
  as_data_frame = FALSE) {
  util_stop_if_not(is.character(x))
  util_stop_if_not(is.character(pattern))
  util_stop_if_not(length(pattern) == 1L)
  util_stop_if_not(is.logical(perl))
  util_stop_if_not(length(perl) == 1L)
  util_stop_if_not(is.logical(as_data_frame))
  util_stop_if_not(length(as_data_frame) == 1L)

  m <- gregexec(pattern, x, perl = perl)
  out <- regmatches(x, m)

  n_cols <- max(1L, vapply(out, function(z) {
    if (length(z)) {
      nrow(z)
    } else {
      0L
    }
  }, integer(1L)))

  res <- matrix(NA_character_, nrow = length(x), ncol = n_cols)

  for (i in seq_along(out)) {
    if (length(out[[i]])) {
      res[i, seq_len(nrow(out[[i]]))] <- out[[i]][, ncol(out[[i]]), drop = TRUE]
    }
  }

  if (ncol(res) > 1) {
    colnames(res) <- c("match", paste0("group", seq_len(ncol(res) - 1L)))
  } else {
    colnames(res) <- c("match")
  }

  if (isTRUE(as_data_frame)) {
    res <- as.data.frame(res, stringsAsFactors = FALSE)
  }

  res
}
