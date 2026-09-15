#' Search for a formal in the stack trace
#'
#' Similar to [dynGet()], find a symbol in the closest data quality indicator
#' function and return its value. Can `stop()`, if symbol evaluation causes a
#' stop.
#'
#' @param symbol symbol to find
#'
#' @return value of the symbol, if available, `NULL` otherwise
#'
#' @family condition_functions
#' @noRd
util_find_indicator_function_in_callers <- function(symbol = "resp_vars") {
  n <- 1
  found <- FALSE
  try(
    {
      while (!any(rlang::call_name(rlang::caller_call(n)) %in%
            names(.indicator_or_descriptor))) {
        # Historical stricter caller filter removed here in commit 214dd76a7d.
        # It only accepted calls containing the requested symbol.
        n <- n + 1
      }
      found <- TRUE
    },
    silent = TRUE
  )
  if (found) {
    r <- util_with_english_language_if_possible(try(dynGet(symbol,
          inherits = TRUE,
          ifnotfound = NULL,
          minframe = n
        ), silent = TRUE)) # n - 1 is relative to my caller, but for dynGet from here, it fits # nolint: line_length_linter.
    if (inherits(r, "try-error")) {
      cnd <- util_attr(r, "condition", exact = TRUE)
      if (conditionMessage(cnd) ==
          sprintf("argument \"%s\" is missing, with no default", symbol)) {
        return(NULL)
      } else {
        util_error(cnd)
      }
    }
    return(r)
  } else {
    return(NULL)
  }
}

#' Internal helper: with english language if possible
#'
#' @noRd
util_with_english_language_if_possible <- function(expr) {
  if (nzchar(Sys.getenv("LC_ALL", unset = ""))) {
    force(expr)
  } else {
    withr::with_language("en", expr)
  }
}
