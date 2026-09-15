#' Detect falsish values
#'
#' @param x a value/vector of values
#'
#' @return vector of logical values:
#'    `TRUE`, wherever x is somehow empty
#'
#' @family missing_functions
#' @concept robustness
#' @noRd
util_is_na_0_empty_or_false <- function(x) {
  cs <- paste0("util_is_na_0_empty_or_false.", rlang::hash(x))
  if (exists(cs, envir = .falsish_value_cache, inherits = FALSE)) {
    return(get(cs, envir = .falsish_value_cache, inherits = FALSE))
  }
  if (inherits(x, "hms")) { # maybe more general: !is.vector(x)
    x <- util_as_character(x)
  }
  y <- x
  class(y) <- "logical"
  attributes(y) <- NULL
  y[] <- FALSE
  y[is.na(x)] <- TRUE
  y[(trimws(x) %in% c("false", "FALSE", "F", "f", "-", ""))] <- TRUE
  idx <- !suppressWarnings(as.numeric(x))
  idx[is.na(idx)] <- FALSE
  y[idx] <- TRUE
  y <- as.logical(y)
  assign(cs, y, envir = .falsish_value_cache)
  return(y)
}

.falsish_value_cache <- new.env(parent = emptyenv())

#' Internal helper: purge falsish value cache
#'
#' @noRd
util_purge_falsish_value_cache <- function() {
  rm(
    list = ls(.falsish_value_cache, all.names = TRUE),
    envir = .falsish_value_cache
  )
}
