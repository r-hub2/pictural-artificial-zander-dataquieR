#' Helpers around the optional `units` package
#'
#' The `units` package is `Suggests:` and depends on the native UDUNITS-2
#' library, so it may legitimately be unavailable. These helpers centralize
#' the availability check and cache results from
#' [units::valid_udunits()] / [units::valid_udunits_prefixes()] so we don't
#' touch UDUNITS at package load time.
#'
#' @name util_units
#' @noRd
NULL

# Package-level cache for the (potentially expensive) UDUNITS tables. Lives
# in the package namespace so the cache survives across calls but is empty
# on a fresh session, and is only populated lazily, on demand.
.dq_units_cache <- new.env(parent = emptyenv())

#' Is the optional `units` package usable?
#'
#' Both `units` and its hard system dependency `xml2` must be installed for
#' [units::valid_udunits()] to work, so we check for both. Result is cached
#' by `util_have_suggested()` so subsequent calls are cheap.
#'
#' @return [logical] `TRUE` iff both `units` and `xml2` are available.
#' @family robustness_functions
#' @concept process
#' @noRd
util_units_available <- function() {
  util_have_suggested("units") && util_have_suggested("xml2")
}

#' Cached UDUNITS table
#'
#' Returns the data.frame produced by [units::valid_udunits()], cached for
#' the lifetime of the session. Returns `NULL` if the `units` package is
#' not available -- callers must handle that.
#'
#' @return data.frame or `NULL`
#' @family robustness_functions
#' @concept process
#' @noRd
util_get_valid_udunits <- function() {
  if (!exists("valud", envir = .dq_units_cache, inherits = FALSE)) {
    if (!util_units_available()) {
      return(NULL)
    }
    assign("valud",
      suppressMessages(units::valid_udunits()),
      envir = .dq_units_cache
    )
  }
  get("valud", envir = .dq_units_cache, inherits = FALSE)
}

#' Cached UDUNITS prefix table
#'
#' Returns the data.frame produced by [units::valid_udunits_prefixes()],
#' cached for the lifetime of the session. Returns `NULL` if the `units`
#' package is not available.
#'
#' @return data.frame or `NULL`
#' @family robustness_functions
#' @concept process
#' @noRd
util_get_valid_udunits_prefixes <- function() {
  if (!exists("pfx", envir = .dq_units_cache, inherits = FALSE)) {
    if (!util_units_available()) {
      return(NULL)
    }
    assign("pfx",
      suppressMessages(units::valid_udunits_prefixes()),
      envir = .dq_units_cache
    )
  }
  get("pfx", envir = .dq_units_cache, inherits = FALSE)
}
