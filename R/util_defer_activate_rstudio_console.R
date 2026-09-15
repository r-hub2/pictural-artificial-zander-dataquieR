#' Restore the RStudio console after deferred report viewers
#'
#' @param envir Environment whose exit handlers should activate the console.
#' @param active Whether the current frontend is RStudio.
#' @param activate Function activating the RStudio console.
#'
#' @return `NULL`, invisibly.
#' @noRd
util_defer_activate_rstudio_console <- function(
  envir = parent.frame(),
  active = util_really_rstudio(),
  activate = function() rstudioapi::executeCommand("activateConsole")) {
  util_stop_if_not(is.environment(envir))

  if (isTRUE(active)) {
    withr::defer(
      try(activate(), silent = TRUE),
      envir = envir,
      priority = "last"
    )
  }

  invisible(NULL)
}
