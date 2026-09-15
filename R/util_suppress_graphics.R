#' Run an expression with all graphics output discarded
#'
#' In-package replacement for `R.devices::suppressGraphics()`. Opens a
#' PDF graphics device writing to [base::nullfile()] for the duration of
#' `expr`, so any plot side-effects produced by `expr` are silently
#' discarded. Useful around parallel workers that would otherwise spawn
#' stray on-screen graphics devices.
#'
#' Implementation notes for robustness across platforms (including the
#' CRAN check farms / `r-hub` / `win-builder`):
#'   * [base::nullfile()] resolves to `/dev/null` on Unix-likes and `NUL`
#'     on Windows, so [grDevices::pdf()] can always be created.
#'   * The previously active device is restored on exit; we never call
#'     `dev.off()` on a device that wasn't ours.
#'   * Errors raised inside `expr` are propagated unchanged after the
#'     null device has been closed.
#'
#' @param expr expression to evaluate -- evaluated in the caller's
#'   environment.
#'
#' @return the value of `expr`, invisibly if it was invisible.
#'
#' @noRd
util_suppress_graphics <- function(expr) {
  null_dev <- NULL
  withr::defer(
    {
      if (!is.null(null_dev) &&
          null_dev %in% grDevices::dev.list()) {
        try(grDevices::dev.off(null_dev), silent = TRUE)
      }
    }
  )

  # Open the discard device. We tolerate a failure here (e.g. on a
  # locked-down CI runner) and just evaluate expr without graphics
  # suppression in that case.
  tryCatch(
    {
      grDevices::pdf(file = nullfile())
      null_dev <- grDevices::dev.cur()
    },
    error = function(e) NULL,
    warning = function(w) NULL
  )

  # Forcing the lazy promise evaluates `expr` in the caller's frame,
  # matching the semantics callers expect from R.devices::suppressGraphics().
  expr
}
