#' View a file in most suitable viewer
#'
#' @param file the file to view
#'
#' @return
#' `invisible(file)`
#'
#' @family system_functions
#' @concept reporting
#' @noRd
util_view_file <- function(file) {
  viewer <- getOption("viewer")
  if (is.null(viewer)) { # nocov start
    if (!identical(Sys.getenv("TESTTHAT"), "true")) {
      # Viewer stuff cannot be tested w/o a GUI
      if (util_really_rstudio() &&
          requireNamespace("rstudioapi", quietly = TRUE) &&
          (exists("viewer", asNamespace("rstudioapi"), mode = "function")) &&
          (exists("isAvailable", asNamespace("rstudioapi"), mode = "function")) && # nolint: line_length_linter.
          rstudioapi::isAvailable()) {
        rstudioapi::viewer(file)
      } else {
        browseURL(paste0("file://", normalizePath(file)))
      }
    }
  } else { # nocov end
    viewer(file)
  }
  invisible(file)
}

#' Internal helper: works in rs viewer
#'
#' @noRd
util_works_in_rs_viewer <- function(fn) {
  if (!util_really_rstudio()) {
    return(FALSE)
  }
  is_existing_path_under_tempdir <- function(fn) {
    util_stop_if_not(
      "`fn` must be one non-empty path" =
        is.character(fn) && length(fn) == 1L && nzchar(fn)
    )
    if (!file.exists(fn)) {
      return(FALSE)
    }

    td <- normalizePath(tempdir(), winslash = "/", mustWork = TRUE)
    fn_real <- normalizePath(fn, winslash = "/", mustWork = TRUE)

    td_prefix <- paste0(td, "/")
    identical(fn_real, td) || startsWith(fn_real, td_prefix)
  }
  is_existing_path_under_tempdir(fn)
}
