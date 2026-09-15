#' `Roxygen`-Template for metadata generation functions
#'
#' @param resp_vars [variable list] the names of variables to inspect in
#'                                  `study_data`. If missing, all variables in
#'                                  `study_data` will be used.
#' @param study_data [data.frame] study data inspected to derive metadata.
#'                                Only data frames are supported, not URLs or
#'                                file names.
#' @param guess_character [logical] guess a data type for character columns
#'                                  based on the values
#' @return `invisible(NULL)`
#' @keywords internal
.template_function_metadata_generation <-
  function(resp_vars, study_data, guess_character) { # nocov start
    util_error("nothing, just a template for Roxygen")
    invisible(NULL)
  } # nocov end
