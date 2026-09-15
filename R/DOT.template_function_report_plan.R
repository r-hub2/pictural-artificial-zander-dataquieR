#' `Roxygen`-Template for report planning utility functions
#'
#' @param study_data [data.frame] the data frame that contains the measurements
#' @param meta_data [data.frame] old name for `item_level`
#' @param label_col [variable attribute] the name of the column in the metadata
#'                                       with labels of variables
#' @param meta_data_dataframe [data.frame] the data frame that contains the
#'                                          metadata for the data frame level
#' @param meta_data_segment [data.frame] -- optional: Segment level metadata
#' @param meta_data_cross_item [data.frame] -- optional: Cross-item level
#'                                           metadata
#' @param meta_data_item_computation [data.frame] -- optional: Computed items
#'                                                                     metadata
#' @param specific_args [list] named list of arguments specifically for one of
#'                             the called functions. The names of the list
#'                             elements correspond to the indicator functions
#'                             whose calls should be modified. The elements are
#'                             lists of arguments.
#' @param arg_overrides [list] arguments to be passed to all called indicator
#'                             functions if applicable.
#' @param resp_vars [variable list] the name of the measurement variables for
#'                                  the report. If missing or `NULL`, all
#'                                  variables will be used.
#' @return `invisible(NULL)`
#' @keywords internal
.template_function_report_plan <-
  function(study_data, meta_data, label_col, meta_data_dataframe,
    meta_data_segment, meta_data_cross_item, meta_data_item_computation,
    specific_args, arg_overrides, resp_vars) { # nocov start
    util_error("nothing, just a template for Roxygen")
    invisible(NULL)
  } # nocov end
