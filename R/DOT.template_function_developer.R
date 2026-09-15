#' `Roxygen`-Template for internal developer utility functions
#'
#' @param study_data [data.frame] the data frame that contains the measurements
#' @param meta_data [data.frame] item-level metadata
#' @param label_col [variable attribute] the name of the column in the metadata
#'                                       with labels of variables
#' @param resp_vars [variable list] selected measurement variables
#' @param group_vars [variable list] selected grouping variables
#' @param co_vars [variable list] selected covariables
#' @param time_vars [variable list] selected time variables
#' @param meta_data_segment [data.frame] -- optional: Segment level metadata
#' @param meta_data_cross_item [data.frame] -- optional: Cross-item level
#'                                           metadata
#' @param meta_data_dataframe [data.frame] the data frame that contains the
#'                                          metadata for the data frame level
#' @param meta_data_item_computation [data.frame] -- optional: Computed items
#'                                                                     metadata
#' @return `invisible(NULL)`
#' @keywords internal
#' @noRd
.template_function_developer <-
  function(study_data, meta_data, label_col, resp_vars, group_vars, co_vars,
    time_vars, meta_data_segment, meta_data_cross_item, meta_data_dataframe,
    meta_data_item_computation) { # nocov start
    util_error("nothing, just a template for Roxygen")
    invisible(NULL)
  } # nocov end
