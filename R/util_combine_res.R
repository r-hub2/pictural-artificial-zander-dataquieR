#' Keep intrinsic-applicability NULL results visible to util_combine_res()
#'
#' @param x a dataquieR result object
#' @param slot result slot to keep
#'
#' @return the selected slot as a one-element result object
#'
#' @noRd
util_subset_result_slot_for_combine <- function(x, slot) {
  r <- x[slot]
  if (inherits(x, "dataquieR_NULL")) {
    class(r) <- unique(c("dataquieR_NULL", class(r)))
  }
  r
}

#' Combine results for Single Variables
#'
#' to, e.g., a data frame with one row per variable or a similar heat-map,
#' see [print.ReportSummaryTable()].
#'
#' @param all_of_f all results of a function
#'
#' @return row-bound combined results
#'
#' @noRd
util_combine_res <- function(all_of_f) {
  util_result_slot <- function(x, slot) {
    unclass(x)[[slot]]
  }

  # getting the call names for all_of_f (subset of the report)
  cn <- unique(unlist(lapply(all_of_f, util_attr, "cn")))
  if (length(cn) != 1) {
    cn <- ""
  }
  # remove the class dataquieR_result
  all_of_f <- lapply(all_of_f, function(x) {
    class(x) <- setdiff(class(x), "dataquieR_result")
    x
  })

  # combine results for the indicator functions related overview----
  # extracting call names (all until first dot)
  nms <- sub("^([^\\.]*).*$", "\\1", names(all_of_f))
  # combine only if results come from the same call
  util_stop_if_not(length(unique(nms)) == 1)
  cll <- nms[[1]]
  # get the function name
  fkt <- util_map_by_largest_prefix(
    cll,
    haystack = util_all_ind_functions()
  )
  # try to combine results, mostly relevant for indicator function outputs
  # where single variable is false
  # check the results for the existence of certain output types
  # each result is a logical vector
  plots <- !vapply(lapply(all_of_f, util_result_slot, "SummaryPlot"), is.null,
    FUN.VALUE = logical(1)
  )
  plot_lists <- !vapply(lapply(all_of_f, util_result_slot, "SummaryPlotList"),
    is.null,
    FUN.VALUE = logical(1)
  )

  summary_tables <- !vapply(lapply(all_of_f, util_result_slot, "SummaryTable"),
    is.null,
    FUN.VALUE = logical(1)
  )
  summary_data <- !vapply(lapply(all_of_f, util_result_slot, "SummaryData"),
    is.null,
    FUN.VALUE = logical(1)
  )
  result_data <- !vapply(lapply(all_of_f, util_result_slot, "ResultData"),
    is.null,
    FUN.VALUE = logical(1)
  )
  report_summary_tables <- lapply(
    all_of_f,
    util_result_slot,
    "ReportSummaryTable"
  )
  report_summary_tables <- !vapply(report_summary_tables, is.null,
    FUN.VALUE = logical(1)
  )
  is_wrapped_dataquieR_NULL <- function(x) {
    if (!is.list(x) || length(x) != 1L ||
        !inherits(x[[1L]], "dataquieR_NULL")) {
      return(FALSE)
    }

    err <- util_attr(x[[1L]], "error")
    if (is.null(err)) {
      err <- util_attr(x, "error")
    }
    is.list(err) && length(err) == 1L
  }
  wrapped_null_results <- vapply(all_of_f, is_wrapped_dataquieR_NULL,
    FUN.VALUE = logical(1)
  )
  null_results <- vapply(all_of_f, inherits, "dataquieR_NULL",
    FUN.VALUE = logical(1)
  ) | wrapped_null_results
  all_of_f[wrapped_null_results] <- lapply(
    all_of_f[wrapped_null_results],
    `[[`,
    1L
  )


  errors <- util_collapse_msgs("error", all_of_f)
  warnings <- util_collapse_msgs("warning", all_of_f)
  messages <- util_collapse_msgs("message", all_of_f)

  if ((fkt %in% c(
    "con_limit_deviations", # check if we are working with a limits function
    "con_hard_limits",
    "con_soft_limits",
    "con_detection_limits"
  )) && (any(plot_lists) || any(plots))) {
    # use limits plots, if available, not the ReportSummaryTable
    return(all_of_f)
    # check if we have to combine some single variable results
  } else if ((!any(report_summary_tables)) && (any(plot_lists) || any(plots))) {
    return(all_of_f) # use the plots, if available
    # otherwise, use ReportSummaryTable, SummaryData, or SummaryTable
    # and combine the results (rbind) using util_combine_res
  } else if (any(report_summary_tables)) {
    res_flags <- report_summary_tables
    slot <- "ReportSummaryTable"
  } else if (any(result_data)) {
    res_flags <- result_data
    slot <- "ResultData"
  } else if (any(summary_data)) {
    res_flags <- summary_data
    slot <- "SummaryData"
  } else if (any(summary_tables)) {
    res_flags <- summary_tables
    slot <- "SummaryTable"
  } else {
    return(all_of_f)
  }

  # null_results contains all results that are NULL; the remaining results
  # must be combinable.
  util_stop_if_not(all(res_flags | null_results))

  # extract all call attributes to combine them
  clls <- lapply(all_of_f[res_flags & !null_results], util_attr, "call")
  clls <- lapply(clls, deparse)
  clls <- vapply(clls, paste0,
    collapse = "\n",
    FUN.VALUE = character(1)
  )
  clls <- paste0(clls, "$", slot, collapse = ", \n\t")
  clls <- paste0("rbind(\n\t", clls, "\n)")

  # for ReportSummaryTables: extract and combine VAR_NAMES attributes
  vns <- NULL
  if (any(report_summary_tables)) {
    vns <- unlist(lapply(unname(lapply(
      all_of_f[res_flags & !null_results],
      util_result_slot,
      slot
    )), util_attr, "VAR_NAMES"))
  }

  # copy hover text for table headers
  description <- lapply(
    lapply(all_of_f[res_flags & !null_results], util_result_slot, slot),
    util_attr, "description"
  )
  description <- Filter(Negate(is.null), description)
  description <- unique(description)


  # rescue plain label attributes if assigned for single result tables
  plain_label_atts <- lapply(
    lapply(lapply(
      all_of_f[res_flags & !null_results],
      util_result_slot,
      slot
    ), `[[`, "Variables"),
    util_attr, "plain_label"
  )
  # plain label should be only 1 or no plain_label(error), be characters, not NA
  plain_label_lengths_valid <-
    all(vapply(plain_label_atts, length, FUN.VALUE = integer(1)) %in% 0:1)
  plain_label_values_valid <-
    all(vapply(plain_label_atts, is.character, FUN.VALUE = logical(1)))
  plain_label_is_na <- vapply(plain_label_atts, identical, NA_character_,
    FUN.VALUE = logical(1)
  )
  plain_label_na_valid <- !any(plain_label_is_na)
  if (plain_label_lengths_valid &&
      plain_label_values_valid &&
      plain_label_na_valid) {
    plain_label_atts <- unname(unlist(plain_label_atts))
  } else {
    if (!is.null(plain_label_atts) &&
        !all(vapply(plain_label_atts, length, FUN.VALUE = integer(1)) == 0)) {
      util_error(
        "Internal error, sorry, please report: invalid plain label atts"
      )
    }
    plain_label_atts <- NULL
  }

  # combine results (ReportSummaryTable and tables)
  # select results according to the logical vectors, extract the corresponding
  # slots, and then bind by row
  # then write the combined result to all_of_f, keeping its original structure
  all_of_f <- list(setNames(list(do.call(util_rbind, lapply(
    all_of_f[res_flags & !null_results],
    util_result_slot,
    slot
  ))), nm = slot))
  if (!is.null(all_of_f[[1]][[slot]][["Variables"]])) {
    attr(all_of_f[[1]][[slot]][["Variables"]], "plain_label") <-
      plain_label_atts
  }
  attr(all_of_f[[1]], "call") <- clls
  if (!is.null(vns)) { # attach VAR_NAMES for ReportSummaryTables
    if (util_is_report_summary_table(all_of_f[[1]][[slot]])) {
      all_of_f[[1]][[slot]] <- util_set_report_summary_table_var_names(
        all_of_f[[1]][[slot]], vns
      )
    } else {
      attr(all_of_f[[1]][[slot]], "VAR_NAMES") <- vns
    }
  }

  # add attribute "description" for hover text
  if (length(description) > 1) {
    description <- unlist(description)
    description_names <- names(description)
    if (!is.null(description_names) &&
      all(nzchar(description_names)) &&
      all(vapply(split(description, description_names), function(x) {
        length(unique(x)) == 1
      }, FUN.VALUE = logical(1)))) {
      description <- description[!duplicated(description_names)]
      attr(all_of_f[[1]][[slot]], "description") <- description
    } else {
      util_warning(c(
        "Internal error: sorry please report to the developers",
        " - util_combine_res incompatible results"
      ))
    }
  } else if (length(description) == 1) {
    attr(all_of_f[[1]][[slot]], "description") <- description[[1]]
  }


  # reattach the error/message/warning attributes using the combined version
  if (any(trimws(errors) != "")) {
    attr(all_of_f[[1]], "error") <-
      list(simpleError(paste(errors, collapse = "\n")))
  } else {
    attr(all_of_f[[1]], "error") <- list()
  }
  if (any(trimws(warnings) != "")) {
    attr(all_of_f[[1]], "warning") <-
      list(simpleWarning(paste(warnings, collapse = "\n")))
  } else {
    attr(all_of_f[[1]], "warning") <- list()
  }
  if (any(trimws(messages) != "")) {
    attr(all_of_f[[1]], "message") <-
      list(simpleMessage(paste(messages, collapse = "\n")))
  } else {
    attr(all_of_f[[1]], "message") <- list()
  }
  names(all_of_f) <- cll

  attr(all_of_f[[1]], "cn") <- cn

  return(all_of_f)
}
