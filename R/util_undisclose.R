#' Remove tables referred to by metadata and use `SVG` for most figures
#'
#' @param x an object to un-disclose
#' @param ... further arguments, used for pointing to the `dataquieR_result`
#'            object, if called recursively
#'
#' @return undisclosed object
#' @noRd
util_undisclose <- function(x, ...) {
  UseMethod("util_undisclose")
}

#' @export
#' @noRd
util_undisclose.dq_lazy_ggplot <- function(x, ...) {
  util_undisclose(prep_realize_ggplot(x), ...)
}

#' @export
#' @noRd
util_undisclose.default <- function(x, ...) {
  if (is.atomic(x)) {
    return(x)
  }
  util_error(
    "Internal error: object of class %s in report.",
    util_pretty_vector_string(quote = sQuote, class(x))
  )
}

#' @export
#' @noRd
util_undisclose.dataquieR_resultset2 <- function(x, ...) {
  if ("cores" %in% names(list(...))) {
    cores <- list(...)$cores
  }
  if ((("cores" %in% names(list(...))) ||
        .called_in_pipeline) && rlang::is_integerish(dynGet("cores")) &&
      as.integer(dynGet("cores")) > 1) {
    mycl <- parallel::makePSOCKcluster(as.integer(dynGet("cores")))
    parallel::clusterCall(mycl, library, "dataquieR", character.only = TRUE)
    parallel::clusterCall(mycl, loadNamespace, "hms")
    withr::defer(parallel::stopCluster(mycl))
  } else {
    mycl <- parallel::getDefaultCluster()
  }

  referred_tables <- util_attr(x, "referred_tables", exact = TRUE)
  my_tabs <- lapply(
    setNames(nm = names(referred_tables)),
    function(dfn) {
      data.frame(
        `NA` = paste(dQuote(dfn), "is not available."),
        check.names = FALSE
      )
    }
  )

  attr(x, "referred_tables")[] <- my_tabs

  study_data_slots <- grep("StudyData$", names(x), value = TRUE)
  table_slots <- grep("_TABLE$", names(x), value = TRUE)
  user_table_slots <- setdiff(table_slots, c(
    names(referred_tables),
    names(WELL_KNOWN_META_VARIABLE_NAMES),
    CODE_LIST_TABLE
  ))
  undisclosed_slots <- unique(c(study_data_slots, user_table_slots))
  if (length(undisclosed_slots)) {
    x_class <- class(x)
    class(x) <- setdiff(x_class, "dataquieR_resultset2")
    x[undisclosed_slots] <- NULL
    class(x) <- x_class
  }

  x[] <- util_par_lapply_lb(
    cl = mycl,
    x, util_undisclose, ...
  )
  return(x)
}

#' @export
#' @noRd
util_undisclose.dataquieR_result <- function(x, ...) {
  if (length(setdiff(
    class(x),
    c(
      "dataquieR_result", "list", "dataquieR_NULL",
      "master_result", "Slot"
    )
  )) > 0) {
    return(NextMethod())
  }
  dataquieR_result <- x
  x[grep("StudyData$", names(x), value = TRUE)] <- NULL
  if ("PlotlyPlot" %in% names(x)) {
    # class plotly
    if (any(endsWith(setdiff(names(x), "PlotlyPlot"), "Plot")) ||
        any(endsWith(setdiff(names(x), "PlotlyPlot"), "PlotList"))) {
      x$PlotlyPlot <- NULL
    } else {
      # ensure, sizing hint sticks at the dqr, only
      fixed <- util_fix_sizing_hints(dqr = dataquieR_result, x = x$PlotlyPlot)

      x$SummaryPlot <- try(
        util_plotly2svg_object(x$PlotlyPlot,
          sizing_hints =
            util_attr(fixed$dqr,
              "sizing_hints",
              exact = TRUE
            )
        ),
        silent = TRUE
      )
      if (util_is_try_error(x$SummaryPlot)) {
        util_warning(
          c(
            "Could not convert a plotly to an SVG or PNG for",
            "undisclosing data. Will delete an output slot. Maybe, a",
            "suggested package is missing: %s"
          ), sQuote(conditionMessage(
            util_attr(x$SummaryPlot, "condition", exact = TRUE)
          ))
        )
        x$SummaryPlot <- NULL
      }
      x$PlotlyPlot <- NULL
    }
  }
  x[] <- lapply(x, util_undisclose, dataquieR_result = dataquieR_result, ...)
  return(x)
}

#' @export
#' @noRd
util_undisclose.list <- function(x, ...) {
  x[] <- lapply(x, util_undisclose, ...)
  return(x)
}

#' @export
#' @noRd
util_undisclose.Slot <- function(x, ...) {
  if (length(setdiff(
    class(x),
    c(
      "dataquieR_result", "list", "dataquieR_NULL",
      "master_result", "Slot"
    )
  )) > 0) {
    return(NextMethod())
  }
  x[] <- lapply(x, util_undisclose, ...)

  return(x)
}

#' @export
#' @noRd
util_undisclose.gg <- function(x, ...) {
  dataquieR_result <- list(...)[["dataquieR_result"]]
  if (util_is_svg_object(x)) {
    return(x)
  }
  fixed <- util_fix_sizing_hints(dqr = dataquieR_result, x = x)
  return(suppressWarnings(util_plot2svg_object(x,
        sizing_hints =
          util_attr(fixed$dqr,
            "sizing_hints",
            exact = TRUE
          )
      )))
}

#' @export
util_undisclose.util_pairs_ggplot_panels <- util_undisclose.gg

#' @export
util_undisclose.svg_plot_proxy <- util_undisclose.gg

#' @export
util_undisclose.ggmatrix_plot_obj <- util_undisclose.gg

#' @export
util_undisclose.ggmatrix_fn_with_params <- util_undisclose.gg

#' @export
util_undisclose.ggplot_built <- util_undisclose.gg

#' @export
#' @noRd
util_undisclose.data.frame <- function(x, ...) {
  return(x)
}

#' Remove data disclosing details
#'
#' new function: no warranty, so far.
#'
#' @param x an object to un-disclose, a
#' @param cores can be an integer with a number of cores to use. if not
#'              specified, the function uses the default cluster, if available
#'              and falls back to serial un-disclosing, otherwise.
#'
#' @return undisclosed object
#' @export
prep_undisclose <- function(x, cores) {
  if (!(inherits(x, "dataquieR_resultset2") ||
        inherits(x, "dataquieR_result"))) {
    util_error(
      "%s works for results or reports, only",
      sQuote("prep_undisclose")
    )
  }
  util_message(
    "%s comes without any warranty, so far",
    sQuote("prep_undisclose")
  )
  if (missing(cores)) {
    return(suppressMessages(util_undisclose(x)))
  } else {
    return(suppressMessages(util_undisclose(x, cores = cores)))
  }
}
