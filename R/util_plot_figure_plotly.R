# nolint start: line_length_linter.
#' Plot a `ggplot2` figure using `plotly`
#'
#' @param x [ggplot2::ggplot2] object
#' @param sizing_hints `object` additional metadata about the natural figure size
#'
#' @return `htmltools` compatible object
#'
#' @noRd
# nolint end
util_plot_figure_plotly <- function(x, sizing_hints = NULL) {
  withCallingHandlers(
    {
      if (inherits(x, "dq_lazy_ggplot")) x <- prep_realize_ggplot(x)
      if (inherits(x, "ggplot") && inherits(x, "patchwork")) {
        x <- htmltools::plotTag(x, "", width = 640)
      } else if ((inherits(x, "ggplot") || inherits(x, "ggmatrix") ||
            inherits(x, "svg_plot_proxy") ||
            inherits(x, "util_pairs_ggplot_panels")) &&
          !util_is_svg_object(x)) {
        title <- NULL
        subtitle <- NULL

        ## Titel und Untertitel S7-sicher aus ggplot_build(x) holen
        try(
          {
            build_obj <- ggplot2::ggplot_build(x)
            plot_obj <- util_gg_get(build_obj, "plot")
            labels_obj <- if (!is.null(plot_obj)) util_gg_get(plot_obj, "labels") else NULL # nolint: line_length_linter.
            title <- if (!is.null(labels_obj)) util_gg_get(labels_obj, "title") else NULL
          },
          silent = TRUE
        )

        try(
          {
            build_obj <- ggplot2::ggplot_build(x)
            plot_obj <- util_gg_get(build_obj, "plot")
            labels_obj <- if (!is.null(plot_obj)) util_gg_get(plot_obj, "labels") else NULL # nolint: line_length_linter.
            subtitle <- if (!is.null(labels_obj)) util_gg_get(labels_obj, "subtitle") else NULL # nolint: line_length_linter.
          },
          silent = TRUE
        )

        py <- util_attr(x, "py", exact = TRUE)
        if (is.null(py)) {
          py <- util_attr(x, "plotly", exact = TRUE)
        }
        if (is.null(py) || !inherits(py, "plotly")) {
          x <- util_remove_dataquieR_result_class(x)
          # Removing aspect.ratio here can prevent a plotly warning.
          py <- suppressWarnings( # prevent a warning
            force(util_ggplotly(x))
          )
          py <- util_adjust_geom_text_for_plotly(py)
        }
        x <- plotly::layout(py,
          autosize = !FALSE,
          margin = list(
            autoexpand = !FALSE,
            r = 200,
            b = 100
          )
        )
        if (!is.null(title) && is.null(subtitle)) {
          x <- plotly::layout(x, title = title)
        } else if (!is.null(title) && !is.null(subtitle)) {
          plotly_title_text <- paste0(
            title,
            "<br />",
            "<sub>",
            subtitle,
            "</sub>"
          )
          x$x$layout$title$text <- plotly_title_text
        }
        x <- plotly::config(x,
          responsive = TRUE,
          autosizable = TRUE,
          fillFrame = TRUE,
          displaylogo = FALSE,
          modeBarButtonsToRemove = list("toImage"),
          modeBarButtonsToAdd =
            list(
              list(
                name = "Save as image",
                icon = htmlwidgets::JS("Plotly.Icons.camera"),
                click = htmlwidgets::JS(paste(
                  sep = "\n",
                  "function(gd) {",
                  "  plotlyDL(gd)",
                  "}"
                ))
              ),
              list(
                name = "Restore initial Size",
                icon =
                  htmlwidgets::JS("PlotlyIconshomeRED()"),
                click =
                  htmlwidgets::JS(
                    paste(
                      sep = "\n",
                      "function() {",
                      "   if (togglePlotlyWindowZoom instanceof Function) {",
                      "     togglePlotlyWindowZoom()",
                      "   }",
                      "}"
                    )
                  )
              ),
              list(
                name = "Print plot",
                icon = htmlwidgets::JS("PlotlyIconsprinter()"),
                click = htmlwidgets::JS(paste(
                  sep = "\n",
                  "function(gd) {",
                  "  plotlyPrint(gd)",
                  "}"
                ))
              ),
              list(
                name = "Download as PDF",
                icon = htmlwidgets::JS("PlotlyIconspdf()"),
                click = htmlwidgets::JS(paste(
                  sep = "\n",
                  "function(gd) {",
                  "  downloadPlotlyAsPDF(gd, {orientation: 'landscape', dpi: 300});", # nolint: line_length_linter.
                  "}"
                ))
              )
            )
        )
        util_ensure_suggested("htmlwidgets",
          goal = "Creating the Print Dialog for Plotly"
        )
        x <- htmlwidgets::onRender(x, "
                          function(el, x) {
                            if (typeof injectPlotlyPrintDialog === 'function') {
                              injectPlotlyPrintDialog();
                            }
                          }
                        ")
      } else {
        x <- util_plot_figure_no_plotly(x = x, sizing_hints = sizing_hints)
      }
    }, # nocov start
    warning = function(cond) { # suppress a waning caused by ggplotly for barplots # nolint: line_length_linter.
      if (startsWith(
        conditionMessage(cond),
        "'bar' objects don't have these attributes: 'mode'"
      ) ||
        startsWith(
          conditionMessage(cond),
          "'box' objects don't have these attributes: 'mode'"
        )) {
        invokeRestart("muffleWarning")
      }
      if (any(grepl("the mode", conditionMessage(cond)))) {
        invokeRestart("muffleWarning")
      }
    },
    message = function(cond) { # suppress eine Message von ggplotly
      if (startsWith(
        conditionMessage(cond),
        "'bar' objects don't have these attributes: 'mode'"
      ) ||
        startsWith(
          conditionMessage(cond),
          "'box' objects don't have these attributes: 'mode'"
        )) {
        invokeRestart("muffleMessage")
      }
      if (any(grepl("the mode", conditionMessage(cond)))) {
        invokeRestart("muffleMessage")
      } # nocov end
    }
  )
  x
}

#' Return the pre-computed `plotly` from a `dataquieR` result
#'
#' @param res the `dataquieR` result
#' @param ... not used
#'
#' @return a `plotly` object
#' @family plotly_shims
#' @concept plotly_shims
#' @noRd
util_as_plotly_from_res <- function(res, ...) {
  util_stop_if_not(
    "Internal error, sorry: res w/o PlotlyPlot in util_as_plotly_from_res(), please report" = # nolint: line_length_linter.
      "PlotlyPlot" %in% names(res)
  )
  have_plot_ly <- util_ensure_suggested("plotly", "plot interactive figures", err = FALSE)
  util_stop_if_not(
    "Internal error, sorry: plotly uninstalled in util_as_plotly_from_res(), please report" = # nolint: line_length_linter.
      have_plot_ly
  )
  py <- res[["PlotlyPlot"]]
  util_stop_if_not(
    "Internal error in util_as_plotly_from_res(), sorry: PlotlyPlot should be a plotly, please report" = # nolint: line_length_linter.
      inherits(py, "plotly")
  )
  py
}

#' Wrapper around `plotly::ggplotly` for optional dependency and lazy plots
#'
#' @param p a \code{ggplot} or \code{dq_lazy_ggplot} object.
#' @param ... passed through to \code{plotly::ggplotly}.
#'
#' @return A \code{plotly} object.
#'
#' @noRd
util_ggplotly <- function(p, ...) {
  util_ensure_suggested("plotly")
  if (inherits(p, "dq_lazy_ggplot")) {
    p <- prep_realize_ggplot(p)
  }
  r <- try(plotly::ggplotly(p, ...))
  if (util_is_try_error(r)) {
    r <- util_plotly_text(conditionMessage(util_attr(r, "condition",
          exact = TRUE
        )))
  }
  # try to reduce size of the plotly object
  r$x$visdat <- NULL
  r$x$attrs <- NULL # no manipulation with R needed any more.
  r$x$cur_data <- NULL
  r
}

#' Wrapper around plotly::plotly_build for optional dependency and lazy plots
#'
#' @noRd
util_plotly_build <- function(p, ...) {
  util_ensure_suggested("plotly")
  if (inherits(p, "dq_lazy_ggplot")) {
    p <- prep_realize_ggplot(p)
  }

  plotly::plotly_build(p, ...)
}
