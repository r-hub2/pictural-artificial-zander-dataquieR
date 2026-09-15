#' Plot to un-disclosed `ggplot` object
#'
#' @param expr plot expression
#' @param w width in cm
#' @param h height in cm
#'
#' @return `ggplot` object, but rendered (no original data included)
#'
#' @noRd
util_plot2svg_object <- function(expr, w = 21.2, h = 15.9, sizing_hints) {
  util_ensure_suggested("grImport2")
  util_ensure_suggested("rsvg")
  orig_sizing_hints <- sizing_hints
  if (!is.null(sizing_hints)) {
    sizing_hints <- util_finalize_sizing_hints(sizing_hints = sizing_hints)
    if (!is.null(sizing_hints) &&
        !is.null(sizing_hints$h_in_cm) &&
        !is.null(sizing_hints$w_in_cm)) {
      w <- sizing_hints$w_in_cm
      h <- sizing_hints$h_in_cm
    }
  }
  tmpfil <- NULL
  withr::with_tempfile("tmpfil", fileext = ".svg", {
    htmltools::capturePlot(
      expr = {
        rlang::eval_tidy(expr)
      },
      filename = tmpfil,
      device = grDevices::svg,
      width = w / 2.54, height = h / 2.54,
      # Default SVG device point size and resolution are used intentionally.
    )
    .svg <- readLines(tmpfil)
    .svg <- gsub("<svg ", '<svg preserveAspectRatio="none" ', .svg, fixed = TRUE) # nolint: line_length_linter.
    # The adjusted SVG is passed through rsvg without keeping an intermediate.
    rsvg::rsvg_svg(charToRaw(paste0(.svg, collapse = "\n")), tmpfil)
    # Historical magick-based fallback removed here. Inspect commit fe31a7aa6c
    # before restoring the old undisclosed-figure wrapper.
    raw <- grImport2::readPicture(tmpfil)
    res <- util_svg_plot_proxy(tmpfil)
    attr(res, "sizing_hints") <- orig_sizing_hints
    return(res)
  })
}

#' `Plotly` to un-disclosed `ggplot` object
#'
#' @param plotly the object
#' @param w width in cm
#' @param h height in cm
#'
#' @return `ggplot` object, but rendered (no original data included)
#'
#' @noRd
util_plotly2svg_object <- function(plotly, w = 21.2, h = 15.9, sizing_hints) {
  util_ensure_suggested("grImport2")
  util_ensure_suggested("rsvg")
  util_ensure_suggested("plotly")
  util_ensure_suggested("reticulate")
  orig_sizing_hints <- sizing_hints
  if (!is.null(sizing_hints)) {
    sizing_hints <- util_finalize_sizing_hints(sizing_hints = sizing_hints)
    if (!is.null(sizing_hints) &&
        !is.null(sizing_hints$h_in_cm) &&
        !is.null(sizing_hints$w_in_cm)) {
      w <- sizing_hints$w_in_cm
      h <- sizing_hints$h_in_cm
    }
  }
  # Reticulate and Kaleido setup is intentionally left to the user environment.

  tmpfil <- NULL
  withr::with_tempfile("tmpfil", fileext = ".svg", {
    fn <- try(
      {
        plotly::save_image(
          p = plotly, file = tmpfil,
          width = w / 2.54 * 96,
          height = h / 2.54 * 96
        )
      },
      silent = TRUE
    )

    if (util_is_try_error(fn)) {
      util_error(
        c(
          "Could not use %s to convert a plotly to a static image:\n",
          "%s",
          "\nYou can try to fix that by setting up reticulate properly and",
          "setting everything up as described in %s. You can also file a bug",
          "report, because this shold be needed only as a fallback during",
          "the development of new indicator functions."
        ),
        sQuote("plotly::save_image()"),
        dQuote(conditionMessage(util_attr(fn, "condition", exact = TRUE))),
        sQuote("? plotly::save_image")
      )
      # Historical STS-specific Reticulate setup removed here. Inspect commit
      # c48791d1e3 before restoring the local Python override.
    }

    .svg <- readLines(tmpfil, warn = FALSE)
    .svg <- gsub("<svg ", '<svg preserveAspectRatio="none" ', .svg, fixed = TRUE) # nolint: line_length_linter.
    # The adjusted SVG is passed through rsvg without keeping an intermediate.
    rsvg::rsvg_svg(charToRaw(paste0(.svg, collapse = "\n")), tmpfil)
    res <- util_svg_plot_proxy(tmpfil)
    attr(res, "sizing_hints") <- orig_sizing_hints
    return(res)
  })
}

#' Detect un-disclosed `ggplot`
#'
#' @param x the object to check
#'
#' @return `TRUE` or `FALSE`
#' @noRd
util_is_svg_object <- function(x) {
  inherits(x, "svg_plot_proxy") || (
    util_is_gg_plot(x) &&
      all(vapply(lapply(x$layers, `[[`, "geom"),
          inherits, "GeomDrawGrob",
          FUN.VALUE = logical(1)
        ))
  )
}

#' Internal helper: svg plot proxy
#'
#' @noRd
util_svg_plot_proxy <- function(svg_file) {
  svg_raw <-
    charToRaw(paste(readLines(svg_file, warn = FALSE), collapse = "\n"))
  structure(list(svg = svg_raw),
    class = "svg_plot_proxy"
  )
}

#' @exportS3Method grid::grid.draw
grid.draw.svg_plot_proxy <- function(x, ...) {
  util_ensure_suggested("grImport2")

  tmp_svg <- tempfile(fileext = ".svg")
  withr::defer(unlink(tmp_svg))
  writeBin(x$svg, tmp_svg)

  pic <- grImport2::readPicture(tmp_svg)
  grob <- grImport2::pictureGrob(pic, clip = "off", distort = TRUE)
  grid::grid.draw(grob)
}

#' @export
print.svg_plot_proxy <- function(x, ...) {
  grid::grid.newpage()
  grid::grid.draw(x)
  invisible(x)
}
