#' Plot to un-disclosed `ggplot` object
#'
#' @param expr plot expression
#' @param w width in cm
#' @param h height in cm
#'
#' @return `ggplot` object, but rendered (no original data included)
#'
#' @keywords internal
util_plot2svg_object <- function(expr, w = 21.2, h = 15.9, sizing_hints) {
  util_ensure_suggested("grImport2")
  util_ensure_suggested("ggpubr")
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
    htmltools::capturePlot(expr = {
      rlang::eval_tidy(expr)
    },
    filename = tmpfil,
    device = grDevices::svg,
    width = w / 2.54, height = h / 2.54,
    #    pointsize = 12
    #    res = 1/72
    )
    .svg <- readLines(tmpfil)
    .svg <- gsub("<svg ", '<svg preserveAspectRatio="none" ', .svg, fixed = TRUE)
    # writeLines(.svg, tmpfil)
    rsvg::rsvg_svg(charToRaw(paste0(.svg, collapse = "\n")), tmpfil)
    # res <- as.environment(list(
    #   x = magick::image_read_svg(tmpfil, width = w, height = h)
    # ))
    # class(res) <- "dataquieR_undisclosed_figure"
    raw <- grImport2::readPicture(tmpfil)
    res <- ggpubr::as_ggplot(grImport2::pictureGrob(raw, clip = "off", distort = TRUE,
                                                    width = w,
                                                    height = h,
                                                    default.units = "cm"))
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
#' @keywords internal
util_plotly2svg_object <- function(plotly, w = 21.2, h = 15.9, sizing_hints) { # FIXME: Also for thumbnails, if not ggplot exists.
  util_ensure_suggested("grImport2")
  util_ensure_suggested("ggpubr")
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
  # install.packages('reticulate')
  # reticulate::install_miniconda()
  # reticulate::conda_install('r-reticulate', 'python-kaleido')
  # reticulate::conda_install('r-reticulate', 'plotly', channel = 'plotly')
  # reticulate::use_miniconda('r-reticulate')

  tmpfil <- NULL
  withr::with_tempfile("tmpfil", fileext = ".svg", {

    fn <- try({
      plotly::save_image(p = plotly, file = tmpfil,
                         width = w / 2.54 * 96,
                         height = h / 2.54 * 96)
    }, silent = TRUE)

    if (util_is_try_error(fn)) {
      util_error(
        c("Could not use %s to convert a plotly to a static image:\n",
          "%s",
          "\nYou can try to fix that by setting up reticulate properly and",
          "setting everything up as described in %s. You can also file a bug",
          "report, because this shold be needed only as a fallback during",
          "the development of new indicator functions."
        ),
        sQuote("plotly::save_image()"),
        dQuote(conditionMessage(attr(fn, "condition"))),
        sQuote("? plotly::save_image")
      )
      # For STS: reticulate::use_python("/Users/struckmanns/Library/r-miniconda-arm64/envs/r-reticulate/bin/python", required = T)
    }

    .svg <- readLines(tmpfil, warn = FALSE)
    .svg <- gsub("<svg ", '<svg preserveAspectRatio="none" ', .svg, fixed = TRUE)
    # writeLines(.svg, tmpfil)
    rsvg::rsvg_svg(charToRaw(paste0(.svg, collapse = "\n")), tmpfil)
    # res <- as.environment(list(
    #   x = magick::image_read_svg(tmpfil, width = w, height = h)
    # ))
    # class(res) <- "dataquieR_undisclosed_figure"
    raw <- grImport2::readPicture(tmpfil)
    res <- ggpubr::as_ggplot(grImport2::pictureGrob(raw, clip = "off", distort = TRUE,
                                                    width = w,
                                                    height = h,
                                                    default.units = "cm"))
    attr(res, "sizing_hints") <- orig_sizing_hints
    return(res)
  })
}

#' Detect un-disclosed `ggplot`
#'
#' @param x the object to check
#'
#' @return `TRUE` or `FALSE`
#' @keywords internal
util_is_svg_object <- function(x) {
  ggplot2::is.ggplot(x) &&
    all(vapply(lapply(x$layers, `[[`, "geom"),
               inherits, "GeomDrawGrob", FUN.VALUE = logical(1)))
}
