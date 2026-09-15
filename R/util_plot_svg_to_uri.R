#' Render a plot as an `<img>` whose source is an inline `SVG` data URI
#'
#' Captures `expr` to an SVG file via [grDevices::svg()] and returns an
#' `htmltools::tags$img(class = "dataquieRfigure", src = ...)`. The
#' `src` is built as a `data:image/svg+xml;charset=utf-8,...` URI with
#' the SVG markup URL-encoded inline (no base64 round-trip).
#'
#' We deliberately keep the result wrapped in an `<img>` tag because
#' downstream code (in particular `util_iframe_it_if_needed()`) checks
#' for `it$name == "img"` to decide whether to wrap the figure in a
#' resizable iframe scaler. Switching to an inline `<svg>` element
#' breaks that detection silently -- the figure renders but loses its
#' resize handle.
#'
#' The root `<svg>` tag is augmented with `preserveAspectRatio="none"`
#' so the CSS in `inst/menu/style_iframe.css` can stretch the plot to
#' fill its parent container without aspect-ratio padding.
#'
#' @param expr plot expression
#' @param w width (in pixels, converted to inches via /72 for the svg device)
#' @param h height
#'
#' @return `htmltools` compatible object
#' @noRd
util_plot_svg_to_uri <- function(expr, w = 800, h = 600) {
  tmpfil <- NULL
  withr::with_tempfile("tmpfil", fileext = ".svg", {
    htmltools::capturePlot(
      expr = {
        rlang::eval_tidy(expr)
      },
      filename = tmpfil,
      device = grDevices::svg,
      width = w / 72, height = h / 72
    )

    svg_str <- paste(readLines(tmpfil, warn = FALSE), collapse = "\n")

    # Strip the XML / DOCTYPE preamble that the svg device writes ahead of
    # the <svg> root tag -- those don't belong inside a data: URI.
    svg_str <- sub(".*?(<svg )", "\\1", svg_str)

    # Inject preserveAspectRatio="none" so the SVG fills its <img> box
    # without aspect-ratio padding.
    svg_str <- sub(
      "<svg ",
      '<svg preserveAspectRatio="none" ',
      svg_str,
      fixed = TRUE
    )

    # Build a data: URI with URL-encoded SVG content. This keeps the
    # <img>-based output that util_iframe_it_if_needed() dispatches on
    # (it tests `it$name == "img"`), and avoids needing a base64 encoder
    # (no base64enc, no knitr::image_uri, ...). All modern browsers
    # accept URL-encoded SVG in data: URIs.
    img_src <- paste0(
      "data:image/svg+xml;charset=utf-8,",
      utils::URLencode(svg_str, reserved = TRUE)
    )

    htmltools::browsable(
      htmltools::tags$img(src = img_src, class = "dataquieRfigure")
    )
  })
}
