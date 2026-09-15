#' Internal helper: statichtmlWidget
#'
#' @noRd
statichtmlWidget <- function(html, width = NULL, height = NULL,
  elementId = NULL, sizingPolicy,
  js = character()) {
  util_ensure_suggested("htmlwidgets", "Render results in RMarkdown")
  if (missing(sizingPolicy)) sizingPolicy <- htmlwidgets::sizingPolicy(fill = TRUE) # nolint: line_length_linter.
  # Historical knit_print and dependency-order variants removed here.

  o <- htmltools::as.tags(html)

  deps <- htmltools::findDependencies(o)
  deps <- c(list(rmarkdown::html_dependency_jquery()), deps) # order matters, jquery before potential datatable.js # nolint: line_length_linter.
  # forward options using x
  deps <- deps[!duplicated(
    vapply(deps, "[[", "name", FUN.VALUE = character(1))
  )]
  x <- list(
    html = as.character(htmltools::div(
      class = "htmlwidget_container",
      o
    )) # htmltools::HTML(o)))
    , js = htmlwidgets::JS(js)
  )

  # create widget
  w <- htmlwidgets::createWidget(
    name = "statichtmlWidget",
    dependencies = deps,
    x,
    width = width,
    height = height,
    package = "dataquieR",
    elementId = elementId,
    sizingPolicy = sizingPolicy
  )

  # Historical plotly-dependency debug breakpoint removed here.

  w
}
