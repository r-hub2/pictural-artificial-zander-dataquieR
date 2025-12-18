#' HTML dependency for `jsPDF`
#'
#' Provides `jsPDF` for use in `Shiny` or `RMarkdown` via `htmltools`.
#'
#' @return An [htmltools::htmlDependency()] object
#' @export
html_dependency_jspdf <- function() {
  htmltools::htmlDependency(
    name = "jspdf",
    version = "2.5.1",
    src = system.file("jsPDF", package = "dataquieR"),
    script = "jspdf.umd.min.js"
  )
}
