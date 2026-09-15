#' Embed dataquieR results in a Shiny application
#'
#' `dq_shiny_panel_ui()` and `dq_shiny_panel_server()` provide an experimental
#' Shiny module for showing already computed dataquieR output in an application.
#' The first version supports either a single result object or an existing
#' report directory. `shiny` remains an optional dependency.
#'
#' @param id [character] module id.
#' @param title [character] optional panel title.
#' @param height [character] CSS height of the embedded panel.
#'
#' @return `dq_shiny_panel_ui()` returns a Shiny UI tag list.
#'
#' @examples
#' \dontrun{
#' if (requireNamespace("shiny", quietly = TRUE)) {
#'   ui <- shiny::fluidPage(
#'     dq_shiny_panel_ui("dq", title = "Data quality")
#'   )
#'   server <- function(input, output, session) {
#'     dq_shiny_panel_server("dq", report_dir = "my_report")
#'   }
#'   shiny::shinyApp(ui, server)
#' }
#' }
#'
#' @export
dq_shiny_panel_ui <- function(id, title = NULL, height = "700px") {
  util_ensure_suggested("shiny", goal = "embed dataquieR results in Shiny")
  ns <- shiny::NS(id)
  htmltools::tagList(
    if (!is.null(title)) htmltools::tags$h3(title),
    htmltools::div(
      class = "dataquieR-shiny-panel",
      style = htmltools::css(width = "100%", min.height = height),
      shiny::uiOutput(ns("panel"))
    )
  )
}

#' @rdname dq_shiny_panel_ui
#'
#' @param result optional computed `dataquieR_result` or `master_result`
#'   object to display.
#' @param report_dir [character] optional directory created by `dq_report2()`
#'   or `dq_report_by()`.
#'
#' @return `dq_shiny_panel_server()` returns the Shiny module server value.
#'
#' @export
dq_shiny_panel_server <- function(id, result = NULL, report_dir = NULL,
  height = "700px") {
  util_ensure_suggested("shiny", goal = "embed dataquieR results in Shiny")
  shiny::moduleServer(id, function(input, output, session) {
    output$panel <- shiny::renderUI({
      dq_shiny_panel_html(
        result = result,
        report_dir = report_dir,
        height = height,
        resource_prefix = paste0(
          "dataquieR-shiny-panel-",
          gsub("[^A-Za-z0-9_-]", "-", session$ns("resource"))
        )
      )
    })
  })
}

#' Build embeddable dataquieR panel HTML
#'
#' This lower-level helper is useful for tests and custom Shiny modules.
#'
#' @inheritParams dq_shiny_panel_server
#' @param resource_prefix [character] Shiny static resource prefix.
#'
#' @return `htmltools` tag object.
#'
#' @export
dq_shiny_panel_html <- function(result = NULL, report_dir = NULL,
  height = "700px", resource_prefix = "dataquieR-shiny-panel") {
  util_ensure_suggested("shiny", goal = "embed dataquieR results in Shiny")
  util_expect_scalar(height, check_type = is.character)
  util_expect_scalar(resource_prefix, check_type = is.character)

  if (!is.null(result) && !is.null(report_dir)) {
    util_error("Please provide either %s or %s, not both.",
      sQuote("result"),
      sQuote("report_dir")
    )
  }
  if (is.null(result) && is.null(report_dir)) {
    return(htmltools::div(
      class = "dataquieR-shiny-panel-empty",
      "No dataquieR result or report directory selected."
    ))
  }

  if (!is.null(report_dir)) {
    src <- util_shiny_panel_register_report_dir(report_dir, resource_prefix)
  } else {
    src <- util_shiny_panel_register_result(result, resource_prefix)
  }

  htmltools::tags$iframe(
    src = src,
    title = "dataquieR result",
    style = htmltools::css(
      border = "0",
      width = "100%",
      height = height
    )
  )
}

#' Internal helper: shiny panel register report dir
#'
#' @noRd
util_shiny_panel_register_report_dir <- function(report_dir, resource_prefix) {
  util_expect_scalar(report_dir, check_type = is.character)
  if (!dir.exists(report_dir)) {
    util_error("%s must point to an existing directory.", sQuote("report_dir"))
  }

  report_dir <- normalizePath(report_dir, winslash = "/", mustWork = TRUE)
  entry <- util_shiny_panel_report_entry(report_dir)
  shiny::addResourcePath(resource_prefix, report_dir)
  file.path(resource_prefix, entry)
}

#' Internal helper: shiny panel report entry
#'
#' @noRd
util_shiny_panel_report_entry <- function(report_dir) {
  candidates <- c(
    "index.html",
    file.path(".report", "report.html"),
    "report.html",
    "dashboard.html",
    "tables.html"
  )
  existing <- candidates[file.exists(file.path(report_dir, candidates))]
  if (length(existing) == 0) {
    util_error(
      "%s does not contain a known dataquieR report entry point.",
      sQuote("report_dir")
    )
  }
  existing[[1]]
}

#' Internal helper: shiny panel register result
#'
#' @noRd
util_shiny_panel_register_result <- function(result, resource_prefix) {
  if (!inherits(result, "dataquieR_result") &&
      !inherits(result, "master_result")) {
    util_error(
      "%s must inherit from %s or %s.",
      sQuote("result"),
      dQuote("dataquieR_result"),
      dQuote("master_result")
    )
  }

  result_dir <- tempfile("dataquieR-shiny-result-")
  dir.create(result_dir, recursive = TRUE)
  if (inherits(result, "master_result")) {
    util_save_master_result_html(result, dir = result_dir)
  } else {
    withr::with_dir(result_dir, {
      rendered <- print(result, view = FALSE)
      if (inherits(rendered, "shiny.tag") ||
          inherits(rendered, "shiny.tag.list")) {
        htmltools::save_html(rendered, "index.html")
      }
    })
  }
  if (!file.exists(file.path(result_dir, "index.html"))) {
    util_error("Could not render %s to HTML.", sQuote("result"))
  }

  shiny::addResourcePath(resource_prefix, result_dir)
  file.path(resource_prefix, "index.html")
}
