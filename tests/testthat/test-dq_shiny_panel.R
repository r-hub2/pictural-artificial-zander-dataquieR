test_that("dq_shiny_panel_ui creates a namespaced output container", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  skip_if_not_installed("htmltools")

  ui <- dq_shiny_panel_ui("dq", title = "Quality panel")
  html <- htmltools::renderTags(ui)$html

  expect_match(html, "Quality panel", fixed = TRUE)
  expect_match(html, "dq-panel", fixed = TRUE)
  expect_match(html, "dataquieR-shiny-panel", fixed = TRUE)
})

test_that("dq_shiny_panel_server renders the module panel output", {
  skip_on_cran()
  skip_if_not_installed("shiny")

  report_dir <- tempfile("dq-report-")
  dir.create(report_dir)
  writeLines(
    "<html><body>report</body></html>",
    file.path(report_dir, "index.html")
  )

  state <- new.env(parent = emptyenv())
  shiny::testServer(
    dq_shiny_panel_server,
    args = list(report_dir = report_dir, height = "321px"),
    {
      state$panel <- output$panel
    }
  )

  panel <- state$panel
  expect_match(panel$html, "<iframe", fixed = TRUE)
  expect_match(panel$html, "index.html", fixed = TRUE)
  expect_match(panel$html, "height:321px", fixed = TRUE)
})

test_that("dq_shiny_panel_html embeds an existing report directory", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  skip_if_not_installed("htmltools")

  report_dir <- tempfile("dq-report-")
  dir.create(report_dir)
  writeLines(
    "<html><body>report</body></html>",
    file.path(report_dir, "index.html")
  )

  panel <- dq_shiny_panel_html(
    report_dir = report_dir,
    resource_prefix = paste0("dq-test-", as.integer(runif(1, 1, 1e8)))
  )
  html <- htmltools::renderTags(panel)$html

  expect_s3_class(panel, "shiny.tag")
  expect_match(html, "<iframe", fixed = TRUE)
  expect_match(html, "index.html", fixed = TRUE)
})

test_that("dq_shiny_panel_html finds standalone dq_report2 entry point", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  skip_if_not_installed("htmltools")

  report_dir <- tempfile("dq-report-")
  dir.create(file.path(report_dir, ".report"), recursive = TRUE)
  writeLines(
    "<html><body>report</body></html>",
    file.path(report_dir, ".report", "report.html")
  )

  panel <- dq_shiny_panel_html(
    report_dir = report_dir,
    resource_prefix = paste0("dq-test-", as.integer(runif(1, 1, 1e8)))
  )
  html <- htmltools::renderTags(panel)$html

  expect_match(html, ".report/report.html", fixed = TRUE)
})

test_that("dq_shiny_panel_html handles empty and invalid report inputs", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  skip_if_not_installed("htmltools")

  empty_panel <- dq_shiny_panel_html()
  empty_html <- htmltools::renderTags(empty_panel)$html

  expect_match(empty_html, "dataquieR-shiny-panel-empty", fixed = TRUE)
  expect_match(
    empty_html,
    "No dataquieR result or report directory selected.",
    fixed = TRUE
  )
  expect_error(
    dq_shiny_panel_html(report_dir = tempfile("missing-report-")),
    "existing directory"
  )

  report_dir <- tempfile("dq-report-")
  dir.create(report_dir)
  writeLines(
    "<html><body>dashboard</body></html>",
    file.path(report_dir, "dashboard.html")
  )

  expect_identical(
    util_shiny_panel_report_entry(report_dir),
    "dashboard.html"
  )

  unlink(file.path(report_dir, "dashboard.html"))
  expect_error(
    util_shiny_panel_report_entry(report_dir),
    "known dataquieR report entry point"
  )
})

test_that("dq_shiny_panel_html validates inputs", {
  skip_on_cran()
  skip_if_not_installed("shiny")

  expect_error(
    dq_shiny_panel_html(result = list(), resource_prefix = "dq-test-invalid"),
    "must inherit"
  )
  expect_error(
    dq_shiny_panel_html(
      result = structure(list(), class = "master_result"),
      report_dir = tempdir(),
      resource_prefix = "dq-test-both"
    ),
    "either"
  )
})

test_that("dq_shiny_panel_html renders master_result objects", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  skip_if_not_installed("htmltools")

  testthat::local_mocked_bindings(
    util_save_master_result_html = function(x, dir, ...) {
      writeLines(
        "<html><body>master result</body></html>",
        file.path(dir, "index.html")
      )
    }
  )
  result <- structure(
    list(),
    class = c("dataquieR_result", "master_result", "list")
  )
  panel <- dq_shiny_panel_html(
    result = result,
    resource_prefix = paste0("dq-test-", as.integer(runif(1, 1, 1e8)))
  )
  html <- htmltools::renderTags(panel)$html

  expect_s3_class(panel, "shiny.tag")
  expect_match(html, "<iframe", fixed = TRUE)
  expect_match(html, "index.html", fixed = TRUE)
})

test_that("dq_shiny_panel_html renders single dataquieR_result objects", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  skip_if_not_installed("htmltools")

  testthat::local_mocked_bindings(
    print.dataquieR_result = function(x, view = TRUE, ...) {
      htmltools::tags$div("single result")
    }
  )
  result <- structure(list(), class = c("dataquieR_result", "list"))

  panel <- dq_shiny_panel_html(
    result = result,
    resource_prefix = paste0("dq-test-", as.integer(runif(1, 1, 1e8)))
  )
  html <- htmltools::renderTags(panel)$html

  expect_s3_class(panel, "shiny.tag")
  expect_match(html, "<iframe", fixed = TRUE)
  expect_match(html, "index.html", fixed = TRUE)
})

test_that("dq_shiny_panel_html reports unrenderable result objects", {
  skip_on_cran()
  skip_if_not_installed("shiny")

  testthat::local_mocked_bindings(
    print.dataquieR_result = function(x, view = TRUE, ...) NULL
  )
  result <- structure(list(), class = c("dataquieR_result", "list"))

  expect_error(
    dq_shiny_panel_html(
      result = result,
      resource_prefix = paste0("dq-test-", as.integer(runif(1, 1, 1e8)))
    ),
    "Could not render"
  )
})
