test_that("report-by output handles failed subreports", {
  skip_on_cran()

  output_dir <- withr::local_tempdir()
  failed_report <- structure(
    "failed",
    class = "try-error",
    condition = simpleError("subreport failed")
  )

  expect_warning(
    result <- util_report_by_output(
      r = failed_report,
      output_dir = output_dir,
      sdn = "Segment A",
      level_name = "Level A",
      name_of_study_data = "study.csv",
      cur_seg = "SEG",
      also_print = TRUE,
      dots = list(),
      disable_plotly = FALSE,
      advanced_options = list(),
      html_table_backend = "default"
    ),
    "Could not compute report"
  )

  expect_null(result)
  expect_equal(list.files(output_dir, all.files = TRUE, no.. = TRUE),
    character())
})

test_that("report-by output skips unavailable output directories", {
  skip_on_cran()

  report <- list(marker = "unchanged")
  missing_dir <- file.path(tempdir(), "missing-report-by-output")

  expect_identical(
    util_report_by_output(
      r = report,
      output_dir = missing_dir,
      sdn = "Segment A",
      level_name = "Level A",
      name_of_study_data = "study.csv",
      cur_seg = "SEG",
      also_print = TRUE,
      dots = list(),
      disable_plotly = FALSE,
      advanced_options = list(),
      html_table_backend = "default"
    ),
    report
  )

  expect_null(
    util_report_by_meta(
      output_dir = missing_dir,
      names = "marker",
      env = list2env(list(marker = "present"))
    )
  )
  expect_false(dir.exists(missing_dir))
})

test_that("report-by metadata stores only available values", {
  skip_on_cran()

  output_dir <- withr::local_tempdir()
  env <- list2env(list(existing = "available"), parent = emptyenv())

  expect_null(
    util_report_by_meta(
      output_dir = output_dir,
      names = c("existing", "missing"),
      env = env
    )
  )

  expect_identical(
    readRDS(file.path(output_dir, "report_by_meta.RDS")),
    list(existing = "available")
  )
})

test_that("report-by output stores local artifacts and optional HTML output", {
  skip_on_cran()

  output_dir <- withr::local_tempdir()
  calls <- new.env(parent = emptyenv())
  summary.mock_report <- function(object, ...) {
    out <- data.frame(metric = "ok", stringsAsFactors = FALSE)
    attr(out, "this") <- list(existing = "value")
    out
  }
  report <- structure(list(marker = TRUE), class = "mock_report")
  report_meta_data <- data.frame(
    VAR_NAMES = "scaleA1",
    LABEL = "scaleA1: Questionnaire item",
    stringsAsFactors = FALSE
  )
  attr(report, "meta_data") <- report_meta_data
  attr(report, "label_col") <- LABEL
  withr::local_options(dataquieR.traceback = TRUE)

  testthat::local_mocked_bindings(
    prep_save_report = function(r, file) {
      calls$saved_report <- file
      calls$saved_meta_data <- util_attr(r, "meta_data", exact = TRUE)
      writeLines("saved", file)
      invisible(file)
    },
    util_setup_dashboard = function(r, make_links, return_table_only, repsum) {
      calls$dashboard <- list(
        make_links = make_links,
        return_table_only = return_table_only,
        repsum = repsum
      )
      data.frame(row = 1, stringsAsFactors = FALSE)
    },
    print.dataquieR_resultset2 = function(x, dir, view, disable_plotly,
      by_report, advanced_options,
      html_table_backend, ...) {
      calls$printed <- list(
        meta_data = util_attr(x, "meta_data", exact = TRUE),
        dir = dir,
        view = view,
        disable_plotly = disable_plotly,
        by_report = by_report,
        advanced_options = advanced_options,
        html_table_backend = html_table_backend,
        dots = list(...)
      )
      invisible(NULL)
    }
  )

  expect_null(util_report_by_output(
    r = report,
    output_dir = output_dir,
    sdn = "Segment A/1",
    level_name = "Level A",
    name_of_study_data = "study.csv",
    cur_seg = "SEG",
    also_print = TRUE,
    dots = list(cores = 2, ignored = "drop"),
    disable_plotly = TRUE,
    advanced_options = list(mode = "test"),
    html_table_backend = "DT2",
    view_meta_data = report_meta_data
  ))

  safe_name <- "SegmentA1"
  expect_true(file.exists(file.path(output_dir, "report_SegmentA1.dq2")))
  expect_identical(calls$saved_report,
    file.path(output_dir, paste0("report_", safe_name, ".dq2")))
  expect_identical(calls$saved_meta_data, report_meta_data)

  summary_file <- file.path(output_dir,
    paste0("report_summary_", safe_name, ".RDS"))
  summary_object <- readRDS(summary_file)
  expect_identical(attr(summary_object, "this")$stratum, "Level A")
  expect_identical(attr(summary_object, "this")$used_data_file, "study.csv")
  expect_identical(attr(summary_object, "this")$segment, "SEG")
  expect_identical(attr(summary_object, "this")$sdn, "Segment A/1")

  dashboard_file <- file.path(output_dir,
    paste0("report_dashboard_", safe_name, ".RDS"))
  dashboard_object <- readRDS(dashboard_file)
  expect_identical(attr(dashboard_object, "name_of_study_data"), "study.csv")
  expect_identical(attr(dashboard_object, "level_name"), "Level A")
  expect_false(calls$dashboard$make_links)
  expect_true(calls$dashboard$return_table_only)

  expect_true(dir.exists(file.path(output_dir, paste0("report_", safe_name))))
  expect_false(calls$printed$view)
  expect_true(calls$printed$disable_plotly)
  expect_true(calls$printed$by_report)
  expect_equal(
    calls$printed$meta_data[[LABEL]],
    "Questionnaire item"
  )
  expect_identical(calls$printed$advanced_options, list(mode = "test"))
  expect_identical(calls$printed$html_table_backend, "DT2")
  expect_identical(calls$printed$dots, list(cores = 2))
})

test_that("report-by output reports artifact and HTML write failures", {
  skip_on_cran()

  output_dir <- withr::local_tempdir()
  summary.mock_report <- function(object, ...) {
    out <- data.frame(metric = "ok", stringsAsFactors = FALSE)
    attr(out, "this") <- list(existing = "value")
    out
  }
  report <- structure(list(marker = TRUE), class = "mock_report")
  withr::local_options(dataquieR.traceback = TRUE)

  testthat::local_mocked_bindings(
    prep_save_report = function(...) {
      structure(
        "save failed",
        class = "try-error",
        condition = simpleError("save failed")
      )
    },
    util_setup_dashboard = function(...) {
      data.frame(row = 1, stringsAsFactors = FALSE)
    },
    print.dataquieR_resultset2 = function(...) {
      structure(
        "html failed",
        class = "try-error",
        condition = simpleError("html failed")
      )
    }
  )

  warnings <- new.env(parent = emptyenv())
  warnings$messages <- character()
  withCallingHandlers(
    util_report_by_output(
      r = report,
      output_dir = output_dir,
      sdn = "Segment A",
      level_name = "Level A",
      name_of_study_data = "study.csv",
      cur_seg = "SEG",
      also_print = TRUE,
      dots = list(),
      disable_plotly = FALSE,
      advanced_options = list(),
      html_table_backend = "DT"
    ),
    warning = function(w) {
      warnings$messages <- c(warnings$messages, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_match(warnings$messages[[1]], "Could not save report")
  expect_match(warnings$messages[[2]], "html failed", fixed = TRUE)
  expect_match(warnings$messages[[3]], "Could not create HTML report")

  expect_true(file.exists(file.path(output_dir, "report_summary_SegmentA.RDS")))
  expect_true(file.exists(file.path(
    output_dir,
    "report_dashboard_SegmentA.RDS"
  )))
})

test_that("report-by output reports local RDS and directory write failures", {
  skip_on_cran()

  output_dir <- tempfile("report-by-output-")
  dir.create(output_dir)
  withr::defer(unlink(output_dir, recursive = TRUE, force = TRUE))
  dir.create(file.path(output_dir, "report_summary_SegmentA.RDS"))
  dir.create(file.path(output_dir, "report_dashboard_SegmentA.RDS"))
  file.create(file.path(output_dir, "report_SegmentA"))

  summary.mock_report <- function(object, ...) {
    out <- data.frame(metric = "ok", stringsAsFactors = FALSE)
    attr(out, "this") <- list(existing = "value")
    out
  }
  report <- structure(list(marker = TRUE), class = "mock_report")

  testthat::local_mocked_bindings(
    prep_save_report = function(...) {
      invisible(NULL)
    },
    util_setup_dashboard = function(...) {
      data.frame(row = 1, stringsAsFactors = FALSE)
    }
  )

  warnings <- new.env(parent = emptyenv())
  warnings$messages <- character()
  capture.output(
    type = "message",
    withCallingHandlers(
      util_report_by_output(
        r = report,
        output_dir = output_dir,
        sdn = "Segment A",
        level_name = "Level A",
        name_of_study_data = "study.csv",
        cur_seg = "SEG",
        also_print = FALSE,
        dots = list(),
        disable_plotly = FALSE,
        advanced_options = list(),
        html_table_backend = "DT"
      ),
      warning = function(w) {
        warnings$messages <- c(warnings$messages, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    ),
    file = tempfile()
  )

  warnings <- paste(warnings$messages, collapse = "\n")
  expect_match(warnings, "Could not save report summary")
  expect_match(warnings, "Could not save report dashboard table")
  expect_match(warnings, "Could not create directory")
})
