test_that("output-directory aliases are interchangeable", {
  target <- file.path(tempdir(), "report-output-alias")

  expect_identical(
    util_resolve_output_dir_alias(
      dir = target,
      dir_missing = FALSE
    ),
    target
  )
  expect_identical(
    util_resolve_output_dir_alias(
      output_dir = target,
      output_dir_missing = FALSE
    ),
    target
  )
  expect_identical(
    util_resolve_output_dir_alias(
      dir = target,
      output_dir = target,
      dir_missing = FALSE,
      output_dir_missing = FALSE
    ),
    target
  )
  expect_null(util_resolve_output_dir_alias(
    dir = NULL,
    output_dir = NULL,
    dir_missing = FALSE,
    output_dir_missing = FALSE
  ))
  expect_error(
    util_resolve_output_dir_alias(
      dir = target,
      output_dir = paste0(target, "-other"),
      dir_missing = FALSE,
      output_dir_missing = FALSE
    ),
    "must refer to the same directory"
  )
})

test_that("report-by bundles retain list compatibility", {
  report <- structure(
    list(marker = TRUE),
    class = c("dataquieR_resultset2", "list")
  )
  bundle <- util_new_dataquieR_report_by(
    list(segment = list(stratum = report)),
    meta = list(title = "Bundle")
  )

  expect_s3_class(bundle, "dataquieR_report_by")
  expect_true(is.list(bundle))
  expect_identical(bundle$segment$stratum, report)
  expect_identical(
    unname(util_report_by_in_memory_reports(bundle)),
    list(report)
  )
})

test_that("in-memory report-by bundles render distinct subreports", {
  output_dir <- file.path(withr::local_tempdir(), "bundle")
  calls <- new.env(parent = emptyenv())
  calls$reports <- list()
  report <- function(sdn) {
    out <- structure(
      list(marker = sdn),
      class = c("dataquieR_resultset2", "list")
    )
    attr(out, "report_by_info") <- list(
      sdn = sdn,
      level_name = paste("Level", sdn),
      name_of_study_data = "study_data",
      cur_seg = paste("Segment", sdn)
    )
    out
  }
  bundle <- util_new_dataquieR_report_by(
    list(A = list(one = report("A_one")), B = list(two = report("B_two"))),
    meta = list(
      title = "Bundle",
      subtitle = "",
      disable_plotly = FALSE,
      advanced_options = list(),
      html_table_backend = "DT2"
    )
  )

  testthat::local_mocked_bindings(
    util_report_by_output = function(r, output_dir, sdn, ...) {
      calls$reports[[length(calls$reports) + 1L]] <- list(
        marker = r$marker,
        output_dir = output_dir,
        sdn = sdn
      )
      invisible(NULL)
    },
    util_create_report_by_overview = function(output_dir) {
      calls$overview <- output_dir
      invisible(NULL)
    }
  )

  expect_invisible(print(
    bundle,
    output_dir = output_dir,
    view = FALSE
  ))
  expect_equal(
    vapply(calls$reports, `[[`, character(1), "sdn"),
    c("A_one", "B_two")
  )
  expect_length(unique(vapply(
    calls$reports, `[[`, character(1), "sdn"
  )), 2L)
  expect_identical(calls$overview, util_normalize_path(output_dir))
})

test_that("disk-backed bundles require their original report files", {
  missing_dir <- file.path(tempdir(), "missing-report-by-bundle")
  bundle <- util_new_dataquieR_report_by(
    list(A = list(one = NULL)),
    output_dir = missing_dir
  )

  expect_s3_class(bundle, "dataquieR_report_by")
  expect_false(any(vapply(
    unclass(bundle),
    inherits,
    logical(1),
    what = "dataquieR_resultset2"
  )))
  expect_error(
    print(bundle, view = FALSE),
    "needs its original output directory"
  )
})

test_that("complete disk-backed bundles are not rendered twice", {
  output_dir <- withr::local_tempdir()
  dir.create(file.path(output_dir, "report_one"))
  writeLines("<!-- done -->", file.path(output_dir, "index.html"))
  writeLines(
    "<!-- done -->",
    file.path(output_dir, "report_one", "index.html")
  )
  file.create(file.path(output_dir, "report_one.dq2"))
  saveRDS(
    list(report_files = "report_one.dq2"),
    file.path(output_dir, "report_by_meta.RDS")
  )
  bundle <- util_new_dataquieR_report_by(
    list(A = list(one = NULL)),
    output_dir = output_dir,
    report_files = "report_one.dq2"
  )

  testthat::local_mocked_bindings(
    prep_load_report = function(...) {
      stop("must not load completed reports")
    },
    util_report_by_output = function(...) {
      stop("must not render completed reports")
    }
  )

  expect_invisible(print(bundle, view = FALSE))
})

test_that("report-by file names remain distinct after sanitizing", {
  output_dir <- withr::local_tempdir()
  first_summary <- data.frame(value = 1)
  attr(first_summary, "this") <- list(sdn = "A-B")
  saveRDS(
    first_summary,
    file.path(output_dir, "report_summary_AB.RDS")
  )

  expect_identical(util_report_by_safe_name("A-B", output_dir), "AB")
  second_name <- util_report_by_safe_name("AB", output_dir)
  expect_match(second_name, "^AB_[[:xdigit:]]{8}$")
  expect_false(identical(second_name, "AB"))
})

test_that("public report interfaces expose both directory aliases", {
  expect_true(all(c("dir", "output_dir") %in% names(formals(dq_report2))))
  expect_true(all(c("dir", "output_dir") %in% names(formals(dq_report_by))))
  expect_true(all(c("dir", "output_dir") %in%
        names(formals(print.dataquieR_resultset2))))
  expect_true(all(c("dir", "output_dir") %in%
        names(formals(print.dataquieR_report_by))))
  expect_identical(formals(dq_report_by)$also_print, FALSE)
})

test_that("public report interfaces forward directory aliases", {
  calls <- new.env(parent = emptyenv())
  calls$args <- list()
  testthat::local_mocked_bindings(
    util_resolve_output_dir_alias = function(...) {
      calls$args[[length(calls$args) + 1L]] <- list(...)
      stop("alias captured")
    }
  )

  expect_error(dq_report2(NULL, dir = "dq2-dir"), "alias captured")
  expect_identical(calls$args[[1]]$dir, "dq2-dir")
  expect_true(calls$args[[1]]$output_dir_missing)

  expect_error(dq_report_by(NULL, dir = "by-dir"), "alias captured")
  expect_identical(calls$args[[2]]$dir, "by-dir")
  expect_true(calls$args[[2]]$output_dir_missing)

  report <- structure(list(), class = c("dataquieR_resultset2", "list"))
  expect_error(
    print.dataquieR_resultset2(report, output_dir = "print-dir"),
    "alias captured"
  )
  expect_identical(calls$args[[3]]$output_dir, "print-dir")
  expect_true(calls$args[[3]]$dir_missing)
})
