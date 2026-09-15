test_that("HTML progress viewer handling respects RStudio viewer constraints", {
  skip_on_cran()

  calls <- new.env(parent = emptyenv())
  calls$opened <- character()
  testthat::local_mocked_bindings(
    util_view_file = function(path) {
      calls$opened <- c(calls$opened, basename(path))
      invisible(path)
    }
  )

  direct_dir <- withr::local_tempdir()
  direct_index <- file.path(direct_dir, "index.html")
  testthat::local_mocked_bindings(
    util_works_in_rs_viewer = function(path) {
      expect_identical(path, direct_index)
      FALSE
    }
  )
  util_init_html_progress(
    output_dir = direct_dir,
    content_file = direct_index,
    title = "Direct viewer",
    view = TRUE,
    rep_id = "direct"
  )
  expect_identical(calls$opened, "index.html")

  calls$opened <- character()
  deferred_dir <- withr::local_tempdir()
  deferred_index <- file.path(deferred_dir, "index.html")
  local({
    testthat::local_mocked_bindings(
      util_works_in_rs_viewer = function(path) {
        expect_identical(path, deferred_index)
        TRUE
      }
    )
    util_init_html_progress(
      output_dir = deferred_dir,
      content_file = deferred_index,
      title = "Deferred viewer",
      view = TRUE,
      rep_id = "deferred"
    )
    util_write_index_html(deferred_index, c("<!-- done -->", "<html></html>"))
    expect_identical(calls$opened, character())
  })
  expect_identical(calls$opened, "index.html")
})

test_that("last-error sentinel distinguishes seeded report phases", {
  skip_on_cran()

  util_seed_last_error("phase marker")

  expect_true(util_is_last_error_sentinel())
})

test_that("HTML progress hooks update index and renderinfo without viewing", {
  skip_on_cran()

  rm(list = ls(dataquieR:::.progress_hooks),
    envir = dataquieR:::.progress_hooks
  )
  withr::defer(rm(list = ls(dataquieR:::.progress_hooks),
      envir = dataquieR:::.progress_hooks
    ))

  out_dir <- withr::local_tempdir()
  index <- file.path(out_dir, "index.html")
  start_time <- as.POSIXct("2026-07-31 12:00:00", tz = "UTC")

  handles <- util_init_html_progress(
    output_dir = out_dir,
    content_file = index,
    title = "Progress Report",
    view = FALSE,
    rep_id = "progress-id",
    start_time = start_time
  )
  withr::defer(util_write_index_html(
    index,
    c("<!-- done -->", "<html></html>")
  ))

  expect_named(handles, c(".hi", ".hp", ".hm", ".hf"))
  expect_true(file.exists(file.path(out_dir, ".report", "logo.png")))
  expect_true(file.exists(file.path(out_dir, ".report", "renderinfo.js")))

  util_call_progress_hooks("init", n = 4)
  util_call_progress_hooks("progress", percent = 50)
  util_call_progress_hooks("msg", status = "Rendering", msg = "Halfway")

  html <- paste(readLines(index, warn = FALSE), collapse = "\n")
  renderinfo <- paste(
    readLines(file.path(out_dir, ".report", "renderinfo.js"), warn = FALSE),
    collapse = "\n"
  )

  expect_match(html, "Progress Report", fixed = TRUE)
  expect_match(html, "Rendering", fixed = TRUE)
  expect_match(html, "Halfway", fixed = TRUE)
  expect_match(renderinfo, "progress-id", fixed = TRUE)
})

test_that("HTML progress preserves a successful final state", {
  skip_on_cran()

  out_dir <- withr::local_tempdir()
  index <- file.path(out_dir, "index.html")

  local({
    handles <- util_init_html_progress(
      output_dir = out_dir,
      content_file = index,
      title = "Completed report",
      view = FALSE,
      rep_id = "completed"
    )
    util_write_index_html(index, util_index_redirect_lines(
      title = "Completed report",
      target_rel = ".report/report.html"
    ))
    handles$.hf()
  })

  html <- paste(readLines(index, warn = FALSE), collapse = "\n")
  expect_match(html, "<!-- done -->", fixed = TRUE)
  expect_true(file.exists(file.path(out_dir, ".report", "render-complete")))
  expect_true(file.exists(file.path(out_dir, ".report", "renderinfo.js")))
})

test_that("a written report page preserves the progress entry point", {
  skip_on_cran()

  out_dir <- withr::local_tempdir()
  index <- file.path(out_dir, "index.html")

  local({
    util_init_html_progress(
      output_dir = out_dir,
      content_file = index,
      title = "Completed report",
      view = FALSE,
      rep_id = "completed"
    )
    writeLines("<html>complete report</html>", file.path(
      out_dir, ".report", "report.html"
    ))
  })

  html <- paste(readLines(index, warn = FALSE), collapse = "\n")
  expect_match(html, "<!-- done -->", fixed = TRUE)
  expect_match(html, "location.replace", fixed = TRUE)
  expect_true(file.exists(file.path(out_dir, ".report", "renderinfo.js")))
})
