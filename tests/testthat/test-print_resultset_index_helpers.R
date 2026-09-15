test_that("index loading lines escape text and switch progress indicators", {
  skip_on_cran()
  skip_if_not_installed("htmltools")

  old_outer_by <- .outer_by_env$outer_by
  on.exit(.outer_by_env$outer_by <- old_outer_by, add = TRUE)
  .outer_by_env$outer_by <- list(i = 2L, n = 4L, msg = "Outer <step>")

  lines <- util_index_loading_lines(
    title = "Report <A>",
    message = "Working <now>",
    detail = "Details <safe>",
    reload_ms = 1500,
    percent = 25,
    logo_rel = NULL
  )
  html <- paste(lines, collapse = "\n")

  expect_match(html, "Report &lt;A&gt;", fixed = TRUE)
  expect_match(html, "Working &lt;now&gt;", fixed = TRUE)
  expect_match(html, "Details &lt;safe&gt;", fixed = TRUE)
  expect_match(html, "width:25%", fixed = TRUE)
  expect_match(html, "Overall: 25% &mdash; step 2 / 4", fixed = TRUE)
  expect_match(html, "Math.max\\((250|2000), ms\\)")

  .outer_by_env$outer_by <- list(i = NA_integer_, n = 3L, msg = "Queued")
  spinner_lines <- util_index_loading_lines(
    title = "Report",
    percent = Inf,
    logo_rel = NULL
  )
  spinner_html <- paste(spinner_lines, collapse = "\n")

  expect_match(spinner_html, "spinner", fixed = TRUE)
  expect_match(spinner_html, "Overall: 3 steps", fixed = TRUE)

  .outer_by_env$outer_by <- list(i = 3L, n = NA_integer_, msg = "")
  partial_outer <- paste(util_index_loading_lines(
    title = "Report",
    percent = NA_real_,
    logo_rel = NULL
  ), collapse = "\n")
  expect_match(partial_outer, "Overall: step 3", fixed = TRUE)
  expect_match(partial_outer, "outer-spinner", fixed = TRUE)

  .outer_by_env$outer_by <- list(i = 5L, n = 4L, msg = "Queued <again>")
  invalid_index <- paste(util_index_loading_lines(
    title = "Report",
    percent = NA_real_,
    logo_rel = NULL
  ), collapse = "\n")
  expect_match(invalid_index, "Queued &lt;again&gt;", fixed = TRUE)
  expect_match(invalid_index, "Overall: step 5 / 4", fixed = TRUE)
  expect_match(invalid_index, "outer-spinner", fixed = TRUE)
})

test_that("index redirect and error lines include safe navigation scripts", {
  skip_on_cran()
  skip_if_not_installed("htmltools")

  redirect <- util_index_redirect_lines(
    title = "Done <report>",
    target_rel = "report.html?x=\"quoted\"",
    delay_ms = -1,
    logo_rel = NULL
  )
  redirect_html <- paste(redirect, collapse = "\n")

  expect_match(redirect_html, "<!-- done -->", fixed = TRUE)
  expect_match(redirect_html, "Done &lt;report&gt;", fixed = TRUE)
  expect_match(redirect_html, "var ms =\\s*0\\s*;")
  expect_match(redirect_html, "location.replace", fixed = TRUE)
  expect_match(redirect_html, "\\\\\"quoted\\\\\"", fixed = TRUE)

  error <- util_index_error_lines(
    title = "Broken <report>",
    message = "Failed <now>",
    detail = "Trace <safe>",
    logo_rel = NULL,
    reload_ms = -5
  )
  error_html <- paste(error, collapse = "\n")

  expect_match(error_html, "Broken &lt;report&gt;", fixed = TRUE)
  expect_match(error_html, "Failed &lt;now&gt;", fixed = TRUE)
  expect_match(error_html, "Trace &lt;safe&gt;", fixed = TRUE)
  expect_match(error_html, ".report/renderinfo.js", fixed = TRUE)
  expect_match(error_html, "var delay=\\s*5000\\s*;")
})

test_that("util_write_index_html writes through a temporary file", {
  skip_on_cran()

  path <- file.path(tempdir(), "dataquier-index-test.html")
  if (file.exists(path)) unlink(path, force = TRUE)

  expect_identical(
    util_write_index_html(path, c("<html>", "ok", "</html>")),
    invisible(path)
  )
  expect_identical(readLines(path, warn = FALSE), c("<html>", "ok", "</html>"))
  expect_false(file.exists(file.path(dirname(path), ".index.html")))
})

test_that("util_write_index_html cleans up after a failed atomic rename", {
  skip_on_cran()

  out_dir <- tempfile()
  dir.create(out_dir)
  path <- file.path(out_dir, "index.html")

  testthat::with_mocked_bindings(
    .package = "base",
    file.rename = function(...) FALSE,
    {
      expect_error(
        util_write_index_html(path, "<html>"),
        "Failed to atomically replace"
      )
    }
  )

  expect_false(file.exists(file.path(out_dir, ".index.html")))
})

test_that("index helper favicons escape logo paths and progress badges", {
  skip_on_cran()
  skip_if_not_installed("htmltools")

  logo <- "assets/logo <dq>.png"

  loading <- paste(util_index_loading_lines(
    title = "Report",
    percent = 42,
    logo_rel = logo
  ), collapse = "\n")
  expect_match(loading, "data:image/svg\\+xml", perl = TRUE)
  expect_match(loading, "assets/logo &lt;dq&gt;.png", fixed = TRUE)
  expect_match(loading, "42%", fixed = TRUE)

  redirect <- paste(util_index_redirect_lines(
    title = "Done",
    target_rel = "report.html",
    logo_rel = ".report/logo <dq>.png"
  ), collapse = "\n")
  expect_match(redirect, "rel=\"icon\"", fixed = TRUE)
  expect_match(redirect, ".report/logo &lt;dq&gt;.png", fixed = TRUE)

  error <- paste(util_index_error_lines(
    title = "Error",
    logo_rel = logo,
    reload_ms = 2500
  ), collapse = "\n")
  expect_match(error, "data:image/svg\\+xml", perl = TRUE)
  expect_match(error, "assets/logo &lt;dq&gt;.png", fixed = TRUE)
  expect_match(error, "var delay=\\s*2500\\s*;", perl = TRUE)
})

test_that("shared filesystem helper handles fallback cluster cases", {
  skip_on_cran()

  sequential <- util_check_shared_filesystem(
    dir = tempdir(),
    cl = NULL,
    cleanup = FALSE,
    tmp_prefix = "qif_seq_"
  )
  on.exit(unlink(sequential$tmpFile, force = TRUE), add = TRUE)

  expect_true(sequential$shared)
  expect_identical(sequential$exists, TRUE)
  expect_true(file.exists(sequential$tmpFile))

  failed_cluster <- testthat::with_mocked_bindings(
    .package = "parallel",
    clusterCall = function(...) {
      stop("mock cluster failure")
    },
    suppressWarnings(util_check_shared_filesystem(
      dir = tempdir(),
      cl = list("worker"),
      cleanup = TRUE,
      tmp_prefix = "qif_cluster_"
    ))
  )

  expect_true(failed_cluster$shared)
  expect_identical(unname(failed_cluster$exists), TRUE)
  expect_identical(names(failed_cluster$exists), "worker1")
  expect_false(file.exists(failed_cluster$tmpFile))
})
