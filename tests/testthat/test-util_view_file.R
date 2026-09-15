test_that("util_view_file delegates to configured viewer", {
  skip_on_cran()

  old_viewer <- getOption("viewer")
  on.exit(options(viewer = old_viewer), add = TRUE)

  seen <- new.env(parent = emptyenv())
  seen$file <- NULL
  options(viewer = function(file) {
    seen$file <- file
  })

  path <- tempfile(fileext = ".html")
  writeLines("<html></html>", path)

  expect_identical(util_view_file(path), path)
  expect_identical(seen$file, path)
})

test_that("util_really_rstudio requires the RStudio environment marker", {
  skip_on_cran()

  withr::local_envvar(RSTUDIO = "")

  expect_false(util_really_rstudio())
  expect_false(util_works_in_rs_viewer(tempfile()))
})

test_that("util_works_in_rs_viewer recognizes existing temp files in RStudio", {
  testthat::local_mocked_bindings(
    util_really_rstudio = function(...) TRUE
  )

  path <- tempfile()
  writeLines("x", path)
  nested_dir <- file.path(tempdir(), "viewer-nested")
  dir.create(nested_dir, showWarnings = FALSE)
  nested_path <- file.path(nested_dir, "x.txt")
  writeLines("x", nested_path)
  outside_path <- withr::local_tempfile(tmpdir = dirname(tempdir()))
  writeLines("x", outside_path)

  expect_true(util_works_in_rs_viewer(path))
  expect_true(util_works_in_rs_viewer(nested_path))
  expect_false(util_works_in_rs_viewer(outside_path))
  expect_false(util_works_in_rs_viewer(file.path(tempdir(), "missing.txt")))
  expect_error(
    util_works_in_rs_viewer(character()),
    "one non-empty path",
    fixed = TRUE
  )
})

test_that("util_view_file is inert without a viewer during tests", {
  skip_on_cran()

  old_viewer <- getOption("viewer")
  on.exit(options(viewer = old_viewer), add = TRUE)

  options(viewer = NULL)

  path <- tempfile(fileext = ".html")
  writeLines("<html></html>", path)

  expect_identical(util_view_file(path), path)
})
