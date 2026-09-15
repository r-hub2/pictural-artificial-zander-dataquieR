test_that("util_load_helpers rejects non-development package contexts", {
  skip_on_cran()
  skip_if_not_installed("pkgload")
  skip_if_not(
    exists("util_load_helpers", mode = "function"),
    "development-only helper is not included in installed package tests"
  )

  testthat::local_mocked_bindings(
    packageName = function(...) "stats"
  )

  expect_error(
    util_load_helpers(),
    "should not be deployed",
    fixed = TRUE
  )
})

test_that("util_load_helpers sources temporary development helpers", {
  skip_on_cran()
  skip_if_not_installed("pkgload")
  skip_if_not(
    exists("util_load_helpers", mode = "function"),
    "development-only helper is not included in installed package tests"
  )
  skip_if_not(
    pkgload::is_dev_package("dataquieR"),
    "only meaningful for load_all-style development package tests"
  )

  helper_dir <- tempfile()
  dir.create(helper_dir)
  helper_file <- file.path(helper_dir, "local_helper.R")
  writeLines(
    "options(dataquieR.util_load_helpers_test = 'loaded')",
    helper_file
  )
  withr::defer(options(dataquieR.util_load_helpers_test = NULL))

  testthat::local_mocked_bindings(
    system.file = function(...) helper_dir
  )

  expect_null(util_load_helpers())
  expect_identical(getOption("dataquieR.util_load_helpers_test"), "loaded")
})

test_that("util_load_helpers tolerates missing helper directories", {
  skip_on_cran()
  skip_if_not_installed("pkgload")
  skip_if_not(
    exists("util_load_helpers", mode = "function"),
    "development-only helper is not included in installed package tests"
  )
  skip_if_not(
    pkgload::is_dev_package("dataquieR"),
    "only meaningful for load_all-style development package tests"
  )

  missing_dir <- tempfile()
  testthat::local_mocked_bindings(
    system.file = function(...) missing_dir
  )

  expect_null(util_load_helpers())
})

test_that("util_load_helpers reports sourced helpers interactively", {
  skip_on_cran()
  skip_if_not_installed("pkgload")
  skip_if_not(
    exists("util_load_helpers", mode = "function"),
    "development-only helper is not included in installed package tests"
  )
  skip_if_not(
    pkgload::is_dev_package("dataquieR"),
    "only meaningful for load_all-style development package tests"
  )

  helper_dir <- tempfile()
  dir.create(helper_dir)
  helper_file <- file.path(helper_dir, "local_helper.R")
  writeLines(
    "options(dataquieR.util_load_helpers_interactive_test = 'loaded')",
    helper_file
  )
  withr::defer(options(
    dataquieR.util_load_helpers_interactive_test = NULL
  ))

  testthat::local_mocked_bindings(
    system.file = function(...) helper_dir
  )
  testthat::local_mocked_bindings(
    interactive = function() TRUE,
    .package = "base"
  )

  expect_message(util_load_helpers(), "Sourcing helpers:")
  expect_identical(
    getOption("dataquieR.util_load_helpers_interactive_test"),
    "loaded"
  )
})
