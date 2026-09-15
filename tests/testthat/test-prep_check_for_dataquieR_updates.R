local_prep_update_mocks <- function(byte_compile_state, calls) {
  testthat::local_mocked_bindings(
    util_update_packages = function(...) {
      calls$update <- list(...)
      invisible(NULL)
    },
    util_install_packages = function(...) {
      calls$install <- list(...)
      invisible(NULL)
    },
    packageVersion = function(...) package_version("1.0.0"),
    util_installed_package_is_byte_compiled = function(...) byte_compile_state,
    util_loaded_package_install_markers = function(...) list(),
    util_loaded_packages_with_available_updates = function(...) character(),
    util_confirm_loaded_package_updates = function(...) TRUE,
    util_loaded_packages_with_changed_install = function(...) character(),
    util_restart_or_warn_about_loaded_updates = function(...) NULL,
    util_message = function(...) NULL,
    .env = parent.frame()
  )
}

local_byte_compile_probe_package <- function() {
  tmp_dir <- tempfile("bytecompile-check-")
  dir.create(tmp_dir)
  pkg_dir <- file.path(tmp_dir, "bcprobe")
  dir.create(pkg_dir)
  dir.create(file.path(pkg_dir, "R"))
  writeLines(c(
    "Package: bcprobe",
    "Version: 0.0.1",
    "Title: Byte Compile Probe",
    "Description: Byte compile probe.",
    "Author: A B",
    "Maintainer: A B <a@example.org>",
    "License: MIT",
    "Encoding: UTF-8"
  ), file.path(pkg_dir, "DESCRIPTION"))
  writeLines(c("export(probe)", "export(loop_probe)"),
    file.path(pkg_dir, "NAMESPACE"))
  writeLines(c(
    "probe <- function(x) {",
    "  x + 1",
    "}",
    "loop_probe <- function(x) {",
    "  y <- 0",
    "  for (i in seq_len(x)) y <- y + i",
    "  y",
    "}"
  ), file.path(pkg_dir, "R", "probe.R"))
  pkg_dir
}

local_install_byte_compile_probe <- function(pkg_dir, lib_dir, install_opt) {
  dir.create(lib_dir)
  output <- system2(file.path(R.home("bin"), "R"),
    c("CMD", "INSTALL", "--no-multiarch", "-l", lib_dir, install_opt, pkg_dir),
    stdout = TRUE, stderr = TRUE)
  status <- attr(output, "status")
  if (is.null(status)) {
    status <- 0
  }
  expect_identical(status, 0)
}

test_that("prep_check_for_dataquieR_updates reinstalls byte-compiled package", {
  skip_on_cran()

  calls <- new.env(parent = emptyenv())
  local_prep_update_mocks(FALSE, calls)

  prep_check_for_dataquieR_updates(ask = FALSE, deps = FALSE,
    byte_compile = TRUE)

  expect_identical(calls$update$type, "source")
  expect_identical(calls$update$INSTALL_opts, "--byte-compile")
  expect_identical(calls$install$pkgs, "dataquieR")
  expect_identical(calls$install$type, "source")
  expect_identical(calls$install$INSTALL_opts, "--byte-compile")
})

test_that("prep_check_for_dataquieR_updates reinstalls uncompiled package", {
  skip_on_cran()

  calls <- new.env(parent = emptyenv())
  local_prep_update_mocks(TRUE, calls)

  prep_check_for_dataquieR_updates(ask = FALSE, deps = FALSE,
    byte_compile = FALSE)

  expect_identical(calls$update$type, "source")
  expect_identical(calls$update$INSTALL_opts, "--no-byte-compile")
  expect_identical(calls$install$pkgs, "dataquieR")
  expect_identical(calls$install$type, "source")
  expect_identical(calls$install$INSTALL_opts, "--no-byte-compile")
})

test_that("prep_check_for_dataquieR_updates keeps matching install", {
  skip_on_cran()

  calls <- new.env(parent = emptyenv())
  local_prep_update_mocks(TRUE, calls)

  prep_check_for_dataquieR_updates(ask = FALSE, deps = FALSE,
    byte_compile = TRUE)

  expect_identical(calls$update$type, "source")
  expect_identical(calls$update$INSTALL_opts, "--byte-compile")
  expect_false(exists("install", envir = calls, inherits = FALSE))
})

test_that("prep_check_for_dataquieR_updates confirms loaded package updates", {
  skip_on_cran()

  calls <- new.env(parent = emptyenv())
  testthat::local_mocked_bindings(
    util_update_packages = function(...) {
      calls$update <- list(...)
      invisible(NULL)
    },
    packageVersion = function(...) package_version("1.0.0"),
    util_installed_package_is_byte_compiled = function(...) TRUE,
    util_loaded_package_install_markers = function(...) {
      list(dataquieR = c(version = "1.0.0"))
    },
    util_loaded_packages_with_available_updates = function(...) "dataquieR",
    util_confirm_loaded_package_updates = function(packages, ask) {
      calls$confirm <- list(packages = packages, ask = ask)
      FALSE
    },
    util_message = function(...) NULL
  )

  prep_check_for_dataquieR_updates(ask = TRUE, deps = FALSE,
    byte_compile = TRUE)

  expect_identical(calls$confirm$packages, "dataquieR")
  expect_true(calls$confirm$ask)
  expect_false(exists("update", envir = calls, inherits = FALSE))
})

test_that("prep_check_for_dataquieR_updates restarts after loaded changes", {
  skip_on_cran()

  calls <- new.env(parent = emptyenv())
  testthat::local_mocked_bindings(
    util_update_packages = function(...) {
      calls$update <- list(...)
      invisible(NULL)
    },
    packageVersion = function(...) package_version("1.0.0"),
    util_installed_package_is_byte_compiled = function(...) TRUE,
    util_loaded_package_install_markers = function(...) {
      list(dataquieR = c(version = "1.0.0"))
    },
    util_loaded_packages_with_available_updates = function(...) character(),
    util_confirm_loaded_package_updates = function(...) TRUE,
    util_loaded_packages_with_changed_install = function(...) "dataquieR",
    util_restart_or_warn_about_loaded_updates = function(packages) {
      calls$restart <- packages
      invisible(TRUE)
    },
    util_message = function(...) NULL
  )

  prep_check_for_dataquieR_updates(ask = FALSE, deps = FALSE,
    byte_compile = TRUE)

  expect_identical(calls$restart, "dataquieR")
})

test_that("util_restart_or_warn_about_loaded_updates warns without restart", {
  skip_on_cran()

  calls <- new.env(parent = emptyenv())
  testthat::local_mocked_bindings(
    util_restart_rstudio_session = function() FALSE,
    util_warning = function(...) {
      calls$warning <- list(...)
      invisible(NULL)
    }
  )

  expect_false(util_restart_or_warn_about_loaded_updates("dataquieR"))
  expect_identical(calls$warning[[1]], "%s")
  expect_match(calls$warning[[2]], "dataquieR")
  expect_true(calls$warning$immediate)
})

test_that("util_installed_package_is_byte_compiled detects installed state", {
  skip_on_cran()

  pkg_dir <- local_byte_compile_probe_package()
  lib_byte <- tempfile("lib-byte-")
  lib_no_byte <- tempfile("lib-no-byte-")
  local_install_byte_compile_probe(pkg_dir, lib_byte, "--byte-compile")
  local_install_byte_compile_probe(pkg_dir, lib_no_byte, "--no-byte-compile")
  old_lib_paths <- .libPaths()
  withr::defer(.libPaths(old_lib_paths))

  .libPaths(c(lib_byte, old_lib_paths))
  expect_true(util_installed_package_is_byte_compiled("bcprobe"))

  .libPaths(c(lib_no_byte, old_lib_paths))
  expect_false(util_installed_package_is_byte_compiled("bcprobe"))
})
