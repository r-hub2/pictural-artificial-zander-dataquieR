missing_test_package <- function(prefix) {
  installed <- rownames(installed.packages())
  i <- 1L
  repeat {
    candidate <- sprintf("%s_%d", prefix, i)
    if (!candidate %in% installed) {
      return(candidate)
    }
    i <- i + 1L
  }
}

test_that("util_ensure_suggested works", {
  skip_on_cran()
  ip <- installed.packages()
  existing <- "ggplot2"
  if (!(existing %in% ip[, "Package"])) {
    fail(paste(
      "Package", dQuote(existing),
      "is a dependency as of writing this.",
      "Therefore, it is assumed to be an installed package here.",
      "However, it is missing."
    ))
  }
  unexist <- missing_test_package(existing)
  expect_silent(util_ensure_suggested(existing, "test the function"))
  expect_error(util_ensure_suggested(unexist, "test the function"),
    regexp = "The package.+is required to\\s+test\\s+the\\s+function",
    perl = TRUE
  )
})

test_that("util_ensure_suggested reports optional misses without stopping", {
  skip_on_cran()
  unexist <- missing_test_package("dataquieR_optional")

  expect_warning(
    available <- util_ensure_suggested(unexist, "exercise an optional branch",
      err = FALSE
    ),
    regexp = "Missing the package"
  )
  expect_false(available)
})

test_that("util_ensure_suggested formats optional misses without cli", {
  skip_on_cran()
  unexist <- missing_test_package("dataquieR_optional_no_cli")

  testthat::local_mocked_bindings(
    is_installed = function(pkg) FALSE,
    .package = "rlang"
  )

  expect_warning(
    available <- util_ensure_suggested(unexist,
      "exercise the plain fallback",
      err = FALSE
    ),
    regexp = dQuote("prep_check_for_dataquieR_updates()"),
    fixed = TRUE
  )
  expect_false(available)
})

test_that("util_ensure_suggested reports hard misses after install checks", {
  skip_on_cran()
  unexist <- missing_test_package("dataquieR_hard_missing_no_cli")

  testthat::local_mocked_bindings(
    requireNamespace = function(...) FALSE,
    .package = "base"
  )
  testthat::local_mocked_bindings(
    check_installed = function(...) NULL,
    is_installed = function(...) FALSE,
    .package = "rlang"
  )

  expect_error(
    util_ensure_suggested(unexist, "exercise hard fallback", err = TRUE),
    regexp = dQuote("prep_check_for_dataquieR_updates()"),
    fixed = TRUE
  )
})

test_that("util_ensure_suggested can import exported helpers", {
  skip_on_cran()
  skip_if_not_installed("ggplot2")

  imported <- local({
    util_ensure_suggested("ggplot2", "build a plot", and_import = "^aes$")
    exists("aes", inherits = FALSE) && identical(aes, ggplot2::aes)
  })

  expect_true(imported)
})

test_that("util_have_suggested caches availability checks", {
  skip_on_cran()
  skip_if_not_installed("ggplot2")
  unexist <- missing_test_package("dataquieR_cached_optional")

  rm(
    list = intersect(
      c("ggplot2", unexist),
      ls(envir = .util_optional_packages, all.names = TRUE)
    ),
    envir = .util_optional_packages
  )

  expect_true(util_have_suggested("ggplot2", "exercise the cache"))
  expect_true(exists("ggplot2",
      envir = .util_optional_packages,
      inherits = FALSE
    ))

  expect_silent(
    available <- util_have_suggested(unexist, "exercise the missing cache")
  )
  expect_false(available)
  expect_false(util_have_suggested(unexist, "reuse the missing cache"))
})
