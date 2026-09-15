test_that("util_odm2dataquieR falls back and caches when converter is absent", {
  skip_on_cran()
  testthat::local_mocked_bindings(
    util_ensure_suggested = function(...) FALSE
  )
  util_prune_odm_cache()

  expect_message(
    first <- util_odm2dataquieR("study.xml"),
    "Optional feature requires 'dataquieR2odm'"
  )
  expect_null(first)
  expect_length(ls(envir = odm_cache, all.names = TRUE), 1L)

  expect_silent(second <- util_odm2dataquieR("study.xml"))
  expect_null(second)

  util_prune_odm_cache()
  expect_length(ls(envir = odm_cache, all.names = TRUE), 0L)
})

test_that("util_odm2dataquieR checks installed converter before fallback", {
  skip_on_cran()

  calls <- new.env(parent = emptyenv())
  testthat::local_mocked_bindings(
    odm_installed = TRUE,
    util_ensure_suggested = function(...) {
      calls$args <- list(...)
      FALSE
    }
  )
  util_prune_odm_cache()

  expect_message(
    result <- util_odm2dataquieR("study.xml"),
    "Optional feature requires 'dataquieR2odm'"
  )

  expect_null(result)
  expect_identical(calls$args[[1L]], "dataquieR2odm")
  expect_identical(calls$args$goal, "Read ODM files")
  expect_false(calls$args$err)
  expect_identical(calls$args$and_import, "odm2dataquieR")
  util_prune_odm_cache()
})

test_that("util_is_odm_xml detects ODM root and namespace", {
  skip_on_cran()
  odm_file <- tempfile(fileext = ".xml")
  non_odm_file <- tempfile(fileext = ".xml")
  missing_namespace_file <- tempfile(fileext = ".xml")

  writeLines(
    paste0(
      "<?xml version='1.0'?><ODM ",
      "xmlns='http://www.cdisc.org/ns/odm/v1.3'><Study /></ODM>"
    ),
    odm_file,
    useBytes = TRUE
  )
  writeLines("<Root xmlns='http://www.cdisc.org/ns/odm/v1.3' />",
    non_odm_file,
    useBytes = TRUE
  )
  writeLines("<ODM><Study /></ODM>", missing_namespace_file, useBytes = TRUE)

  expect_true(util_is_odm_xml(odm_file))
  expect_false(util_is_odm_xml(non_odm_file))
  expect_false(util_is_odm_xml(missing_namespace_file))
})
