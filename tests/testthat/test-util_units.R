clear_units_cache <- function(names) {
  rm(list = intersect(names, ls(envir = .dq_units_cache, all.names = TRUE)),
    envir = .dq_units_cache)
}

test_that("util_units helpers return NULL when suggested packages are absent", {
  skip_on_cran()

  clear_units_cache(c("valud", "pfx"))

  testthat::local_mocked_bindings(
    util_units_available = function() FALSE
  )

  expect_null(util_get_valid_udunits())
  expect_null(util_get_valid_udunits_prefixes())
  expect_false(exists("valud", envir = .dq_units_cache, inherits = FALSE))
  expect_false(exists("pfx", envir = .dq_units_cache, inherits = FALSE))
})

test_that("util_units helpers cache UDUNITS tables when available", {
  skip_on_cran()
  skip_if_not_installed("units")
  skip_if_not_installed("xml2")
  skip_if_not(util_units_available(), "units/xml2 are not usable here")

  clear_units_cache(c("valud", "pfx"))
  withr::defer(clear_units_cache(c("valud", "pfx")))

  valid_units <- util_get_valid_udunits()
  valid_prefixes <- util_get_valid_udunits_prefixes()

  expect_s3_class(valid_units, "data.frame")
  expect_true(all(c("symbol", "def") %in% names(valid_units)))
  expect_true(exists("valud", envir = .dq_units_cache, inherits = FALSE))
  expect_identical(util_get_valid_udunits(), valid_units)

  expect_s3_class(valid_prefixes, "data.frame")
  expect_true(ncol(valid_prefixes) >= 3)
  expect_true(exists("pfx", envir = .dq_units_cache, inherits = FALSE))
  expect_identical(util_get_valid_udunits_prefixes(), valid_prefixes)
})
