test_that("util_col_description maps report aliases through alias metadata", {
  skip_on_cran()

  testthat::local_mocked_bindings(
    util_function_description = function(fname) fname,
    util_map_by_largest_prefix = function(needle, haystack) needle
  )

  function_alias_map <- data.frame(
    alias = "grouped_report_alias",
    name = "acc_cat_distributions"
  )

  expect_identical(
    util_col_description(
      "grouped_report_alias",
      function_alias_map = function_alias_map
    ),
    "acc_cat_distributions"
  )
})

test_that("util_col_description maps aliases through report metadata", {
  skip_on_cran()

  testthat::local_mocked_bindings(
    util_cll_nm2fkt_nm = function(cn, report) {
      expect_identical(cn, "report_alias")
      expect_identical(report, "report-object")
      "des_summary"
    },
    util_function_description = function(fname) fname,
    util_map_by_largest_prefix = function(needle, haystack) needle
  )

  expect_identical(
    util_col_description("report_alias", report = "report-object"),
    "des_summary"
  )
})

test_that("util_col_description keeps manual-prefix fallback", {
  skip_on_cran()

  testthat::local_mocked_bindings(
    util_function_description = function(fname) fname,
    util_map_by_largest_prefix = function(needle, haystack) {
      if (identical(needle, "acc_cat_distributions_ABC")) {
        "acc_cat_distributions"
      } else {
        NA_character_
      }
    }
  )

  expect_identical(
    util_col_description("acc_cat_distributions_ABC"),
    "acc_cat_distributions"
  )
})

test_that("util_col_description applies prefix fallback after alias metadata", {
  skip_on_cran()

  testthat::local_mocked_bindings(
    util_function_description = function(fname) fname,
    util_map_by_largest_prefix = function(needle, haystack) {
      if (identical(needle, "acc_cat_distributions_ABC")) {
        "acc_cat_distributions"
      } else {
        NA_character_
      }
    }
  )

  function_alias_map <- data.frame(
    alias = "grouped_report_alias",
    name = "acc_cat_distributions_ABC"
  )

  expect_identical(
    util_col_description(
      "grouped_report_alias",
      function_alias_map = function_alias_map
    ),
    "acc_cat_distributions"
  )
})

test_that("util_col_description returns known function descriptions", {
  skip_on_cran()

  description <- util_col_description("des_summary")

  expect_type(description, "character")
  expect_length(description, 1L)
  expect_false(grepl("No description found", description, fixed = TRUE))
})

test_that("util_col_description falls back for unknown aliases", {
  skip_on_cran()

  unknown <- "local_helper_without_catalog"

  expect_identical(
    util_col_description(unknown),
    util_function_description(unknown)
  )
  expect_match(
    util_col_description(unknown),
    "No description found",
    fixed = TRUE
  )
})
