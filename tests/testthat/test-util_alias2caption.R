test_that("util_alias2caption falls back for unknown aliases", {
  skip_on_cran()

  expect_identical(
    unname(util_alias2caption(NA_character_)),
    "Unkown Alias"
  )
  expect_identical(
    util_alias2caption("local_helper_without_catalog"),
    c(local_helper_without_catalog = "local_helper_without_catalog")
  )
  expect_identical(
    util_alias2caption("local_helper_without_catalog", long = TRUE),
    c(local_helper_without_catalog = "local_helper_without_catalog")
  )
  expect_error(
    util_alias2caption("local_helper_without_catalog", long = NA),
    "must not contain NAs"
  )
})

test_that("util_alias2caption maps known report prefixes and suffixes", {
  skip_on_cran()

  expect_identical(
    util_alias2caption("acc_cat_distributions_ABC"),
    c(acc_cat_distributions_ABC = "Distributions-Cat ABC")
  )

  long_caption <- util_alias2caption("acc_cat_distributions_ABC", long = TRUE)
  expect_named(long_caption, "acc_cat_distributions_ABC")
  expect_match(
    unname(long_caption),
    "Distribution",
    ignore.case = TRUE
  )
  expect_match(unname(long_caption), "ABC", fixed = TRUE)
})

test_that("util_alias2caption keeps non-prefix aliases as suffixes", {
  skip_on_cran()

  testthat::local_mocked_bindings(
    util_map_by_largest_prefix = function(needle, haystack) {
      "acc_cat_distributions"
    },
    .package = "dataquieR"
  )

  expect_identical(
    util_alias2caption("legacy_alias"),
    c(legacy_alias = "Distributions-Cat legacy alias")
  )
})
