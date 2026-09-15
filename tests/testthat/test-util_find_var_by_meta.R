test_that("util_find_var_by_meta maps across allowed metadata columns", {
  skip_on_cran()

  meta_data <- data.frame(
    VAR_NAMES = c("x", "y"),
    LABEL = c("X label", "Y label"),
    LONG_LABEL = c("Long X", "Long Y"),
    TARGET = c("target_x", "target_y")
  )

  expect_equal(
    util_find_var_by_meta(
      c("x", "Y label", "Long X", "missing", NA_character_),
      meta_data,
      target = "TARGET",
      ifnotfound = c("fallback_x", "fallback_y", "fallback_long",
        "fallback_missing", "fallback_na")
    ),
    c("target_x", "target_y", "target_x", "fallback_missing",
      NA_character_)
  )
})

test_that("util_find_var_by_meta validates fallbacks and sources", {
  skip_on_cran()

  meta_data <- data.frame(VAR_NAMES = "x", TARGET = "target_x")

  expect_equal(
    util_find_var_by_meta(NA_character_, meta_data, target = "TARGET",
      ifnotfound = "fallback"),
    "fallback"
  )

  expect_error(
    util_find_var_by_meta(c("x", "y"), meta_data, target = "TARGET",
      ifnotfound = c("one", "two", "three")),
    "ifnotfound"
  )

  expect_error(
    util_find_var_by_meta("x", data.frame(OTHER = "x"),
      allowed_sources = "MISSING"),
    "mappable column"
  )
})
