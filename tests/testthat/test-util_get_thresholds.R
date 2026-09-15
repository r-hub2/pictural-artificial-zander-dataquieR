test_that("missing grading rulesets fall back with one message", {
  skip_on_cran()
  util_clean_condition_once_cache()
  meta_data <- data.frame(
    VAR_NAMES = c("v1", "v2"),
    GRADING_RULESET = c("1", "1"),
    stringsAsFactors = FALSE
  )

  expect_message2(
    first <- util_get_thresholds("PCT_con_con", meta_data),
    "Using the default ruleset"
  )
  expect_message2(
    second <- util_get_thresholds("PCT_con_con", meta_data),
    NA
  )

  expect_equal(first, second)
  expect_named(first, c("v1", "v2"))
})

test_that("util_get_thresholds uses default and first matching rules", {
  skip_on_cran()
  rulesets <- list(
    "0" = data.frame(
      GRADING_RULESET = "0",
      indicator_metric = c("metric", "metric", "other"),
      dqi_catnum = c(2L, 2L, 1L),
      dqi_cat_1 = c("<=1", "<=10", "ok"),
      dqi_cat_2 = c(">1", ">10", NA_character_),
      stringsAsFactors = FALSE
    )
  )
  meta_data <- data.frame(
    VAR_NAMES = "v1",
    stringsAsFactors = FALSE
  )
  testthat::local_mocked_bindings(
    util_get_rule_sets = function(...) rulesets
  )

  expect_message2(
    thresholds <- util_get_thresholds("metric", meta_data),
    "More than one ruleset"
  )

  expect_equal(
    thresholds,
    list(v1 = c("1" = "<=1", "2" = ">1"))
  )

  missing_metric <- util_get_thresholds("missing", meta_data)
  expect_equal(
    missing_metric,
    list(v1 = setNames(character(0), character(0)))
  )
})
