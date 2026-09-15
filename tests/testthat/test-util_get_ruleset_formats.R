test_that("util_get_ruleset_formats uses cached custom formats", {
  skip_on_cran()

  formats <- data.frame(
    category = 1:5,
    label = paste0("label-", 1:5),
    color = rep("1 2 3", 5),
    stringsAsFactors = FALSE
  )
  cache <- new.env(parent = emptyenv())

  result <- with_dataframe_environment(quote({
    prep_add_data_frames(custom_formats = formats)
    withr::with_options(
      list(dataquieR.grading_formats = "custom_formats"),
      util_get_ruleset_formats()
    )
  }), env = cache)

  expect_identical(result$label, formats$label)
  expect_type(result$category, "integer")
})

test_that("util_get_ruleset_formats accepts one data-frame ruleset", {
  skip_on_cran()

  formats <- data.frame(
    category = 1:5,
    label = paste0("label-", 1:5),
    color = rep("1 2 3", 5),
    stringsAsFactors = FALSE
  )
  cache <- new.env(parent = emptyenv())

  testthat::local_mocked_bindings(
    util_get_rule_sets = function(...) {
      data.frame(
        GRADING_RULESET = "custom",
        indicator_metric = "metric_a",
        dqi_catnum = 3L,
        stringsAsFactors = FALSE
      )
    }
  )

  result <- with_dataframe_environment(quote({
    prep_add_data_frames(custom_formats = formats)
    withr::with_options(
      list(dataquieR.grading_formats = "custom_formats"),
      util_get_ruleset_formats()
    )
  }), env = cache)

  expect_identical(result$category, 1:5)
  expect_identical(result$label, formats$label)
})

test_that(
  "util_get_ruleset_formats falls back for unavailable custom formats",
  {
    skip_on_cran()

    expect_message(
      result <- withr::with_options(
        list(dataquieR.grading_formats = "missing_custom_formats"),
        util_get_ruleset_formats()
      ),
      "using the default formats"
    )

    expect_s3_class(result, "data.frame")
    expect_true(all(c("category", "label", "color") %in% colnames(result)))
  }
)

test_that("util_get_ruleset_formats rejects incomplete category formats", {
  skip_on_cran()

  formats <- data.frame(
    category = 1:4,
    label = paste0("label-", 1:4),
    color = rep("1 2 3", 4),
    stringsAsFactors = FALSE
  )
  cache <- new.env(parent = emptyenv())

  expect_error(
    with_dataframe_environment(quote({
      prep_add_data_frames(incomplete_formats = formats)
      withr::with_options(
        list(dataquieR.grading_formats = "incomplete_formats"),
        util_get_ruleset_formats()
      )
    }), env = cache),
    "Did not find formats for all categories"
  )
})

test_that("util_get_ruleset_formats follows rule-set category counts", {
  skip_on_cran()
  skip_if_not_installed("openxlsx2")

  incomplete_formats <- data.frame(
    category = 1:5,
    label = paste0("label-", 1:5),
    color = rep("1 2 3", 5),
    stringsAsFactors = FALSE
  )
  formats <- data.frame(
    category = 1:6,
    label = paste0("label-", 1:6),
    color = rep("1 2 3", 6),
    stringsAsFactors = FALSE
  )
  cache <- new.env(parent = emptyenv())

  testthat::local_mocked_bindings(
    util_get_rule_sets = function(...) {
      list(`0` = data.frame(
        GRADING_RULESET = "0",
        indicator_metric = "metric_a",
        dqi_catnum = 6L,
        stringsAsFactors = FALSE
      ))
    }
  )

  expect_error(
    with_dataframe_environment(quote({
      prep_add_data_frames(incomplete_formats = incomplete_formats)
      withr::with_options(
        list(dataquieR.grading_formats = "incomplete_formats"),
        util_get_ruleset_formats()
      )
    }), env = cache),
    "Did not find formats for all categories"
  )

  result <- with_dataframe_environment(quote({
    prep_add_data_frames(custom_formats = formats)
    withr::with_options(
      list(dataquieR.grading_formats = "custom_formats"),
      util_get_ruleset_formats()
    )
  }), env = cache)

  expect_identical(result$category, 1:6)
  expect_identical(result$label, formats$label)
})

test_that("util_get_ruleset_formats defaults to five categories", {
  skip_on_cran()

  formats <- data.frame(
    category = 1:5,
    label = paste0("label-", 1:5),
    color = rep("1 2 3", 5),
    stringsAsFactors = FALSE
  )
  cache <- new.env(parent = emptyenv())

  testthat::local_mocked_bindings(
    util_get_rule_sets = function(...) {
      list(`0` = data.frame(indicator_metric = "metric_a"))
    }
  )

  result <- with_dataframe_environment(quote({
    prep_add_data_frames(custom_formats = formats)
    withr::with_options(
      list(dataquieR.grading_formats = "custom_formats"),
      util_get_ruleset_formats()
    )
  }), env = cache)

  expect_identical(result$category, 1:5)
  expect_identical(result$label, formats$label)
})

test_that("util_get_ruleset_formats drops empty extra categories", {
  skip_on_cran()

  formats <- data.frame(
    category = c(1:5, NA),
    label = c(paste0("label-", 1:5), "unused"),
    color = rep("1 2 3", 6),
    stringsAsFactors = FALSE
  )
  cache <- new.env(parent = emptyenv())

  result <- with_dataframe_environment(quote({
    prep_add_data_frames(custom_formats = formats)
    withr::with_options(
      list(dataquieR.grading_formats = "custom_formats"),
      util_get_ruleset_formats()
    )
  }), env = cache)

  expect_identical(result$category, 1:5)
  expect_identical(result$label, paste0("label-", 1:5))
})
