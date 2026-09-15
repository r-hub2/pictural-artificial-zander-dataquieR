test_that("util_get_rule_sets returns shipped default ruleset", {
  skip_on_cran()
  skip_if_not_installed("openxlsx2")

  rulesets <- util_get_rule_sets()

  expect_named(rulesets, "0")
  expect_s3_class(rulesets[["0"]], "data.frame")
  expect_true(all(c(GRADING_RULESET, "indicator_metric") %in%
        colnames(rulesets[["0"]])))
  expect_true(all(c("CAT_applicability", "CAT_error",
        "CAT_anamat", "CAT_indicator_or_descriptor") %in%
        rulesets[["0"]]$indicator_metric))
})

test_that("util_get_rule_sets amends custom rulesets from default rules", {
  skip_on_cran()
  skip_if_not_installed("openxlsx2")

  shipped <- prep_get_data_frame(
    system.file("grading_rulesets.xlsx", package = "dataquieR")
  )
  custom_rulesets <- shipped[shipped[[GRADING_RULESET]] == "0", ,
    drop = FALSE]
  custom_row <- custom_rulesets[1, , drop = FALSE]
  custom_row[[GRADING_RULESET]] <- "custom"
  custom_rulesets <- util_rbind(custom_rulesets, custom_row)
  cache <- new.env(parent = emptyenv())

  rulesets <- with_dataframe_environment(quote({
    prep_add_data_frames(custom_rulesets = custom_rulesets)
    withr::with_options(
      list(dataquieR.grading_rulesets = "custom_rulesets"),
      util_get_rule_sets()
    )
  }), env = cache)

  expect_named(rulesets, c("0", "custom"))
  expect_equal(nrow(rulesets[["custom"]]), nrow(rulesets[["0"]]))
  expect_equal(unique(rulesets[["custom"]][[GRADING_RULESET]]), "custom")
})

test_that("util_get_rule_sets falls back for unavailable custom rulesets", {
  skip_on_cran()
  skip_if_not_installed("openxlsx2")

  expect_message(
    rulesets <- withr::with_options(
      list(dataquieR.grading_rulesets = "missing_custom_rulesets"),
      util_get_rule_sets()
    ),
    "using the default rulesets"
  )

  expect_named(rulesets, "0")
})

test_that("util_get_rule_sets rejects custom rulesets without a default", {
  skip_on_cran()
  skip_if_not_installed("openxlsx2")

  custom_rulesets <- data.frame(
    GRADING_RULESET = c("custom", "custom", ""),
    indicator_metric = c("metric_a", "", "metric_b"),
    dqi_catnum = c(5L, 5L, 5L),
    stringsAsFactors = FALSE
  )
  cache <- new.env(parent = emptyenv())

  expect_error(
    with_dataframe_environment(quote({
      prep_add_data_frames(custom_rulesets = custom_rulesets)
      withr::with_options(
        list(dataquieR.grading_rulesets = "custom_rulesets"),
        util_get_rule_sets()
      )
    }), env = cache),
    "No default GRADING_RULESET"
  )
})
