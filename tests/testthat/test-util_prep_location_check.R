test_that("util_prep_location_check handles incomplete location metadata", {
  skip_on_cran()

  meta_data <- data.frame(
    VAR_NAMES = c("complete", "metric_only", "range_only", "bad_metric"),
    DATA_TYPE = "float",
    MISSING_LIST = "",
    LOCATION_METRIC = c("mean", "median", "", "mode"),
    LOCATION_RANGE = c("[1; 3]", "", "[2; 4]", "[0; 1]")
  )

  complete <- util_prep_location_check("complete", meta_data,
    report_problems = "message"
  )
  expect_equal(complete$Metric[["complete"]], "mean")
  expect_s3_class(complete$Range[["complete"]], "interval")

  expect_message(
    incomplete <- util_prep_location_check(
      c("metric_only", "range_only"),
      meta_data,
      report_problems = "message"
    ),
    "metadata for the expected location is incomplete"
  )
  expect_named(incomplete$Metric, c("metric_only", "range_only"))
  expect_named(incomplete$Range, c("metric_only", "range_only"))

  expect_message(
    invalid <- util_prep_location_check("bad_metric", meta_data,
      report_problems = "message"
    ),
    "mean or median"
  )
  expect_true(is.na(invalid$Metric[["bad_metric"]]))
  expect_true(is.na(invalid$Range[["bad_metric"]]))
})

test_that(
  "util_prep_location_check reports missing metadata by configured severity",
  {
    skip_on_cran()

    meta_data <- data.frame(
      VAR_NAMES = c("missing", "bad_metric"),
      DATA_TYPE = DATA_TYPES$FLOAT,
      MISSING_LIST = "",
      LOCATION_METRIC = c("", "mode"),
      LOCATION_RANGE = c("", "[0; 1]"),
      stringsAsFactors = FALSE
    )

    expect_warning(
      missing_warn <- util_prep_location_check(
        "missing",
        meta_data,
        report_problems = "warning"
      ),
      "metadata for the expected location is incomplete"
    )
    expect_true(is.na(missing_warn$Metric[["missing"]]))
    expect_true(is.na(missing_warn$Range[["missing"]]))

    expect_error(
      util_prep_location_check(
        "bad_metric",
        meta_data,
        report_problems = "error"
      ),
      "mean or median",
      class = dataquieR.intrinsic_applicability_problem
    )
  }
)
