test_that("util_translate_indicator_metrics translates known metric columns", {
  skip_on_cran()

  expect_equal(
    unname(util_translate_indicator_metrics(
      c(VAR_NAMES, STUDY_SEGMENT, "PCT_com_qum_nonresp")
    )),
    c(VAR_NAMES, STUDY_SEGMENT,
      "Non-response rate (Percentage (0 to 100))")
  )

  expect_equal(
    unname(util_translate_indicator_metrics(
      "PCT_com_qum_nonresp",
      long = FALSE
    )),
    "Non-response rate (%)"
  )

  expect_equal(
    unname(util_translate_indicator_metrics(
      "PCT_com_qum_nonresp",
      short = TRUE,
      long = FALSE
    )),
    "Non-resp. rate (%)"
  )
})

test_that("util_translate_indicator_metrics handles unknown metrics", {
  skip_on_cran()

  expect_true(is.na(util_translate_indicator_metrics("UNKNOWN")))
  expect_equal(
    unname(util_translate_indicator_metrics("UNKNOWN", ignore_unknown = TRUE)),
    "UNKNOWN"
  )
})

test_that("util_translate_indicator_metrics rejects conflicting label modes", {
  skip_on_cran()

  expect_error(
    util_translate_indicator_metrics("PCT_com_qum_nonresp",
      short = TRUE,
      long = TRUE),
    "Cannot create short labels"
  )
})

test_that(
  "util_translate_indicator_metrics falls back for missing public names",
  {
    skip_on_cran()

    testthat::local_mocked_bindings(
      util_get_concept_info = function(sheet) {
        if (identical(sheet, "abbreviationMetrics")) {
          return(data.frame(
            Abbreviation = "PCT",
            Metrics = "Percentage (0 to 100)",
            public_name = "%",
            stringsAsFactors = FALSE
          ))
        }
        if (identical(sheet, "dqi")) {
          return(data.frame(
            abbreviation = "custom_metric",
            Name = "Custom Metric",
            public_name = NA_character_,
            stringsAsFactors = FALSE
          ))
        }
        stop("Unexpected concept sheet")
      }
    )

    expect_equal(
      unname(util_translate_indicator_metrics(
        "PCT_custom_metric",
        short = TRUE,
        long = FALSE
      )),
      "Custom Metric (%)"
    )
  }
)
