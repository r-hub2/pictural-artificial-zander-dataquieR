test_that("hard-limit replacement requires prepared mapped data", {
  skip_on_cran()

  study_data <- data.frame(x = 1)
  meta_data <- data.frame(LABEL = "x", stringsAsFactors = FALSE)

  expect_error(
    util_replace_hard_limit_violations(
      study_data, meta_data, label_col = "LABEL"
    ),
    "Missing codes have to have been replaced"
  )

  attr(study_data, "Codes_to_NA") <- TRUE
  expect_error(
    util_replace_hard_limit_violations(
      study_data, meta_data, label_col = "LABEL"
    ),
    "must have been mapped"
  )
})

test_that(
  "hard-limit replacement handles absent limits and numeric intervals",
  {
    skip_on_cran()

    study_data <- data.frame(x = c(-1, 0, 1, 2))
    attr(study_data, "Codes_to_NA") <- TRUE
    attr(study_data, "MAPPED") <- TRUE

    expect_message(
      no_limits <- util_replace_hard_limit_violations(
        study_data,
        data.frame(LABEL = "x", stringsAsFactors = FALSE),
        label_col = "LABEL"
      ),
      "do not provide"
    )
    expect_identical(no_limits$x, study_data$x)
    expect_true(util_attr(no_limits, "HL_viol_to_NA", exact = TRUE))

    meta_data <- data.frame(
      LABEL = "x",
      HARD_LIMITS = "[0;1]",
      DATA_TYPE = "float",
      stringsAsFactors = FALSE
    )
    replaced <- util_replace_hard_limit_violations(
      study_data,
      meta_data,
      label_col = "LABEL"
    )

    expect_identical(replaced$x, c(NA_real_, 0, 1, NA_real_))
    expect_true(util_attr(replaced, "HL_viol_to_NA", exact = TRUE))
  }
)

test_that("hard-limit replacement warns for interval limits on text columns", {
  skip_on_cran()

  study_data <- data.frame(x = c("low", "high"), stringsAsFactors = FALSE)
  attr(study_data, "Codes_to_NA") <- TRUE
  attr(study_data, "MAPPED") <- TRUE

  meta_data <- data.frame(
    LABEL = "x",
    HARD_LIMITS = "[0;1]",
    stringsAsFactors = FALSE
  )

  expect_warning(
    replaced <- util_replace_hard_limit_violations(
      study_data,
      meta_data,
      label_col = "LABEL"
    ),
    "column is of type"
  )

  expect_identical(replaced$x, study_data$x)
  expect_true(util_attr(replaced, "HL_viol_to_NA", exact = TRUE))
})
