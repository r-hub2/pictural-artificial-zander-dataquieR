test_that("acc_multivariate_outlier normalizes local arguments", {
  skip_on_cran()

  study_data <- data.frame(
    x = as.numeric(c(1:20, 80)) + 0.1,
    y = as.numeric(c(2:21, 90)) + 0.2,
    stringsAsFactors = FALSE
  )
  meta_data <- data.frame(
    VAR_NAMES = c("x", "y"),
    LABEL = c("x", "y"),
    DATA_TYPE = c(DATA_TYPES$FLOAT, DATA_TYPES$FLOAT),
    SCALE_LEVEL = c(SCALE_LEVELS$RATIO, SCALE_LEVELS$RATIO),
    MISSING_LIST = c("", ""),
    JUMP_LIST = c("", ""),
    HARD_LIMITS = c("", ""),
    stringsAsFactors = FALSE
  )

  result <- suppressWarnings(suppressMessages(acc_multivariate_outlier(
    variable_group = c("x", "y"),
    study_data = study_data,
    meta_data = meta_data,
    label_col = VAR_NAMES,
    criteria = "sixsigma",
    n_rules = 2,
    max_non_outliers_plot = 0,
    scale = FALSE
  )))
  invalid_plot_limit <- suppressWarnings(suppressMessages(
    acc_multivariate_outlier(
      variable_group = c("x", "y"),
      study_data = study_data,
      meta_data = meta_data,
      label_col = VAR_NAMES,
      criteria = "tukey",
      max_non_outliers_plot = "invalid",
      scale = TRUE
    )
  ))

  expect_true(all(c(
    "FlaggedStudyData",
    "SummaryTable",
    "SummaryData",
    "SummaryPlot"
  ) %in% names(result)))
  expect_equal(unique(result$SummaryTable$Variables), "x | y")
  expect_true("threeSD" %in% colnames(result$FlaggedStudyData))
  expect_s3_class(result$SummaryPlot, "ggplot")
  expect_equal(unique(invalid_plot_limit$SummaryTable$Variables), "x | y")
})

test_that("acc_multivariate_outlier works with 3 args", {
  skip_on_cran() # slow and tested elsewhere
  skip_if_translated()
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  meta_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data.RData") # nolint: line_length_linter.
  study_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData", keep_types = TRUE) # nolint: line_length_linter.
  meta_data <-
    prep_scalelevel_from_data_and_metadata(
      study_data = study_data,
      meta_data = meta_data
    )
  expect_error(
    res1 <-
      acc_multivariate_outlier(
        study_data = study_data, meta_data = meta_data, scale = FALSE
      ),
    regexp =
      "Argument variable_group is NULL",
    perl = TRUE
  )

  expect_error(
    res1 <-
      acc_multivariate_outlier(
        variable_group = "v00014",
        study_data = study_data, meta_data = meta_data, scale = FALSE
      ),
    regexp =
      "Need at least two variables for multivariate outliers.",
    perl = TRUE
  )

  expect_message2(
    res1 <-
      acc_multivariate_outlier(
        variable_group = c("v00014", "v00006"),
        study_data = study_data, meta_data = meta_data,
        scale = FALSE
      ),
    regexp =
      sprintf(
        "(%s|%s)",
        paste("As no ID-var has been specified the rownumbers will be used."),
        paste(
          "Due to missing values in v00014, v00006 or dq_id N=602",
          "observations were excluded."
        )
      ),
    perl = TRUE
  )

  expect_true(all(c(
    "FlaggedStudyData",
    "SummaryTable",
    "SummaryPlot"
  ) %in% names(res1)))
  expect_lt(
    suppressWarnings(abs(sum(
      as.numeric(
        as.matrix(res1$FlaggedStudyData)
      ),
      na.rm = TRUE
    ) - 4492191)), 0.5
  )
  expect_equal(
    suppressWarnings(abs(sum(
      as.numeric(
        as.matrix(res1$SummaryTable)
      ),
      na.rm = TRUE
    ))), 182 + 3.13
  )
})

test_that("acc_multivariate_outlier works with label_col", {
  skip_on_cran() # slow and tested elsewhere
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  meta_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data.RData") # nolint: line_length_linter.
  study_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData", keep_types = TRUE) # nolint: line_length_linter.
  meta_data <-
    prep_scalelevel_from_data_and_metadata(
      study_data = study_data,
      meta_data = meta_data
    )
  expect_error(
    res1 <-
      acc_multivariate_outlier(
        label_col = LABEL,
        study_data = study_data, meta_data = meta_data, scale = FALSE
      ),
    regexp =
      "Argument variable_group is NULL",
    perl = TRUE
  )

  expect_error(
    res1 <-
      acc_multivariate_outlier(
        label_col = LABEL,
        variable_group = "CRP_0",
        study_data = study_data, meta_data = meta_data, scale = FALSE
      ),
    regexp =
      "Need at least two variables for multivariate outliers.",
    perl = TRUE
  )

  expect_message2(
    res1 <-
      acc_multivariate_outlier(
        variable_group = c("CRP_0", "GLOBAL_HEALTH_VAS_0"),
        label_col = LABEL,
        study_data = study_data, meta_data = meta_data,
        scale = FALSE
      ),
    regexp =
      sprintf(
        "(%s|%s)",
        paste("As no ID-var has been specified the rownumbers will be used."),
        paste(
          "Due to missing values in CRP_0, GLOBAL_HEALTH_VAS_0 or",
          "dq_id N=602 observations were excluded."
        )
      ),
    perl = TRUE
  )

  expect_true(all(c(
    "FlaggedStudyData",
    "SummaryTable",
    "SummaryPlot"
  ) %in% names(res1)))
  expect_lt(
    suppressWarnings(abs(sum(
      as.numeric(
        as.matrix(res1$FlaggedStudyData)
      ),
      na.rm = TRUE
    ) - 4492191)), 0.5
  )
  expect_equal(
    suppressWarnings(abs(sum(
      as.numeric(
        as.matrix(res1$SummaryTable)
      ),
      na.rm = TRUE
    ))), 182 + 3.13
  )

  skip_on_cran()
  skip_if_not_installed("vdiffr")
  expect_doppelganger2(
    "acc_mv_outlierCRP0GLOBHEAVA0",
    res1$SummaryPlot
  )
})

test_that("acc_multivariate_outlier works with min-max-scaling", {
  skip_on_cran() # slow and tested elsewhere
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  meta_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data.RData") # nolint: line_length_linter.
  study_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData", keep_types = TRUE) # nolint: line_length_linter.
  meta_data <-
    prep_scalelevel_from_data_and_metadata(
      study_data = study_data,
      meta_data = meta_data
    )
  expect_message2(
    res1 <-
      acc_multivariate_outlier(
        variable_group = c("CRP_0", "GLOBAL_HEALTH_VAS_0"),
        label_col = LABEL,
        study_data = study_data, meta_data = meta_data,
        scale = TRUE
      ),
    regexp =
      sprintf(
        "(%s|%s)",
        paste("As no ID-var has been specified the rownumbers will be used."),
        paste(
          "Due to missing values in CRP_0, GLOBAL_HEALTH_VAS_0 or",
          "dq_id N=602 observations were excluded."
        )
      ),
    perl = TRUE
  )

  expect_true(all(c(
    "FlaggedStudyData",
    "SummaryTable",
    "SummaryPlot"
  ) %in% names(res1)))

  expect_false(
    inherits(try(ggplot_build(res1$SummaryPlot)), "try-error")
  )
})
