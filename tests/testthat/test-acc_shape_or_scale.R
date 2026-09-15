test_that("acc_shape_or_scale works with 3 args", {
  skip_on_cran() # slow
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  meta_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data.RData") # nolint: line_length_linter.
  study_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData", keep_types = TRUE) # nolint: line_length_linter.
  meta_data2 <-
    prep_scalelevel_from_data_and_metadata(
      study_data = study_data,
      meta_data = meta_data
    )
  meta_data[[SCALE_LEVEL]] <-
    setNames(meta_data2[[SCALE_LEVEL]], nm = meta_data2[[VAR_NAMES]])[
      meta_data[[VAR_NAMES]]
    ]


  expect_message2(
    res1 <-
      acc_shape_or_scale(
        resp_vars = "v00014",
        study_data = study_data,
        meta_data = meta_data,
        dist_col = DISTRIBUTION,
        guess = c(TRUE, FALSE, TRUE)
      ),
    regexp = sprintf(
      "(%s|%s)",
      paste(
        "guess should be a scalar logical value.",
        "Have more than one value, use the first one only"
      ),
      paste("Due to missing values in v00014 301 observations were deleted.")
    ),
    perl = TRUE
  )

  expect_message2(
    res1 <-
      acc_shape_or_scale(
        resp_vars = "v00014",
        study_data = study_data,
        meta_data = meta_data,
        dist_col = DISTRIBUTION,
        end_digits = c(TRUE, FALSE, TRUE)
      ),
    regexp = sprintf(
      "(%s|%s)",
      paste(
        "end_digits should be a scalar logical value.",
        "Have more than one value, use the first one only"
      ),
      paste("Due to missing values in v00014 301 observations were deleted.")
    ),
    perl = TRUE
  )

  expect_error(
    res1 <-
      acc_shape_or_scale(
        resp_vars = "v00014",
        study_data = study_data,
        meta_data = meta_data,
        dist_col = DISTRIBUTION,
        par1 = "xxx"
      ),
    regexp = "par1 should be a numeric value"
  )

  expect_error(
    res1 <-
      acc_shape_or_scale(
        resp_vars = "v00014",
        study_data = study_data,
        meta_data = meta_data,
        dist_col = DISTRIBUTION,
        par1 = 0,
        par2 = "xxx",
        guess = FALSE
      ),
    regexp = "par2 should be a numeric value"
  )

  md1 <- meta_data
  md1[md1$VAR_NAMES == "v00014", DISTRIBUTION] <- NA
  suppressMessages(suppressWarnings(expect_error(
    res1 <-
      acc_shape_or_scale(
        resp_vars = "v00014",
        study_data = study_data,
        meta_data = md1,
        dist_col = DISTRIBUTION,
        par1 = 0,
        par2 = 1,
        guess = FALSE
      ),
    regexp = "No distribution specified for v00014 in DISTRIBUTION"
  )))

  md1 <- meta_data
  md1[md1$VAR_NAMES == "v00014", DISTRIBUTION] <- "dirichlet"
  suppressWarnings(expect_error(
    res1 <-
      acc_shape_or_scale(
        resp_vars = "v00014",
        study_data = study_data,
        meta_data = md1,
        dist_col = DISTRIBUTION,
        par1 = 0,
        par2 = 1,
        guess = FALSE
      ),
    regexp = "This distribution .+dirichlet.+ is not supported yet..."
  ))

  expect_message2(
    res1 <-
      acc_shape_or_scale(
        resp_vars = "v00006", # uniform and integer
        study_data = study_data,
        meta_data = meta_data,
        dist_col = DISTRIBUTION,
        par1 = 1,
        par2 = 9,
        guess = FALSE
      ),
    regexp = "Due to missing values in v00006 382 observations were deleted."
  )
  expect_equal(sum(1 == res1$ResultData$GRADING), 8)
  expect_error(
    suppressWarnings(
      res1 <-
        acc_shape_or_scale(
          resp_vars = "v00014",
          study_data = study_data,
          meta_data = meta_data,
          dist_col = DISTRIBUTION,
          par1 = Inf,
          par2 = 1,
          guess = FALSE
        )
    ),
    regexp = paste(
      "Since .+guess.+ is not true finite numerical",
      "parameters must be prespecified"
    ),
    perl = TRUE
  )

  expect_error(
    res1 <-
      acc_shape_or_scale(
        resp_vars = "v00001",
        study_data = study_data,
        meta_data = meta_data,
        dist_col = DISTRIBUTION,
        par1 = 0,
        par2 = 1,
        guess = FALSE
      ),
    regexp = paste(
      "Argument .resp_vars.+ Variable .v00001.+string. does",
      "not have an allowed type"
    ),
    perl = TRUE
  )
  expect_message2(
    res1 <-
      acc_shape_or_scale(
        resp_vars = "v00014",
        study_data = study_data,
        meta_data = meta_data,
        dist_col = DISTRIBUTION,
        par1 = 0:10,
        par2 = 1:11
      ),
    regexp = sprintf(
      "(%s|%s|%s|%s)",
      paste("Since parameters were specified: .+guess.+ is set to false"),
      paste(
        "par1 should be a scalar numeric value. Have more than one value,",
        "use the first one only"
      ),
      paste(
        "par2 should be a scalar numeric value. Have more than one value,",
        "use the first one only"
      ),
      paste("Due to missing values in v00014 301 observations were deleted.")
    ),
    perl = TRUE
  )
  expect_message2(
    res1 <-
      acc_shape_or_scale(
        resp_vars = "v00014",
        study_data = study_data,
        meta_data = meta_data,
        dist_col = DISTRIBUTION,
        par1 = 0,
        par2 = 1
      ),
    regexp = sprintf(
      "(%s|%s)",
      paste("Since parameters were specified: .+guess.+ is set to false"),
      paste("Due to missing values in v00014 301 observations were deleted.")
    ),
    perl = TRUE
  )
  expect_message2(
    res1 <-
      acc_shape_or_scale(
        resp_vars = "v00014",
        study_data = study_data,
        meta_data = meta_data,
        dist_col = DISTRIBUTION,
        guess = c(NA, TRUE)
      ),
    regexp = sprintf(
      "(%s|%s)",
      paste(
        "Have more than one value for guess,",
        "use the first one only"
      ),
      paste(
        "Due to missing values in v00014 301",
        "observations were deleted."
      )
    ),
    perl = TRUE
  )
  md1 <- meta_data[, setdiff(colnames(meta_data), DISTRIBUTION)]
  expect_error(
    res1 <-
      acc_shape_or_scale(
        resp_vars = "v00014",
        study_data = study_data,
        meta_data = md1,
        dist_col = DISTRIBUTION
      ),
    regexp = "Did not find variable attribute DISTRIBUTION in the meta_data"
  )
  expect_warning(
    expect_error(
      res1 <-
        acc_shape_or_scale(study_data = study_data, meta_data = meta_data),
      regexp =
        "Argument resp_vars is NULL",
      perl = TRUE
    ),
    regexp =
      sprintf(
        "(%s)",
        paste(
          "Missing argument .+resp_vars.+ without default value.",
          "Setting to NULL. As a dataquieR developer,"
        )
      ),
    perl = TRUE
  )

  expect_message2(
    res1 <-
      acc_shape_or_scale(
        resp_vars = "v00014",
        study_data = study_data, meta_data = meta_data
      ),
    regexp =
      sprintf(
        "(%s|%s)",
        paste(
          "A column of the metaddata specifying the distributions has",
          "not been specified. Trying the default .+DISTRIBUTION.+."
        ),
        paste("Due to missing values in v00014 301 observations were deleted.")
      ),
    perl = TRUE
  )

  expect_true(all(c(
    "ResultData",
    "SummaryPlot",
    "SummaryTable"
  ) %in% names(res1)))
  expect_lt(
    suppressWarnings(abs(sum(
      as.numeric(
        as.matrix(res1$ResultData)
      ),
      na.rm = TRUE
    ) - 5484.87)), 0.5
  )
  expect_equal(
    suppressWarnings(abs(sum(
      as.numeric(
        as.matrix(res1$SummaryTable)
      ),
      na.rm = TRUE
    ))), 1
  )
})

test_that("distribution parameter fitting matches previous MASS estimates", {
  skip_on_cran()
  skip_if_not_installed("MASS")

  set.seed(458)
  x_norm <- stats::rnorm(500, mean = 3, sd = 2)
  x_gamma <- stats::rgamma(500, shape = 3.5, rate = 1.2)

  expect_equal(
    util_fit_distribution_params(x_norm, "normal"),
    MASS::fitdistr(x_norm, "normal")$estimate,
    tolerance = 1e-12
  )
  expect_equal(
    util_fit_distribution_params(x_gamma, "gamma"),
    MASS::fitdistr(x_gamma, "gamma")$estimate,
    tolerance = 1e-12
  )
})

test_that("distribution parameter fitting validates local inputs", {
  skip_on_cran()

  expect_error(
    util_fit_distribution_params(character(0), "normal"),
    "non-empty numeric"
  )
  expect_error(
    util_fit_distribution_params(c(1, Inf), "normal"),
    "missing or infinite"
  )
  expect_error(
    util_fit_distribution_params(c(-1, 1, 2), "gamma"),
    "gamma values must be >= 0"
  )
  expect_error(
    util_fit_distribution_params(c(1, 1, 1), "gamma"),
    "positive variance"
  )
  expect_error(
    util_fit_distribution_params(c(1, 2, 3), "weibull"),
    "not supported"
  )
})

test_that("acc_shape_or_scale works with label_col", {
  skip_on_cran() # slow
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  meta_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data.RData") # nolint: line_length_linter.
  study_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData", keep_types = TRUE) # nolint: line_length_linter.
  meta_data2 <-
    prep_scalelevel_from_data_and_metadata(
      study_data = study_data,
      meta_data = meta_data
    )
  meta_data[[SCALE_LEVEL]] <-
    setNames(meta_data2[[SCALE_LEVEL]], nm = meta_data2[[VAR_NAMES]])[
      meta_data[[VAR_NAMES]]
    ]

  expect_warning(
    expect_error(
      res1 <-
        acc_shape_or_scale(
          study_data = study_data, meta_data = meta_data,
          label_col = LABEL
        ),
      regexp =
        "Argument resp_vars is NULL",
      perl = TRUE
    ),
    regexp =
      sprintf(
        "(%s)",
        paste(
          "Missing argument .+resp_vars.+ without default value.",
          "Setting to NULL. As a dataquieR developer,"
        )
      ),
    perl = TRUE
  )

  expect_message2(
    res1 <-
      acc_shape_or_scale(
        resp_vars = "CRP_0",
        study_data = study_data, meta_data = meta_data,
        label_col = LABEL
      ),
    regexp =
      sprintf(
        "(%s|%s)",
        paste(
          "A column of the metaddata specifying the distributions has",
          "not been specified. Trying the default .+DISTRIBUTION.+."
        ),
        paste("Due to missing values in CRP_0 301 observations were deleted.")
      ),
    perl = TRUE
  )

  expect_true(all(c(
    "ResultData",
    "SummaryPlot",
    "SummaryTable"
  ) %in% names(res1)))
  expect_lt(
    suppressWarnings(abs(sum(
      as.numeric(
        as.matrix(res1$ResultData)
      ),
      na.rm = TRUE
    ) - 5484.87)), 0.5
  )
  expect_equal(
    suppressWarnings(abs(sum(
      as.numeric(
        as.matrix(res1$SummaryTable)
      ),
      na.rm = TRUE
    ))), 1
  )
  skip_on_cran()
  skip_if_not_installed("vdiffr")
  expect_doppelganger2(
    "shape_or_scale plot for CRP_0 ok",
    res1$SummaryPlot
  )
})

test_that("acc_shape_or_scale reports local argument guardrails", {
  skip_on_cran()

  study_data <- data.frame(
    x = c(1, 2, 3, 4),
    y = c("a", "b", "c", "d"),
    stringsAsFactors = FALSE
  )
  meta_data <- data.frame(
    VAR_NAMES = c("x", "y"),
    LABEL = c("x", "y"),
    DATA_TYPE = c(DATA_TYPES$INTEGER, DATA_TYPES$STRING),
    SCALE_LEVEL = c(SCALE_LEVELS$RATIO, SCALE_LEVELS$NOMINAL),
    DISTRIBUTION = c("uniform", "uniform"),
    MISSING_LIST = SPLIT_CHAR,
    JUMP_LIST = SPLIT_CHAR,
    stringsAsFactors = FALSE
  )

  expect_error(
    withr::with_options(
      list(dataquieR.acc_shape_or_scale_ci = "unsupported"),
      suppressMessages(acc_shape_or_scale(
        resp_vars = "x",
        study_data = study_data,
        meta_data = meta_data,
        label_col = VAR_NAMES,
        guess = FALSE,
        par1 = 1,
        par2 = 4
      ))
    ),
    "should be either"
  )

  expect_error(
    suppressMessages(acc_shape_or_scale(
      resp_vars = "x",
      study_data = study_data,
      meta_data = meta_data,
      label_col = VAR_NAMES,
      guess = "maybe"
    )),
    "guess should be a logical value"
  )

  expect_error(
    suppressMessages(acc_shape_or_scale(
      resp_vars = "x",
      study_data = study_data,
      meta_data = meta_data,
      label_col = VAR_NAMES,
      end_digits = "maybe"
    )),
    "end_digits should be a logical value"
  )

  expect_error(
    suppressMessages(acc_shape_or_scale(
      resp_vars = "y",
      study_data = study_data,
      meta_data = meta_data,
      label_col = VAR_NAMES
    )),
    "allowed type"
  )
})
