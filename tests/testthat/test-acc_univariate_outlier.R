test_that("acc_univariate_outlier normalizes local arguments", {
  skip_on_cran()

  study_data <- data.frame(
    score = as.numeric(c(1:20, 100)) + 0.25,
    count = seq_len(21),
    stringsAsFactors = FALSE
  )
  meta_data <- data.frame(
    VAR_NAMES = c("score", "count"),
    LABEL = c("score", "count"),
    DATA_TYPE = c(DATA_TYPES$FLOAT, DATA_TYPES$INTEGER),
    SCALE_LEVEL = c(SCALE_LEVELS$RATIO, SCALE_LEVELS$RATIO),
    MISSING_LIST = c("", ""),
    JUMP_LIST = c("", ""),
    HARD_LIMITS = c("", ""),
    stringsAsFactors = FALSE
  )

  result <- suppressWarnings(suppressMessages(acc_univariate_outlier(
    resp_vars = "score",
    study_data = study_data,
    meta_data = meta_data,
    label_col = VAR_NAMES,
    criteria = "sixsigma",
    n_rules = 2,
    max_non_outliers_plot = 0
  )))
  invalid_plot_limit <- suppressWarnings(suppressMessages(
    acc_univariate_outlier(
      resp_vars = "score",
      study_data = study_data,
      meta_data = meta_data,
      label_col = VAR_NAMES,
      criteria = "tukey",
      max_non_outliers_plot = "invalid"
    )
  ))

  expect_named(result, c("SummaryTable", "SummaryData", "SummaryPlotList"))
  expect_equal(as.character(result$SummaryTable$Variables), "score")
  expect_true("3SD (N)" %in% colnames(result$SummaryTable))
  expect_named(result$SummaryPlotList, "score")
  expect_equal(
    as.character(invalid_plot_limit$SummaryTable$Variables),
    "score"
  )
})

test_that("acc_univariate_outlier falls back and excludes valid roles", {
  skip_on_cran()

  study_data <- data.frame(
    score = as.numeric(c(1:20, 100)) + 0.25,
    aux = as.numeric(21:41) + 0.25,
    stringsAsFactors = FALSE
  )
  meta_data <- data.frame(
    VAR_NAMES = c("score", "aux"),
    LABEL = c("score", "aux"),
    DATA_TYPE = DATA_TYPES$FLOAT,
    SCALE_LEVEL = SCALE_LEVELS$RATIO,
    VARIABLE_ROLE = c("primary", "secondary"),
    MISSING_LIST = c("", ""),
    JUMP_LIST = c("", ""),
    HARD_LIMITS = c("", ""),
    stringsAsFactors = FALSE
  )

  result <- suppressWarnings(suppressMessages(acc_univariate_outlier(
    resp_vars = c("score", "aux"),
    study_data = study_data,
    meta_data = meta_data,
    label_col = VAR_NAMES,
    criteria = "unknown",
    exclude_roles = "secondary",
    max_non_outliers_plot = 0
  )))

  expect_equal(as.character(result$SummaryTable$Variables), "score")
  expect_named(result$SummaryPlotList, "score")
  expect_false("aux" %in% as.character(result$SummaryData$Variables))
})

test_that("acc_univariate_outlier uses compact pipeline plots", {
  skip_on_cran()

  study_data <- data.frame(
    score = as.numeric(c(1:20, 100)) + 0.25,
    stringsAsFactors = FALSE
  )
  meta_data <- data.frame(
    VAR_NAMES = "score",
    LABEL = "Score label",
    DATA_TYPE = DATA_TYPES$FLOAT,
    SCALE_LEVEL = SCALE_LEVELS$RATIO,
    MISSING_LIST = "",
    JUMP_LIST = "",
    HARD_LIMITS = "",
    stringsAsFactors = FALSE
  )

  result <- suppressWarnings(suppressMessages(with_pipeline(
    acc_univariate_outlier(
      resp_vars = "score",
      study_data = study_data,
      meta_data = meta_data,
      label_col = VAR_NAMES,
      criteria = "tukey",
      max_non_outliers_plot = 0
    )
  )))

  plot <- result$SummaryPlotList$score
  expect_s3_class(plot, "ggplot")
  expect_length(plot$facet$params$facets, 0L)
})

test_that("acc_univariate_outlier guesses missing local data types", {
  skip_on_cran()

  study_data <- data.frame(
    score = as.numeric(c(1:20, 100)) + 0.25,
    ignored = letters[seq_len(21)],
    stringsAsFactors = FALSE
  )
  meta_data <- data.frame(
    VAR_NAMES = c("score", "ignored"),
    LABEL = c("score", "ignored"),
    DATA_TYPE = c(NA, DATA_TYPES$STRING),
    SCALE_LEVEL = c(SCALE_LEVELS$RATIO, SCALE_LEVELS$NOMINAL),
    MISSING_LIST = c("", ""),
    JUMP_LIST = c("", ""),
    HARD_LIMITS = c("", ""),
    stringsAsFactors = FALSE
  )

  result <- suppressWarnings(suppressMessages(acc_univariate_outlier(
    study_data = study_data,
    meta_data = meta_data,
    label_col = VAR_NAMES,
    criteria = "tukey",
    max_non_outliers_plot = 0
  )))

  expect_named(result, c("SummaryTable", "SummaryData", "SummaryPlotList"))
  expect_equal(as.character(result$SummaryTable$Variables), "score")
  expect_named(result$SummaryPlotList, "score")
})

test_that("acc_univariate_outlier filters local nonnumeric variables", {
  skip_on_cran()

  study_data <- data.frame(
    score = as.numeric(c(1:20, 100)) + 0.25,
    coded = as.character(c(1:20, 100)),
    text = rep(c("a", "b"), length.out = 21),
    stringsAsFactors = FALSE
  )
  meta_data <- data.frame(
    VAR_NAMES = c("score", "coded", "text"),
    LABEL = c("score", "coded", "text"),
    DATA_TYPE = c(
      DATA_TYPES$FLOAT,
      DATA_TYPES$FLOAT,
      DATA_TYPES$STRING
    ),
    SCALE_LEVEL = c(
      SCALE_LEVELS$RATIO,
      SCALE_LEVELS$RATIO,
      SCALE_LEVELS$NOMINAL
    ),
    MISSING_LIST = c("", "", ""),
    JUMP_LIST = c("", "", ""),
    HARD_LIMITS = c("", "", ""),
    stringsAsFactors = FALSE
  )

  result <- suppressWarnings(suppressMessages(acc_univariate_outlier(
    resp_vars = c("score", "coded", "text"),
    study_data = study_data,
    meta_data = meta_data,
    label_col = VAR_NAMES,
    criteria = "tukey",
    max_non_outliers_plot = 0
  )))

  expect_equal(as.character(result$SummaryTable$Variables), c("score", "coded"))
  expect_named(result$SummaryPlotList, c("score", "coded"))

  no_numeric_data <- data.frame(
    coded = rep("x", 21),
    stringsAsFactors = FALSE
  )
  no_numeric <- meta_data[meta_data[[VAR_NAMES]] == "coded", , drop = FALSE]

  expect_error(
    suppressWarnings(suppressMessages(acc_univariate_outlier(
      resp_vars = c("coded", "text"),
      study_data = no_numeric_data,
      meta_data = no_numeric,
      label_col = VAR_NAMES,
      criteria = "tukey",
      max_non_outliers_plot = 0
    ))),
    "No data left, aborting"
  )
})

test_that("acc_univariate_outlier reports unsuitable local metadata", {
  skip_on_cran()

  study_data <- data.frame(
    text = rep(c("a", "b"), length.out = 21),
    stringsAsFactors = FALSE
  )
  meta_data <- data.frame(
    VAR_NAMES = "text",
    LABEL = "text",
    DATA_TYPE = DATA_TYPES$STRING,
    SCALE_LEVEL = SCALE_LEVELS$NOMINAL,
    VARIABLE_ROLE = VARIABLE_ROLES$PRIMARY,
    MISSING_LIST = "",
    JUMP_LIST = "",
    HARD_LIMITS = "",
    stringsAsFactors = FALSE
  )

  expect_error(
    suppressWarnings(suppressMessages(acc_univariate_outlier(
      study_data = study_data,
      meta_data = meta_data,
      label_col = VAR_NAMES
    ))),
    "No variables with suitable data type defined"
  )

  numeric_data <- data.frame(score = as.numeric(c(1:20, 100)) + 0.25)
  numeric_meta <- data.frame(
    VAR_NAMES = "score",
    LABEL = "score",
    DATA_TYPE = DATA_TYPES$FLOAT,
    SCALE_LEVEL = SCALE_LEVELS$RATIO,
    VARIABLE_ROLE = VARIABLE_ROLES$PRIMARY,
    MISSING_LIST = "",
    JUMP_LIST = "",
    HARD_LIMITS = "",
    stringsAsFactors = FALSE
  )

  result <- suppressWarnings(suppressMessages(acc_univariate_outlier(
    resp_vars = "score",
    study_data = numeric_data,
    meta_data = numeric_meta,
    label_col = VAR_NAMES,
    exclude_roles = "unknown_role",
    max_non_outliers_plot = 0
  )))

  expect_equal(as.character(result$SummaryTable$Variables), "score")
  expect_named(result$SummaryPlotList, "score")
})

test_that("acc_univariate_outlier works without label_col", {
  skip_on_cran() # slow, errors obvious
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

  sd1 <- study_data # no outliers at all
  set.seed(234325)
  sd1$v00014 <- rnorm(nrow(sd1), sd = 0.01)
  sd1 <- sd1[sd1$v00014 < 0.02 & sd1$v00014 > -0.02, ]
  expect_message2(
    no_ol_det <- acc_univariate_outlier(
      resp_vars = "v00014", study_data = sd1,
      meta_data = meta_data, max_non_outliers_plot = 0
    ),
    regexp =
      paste(
        "For .+v00014.+, 0 from 1466 non-outlier data values",
        "were sampled to avoid large plots."
      )
  )

  expect_match(
    ggplot2::ggplot_build(no_ol_det$SummaryPlotList$v00014)$data[[1]]$label,
    "No outliers detected for .+v00014.+",
    perl = TRUE
  )

  expect_message2(
    invisible(
      acc_univariate_outlier(
        resp_vars = "v00014", study_data = study_data,
        meta_data = meta_data, n_rules = cars
      )
    ),
    regexp =
      "The formal n_rules is not an integer between 1 and 4, default .4. is used." # nolint: line_length_linter.
  )

  expect_message2(
    invisible(
      acc_univariate_outlier(
        resp_vars = "v00014", study_data = study_data,
        meta_data = meta_data,
        max_non_outliers_plot = 100
      )
    ),
    regexp =
      paste(
        "For .+v00014.+, 100 from 2633 non-outlier data values",
        "were sampled to avoid large plots."
      )
  )

  expect_message2(
    invisible(
      acc_univariate_outlier(
        resp_vars = "v00014", study_data = study_data,
        meta_data = meta_data,
        max_non_outliers_plot = -42
      )
    ),
    regexp =
      paste(
        "The formal max_non_outliers_plot is not an",
        "integer >= 0, default .10000. is used."
      )
  )

  sd1 <- study_data[, c("v00103", "v00001", "v00013")]
  md1 <- subset(meta_data, VAR_NAMES %in% colnames(sd1))
  md1$JUMP_LIST <- "|"
  md1$MISSING_LIST <- "|"

  expect_message2(
    expect_error(
      invisible(
        acc_univariate_outlier(
          study_data = sd1,
          meta_data = md1
        )
      ),
      regexp =
        paste("No variables with suitable data type defined.")
    ),
    regexp = "The following variables: were selected.",
    all = !TRUE,
    fixed = TRUE
  )

  md0 <- meta_data
  md0$DATA_TYPE <- NULL
  expect_error(
    invisible(
      acc_univariate_outlier(
        study_data = study_data,
        meta_data = md0, n_rules = 4
      )
    ),
    regexp = "Missing columns .+DATA_TYPE.+ from .+meta_data.+"
  )

  md0 <- meta_data
  md0$DATA_TYPE[[1]] <- NA
  expect_message2(
    expect_warning(
      invisible(
        acc_univariate_outlier(
          study_data = study_data,
          meta_data = md0, n_rules = 4
        )
      ),
      regexp = sprintf(
        "(%s)",
        paste(
          "For the variables.+?v00000.+?I have no valid.+?DATA_TYPE.+?in the",
          ".+?meta_data.+?I.+?ve predicted the.+?DATA_TYPE.+?from",
          "the.+?study_data.+?yielding.+?v00000 = integer.+?"
        )
      ),
      perl = TRUE
    ),
    perl = TRUE,
    regexp = sprintf(
      "(?ms).*(%s|%s|%s).*", paste(
        "The following variables: v00000, v00002, v00003,",
        "v01003, v01002, v10000, v00004, v00005, v00006, v00007, v00009, v00109,", # nolint: line_length_linter.
        "v00010, v20000, v00014, v00015, v00016, v30000, v00018, v01018, v00019,", # nolint: line_length_linter.
        "v00020, v00021, v00022, v00023, v00024, v00025, v00026, v00027, v00028,", # nolint: line_length_linter.
        "v00029, v00030, v00031, v40000, v00034, v00035, v00036, v00037, v00038,", # nolint: line_length_linter.
        "v00039, v00040, v00041, v50000.*"
      ),
      paste("Variables:.+?v00000.+?,.+?v00002.+?,.+?v00003.+?,.+?v01003.+?,.+?v01002.+?,.+?v10000.+?,.+?v00004.+?,.+?v00005.+?,.+?v00007.+?,.+?v00009.+?,.+?v00109.+?,.+?v00010.+?,.+?v20000.+?,.+?v00015.+?,.+?v00016.+?,.+?v30000.+?,.+?v00018.+?,.+?v01018.+?,.+?v00019.+?,.+?v00020.+?,.+?v00021.+?,.+?v00022.+?,.+?v00023.+?,.+?v00024.+?,.+?v00025.+?,.+?v00026.+?,.+?v00027.+?,.+?v00028.+?,.+?v00029.+?,.+?v00030.+?,.+?v00031.+?,.+?v40000.+?,.+?v00034.+?,.+?v00035.+?,.+?v00036.+?,.+?v00037.+?,.+?v00038.+?,.+?v00039.+?,.+?v00040.+?,.+?v00041.+?,.+?v50000.+?show integer values only, but will be nonetheless considered."), # nolint: line_length_linter.
      paste("The variables.+?v00000.+?,.+?v00002.+?,.+?v01002.+?,.+?v10000.+?,.+?v00007.+?,.+?v00109.+?,.+?v00010.+?,.+?v20000.+?,.+?v30000.+?,.+?v00018.+?,.+?v01018.+?,.+?v00019.+?,.+?v00020.+?,.+?v00022.+?,.+?v00023.+?,.+?v00024.+?,.+?v00025.+?,.+?v00028.+?,.+?v00029.+?,.+?v00030.+?,.+?v40000.+?,.+?v50000.+? are neither float nor integer without VALUE_LABELS. Ignoring those.*") # nolint: line_length_linter.
    )
  )

  res1 <-
    acc_univariate_outlier(
      resp_vars = "v00014", study_data = study_data,
      meta_data = meta_data
    )

  expect_true(all(
    c(
      "SummaryTable",
      "SummaryPlotList"
    ) %in% names(res1)
  ))

  expect_lt(
    suppressWarnings(abs(sum(
      as.numeric(
        as.matrix(res1$SummaryTable)
      ),
      na.rm = TRUE
    ) - 2699 - 112.45)), 0.1
  )

  expect_equal(
    suppressWarnings(abs(sum(
      as.numeric(
        as.matrix(res1$SummaryPlotList$v00014$data)
      ),
      na.rm = TRUE
    ) - 8005.172)), 0
  )

  expect_identical(
    colnames(res1$SummaryTable),
    c(
      "Variables", "Mean", "No.records", "SD", "Median", "Skewness",
      "Tukey (N)", "3SD (N)", "Hubert (N)",
      "Sigma-gap (N)", "NUM_acc_ud_outlu", "Outliers, low (N)",
      "Outliers, high (N)", "GRADING", "PCT_acc_ud_outlu"
    )
  )
})

test_that("acc_univariate_outlier works with label_col", {
  skip_on_cran() # slow, errors obvious
  skip_if_translated()
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

  md0 <- meta_data
  md0$DATA_TYPE <- NA
  {
    res1 <-
      acc_univariate_outlier(
        resp_vars = c("USR_SOCDEM_0", "CRP_0"),
        study_data = study_data,
        meta_data = md0, label_col = LABEL
      )
  } %>% # nolint: brace_linter
    expect_message2("Argument.+resp_vars.+Variable.+USR_SOCDEM_0.+string.+does not have an allowed type.+integer.+float.+", perl = TRUE) %>% # nolint: line_length_linter.
    expect_message2("Argument.+resp_vars.+Variable.+USR_SOCDEM_0.+nominal.+does not have an allowed scale level.+interval.+ratio.+", perl = TRUE) %>% # nolint: line_length_linter.
    expect_warning("I've predicted the.+DATA_TYPE.+from the.+study_data", perl = TRUE) %>% # nolint: line_length_linter.
    expect_warning("resp_vars.+USR_SOCDEM_0.+were excluded", perl = TRUE) %>%
    expect_warning("resp_vars.+USR_SOCDEM_0.+were excluded", perl = TRUE)

  {
    res1 <-
      acc_univariate_outlier(
        resp_vars = c("USR_SOCDEM_0", "CRP_0"),
        study_data = study_data,
        meta_data = md0, label_col = LABEL,
        exclude_roles = "XXX"
      )
  } %>% # nolint: brace_linter
    expect_message2("Argument.+resp_vars.+Variable.+USR_SOCDEM_0.+string.+does not have an allowed type.+integer.+float.+", perl = TRUE) %>% # nolint: line_length_linter.
    expect_message2("Argument.+resp_vars.+Variable.+USR_SOCDEM_0.+nominal.+does not have an allowed scale level.+interval.+ratio.+", perl = TRUE) %>% # nolint: line_length_linter.
    expect_warning("Specified VARIABLE_ROLE not in meta_data. No exclusion applied.") %>% # nolint: line_length_linter.
    expect_warning(".*predicted.+DATA_TYPE.*", perl = TRUE) %>%
    expect_warning("resp_vars.+USR_SOCDEM_0.+were excluded", perl = TRUE) %>%
    expect_warning("resp_vars.+USR_SOCDEM_0.+were excluded", perl = TRUE)


  {
    res1 <-
      acc_univariate_outlier(
        resp_vars = c(
          "USR_SOCDEM_0", "CRP_0",
          "CENTER_0", "SEX_0", "AGE_0"
        ),
        study_data = study_data,
        meta_data = md0,
        label_col = LABEL,
        exclude_roles = c("intro", "process")
      )
  } %>% # nolint: brace_linter
    expect_warning(
      regexp =
        paste(
          "variables.+SEX_0.+USR_SOCDEM_0.+CENTER_0.+were",
          "excluded."
        ), perl = TRUE
    ) %>%
    expect_warning(paste("variables.+USR_SOCDEM_0.+were excluded."),
      perl = TRUE
    ) %>%
    expect_warning(
      paste(
        "For the variables",
        ".*predicted.+DATA_TYPE.*from the .study_data."
      ),
      perl = TRUE
    ) %>%
    expect_message2(paste(
      "Argument.+resp_vars.+Variable.+USR_SOCDEM_0.+string.+does",
      "not have an allowed type.+integer.+float.+"
    ), perl = TRUE) %>%
    expect_message2(paste(
      "Argument.+resp_vars.+Variable.+USR_SOCDEM_0.+nominal.+does not have an allowed scale level.+interval.+ratio.+" # nolint: line_length_linter.
    ), perl = TRUE) %>%
    expect_message2(paste(
      "Argument.+resp_vars.+Variable.+SEX_0.+nominal.+does not have an allowed scale level.+interval.+ratio.+" # nolint: line_length_linter.
    ), perl = TRUE) %>%
    expect_message2(paste("Argument.+resp_vars.+Variable.+CENTER_0.+nominal.+does not have an allowed scale level.+interval.+ratio.+"), perl = TRUE) %>% # nolint: line_length_linter.
    expect_message2(paste(
      "Study variables.+AGE_0.+have",
      "been excluded."
    ), perl = TRUE)

  sd0 <- study_data
  sd0$v00014 <- NA_integer_
  expect_message2(
    #    expect_warning(
    expect_error(
      res1 <-
        acc_univariate_outlier(
          resp_vars = c("CRP_0", "AGE_0"),
          study_data = sd0,
          meta_data = meta_data, label_col = LABEL,
          exclude_roles = c("intro", "process")
        ),
      regexp = "No data left, aborting",
      perl = TRUE
    ),
    # A previous expectation checked that CRP_0 was excluded.
    regexp = sprintf(
      "(%s|%s|%s)",
      paste(
        "Argument.+resp_vars.+Variable.+CRP_0.+nominal.+does not have an allowed scale level.+interval.+ratio.+" # nolint: line_length_linter.
      ),
      paste(
        "Study variables.+AGE_0.+have",
        "been excluded."
      ),
      paste(
        "Variables: .+CRP_0.+show integer values only,",
        "but will be nonetheless considered."
      )
    ),
    perl = TRUE
  )


  sd0 <- study_data
  sd0$v00014 <- NA
  expect_message2( # no data left v14(crp0) wird rausgworfen, weil in diesem test NA gesetzt. komsich: je nach option ist sie dann noch in den resp_vars oder nicht. # nolint: line_length_linter.
    expect_error(
      res1 <-
        acc_univariate_outlier(
          resp_vars = c("CRP_0", "AGE_0"),
          study_data = sd0,
          meta_data = meta_data, label_col = LABEL,
          exclude_roles = c("intro", "process")
        ),
      regexp = "No data left, aborting.",
      perl = TRUE
    ),
    regexp = sprintf(
      "(%s|%s)",
      paste(
        "Variables .+CRP_0.+ are not of type float or",
        "integer and will be removed from univariate",
        "outlier analysis."
      ),
      paste(
        "Study variables:",
        ".+AGE_0.+ have been excluded."
      )
    ),
    perl = TRUE
  )

  res1 <-
    acc_univariate_outlier(
      resp_vars = "CRP_0", study_data = study_data,
      meta_data = meta_data, label_col = LABEL,
      n_rules = 1
    )
  expect_identical(res1$SummaryTable$GRADING, 1)

  res1 <-
    acc_univariate_outlier(
      resp_vars = "CRP_0", study_data = study_data,
      meta_data = meta_data, label_col = LABEL
    )

  expect_true(all(
    c(
      "SummaryTable",
      "SummaryPlotList"
    ) %in% names(res1)
  ))

  expect_lt(
    suppressWarnings(abs(sum(
      as.numeric(
        as.matrix(res1$SummaryTable)
      ),
      na.rm = TRUE
    ) - 2699 - 112.45)), 0.1
  )

  expect_equal(
    suppressWarnings(abs(sum(
      as.numeric(
        as.matrix(res1$SummaryPlotList$CRP_0$data)
      ),
      na.rm = TRUE
    ) - 8005.172)), 0
  )

  expect_identical(
    colnames(res1$SummaryTable),
    c(
      "Variables", "Mean", "No.records", "SD", "Median", "Skewness",
      "Tukey (N)", "3SD (N)", "Hubert (N)",
      "Sigma-gap (N)", "NUM_acc_ud_outlu", "Outliers, low (N)",
      "Outliers, high (N)", "GRADING", "PCT_acc_ud_outlu"
    )
  )
})
