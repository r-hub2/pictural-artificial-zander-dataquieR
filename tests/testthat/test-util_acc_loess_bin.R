test_that("util_acc_loess_bin works", {
  skip_on_cran() # slow

  # Use testthat::local_reproducible_output() when debugging locally.
  withr::local_options(
    dataquieR.CONDITIONS_WITH_STACKTRACE = TRUE,
    dataquieR.ERRORS_WITH_CALLER = TRUE,
    dataquieR.WARNINGS_WITH_CALLER = TRUE,
    dataquieR.MESSAGES_WITH_CALLER = TRUE
  )
  require_english_locale_and_berlin_tz()
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  meta_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data.RData") # nolint: line_length_linter.
  study_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData", keep_types = TRUE) # nolint: line_length_linter.
  meta_data <-
    prep_scalelevel_from_data_and_metadata(
      study_data = study_data,
      meta_data = meta_data
    )

  # numeric response, contrast 'intermediate' and 'low'/'high' values,
  # without co_vars
  sd0 <- study_data
  sd0$v00003[1:10] <- NA
  sd0$v00002[11:20] <- NA
  expect_message2(
    res1 <-
      util_acc_loess_bin(
        resp_vars = "CRP_0", study_data = sd0,
        meta_data = meta_data, group_vars = "DEV_NO_0",
        time_vars = "LAB_DT_0", co_vars = NULL,
        label_col = LABEL
      ),
    regexp = "Due to missing values in DEV_NO_0 or LAB_DT_0, N = 308 observations were excluded. Due to missing values in CRP_0, N = 131 observations were excluded" # nolint: line_length_linter.
  )

  expect_false(
    inherits(try(ggplot_build(res1$SummaryPlotList$CRP_0)), "try-error")
  )

  # numeric response, contrast 'intermediate' and 'low'/'high' values,
  # with co_vars
  expect_message2(
    res2 <-
      util_acc_loess_bin(
        resp_vars = "CRP_0", study_data = sd0,
        meta_data = meta_data, group_vars = "DEV_NO_0",
        time_vars = "LAB_DT_0", co_vars = c("AGE_0", "SEX_0"),
        label_col = LABEL
      ),
    regexp = "Due to missing values in DEV_NO_0, AGE_0, SEX_0 or LAB_DT_0, N = 327 observations were excluded. Due to missing values in CRP_0, N = 130 observations were excluded" # nolint: line_length_linter.
  )

  expect_false(
    inherits(try(ggplot_build(res2$SummaryPlotList$CRP_0)), "try-error")
  )

  # nominal response, recoding to binary should be done automatically
  # no group_var
  expect_message2(
    res3 <-
      util_acc_loess_bin(
        resp_vars = "CENTER_0", study_data = study_data,
        meta_data = meta_data, time_vars = "EXAM_DT_0",
        label_col = LABEL
      ) # plot is not helpful
  )

  expect_false(
    inherits(try(ggplot_build(res3$SummaryPlotList$CENTER_0)), "try-error")
  )

  # binary response, with group_var
  expect_message2(
    res4 <-
      util_acc_loess_bin(
        resp_vars = "ASTHMA_0", group_vars = "CENTER_0",
        time_vars = "EXAM_DT_0",
        study_data = study_data, meta_data = meta_data,
        label_col = LABEL
      )
  )

  expect_false(
    inherits(try(ggplot_build(res4$SummaryPlotList$ASTHMA_0)), "try-error")
  )
})

test_that("util_acc_loess_bin handles local plot selection guardrails", {
  skip_on_cran()

  study_data <- data.frame(
    y = rep(c(0L, 1L), 30L),
    t = as.POSIXct("2020-01-01", tz = "UTC") +
      seq(0, by = 86400, length.out = 60L),
    group = rep(sprintf("g%02d", 1:12), each = 5L),
    stringsAsFactors = FALSE
  )
  meta_data <- data.frame(
    VAR_NAMES = c("y", "t", "group"),
    LABEL = c("y", "t", "group"),
    DATA_TYPE = c(
      DATA_TYPES$INTEGER, DATA_TYPES$DATETIME, DATA_TYPES$STRING
    ),
    SCALE_LEVEL = c(
      SCALE_LEVELS$NOMINAL, SCALE_LEVELS$INTERVAL, SCALE_LEVELS$NOMINAL
    ),
    MISSING_LIST = SPLIT_CHAR,
    JUMP_LIST = SPLIT_CHAR,
    RECODE_CASES = "1",
    RECODE_CONTROL = "0",
    stringsAsFactors = FALSE
  )

  combined <- suppressMessages(util_acc_loess_bin(
    resp_vars = "y",
    group_vars = "group",
    time_vars = "t",
    study_data = study_data,
    meta_data = meta_data,
    label_col = LABEL,
    min_obs_in_subgroup = 2L,
    min_proportion = 0.1,
    n_group_max = 3L,
    resolution = 5L,
    plot_format = "COMBINED"
  ))

  expect_named(combined$SummaryPlotList, "y")
  expect_equal(
    util_attr(combined, "sizing_hints", exact = TRUE)$figure_type_id,
    "dot_loess"
  )
  expect_gte(
    util_attr(combined, "sizing_hints", exact = TRUE)$n_groups,
    2
  )

  facets <- suppressMessages(util_acc_loess_bin(
    resp_vars = "y",
    group_vars = "group",
    time_vars = "t",
    study_data = study_data,
    meta_data = meta_data,
    label_col = LABEL,
    min_obs_in_subgroup = 2L,
    min_proportion = 0.1,
    n_group_max = 3L,
    resolution = 5L,
    plot_format = "FACETS"
  ))

  expect_named(facets$SummaryPlotList, "y")
  expect_null(util_attr(facets, "sizing_hints", exact = TRUE))

  both <- suppressMessages(util_acc_loess_bin(
    resp_vars = "y",
    group_vars = "group",
    time_vars = "t",
    study_data = study_data,
    meta_data = meta_data,
    label_col = LABEL,
    min_obs_in_subgroup = 2L,
    min_proportion = 0.1,
    n_group_max = 3L,
    resolution = 5L,
    plot_format = "BOTH"
  ))

  expect_named(
    both$SummaryPlotList,
    c("Loess_fits_facets", "Loess_fits_combined")
  )

  dummy_group <- suppressMessages(util_acc_loess_bin(
    resp_vars = "y",
    time_vars = "t",
    study_data = study_data,
    meta_data = meta_data,
    label_col = LABEL,
    min_obs_in_subgroup = 2L,
    min_proportion = 0.1,
    resolution = 5L,
    plot_format = c("AUTO", "BOTH")
  ))

  expect_named(dummy_group$SummaryPlotList, "y")
  expect_equal(
    util_attr(dummy_group, "sizing_hints", exact = TRUE)$figure_type_id,
    "dot_loess"
  )
})

test_that("util_acc_loess_bin reports local response guardrails", {
  skip_on_cran()

  study_data <- data.frame(
    y = rep(c(0L, 1L), 30L),
    t = as.POSIXct("2020-01-01", tz = "UTC") +
      seq(0, by = 86400, length.out = 60L),
    group = rep(c("first", "second", "third"), length.out = 60L),
    stringsAsFactors = FALSE
  )
  meta_data <- data.frame(
    VAR_NAMES = c("y", "t", "group"),
    LABEL = c("y", "t", "group"),
    DATA_TYPE = c(
      DATA_TYPES$INTEGER, DATA_TYPES$DATETIME, DATA_TYPES$STRING
    ),
    SCALE_LEVEL = c(
      SCALE_LEVELS$NOMINAL, SCALE_LEVELS$INTERVAL, SCALE_LEVELS$NOMINAL
    ),
    MISSING_LIST = SPLIT_CHAR,
    JUMP_LIST = SPLIT_CHAR,
    RECODE_CASES = "1",
    RECODE_CONTROL = "0",
    stringsAsFactors = FALSE
  )

  constant_data <- study_data
  constant_data$y <- 1L
  expect_error(
    suppressMessages(util_acc_loess_bin(
      resp_vars = "y",
      group_vars = "group",
      time_vars = "t",
      study_data = constant_data,
      meta_data = meta_data,
      label_col = LABEL,
      min_obs_in_subgroup = 2L,
      min_proportion = 0.1,
      resolution = 5L
    )),
    "response variable is constant"
  )

  rare_data <- study_data
  rare_data$y <- c(rep(0L, 58L), 1L, 1L)
  expect_error(
    suppressMessages(util_acc_loess_bin(
      resp_vars = "y",
      group_vars = "group",
      time_vars = "t",
      study_data = rare_data,
      meta_data = meta_data,
      label_col = LABEL,
      min_obs_in_subgroup = 2L,
      min_proportion = 0.1,
      resolution = 5L
    )),
    "too few cases/controls"
  )
})
