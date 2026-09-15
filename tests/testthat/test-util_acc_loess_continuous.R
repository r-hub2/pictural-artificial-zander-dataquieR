test_that("util_acc_loess_continuous handles local plot guardrails", {
  skip_on_cran()

  study_data <- data.frame(
    y = seq_len(60L),
    t = as.POSIXct("2020-01-01", tz = "UTC") +
      seq(0, by = 86400, length.out = 60L),
    group = rep(sprintf("g%02d", 1:12), each = 5L),
    stringsAsFactors = FALSE
  )
  meta_data <- data.frame(
    VAR_NAMES = c("y", "t", "group"),
    LABEL = c("y", "t", "group"),
    DATA_TYPE = c(DATA_TYPES$FLOAT, DATA_TYPES$DATETIME, DATA_TYPES$STRING),
    SCALE_LEVEL = c(
      SCALE_LEVELS$RATIO, SCALE_LEVELS$INTERVAL, SCALE_LEVELS$NOMINAL
    ),
    MISSING_LIST = SPLIT_CHAR,
    JUMP_LIST = SPLIT_CHAR,
    stringsAsFactors = FALSE
  )

  combined <- suppressMessages(util_acc_loess_continuous(
    resp_vars = "y",
    group_vars = "group",
    time_vars = "t",
    study_data = study_data,
    meta_data = meta_data,
    label_col = LABEL,
    min_obs_in_subgroup = 2L,
    resolution = 5L,
    comparison_lines = list(type = "quartiles"),
    n_group_max = 3L,
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

  facets <- suppressMessages(util_acc_loess_continuous(
    resp_vars = "y",
    group_vars = "group",
    time_vars = "t",
    study_data = study_data,
    meta_data = meta_data,
    label_col = LABEL,
    min_obs_in_subgroup = 2L,
    resolution = 5L,
    comparison_lines = list(type = "mean/sd", sd_factor = 0.25),
    n_group_max = 3L,
    plot_format = "FACETS"
  ))

  expect_named(facets$SummaryPlotList, "y")
  expect_null(util_attr(facets, "sizing_hints", exact = TRUE))

  both <- suppressMessages(util_acc_loess_continuous(
    resp_vars = "y",
    group_vars = "group",
    time_vars = "t",
    study_data = study_data,
    meta_data = meta_data,
    label_col = LABEL,
    min_obs_in_subgroup = 2L,
    resolution = 5L,
    comparison_lines = list(type = c("mean/sd", "quartiles")),
    mark_time_points = TRUE,
    plot_observations = TRUE,
    n_group_max = 3L,
    plot_format = "BOTH"
  ))

  expect_named(
    both$SummaryPlotList,
    c("Loess_fits_facets", "Loess_fits_combined")
  )

  dummy_group <- suppressMessages(util_acc_loess_continuous(
    resp_vars = "y",
    time_vars = "t",
    study_data = study_data,
    meta_data = meta_data,
    label_col = LABEL,
    min_obs_in_subgroup = 2L,
    resolution = 5L,
    plot_format = c("AUTO", "BOTH")
  ))

  expect_named(dummy_group$SummaryPlotList, "y")
  expect_equal(
    util_attr(dummy_group, "sizing_hints", exact = TRUE)$figure_type_id,
    "dot_loess"
  )
})

test_that("util_acc_loess_continuous reports argument and window guardrails", {
  skip_on_cran()

  study_data <- data.frame(
    y = seq_len(12L),
    t = as.POSIXct("2020-01-01", tz = "UTC") +
      seq(0, by = 86400, length.out = 12L),
    group = rep(c("first", "second", "third"), each = 4L),
    stringsAsFactors = FALSE
  )
  meta_data <- data.frame(
    VAR_NAMES = c("y", "t", "group"),
    LABEL = c("y", "t", "group"),
    DATA_TYPE = c(DATA_TYPES$FLOAT, DATA_TYPES$DATETIME, DATA_TYPES$STRING),
    SCALE_LEVEL = c(
      SCALE_LEVELS$RATIO, SCALE_LEVELS$INTERVAL, SCALE_LEVELS$NOMINAL
    ),
    MISSING_LIST = SPLIT_CHAR,
    JUMP_LIST = SPLIT_CHAR,
    stringsAsFactors = FALSE
  )

  expect_error(
    suppressMessages(util_acc_loess_continuous(
      resp_vars = "y",
      group_vars = "group",
      time_vars = "t",
      study_data = study_data,
      meta_data = meta_data,
      label_col = LABEL,
      comparison_lines = list(type = "invalid")
    )),
    "comparison_lines"
  )

  null_window <- util_for_moving_window(
    study_data$t, study_data$y, study_data$t,
    i = NULL, part1 = 1L, part2 = 2L,
    mode = "mean/sd", sd_fac = 0.5
  )
  expect_equal(names(null_window), c("low", "mid", "high"))
  expect_true(all(is.na(null_window)))

  expect_equal(
    util_for_moving_window(
      study_data$t, study_data$y, study_data$t,
      i = 1:2, part1 = 1L, part2 = 2L,
      mode = "mean/sd", sd_fac = 0.5
    ),
    list(
      c(low = 1.5, mid = 2, high = 2.5),
      c(low = 2.5, mid = 3, high = 3.5)
    )
  )

  edge_window <- util_for_moving_window(
    study_data$t, study_data$y, study_data$t,
    i = 0L, part1 = 1L, part2 = 2L,
    mode = "quartiles", sd_fac = 0.5
  )
  expect_equal(names(edge_window), c("low", "mid", "high"))
  expect_true(all(is.na(edge_window)))
})
