skip_on_cran()

test_that("stratified item-level limits use first matching interval", {
  skip_on_cran()

  study_data <- data.frame(
    SBP = c(95, 110, 195, 95, 170),
    SEX = c("f", "f", "m", "m", "x")
  )
  meta_data <- data.frame(
    VAR_NAMES = c("SBP", "SEX"),
    DATA_TYPE = c(DATA_TYPES$INTEGER, DATA_TYPES$STRING),
    SCALE_LEVEL = c(SCALE_LEVELS$RATIO, SCALE_LEVELS$NOMINAL),
    MISSING_LIST = SPLIT_CHAR,
    JUMP_LIST = SPLIT_CHAR,
    HARD_LIMITS = c(
      '[SEX] = "f": [100;180] | [SEX] = "m": [90;190] | [0;200]',
      NA_character_
    ),
    stringsAsFactors = FALSE
  )

  res <- con_limit_deviations(
    resp_vars = "SBP",
    study_data = study_data,
    meta_data = meta_data,
    label_col = VAR_NAMES
  )

  expect_equal(
    res$SummaryData$`Below limits N (%)`[
      res$SummaryData$Limits == HARD_LIMITS
    ],
    "1 (20)"
  )
  expect_equal(
    res$SummaryData$`Above limits N (%)`[
      res$SummaryData$Limits == HARD_LIMITS
    ],
    "1 (20)"
  )
  expect_equal(
    unname(as.numeric(res$SummaryTable$NUM_con_rvv_inum)),
    2
  )
})


test_that("stratified item-level limits can use an open default interval", {
  skip_on_cran()

  study_data <- data.frame(
    VALUE = c(-1, 5, 11, 100),
    GROUP = c("checked", "checked", "checked", "unchecked")
  )
  meta_data <- data.frame(
    VAR_NAMES = c("VALUE", "GROUP"),
    DATA_TYPE = c(DATA_TYPES$INTEGER, DATA_TYPES$STRING),
    SCALE_LEVEL = c(SCALE_LEVELS$RATIO, SCALE_LEVELS$NOMINAL),
    MISSING_LIST = SPLIT_CHAR,
    JUMP_LIST = SPLIT_CHAR,
    HARD_LIMITS = c('[GROUP] = "checked": [0;10] | (;)', NA_character_),
    stringsAsFactors = FALSE
  )

  res <- con_limit_deviations(
    resp_vars = "VALUE",
    study_data = study_data,
    meta_data = meta_data,
    label_col = VAR_NAMES
  )

  expect_equal(
    res$SummaryData$`All outside limits N (%)`[
      res$SummaryData$Limits == HARD_LIMITS
    ],
    "2 (50)"
  )
})


test_that("stratified item-level limits support issue-style IN intervals", {
  skip_on_cran()

  study_data <- data.frame(
    SBP = c(89, 100, 181, 109, 150, 191, 94, 200),
    AGE = c(10, 12, 17, 18, 40, 80, NA, NA)
  )
  meta_data <- data.frame(
    VAR_NAMES = c("SBP", "AGE"),
    DATA_TYPE = c(DATA_TYPES$INTEGER, DATA_TYPES$INTEGER),
    SCALE_LEVEL = c(SCALE_LEVELS$RATIO, SCALE_LEVELS$RATIO),
    MISSING_LIST = SPLIT_CHAR,
    JUMP_LIST = SPLIT_CHAR,
    HARD_LIMITS = c(
      "[AGE] IN [0, 18): [90;180] | [AGE] IN [18, Inf): [110;190] | [95;185]",
      NA_character_
    ),
    stringsAsFactors = FALSE
  )

  res <- con_limit_deviations(
    resp_vars = "SBP",
    study_data = study_data,
    meta_data = meta_data,
    label_col = VAR_NAMES
  )
  plot_build <- ggplot2::ggplot_build(res$SummaryPlotList$SBP)

  expect_equal(
    res$SummaryData$`All outside limits N (%)`[
      res$SummaryData$Limits == HARD_LIMITS
    ],
    "6 (75)"
  )
  expect_equal(
    sort(unique(as.integer(plot_build$layout$layout$PANEL))),
    1:3
  )
})


test_that("stratified item-level limits honor DATA_PREPARATION LABEL", {
  skip_on_cran()

  study_data <- data.frame(
    SBP = c(95, 110, 195, 95),
    SEX = c(0, 0, 1, 1)
  )
  meta_data <- data.frame(
    VAR_NAMES = c("SBP", "SEX"),
    DATA_TYPE = c(DATA_TYPES$INTEGER, DATA_TYPES$INTEGER),
    SCALE_LEVEL = c(SCALE_LEVELS$RATIO, SCALE_LEVELS$NOMINAL),
    MISSING_LIST = SPLIT_CHAR,
    JUMP_LIST = SPLIT_CHAR,
    VALUE_LABELS = c(NA_character_, "0 = female | 1 = male"),
    DATA_PREPARATION = c(
      sprintf(
        "LABEL %s MISSING_NA",
        SPLIT_CHAR
      ),
      NA_character_
    ),
    HARD_LIMITS = c(
      '[SEX] = "female": [100;180] | [SEX] = "male": [90;190]',
      NA_character_
    ),
    stringsAsFactors = FALSE
  )

  res <- con_limit_deviations(
    resp_vars = "SBP",
    study_data = study_data,
    meta_data = meta_data,
    label_col = VAR_NAMES
  )

  expect_equal(
    res$SummaryData$`All outside limits N (%)`[
      res$SummaryData$Limits == HARD_LIMITS
    ],
    "2 (50)"
  )
})


test_that("stratified item-level limits can use raw DATA_PREPARATION", {
  skip_on_cran()

  study_data <- data.frame(
    SBP = c(95, 110, 195, 95),
    SEX = c(0, 0, 1, 1)
  )
  meta_data <- data.frame(
    VAR_NAMES = c("SBP", "SEX"),
    DATA_TYPE = c(DATA_TYPES$INTEGER, DATA_TYPES$INTEGER),
    SCALE_LEVEL = c(SCALE_LEVELS$RATIO, SCALE_LEVELS$NOMINAL),
    MISSING_LIST = SPLIT_CHAR,
    JUMP_LIST = SPLIT_CHAR,
    VALUE_LABELS = c(NA_character_, "0 = female | 1 = male"),
    DATA_PREPARATION = c("MISSING_NA", NA_character_),
    HARD_LIMITS = c(
      "[SEX] = 0: [100;180] | [SEX] = 1: [90;190]",
      NA_character_
    ),
    stringsAsFactors = FALSE
  )

  res <- con_limit_deviations(
    resp_vars = "SBP",
    study_data = study_data,
    meta_data = meta_data,
    label_col = VAR_NAMES
  )

  expect_equal(
    res$SummaryData$`All outside limits N (%)`[
      res$SummaryData$Limits == HARD_LIMITS
    ],
    "2 (50)"
  )
})


test_that("stratified item-level limits reject DATA_PREPARATION LIMITS", {
  skip_on_cran()

  study_data <- data.frame(
    SBP = c(95, 110, 195, 95),
    SEX = c(0, 0, 1, 1)
  )
  meta_data <- data.frame(
    VAR_NAMES = c("SBP", "SEX"),
    DATA_TYPE = c(DATA_TYPES$INTEGER, DATA_TYPES$INTEGER),
    SCALE_LEVEL = c(SCALE_LEVELS$RATIO, SCALE_LEVELS$NOMINAL),
    MISSING_LIST = SPLIT_CHAR,
    JUMP_LIST = SPLIT_CHAR,
    DATA_PREPARATION = c("LABEL | MISSING_NA | LIMITS", NA_character_),
    HARD_LIMITS = c(
      "[SEX] = 0: [100;180] | [SEX] = 1: [90;190]",
      NA_character_
    ),
    stringsAsFactors = FALSE
  )

  expect_error(
    con_limit_deviations(
      resp_vars = "SBP",
      study_data = study_data,
      meta_data = meta_data,
      label_col = VAR_NAMES
    ),
    "cannot be used for stratified item-level limits"
  )
})


test_that("stratified item-level limits create a stratified plot", {
  skip_on_cran()

  study_data <- data.frame(
    SBP = c(95, 110, 195, 95, 170),
    SEX = c("f", "f", "m", "m", "x")
  )
  meta_data <- data.frame(
    VAR_NAMES = c("SBP", "SEX"),
    DATA_TYPE = c(DATA_TYPES$INTEGER, DATA_TYPES$STRING),
    SCALE_LEVEL = c(SCALE_LEVELS$RATIO, SCALE_LEVELS$NOMINAL),
    MISSING_LIST = SPLIT_CHAR,
    JUMP_LIST = SPLIT_CHAR,
    HARD_LIMITS = c(
      '[SEX] = "f": [100;180] | [SEX] = "m": [90;190] | [0;200]',
      NA_character_
    ),
    stringsAsFactors = FALSE
  )

  res <- con_limit_deviations(
    resp_vars = "SBP",
    study_data = study_data,
    meta_data = meta_data,
    label_col = VAR_NAMES
  )
  plot_build <- ggplot2::ggplot_build(res$SummaryPlotList$SBP)

  expect_s3_class(res$SummaryPlotList$SBP, "dq_lazy_ggplot")
  expect_equal(
    sort(unique(as.integer(plot_build$layout$layout$PANEL))),
    1:3
  )
})


test_that("stratified item-level limit plots honor the strata limit option", {
  skip_on_cran()

  old_options <- options(dataquieR.max_strata_in_limit_plots = 2)
  on.exit(options(old_options), add = TRUE)

  study_data <- data.frame(
    SBP = c(95, 110, 195, 95, 170),
    SEX = c("f", "f", "m", "m", "x")
  )
  meta_data <- data.frame(
    VAR_NAMES = c("SBP", "SEX"),
    DATA_TYPE = c(DATA_TYPES$INTEGER, DATA_TYPES$STRING),
    SCALE_LEVEL = c(SCALE_LEVELS$RATIO, SCALE_LEVELS$NOMINAL),
    MISSING_LIST = SPLIT_CHAR,
    JUMP_LIST = SPLIT_CHAR,
    HARD_LIMITS = c(
      '[SEX] = "f": [100;180] | [SEX] = "m": [90;190] | [0;200]',
      NA_character_
    ),
    stringsAsFactors = FALSE
  )

  res <- con_limit_deviations(
    resp_vars = "SBP",
    study_data = study_data,
    meta_data = meta_data,
    label_col = VAR_NAMES
  )
  plot_build <- ggplot2::ggplot_build(res$SummaryPlotList$SBP)

  expect_match(
    plot_build$data[[2]]$label,
    "Too many strata"
  )
})


test_that("stratified item-level limit plots convert to plotly", {
  skip_on_cran()
  skip_if_not_installed("plotly")

  study_data <- data.frame(
    SBP = c(110, 120, 130, 140, 150, 160),
    THERAPY = c("1", "1", "2", "2", "9", "9")
  )
  meta_data <- data.frame(
    VAR_NAMES = c("SBP", "THERAPY"),
    DATA_TYPE = c(DATA_TYPES$FLOAT, DATA_TYPES$STRING),
    SCALE_LEVEL = c(SCALE_LEVELS$RATIO, SCALE_LEVELS$NOMINAL),
    MISSING_LIST = SPLIT_CHAR,
    JUMP_LIST = SPLIT_CHAR,
    VALUE_LABELS = c(NA, "1 = treated | 2 = untreated"),
    DATA_PREPARATION = c("LABEL | MISSING_NA", NA),
    HARD_LIMITS = c(
      paste0(
        '[THERAPY] = "treated": [100;170] | ',
        '[THERAPY] = "untreated": [110;190] | [95;195]'
      ),
      NA_character_
    ),
    stringsAsFactors = FALSE
  )

  res <- con_limit_deviations(
    resp_vars = "SBP",
    study_data = study_data,
    meta_data = meta_data,
    label_col = VAR_NAMES
  )
  py <- util_as_plotly_con_limit_deviations(res)
  annotation_text <- vapply(
    py$x$layout$annotations,
    function(annotation) annotation[["text"]] %||% "",
    FUN.VALUE = character(1)
  )

  expect_s3_class(py, "plotly")
  expect_true(all(c(
    '[THERAPY] = "treated"',
    '[THERAPY] = "untreated"',
    "default"
  ) %in% annotation_text))
})


test_that("simple item-level intervals are still parsed as plain intervals", {
  expect_s3_class(util_parse_stratified_limit("[0;10]"), "interval")
})

test_that("stratified limit parser ignores malformed parts", {
  skip_on_cran()

  expect_true(is.na(util_parse_stratified_limit("")))
  expect_true(is.na(util_parse_stratified_limit("not a rule")))
  expect_true(is.na(util_parse_stratified_limit(": [0;10]")))
  expect_true(is.na(util_parse_stratified_limit("[GROUP] = 1: bad interval")))

  parsed <- util_parse_stratified_limit(
    "[GROUP] = 1: [0;10] | malformed | [1;2]"
  )

  expect_s3_class(parsed, "stratified_limit")
  expect_length(parsed$rules, 2)
  expect_false(parsed$rules[[1]]$is_default)
  expect_true(parsed$rules[[2]]$is_default)
})

test_that("stratified limit preparation helpers use conservative defaults", {
  skip_on_cran()

  meta_data <- data.frame(
    VAR_NAMES = "SBP",
    stringsAsFactors = FALSE
  )

  expect_identical(
    util_stratified_limit_data_preparation(meta_data, VAR_NAMES, "SBP"),
    sprintf("LABEL %s MISSING_NA", SPLIT_CHAR)
  )
  expect_identical(
    util_stratified_limit_eval_preparation(character()),
    list(
      use_value_labels = TRUE,
      replace_missing_by = "NA",
      replace_limits = FALSE
    )
  )
})

test_that("stratified limit plot option falls back to five strata", {
  skip_on_cran()

  withr::local_options(dataquieR.max_strata_in_limit_plots = "many")
  expect_identical(util_max_strata_in_limit_plots(), 5L)

  withr::local_options(dataquieR.max_strata_in_limit_plots = 2.9)
  expect_identical(util_max_strata_in_limit_plots(), 2L)
})

test_that("stratified limit helper paths resolve and classify locally", {
  skip_on_cran()

  expect_identical(
    util_normalize_stratified_limit_condition("[age] in [1,3]"),
    "[age] in [1;3]"
  )

  meta_data <- data.frame(
    VAR_NAMES = c("value", "group"),
    HARD_LIMITS = c('[group] = "A": [0;10] | [10;20]', "[0;5]"),
    SOFT_LIMITS = c("not a rule", '[group] = "B": [0;1] | [1;2]'),
    stringsAsFactors = FALSE
  )
  masked <- util_mask_stratified_limits_for_validation(meta_data)
  expect_true(is.na(masked$HARD_LIMITS[[1]]))
  expect_identical(masked$HARD_LIMITS[[2]], "[0;5]")
  expect_identical(masked$SOFT_LIMITS[[1]], "not a rule")
  expect_true(is.na(masked$SOFT_LIMITS[[2]]))

  limit <- util_parse_stratified_limit(
    '[group] = "A": [0;10] | [10;20]'
  )
  ds1 <- data.frame(
    group = c("A", "B", "A", NA),
    value = c(-1, 15, 25, 12),
    stringsAsFactors = FALSE
  )
  item_level <- prep_create_meta(
    VAR_NAMES = c("group", "value"),
    DATA_TYPE = c(DATA_TYPES$STRING, DATA_TYPES$INTEGER),
    SCALE_LEVEL = c(SCALE_LEVELS$NOMINAL, SCALE_LEVELS$RATIO),
    MISSING_LIST = SPLIT_CHAR,
    JUMP_LIST = SPLIT_CHAR
  )

  rules <- suppressMessages(util_resolve_stratified_limit_rules(
    limit,
    ds1 = ds1,
    meta_data = item_level,
    data_preparation = "VALUE | MISSING_NA"
  ))
  intervals <- suppressMessages(util_resolve_stratified_limit(
    limit,
    ds1 = ds1,
    meta_data = item_level,
    data_preparation = "VALUE | MISSING_NA"
  ))

  expect_identical(rules, c(1L, 2L, 1L, 2L))
  expect_equal(
    util_classify_stratified_limits(ds1$value, intervals),
    factor(
      c("below", "within", "above", "within"),
      levels = c("below", "within", "above")
    )
  )
})

test_that("stratified limit condition normalization keeps rule structure", {
  skip_on_cran()

  condition <- paste(
    "[age] in [1,3]",
    "and",
    "[group] not in (2,4)"
  )

  expect_identical(
    util_normalize_stratified_limit_condition(condition),
    paste(
      "[age] in [1;3]",
      "and",
      "[group] not in (2;4)"
    )
  )
})

test_that("stratified limit parser keeps a single default fallback", {
  skip_on_cran()

  withr::local_options(dataquieR.testthat_expect_message_active = TRUE)

  msg <- expect_message(
    parsed <- util_parse_stratified_limit("[1;2] | malformed"),
    "Ignoring malformed stratified limit part"
  )

  expect_s3_class(parsed, "interval")
  expect_equal(parsed$low, 1)
  expect_equal(parsed$upp, 2)
  expect_s3_class(msg, dataquieR.applicability_problem)
})

test_that("stratified limit helpers handle empty prep and missing values", {
  skip_on_cran()

  meta_data <- data.frame(
    VAR_NAMES = "SBP",
    DATA_PREPARATION = "",
    stringsAsFactors = FALSE
  )
  expect_identical(
    util_stratified_limit_data_preparation(meta_data, VAR_NAMES, "SBP"),
    sprintf("LABEL %s MISSING_NA", SPLIT_CHAR)
  )

  intervals <- list(util_try_parse_interval("[0;10]"), NA)
  expect_equal(
    util_classify_stratified_limits(c(NA_real_, 5), intervals),
    factor(c(NA_character_, NA_character_),
      levels = c("below", "within", "above")
    )
  )
})

test_that("stratified limit plot returns message plots for guardrails", {
  skip_on_cran()

  item_level <- prep_create_meta(
    VAR_NAMES = c("value", "group"),
    LABEL = c("Value", "Group"),
    DATA_TYPE = c(DATA_TYPES$INTEGER, DATA_TYPES$INTEGER),
    SCALE_LEVEL = c(SCALE_LEVELS$RATIO, SCALE_LEVELS$NOMINAL),
    MISSING_LIST = SPLIT_CHAR,
    JUMP_LIST = SPLIT_CHAR
  )
  ds1 <- data.frame(
    value = c(NA_real_, NA_real_),
    group = c(1L, 2L)
  )
  stratified <- util_parse_stratified_limit("[group] = 1: [0;1] | [0;10]")
  no_data_plot <- suppressMessages(util_create_stratified_limit_plot(
    rv = "value",
    ds1 = ds1,
    meta_data = item_level,
    label_col = VAR_NAMES,
    limits = list(HARD_LIMITS = stratified),
    limit_results = list(HARD_LIMITS = factor(c(NA, NA),
        levels = c("below", "within", "above")
      )),
    data_preparation = "VALUE | MISSING_NA",
    is_datetime_var = FALSE,
    is_time_var = FALSE,
    spec_txt = ggplot2::element_text(),
    ref_env = new.env(parent = emptyenv()),
    show_obs = TRUE
  ))
  multiple_plot <- util_create_stratified_limit_plot(
    rv = "value",
    ds1 = data.frame(value = 1L, group = 1L),
    meta_data = item_level,
    label_col = VAR_NAMES,
    limits = list(HARD_LIMITS = stratified, SOFT_LIMITS = stratified),
    limit_results = list(
      HARD_LIMITS = factor("within", levels = c("below", "within", "above")),
      SOFT_LIMITS = factor("within", levels = c("below", "within", "above"))
    ),
    data_preparation = "VALUE | MISSING_NA",
    is_datetime_var = FALSE,
    is_time_var = FALSE,
    spec_txt = ggplot2::element_text(),
    ref_env = new.env(parent = emptyenv()),
    show_obs = TRUE
  )

  expect_s3_class(no_data_plot, "dq_lazy_ggplot")
  expect_s3_class(multiple_plot, "dq_lazy_ggplot")
})

test_that("stratified limit plot handles datetime and time values", {
  skip_on_cran()
  skip_if_not_installed("hms")

  base_item_level <- prep_create_meta(
    VAR_NAMES = c("value", "group"),
    LABEL = c("Value", "Group"),
    DATA_TYPE = c(DATA_TYPES$DATETIME, DATA_TYPES$INTEGER),
    SCALE_LEVEL = c(SCALE_LEVELS$RATIO, SCALE_LEVELS$NOMINAL),
    MISSING_LIST = SPLIT_CHAR,
    JUMP_LIST = SPLIT_CHAR
  )
  limit <- util_parse_stratified_limit("[group] = 1: [0;Inf]")
  classified <- factor("within", levels = c("below", "within", "above"))

  create_plot <- function(ds1, meta_data, is_datetime_var, is_time_var,
    flip_mode = "noflip") {
    suppressMessages(util_create_stratified_limit_plot(
      rv = "value",
      ds1 = ds1,
      meta_data = meta_data,
      label_col = VAR_NAMES,
      limits = list(SOFT_LIMITS = limit),
      limit_results = list(SOFT_LIMITS = classified),
      data_preparation = "VALUE | MISSING_NA",
      is_datetime_var = is_datetime_var,
      is_time_var = is_time_var,
      spec_txt = ggplot2::element_text(),
      ref_env = environment(),
      show_obs = FALSE
    ))
  }

  datetime_plot <- create_plot(
    ds1 = data.frame(
      value = as.POSIXct("2026-07-28 09:00:00", tz = "UTC"),
      group = 1L
    ),
    meta_data = base_item_level,
    is_datetime_var = TRUE,
    is_time_var = FALSE
  )

  time_item_level <- base_item_level
  time_item_level[[DATA_TYPE]][time_item_level[[VAR_NAMES]] == "value"] <-
    DATA_TYPES$TIME
  time_plot <- create_plot(
    ds1 = data.frame(value = "09:00:00", group = 1L),
    meta_data = time_item_level,
    is_datetime_var = FALSE,
    is_time_var = TRUE
  )

  expect_s3_class(datetime_plot, "dq_lazy_ggplot")
  expect_s3_class(time_plot, "dq_lazy_ggplot")
})
