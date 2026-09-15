test_that("distribution plot works", { # acc_distributions.R ----
  skip_on_cran()
  skip_if_not_installed("lobstr")
  skip_if_not_installed("vdiffr")

  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  withr::local_options(dataquieR.CONDITIONS_LEVEL_TRHESHOLD = Inf)

  ({
    r <-
      acc_distributions(
        resp_vars = "DBP_0",
        study_data = "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData", # nolint: line_length_linter.
        meta_data = "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data.RData", # nolint: line_length_linter.
        label_col = LABEL
      )

    expect_doppelganger2("acc_distributions gr dbp0", r$SummaryPlotList$DBP_0)
    expect_lt(lobstr::obj_size(r$SummaryPlotList$DBP_0), 15 * 1024 * 1024)

    r <-
      acc_distributions(
        resp_vars = "EDUCATION_0",
        study_data = "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData", # nolint: line_length_linter.
        meta_data = "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data.RData", # nolint: line_length_linter.
        label_col = LABEL
      )

    expect_doppelganger2("acc_distributions def edu0", r$SummaryPlotList$EDUCATION_0) # nolint: line_length_linter.
    expect_lt(lobstr::obj_size(r$SummaryPlotList$EDUCATION_0), 15 * 1024 * 1024)
  })
})

test_that("loess plot works", { # acc_loess.R ----
  # Use testthat::local_reproducible_output() when debugging locally.
  # Travis used to fail these vdiffr expectations.
  skip_on_cran()
  skip_if_not_installed("lobstr")
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")

  skip_if_not_installed("vdiffr")
  withr::local_options(dataquieR.CONDITIONS_LEVEL_TRHESHOLD = Inf)

  require_english_locale_and_berlin_tz()

  ({
    time_vars <- prep_map_labels("DBP_0",
      meta_data = "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx|item_level", from = LABEL, # nolint: line_length_linter.
      to = TIME_VAR
    )
    group_vars <- prep_map_labels("DBP_0",
      meta_data = "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx|item_level", from = LABEL, # nolint: line_length_linter.
      to = GROUP_VAR_OBSERVER
    )
    r <-
      acc_loess(
        resp_vars = "DBP_0",
        study_data = "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData", # nolint: line_length_linter.
        meta_data = "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data.RData", # nolint: line_length_linter.
        group_vars = group_vars,
        time_vars = time_vars,
        label_col = LABEL,
        co_vars = "AGE_0"
      )

    expect_doppelganger2("loess def dbp0", r$SummaryPlotList$DBP_0)
    expect_lt(lobstr::obj_size(r$SummaryPlotList$DBP_0), 15 * 1024 * 1024)

    r <- acc_loess(
      resp_vars = "EDUCATION_0",
      study_data = "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData", # nolint: line_length_linter.
      meta_data = "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data.RData", # nolint: line_length_linter.
      time_vars = time_vars,
      group_vars = group_vars,
      label_col = LABEL,
      co_vars = "AGE_0"
    )

    expect_doppelganger2("loess def edu0", r$SummaryPlotList$EDUCATION_0)
    expect_lt(lobstr::obj_size(r$SummaryPlotList$EDUCATION_0), 15 * 1024 * 1024)

    r <-
      acc_loess(
        resp_vars = "DBP_0",
        study_data = "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData", # nolint: line_length_linter.
        meta_data = "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data.RData", # nolint: line_length_linter.
        group_vars = group_vars,
        time_vars = time_vars,
        plot_format = "FACETS",
        label_col = LABEL,
        co_vars = "AGE_0"
      )

    expect_doppelganger2("loess fac dbp0", r$SummaryPlotList$DBP_0)
    expect_lt(lobstr::obj_size(r$SummaryPlotList$DBP_0), 15 * 1024 * 1024)

    r <-
      acc_loess(
        resp_vars = "EDUCATION_0",
        study_data = "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData", # nolint: line_length_linter.
        meta_data = "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data.RData", # nolint: line_length_linter.
        time_vars = time_vars,
        group_vars = group_vars,
        plot_format = "FACETS",
        label_col = LABEL,
        co_vars = "AGE_0"
      )

    expect_doppelganger2("loess fac edu0", r$SummaryPlotList$EDUCATION_0)
    expect_lt(lobstr::obj_size(r$SummaryPlotList$EDUCATION_0), 15 * 1024 * 1024)
  })
})

test_that("margins plot works", { # acc_margins.R -----
  skip_on_cran()
  skip_if_not_installed("lobstr")
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  skip_if_not_installed("vdiffr")

  withr::local_options(dataquieR.CONDITIONS_LEVEL_TRHESHOLD = Inf)
  ({
    group_vars <- prep_map_labels("DBP_0",
      meta_data = "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx|item_level", # nolint: line_length_linter.
      from = LABEL,
      to = GROUP_VAR_OBSERVER
    )
    r <-
      acc_margins(
        resp_vars = "DBP_0",
        study_data = "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData", # nolint: line_length_linter.
        meta_data = "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data.RData", # nolint: line_length_linter.
        group_vars = group_vars,
        label_col = LABEL,
        co_vars = "AGE_0",
        sort_group_var_levels = FALSE
      )

    expect_doppelganger2("margins dbp0", r$SummaryPlot)
    r1 <- r

    prep_purge_data_frame_cache()
    skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
    prep_load_workbook_like_file("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx") # nolint: line_length_linter.
    md <- prep_get_data_frame("item_level")

    # tweak metadata to enable the test
    md$SCALE_LEVEL[md$LABEL == "EDUCATION_0"] <- SCALE_LEVELS$INTERVAL

    r <-
      acc_margins(
        resp_vars = "EDUCATION_0",
        study_data = "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData", # nolint: line_length_linter.
        meta_data = md,
        group_vars = group_vars,
        label_col = LABEL,
        co_vars = "AGE_0"
      )

    expect_doppelganger2("margins edu0", r$SummaryPlot)
    skip_on_covr()
    expect_lt(lobstr::obj_size(r1$SummaryPlot), 15 * 1024 * 1024)
    expect_lt(lobstr::obj_size(r$SummaryPlot), 15 * 1024 * 1024)
  })
})

test_that("multivariate outlier plot works", { # acc_multivariate_outlier.R ----
  skip_on_cran()

  skip_if_not_installed("vdiffr")
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  withr::local_options(dataquieR.CONDITIONS_LEVEL_TRHESHOLD = Inf)

  withr::with_seed(32245253, {
    r <-
      acc_multivariate_outlier(
        variable_group = c("DBP_0", "SBP_0", "AGE_0"),
        label_col = LABEL,
        study_data = "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData", # nolint: line_length_linter.
        meta_data = "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data.RData", # nolint: line_length_linter.
        scale = FALSE
      )

    expect_doppelganger2(
      "acc_multivariate_outlier test 1",
      r$SummaryPlot
    )
    # Historical version-specific skip for lobstr size differences removed.
    skip_if_not_installed("lobstr")
    skip_on_covr()
    expect_lt(lobstr::obj_size(r$SummaryPlot), 15 * 1024 * 1024)
  })
})


test_that("shape or scale plot works", { # acc_shape_or_scale.R -----
  skip_on_cran()
  skip_if_not_installed("lobstr")

  skip_if_not_installed("vdiffr")
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  withr::local_options(dataquieR.CONDITIONS_LEVEL_TRHESHOLD = Inf)
  ({
    r <-
      acc_shape_or_scale(
        resp_vars = "DBP_0",
        study_data = "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData", # nolint: line_length_linter.
        meta_data = "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data.RData", # nolint: line_length_linter.
        label_col = LABEL
      )

    expect_doppelganger2("shape or scale dbp0", r$SummaryPlot)
    expect_lt(lobstr::obj_size(r$SummaryPlot), 15 * 1024 * 1024)
  })
})

test_that("univariate outlier plot works", { # acc_univariate_outlier.R ----
  skip_on_cran()
  skip_if_not_installed("lobstr")
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")

  skip_if_not_installed("vdiffr")
  withr::local_options(dataquieR.CONDITIONS_LEVEL_TRHESHOLD = Inf)
  withr::with_seed(32245253, {
    r <-
      acc_univariate_outlier(
        resp_vars = c("DBP_0", "DEV_NO_0"),
        label_col = LABEL,
        study_data = "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData", # nolint: line_length_linter.
        meta_data = "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data.RData" # nolint: line_length_linter.
      )

    expect_doppelganger2(
      "acc_univariate_outlier.R DBP_0",
      r$SummaryPlotList$DBP_0
    )
    expect_lt(lobstr::obj_size(r$SummaryPlotList$DBP_0), 15 * 1024 * 1024)

    # Argument “resp_vars”: Variable 'DEV_NO_0' (nominal) does not have an
    # allowed scale level (interval | ratio)
    # In “resp_vars”, variables “DEV_NO_0” were excluded.
    expect_null(r$SummaryPlotList$DEV_NO_0)
  })
})

test_that("old contradiction plots work", { # con_contradictions.R ----
  skip_on_cran()
  skip_if_not_installed("lobstr")
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  skip_if_not_installed("vdiffr")

  withr::local_options(dataquieR.CONDITIONS_LEVEL_TRHESHOLD = Inf)

  ({
    check_table <- read.csv(
      "https://dataquality.qihs.uni-greifswald.de/extdata/contradiction_checks.csv", # nolint: line_length_linter.
      header = TRUE, sep = "#"
    )
    check_table[1, "tag"] <- "Logical"
    check_table[1, "Label"] <- "Becomes younger"
    check_table[2, "tag"] <- "Empirical"
    check_table[2, "Label"] <- "sex transformation"
    check_table[3, "tag"] <- "Empirical"
    check_table[3, "Label"] <- "looses academic degree"
    check_table[4, "tag"] <- "Logical"
    check_table[4, "Label"] <- "vegetarian eats meat"
    check_table[5, "tag"] <- "Logical"
    check_table[5, "Label"] <- "vegan eats meat"
    check_table[6, "tag"] <- "Empirical"
    check_table[6, "Label"] <- "non-veg* eats meat"
    check_table[7, "tag"] <- "Empirical"
    check_table[7, "Label"] <- "Non-smoker buys cigarettes"
    check_table[8, "tag"] <- "Empirical"
    check_table[8, "Label"] <- "Smoker always scrounges"
    check_table[9, "tag"] <- "Logical"
    check_table[9, "Label"] <- "Cuff didn't fit arm"
    check_table[10, "tag"] <- "Empirical"
    check_table[10, "Label"] <- "Very mature pregnant woman"
    check_table[1, "tag"] <- "Logical, Age-Related"
    check_table[10, "tag"] <- "Empirical, Age-Related"
    label_col <- "LABEL"
    threshold_value <- 1
    study_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData", keep_types = TRUE) # nolint: line_length_linter.
    meta_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data.RData") # nolint: line_length_linter.
    meta_data <- prep_scalelevel_from_data_and_metadata(
      meta_data = meta_data,
      study_data = study_data
    )
    meta_data[startsWith(meta_data[[LABEL]], "EDUCATION_"), SCALE_LEVEL] <-
      SCALE_LEVELS$ORDINAL
    r <-
      con_contradictions(
        study_data = study_data, meta_data = meta_data, label_col =
        label_col,
        threshold_value = threshold_value, check_table = check_table,
        summarize_categories = TRUE
      )
    expect_doppelganger2(
      "con_contradictions by tag",
      r$SummaryPlot
    )
    expect_lt(lobstr::obj_size(r$SummaryPlot), 15 * 1024 * 1024)

    expect_doppelganger2(
      "con_contradictions logical age checks, only",
      r$`Logical, Age-Related`$SummaryPlot
    )
    expect_lt(
      lobstr::obj_size(r$`Logical, Age-Related`$SummaryPlot),
      15 * 1024 * 1024
    )

    expect_doppelganger2(
      "con_contradictions all checks",
      r$all_checks$SummaryPlot
    )
    expect_lt(
      lobstr::obj_size(r$all_checks$SummaryPlot),
      15 * 1024 * 1024
    )
  })
})

test_that("redcap based contradiction plots work", { # con_contradictions_redcap.R ---- # nolint: line_length_linter.
  skip_on_cran()
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  skip_if_not_installed("lobstr")
  skip_if_not_installed("vdiffr")

  withr::local_options(dataquieR.CONDITIONS_LEVEL_TRHESHOLD = Inf)

  ({
    label_col <- "LABEL"
    threshold_value <- 1
    r <- con_contradictions_redcap(
      study_data = "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData", # nolint: line_length_linter.
      meta_data = "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data.RData", label_col = label_col, # nolint: line_length_linter.
      threshold_value = threshold_value, meta_data_cross_item =
        "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx|cross-item_level", # nolint: line_length_linter.
      summarize_categories = TRUE
    )

    expect_doppelganger2(
      "con_contradictions rc by tag",
      r$SummaryPlot
    )
    expect_lt(lobstr::obj_size(r$SummaryPlot), 15 * 1024 * 1024)

    expect_doppelganger2(
      "con_contradictions rc logical checks, only",
      r$Other$LOGICAL$SummaryPlot
    )
    expect_lt(lobstr::obj_size(r$Other$LOGICAL$SummaryPlot), 15 * 1024 * 1024)

    expect_doppelganger2(
      "con_contradictions rc all checks",
      r$Other$all_checks$SummaryPlot
    )
    expect_lt(
      lobstr::obj_size(r$Other$all_checks$SummaryPlot),
      15 * 1024 * 1024
    )
  })
})

test_that("limit deviation plots work", { # con_limit_deviations.R ----
  skip_on_cran()
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  skip_if_not_installed("lobstr")
  skip_if_not_installed("vdiffr")

  withr::local_options(dataquieR.CONDITIONS_LEVEL_TRHESHOLD = Inf)
  set.seed(2012) # randomly scattered points should stay in their position for testing # nolint: line_length_linter.

  ({
    label_col <- "LABEL"
    threshold_value <- 1
    meta_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data.RData") # nolint: line_length_linter.
    meta_data[meta_data$LABEL == "QUEST_DT_0", "HARD_LIMITS"] <-
      "[2018-01-01 00:00:00 CET; 2022-12-12 23:59:59 CET)"

    r1 <- con_limit_deviations( # all limits exist
      resp_vars = c("SBP_0", "ITEM_1_0", "QUEST_DT_0"),
      study_data = "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData", meta_data = meta_data, # nolint: line_length_linter.
      label_col = label_col,
      limits = "HARD"
    )

    meta_data[meta_data$LABEL == "SBP_0", "HARD_LIMITS"] <-
      "[80; )"
    meta_data[meta_data$LABEL == "ITEM_1_0", "HARD_LIMITS"] <-
      "[0; )"
    meta_data[meta_data$LABEL == "QUEST_DT_0", "HARD_LIMITS"] <-
      "[2018-01-01 00:00:00 CET; )"

    r2 <- con_limit_deviations( # lower limits exist
      resp_vars = c("SBP_0", "ITEM_1_0", "QUEST_DT_0"),
      study_data = "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData", meta_data = meta_data, # nolint: line_length_linter.
      label_col = label_col,
      limits = "HARD"
    )

    meta_data[meta_data$LABEL == "SBP_0", "HARD_LIMITS"] <-
      "(; 180]"
    meta_data[meta_data$LABEL == "ITEM_1_0", "HARD_LIMITS"] <-
      "(; 10]"
    meta_data[meta_data$LABEL == "QUEST_DT_0", "HARD_LIMITS"] <-
      "(; 2022-12-12 23:59:59 CET]"

    r3 <- con_limit_deviations( # upper limits exist
      resp_vars = c("SBP_0", "ITEM_1_0", "QUEST_DT_0"),
      study_data = "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData", # nolint: line_length_linter.
      meta_data = meta_data,
      label_col = label_col,
      limits = "HARD"
    )

    # Use con_limit_deviations() locally with SBP_0, ITEM_1_0, and QUEST_DT_0
    # after clearing HARD_LIMITS to inspect the no-limits fixture.

    # > 20 < 20 obs, w/ and w/o limits (upper/lower), date vars .//. others

    expect_doppelganger2(
      "con_limit_deviations all sbp_0",
      r1$SummaryPlotList$SBP_0
    )
    expect_lt(lobstr::obj_size(r1$SummaryPlotList$SBP_0), 15 * 1024 * 1024)

    expect_doppelganger2(
      "con_limit_deviations all item",
      r1$SummaryPlotList$ITEM_1_0
    )
    expect_lt(lobstr::obj_size(r1$SummaryPlotList$ITEM_1_0), 15 * 1024 * 1024)

    expect_doppelganger2(
      "con_limit_deviations all quest_dt",
      r1$SummaryPlotList$QUEST_DT_0
    )
    expect_lt(lobstr::obj_size(r1$SummaryPlotList$QUEST_DT_0), 15 * 1024 * 1024)

    expect_doppelganger2(
      "con_limit_deviations low sbp_0",
      r2$SummaryPlotList$SBP_0
    )
    expect_lt(lobstr::obj_size(r2$SummaryPlotList$SBP_0), 15 * 1024 * 1024)

    expect_doppelganger2(
      "con_limit_deviations low item",
      r2$SummaryPlotList$ITEM_1_0
    )
    expect_lt(lobstr::obj_size(r2$SummaryPlotList$ITEM_1_0), 15 * 1024 * 1024)

    expect_doppelganger2(
      "con_limit_deviations low quest_dt",
      r2$SummaryPlotList$QUEST_DT_0
    )
    expect_lt(lobstr::obj_size(r2$SummaryPlotList$QUEST_DT_0), 15 * 1024 * 1024)

    expect_doppelganger2(
      "con_limit_deviations upp sbp_0",
      r3$SummaryPlotList$SBP_0
    )
    expect_lt(lobstr::obj_size(r3$SummaryPlotList$SBP_0), 15 * 1024 * 1024)

    expect_doppelganger2(
      "con_limit_deviations upp item",
      r3$SummaryPlotList$ITEM_1_0
    )
    expect_lt(lobstr::obj_size(r3$SummaryPlotList$ITEM_1_0), 15 * 1024 * 1024)

    expect_doppelganger2(
      "con_limit_deviations upp quest_dt",
      r3$SummaryPlotList$QUEST_DT_0
    )
    expect_lt(lobstr::obj_size(r3$SummaryPlotList$QUEST_DT_0), 15 * 1024 * 1024)
  })
})

test_that("stratified item-level limit plots work", { # con_limit_deviations.R
  skip_on_cran()
  skip_if_not_installed("hms")
  skip_if_not_installed("vdiffr")

  female_dbp <- c(
    54.2, 60, round(stats::qnorm(ppoints(90), 76, 6), 1), 90, 99.8
  )
  male_dbp <- c(56.4, 60, round(stats::qnorm(ppoints(90), 80, 7), 1), 95, 104.2)
  unknown_dbp <- c(
    50.5, 55, round(stats::qnorm(ppoints(60), 78, 8), 1), 100, 102.1
  )
  study_data <- data.frame(
    DBP = c(female_dbp, male_dbp, unknown_dbp),
    SEX = rep(
      c("female", "male", "unknown"),
      c(length(female_dbp), length(male_dbp), length(unknown_dbp))
    )
  )
  meta_data <- prep_create_meta(
    VAR_NAMES = c("DBP", "SEX"),
    DATA_TYPE = c(DATA_TYPES$FLOAT, DATA_TYPES$STRING),
    SCALE_LEVEL = c(SCALE_LEVELS$RATIO, SCALE_LEVELS$NOMINAL),
    MISSING_LIST = SPLIT_CHAR,
    JUMP_LIST = SPLIT_CHAR,
    VARIABLE_ROLE = c(VARIABLE_ROLES$PRIMARY, VARIABLE_ROLES$PROCESS),
    HARD_LIMITS = c(
      paste(
        '[SEX] = "female": [60;90]',
        '[SEX] = "male": [60;95]',
        '[SEX] = "unknown": [55;100]',
        "[45;110]",
        sep = " | "
      ),
      NA_character_
    )
  )

  r <- con_limit_deviations(
    resp_vars = "DBP",
    study_data = study_data,
    meta_data = meta_data,
    label_col = VAR_NAMES,
    show_obs = FALSE
  )
  dbp_plot <- prep_realize_ggplot(r$SummaryPlotList$DBP)
  dbp_histogram_data <- dbp_plot$data
  stratum_limits <- list(
    '[SEX] = "female"' = c(60, 90),
    '[SEX] = "male"' = c(60, 95),
    '[SEX] = "unknown"' = c(55, 100)
  )
  spans_limit <- vapply(seq_len(nrow(dbp_histogram_data)), function(ii) {
    stratum <- as.character(dbp_histogram_data$stratum[[ii]])
    boundaries <- stratum_limits[[stratum]]
    if (is.null(boundaries)) {
      return(FALSE)
    }
    bin_range <- dbp_histogram_data$histogram_x[[ii]] +
      c(-0.5, 0.5) * dbp_histogram_data$bin_width[[ii]]
    any(
      bin_range[[1]] < boundaries - .Machine$double.eps &
        boundaries < bin_range[[2]] - .Machine$double.eps
    )
  }, FUN.VALUE = logical(1))
  expect_false(any(spans_limit))
  expect_equal(sum(dbp_histogram_data$histogram_y), nrow(study_data))
  expect_doppelganger2(
    "con_limit_deviations stratified dbp",
    dbp_plot
  )

  item_level <- prep_create_meta(
    VAR_NAMES = c("value", "group"),
    LABEL = c("Value", "Group"),
    DATA_TYPE = c(DATA_TYPES$DATETIME, DATA_TYPES$INTEGER),
    SCALE_LEVEL = c(SCALE_LEVELS$RATIO, SCALE_LEVELS$NOMINAL),
    MISSING_LIST = SPLIT_CHAR,
    JUMP_LIST = SPLIT_CHAR
  )
  limit <- util_parse_stratified_limit("[group] = 1: [0;Inf]")
  classified <- factor("within", levels = c("below", "within", "above"))
  make_limit_plot <- function(ds1, meta_data, is_datetime_var, is_time_var,
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

  datetime_plot <- make_limit_plot(
    ds1 = data.frame(
      value = as.POSIXct("2026-07-28 09:00:00", tz = "UTC"),
      group = 1L
    ),
    meta_data = item_level,
    is_datetime_var = TRUE,
    is_time_var = FALSE
  )
  time_item_level <- item_level
  time_item_level[[DATA_TYPE]][time_item_level[[VAR_NAMES]] == "value"] <-
    DATA_TYPES$TIME
  time_plot <- make_limit_plot(
    ds1 = data.frame(value = "09:00:00", group = 1L),
    meta_data = time_item_level,
    is_datetime_var = FALSE,
    is_time_var = TRUE
  )

  expect_doppelganger2(
    "con_limit_deviations stratified datetime",
    datetime_plot
  )
  expect_doppelganger2(
    "con_limit_deviations stratified time",
    time_plot
  )

  make_no_data_plot <- function(flip_mode = "noflip") {
    suppressMessages(util_create_stratified_limit_plot(
      rv = "value",
      ds1 = data.frame(value = c(NA_real_, NA_real_), group = c(1L, 2L)),
      meta_data = item_level,
      label_col = VAR_NAMES,
      limits = list(SOFT_LIMITS = limit),
      limit_results = list(SOFT_LIMITS = factor(
        c(NA, NA),
        levels = c("below", "within", "above")
      )),
      data_preparation = "VALUE | MISSING_NA",
      is_datetime_var = FALSE,
      is_time_var = FALSE,
      spec_txt = ggplot2::element_text(),
      ref_env = environment(),
      show_obs = TRUE
    ))
  }
  no_data_plot <- make_no_data_plot()
  expect_doppelganger2(
    "con_limit_deviations stratified no data message",
    no_data_plot
  )

  skip_on_covr()
  skip_if_not_installed("lobstr")
  expect_lt(lobstr::obj_size(dbp_plot), 15 * 1024 * 1024)
  expect_lt(lobstr::obj_size(datetime_plot), 15 * 1024 * 1024)
  expect_lt(lobstr::obj_size(time_plot), 15 * 1024 * 1024)
  expect_lt(lobstr::obj_size(no_data_plot), 15 * 1024 * 1024)
})

test_that("data type matrix and print.ReportSummaryTable plots work", {
  # int_datatype_matrix.R and print.ReportSummaryTable.R ----
  skip_on_cran()
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  skip_if_not_installed("lobstr")
  skip_if_not_installed("vdiffr")

  withr::local_options(dataquieR.CONDITIONS_LEVEL_TRHESHOLD = Inf)

  ({
    label_col <- "LABEL"

    r <- int_datatype_matrix(
      resp_vars = c("SBP_0", "ITEM_1_0", "QUEST_DT_0"),
      study_data = "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData", # nolint: line_length_linter.
      meta_data = "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data.RData", # nolint: line_length_linter.
      label_col = label_col,
      split_segments = TRUE
    )

    expect_doppelganger2(
      "int_datatype_matrix",
      r$SummaryPlot
    )
    expect_lt(lobstr::obj_size(r$SummaryPlot), 15 * 1024 * 1024)

    expect_doppelganger2(
      "int_datatype_matrix segment v50000",
      r$DataTypePlotList$PART_QUESTIONNAIRE
    )
    expect_lt(
      lobstr::obj_size(r$DataTypePlotList$PART_QUESTIONNAIRE),
      15 * 1024 * 1024
    )

    expect_doppelganger2(
      "int_datatype_matrix ReportSummaryTable",
      print(r$ReportSummaryTable,
        view = FALSE,
        dt = FALSE
      )
    )
    expect_lt(
      lobstr::obj_size(print(r$ReportSummaryTable,
          view = FALSE,
          dt = FALSE
        )),
      15 * 1024 * 1024
    )

    e <- structure(list(Variables = "CENTER_0", N = 2940L), # continuous vs not continuous; fully empty # nolint: line_length_linter.
      row.names = c(NA, -1L),
      class = c("ReportSummaryTable", "data.frame")
    )

    expect_doppelganger2(
      "Empty ReportSummaryTable",
      print(e,
        view = FALSE,
        dt = FALSE
      )
    )
    expect_lt(
      lobstr::obj_size(print(e,
          view = FALSE,
          dt = FALSE
        )),
      15 * 1024 * 1024
    )

    cont <- data.frame(
      Variables = letters[1:10L], N = 2940L,
      a = sin(1:10L),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )

    class(cont) <- c("ReportSummaryTable", "data.frame")

    expect_doppelganger2(
      "ReportSummaryTable cont",
      print(cont,
        view = FALSE,
        dt = FALSE
      )
    )
    expect_lt(
      lobstr::obj_size(print(cont,
          view = FALSE,
          dt = FALSE
        )),
      15 * 1024 * 1024
    )
  })
})

test_that("pro-applicability matrix plots work", { # pro_applicability_matrix.R ---- # nolint: line_length_linter.
  skip_on_cran()
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  skip_if_not_installed("lobstr")
  skip_if_not_installed("vdiffr")

  withr::local_options(dataquieR.CONDITIONS_LEVEL_TRHESHOLD = Inf)

  ({
    r <- pro_applicability_matrix(
      study_data = "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData", # nolint: line_length_linter.
      meta_data = "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data.RData", # nolint: line_length_linter.
      split_segments = FALSE,
      label_col = LABEL,
      meta_data_segment = "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx|segment_level", # nolint: line_length_linter.
      meta_data_dataframe = "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx|dataframe_level" # nolint: line_length_linter.
    )

    expect_doppelganger2(
      "pro_applicability_matrix",
      r$ApplicabilityPlot
    )
    expect_lt(lobstr::obj_size(r$ApplicabilityPlot), 15 * 1024 * 1024)

    expect_doppelganger2(
      "pro_applicability_matrix segment v50000",
      r$ApplicabilityPlotList$PART_QUESTIONNAIRE
    )
    expect_lt(
      lobstr::obj_size(r$ApplicabilityPlotList$PART_QUESTIONNAIRE),
      15 * 1024 * 1024
    )

    expect_doppelganger2(
      "pro_applicability_matrix ReportSummaryTable",
      print(r$ReportSummaryTable,
        view = FALSE,
        dt = FALSE
      )
    )
    expect_lt(lobstr::obj_size(print(r$ReportSummaryTable,
          view = FALSE,
          dt = FALSE
        )), 15 * 1024 * 1024)
  })
})

test_that("heatmap with 1 threshold plots work", { # util_heatmap_1th.R ----
  skip_on_cran()
  skip_if_not_installed("lobstr")
  skip_if_not_installed("vdiffr")


  withr::local_options(dataquieR.CONDITIONS_LEVEL_TRHESHOLD = Inf)

  ({
    x <- iris
    x$a <- as.integer(x$Species)
    x$Species2 <- rev(x$Species)
    x$b <- as.integer(x$Species2)
    x$c <- x$a + x$b
    x$str <- round(x$Sepal.Width, 0)

    p1 <- util_heatmap_1th(
      df = x,
      cat_vars = c("Species", "Species2"),
      values = "c",
      threshold = 0,
      invert = FALSE
    )

    p2 <- util_heatmap_1th(
      df = x,
      cat_vars = c("Species", "Species2"),
      strata = "str",
      values = "c",
      threshold = 0,
      invert = FALSE
    )

    p3 <- util_heatmap_1th(
      df = x,
      cat_vars = "Species",
      values = "a",
      threshold = 0,
      invert = FALSE
    )

    expect_doppelganger2(
      "heatmap with 2 cat-vars but w/o strata vars",
      p1
    )

    expect_doppelganger2(
      "heatmap with 2 cat-vars but w/ strata vars",
      p2
    )

    expect_doppelganger2(
      "heatmap with 1 cat-var",
      p3
    )
  })

})

test_that("prep_acc_distributions_with_ecdf patchwork converts to plotly", {
  skip_on_cran()
  skip_if_not_installed("colorspace")
  skip_if_not_installed("plotly")

  withr::local_options(dataquieR.lazy_plots = FALSE)
  study_data <- data.frame(
    value = c(seq(70, 114, length.out = 45), seq(84, 128, length.out = 45)),
    observer = rep(c("observer_a", "observer_b"), each = 45)
  )
  meta_data <- prep_create_meta(
    VAR_NAMES = c("value", "observer"),
    DATA_TYPE = c(DATA_TYPES$FLOAT, DATA_TYPES$STRING),
    MISSING_LIST = "",
    JUMP_LIST = "",
    HARD_LIMITS = "",
    GROUP_VAR_OBSERVER = c("", "observer"),
    VALUE_LABELS = "",
    VARIABLE_ROLE = VARIABLE_ROLES$PRIMARY
  )
  meta_data <- suppressWarnings(prep_scalelevel_from_data_and_metadata(
    study_data = study_data,
    meta_data = meta_data
  ))
  res <- suppressWarnings(prep_acc_distributions_with_ecdf(
    resp_vars = "value",
    group_vars = "observer",
    study_data = study_data,
    meta_data = meta_data,
    label_col = VAR_NAMES,
    n_group_max = 5,
    n_obs_per_group_min = 1
  ))

  py <- util_as_plotly_prep_acc_distributions_with_ecdf(res)

  expect_s3_class(py, "plotly")
  expect_gte(length(py$x$data), 2)
  plot_text <- unlist(lapply(py$x$data, `[[`, "text"),
    use.names = FALSE)
  expect_false(any(grepl("Internal error", plot_text, fixed = TRUE)))
})
