test_that(
  "con_limit_deviations covers local numeric and datetime hard limits",
  {
    skip_on_cran()

    study_data <- data.frame(
      score = c(-1, 0, 5, 11),
      when = as.POSIXct("2026-01-01", tz = "UTC") +
        c(-3600, 0, 3600, 86400)
    )
    meta_data <- data.frame(
      VAR_NAMES = c("score", "when"),
      LABEL = c("Score", "When"),
      DATA_TYPE = c(DATA_TYPES$INTEGER, DATA_TYPES$DATETIME),
      SCALE_LEVEL = c(SCALE_LEVELS$RATIO, SCALE_LEVELS$INTERVAL),
      VARIABLE_ROLE = VARIABLE_ROLES$PRIMARY,
      MISSING_LIST = "",
      JUMP_LIST = "",
      HARD_LIMITS = c("[0;10]", "[2026-01-01;2026-01-02]"),
      SOFT_LIMITS = NA_character_,
      DETECTION_LIMITS = NA_character_,
      stringsAsFactors = FALSE
    )

    result <- suppressMessages(suppressWarnings(
      con_limit_deviations(
        resp_vars = c("Score", "When"),
        study_data = study_data,
        meta_data = meta_data,
        label_col = LABEL,
        limits = HARD_LIMITS,
        return_flagged_study_data = TRUE,
        show_obs = FALSE
      )
    ))

    row_by_var <- split(result$SummaryTable, result$SummaryTable$Variables)
    expect_equal(as.numeric(row_by_var$Score$NUM_con_rvv_inum), 2)
    expect_equal(as.numeric(row_by_var$When$NUM_con_rvv_itdat), 1)
    expect_equal(as.character(result$FlaggedStudyData$Score_HARD_LIMITS[[1]]),
      "below")
    expect_equal(as.character(result$FlaggedStudyData$Score_HARD_LIMITS[[4]]),
      "above")
    expect_equal(as.character(result$FlaggedStudyData$When_HARD_LIMITS[[1]]),
      "below")
    expect_named(result, c(
      "FlaggedStudyData", "SummaryTable", "SummaryData",
      "ReportSummaryTable", "SummaryPlotList"
    ))
  }
)

test_that("con_limit_deviations works", {
  skip_on_cran() # slow, errors obvious

  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  require_english_locale_and_berlin_tz()
  prep_load_workbook_like_file("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx") # nolint: line_length_linter.
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
    my_value_limits <- con_limit_deviations(
      resp_vars = c(
        "AGE_0", "SBP_0",
        "DBP_0", "SEX_0",
        "QUEST_DT_0",
        "EDUCATION_1",
        "SMOKE_SHOP_0"
      ),
      label_col = "LABEL",
      study_data = study_data,
      meta_data = meta_data
    ),
    regexp = "No limits specified for SEX_0"
  )

  expect_equal(
    # Sum all values outside HARD_LIMITS in SummaryTable
    sum(unname(rowSums(my_value_limits$SummaryTable[
      order(my_value_limits$SummaryTable$Variables),
      c("NUM_con_rvv_inum", "NUM_con_rvv_itdat")
    ], na.rm = TRUE))),
    sum(as.numeric( # Sum all values outside HARD_LIMITS in SummaryData
      unname(
        vapply(
          X = unlist(
            as.vector(
              my_value_limits$SummaryData[
                my_value_limits$SummaryData$Limits == "HARD_LIMITS",
                c(
                  "Below limits N (%)",
                  "Above limits N (%)"
                )
              ]
            )
          ),
          FUN = function(x) {
            gsub("\\s*\\([^\\)]+\\)", "", x)
          },
          FUN.VALUE = character(1)
        )
      )
    ))
  )
})

test_that("con_limit_deviations and timevars with < 20 integer sec-values ok", {
  skip_on_cran() # slow, errors obvious

  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  require_english_locale_and_berlin_tz()
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


  vn <- subset(meta_data, LABEL == "QUEST_DT_0", VAR_NAMES, TRUE)

  sd0 <- study_data[
    study_data[[vn]] >= as.POSIXct("2018-05-01") &
      study_data[[vn]] < as.POSIXct("2018-05-15"), , FALSE
  ]

  sd0[[vn]] <- round.POSIXt(sd0[[vn]], "days")

  expect_lt(length(unique(as.character(sd0[[vn]]))), 20)

  set.seed(1843) # randomly scattered points should stay in their position for testing # nolint: line_length_linter.
  my_value_limits <- con_limit_deviations(
    resp_vars = "QUEST_DT_0",
    label_col = "LABEL",
    study_data = sd0,
    meta_data = meta_data,
    limits = "HARD_LIMITS"
  )


  skip_on_cran()
  skip_if_not_installed("vdiffr")
  suppressWarnings(
    expect_doppelganger2(
      "con_limit_deviations QUEST_DT_0",
      my_value_limits$SummaryPlotList$QUEST_DT_0
    )
  )
})

test_that("con_limit_deviations works w/o resp_vars", {
  skip_on_cran() # slow, errors obvious
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  require_english_locale_and_berlin_tz()
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


  resp_vars <- c(
    "AGE_0", "SBP_0",
    "DBP_0", "SEX_0",
    "QUEST_DT_0",
    "EDUCATION_1",
    "SMOKE_SHOP_0"
  )

  resp_vars_names <- prep_map_labels(resp_vars,
    meta_data,
    from = LABEL,
    to = VAR_NAMES
  )

  sd0 <- study_data[, resp_vars_names, FALSE]
  md0 <- meta_data[meta_data$VAR_NAMES %in% resp_vars_names, , FALSE]
  md0$JUMP_LIST <- SPLIT_CHAR # signals unambiguously no codes intentionally


  expect_message2(
    my_value_limits <- con_limit_deviations(
      label_col = "LABEL",
      study_data = sd0,
      meta_data = md0,
      limits = "HARD_LIMITS",
      return_flagged_study_data = TRUE
    ),
    regexp = sprintf(
      "(%s|%s)",
      paste(
        "All variables for which limits are specified",
        "in the metadata are used."
      ),
      paste(
        "Not all entries in.+KEY_STUDY_SEGMENT.+are",
        "found in.+VAR_NAMES.+"
      )
    ),
    perl = TRUE
  )

  expect_equal(sum(rowSums(
    my_value_limits$SummaryTable[
      ,
      grepl("^FLG\\_.*$", colnames(my_value_limits$SummaryTable))
    ],
    na.rm = TRUE
  ) > 0), 4)
  expect_true(all(my_value_limits$FlaggedStudyData$QUEST_DT_0[which(as.logical(
    my_value_limits$FlaggedStudyData$QUEST_DT_0_below_HARD
  ))] <
    as.POSIXct("2018-01-01")))
  expect_true(all(my_value_limits$FlaggedStudyData$QUEST_DT_0[which(!as.logical(
    my_value_limits$FlaggedStudyData$QUEST_DT_0_below_HARD
  ))] >=
    as.POSIXct("2018-01-01")))
})

test_that("con_limit_deviations handles errors", {
  skip_on_cran() # slow, errors obvious
  require_english_locale_and_berlin_tz()
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


  resp_vars <- c(
    "AGE_0", "SBP_0",
    "DBP_0", "SEX_0",
    "QUEST_DT_0",
    "EDUCATION_1",
    "SMOKE_SHOP_0"
  )

  resp_vars_names <- prep_map_labels(resp_vars,
    meta_data,
    from = LABEL,
    to = VAR_NAMES
  )

  sd0 <- study_data[, resp_vars_names, FALSE]
  md0 <- meta_data[meta_data$VAR_NAMES %in% resp_vars_names, , FALSE]
  md0$JUMP_LIST <- SPLIT_CHAR # signals unambiguously no codes intentionally
  # no limits:
  md0$HARD_LIMITS <- NA
  md0$DETECTION_LIMITS <- NA
  md0$SOFT_LIMITS <- NA

  expect_message2(
    expect_error(
      my_value_limits <- con_limit_deviations(
        resp_vars = resp_vars,
        label_col = "LABEL",
        study_data = sd0,
        meta_data = md0
      ),
      regexp = paste(
        "The limit deviation check cannot be performed",
        "without a metadata column specifying suitable limits"
      )
    ),
    regexp = "Not all entries in .+KEY_STUDY_SEGMENT.+0.+7.*"
  )

  resp_vars <- meta_data$LABEL
  resp_vars_names <- prep_map_labels(resp_vars,
    meta_data,
    from = LABEL,
    to = VAR_NAMES
  )

  sd0 <- study_data[, resp_vars_names, FALSE]
  md0 <- meta_data[meta_data$VAR_NAMES %in% resp_vars_names, , FALSE]
  md0$JUMP_LIST <- SPLIT_CHAR # signals unambiguously no codes intentionally
  md0$HARD_LIMITS[md0$DATA_TYPE == DATA_TYPES$STRING] <-
    "[1; 5]" # Some unsuitable limits

  expect_error(
    my_value_limits <- suppressWarnings(
      con_limit_deviations(
        resp_vars = md0$LABEL, # missing, only resp_vars with suitable data type are picked by the function now. # nolint: line_length_linter.
        label_col = "LABEL",
        study_data = sd0,
        meta_data = md0
      )
    ),
    regexp = "Variable .+PSEUDO_ID.+ .+string.+ does not have an allowed type .+integer.+float.+datetime.+", # nolint: line_length_linter.
    perl = TRUE
  )

  sd0 <- study_data
  sd0[[prep_map_labels("BSG_0", meta_data, VAR_NAMES, LABEL)]] <- NA
  expect_error(
    my_value_limits <- con_limit_deviations(
      resp_vars = "BSG_0",
      label_col = "LABEL",
      study_data = sd0,
      meta_data = meta_data,
      limits = "HARD_LIMITS"
    ),
    regexp = "Variable .+BSG_0.+ .+resp_vars.+ has only NA observations"
  )
})

test_that("con_limit_deviations and values < 0", {
  skip_on_cran() # slow, errors obvious

  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  require_english_locale_and_berlin_tz()

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


  sd0 <- study_data
  x <- sd0[[prep_map_labels("EDUCATION_0", meta_data, VAR_NAMES, LABEL)]]

  x[!is.na(x) & x < 10] <- 0 - x[!is.na(x) & x < 10]

  sd0[[prep_map_labels("EDUCATION_0", meta_data, VAR_NAMES, LABEL)]] <- x

  md0 <- meta_data
  md0[md0$LABEL == "EDUCATION_0", HARD_LIMITS] <-
    "[-6;0]"
  md0[md0$LABEL == "EDUCATION_0", SCALE_LEVEL] <- SCALE_LEVELS$INTERVAL
  set.seed(2012) # randomly scattered points should stay in their position for testing # nolint: line_length_linter.
  my_value_limits <- con_limit_deviations(
    resp_vars = "EDUCATION_0",
    label_col = "LABEL",
    study_data = sd0,
    meta_data = md0,
    limits = "HARD_LIMITS"
  )

  expect_type(my_value_limits, "list")

  skip_on_cran()
  skip_if_not_installed("vdiffr")
  suppressWarnings(
    expect_doppelganger2(
      "con_limit_deviations for reverted",
      my_value_limits$SummaryPlotList$EDUCATION_0
    )
  )
})

test_that("con_limit_deviations and values < 0 with max -1", {
  skip_on_cran() # slow, errors obvious
  require_english_locale_and_berlin_tz()
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


  sd0 <- study_data
  x <- sd0[[prep_map_labels("EDUCATION_0", meta_data, VAR_NAMES, LABEL)]]

  x[!is.na(x) & x < 10] <- -1 - x[!is.na(x) & x < 10]

  sd0[[prep_map_labels("EDUCATION_0", meta_data, VAR_NAMES, LABEL)]] <- x

  md0 <- meta_data
  md0[md0$LABEL == "EDUCATION_0", HARD_LIMITS] <-
    "[-7;-1]"
  md0[md0$LABEL == "EDUCATION_0", SCALE_LEVEL] <- SCALE_LEVELS$INTERVAL

  set.seed(2024) # randomly scattered points should stay in their position for testing # nolint: line_length_linter.
  my_value_limits <- con_limit_deviations(
    resp_vars = "EDUCATION_0",
    label_col = "LABEL",
    study_data = sd0,
    meta_data = md0,
    limits = "HARD_LIMITS"
  )

  expect_type(my_value_limits, "list")

  skip_on_cran()
  skip_if_not_installed("vdiffr")
  suppressWarnings(
    expect_doppelganger2(
      "con_limit_deviations reverse w/ -1 ==> 0 max",
      my_value_limits$SummaryPlotList$EDUCATION_0
    )
  )
})

test_that("con_limit_deviations with no lower limit", {
  skip_on_cran() # slow, errors obvious
  require_english_locale_and_berlin_tz()
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


  sd0 <- study_data
  x <- sd0[[prep_map_labels("EDUCATION_0", meta_data, VAR_NAMES, LABEL)]]

  x[2:5] <- -2

  sd0[[prep_map_labels("EDUCATION_0", meta_data, VAR_NAMES, LABEL)]] <- x
  md0 <- meta_data
  md0[md0$LABEL == "EDUCATION_0", SCALE_LEVEL] <- SCALE_LEVELS$INTERVAL
  expect_silent(
    my_value_limits_1 <- con_limit_deviations(
      resp_vars = "EDUCATION_0",
      label_col = "LABEL",
      study_data = sd0,
      meta_data = md0,
      limits = "HARD_LIMITS"
    )
  )

  md0[md0$LABEL == "EDUCATION_0", HARD_LIMITS] <- "[;6]"

  my_value_limits_2 <- con_limit_deviations(
    resp_vars = "EDUCATION_0",
    label_col = "LABEL",
    study_data = sd0,
    meta_data = md0,
    limits = "HARD_LIMITS"
  )

  expect_equal(my_value_limits_1$SummaryTable$NUM_con_rvv_inum, 4,
    ignore_attr = TRUE
  )
  expect_equal(my_value_limits_2$SummaryTable$NUM_con_rvv_inum, 0,
    ignore_attr = TRUE
  )

  expect_equal(my_value_limits_1$SummaryData$Number[
    which(my_value_limits_1$SummaryData$Section == "above")
  ], my_value_limits_2$SummaryData$Number[
    which(my_value_limits_2$SummaryData$Section == "above")
  ])
})

test_that("con_limit_deviations with constant data", {
  skip_on_cran() # slow, errors obvious
  require_english_locale_and_berlin_tz()
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


  sd0 <- study_data
  sd0[[prep_map_labels("CRP_0", meta_data, VAR_NAMES, LABEL)]] <- 1.1

  md0 <- meta_data

  set.seed(12) # randomly scattered points should stay in their position for testing # nolint: line_length_linter.
  expect_silent(
    my_value_limits <- con_limit_deviations(
      resp_vars = "CRP_0",
      label_col = "LABEL",
      study_data = sd0,
      meta_data = md0,
      limits = "HARD_LIMITS"
    )
  )

  expect_type(my_value_limits, "list")

  skip_on_cran()
  skip_if_not_installed("vdiffr")
  suppressWarnings(
    expect_doppelganger2(
      "con_limit_deviations 4 hists 1 val",
      my_value_limits$SummaryPlotList$CRP_0
    )
  )

  md0[which(md0$LABEL == "CRP_0"), HARD_LIMITS] <- NA
  md0[which(md0$LABEL == "CRP_0"), DETECTION_LIMITS] <- NA
  md0[which(md0$LABEL == "CRP_0"), SOFT_LIMITS] <- NA
  md0[["NEW_LIMITS_ONE"]] <- NA
  md0[which(md0$LABEL == "CRP_0"), "NEW_LIMITS_ONE"] <- "(-Inf; 1.1)"
  md0[["NEW_LIMITS_TWO"]] <- NA
  md0[which(md0$LABEL == "CRP_0"), "NEW_LIMITS_TWO"] <- "(1.1; Inf)"

  expect_silent(
    my_value_limits <- con_limit_deviations(
      resp_vars = "CRP_0",
      label_col = "LABEL",
      study_data = sd0,
      meta_data = md0,
      limits = c("NEW_LIMITS_ONE", "NEW_LIMITS_TWO")
    )
  )
})

test_that("con_limit_deviations does not crash with missing codes", {
  skip_on_cran() # slow, errors obvious
  require_english_locale_and_berlin_tz()
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


  sd0 <- study_data
  x <- sd0[[prep_map_labels("SBP_0", meta_data, VAR_NAMES, LABEL)]]

  x[[5]] <- 99999 # add a "forgotten missing code" to the data, which causes
  # too many breaks, that should be removed to avoid a crash

  sd0[[prep_map_labels("SBP_0", meta_data, VAR_NAMES, LABEL)]] <- x
  md0 <- meta_data

  expect_silent(
    my_value_limits <- con_limit_deviations(
      resp_vars = "SBP_0",
      label_col = "LABEL",
      study_data = sd0,
      meta_data = md0,
      limits = "HARD_LIMITS"
    )
  )

  expect_type(my_value_limits, "list")
  expect_equal(
    as.numeric(
      gsub(
        "\\s*\\([^\\)]+\\)",
        "",
        unlist(my_value_limits$SummaryData[
          my_value_limits$SummaryData$Limits ==
            "HARD_LIMITS",
          "Above limits N (%)"
        ])
      )
    ),
    1
  )
  expect_equal(
    as.numeric(
      gsub(
        "\\s*\\([^\\)]+\\)",
        "",
        unlist(my_value_limits$SummaryData[my_value_limits$SummaryData$Limits ==
              "HARD_LIMITS", "Below limits N (%)"])
      )
    ),
    0
  )

  sd0 <- study_data
  x <- sd0[[prep_map_labels("SBP_0", meta_data, VAR_NAMES, LABEL)]]

  x[[5]] <- 100000 # add a less obvious "forgotten missing code" to the data,
  # which could cause too many breaks, that should be caught to avoid a crash

  sd0[[prep_map_labels("SBP_0", meta_data, VAR_NAMES, LABEL)]] <- x

  set.seed(20) # randomly scattered points should stay in their position for testing # nolint: line_length_linter.
  expect_silent(
    my_value_limits <- con_limit_deviations(
      resp_vars = "SBP_0",
      label_col = "LABEL",
      study_data = sd0,
      meta_data = md0,
      limits = "HARD_LIMITS"
    )
  )

  expect_type(my_value_limits, "list")
  expect_equal(
    as.numeric(gsub(
      "\\s*\\([^\\)]+\\)",
      "",
      unlist(my_value_limits$SummaryData[
        my_value_limits$SummaryData$Limits ==
          "HARD_LIMITS",
        "Above limits N (%)"
      ])
    )),
    1
  )
  expect_equal(
    as.numeric(gsub(
      "\\s*\\([^\\)]+\\)",
      "",
      unlist(my_value_limits$SummaryData[
        my_value_limits$SummaryData$Limits ==
          "HARD_LIMITS",
        "Below limits N (%)"
      ])
    )),
    0
  )
  expect_lt(nrow(my_value_limits$SummaryPlotList[[1]]$data), 1000)

  skip_on_cran()
  skip_if_not_installed("vdiffr")
  suppressWarnings(
    expect_doppelganger2(
      "con_limit_deviations histgrms + misscds",
      my_value_limits$SummaryPlotList$SBP_0
    )
  )
})

test_that("con_limit_deviations does not crash with strong outliers", {
  skip_on_cran() # slow, errors obvious
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  # issue 116
  require_english_locale_and_berlin_tz()

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
  md0[["HARD_LIMITS"]][which(md0$LABEL == "BSG_0")] <- "[0;5000]"
  sd0 <- study_data
  x <- sd0[[prep_map_labels("BSG_0", meta_data, VAR_NAMES, LABEL)]]
  x[c(2, 8, 92)] <- c(26027, 28097, 4031012019)
  sd0[[prep_map_labels("BSG_0", meta_data, VAR_NAMES, LABEL)]] <- x
  expect_silent(
    my_value_limits <- con_limit_deviations(
      resp_vars = "BSG_0",
      label_col = "LABEL",
      study_data = sd0,
      meta_data = md0,
      limits = "HARD_LIMITS"
    )
  )

  expect_type(my_value_limits, "list")
  expect_equal(my_value_limits$SummaryTable$NUM_con_rvv_inum, 3,
    ignore_attr = TRUE
  )
})

test_that("con_limits_deviations does not crash with NAs in datetime vars", {
  skip_on_cran() # slow, errors obvious
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  # issue 106
  require_english_locale_and_berlin_tz()

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


  # QUEST_DT_0 already contains NA values in the fixture data.
  expect_silent(
    my_value_limits <- con_limit_deviations(
      resp_vars = "QUEST_DT_0",
      label_col = "LABEL",
      study_data = study_data,
      meta_data = meta_data,
      limits = "HARD_LIMITS"
    )
  )

  expect_type(my_value_limits, "list")
  expect_equal(my_value_limits$SummaryTable$NUM_con_rvv_itdat, 9,
    ignore_attr = TRUE
  )
})

test_that("con_limit_deviations works with no values within limits", {
  skip_on_cran() # slow, errors obvious
  require_english_locale_and_berlin_tz()
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
  md0[["HARD_LIMITS"]][which(md0$LABEL == "BSG_0")] <- "[51.5;51.8]"
  sd0 <- study_data

  set.seed(24) # randomly scattered points should stay in their position for testing # nolint: line_length_linter.
  expect_silent(
    my_value_limits <- con_limit_deviations(
      resp_vars = "BSG_0",
      label_col = "LABEL",
      study_data = sd0,
      meta_data = md0,
      limits = "HARD_LIMITS"
    )
  )

  expect_type(my_value_limits, "list")
  skip_on_cran()
  skip_if_not_installed("vdiffr")
  suppressWarnings(
    expect_doppelganger2(
      "con_limit_deviations_nothing_within",
      my_value_limits$SummaryPlotList$BSG_0
    )
  )
})

test_that("con_limit_deviations works with wrong datetime limits", {
  skip_on_cran() # slow, errors obvious
  require_english_locale_and_berlin_tz()
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
  md0[["HARD_LIMITS"]][which(md0$LABEL == "QUEST_DT_0")] <- "[51.5;51.8]"
  sd0 <- study_data

  expect_warning(
    my_value_limits <- con_limit_deviations(
      resp_vars = "QUEST_DT_0",
      label_col = "LABEL",
      study_data = sd0,
      meta_data = md0,
      limits = "HARD_LIMITS"
    ),
    regexp = "Invalid limits detected"
  )

  md0[["HARD_LIMITS"]][which(md0$LABEL == "QUEST_DT_0")] <-
    "[2018-10-01; 2017-10-01]"
  suppressWarnings(suppressMessages(expect_warning(expect_error(
    my_value_limits <- con_limit_deviations(
      resp_vars = "QUEST_DT_0",
      label_col = "LABEL",
      study_data = sd0,
      meta_data = md0,
      limits = "HARD_LIMITS"
    )
  ))))

  md0[["HARD_LIMITS"]][which(md0$LABEL == "QUEST_DT_0")] <-
    "[0; test]"
  suppressWarnings(suppressMessages(expect_warning(expect_error(
    my_value_limits <- con_limit_deviations(
      resp_vars = "QUEST_DT_0",
      label_col = "LABEL",
      study_data = sd0,
      meta_data = md0,
      limits = "HARD_LIMITS"
    )
  ))))
})

test_that("con_limit_deviations chooses suitable breaks for integer variables", { # nolint: line_length_linter.
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

  md0 <- meta_data[meta_data[[LABEL]] %in% c("PSEUDO_ID", "AGE_0"), ]
  md0$KEY_STUDY_SEGMENT <- ""
  md0[["HARD_LIMITS"]][which(md0$LABEL == "AGE_0")] <- "[1; 110]"

  set.seed(2204)
  sd0 <- data.frame(
    "v00001" = 1:(6 * 10^4 + 40),
    "v00003" = c(
      rep(0, 40),
      round(abs(rnorm(mean = 53, sd = 12, n = 6 * 10^4)))
    )
  )
  suppressMessages(suppressWarnings(
    cc_int <- con_limit_deviations(
      resp_vars = "AGE_0", study_data = sd0,
      meta_data = md0
    )
  ))
  plotdata <- ggplot2::ggplot_build(cc_int$SummaryPlotList[[1]])$data
  plotdata_bind <- util_rbind(data_frames_list = plotdata)
  plotdata_bind <- plotdata_bind[, c("xmin", "xmax")]
  plotdata_bind <- plotdata_bind[complete.cases(plotdata_bind), ]
  bin_widths <- plotdata_bind$xmax - plotdata_bind$xmin
  expect_true(all(util_is_integer(unique(round(bin_widths, 6)))))
})

test_that("con_limit_deviations complex limits alone", {
  skip_on_cran() # slow, errors obvious
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  meta_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data.RData") # nolint: line_length_linter.
  study_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData", keep_types = TRUE) # nolint: line_length_linter.

  cil <- data.frame(
    VARIABLE_LIST = "SBP_0",
    HARD_LIMITS = paste0('[SEX_0] == "" or ([SEX_0] =="males" or ([SEX_0] =="females" and [SBP_0] > 100))') # nolint: line_length_linter.
  )

  my_value_limits <- con_limit_deviations(
    resp_vars = "SBP_0",
    study_data = study_data,
    meta_data = meta_data,
    meta_data_cross_item = cil
  )


  expect_equal(
    my_value_limits$SummaryData$`All outside limits N (%)`, "2 (0.07)",
    ignore_attr = TRUE
  )
})


test_that("con_limit_deviations complex limits alone", {
  skip_on_cran() # slow, errors obvious
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  meta_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data.RData") # nolint: line_length_linter.
  study_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData", keep_types = TRUE) # nolint: line_length_linter.

  cil <- data.frame(
    VARIABLE_LIST = "DBP_0",
    HARD_LIMITS = paste0("[DBP_0] > 55")
  )

  my_value_limits <- con_limit_deviations(
    resp_vars = "DBP_0",
    label_col = LABEL,
    study_data = study_data,
    meta_data = meta_data,
    meta_data_cross_item = cil
  )


  expect_equal(
    my_value_limits$SummaryData$`All outside limits N (%)`, "3 (0.1)",
    ignore_attr = TRUE
  )
})

test_that("con_limit_deviations evaluates a small cross-item hard limit", {
  skip_on_cran()

  study_data <- data.frame(v1 = c(0L, 5L, 10L))
  meta_data <- data.frame(
    VAR_NAMES = "v1",
    LABEL = "v1",
    DATA_TYPE = DATA_TYPES$INTEGER,
    SCALE_LEVEL = SCALE_LEVELS$RATIO,
    MISSING_LIST = SPLIT_CHAR,
    JUMP_LIST = SPLIT_CHAR,
    HARD_LIMITS = "[-Inf;Inf]",
    DETECTION_LIMITS = NA_character_,
    SOFT_LIMITS = NA_character_,
    stringsAsFactors = FALSE
  )
  meta_data_cross_item <- data.frame(
    VARIABLE_LIST = "v1",
    HARD_LIMITS = "[v1] >= 1",
    stringsAsFactors = FALSE
  )

  result <- suppressMessages(suppressWarnings(con_limit_deviations(
    resp_vars = "v1",
    study_data = study_data,
    meta_data = meta_data,
    label_col = LABEL,
    meta_data_cross_item = meta_data_cross_item
  )))

  expect_equal(result$SummaryTable$NUM_con_rvv_inum, 1, ignore_attr = TRUE)
  expect_equal(result$SummaryTable$PCT_con_rvv_inum, 33.33, ignore_attr = TRUE)
  expect_equal(
    result$SummaryData$`All outside limits N (%)`,
    "1 (33.33)",
    ignore_attr = TRUE
  )
})

test_that("con_limit_deviations evaluates a small cross-item soft limit", {
  skip_on_cran()

  study_data <- data.frame(v1 = c(0L, 5L, 10L))
  meta_data <- data.frame(
    VAR_NAMES = "v1",
    LABEL = "v1",
    DATA_TYPE = DATA_TYPES$INTEGER,
    SCALE_LEVEL = SCALE_LEVELS$RATIO,
    MISSING_LIST = SPLIT_CHAR,
    JUMP_LIST = SPLIT_CHAR,
    HARD_LIMITS = NA_character_,
    DETECTION_LIMITS = NA_character_,
    SOFT_LIMITS = "[-Inf;Inf]",
    stringsAsFactors = FALSE
  )
  meta_data_cross_item <- data.frame(
    VARIABLE_LIST = "v1",
    SOFT_LIMITS = "[v1] >= 1",
    stringsAsFactors = FALSE
  )

  result <- suppressMessages(suppressWarnings(con_limit_deviations(
    resp_vars = "v1",
    study_data = study_data,
    meta_data = meta_data,
    label_col = LABEL,
    meta_data_cross_item = meta_data_cross_item
  )))

  expect_equal(result$SummaryTable$NUM_con_rvv_unum, 1, ignore_attr = TRUE)
  expect_equal(result$SummaryTable$PCT_con_rvv_unum, 33.33, ignore_attr = TRUE)
  expect_true(is.na(result$SummaryTable$NUM_con_rvv_inum))
  expect_equal(result$SummaryData$Limits, "SOFT_LIMITS", ignore_attr = TRUE)
})

test_that("con_limit_deviations evaluates a datetime cross-item hard limit", {
  skip_on_cran()

  study_data <- data.frame(
    vdt = as.POSIXct(c("2019-12-31", "2020-01-02", "2020-01-03"), tz = "UTC")
  )
  meta_data <- data.frame(
    VAR_NAMES = "vdt",
    LABEL = "vdt",
    DATA_TYPE = DATA_TYPES$DATETIME,
    SCALE_LEVEL = SCALE_LEVELS$INTERVAL,
    MISSING_LIST = SPLIT_CHAR,
    JUMP_LIST = SPLIT_CHAR,
    HARD_LIMITS = "[-Inf;Inf]",
    DETECTION_LIMITS = NA_character_,
    SOFT_LIMITS = NA_character_,
    stringsAsFactors = FALSE
  )
  meta_data_cross_item <- data.frame(
    VARIABLE_LIST = "vdt",
    HARD_LIMITS = "[vdt] >= [2020-01-01]",
    stringsAsFactors = FALSE
  )

  result <- suppressMessages(suppressWarnings(con_limit_deviations(
    resp_vars = "vdt",
    study_data = study_data,
    meta_data = meta_data,
    label_col = LABEL,
    meta_data_cross_item = meta_data_cross_item
  )))

  expect_equal(result$SummaryTable$NUM_con_rvv_itdat, 1, ignore_attr = TRUE)
  expect_equal(result$SummaryTable$PCT_con_rvv_itdat, 33.33, ignore_attr = TRUE)
  expect_equal(
    result$SummaryData$`All outside limits N (%)`,
    "1 (33.33)",
    ignore_attr = TRUE
  )
})

test_that("con_limit_deviations merges cross-item hard and soft limits", {
  skip_on_cran()

  study_data <- data.frame(v1 = c(0L, 5L, 10L))
  meta_data <- data.frame(
    VAR_NAMES = "v1",
    LABEL = "v1",
    DATA_TYPE = DATA_TYPES$INTEGER,
    SCALE_LEVEL = SCALE_LEVELS$RATIO,
    MISSING_LIST = SPLIT_CHAR,
    JUMP_LIST = SPLIT_CHAR,
    HARD_LIMITS = "[-Inf;Inf]",
    DETECTION_LIMITS = NA_character_,
    SOFT_LIMITS = "[-Inf;Inf]",
    stringsAsFactors = FALSE
  )
  meta_data_cross_item <- data.frame(
    VARIABLE_LIST = c("v1", "v1"),
    HARD_LIMITS = c("[v1] >= 1", NA),
    SOFT_LIMITS = c(NA, "[v1] <= 5"),
    stringsAsFactors = FALSE
  )

  result <- suppressMessages(suppressWarnings(con_limit_deviations(
    resp_vars = "v1",
    study_data = study_data,
    meta_data = meta_data,
    label_col = LABEL,
    meta_data_cross_item = meta_data_cross_item
  )))

  expect_equal(result$SummaryTable$NUM_con_rvv_inum, 1, ignore_attr = TRUE)
  expect_equal(result$SummaryTable$NUM_con_rvv_unum, 1, ignore_attr = TRUE)
  expect_equal(
    result$SummaryData$Limits,
    c("HARD_LIMITS", "SOFT_LIMITS"),
    ignore_attr = TRUE
  )
})

test_that("con_limit_deviations evaluates cross-item detection limits", {
  skip_on_cran()

  numeric_data <- data.frame(v1 = c(0L, 5L, 10L))
  numeric_metadata <- data.frame(
    VAR_NAMES = "v1",
    LABEL = "v1",
    DATA_TYPE = DATA_TYPES$INTEGER,
    SCALE_LEVEL = SCALE_LEVELS$RATIO,
    MISSING_LIST = SPLIT_CHAR,
    JUMP_LIST = SPLIT_CHAR,
    HARD_LIMITS = NA_character_,
    DETECTION_LIMITS = "[-Inf;Inf]",
    SOFT_LIMITS = NA_character_,
    stringsAsFactors = FALSE
  )
  numeric_cross_item <- data.frame(
    VARIABLE_LIST = "v1",
    DETECTION_LIMITS = "[v1] >= 1",
    stringsAsFactors = FALSE
  )

  numeric_result <- suppressMessages(suppressWarnings(con_limit_deviations(
    resp_vars = "v1",
    study_data = numeric_data,
    meta_data = numeric_metadata,
    label_col = LABEL,
    meta_data_cross_item = numeric_cross_item
  )))

  expect_equal(
    numeric_result$SummaryTable$NUM_con_rvv_unum,
    1,
    ignore_attr = TRUE
  )
  expect_equal(
    numeric_result$SummaryData$Limits,
    "DETECTION_LIMITS",
    ignore_attr = TRUE
  )
  expect_equal(
    numeric_result$SummaryData$`All outside limits N (%)`,
    "1 (33.33)",
    ignore_attr = TRUE
  )

  datetime_data <- data.frame(
    vdt = as.POSIXct(
      c("2019-12-31", "2020-01-02", "2020-01-03"),
      tz = "UTC"
    )
  )
  datetime_metadata <- data.frame(
    VAR_NAMES = "vdt",
    LABEL = "vdt",
    DATA_TYPE = DATA_TYPES$DATETIME,
    SCALE_LEVEL = SCALE_LEVELS$INTERVAL,
    MISSING_LIST = SPLIT_CHAR,
    JUMP_LIST = SPLIT_CHAR,
    HARD_LIMITS = NA_character_,
    DETECTION_LIMITS = "[-Inf;Inf]",
    SOFT_LIMITS = NA_character_,
    stringsAsFactors = FALSE
  )
  datetime_cross_item <- data.frame(
    VARIABLE_LIST = "vdt",
    DETECTION_LIMITS = "[vdt] >= [2020-01-01]",
    stringsAsFactors = FALSE
  )

  datetime_result <- suppressMessages(suppressWarnings(con_limit_deviations(
    resp_vars = "vdt",
    study_data = datetime_data,
    meta_data = datetime_metadata,
    label_col = LABEL,
    meta_data_cross_item = datetime_cross_item
  )))

  expect_equal(
    datetime_result$SummaryTable$NUM_con_rvv_utdat,
    1,
    ignore_attr = TRUE
  )
  expect_equal(
    datetime_result$SummaryData$Limits,
    "DETECTION_LIMITS",
    ignore_attr = TRUE
  )
  expect_equal(
    datetime_result$SummaryData$`All outside limits N (%)`,
    "1 (33.33)",
    ignore_attr = TRUE
  )
})

test_that("cross-item limit metric mapping is table-driven", {
  skip_on_cran()

  contradiction_table <- data.frame(
    VARIABLE_LIST = c("v_num", "v_dt"),
    NUM_con_con_contc = c(1, 2),
    PCT_con_con_contc = c(10, 20),
    NUM_con_con_contu = c(3, 4),
    PCT_con_con_contu = c(30, 40),
    NUM_con_con = c(5, 6),
    PCT_con_con = c(50, 60),
    data_type = c(DATA_TYPES$INTEGER, DATA_TYPES$DATETIME),
    stringsAsFactors = FALSE
  )

  hard <- util_con_limit_deviations_cross_item_metrics(
    contradiction_table,
    HARD_LIMITS
  )
  soft <- util_con_limit_deviations_cross_item_metrics(
    contradiction_table,
    SOFT_LIMITS
  )

  expect_equal(hard$NUM_con_rvv_inum, c(NA, 1), ignore_attr = TRUE)
  expect_equal(hard$NUM_con_rvv_itdat, c(2, NA), ignore_attr = TRUE)
  expect_equal(soft$NUM_con_rvv_unum, c(NA, 3), ignore_attr = TRUE)
  expect_equal(soft$NUM_con_rvv_utdat, c(4, NA), ignore_attr = TRUE)
  expect_false("data_type" %in% names(hard))
  expect_equal(hard$limits, rep(HARD_LIMITS, 2), ignore_attr = TRUE)
  expect_equal(soft$limits, rep(SOFT_LIMITS, 2), ignore_attr = TRUE)
})

test_that("Time-only variables limits", {
  skip_on_cran() # slow
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  study_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData", # nolint: line_length_linter.
    keep_types = TRUE
  )

  prep_load_workbook_like_file("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx") # nolint: line_length_linter.
  meta_data <- prep_get_data_frame("item_level")

  set.seed(12345)
  day_course <- unlist(lapply(
    0:23,
    function(h) {
      list(
        hms::hms(
          hours = h,
          minutes = 3
        ),
        hms::hms(
          hours = h,
          minutes = 13
        ),
        hms::hms(
          hours = h,
          minutes = 24
        ),
        hms::hms(
          hours = h,
          minutes = 30
        ),
        hms::hms(
          hours = h,
          minutes = 50
        )
      )
    }
  ), recursive = FALSE)
  probs <-
    rep(c(.7, .2, .05, .04, 0.01), 24)
  times <- sample(
    x = day_course,
    prob = probs,
    size = nrow(study_data),
    replace = TRUE
  )
  times[sample(seq_along(times),
      size = length(times) %/% 100,
      replace = TRUE
    )] <-
    list(hms::hms(seconds = 0, minutes = 0, hours = 0))
  study_data$v02000 <- times
  meta_data <- util_rbind(
    meta_data,
    data.frame(
      stringsAsFactors = FALSE,
      VAR_NAMES = "v02000",
      LABEL = "ADMIS_TM_0",
      DATA_TYPE = DATA_TYPES$TIME,
      SCALE_LEVEL = SCALE_LEVELS$INTERVAL,
      VALUE_LABELS = NA_character_,
      STANDARDIZED_VOCABULARY_TABLE = NA_character_,
      MISSING_LIST_TABLE = NA_character_,
      HARD_LIMITS = "[09:00:00;18:00:00]",
      DETECTION_LIMITS = NA_character_,
      SOFT_LIMITS = NA_character_,
      DISTRIBUTION = NA_character_,
      DECIMALS = NA_character_,
      DATA_ENTRY_TYPE = NA_character_,
      GROUP_VAR_OBSERVER = NA_character_,
      GROUP_VAR_DEVICE = NA_character_,
      TIME_VAR = NA_character_,
      STUDY_SEGMENT = "STUDY",
      PART_VAR = "PART_STUDY",
      VARIABLE_ROLE = VARIABLE_ROLES$PRIMARY,
      VARIABLE_ORDER = "54",
      LONG_LABEL = "Admission time",
      ELEMENT_HOMOGENITY_CHECKTYPE = NA_character_,
      UNIVARIATE_OUTLIER_CHECKTYPE = NA_character_,
      N_RULES = "4",
      LOCATION_METRIC = NA_character_,
      LOCATION_RANGE = NA_character_,
      PROPORTION_RANGE = NA_character_,
      REPEATED_MEASURES_VARS = NA_character_,
      CO_VARS = NA_character_,
      MISSING_LIST = "00:00:00 = not available"
    )
  )

  r <- con_limit_deviations(
    "v02000",
    study_data = study_data, meta_data =
      meta_data, label_col = LABEL,
    meta_data_v2 =
      "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx" # nolint: line_length_linter.
  )

  skip_if_not_installed("vdiffr")
  suppressWarnings(
    expect_doppelganger2(
      "con_limit_deviations time only vars",
      r$SummaryPlotList$ADMIS_TM_0
    )
  )


  # Use with_testthat() locally to inspect v02000 or v00042 time-only reports.
})
