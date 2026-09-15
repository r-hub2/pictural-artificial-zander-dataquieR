skip_on_cran()

mahalanobis_small_inputs <- function() {
  list(
    study_data = data.frame(
      x = c(1, 2, 3, 4, 5, 100),
      y = c(2, 1, 4, 3, 6, 100)
    ),
    meta_data = data.frame(
      VAR_NAMES = c("x", "y"),
      DATA_TYPE = c("float", "float"),
      SCALE_LEVEL = c("ratio", "ratio"),
      MISSING_LIST = NA_character_,
      JUMP_LIST = NA_character_,
      HARD_LIMITS = NA_character_,
      stringsAsFactors = FALSE
    )
  )
}

test_that("acc_mahalanobis falls back for invalid direct thresholds", {
  skip_on_cran()

  input <- mahalanobis_small_inputs()

  result <- NULL
  invisible(capture.output(
    result <- suppressWarnings(suppressMessages(acc_mahalanobis(
      variable_group = c("x", "y"),
      study_data = input$study_data,
      meta_data = input$meta_data,
      meta_data_cross_item = data.frame(
        VARIABLE_LIST = character(),
        CHECK_LABEL = character()
      ),
      mahalanobis_threshold = "not-a-number"
    ))),
    type = "output"
  ))

  expect_equal(
    as.numeric(result$SummaryTable$mahalanobis_threshold),
    dataquieR.MAHALANOBIS_THRESHOLD_default
  )
  expect_equal(result$SummaryTable$Variables, "variable_group",
    ignore_attr = TRUE
  )
  expect_identical(
    attr(result$SummaryTable$Variables, DATA_TYPE),
    DATA_TYPES$STRING
  )
  expect_s3_class(result$SummaryPlotList$variable_group, "ggplot")
})

test_that("acc_mahalanobis reads metadata threshold defaults", {
  skip_on_cran()

  input <- mahalanobis_small_inputs()
  meta_data_cross_item <- data.frame(
    VARIABLE_LIST = "x | y",
    CHECK_LABEL = "xy",
    MAHALANOBIS_THRESHOLD = "true",
    stringsAsFactors = FALSE
  )

  result <- NULL
  invisible(capture.output(
    result <- suppressWarnings(suppressMessages(acc_mahalanobis(
      study_data = input$study_data,
      meta_data = input$meta_data,
      meta_data_cross_item = meta_data_cross_item
    ))),
    type = "output"
  ))

  expect_equal(result$SummaryTable$Variables, "xy", ignore_attr = TRUE)
  expect_equal(
    as.numeric(result$SummaryTable$mahalanobis_threshold),
    dataquieR.MAHALANOBIS_THRESHOLD_default
  )
  expect_s3_class(result$SummaryPlotList$xy, "ggplot")
})

test_that("acc_mahalanobis combines several metadata-defined checks", {
  skip_on_cran()

  input <- list(
    study_data = data.frame(
      x1 = c(1, 2, 3, 4, 20),
      x2 = c(1, 3, 2, 4, 20),
      y1 = c(2, 3, 4, 5, 30),
      y2 = c(2, 4, 3, 5, 30)
    ),
    meta_data = data.frame(
      VAR_NAMES = c("x1", "x2", "y1", "y2"),
      DATA_TYPE = rep("float", 4),
      SCALE_LEVEL = rep("ratio", 4),
      MISSING_LIST = NA_character_,
      JUMP_LIST = NA_character_,
      HARD_LIMITS = NA_character_,
      stringsAsFactors = FALSE
    )
  )
  meta_data_cross_item <- data.frame(
    stringsAsFactors = FALSE,
    check = c("x_pair", "y_pair"),
    vars = c("x1 | x2", "y1 | y2"),
    threshold = c("0.9", "true")
  )
  names(meta_data_cross_item) <- c(
    CHECK_LABEL,
    VARIABLE_LIST,
    MAHALANOBIS_THRESHOLD
  )

  result <- NULL
  invisible(capture.output(
    result <- suppressWarnings(suppressMessages(acc_mahalanobis(
      study_data = input$study_data,
      meta_data = input$meta_data,
      meta_data_cross_item = meta_data_cross_item
    ))),
    type = "output"
  ))

  expect_equal(result$SummaryTable$Variables, c("x_pair", "y_pair"),
    ignore_attr = TRUE
  )
  expect_equal(as.numeric(result$SummaryTable$mahalanobis_threshold),
    c(0.9, dataquieR.MAHALANOBIS_THRESHOLD_default)
  )
  expect_named(result$SummaryPlotList, c("x_pair", "y_pair"))
  expect_s3_class(result$SummaryPlotList$x_pair, "ggplot")
  expect_true(all(c(
    "MD_x_pair",
    "MD_outliers_x_pair",
    "MD_y_pair",
    "MD_outliers_y_pair"
  ) %in% names(result$FlaggedStudyData)))
  expect_false("row_n" %in% names(result$FlaggedStudyData))
  expect_equal(nrow(result$FlaggedStudyData), nrow(input$study_data))
})

test_that("mahalanobis works", {
  skip_on_cran() # slow and covered by CI
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")

  df1 <- data.frame(
    ID = c(letters[1:26], "aa", "bb", "cc", "dd"),
    Q1 = c(5, 4, 1, 2, 2, 99999, 2, rep(1, 10), 2, 2, 2, 2, 3, 2, 1, 1, 2, 2, 2, 2, 1), # nolint: line_length_linter.
    Q2 = c(2, 5, rep(1, 4), 2, rep(1, 13), 2, 2, 1, 1, 1, 1, 1, 1, 2, 1),
    Q3 = c(4, 5, rep(1, 20), 3, 1, 1, 1, 1, 1, 1, 1),
    Q4 = c(NA, 1, 4, 4, 5, 3, 4, 5, 3, 3, 5, 5, 4, 4, 4, 3, 4, 3, 4, 5, 5, 4, 4, 3, 4, 4, 4, 4, 5, 4), # nolint: line_length_linter.
    Q5 = c(88888, 1, 3, 4, 3, 3, 4, 3, 4, 3, 4, 3, 3, 3, 3, 2, 3, 3, 3, 3, 3, 4, 4, 3, 3, 3, 4, 3, 4, 3), # nolint: line_length_linter.
    Q6 = c(88888, 1, 5, 5, 5, 4, 5, 4, 5, 3, 4, 5, 4, 4, rep(5, 5), 4, 4, 5, 5, 4, 4, 5, 5, 4, 5, 5), # nolint: line_length_linter.
    Q7 = c(NA, 2, 3, 3, 2, rep(3, 4), 4, 3, 3, 4, 3, 3, 4, 3, 2, 3, 3, 4, 3, 4, 3, 3, 4, 3, 3, 3, 4), # nolint: line_length_linter.
    Q8 = c(NA, 1, rep(4, 5), 5, 4, 4, 3, 4, 3, 3, 4, 4, 5, 5, 4, 4, 3, 3, 4, 4, 4, 4, 5, 5, 4, 4), # nolint: line_length_linter.
    Q9 = c(NA, 1, 4, 3, 3, 4, 3, 3, 2, 3, 3, 2, rep(3, 4), 4, 3, 3, 4, 3, 3, 3, 4, 3, 4, 4, 3, 3, 3), # nolint: line_length_linter.
    Q10 = c(1, 1, 2, 1, rep(2, 4), 1, 2, 2, 2, 3, 3, rep(2, 4), 3, rep(2, 4), 3, 2, 2, 3, 2, 1, 2), # nolint: line_length_linter.
    stringsAsFactors = FALSE
  )

  # PREPARE_METADATA
  # Define the common value labels to avoid repetitive typing
  likert_labels <-
    "1 = not apply | 2 = rather not apply | 3 = intermediate | 4 = rather apply | 5 = apply completely" # nolint: line_length_linter.

  # Create the item_level data frame
  item_level <- data.frame(
    VAR_NAMES = c("ID", paste0("Q", 1:10), "T1", "T2"),
    LABEL = c(
      "pseudo id", "shy and reserved", "trust people", "tasks throughly",
      "relaxed", "active imagination", "sociable", "find faults in others",
      "lazy", "nervous", "artistic interest", "begin", "end"
    ),
    DATA_TYPE = c("string", rep("integer", 10), "datetime", "datetime"),
    SCALE_LEVEL = c("na", rep("ordinal", 10), "interval", "interval"),
    TIME_VAR = c(NA, rep("T1", 10), NA, NA),
    TIME_VAR_END = c(NA, rep("T2", 10), NA, NA),
    MISSING_LIST_TABLE = c(NA, rep("missing_table", 10), "missing_table", "missing_table"), # nolint: line_length_linter.
    VALUE_LABELS = c(
      NA,
      rep(likert_labels, 10),
      NA,
      NA
    ),
    STUDY_SEGMENT = c(NA, rep("Questionnaire", 12)),
    VARIABLE_ROLE = c("intro", rep("primary", 10), "process", "process"),
    stringsAsFactors = FALSE
  )

  # add it to the cache
  prep_purge_data_frame_cache()
  prep_add_data_frames(item_level)

  # Create the missing_table data frame
  missing_table <- data.frame(
    CODE_VALUE = c(99999, 99998, 99997, 88888),
    CODE_LABEL = c(
      "Missing - other reason",
      "Missing - exclusion criteria",
      "Missing - refusal",
      "JUMP 88880"
    ),
    CODE_CLASS = c("MISSING", "MISSING", "MISSING", "JUMP"),
    stringsAsFactors = FALSE
  )

  # add in the cache
  prep_add_data_frames(missing_table)


  # Create the cross_item_level data frame
  cross_item_level <- data.frame(
    VARIABLE_LIST = c(
      "Q1 | Q2 | Q3 | Q4 | Q5 | Q6",
      "Q7 | Q8 | Q9 | Q10",
      "Q1 | Q2 | Q3 | Q4 | Q5 | Q6 | Q7 | Q8 | Q9 | Q10"
    ),
    CHECK_LABEL = c(
      "First part",
      "Second part",
      "all questionnaire"
    ),
    CONTRADICTION_TERM = c(NA, NA, NA),
    CONTRADICTION_TYPE = c(NA, NA, NA),
    MULTIVARIATE_OUTLIER_CHECKTYPE = c(NA, NA, NA),
    N_RULES = c(NA, NA, NA),
    MAXIMUM_LONG_STRING = c("[;2)", "[;]", "[;3]"),
    IRV = c(NA, "[-1; 1]", NA),
    IRV_ARGS = c(NA, "20", NA),
    stringsAsFactors = FALSE
  )

  ## add in the cache
  prep_add_data_frames(cross_item_level)


  mahal_res <- acc_mahalanobis(
    variable_group = c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8", "Q9", "Q10"), # nolint: line_length_linter.
    study_data = df1,
    meta_data = "item_level"
  )


  expect_equal(mahal_res$SummaryTable$NUM_ssc_mah, 2, ignore_attr = TRUE)
  expect_identical(
    attr(mahal_res$SummaryTable$NUM_ssc_mah, DATA_TYPE),
    DATA_TYPES$INTEGER
  )
  expect_equal(mahal_res$SummaryTable$observational_units_removed, 2,
    ignore_attr = TRUE
  )
  expect_identical(
    attr(mahal_res$SummaryTable$observational_units_removed, DATA_TYPE),
    DATA_TYPES$INTEGER
  )
  expect_equal(mahal_res$SummaryData$`MD_outliers (%)`, 7.14,
    ignore_attr = TRUE
  )
  expect_identical(
    attr(mahal_res$SummaryData$`MD_outliers (%)`, DATA_TYPE),
    DATA_TYPES$FLOAT
  )
})

test_that("acc_mahalanobis refuses report-pipeline execution", {
  skip_on_cran()

  expect_error(
    with_pipeline(acc_mahalanobis()),
    "not meant to run in the pipeline"
  )
})

test_that("acc_mahalanobis handles cross-item thresholds and labels", {
  prep_purge_data_frame_cache()
  withr::defer(prep_purge_data_frame_cache())

  study_data <- data.frame(
    Q1 = c(1, 2, 3, 4, 10),
    Q2 = c(2, 1, 4, 3, 9),
    Q3 = c(1, 3, 2, 4, 10),
    Q4 = c(4, 1, 3, 2, 9)
  )
  item_level <- data.frame(
    VAR_NAMES = paste0("Q", 1:4),
    LABEL = paste("Question", 1:4),
    DATA_TYPE = DATA_TYPES$INTEGER,
    SCALE_LEVEL = SCALE_LEVELS$ORDINAL,
    VARIABLE_ROLE = VARIABLE_ROLES$PRIMARY,
    STUDY_SEGMENT = "Questionnaire",
    stringsAsFactors = FALSE
  )
  cross_item_level <- data.frame(
    VARIABLE_LIST = c("Q1 | Q2", "Q3 | Q4"),
    CHECK_ID = c("mh1", "mh2"),
    CHECK_LABEL = c("Mahalanobis", "Mahalanobis"),
    CONTRADICTION_TERM = NA_character_,
    CONTRADICTION_TYPE = NA_character_,
    MULTIVARIATE_OUTLIER_CHECKTYPE = NA_character_,
    N_RULES = NA_integer_,
    MAHALANOBIS_THRESHOLD = c("not a threshold", "0.9"),
    stringsAsFactors = FALSE
  )
  prep_add_data_frames(item_level, cross_item_level)

  result <- NULL
  expect_message2(
    result <- suppressWarnings(acc_mahalanobis(
      study_data = study_data,
      meta_data = "item_level",
      meta_data_cross_item = "cross_item_level"
    )),
    "Invalid"
  )

  expect_equal(
    result$SummaryData$Variables,
    c("Mahalanobis", "Check #2"),
    ignore_attr = TRUE
  )
  expect_equal(
    as.numeric(result$SummaryData$mahalanobis_threshold),
    c(dataquieR.MAHALANOBIS_THRESHOLD_default, 0.9)
  )
  expect_true("MD_outliers_Mahalanobis" %in% colnames(result$FlaggedStudyData))
  expect_true("MD_outliers_Check #2" %in% colnames(result$FlaggedStudyData))
})

test_that("acc_mahalanobis reports missing variable groups once", {
  prep_purge_data_frame_cache()
  withr::defer(prep_purge_data_frame_cache())

  study_data <- data.frame(
    Q1 = c(1, 2, 3),
    Q2 = c(2, 3, 4)
  )
  item_level <- data.frame(
    VAR_NAMES = c("Q1", "Q2"),
    LABEL = c("Question 1", "Question 2"),
    DATA_TYPE = DATA_TYPES$INTEGER,
    SCALE_LEVEL = SCALE_LEVELS$ORDINAL,
    VARIABLE_ROLE = VARIABLE_ROLES$PRIMARY,
    stringsAsFactors = FALSE
  )
  prep_add_data_frames(item_level)

  expect_error(
    suppressWarnings(suppressMessages(acc_mahalanobis(
      study_data = study_data,
      meta_data = "item_level"
    ))),
    "No variables provided to calculate Mahalanobis distance"
  )
})
