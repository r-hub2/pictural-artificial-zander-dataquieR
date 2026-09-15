test_that("con_contradictions covers local guardrails", {
  skip_on_cran()

  study_data <- data.frame(a = 1:3, b = 1:3)
  meta_data <- data.frame(
    VAR_NAMES = c("a", "b"),
    LABEL = c("a", "b"),
    DATA_TYPE = c(DATA_TYPES$INTEGER, DATA_TYPES$INTEGER),
    SCALE_LEVEL = c(SCALE_LEVELS$RATIO, SCALE_LEVELS$RATIO),
    stringsAsFactors = FALSE
  )
  check_table <- data.frame(
    ID = 1,
    Label = "a equals b",
    Function_name = "local_check",
    A = "a",
    B = "b",
    A_value = "1",
    B_value = "1",
    A_levels = "",
    B_levels = "",
    stringsAsFactors = FALSE
  )

  expect_error(
    suppressMessages(suppressWarnings(
      con_contradictions(
        study_data = study_data,
        meta_data = meta_data,
        label_col = VAR_NAMES,
        check_table = check_table[-1]
      )
    )),
    "Missing columns"
  )
  expect_error(
    suppressMessages(suppressWarnings(
      con_contradictions(
        study_data = study_data,
        meta_data = meta_data,
        label_col = VAR_NAMES,
        check_table = check_table,
        summarize_categories = TRUE
      )
    )),
    "Cannot summerize categories"
  )
  expect_error(
    suppressMessages(suppressWarnings(
      con_contradictions(
        study_data = study_data,
        meta_data = meta_data,
        label_col = VAR_NAMES,
        check_table = check_table
      )
    )),
    "Missing column .CONTRADICTIONS."
  )
})

test_that("con_contradictions summarizes local tagged checks", {
  skip_on_cran()

  study_data <- data.frame(
    a = c(1, 2, 3),
    b = c(1, 1, 4),
    c = c(1, 2, 3)
  )
  meta_data <- data.frame(
    VAR_NAMES = c("a", "b", "c"),
    LABEL = c("a", "b", "c"),
    DATA_TYPE = rep(DATA_TYPES$INTEGER, 3),
    SCALE_LEVEL = rep(SCALE_LEVELS$RATIO, 3),
    CONTRADICTIONS = c("1", "1 | 2", "2"),
    MISSING_LIST = NA_character_,
    JUMP_LIST = NA_character_,
    HARD_LIMITS = NA_character_,
    stringsAsFactors = FALSE
  )
  check_table <- data.frame(
    ID = c(1, 2),
    Label = c("a bigger b", "c smaller b"),
    Function_name = c("A_greater_than_B_vv", "A_less_than_B_vv"),
    A = c("a", "c"),
    B = c("b", "b"),
    A_value = "",
    B_value = "",
    A_levels = "",
    B_levels = "",
    tag = c("Logical", "Empirical | Logical"),
    stringsAsFactors = FALSE
  )

  result <- suppressWarnings(suppressMessages(con_contradictions(
    study_data = study_data,
    meta_data = meta_data,
    label_col = VAR_NAMES,
    threshold_value = 0,
    check_table = check_table,
    summarize_categories = TRUE
  )))

  expect_named(result, c(
    "Empirical",
    "Logical",
    "all_checks",
    "SummaryData",
    "SummaryPlot"
  ))
  expect_equal(result$SummaryData$category,
    c("Empirical", "Logical", "all_checks")
  )
  expect_equal(round(result$SummaryData$percent, 2), c(33.33, 66.67, 66.67))
  expect_equal(nrow(result$all_checks$SummaryTable), 2L)
  expect_equal(nrow(result$Empirical$SummaryTable), 1L)
  expect_equal(nrow(result$Logical$SummaryTable), 2L)
  expect_s3_class(result$SummaryPlot, "ggplot")
})

test_that("con_contradictions works", {
  skip_on_cran() # slow and deprecated, use redcap rules, now
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  require_english_locale_and_berlin_tz()
  study_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData", keep_types = TRUE) # nolint: line_length_linter.
  meta_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data.RData") # nolint: line_length_linter.
  meta_data <- prep_scalelevel_from_data_and_metadata(
    meta_data = meta_data,
    study_data = study_data
  )
  meta_data[startsWith(meta_data[[LABEL]], "EDUCATION_"), SCALE_LEVEL] <-
    SCALE_LEVELS$ORDINAL
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
  label_col <- LABEL
  threshold_value <- 1
  check_table[1, "tag"] <- "Logical, Age-Related"
  check_table[10, "tag"] <- "Empirical, Age-Related"
  suppressMessages(suppressWarnings({
    default <- con_contradictions(
      study_data = study_data, meta_data = meta_data, label_col = label_col,
      threshold_value = threshold_value, check_table = check_table
    )
    off <- con_contradictions(
      study_data = study_data, meta_data = meta_data, label_col = label_col,
      threshold_value = threshold_value, check_table = check_table,
      summarize_categories = FALSE
    )
    on <- con_contradictions(
      study_data = study_data, meta_data = meta_data, label_col = label_col,
      threshold_value = threshold_value, check_table = check_table,
      summarize_categories = TRUE
    )
  }))
  # expect_equal(off, default) because of plots not always true,
  # e.g.:   Component "SummaryPlot": Component "layers": Component 1:
  # Component 11: Component 1: target is not list-like
  expect_equal(off$FlaggedStudyData, on$all_checks$FlaggedStudyData)
  expect_equal(off$SummaryTable, on$all_checks$SummaryTable)
  expect_equal(off$SummaryData, on$all_checks$SummaryData)

  expect_lt(
    abs(suppressWarnings(sum(as.numeric(as.matrix(default$SummaryData)),
          na.rm = TRUE
        )) - 12052.56), 10
  )

  expect_lt(
    abs(suppressWarnings(sum(as.numeric(as.matrix(on$Empirical$SummaryTable)),
          na.rm = TRUE
        )) - 5474.32), 10
  )

  skip_on_cran()
  skip_if_not_installed("vdiffr")
  expect_doppelganger2(
    "summary contradictio1 plot ok",
    on$all_checks$SummaryPlot
  )
  expect_doppelganger2(
    "summary contradictio2 plot ok",
    default$SummaryPlot
  )
  expect_doppelganger2(
    "summary contradictio3 plot ok",
    off$SummaryPlot
  )

  expect_doppelganger2(
    "one cat contradiction plot ok",
    on$Empirical$SummaryPlot
  )
})
