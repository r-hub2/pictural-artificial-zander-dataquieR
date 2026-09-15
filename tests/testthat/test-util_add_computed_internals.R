test_that("util_add_computed_internals generates computed variable metadata", {
  skip_on_cran()

  meta_data <- data.frame(
    VAR_NAMES = paste0("Q", seq_len(4)),
    LABEL = paste("Question", seq_len(4)),
    LONG_LABEL = paste("Long", seq_len(4)),
    DATA_TYPE = DATA_TYPES$INTEGER,
    SCALE_LEVEL = SCALE_LEVELS$ORDINAL,
    VARIABLE_ROLE = VARIABLE_ROLES$PROCESS,
    STUDY_SEGMENT = "questionnaire",
    stringsAsFactors = FALSE
  )
  meta_data_cross_item <- data.frame(
    VARIABLE_LIST = c("Q1 | Q2 | Q3", "Q2 | Q3 | Q4"),
    CHECK_ID = c("long", "irv", "syn", "attention"),
    CHECK_LABEL = c("First part", "Variation", "Synonyms", "Attention"),
    CONTRADICTION_TERM = NA_character_,
    CONTRADICTION_TYPE = NA_character_,
    MULTIVARIATE_OUTLIER_CHECKTYPE = NA_character_,
    N_RULES = NA_integer_,
    MAXIMUM_LONG_STRING = c("[;2)", NA, NA, NA),
    IRV = c(NA, "[-1;1]", NA, NA),
    IRV_ARGS = c(NA, "20", NA, NA),
    PSYCHOMETRIC_SYN = c(NA, NA, "[-1;1]", NA),
    SUM_ATTENTION_CHECK_ITEMS = c(NA, NA, NA, "[0;3]"),
    SCALE_NAME = c("Scale", NA, NA, NA),
    SCALE_ACRONYM = c("SC", NA, NA, NA),
    stringsAsFactors = FALSE
  )
  meta_data_item_computation <- data.frame(
    VAR_NAMES = character(),
    COMPUTATION_RULE = character(),
    DATA_PREPARATION = character(),
    CHECK_ID = character()
  )

  result <- util_add_computed_internals(
    meta_data_item_computation = meta_data_item_computation,
    meta_data_cross_item = meta_data_cross_item,
    meta_data = meta_data,
    label_col = LABEL
  )

  expect_equal(nrow(result$meta_data_item_computation), 4)
  expect_setequal(
    result$meta_data_item_computation$COMPUTATION_RULE,
    c(
      "maxLongStr([Q1], [Q2], [Q3])",
      "IRV(CASES_NA_BELOW(20, [Q2], [Q3], [Q4]))",
      "PSYCHOMETRIC_SYN([Q1], [Q2], [Q3])",
      "SUM_ATTENTION_CHECK_ITEMS([Q2], [Q3], [Q4])"
    )
  )
  expect_setequal(
    result$meta_data_item_computation$CHECK_ID,
    c("long", "irv", "syn", "attention")
  )

  computed <- result$meta_data[
    result$meta_data[[COMPUTED_VARIABLE_ROLE]] %in%
      c(
        COMPUTED_VARIABLE_ROLES$MAXIMUM_LONG_STRING,
        COMPUTED_VARIABLE_ROLES$IRV,
        COMPUTED_VARIABLE_ROLES$PSYCHOMETRIC_SYN,
        COMPUTED_VARIABLE_ROLES$SUM_ATTENTION_CHECK_ITEMS
      ),
    ,
    drop = FALSE
  ]
  computed <- computed[order(computed[[CHECK_ID]]), , drop = FALSE]

  expect_equal(computed[[VARIABLE_ROLE]], rep(VARIABLE_ROLES$PRIMARY, 4))
  expect_equal(computed[[CHECK_ID]], c("attention", "irv", "long", "syn"))
  expect_equal(computed[[HARD_LIMITS]], c("[0;3]", "[-1;1]", "[;2)", "[-1;1]"))
  expect_equal(
    computed[[COMPUTED_VARIABLE_ROLE]],
    c(
      COMPUTED_VARIABLE_ROLES$SUM_ATTENTION_CHECK_ITEMS,
      COMPUTED_VARIABLE_ROLES$IRV,
      COMPUTED_VARIABLE_ROLES$MAXIMUM_LONG_STRING,
      COMPUTED_VARIABLE_ROLES$PSYCHOMETRIC_SYN
    )
  )
  expect_match(computed[[LABEL]][computed[[CHECK_ID]] == "long"],
    "Maximum Long String.First part",
    fixed = TRUE)
  expect_match(computed[[LABEL]][computed[[CHECK_ID]] == "irv"],
    "Intra-individual Response Variability.Variation",
    fixed = TRUE)
  expect_false(any(grepl("_[0-9]+$", computed[[LABEL]])))
})

test_that("util_add_computed_internals ignores invalid computed intervals", {
  skip_on_cran()

  meta_data <- data.frame(
    VAR_NAMES = paste0("Q", seq_len(2)),
    LABEL = paste("Question", seq_len(2)),
    LONG_LABEL = paste("Long", seq_len(2)),
    DATA_TYPE = DATA_TYPES$INTEGER,
    SCALE_LEVEL = SCALE_LEVELS$ORDINAL,
    VARIABLE_ROLE = VARIABLE_ROLES$PROCESS,
    STUDY_SEGMENT = "questionnaire",
    stringsAsFactors = FALSE
  )
  meta_data_cross_item <- data.frame(
    VARIABLE_LIST = "Q1 | Q2",
    CHECK_ID = "invalid",
    CHECK_LABEL = "Invalid interval",
    MAXIMUM_LONG_STRING = "not-an-interval",
    SCALE_NAME = NA_character_,
    SCALE_ACRONYM = NA_character_,
    stringsAsFactors = FALSE
  )
  meta_data_item_computation <- data.frame(
    VAR_NAMES = character(),
    COMPUTATION_RULE = character(),
    DATA_PREPARATION = character(),
    CHECK_ID = character()
  )

  result <- suppressWarnings(util_add_computed_internals(
    meta_data_item_computation = meta_data_item_computation,
    meta_data_cross_item = meta_data_cross_item,
    meta_data = meta_data,
    label_col = LABEL
  ))

  expect_equal(nrow(result$meta_data_item_computation), 0)
  expect_equal(nrow(result$meta_data), nrow(meta_data))
  expect_false(COMPUTED_VARIABLE_ROLE %in% names(result$meta_data))
})

test_that("util_add_computed_internals honors custom label columns", {
  skip_on_cran()

  meta_data <- data.frame(
    VAR_NAMES = c("Q1", "Q2", "IRV"),
    LABEL = c("Question 1", "Question 2", "Intra-individual Resp. Variab."),
    DISPLAY_LABEL = c("Shown 1", "Shown 2", "Intra-individual Resp. Variab."),
    LONG_LABEL = c("Long 1", "Long 2", "Intra-individual Response Variability"),
    DATA_TYPE = DATA_TYPES$INTEGER,
    SCALE_LEVEL = SCALE_LEVELS$ORDINAL,
    VARIABLE_ROLE = VARIABLE_ROLES$PROCESS,
    STUDY_SEGMENT = c("questionnaire", "questionnaire", "COMPUTED_IRV"),
    stringsAsFactors = FALSE
  )
  meta_data_cross_item <- data.frame(
    VARIABLE_LIST = "Q1 | Q2",
    CHECK_ID = "irv_custom",
    CHECK_LABEL = NA_character_,
    IRV = "[0;1]",
    IRV_ARGS = NA_character_,
    SCALE_NAME = NA_character_,
    SCALE_ACRONYM = "ACR",
    stringsAsFactors = FALSE
  )
  meta_data_item_computation <- data.frame(
    VAR_NAMES = character(),
    COMPUTATION_RULE = character(),
    DATA_PREPARATION = character(),
    CHECK_ID = character()
  )

  result <- util_add_computed_internals(
    meta_data_item_computation = meta_data_item_computation,
    meta_data_cross_item = meta_data_cross_item,
    meta_data = meta_data,
    label_col = "DISPLAY_LABEL"
  )

  computed <- result$meta_data[
    !is.na(result$meta_data[[CHECK_ID]]) &
      result$meta_data[[CHECK_ID]] == "irv_custom",
    ,
    drop = FALSE
  ]

  expect_equal(nrow(computed), 1)
  expect_identical(computed[[VAR_NAMES]], "IRV_Check #1")
  expect_identical(computed[[STUDY_SEGMENT]], ".COMPUTED__ssi")
  expect_identical(computed[["DISPLAY_LABEL"]], computed[[LABEL]])
  expect_identical(
    computed[[LABEL]],
    "Intra-individual Response Variability.ACR"
  )
  expect_match(computed[[LONG_LABEL]], "ACR")
})

test_that("computed internals preserve metadata without SSI columns", {
  skip_on_cran()

  meta_data <- data.frame(
    VAR_NAMES = "Q1",
    LABEL = "Question 1",
    DATA_TYPE = DATA_TYPES$INTEGER,
    stringsAsFactors = FALSE
  )
  computations <- data.frame(
    VAR_NAMES = "existing",
    COMPUTATION_RULE = "identity([Q1])",
    DATA_PREPARATION = "",
    CHECK_ID = "existing",
    stringsAsFactors = FALSE
  )
  cross_item <- data.frame(
    VARIABLE_LIST = "Q1",
    CHECK_ID = "unused",
    stringsAsFactors = FALSE
  )

  result <- util_add_computed_internals(
    meta_data_item_computation = computations,
    meta_data_cross_item = cross_item,
    meta_data = meta_data,
    label_col = NULL
  )

  expect_identical(
    as.character(result$meta_data_item_computation[[VAR_NAMES]]),
    computations[[VAR_NAMES]]
  )
  expect_identical(
    as.character(result$meta_data_item_computation[[COMPUTATION_RULE]]),
    computations[[COMPUTATION_RULE]]
  )
  expect_identical(
    as.character(result$meta_data[[VAR_NAMES]]),
    meta_data[[VAR_NAMES]]
  )
  expect_identical(
    as.character(result$meta_data[[LABEL]]),
    meta_data[[LABEL]]
  )
})

test_that("computed internals treat missing cross-item metadata as empty", {
  skip_on_cran()

  meta_data <- data.frame(
    VAR_NAMES = "Q1",
    LABEL = "Question 1",
    DATA_TYPE = DATA_TYPES$INTEGER,
    stringsAsFactors = FALSE
  )
  computations <- data.frame(
    VAR_NAMES = "existing",
    COMPUTATION_RULE = "identity([Q1])",
    DATA_PREPARATION = "",
    CHECK_ID = "existing",
    stringsAsFactors = FALSE
  )

  result <- util_add_computed_internals(
    meta_data_item_computation = computations,
    meta_data = meta_data,
    label_col = LABEL
  )

  expect_identical(
    as.character(result$meta_data_item_computation[[VAR_NAMES]]),
    computations[[VAR_NAMES]]
  )
  expect_identical(
    as.character(result$meta_data[[VAR_NAMES]]),
    meta_data[[VAR_NAMES]]
  )
})

test_that("util_add_computed_internals supplies empty optional SSI arguments", {
  skip_on_cran()

  meta_data <- data.frame(
    VAR_NAMES = c("Q1", "Q2"),
    LABEL = c("Question 1", "Question 2"),
    LONG_LABEL = c("Long 1", "Long 2"),
    DATA_TYPE = DATA_TYPES$INTEGER,
    SCALE_LEVEL = SCALE_LEVELS$ORDINAL,
    VARIABLE_ROLE = VARIABLE_ROLES$PROCESS,
    STUDY_SEGMENT = "questionnaire",
    stringsAsFactors = FALSE
  )
  cross_item <- data.frame(
    VARIABLE_LIST = "Q1 | Q2",
    CHECK_ID = "irv_without_args",
    CHECK_LABEL = "IRV check",
    IRV = "[0;1]",
    IRV_ARGS = "",
    SCALE_NAME = NA_character_,
    SCALE_ACRONYM = NA_character_,
    stringsAsFactors = FALSE
  )
  computations <- data.frame(
    VAR_NAMES = character(),
    COMPUTATION_RULE = character(),
    DATA_PREPARATION = character(),
    CHECK_ID = character()
  )

  result <- util_add_computed_internals(
    meta_data_item_computation = computations,
    meta_data_cross_item = cross_item,
    meta_data = meta_data,
    label_col = NULL
  )

  expect_identical(
    as.character(result$meta_data_item_computation[[COMPUTATION_RULE]]),
    "IRV(CASES_NA_BELOW([.], [Q1], [Q2]))"
  )
  expect_identical(result$meta_data[[CHECK_ID]][3], "irv_without_args")
})

test_that("computed internals default absent optional SSI argument columns", {
  skip_on_cran()

  meta_data <- data.frame(
    VAR_NAMES = c("Q1", "Q2"),
    LABEL = c("Question 1", "Question 2"),
    LONG_LABEL = c("Long 1", "Long 2"),
    DATA_TYPE = DATA_TYPES$INTEGER,
    SCALE_LEVEL = SCALE_LEVELS$ORDINAL,
    VARIABLE_ROLE = VARIABLE_ROLES$PROCESS,
    STUDY_SEGMENT = "questionnaire",
    stringsAsFactors = FALSE
  )
  cross_item <- data.frame(
    VARIABLE_LIST = "Q1 | Q2",
    CHECK_ID = "irv_without_argument_column",
    CHECK_LABEL = "IRV check",
    IRV = "[0;1]",
    SCALE_NAME = NA_character_,
    SCALE_ACRONYM = NA_character_,
    stringsAsFactors = FALSE
  )
  computations <- data.frame(
    VAR_NAMES = character(),
    COMPUTATION_RULE = character(),
    DATA_PREPARATION = character(),
    CHECK_ID = character()
  )

  result <- util_add_computed_internals(
    meta_data_item_computation = computations,
    meta_data_cross_item = cross_item,
    meta_data = meta_data,
    label_col = NULL
  )

  expect_identical(
    as.character(result$meta_data_item_computation[[COMPUTATION_RULE]]),
    "IRV(CASES_NA_BELOW([.], [Q1], [Q2]))"
  )
})

test_that("computed internals use variable lists inferred from rules", {
  skip_on_cran()

  meta_data <- data.frame(
    VAR_NAMES = c("AGE_0", "AGE_1"),
    LABEL = c("Age B/L", "Age F/U"),
    LONG_LABEL = c("Age at baseline", "Age at follow-up"),
    DATA_TYPE = DATA_TYPES$INTEGER,
    SCALE_LEVEL = SCALE_LEVELS$RATIO,
    VARIABLE_ROLE = VARIABLE_ROLES$PRIMARY,
    STUDY_SEGMENT = "questionnaire",
    stringsAsFactors = FALSE
  )
  cross_item <- data.frame(
    VARIABLE_LIST = NA_character_,
    CONTRADICTION_TERM = "[AGE_1] < [AGE_0]",
    CHECK_ID = "age_follow_up",
    CHECK_LABEL = "Age follow-up",
    MISS_RESP = "[;2)",
    stringsAsFactors = FALSE
  )

  result <- util_add_computed_internals(
    meta_data_item_computation = data.frame(),
    meta_data_cross_item = cross_item,
    meta_data = meta_data,
    label_col = LABEL
  )

  expect_identical(
    as.character(result$meta_data_item_computation[[COMPUTATION_RULE]]),
    "perc_miss_in_row([AGE_0], [AGE_1])"
  )
  expect_identical(
    as.character(result$meta_data_item_computation[[CHECK_ID]]),
    "age_follow_up"
  )
})
