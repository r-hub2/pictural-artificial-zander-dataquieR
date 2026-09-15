test_that("con_inadmissible_categorical works", {
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
  ({
    iav_cat_all <- con_inadmissible_categorical(
      study_data = study_data,
      meta_data = meta_data,
      label_col = "LABEL"
    )
  }) %>%
    expect_message2(
      regexp = paste(
        "All variables with VALUE_LABELS.+",
        "in the metadata are used."
      ),
      perl = TRUE
    ) %>%
    expect_message2(
      regexp = paste(
        "The following variable.s.: .+_IAV.+flag.s.",
        "inadmissible values."
      ),
      perl = TRUE
    )

  expect_equal(sum(iav_cat_all$SummaryTable$GRADING), 5)
  expect_equal(sum(iav_cat_all$FlaggedStudyData$EDUCATION_1_IAV), 3)

  expect_silent(suppressWarnings({
    iav_cat_all <- con_inadmissible_categorical(
      study_data = study_data,
      meta_data = meta_data,
      label_col = "LABEL",
      resp_vars =
        c(
          "MARRIED_0",
          "SMOKING_0",
          "PREGNANT_0"
        )
    )

    iav_cat_all <- con_inadmissible_categorical(
      study_data = study_data,
      meta_data = meta_data,
      label_col = "LABEL",
      resp_vars = c("MARRIED_0")
    )
  }))
})

test_that("con_inadmissible_categorical handles local numeric code tables", {
  skip_on_cran()

  prep_purge_data_frame_cache()
  withr::defer(prep_purge_data_frame_cache())

  value_table <- data.frame(
    CODE_VALUE = c(1L, 2L),
    CODE_LABEL = c("one", "two"),
    stringsAsFactors = FALSE
  )
  prep_add_data_frames(value_table = value_table)

  study_data <- data.frame(
    x = c(1L, 2L, 9L, 9L),
    stringsAsFactors = FALSE
  )
  meta_data <- data.frame(
    VAR_NAMES = "x",
    LABEL = "x",
    DATA_TYPE = DATA_TYPES$INTEGER,
    SCALE_LEVEL = SCALE_LEVELS$NOMINAL,
    VALUE_LABEL_TABLE = "value_table",
    MISSING_LIST = "",
    JUMP_LIST = "",
    stringsAsFactors = FALSE
  )

  result <- suppressWarnings(suppressMessages(
    con_inadmissible_categorical(
      study_data = study_data,
      meta_data = meta_data,
      label_col = LABEL,
      resp_vars = "x",
      threshold_value = 75
    )
  ))
  result <- unclass(result)

  expect_equal(as.character(result$SummaryData$NON_MATCHING), "9")
  expect_equal(as.integer(result$SummaryData$NON_MATCHING_N), 2L)
  expect_equal(result$SummaryTable$PCT_con_rvv_icat, 50)
  expect_equal(result$SummaryTable$GRADING, 0)
  expect_false(result$SummaryTable$FLG_con_rvv_icat)
  expect_equal(result$ModifiedStudyData$x, c(1L, 2L, NA, NA))
  expect_equal(result$FlaggedStudyData$x_IAV, c(0, 0, 1, 1))
})

test_that("con_inadmissible_categorical removes local string values", {
  skip_on_cran()

  prep_purge_data_frame_cache()
  withr::defer(prep_purge_data_frame_cache())

  value_table <- data.frame(
    CODE_VALUE = c("A", "B"),
    CODE_LABEL = c("Alpha", "Beta"),
    stringsAsFactors = FALSE
  )
  prep_add_data_frames(value_table = value_table)

  study_data <- data.frame(
    x = c("A", "B", "Z", NA_character_),
    stringsAsFactors = FALSE
  )
  meta_data <- data.frame(
    VAR_NAMES = "x",
    LABEL = "x",
    DATA_TYPE = DATA_TYPES$STRING,
    SCALE_LEVEL = SCALE_LEVELS$NOMINAL,
    VALUE_LABEL_TABLE = "value_table",
    MISSING_LIST = "",
    JUMP_LIST = "",
    stringsAsFactors = FALSE
  )

  result <- suppressWarnings(suppressMessages(
    con_inadmissible_categorical(
      study_data = study_data,
      meta_data = meta_data,
      label_col = LABEL,
      resp_vars = "x",
      threshold_value = 0
    )
  ))
  result <- unclass(result)

  expect_equal(as.character(result$SummaryData$NON_MATCHING), "Z")
  expect_equal(as.integer(result$SummaryData$NON_MATCHING_N), 1L)
  expect_equal(result$ModifiedStudyData$x, c("A", "B", NA, NA))
  expect_equal(result$FlaggedStudyData$x_IAV, c(0, 0, 1, 0))
})

test_that("con_inadmissible_categorical supports self-coded value labels", {
  skip_on_cran()

  prep_purge_data_frame_cache()
  withr::defer(prep_purge_data_frame_cache())

  study_data <- data.frame(
    sex = c("male", "female", "other"),
    stringsAsFactors = FALSE
  )
  meta_data <- data.frame(
    VAR_NAMES = "sex",
    LABEL = "sex",
    DATA_TYPE = DATA_TYPES$STRING,
    SCALE_LEVEL = SCALE_LEVELS$NOMINAL,
    VALUE_LABELS = "male|female",
    MISSING_LIST = "",
    JUMP_LIST = "",
    stringsAsFactors = FALSE
  )

  result <- suppressWarnings(suppressMessages(
    con_inadmissible_categorical(
      study_data = study_data,
      meta_data = meta_data,
      label_col = LABEL,
      resp_vars = "sex",
      threshold_value = 0
    )
  ))
  result <- unclass(result)

  expect_equal(
    as.character(result$SummaryData$DEFINED_CATEGORIES),
    "male, female"
  )
  expect_equal(as.character(result$SummaryData$NON_MATCHING), "other")
  expect_equal(as.integer(result$SummaryData$NON_MATCHING_N), 1L)
  expect_equal(result$ModifiedStudyData$sex, c("male", "female", NA))
  expect_equal(result$FlaggedStudyData$sex_IAV, c(0, 0, 1))
})

test_that("con_inadmissible_categorical selects variables with local tables", {
  skip_on_cran()

  prep_purge_data_frame_cache()
  withr::defer(prep_purge_data_frame_cache())

  value_table <- data.frame(
    CODE_VALUE = c("A", "B"),
    CODE_LABEL = c("Alpha", "Beta"),
    stringsAsFactors = FALSE
  )
  prep_add_data_frames(value_table = value_table)

  study_data <- data.frame(
    x = c("A", "B", "Z"),
    y = c("A", "Z", "Z"),
    stringsAsFactors = FALSE
  )
  meta_data <- data.frame(
    VAR_NAMES = c("x", "y"),
    LABEL = c("x", "y"),
    DATA_TYPE = DATA_TYPES$STRING,
    SCALE_LEVEL = SCALE_LEVELS$NOMINAL,
    VALUE_LABEL_TABLE = c("value_table", ""),
    MISSING_LIST = "",
    JUMP_LIST = "",
    stringsAsFactors = FALSE
  )

  expect_message(
    auto_result <- suppressWarnings(con_inadmissible_categorical(
      study_data = study_data,
      meta_data = meta_data,
      label_col = LABEL
    )),
    "All variables with VALUE_LABELS"
  )
  auto_result <- unclass(auto_result)

  expect_equal(as.character(auto_result$SummaryData$Variables), "x")
  expect_false("y_IAV" %in% names(auto_result$FlaggedStudyData))

  expect_message(
    explicit_result <- suppressWarnings(con_inadmissible_categorical(
      study_data = study_data,
      meta_data = meta_data,
      label_col = LABEL,
      resp_vars = c("x", "y")
    )),
    "variables y have no defined"
  )
  explicit_result <- unclass(explicit_result)

  expect_equal(as.character(explicit_result$SummaryData$Variables), "x")
  expect_false("y_IAV" %in% names(explicit_result$FlaggedStudyData))
})

test_that("con_inadmissible_categorical rejects tables without CODE_VALUE", {
  skip_on_cran()

  prep_purge_data_frame_cache()
  withr::defer(prep_purge_data_frame_cache())

  value_table <- data.frame(
    CODE_LABEL = c("Alpha", "Beta"),
    stringsAsFactors = FALSE
  )
  prep_add_data_frames(value_table = value_table)

  valid_table <- data.frame(
    CODE_VALUE = c("A", "B"),
    CODE_LABEL = c("Alpha", "Beta"),
    stringsAsFactors = FALSE
  )
  prep_add_data_frames(valid_table = valid_table)

  study_data <- data.frame(
    x = c("A", "B"),
    y = c("A", "Z"),
    stringsAsFactors = FALSE
  )
  meta_data <- data.frame(
    VAR_NAMES = c("x", "y"),
    LABEL = c("x", "y"),
    DATA_TYPE = DATA_TYPES$STRING,
    SCALE_LEVEL = SCALE_LEVELS$NOMINAL,
    VALUE_LABEL_TABLE = c("value_table", "valid_table"),
    MISSING_LIST = "",
    JUMP_LIST = "",
    stringsAsFactors = FALSE
  )

  expect_message(
    result <- suppressWarnings(con_inadmissible_categorical(
      study_data = study_data,
      meta_data = meta_data,
      label_col = LABEL,
      resp_vars = c("x", "y")
    )),
    "value label table.+has no.+CODE_VALUE"
  )
  result <- unclass(result)

  expect_equal(as.character(result$SummaryData$Variables), "y")
  expect_false("x_IAV" %in% names(result$FlaggedStudyData))
  expect_equal(result$ModifiedStudyData$x, c("A", "B"))
  expect_equal(result$ModifiedStudyData$y, c("A", NA_character_))

  expect_error(
    suppressWarnings(suppressMessages(con_inadmissible_categorical(
      study_data = study_data["x"],
      meta_data = meta_data[1, , drop = FALSE],
      label_col = LABEL,
      resp_vars = "x"
    ))),
    "No categorical variables with usable value lists"
  )
})

test_that("con_inadmissible_vocabulary uses local vocabulary codes", {
  skip_on_cran()

  prep_purge_data_frame_cache()
  withr::defer(prep_purge_data_frame_cache())

  vocabulary <- data.frame(
    code = c("A", "B"),
    stringsAsFactors = FALSE
  )
  prep_add_data_frames(vocabulary = vocabulary)

  study_data <- data.frame(
    x = c("A", "B", "Z"),
    stringsAsFactors = FALSE
  )
  meta_data <- data.frame(
    VAR_NAMES = "x",
    LABEL = "x",
    DATA_TYPE = DATA_TYPES$STRING,
    SCALE_LEVEL = SCALE_LEVELS$NOMINAL,
    STANDARDIZED_VOCABULARY_TABLE = "vocabulary",
    MISSING_LIST = "",
    JUMP_LIST = "",
    stringsAsFactors = FALSE
  )

  result <- suppressWarnings(suppressMessages(
    con_inadmissible_vocabulary(
      study_data = study_data,
      meta_data = meta_data,
      label_col = LABEL,
      resp_vars = "x",
      threshold_value = 0
    )
  ))
  result <- unclass(result)

  expect_equal(
    as.character(result$SummaryData$DEFINED_CATEGORIES),
    "vocabulary"
  )
  expect_equal(as.character(result$SummaryData$NON_MATCHING), "Z")
  expect_equal(as.integer(result$SummaryData$NON_MATCHING_N), 1L)
  expect_equal(result$SummaryTable$PCT_con_rvv_icat, 33.3)
  expect_true(result$SummaryTable$FLG_con_rvv_icat)
  expect_equal(result$ModifiedStudyData$x, c("A", "B", NA))
  expect_equal(result$FlaggedStudyData$x_IAV, c(0, 0, 1))
})

test_that("con_inadmissible_vocabulary maps optional labels", {
  skip_on_cran()

  prep_purge_data_frame_cache()
  withr::defer(prep_purge_data_frame_cache())

  vocabulary <- data.frame(
    code = c("A", "B"),
    label = c("Alpha", "Beta"),
    stringsAsFactors = FALSE
  )
  prep_add_data_frames(vocabulary = vocabulary)

  study_data <- data.frame(
    x = c("A", "B", "Z"),
    stringsAsFactors = FALSE
  )
  meta_data <- data.frame(
    VAR_NAMES = "x",
    LABEL = "x",
    DATA_TYPE = DATA_TYPES$STRING,
    SCALE_LEVEL = SCALE_LEVELS$NOMINAL,
    STANDARDIZED_VOCABULARY_TABLE = "vocabulary",
    MISSING_LIST = "",
    JUMP_LIST = "",
    stringsAsFactors = FALSE
  )

  result <- suppressWarnings(suppressMessages(
    con_inadmissible_vocabulary(
      study_data = study_data,
      meta_data = meta_data,
      label_col = LABEL,
      resp_vars = "x"
    )
  ))
  result <- unclass(result)

  expect_equal(
    as.character(result$SummaryData$DEFINED_CATEGORIES),
    "vocabulary"
  )
  expect_equal(as.character(result$SummaryData$NON_MATCHING), "Z")
  expect_equal(result$ModifiedStudyData$x, c("A", "B", NA))
})

test_that("con_inadmissible_vocabulary can auto-select local vocabularies", {
  skip_on_cran()

  prep_purge_data_frame_cache()
  withr::defer(prep_purge_data_frame_cache())

  vocabulary <- data.frame(
    code = c("A", "B"),
    stringsAsFactors = FALSE
  )
  prep_add_data_frames(vocabulary = vocabulary)

  study_data <- data.frame(
    x = c("A", "Z"),
    stringsAsFactors = FALSE
  )
  meta_data <- data.frame(
    VAR_NAMES = "x",
    LABEL = "x",
    DATA_TYPE = DATA_TYPES$STRING,
    SCALE_LEVEL = SCALE_LEVELS$NOMINAL,
    STANDARDIZED_VOCABULARY_TABLE = "vocabulary",
    MISSING_LIST = "",
    JUMP_LIST = "",
    stringsAsFactors = FALSE
  )

  expect_message(
    result <- suppressWarnings(con_inadmissible_vocabulary(
      study_data = study_data,
      meta_data = meta_data,
      label_col = LABEL
    )),
    "All variables with STANDARDIZED_VOCABULARY_TABLE"
  )
  result <- unclass(result)

  expect_equal(as.character(result$SummaryData$NON_MATCHING), "Z")
  expect_equal(result$ModifiedStudyData$x, c("A", NA))
})

test_that("con_inadmissible_vocabulary reports partially missing metadata", {
  skip_on_cran()

  prep_purge_data_frame_cache()
  withr::defer(prep_purge_data_frame_cache())

  vocabulary <- data.frame(
    code = c("A", "B"),
    stringsAsFactors = FALSE
  )
  prep_add_data_frames(vocabulary = vocabulary)

  study_data <- data.frame(
    x = c("A", "Z"),
    y = c("C", "D"),
    stringsAsFactors = FALSE
  )
  meta_data <- data.frame(
    VAR_NAMES = c("x", "y"),
    LABEL = c("x", "y"),
    DATA_TYPE = DATA_TYPES$STRING,
    SCALE_LEVEL = SCALE_LEVELS$NOMINAL,
    STANDARDIZED_VOCABULARY_TABLE = c("vocabulary", ""),
    MISSING_LIST = "",
    JUMP_LIST = "",
    stringsAsFactors = FALSE
  )

  expect_message(
    result <- suppressWarnings(con_inadmissible_vocabulary(
      study_data = study_data,
      meta_data = meta_data,
      label_col = LABEL,
      resp_vars = c("x", "y")
    )),
    "The variables y have no definedSTANDARDIZED_VOCABULARY_TABLE"
  )
  result <- unclass(result)

  expect_equal(as.character(result$SummaryData$NON_MATCHING), "Z")
  expect_equal(result$ModifiedStudyData$x, c("A", NA))
  expect_equal(result$ModifiedStudyData$y, c("C", "D"))
})

test_that("con_inadmissible_vocabulary requires selectable variables", {
  skip_on_cran()

  prep_purge_data_frame_cache()
  withr::defer(prep_purge_data_frame_cache())

  vocabulary <- data.frame(
    code = "A",
    stringsAsFactors = FALSE
  )
  prep_add_data_frames(vocabulary = vocabulary)

  study_data <- data.frame(x = 1, stringsAsFactors = FALSE)
  meta_data <- data.frame(
    VAR_NAMES = "x",
    LABEL = "x",
    DATA_TYPE = DATA_TYPES$INTEGER,
    SCALE_LEVEL = SCALE_LEVELS$RATIO,
    STANDARDIZED_VOCABULARY_TABLE = "vocabulary",
    MISSING_LIST = "",
    JUMP_LIST = "",
    stringsAsFactors = FALSE
  )

  expect_error(
    suppressWarnings(suppressMessages(con_inadmissible_vocabulary(
      study_data = study_data,
      meta_data = meta_data,
      label_col = LABEL
    ))),
    "No categorical variables with value lists / vocabulary found"
  )
})

test_that("con_inadmissible_vocabulary requires vocabulary metadata", {
  skip_on_cran()

  study_data <- data.frame(
    x = c("A", "B", "Z"),
    stringsAsFactors = FALSE
  )
  meta_data <- data.frame(
    VAR_NAMES = "x",
    LABEL = "x",
    DATA_TYPE = DATA_TYPES$STRING,
    SCALE_LEVEL = SCALE_LEVELS$NOMINAL,
    stringsAsFactors = FALSE
  )

  expect_error(
    suppressWarnings(con_inadmissible_vocabulary(
      study_data = study_data,
      meta_data = meta_data,
      label_col = LABEL,
      resp_vars = "x"
    )),
    "requires.*STANDARDIZED_VOCABULARY_TABLE"
  )
})

test_that("con_inadmissible_vocabulary reports empty local vocabularies", {
  skip_on_cran()

  study_data <- data.frame(
    x = c("A", "B", "Z"),
    stringsAsFactors = FALSE
  )
  meta_data <- data.frame(
    VAR_NAMES = "x",
    LABEL = "x",
    DATA_TYPE = DATA_TYPES$STRING,
    SCALE_LEVEL = SCALE_LEVELS$NOMINAL,
    STANDARDIZED_VOCABULARY_TABLE = "",
    MISSING_LIST = SPLIT_CHAR,
    JUMP_LIST = SPLIT_CHAR,
    stringsAsFactors = FALSE
  )

  expect_error(
    suppressMessages(con_inadmissible_vocabulary(
      study_data = study_data,
      meta_data = meta_data,
      label_col = LABEL
    )),
    "No variables with defined.*STANDARDIZED_VOCABULARY_TABLE"
  )
})

test_that("con_inadmissible_vocabulary ignores malformed local vocabularies", {
  skip_on_cran()

  prep_purge_data_frame_cache()
  withr::defer(prep_purge_data_frame_cache())

  vocabulary <- data.frame(
    code = "A",
    label = "Alpha",
    extra = "ignored",
    stringsAsFactors = FALSE
  )
  prep_add_data_frames(vocabulary = vocabulary)

  study_data <- data.frame(
    x = c("A", "B"),
    stringsAsFactors = FALSE
  )
  meta_data <- data.frame(
    VAR_NAMES = "x",
    LABEL = "x",
    DATA_TYPE = DATA_TYPES$STRING,
    SCALE_LEVEL = SCALE_LEVELS$NOMINAL,
    STANDARDIZED_VOCABULARY_TABLE = "vocabulary",
    MISSING_LIST = SPLIT_CHAR,
    JUMP_LIST = SPLIT_CHAR,
    stringsAsFactors = FALSE
  )

  expect_warning(
    result <- suppressMessages(con_inadmissible_vocabulary(
      study_data = study_data,
      meta_data = meta_data,
      label_col = LABEL,
      resp_vars = "x"
    ))
  )
  result <- unclass(result)

  expect_equal(
    as.character(result$SummaryData$DEFINED_CATEGORIES),
    "vocabulary"
  )
  expect_equal(as.character(result$SummaryData$NON_MATCHING), "A, B")
  expect_equal(result$ModifiedStudyData$x, c(NA_character_, NA_character_))
})
