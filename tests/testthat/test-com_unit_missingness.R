test_that("com_unit_missingness works", {
  skip_on_cran()
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  meta_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data.RData") # nolint: line_length_linter.
  study_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData", keep_types = TRUE) # nolint: line_length_linter.
  r <- com_unit_missingness(study_data,
    item_level = meta_data,
    label_col = LABEL,
    id_vars = "PSEUDO_ID", strata_vars = "CENTER_0"
  )
  expect_equal(
    length(intersect(
      names(r),
      c("FlaggedStudyData", "SummaryData")
    )), length(union(
      names(r),
      c("FlaggedStudyData", "SummaryData")
    ))
  )
  expect_equal(r$SummaryData,
    structure(
      list(
        CENTER_0 = c(
          "Berlin",
          "Hamburg",
          "Leipzig",
          "Cologne",
          "Munich"
        ),
        N_OBS = c(617L, 581L, 593L, 564L, 585L),
        N_UNIT_MISSINGS = c(15L, 11L, 9L, 13L, 12L),
        "N_UNIT_MISSINGS_(%)" = c(2.43, 1.89, 1.52, 2.3, 2.05)
      ),
      row.names = c(NA, -5L),
      class = "data.frame"
    ),
    ignore_attr = TRUE
  )
  expect_identical(
    attr(r$SummaryData$CENTER_0, DATA_TYPE),
    DATA_TYPES$STRING
  )
  expect_identical(
    attr(r$SummaryData$N_OBS, DATA_TYPE),
    DATA_TYPES$INTEGER
  )
  expect_identical(
    attr(r$SummaryData$N_UNIT_MISSINGS, DATA_TYPE),
    DATA_TYPES$INTEGER
  )
  expect_identical(
    attr(r$SummaryData$`N_UNIT_MISSINGS_(%)`, DATA_TYPE),
    DATA_TYPES$FLOAT
  )
  expect_equal(sum(r$FlaggedStudyData$Unit_missing == 1), 60)
  expect_equal(unique(r$FlaggedStudyData$Unit_missing), 0:1)
})

test_that("com_unit_missingness covers local no-ID and empty-strata paths", {
  skip_on_cran()

  study_data <- data.frame(
    id = c(1L, 2L, 3L),
    group = c("a", "b", "a"),
    x = c(1L, 2L, 3L),
    y = c(2L, 3L, 4L)
  )
  meta_data <- data.frame(
    VAR_NAMES = c("id", "group", "x", "y"),
    DATA_TYPE = c(
      DATA_TYPES$INTEGER,
      DATA_TYPES$STRING,
      DATA_TYPES$INTEGER,
      DATA_TYPES$INTEGER
    ),
    MISSING_LIST = "",
    JUMP_LIST = "",
    MISSING_LIST_TABLE = NA_character_,
    JUMP_LIST_TABLE = NA_character_,
    SCALE_LEVEL = c(
      SCALE_LEVELS$ORDINAL,
      SCALE_LEVELS$NOMINAL,
      SCALE_LEVELS$RATIO,
      SCALE_LEVELS$RATIO
    ),
    VARIABLE_ROLE = VARIABLE_ROLES$PRIMARY,
    PART_VAR = NA_character_,
    stringsAsFactors = FALSE
  )

  suppressWarnings(
    expect_message(
      no_ids <- com_unit_missingness(
        study_data = study_data,
        meta_data = meta_data,
        label_col = VAR_NAMES
      ),
      "No ID-variables specified"
    )
  )
  expect_equal(no_ids$SummaryData$N, 0L, ignore_attr = TRUE)
  expect_equal(no_ids$SummaryData[[2L]], 0, ignore_attr = TRUE)

  stratified <- suppressWarnings(suppressMessages(com_unit_missingness(
    study_data = study_data,
    meta_data = meta_data,
    label_col = VAR_NAMES,
    id_vars = "id",
    strata_vars = "group"
  )))

  expect_equal(stratified$FlaggedStudyData$Unit_missing, c(0L, 0L, 0L))
  expect_equal(
    as.data.frame(stratified$SummaryData),
    data.frame(
      group = c("a", "b"),
      N_OBS = c(2L, 1L),
      N_UNIT_MISSINGS = c(0, 0),
      "N_UNIT_MISSINGS_(%)" = c(0, 0),
      check.names = FALSE
    ),
    ignore_attr = TRUE
  )
  expect_identical(
    attr(stratified$SummaryData$N_UNIT_MISSINGS, DATA_TYPE),
    DATA_TYPES$INTEGER
  )
})
