test_that(
  "prep_scalelevel_from_data_and_metadata warns for deprecated resp_vars",
  {
    skip_on_cran()

    study_data <- data.frame(score = seq_len(4))
    meta_data <- data.frame(
      VAR_NAMES = "score",
      LABEL = "score",
      DATA_TYPE = DATA_TYPES$INTEGER,
      SCALE_LEVEL = NA_character_,
      MISSING_LIST = "",
      JUMP_LIST = "",
      stringsAsFactors = FALSE
    )

    expect_warning(
      suppressWarningsMatching(
        prep_scalelevel_from_data_and_metadata(
          resp_vars = "score",
          study_data = study_data,
          meta_data = meta_data,
          label_col = LABEL
        ),
        "Metadata does not provide a filled column"
      ),
      "resp_vars.*prep_scalelevel_from_data_and_metadata"
    )
  }
)

test_that("prep_scalelevel_from_data_and_metadata covers heuristic branches", {
  skip_on_cran()

  prep_purge_data_frame_cache()
  withr::defer(prep_purge_data_frame_cache())

  study_data <- data.frame(
    ordered_code = rep(c(1L, 2L, 3L), length.out = 30),
    group_id = rep(c("a", "b"), length.out = 30),
    timepoint = as.POSIXct("2026-07-20 10:00:00", tz = "UTC") + seq_len(30),
    note = paste("free text", seq_len(30)),
    score = seq_len(30),
    change = seq(-1, 1.9, length.out = 30),
    tiny_count = rep(c(0L, 1L), length.out = 30),
    stringsAsFactors = FALSE
  )

  value_table <- data.frame(
    CODE_VALUE = c(1L, 2L, 3L),
    CODE_LABEL = c("low", "middle", "high"),
    CODE_ORDER = c(1L, 2L, 3L),
    stringsAsFactors = FALSE
  )
  prep_add_data_frames(ordered_value_table = value_table)

  meta_data <- data.frame(
    VAR_NAMES = names(study_data),
    LABEL = names(study_data),
    DATA_TYPE = c(
      DATA_TYPES$INTEGER,
      DATA_TYPES$STRING,
      DATA_TYPES$DATETIME,
      DATA_TYPES$STRING,
      DATA_TYPES$INTEGER,
      DATA_TYPES$FLOAT,
      DATA_TYPES$INTEGER
    ),
    SCALE_LEVEL = NA_character_,
    VALUE_LABEL_TABLE = c("ordered_value_table", rep(NA_character_, 6)),
    GROUP_VAR_1 = c(NA_character_, NA_character_, NA_character_,
      NA_character_, "group_id", NA_character_,
      NA_character_),
    TIME_VAR_1 = c(NA_character_, NA_character_, NA_character_,
      NA_character_, NA_character_, "timepoint",
      NA_character_),
    MISSING_LIST = "",
    JUMP_LIST = "",
    stringsAsFactors = FALSE
  )

  withr::local_options(
    dataquieR.scale_level_heuristics_control_binaryrecodelimit = 2,
    dataquieR.scale_level_heuristics_control_metriclevels = 4
  )

  amended <- suppressWarningsMatching(
    prep_scalelevel_from_data_and_metadata(
      study_data = study_data,
      meta_data = meta_data,
      label_col = LABEL
    ),
    "Metadata does not provide a filled column"
  )
  scale_levels <- setNames(amended[[SCALE_LEVEL]], amended[[VAR_NAMES]])

  expect_equal(scale_levels[["ordered_code"]], SCALE_LEVELS$ORDINAL)
  expect_equal(scale_levels[["group_id"]], SCALE_LEVELS$NOMINAL)
  expect_equal(scale_levels[["timepoint"]], SCALE_LEVELS$INTERVAL)
  expect_equal(scale_levels[["note"]], SCALE_LEVELS$`NA`)
  expect_equal(scale_levels[["score"]], SCALE_LEVELS$RATIO)
  expect_equal(scale_levels[["change"]], SCALE_LEVELS$INTERVAL)
  expect_equal(scale_levels[["tiny_count"]], SCALE_LEVELS$NOMINAL)
})

test_that("prep_scalelevel_from_data_and_metadata repairs invalid thresholds", {
  skip_on_cran()

  study_data <- data.frame(score = seq_len(10))
  meta_data <- data.frame(
    VAR_NAMES = "score",
    LABEL = "score",
    DATA_TYPE = DATA_TYPES$INTEGER,
    SCALE_LEVEL = NA_character_,
    MISSING_LIST = "",
    JUMP_LIST = "",
    stringsAsFactors = FALSE
  )

  withr::local_options(
    dataquieR.scale_level_heuristics_control_binaryrecodelimit = 8,
    dataquieR.scale_level_heuristics_control_metriclevels = 3
  )

  expect_warning(
    amended <- suppressWarningsMatching(
      prep_scalelevel_from_data_and_metadata(
        study_data = study_data,
        meta_data = meta_data,
        label_col = LABEL
      ),
      "Metadata does not provide a filled column"
    ),
    "threshold for metric variables"
  )

  expect_false(util_empty(amended[[SCALE_LEVEL]]))
})

test_that(
  "prep_scalelevel_from_data_and_metadata ignores invalid code tables",
  {
    skip_on_cran()

    prep_purge_data_frame_cache()
    withr::defer(prep_purge_data_frame_cache())

    study_data <- data.frame(
      bad_codes = c(1L, 2L, 1L, 2L),
      stringsAsFactors = FALSE
    )
    bad_value_table <- data.frame(
      CODE_LABEL = c("a", "b"),
      stringsAsFactors = FALSE
    )
    prep_add_data_frames(bad_value_table = bad_value_table)
    meta_data <- data.frame(
      VAR_NAMES = "bad_codes",
      LABEL = "bad_codes",
      DATA_TYPE = DATA_TYPES$INTEGER,
      SCALE_LEVEL = NA_character_,
      VALUE_LABEL_TABLE = "bad_value_table",
      MISSING_LIST = "",
      JUMP_LIST = "",
      stringsAsFactors = FALSE
    )

    expect_warning(
      amended <- suppressWarningsMatching(
        prep_scalelevel_from_data_and_metadata(
          study_data = study_data,
          meta_data = meta_data,
          label_col = LABEL
        ),
        "Metadata does not provide a filled column"
      ),
      "Missing at least a column"
    )

    expect_equal(amended[[SCALE_LEVEL]], SCALE_LEVELS$NOMINAL)
  }
)

test_that(
  "prep_scalelevel_from_data_and_metadata covers metadata fallbacks",
  {
    skip_on_cran()

    study_data <- data.frame(score = c(1L, 2L, 1L, 2L))

    expect_warning(
      expect_warning(
        no_meta <- suppressWarningsMatching(
          prep_scalelevel_from_data_and_metadata(
            study_data = study_data,
            meta_data = NULL,
            label_col = LABEL
          ),
          "Metadata does not provide a filled column"
        ),
        "No item-level metadata provided at all"
      ),
      "No item-level metadata provided at all"
    )
    expect_equal(no_meta[[VAR_NAMES]], "score")
    expect_false(util_empty(no_meta[[SCALE_LEVEL]]))

    prep_purge_data_frame_cache()
    withr::defer(prep_purge_data_frame_cache())

    unordered_codes <- data.frame(
      CODE_VALUE = c(2L, 1L),
      CODE_ORDER = c(1L, 2L),
      stringsAsFactors = FALSE
    )
    prep_add_data_frames(unordered_codes = unordered_codes)
    meta_data <- data.frame(
      VAR_NAMES = "score",
      LABEL = "score",
      DATA_TYPE = DATA_TYPES$INTEGER,
      SCALE_LEVEL = NA_character_,
      VALUE_LABEL_TABLE = "unordered_codes",
      MISSING_LIST = "",
      JUMP_LIST = "",
      stringsAsFactors = FALSE
    )

    expect_message(
      amended <- suppressWarningsMatching(
        prep_scalelevel_from_data_and_metadata(
          study_data = study_data,
          meta_data = meta_data,
          label_col = LABEL
        ),
        "Metadata does not provide a filled column"
      ),
      "Found counter-intuitive"
    )
    expect_equal(amended[[SCALE_LEVEL]], SCALE_LEVELS$ORDINAL)
  }
)
