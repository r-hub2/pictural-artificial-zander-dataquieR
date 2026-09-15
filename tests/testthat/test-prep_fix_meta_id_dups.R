test_that("prep_fix_meta_id_dups collapses duplicated ID variables", {
  skip_on_cran()

  item_level <- data.frame(
    VAR_NAMES = c("ID", "ID", "AGE"),
    LABEL = c("ID", "ID", "Age"),
    DATA_TYPE = DATA_TYPES$INTEGER,
    MISSING_LIST_TABLE = NA_character_,
    SCALE_LEVEL = SCALE_LEVELS$ORDINAL,
    VARIABLE_ROLE = c(
      VARIABLE_ROLES$INTRO, VARIABLE_ROLES$INTRO,
      VARIABLE_ROLES$PRIMARY
    ),
    STUDY_SEGMENT = c("seg1", "seg2", "seg1"),
    DATAFRAMES = NA_character_,
    stringsAsFactors = FALSE
  )
  dataframe_level <- data.frame(
    DF_NAME = "df",
    DF_ID_VARS = "ID",
    stringsAsFactors = FALSE
  )
  segment_level <- data.frame(
    STUDY_SEGMENT = "seg1",
    SEGMENT_ID_VARS = NA_character_,
    stringsAsFactors = FALSE
  )

  expect_message(
    prep_fix_meta_id_dups(
      item_level = item_level,
      meta_data_dataframe = dataframe_level,
      meta_data_segment = segment_level
    ),
    "Consolidating duplicated ID-variable metadata"
  )
  fixed <- suppressMessages(prep_fix_meta_id_dups(
    item_level = item_level,
    meta_data_dataframe = dataframe_level,
    meta_data_segment = segment_level
  ))

  expect_equal(fixed[[VAR_NAMES]], c("ID", "AGE"))
  expect_equal(
    fixed[[STUDY_SEGMENT]][fixed[[VAR_NAMES]] == "ID"],
    "**INTRO**"
  )
  expect_equal(fixed[[DATAFRAMES]][fixed[[VAR_NAMES]] == "ID"], "**INTRO**")
})

test_that(
  paste0(
    "prep_fix_meta_id_dups returns metadata without ",
    "duplicate variable names unchanged"
  ),
  {
    skip_on_cran()

    item_level <- data.frame(
      VAR_NAMES = c("ID", "AGE"),
      LABEL = c("ID", "Age"),
      DATA_TYPE = DATA_TYPES$INTEGER,
      MISSING_LIST_TABLE = NA_character_,
      SCALE_LEVEL = SCALE_LEVELS$ORDINAL,
      VARIABLE_ROLE = c(VARIABLE_ROLES$INTRO, VARIABLE_ROLES$PRIMARY),
      STUDY_SEGMENT = c("intro", "main"),
      DATAFRAMES = NA_character_,
      stringsAsFactors = FALSE
    )

    fixed <- prep_fix_meta_id_dups(
      item_level = item_level,
      meta_data_dataframe = data.frame(DF_NAME = "df"),
      meta_data_segment = data.frame(STUDY_SEGMENT = "intro")
    )

    expect_equal(fixed, item_level)
  }
)

test_that(
  paste0(
    "prep_fix_meta_id_dups warns for non-ID duplicates ",
    "that only differ by location"
  ),
  {
    skip_on_cran()

    item_level <- data.frame(
      VAR_NAMES = c("AGE", "AGE", "ID"),
      LABEL = c("Age", "Age", "ID"),
      DATA_TYPE = DATA_TYPES$INTEGER,
      MISSING_LIST_TABLE = NA_character_,
      SCALE_LEVEL = SCALE_LEVELS$ORDINAL,
      VARIABLE_ROLE = c(
        VARIABLE_ROLES$PRIMARY, VARIABLE_ROLES$PRIMARY,
        VARIABLE_ROLES$INTRO
      ),
      STUDY_SEGMENT = c("seg1", "seg2", "seg1"),
      DATAFRAMES = NA_character_,
      stringsAsFactors = FALSE
    )
    dataframe_level <- data.frame(
      DF_NAME = "df",
      stringsAsFactors = FALSE
    )
    segment_level <- data.frame(
      STUDY_SEGMENT = "seg1",
      stringsAsFactors = FALSE
    )

    expect_warning(
      fixed <- prep_fix_meta_id_dups(
        item_level = item_level,
        meta_data_dataframe = dataframe_level,
        meta_data_segment = segment_level
      ),
      "none-id-vars are duplicated"
    )

    expect_equal(fixed[[VAR_NAMES]], c("AGE", "ID"))
    expect_equal(
      fixed[[STUDY_SEGMENT]][fixed[[VAR_NAMES]] == "AGE"],
      "**INTRO**"
    )
    expect_equal(fixed[[DATAFRAMES]][fixed[[VAR_NAMES]] == "AGE"], "**INTRO**")
  }
)

test_that("prep_fix_meta_id_dups rejects real variable name conflicts", {
  skip_on_cran()

  item_level <- data.frame(
    VAR_NAMES = c("ID", "ID"),
    LABEL = c("ID", "Different ID label"),
    DATA_TYPE = DATA_TYPES$INTEGER,
    MISSING_LIST_TABLE = NA_character_,
    SCALE_LEVEL = SCALE_LEVELS$ORDINAL,
    VARIABLE_ROLE = VARIABLE_ROLES$INTRO,
    STUDY_SEGMENT = c("seg1", "seg2"),
    DATAFRAMES = NA_character_,
    stringsAsFactors = FALSE
  )
  dataframe_level <- data.frame(
    DF_NAME = "df",
    DF_ID_VARS = "ID",
    stringsAsFactors = FALSE
  )
  segment_level <- data.frame(
    STUDY_SEGMENT = "seg1",
    SEGMENT_ID_VARS = NA_character_,
    stringsAsFactors = FALSE
  )

  expect_error(
    prep_fix_meta_id_dups(
      item_level = item_level,
      meta_data_dataframe = dataframe_level,
      meta_data_segment = segment_level
    ),
    "Fixing of different variables using the same name not yet supported",
    class = dataquieR.applicability_problem
  )
})
