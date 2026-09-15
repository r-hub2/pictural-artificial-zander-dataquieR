test_that("prep_apply_coding works", {
  skip_on_cran()
  study_data <- iris
  x <- prep_study2meta(
    study_data = study_data,
    convert_factors = TRUE
  )
  x$MetaData[[STUDY_SEGMENT]] <- "SEGMENT"
  study_data$Species <- as.character(study_data$Species)
  expect_warning(
    expect_equal(
      prep_apply_coding(
        study_data = study_data,
        item_level = x$MetaData
      )$Species,
      x$ModifiedStudyData$Species
    ),
    regexp = "Metadata does not provide a filled column called .+JUMP_LIST"
  )
})

test_that("prep_apply_coding keeps non-integer codes as character values", {
  skip_on_cran()

  prep_purge_data_frame_cache()
  withr::defer(prep_purge_data_frame_cache())

  prep_add_data_frames(response_codes = data.frame(
    CODE_LABEL = c("yes", "no"),
    CODE_VALUE = c("Y", "N")
  ))
  study_data <- data.frame(
    response = c("yes", "no", "maybe"),
    stringsAsFactors = FALSE
  )
  meta_data <- data.frame(
    VAR_NAMES = "response",
    VALUE_LABEL_TABLE = "response_codes",
    DATA_TYPE = DATA_TYPES$STRING,
    SCALE_LEVEL = SCALE_LEVELS$NOMINAL,
    MISSING_LIST = "",
    JUMP_LIST = "",
    HARD_LIMITS = "",
    stringsAsFactors = FALSE
  )

  coded <- suppressWarnings(prep_apply_coding(
    study_data = study_data,
    item_level = meta_data
  ))

  expect_identical(coded$response, c("Y", "N", "maybe"))
})
