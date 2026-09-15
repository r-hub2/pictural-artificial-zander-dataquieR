skip_on_cran()

test_that("prep_get_study_data_segment keeps prepared attributes aligned", {
  prep_purge_data_frame_cache()
  withr::defer(prep_purge_data_frame_cache())

  study_data <- data.frame(
    id = 1:3,
    lab = c(1L, NA_integer_, 2L),
    other = c(10L, 11L, 12L)
  )
  meta_data <- data.frame(
    VAR_NAMES = c("id", "lab", "other"),
    LABEL = c("id", "lab", "other"),
    DATA_TYPE = rep(DATA_TYPES$INTEGER, 3),
    SCALE_LEVEL = c(
      SCALE_LEVELS$NOMINAL, SCALE_LEVELS$RATIO,
      SCALE_LEVELS$RATIO
    ),
    STUDY_SEGMENT = c("LAB", "LAB", "OTHER")
  )
  meta_data_segment <- data.frame(
    STUDY_SEGMENT = "LAB",
    SEGMENT_ID_VARS = "id"
  )

  segment_data <- prep_get_study_data_segment(
    "LAB",
    study_data = study_data,
    meta_data = meta_data,
    meta_data_segment = meta_data_segment
  )
  raw_segment_data <- util_attr(segment_data, "study_data", exact = TRUE)

  expect_s3_class(segment_data, "dataquieR_data_frame_prepared")
  expect_s3_class(raw_segment_data, "data.frame")
  expect_equal(dim(raw_segment_data), dim(segment_data))
  expect_equal(names(raw_segment_data), names(segment_data))
  expect_equal(names(segment_data), c("id", "lab"))
})

test_that("prep_get_study_data_segment tolerates missing segment IDs", {
  skip_on_cran()

  prep_purge_data_frame_cache()
  withr::defer(prep_purge_data_frame_cache())

  study_data <- data.frame(
    id = 1:3,
    lab = c(1L, NA_integer_, 2L),
    other = c(10L, 11L, 12L)
  )
  meta_data <- data.frame(
    VAR_NAMES = c("id", "lab", "other"),
    LABEL = c("id", "lab", "other"),
    DATA_TYPE = rep(DATA_TYPES$INTEGER, 3),
    SCALE_LEVEL = c(
      SCALE_LEVELS$NOMINAL, SCALE_LEVELS$RATIO,
      SCALE_LEVELS$RATIO
    ),
    STUDY_SEGMENT = c("LAB", "LAB", "OTHER")
  )
  meta_data_segment <- data.frame(
    STUDY_SEGMENT = character(0),
    SEGMENT_ID_VARS = character(0)
  )

  expect_warning(
    segment_data <- prep_get_study_data_segment(
      "LAB",
      study_data = study_data,
      meta_data = meta_data,
      meta_data_segment = meta_data_segment
    ),
    "No ID-vars"
  )

  expect_s3_class(segment_data, "dataquieR_data_frame_prepared")
  expect_equal(names(segment_data), c("id", "lab"))
  expect_equal(nrow(segment_data), 3)
})

test_that(
  "prep_get_study_data_segment falls back from corrupted segment meta",
  {
    skip_on_cran()

    prep_purge_data_frame_cache()
    withr::defer(prep_purge_data_frame_cache())

    study_data <- data.frame(
      id = 1:3,
      lab = c(1L, NA_integer_, 2L)
    )
    meta_data <- data.frame(
      VAR_NAMES = c("id", "lab"),
      LABEL = c("id", "lab"),
      DATA_TYPE = rep(DATA_TYPES$INTEGER, 2),
      SCALE_LEVEL = c(SCALE_LEVELS$NOMINAL, SCALE_LEVELS$RATIO),
      STUDY_SEGMENT = c("LAB", "LAB"),
      stringsAsFactors = FALSE
    )

    expect_warning(
      expect_warning(
        segment_data <- prep_get_study_data_segment(
          "LAB",
          study_data = study_data,
          meta_data = meta_data,
          meta_data_segment = "not a data frame"
        ),
        "segment_level.*missing/corrupted"
      ),
      "No ID-vars"
    )

    expect_s3_class(segment_data, "dataquieR_data_frame_prepared")
    expect_equal(names(segment_data), c("id", "lab"))
    expect_equal(nrow(segment_data), 3)
  }
)

test_that("prep_get_study_data_segment defaults missing item segment to ALL", {
  skip_on_cran()

  prep_purge_data_frame_cache()
  withr::defer(prep_purge_data_frame_cache())

  study_data <- data.frame(
    id = 1:3,
    lab = c(1L, NA_integer_, 2L)
  )
  meta_data <- data.frame(
    VAR_NAMES = c("id", "lab"),
    LABEL = c("id", "lab"),
    DATA_TYPE = rep(DATA_TYPES$INTEGER, 2),
    SCALE_LEVEL = c(SCALE_LEVELS$NOMINAL, SCALE_LEVELS$RATIO),
    stringsAsFactors = FALSE
  )
  meta_data_segment <- data.frame(
    STUDY_SEGMENT = "ALL",
    SEGMENT_ID_VARS = "id"
  )

  segment_data <- prep_get_study_data_segment(
    "ALL",
    study_data = study_data,
    meta_data = meta_data,
    meta_data_segment = meta_data_segment
  )

  expect_s3_class(segment_data, "dataquieR_data_frame_prepared")
  expect_equal(names(segment_data), c("id", "lab"))
  expect_equal(nrow(segment_data), 2)
})

test_that("prep_get_study_data_segment works", {
  skip_on_cran() # needs online access
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  sds <-
    prep_get_study_data_segment("LAB", "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData", # nolint: line_length_linter.
      meta_data_v2 = "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx" # nolint: line_length_linter.
    )
  raw_sds <- util_attr(sds, "study_data", exact = TRUE)
  expect_s3_class(sds, "dataquieR_data_frame_prepared")
  expect_s3_class(raw_sds, "data.frame")
  expect_equal(dim(raw_sds), dim(sds))
  expect_equal(names(raw_sds), names(sds))

  expect_equal(names(sds), c(
    "v00001", "v00014", "v00015", "v00016",
    "v00017", "v30000"
  ))
  expect_gt(nrow(sds), 0)
  expect_equal(ncol(sds), 6)
})
