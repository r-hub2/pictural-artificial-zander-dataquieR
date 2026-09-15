test_that("int_sts_element_segment aggregates unexpected elements by segment", {
  skip_on_cran()

  study_data <- data.frame(speed = 1:3, extra = 4:6)
  meta_data <- data.frame(
    VAR_NAMES = c("speed", "missing"),
    LABEL = c("speed", "missing"),
    DATA_TYPE = DATA_TYPES$INTEGER,
    SCALE_LEVEL = SCALE_LEVELS$RATIO,
    STUDY_SEGMENT = c("Seg1", "Seg2"),
    MISSING_LIST = SPLIT_CHAR,
    JUMP_LIST = SPLIT_CHAR,
    stringsAsFactors = FALSE
  )

  result <- suppressMessages(suppressWarnings(int_sts_element_segment(
    study_data = study_data,
    meta_data = meta_data,
    label_col = VAR_NAMES
  )))

  expect_true(all(c("SegmentData", "SegmentTable") %in% names(result)))
  expect_equal(as.character(result$SegmentTable$Segment), c(
    "Seg2",
    "Seg1", "ALL"
  ))
  expect_equal(
    as.character(result$SegmentTable$MISSING),
    c(NA_character_, NA_character_, "metadata")
  )
  expect_equal(as.numeric(result$SegmentTable$NUM_int_sts_element), c(0, 0, 1))
  expect_equal(as.character(result$SegmentTable$resp_vars), c(NA, NA, "extra"))
  expect_equal(
    attr(result$SegmentTable$PCT_int_sts_element, DATA_TYPE, exact = TRUE),
    DATA_TYPES$FLOAT
  )
  expect_equal(
    names(result$SegmentData),
    c(
      "Segment",
      "Missing",
      "Percentage of unexpected elements",
      "Number of unexpected elements",
      "Response variables"
    )
  )
})

test_that("int_sts_element_segment reports study-data misses per segment", {
  skip_on_cran()
  withr::local_options(dataquieR.ELEMENT_MISSMATCH_CHECKTYPE = "exact")

  study_data <- data.frame(speed = 1:3)
  meta_data <- data.frame(
    VAR_NAMES = c("speed", "missing_a", "missing_b"),
    LABEL = c("Speed", "Missing A", "Missing B"),
    DATA_TYPE = DATA_TYPES$INTEGER,
    SCALE_LEVEL = SCALE_LEVELS$RATIO,
    STUDY_SEGMENT = c("Seg1", "Seg1", "Seg2"),
    MISSING_LIST = SPLIT_CHAR,
    JUMP_LIST = SPLIT_CHAR,
    stringsAsFactors = FALSE
  )

  result <- suppressMessages(suppressWarnings(int_sts_element_segment(
    study_data = study_data,
    meta_data = meta_data,
    label_col = VAR_NAMES
  )))

  problem <- result$SegmentTable[
    result$SegmentTable$MISSING %in% "study data",
    ,
    FALSE
  ]

  expect_equal(as.character(problem$Segment), c("Seg1", "Seg2"))
  expect_equal(as.numeric(problem$NUM_int_sts_element), c(1, 1))
  expect_equal(as.numeric(problem$PCT_int_sts_element), c(0.5, 1))
  expect_equal(
    as.character(problem$resp_vars),
    c("missing_a = Missing A", "missing_b = Missing B")
  )
})

test_that(
  "int_sts_element_segment permits metadata-only variables in subset_u",
  {
    skip_on_cran()
    withr::local_options(dataquieR.ELEMENT_MISSMATCH_CHECKTYPE = "subset_u")

    study_data <- data.frame(speed = 1:3)
    meta_data <- data.frame(
      VAR_NAMES = c("speed", "missing"),
      LABEL = c("Speed", "Missing variable"),
      DATA_TYPE = DATA_TYPES$INTEGER,
      SCALE_LEVEL = SCALE_LEVELS$RATIO,
      STUDY_SEGMENT = c("Seg1", "Seg2"),
      MISSING_LIST = SPLIT_CHAR,
      JUMP_LIST = SPLIT_CHAR,
      stringsAsFactors = FALSE
    )

    result <- suppressMessages(suppressWarnings(int_sts_element_segment(
      study_data = study_data,
      meta_data = meta_data,
      label_col = VAR_NAMES
    )))

    expect_false(any(result$SegmentTable$MISSING %in% "study data"))
  }
)

test_that("int_sts_element_segment renames reserved ALL segment names", {
  skip_on_cran()

  study_data <- data.frame(speed = 1:3)
  meta_data <- data.frame(
    VAR_NAMES = "speed",
    LABEL = "speed",
    DATA_TYPE = DATA_TYPES$INTEGER,
    SCALE_LEVEL = SCALE_LEVELS$RATIO,
    STUDY_SEGMENT = "ALL",
    MISSING_LIST = SPLIT_CHAR,
    JUMP_LIST = SPLIT_CHAR,
    stringsAsFactors = FALSE
  )

  captured <- new.env(parent = emptyenv())
  captured$messages <- character(0)
  result <- withCallingHandlers(
    suppressWarnings(int_sts_element_segment(
      study_data = study_data,
      meta_data = meta_data,
      label_col = VAR_NAMES
    )),
    message = function(m) {
      captured$messages <- c(captured$messages, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )

  expect_true(any(grepl("No segment should be named", captured$messages)))
  expect_equal(
    as.character(result$SegmentTable$Segment),
    "RENAMED SEGMENT: ALL"
  )
  expect_equal(as.numeric(result$SegmentTable$NUM_int_sts_element), 0)
})

test_that("int_sts_element_segment fills absent segment metadata", {
  skip_on_cran()

  study_data <- data.frame(speed = 1:3, extra = 4:6)
  meta_data <- data.frame(
    VAR_NAMES = "speed",
    LABEL = "Speed",
    DATA_TYPE = DATA_TYPES$INTEGER,
    SCALE_LEVEL = SCALE_LEVELS$RATIO,
    MISSING_LIST = SPLIT_CHAR,
    JUMP_LIST = SPLIT_CHAR,
    stringsAsFactors = FALSE
  )

  result <- suppressMessages(suppressWarnings(int_sts_element_segment(
    study_data = study_data,
    meta_data = meta_data,
    label_col = VAR_NAMES
  )))

  expect_equal(
    as.character(result$SegmentTable$Segment),
    c("<NO SEGMENT>", "ALL")
  )
  expect_equal(
    as.character(result$SegmentTable$MISSING),
    c(NA_character_, "metadata")
  )
  expect_equal(
    as.numeric(result$SegmentTable$PCT_int_sts_element),
    c(0, 50)
  )
  expect_equal(
    as.numeric(result$SegmentTable$NUM_int_sts_element),
    c(0, 1)
  )
  expect_equal(
    as.character(result$SegmentTable$resp_vars),
    c(NA_character_, "extra")
  )
})
