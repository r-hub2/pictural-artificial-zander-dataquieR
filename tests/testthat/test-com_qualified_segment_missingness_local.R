test_that(
  "com_qualified_segment_missingness computes local AAPOR segment rates",
  {
    skip_on_cran()

    cache <- new.env(parent = emptyenv())
    result <- with_dataframe_environment(quote({
      study_data <- data.frame(part = c(1, 1, 2, 3, 4, 5))
      meta_data <- data.frame(
        VAR_NAMES = "part",
        LABEL = "part",
        DATA_TYPE = DATA_TYPES$INTEGER,
        SCALE_LEVEL = SCALE_LEVELS$NOMINAL,
        MISSING_LIST_TABLE = "aapor_codes",
        MISSING_LIST = NA_character_,
        JUMP_LIST = NA_character_,
        stringsAsFactors = FALSE
      )
      segment_level <- data.frame(
        STUDY_SEGMENT = "SegA",
        SEGMENT_PART_VARS = "part",
        stringsAsFactors = FALSE
      )
      aapor_codes <- data.frame(
        CODE_VALUE = as.character(1:5),
        CODE_LABEL = c(
          "interview", "partial", "partial late", "refusal", "breakoff"
        ),
        CODE_INTERPRET = c("I", "P", "PL", "R", "BO"),
        stringsAsFactors = FALSE
      )
      prep_add_data_frames(aapor_codes = aapor_codes)
      suppressMessages(com_qualified_segment_missingness(
        study_data = study_data,
        meta_data = meta_data,
        meta_data_segment = segment_level,
        label_col = VAR_NAMES,
        expected_observations = "ALL"
      ))
    }), env = cache)

    expect_named(result, c("SegmentTable", "SegmentData"))
    expect_equal(as.character(result$SegmentTable$Segment), "SegA")
    expect_equal(as.integer(result$SegmentTable$I), 2L)
    expect_equal(as.integer(result$SegmentTable$P), 1L)
    expect_equal(as.integer(result$SegmentTable$PL), 1L)
    expect_equal(as.integer(result$SegmentTable$R), 1L)
    expect_equal(as.integer(result$SegmentTable$BO), 1L)
    expect_equal(as.numeric(result$SegmentTable$RR1), 0.5)
    expect_equal(as.numeric(result$SegmentTable$PCT_com_qum_nonresp), 50)
    expect_equal(as.numeric(result$SegmentTable$PCT_com_qum_refusal), 100 / 3)
    expect_equal(as.integer(result$SegmentTable$N), 6L)
    expect_equal(as.integer(result$SegmentTable$N2), 6L)
    expect_equal(
      attr(result$SegmentTable$PCT_com_qum_nonresp, DATA_TYPE, exact = TRUE),
      DATA_TYPES$FLOAT
    )
    expect_true(any(grepl("Non-response", names(result$SegmentData),
          fixed = TRUE
        )))
  }
)

test_that(
  "com_qualified_segment_missingness skips absent participation variables",
  {
    skip_on_cran()

    cache <- new.env(parent = emptyenv())
    captured <- new.env(parent = emptyenv())
    captured$warnings <- character(0)
    result <- withCallingHandlers(
      with_dataframe_environment(quote({
        study_data <- data.frame(other = c(1, 2, 3))
        meta_data <- data.frame(
          VAR_NAMES = c("part", "other"),
          LABEL = c("part", "other"),
          DATA_TYPE = DATA_TYPES$INTEGER,
          SCALE_LEVEL = SCALE_LEVELS$NOMINAL,
          MISSING_LIST_TABLE = c("aapor_codes", NA_character_),
          MISSING_LIST = NA_character_,
          JUMP_LIST = NA_character_,
          stringsAsFactors = FALSE
        )
        segment_level <- data.frame(
          STUDY_SEGMENT = "SegA",
          SEGMENT_PART_VARS = "part",
          stringsAsFactors = FALSE
        )
        aapor_codes <- data.frame(
          CODE_VALUE = "1",
          CODE_LABEL = "interview",
          CODE_INTERPRET = "I",
          stringsAsFactors = FALSE
        )
        prep_add_data_frames(aapor_codes = aapor_codes)
        suppressMessages(com_qualified_segment_missingness(
          study_data = study_data,
          meta_data = meta_data,
          meta_data_segment = segment_level,
          label_col = VAR_NAMES,
          expected_observations = "ALL"
        ))
      }), env = cache),
      warning = function(w) {
        captured$warnings <- c(captured$warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    )

    expect_true(any(grepl("Missing or doubled", captured$warnings)))
    expect_true(any(grepl("part", captured$warnings, fixed = TRUE)))
    expect_s3_class(result$SegmentTable, "data.frame")
    expect_equal(nrow(result$SegmentTable), 0)
    expect_equal(nrow(result$SegmentData), 0)
  }
)

test_that("com_qualified_segment_missingness skips missing AAPOR code tables", {
  skip_on_cran()

  cache <- new.env(parent = emptyenv())
  expect_warning(
    with_dataframe_environment(quote({
      study_data <- data.frame(part = c(1, 2, 3))
      meta_data <- data.frame(
        VAR_NAMES = "part",
        LABEL = "part",
        DATA_TYPE = DATA_TYPES$INTEGER,
        SCALE_LEVEL = SCALE_LEVELS$NOMINAL,
        MISSING_LIST_TABLE = NA_character_,
        MISSING_LIST = NA_character_,
        JUMP_LIST = NA_character_,
        stringsAsFactors = FALSE
      )
      segment_level <- data.frame(
        STUDY_SEGMENT = "SegA",
        SEGMENT_PART_VARS = "part",
        stringsAsFactors = FALSE
      )

      suppressMessages(com_qualified_segment_missingness(
        study_data = study_data,
        meta_data = meta_data,
        meta_data_segment = segment_level,
        label_col = VAR_NAMES,
        expected_observations = "ALL"
      ))
    }), env = cache),
    "No missing-match-table.*SegA"
  )
})

test_that(
  "com_qualified_segment_missingness reports unmet AAPOR preconditions",
  {
    skip_on_cran()

    cache <- new.env(parent = emptyenv())
    captured <- new.env(parent = emptyenv())
    captured$warnings <- character(0)
    result <- withCallingHandlers(
      with_dataframe_environment(quote({
        study_data <- data.frame(part = c(9, 9, 9))
        meta_data <- data.frame(
          VAR_NAMES = "part",
          LABEL = "part",
          DATA_TYPE = DATA_TYPES$INTEGER,
          SCALE_LEVEL = SCALE_LEVELS$NOMINAL,
          MISSING_LIST_TABLE = "aapor_codes",
          MISSING_LIST = NA_character_,
          JUMP_LIST = NA_character_,
          stringsAsFactors = FALSE
        )
        segment_level <- data.frame(
          STUDY_SEGMENT = "SegA",
          SEGMENT_PART_VARS = "part",
          stringsAsFactors = FALSE
        )
        aapor_codes <- data.frame(
          CODE_VALUE = "9",
          CODE_LABEL = "not contacted",
          CODE_INTERPRET = "NC",
          stringsAsFactors = FALSE
        )
        prep_add_data_frames(aapor_codes = aapor_codes)
        suppressMessages(com_qualified_segment_missingness(
          study_data = study_data,
          meta_data = meta_data,
          meta_data_segment = segment_level,
          label_col = VAR_NAMES,
          expected_observations = "ALL"
        ))
      }), env = cache),
      warning = function(w) {
        captured$warnings <- c(captured$warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    )

    expect_true(any(grepl(
      "Preconditions to generate Nonresponse Rate 1",
      captured$warnings,
      fixed = TRUE
    )))
    expect_true(is.na(result$SegmentTable$PCT_com_qum_nonresp))
    expect_true(is.na(result$SegmentTable$PCT_com_qum_refusal))
    expect_equal(as.integer(result$SegmentTable$N), 3L)
  }
)

test_that(
  "com_qualified_segment_missingness skips invalid AAPOR code tables",
  {
    skip_on_cran()

    cache <- new.env(parent = emptyenv())
    captured <- new.env(parent = emptyenv())
    captured$warnings <- character(0)
    result <- withCallingHandlers(
      with_dataframe_environment(quote({
        study_data <- data.frame(part = c(1, 2, 3))
        meta_data <- data.frame(
          VAR_NAMES = "part",
          LABEL = "part",
          DATA_TYPE = DATA_TYPES$INTEGER,
          SCALE_LEVEL = SCALE_LEVELS$NOMINAL,
          MISSING_LIST_TABLE = "aapor_codes",
          MISSING_LIST = NA_character_,
          JUMP_LIST = NA_character_,
          stringsAsFactors = FALSE
        )
        segment_level <- data.frame(
          STUDY_SEGMENT = "SegA",
          SEGMENT_PART_VARS = "part",
          stringsAsFactors = FALSE
        )
        aapor_codes <- data.frame(
          CODE_VALUE = "1",
          CODE_LABEL = "bad",
          CODE_INTERPRET = "not-aapor",
          stringsAsFactors = FALSE
        )
        prep_add_data_frames(aapor_codes = aapor_codes)
        suppressMessages(com_qualified_segment_missingness(
          study_data = study_data,
          meta_data = meta_data,
          meta_data_segment = segment_level,
          label_col = VAR_NAMES,
          expected_observations = "ALL"
        ))
      }), env = cache),
      warning = function(w) {
        captured$warnings <- c(captured$warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    )

    expect_true(any(grepl(
      "Could not load missing-match-table",
      captured$warnings,
      fixed = TRUE
    )))
    expect_equal(nrow(result$SegmentTable), 0L)
    expect_equal(nrow(result$SegmentData), 0L)
  }
)
