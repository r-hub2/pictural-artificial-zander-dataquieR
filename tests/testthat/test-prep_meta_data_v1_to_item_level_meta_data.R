test_that(
  "prep_meta_data_v1_to_item_level_meta_data normalizes legacy columns",
  {
    skip_on_cran()

    meta_data <- data.frame(
      var_names = c("id", "visit_dt", "device"),
      data_type = DATA_TYPES$INTEGER,
      data_entry_type = c("manual", "auto", NA_character_),
      key_study_segment = c(NA_character_, "id", NA_character_),
      key_datetime = c(NA_character_, "visit_dt", NA_character_),
      key_device = c(NA_character_, NA_character_, "device"),
      missing_list = SPLIT_CHAR,
      jump_list = SPLIT_CHAR,
      stringsAsFactors = FALSE
    )
    names(meta_data) <- c(
      VAR_NAMES,
      DATA_TYPE,
      DATA_ENTRY_TYPE,
      KEY_STUDY_SEGMENT,
      KEY_DATETIME,
      "KEY_DEVICE",
      MISSING_LIST,
      JUMP_LIST
    )

    normalized <- suppressMessages(prep_meta_data_v1_to_item_level_meta_data(
      meta_data,
      verbose = FALSE,
      label_col = LABEL
    ))

    expect_equal(normalized[[LABEL]], normalized[[VAR_NAMES]])
    expect_false(DATA_ENTRY_TYPE %in% names(normalized))
    expect_equal(normalized[[END_DIGIT_CHECK]], c(TRUE, FALSE, FALSE))
    expect_equal(normalized[[PART_VAR]], c(NA_character_, "id", NA_character_))
    expect_equal(normalized[[STUDY_SEGMENT]], c(
      NA_character_, "id",
      NA_character_
    ))
    expect_equal(normalized[[TIME_VAR]], c(
      NA_character_, "visit_dt",
      NA_character_
    ))
    expect_equal(
      normalized[["GROUP_VAR_DEVICE"]],
      c(NA_character_, NA_character_, "device")
    )
    expect_identical(attr(normalized, "version", exact = TRUE), 2)
    expect_true(isTRUE(attr(normalized, "normalized", exact = TRUE)))
  }
)

test_that(
  "prep_meta_data_v1_to_item_level_meta_data maps legacy segment labels",
  {
    skip_on_cran()

    meta_data <- data.frame(
      VAR_NAMES = c("id", "visit"),
      LABEL = c("Identifier", "Visit label"),
      DATA_TYPE = DATA_TYPES$INTEGER,
      KEY_STUDY_SEGMENT = c(NA_character_, "Identifier"),
      DATA_ENTRY_TYPE = c("manual", "auto"),
      END_DIGIT_CHECK = c(FALSE, TRUE),
      MISSING_LIST = SPLIT_CHAR,
      JUMP_LIST = SPLIT_CHAR,
      stringsAsFactors = FALSE
    )
    names(meta_data) <- c(
      VAR_NAMES,
      LABEL,
      DATA_TYPE,
      KEY_STUDY_SEGMENT,
      DATA_ENTRY_TYPE,
      END_DIGIT_CHECK,
      MISSING_LIST,
      JUMP_LIST
    )

    normalized <- suppressMessages(suppressWarnings(
      prep_meta_data_v1_to_item_level_meta_data(
        meta_data,
        verbose = FALSE,
        label_col = LONG_LABEL
      )
    ))

    expect_false(DATA_ENTRY_TYPE %in% names(normalized))
    expect_equal(normalized[[END_DIGIT_CHECK]], c(FALSE, TRUE))
    expect_equal(normalized[[PART_VAR]], c(NA_character_, "id"))
    expect_equal(
      normalized[[STUDY_SEGMENT]],
      c(NA_character_, "Identifier")
    )
    expect_equal(normalized[[LONG_LABEL]], normalized[[LABEL]])
    expect_identical(attr(normalized, "version", exact = TRUE), 2)
  }
)

test_that(
  "prep_meta_data_v1_to_item_level_meta_data applies cause labels",
  {
    skip_on_cran()

    meta_data <- data.frame(
      VAR_NAMES = "item",
      DATA_TYPE = DATA_TYPES$INTEGER,
      MISSING_LIST = "-1",
      JUMP_LIST = SPLIT_CHAR,
      stringsAsFactors = FALSE
    )
    cause_label_df <- data.frame(
      CODE_VALUE = "-1",
      CODE_LABEL = "not answered",
      stringsAsFactors = FALSE
    )

    normalized <- suppressMessages(prep_meta_data_v1_to_item_level_meta_data(
      meta_data,
      verbose = FALSE,
      label_col = VAR_NAMES,
      cause_label_df = cause_label_df
    ))

    expect_equal(normalized[[MISSING_LIST]], "-1 = not answered")
    expect_match(normalized[[MISSING_LIST_TABLE]], "MISSING_LIST_TABLE_")
    expect_identical(attr(normalized, "version", exact = TRUE), 2)
    expect_true(isTRUE(attr(normalized, "normalized", exact = TRUE)))
  }
)

test_that(
  "prep_meta_data_v1_to_item_level_meta_data merges missing-code tables",
  {
    skip_on_cran()

    captured <- new.env(parent = emptyenv())
    captured$warnings <- character()

    normalized <- with_dataframe_environment(quote({
      existing <- data.frame(
        CODE_VALUE = c("-1", "-2", "-3"),
        CODE_LABEL = c("Table missing", "Table jump", "Table extra"),
        CODE_INTERPRET = c("NE", "NE", "MISS"),
        stringsAsFactors = FALSE
      )
      prep_add_data_frames(
        existing = existing,
        existing_Score = data.frame(x = 1),
        existing_Score_0 = data.frame(x = 2)
      )
      meta_data <- data.frame(
        VAR_NAMES = "score",
        LABEL = "Score",
        DATA_TYPE = DATA_TYPES$INTEGER,
        MISSING_LIST = "-1 = MISSING -1",
        JUMP_LIST = "-2 = JUMP -2",
        MISSING_LIST_TABLE = "existing",
        stringsAsFactors = FALSE
      )
      names(meta_data) <- c(
        VAR_NAMES,
        LABEL,
        DATA_TYPE,
        MISSING_LIST,
        JUMP_LIST,
        MISSING_LIST_TABLE
      )

      withCallingHandlers(
        suppressMessages(prep_meta_data_v1_to_item_level_meta_data(
          meta_data,
          verbose = FALSE,
          label_col = LABEL
        )),
        warning = function(w) {
          captured$warnings <- c(captured$warnings, conditionMessage(w))
          invokeRestart("muffleWarning")
        }
      )
    }))

    expect_equal(normalized[[MISSING_LIST_TABLE]], "existing_Score_1")
    expect_equal(normalized[[MISSING_LIST]], "-3 = Table extra")
    expect_equal(
      normalized[[JUMP_LIST]],
      "-1 = Table missing | -2 = Table jump"
    )
    expect_true(any(grepl(
      "Code classes in old and new missing code settings",
      captured$warnings,
      fixed = TRUE
    )))
  }
)

test_that(
  "prep_meta_data_v1_to_item_level_meta_data keeps v2 metadata idempotent",
  {
    skip_on_cran()

    meta_data <- data.frame(
      VAR_NAMES = "score",
      DATA_TYPE = DATA_TYPES$INTEGER,
      LABEL = "Score",
      stringsAsFactors = FALSE
    )
    names(meta_data) <- c(VAR_NAMES, DATA_TYPE, LABEL)
    attr(meta_data, "version") <- 2
    attr(meta_data, "normalized") <- TRUE

    normalized <- prep_meta_data_v1_to_item_level_meta_data(
      meta_data,
      verbose = FALSE,
      label_col = LABEL
    )

    expect_equal(normalized[[VAR_NAMES]], "score")
    expect_equal(normalized[[LABEL]], "Score")
    expect_identical(attr(normalized, "version", exact = TRUE), 2)
    expect_true(isTRUE(attr(normalized, "normalized", exact = TRUE)))
  }
)

test_that(
  "prep_meta_data_v1_to_item_level_meta_data treats segment labels as segments",
  {
    skip_on_cran()

    meta_data <- data.frame(
      VAR_NAMES = c("score", "group"),
      DATA_TYPE = DATA_TYPES$INTEGER,
      LABEL = c("Score", "Group"),
      KEY_STUDY_SEGMENT = c("Baseline", "Follow-up"),
      stringsAsFactors = FALSE
    )
    names(meta_data) <- c(VAR_NAMES, DATA_TYPE, LABEL, KEY_STUDY_SEGMENT)

    expect_message(
      normalized <- prep_meta_data_v1_to_item_level_meta_data(
        meta_data,
        verbose = TRUE,
        label_col = LABEL
      ),
      "STUDY_SEGMENT"
    )

    expect_equal(normalized[[STUDY_SEGMENT]], c("Baseline", "Follow-up"))
    expect_false(KEY_STUDY_SEGMENT %in% names(normalized))
    expect_false(PART_VAR %in% names(normalized))
    expect_identical(attr(normalized, "version", exact = TRUE), 2)
  }
)

test_that(
  "internal metadata normalization rejects pre-v2 input",
  {
    skip_on_cran()

    meta_data <- data.frame(
      VAR_NAMES = "score",
      DATA_TYPE = DATA_TYPES$INTEGER,
      LABEL = "Score",
      stringsAsFactors = FALSE
    )
    names(meta_data) <- c(VAR_NAMES, DATA_TYPE, LABEL)

    expect_error(
      .util_internal_normalize_meta_data(meta_data, label_col = LABEL),
      "below v2"
    )
  }
)

test_that(
  "prep_meta_data_v1_to_item_level_meta_data warns on end-digit conflicts",
  {
    skip_on_cran()

    meta_data <- data.frame(
      VAR_NAMES = "score",
      DATA_TYPE = DATA_TYPES$INTEGER,
      DATA_ENTRY_TYPE = "auto",
      END_DIGIT_CHECK = TRUE,
      MISSING_LIST = SPLIT_CHAR,
      JUMP_LIST = SPLIT_CHAR,
      stringsAsFactors = FALSE
    )
    names(meta_data) <- c(
      VAR_NAMES,
      DATA_TYPE,
      DATA_ENTRY_TYPE,
      END_DIGIT_CHECK,
      MISSING_LIST,
      JUMP_LIST
    )

    expect_warning(
      normalized <- prep_meta_data_v1_to_item_level_meta_data(
        meta_data,
        verbose = FALSE,
        label_col = LABEL
      ),
      "will prefer"
    )

    expect_false(DATA_ENTRY_TYPE %in% names(normalized))
    expect_true(normalized[[END_DIGIT_CHECK]])
  }
)

test_that(
  "prep_meta_data_v1_to_item_level_meta_data maps mixed PART_VAR labels",
  {
    skip_on_cran()

    meta_data <- data.frame(
      VAR_NAMES = c("id", "score", "free"),
      DATA_TYPE = DATA_TYPES$INTEGER,
      LABEL = c("Identifier", "Score", "Free"),
      PART_VAR = c("Identifier", "id", "unknown"),
      MISSING_LIST = SPLIT_CHAR,
      JUMP_LIST = SPLIT_CHAR,
      stringsAsFactors = FALSE
    )
    names(meta_data) <- c(
      VAR_NAMES,
      DATA_TYPE,
      LABEL,
      PART_VAR,
      MISSING_LIST,
      JUMP_LIST
    )

    expect_message(
      normalized <- prep_meta_data_v1_to_item_level_meta_data(
        meta_data,
        verbose = FALSE,
        label_col = LABEL
      ),
      "Not all entries"
    )

    expect_equal(normalized[[PART_VAR]], c("id", "id", "unknown"))
    expect_identical(attr(normalized, "version", exact = TRUE), 2)
  }
)

test_that(
  "internal metadata normalization fills missing labels from variable names",
  {
    skip_on_cran()

    meta_data <- data.frame(
      VAR_NAMES = c("score", "group"),
      DATA_TYPE = DATA_TYPES$INTEGER,
      LABEL = c("", "Group label"),
      LONG_LABEL = c("", ""),
      MISSING_LIST = SPLIT_CHAR,
      JUMP_LIST = SPLIT_CHAR,
      stringsAsFactors = FALSE
    )
    names(meta_data) <- c(
      VAR_NAMES,
      DATA_TYPE,
      LABEL,
      LONG_LABEL,
      MISSING_LIST,
      JUMP_LIST
    )
    attr(meta_data, "version") <- 2

    normalized <- suppressWarnings(.util_internal_normalize_meta_data(
      meta_data,
      label_col = LONG_LABEL
    ))

    expect_equal(normalized[[LABEL]], c("score", "Group label"))
    expect_equal(normalized[[LONG_LABEL]], c("score", "Group label"))
    expect_true(isTRUE(attr(normalized, "normalized", exact = TRUE)))
  }
)

test_that(
  "prep_meta_data_v1_to_item_level_meta_data maps custom segment labels",
  {
    skip_on_cran()

    meta_data <- data.frame(
      VAR_NAMES = c("part", "visit"),
      DATA_TYPE = DATA_TYPES$INTEGER,
      LABEL = c("Participant", "Visit label"),
      LONG_LABEL = c("Participant long", "Visit long"),
      KEY_STUDY_SEGMENT = c(NA_character_, "Participant long"),
      MISSING_LIST = SPLIT_CHAR,
      JUMP_LIST = SPLIT_CHAR,
      stringsAsFactors = FALSE
    )
    names(meta_data) <- c(
      VAR_NAMES,
      DATA_TYPE,
      LABEL,
      LONG_LABEL,
      KEY_STUDY_SEGMENT,
      MISSING_LIST,
      JUMP_LIST
    )

    expect_message(
      normalized <- prep_meta_data_v1_to_item_level_meta_data(
        meta_data,
        verbose = FALSE,
        label_col = LONG_LABEL
      ),
      "Could convert"
    )

    expect_equal(normalized[[PART_VAR]], c(NA_character_, "part"))
    expect_equal(normalized[[STUDY_SEGMENT]], c(
      NA_character_,
      "Participant"
    ))
    expect_identical(attr(normalized, "version", exact = TRUE), 2)
  }
)

test_that(
  "prep_meta_data_v1_to_item_level_meta_data rejects mixed segment semantics",
  {
    skip_on_cran()

    meta_data <- data.frame(
      VAR_NAMES = c("part", "visit"),
      DATA_TYPE = DATA_TYPES$INTEGER,
      LABEL = c("Participant", "Visit label"),
      KEY_STUDY_SEGMENT = c("part", "Segment literal"),
      MISSING_LIST = SPLIT_CHAR,
      JUMP_LIST = SPLIT_CHAR,
      stringsAsFactors = FALSE
    )
    names(meta_data) <- c(
      VAR_NAMES,
      DATA_TYPE,
      LABEL,
      KEY_STUDY_SEGMENT,
      MISSING_LIST,
      JUMP_LIST
    )

    expect_message(
      expect_error(
        prep_meta_data_v1_to_item_level_meta_data(
          meta_data,
          verbose = FALSE,
          label_col = LABEL
        ),
        "cannot"
      ),
      "Not all entries"
    )
  }
)
