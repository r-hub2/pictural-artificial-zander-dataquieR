skip_on_cran()

test_that("util_int_datastructure_match_type classifies ID-set relations", {
  expect_identical(
    util_int_datastructure_match_type(c("a", "b"), c("b", "a")),
    "exact"
  )
  expect_identical(
    util_int_datastructure_match_type(c("a"), c("a", "b")),
    "subset"
  )
  expect_identical(
    util_int_datastructure_match_type(c("a", "b"), c("a")),
    "superset"
  )
  expect_identical(
    util_int_datastructure_match_type(c("a", "x"), c("a", "b")),
    "mismatch"
  )
})

test_that("util_int_datastructure builds typed empty dataframe elements", {
  result <- util_int_datastructure_empty_dataframe_element()

  expect_s3_class(result, "data.frame")
  expect_named(
    result,
    c(
      "Level",
      "DF_NAME",
      "NUM_int_sts_element",
      "PCT_int_sts_element",
      "GRADING"
    )
  )
  expect_length(result, 5)
  expect_length(result[["Level"]], 0)
  expect_length(result[["DF_NAME"]], 0)
  expect_true(all(vapply(
    result[c("NUM_int_sts_element", "PCT_int_sts_element", "GRADING")],
    is.numeric,
    FUN.VALUE = logical(1)
  )))
})

test_that("util_int_datastructure duplicate result builders summarize counts", {
  duplicate_data <- util_int_datastructure_duplicate_data(
    check = "IDs",
    level_col = "Data frame",
    level = "df_a",
    n_total = 4,
    n_unique = 3,
    id_vars = c("id", "visit")
  )

  expect_identical(duplicate_data[["Any duplicates"]], TRUE)
  expect_identical(duplicate_data[["Number of duplicates"]], 1)
  expect_equal(duplicate_data[["Percentage of duplicates"]], 25)
  expect_identical(duplicate_data[["GRADING"]], 1L)
  expect_identical(duplicate_data[["ID Vars"]], "id | visit")

  table <- util_int_datastructure_duplicate_table(
    duplicate_data,
    level = "Dataframe",
    level_col = DF_NAME,
    num_col = "NUM_int_sts_dupl_ids",
    pct_col = "PCT_int_sts_dupl_ids",
    source_col = "Data frame"
  )

  expect_identical(table[[DF_NAME]], "df_a")
  expect_identical(table$Level, "Dataframe")
  expect_identical(table$NUM_int_sts_dupl_ids, 1)
  expect_equal(table$PCT_int_sts_dupl_ids, 25)

  missing <- util_int_datastructure_missing_duplicate_data("Segment", "LAB")
  expect_true(is.na(missing[["Any duplicates"]]))
  expect_true(is.na(missing[["GRADING"]]))
})

test_that(
  "util_int_datastructure duplicate content result handles empty input",
  {
    empty <- util_int_datastructure_duplicate_content_result(
      data_by_level = list(),
      level_col = "Data frame",
      check = "Duplicates",
      num_col = "NUM_int_sts_dupl_content",
      pct_col = "PCT_int_sts_dupl_content"
    )

    expect_identical(empty$data, setNames(list(), character()))
    expect_s3_class(empty$table, "data.frame")
    expect_equal(nrow(empty$table), 0)

    result <- util_int_datastructure_duplicate_content_result(
      data_by_level = list(
        df_a = data.frame(x = c(1, 1, 2), y = c("a", "a", "b")),
        df_b = data.frame(x = c(1, 2), y = c("a", "b"))
      ),
      level_col = "Data frame",
      check = "Duplicates",
      num_col = "NUM_int_sts_dupl_content",
      pct_col = "PCT_int_sts_dupl_content",
      table_level = "Dataframe",
      table_level_col = DF_NAME,
      source_col = "Data frame"
    )

    expect_identical(result$table[[DF_NAME]], c("df_a", "df_b"))
    expect_equal(result$table$NUM_int_sts_dupl_content, c(1, 0))
    expect_equal(result$table$PCT_int_sts_dupl_content, c(33.333, 0))
    expect_equal(result$table$GRADING, c(1, 0))
  }
)

test_that("util_int_datastructure metadata helpers filter and align values", {
  meta_data <- data.frame(
    DF_NAME = c("df_a", "df_b", "df_c", "df_d"),
    DF_UNIQUE_ROWS = c("true", "false", "no_id", NA_character_),
    stringsAsFactors = FALSE
  )

  filtered <- util_int_datastructure_unique_rows_metadata(
    meta_data,
    DF_UNIQUE_ROWS,
    filter_metadata = TRUE
  )
  expect_identical(filtered[[DF_NAME]], c("df_a", "df_c"))
  expect_identical(
    util_int_datastructure_unique_rows_metadata(
      meta_data,
      DF_UNIQUE_ROWS,
      filter_metadata = FALSE
    ),
    meta_data
  )

  expect_identical(
    util_int_datastructure_named_arg(
      c(df_b = "B", df_a = "A"),
      c("df_a", "df_b")
    ),
    c("A", "B")
  )
})

test_that("util_int_datastructure legacy mode validation is explicit", {
  expect_true(util_int_datastructure_use_metadata(
    meta_arg = "meta_data_dataframe",
    has_meta_arg = TRUE,
    missing_explicit_args = c(TRUE, TRUE),
    explicit_args = c("id_vars_list", "identifier_name_list")
  ))

  expect_false(util_int_datastructure_use_metadata(
    meta_arg = "meta_data_dataframe",
    has_meta_arg = FALSE,
    missing_explicit_args = c(FALSE, FALSE),
    explicit_args = c("id_vars_list", "identifier_name_list")
  ))

  expect_error(
    util_int_datastructure_use_metadata(
      meta_arg = "meta_data_dataframe",
      has_meta_arg = TRUE,
      missing_explicit_args = c(FALSE, TRUE),
      explicit_args = c("id_vars_list", "identifier_name_list")
    ),
    "not supported"
  )
  expect_error(
    util_int_datastructure_use_metadata(
      meta_arg = "meta_data_dataframe",
      has_meta_arg = FALSE,
      missing_explicit_args = c(FALSE, TRUE),
      explicit_args = c("id_vars_list", "identifier_name_list")
    ),
    "also miss"
  )
})

test_that("util_int_datastructure validates duplicate-content row settings", {
  expect_silent(util_int_datastructure_validate_unique_rows(
    c("true", " false ", "no_id", "t", "f", "")
  ))

  expect_error(
    util_int_datastructure_validate_unique_rows("maybe"),
    "must match the predicate"
  )
  expect_error(
    util_int_datastructure_validate_unique_rows(NA_character_),
    "must not contain NAs"
  )
})

test_that(
  "util_find_duplicated_rows ignores empty IDs and allowed repetitions",
  {
    duplicated_rows <- util_find_duplicated_rows(
      data.frame(
        id = c("A", "A", "A", "B", "", NA_character_),
        visit = c(1, 1, 1, 2, 3, 4),
        value = seq_len(6)
      ),
      id_vars = c("id", "visit"),
      repeptitions = 2
    )

    expect_identical(nrow(duplicated_rows), 1L)
    expect_identical(as.character(duplicated_rows$id), "A")
    expect_identical(as.character(duplicated_rows$visit), "1")
    expect_identical(duplicated_rows$Freq, 3L)
    expect_identical(
      unlist(duplicated_rows$which, use.names = FALSE),
      "1 | 2 | 3"
    )
    expect_identical(duplicated_rows$unexp_reps, 1)
  }
)

test_that("util_int_datastructure record-set helpers build shared outputs", {
  record_set <- util_int_datastructure_record_set_data(
    level_col = "Data frame",
    level = "df_a",
    data_values = c("A", "B", "C"),
    metadata_ids = c("A", "B"),
    unexpected_ids = "C",
    match_expected = "subset",
    mismatch_denominator = 2
  )

  expect_identical(record_set[["Data frame"]], "df_a")
  expect_true(record_set[["Unexpected records in set?"]])
  expect_identical(record_set[["Number of records in data"]], 3L)
  expect_identical(record_set[["Number of records in metadata"]], 2L)
  expect_identical(record_set[["Number of mismatches"]], 1L)
  expect_equal(record_set[["Percentage of mismatches"]], 50)
  expect_identical(record_set[["Actual match type"]], "superset")
  expect_identical(record_set[["GRADING"]], 1)

  other <- util_int_datastructure_unexpected_ids(
    level_col = "Dataframe",
    level = "df_a",
    unexpected_ids = c("C", ""),
    data_values = c("A", "C", "B", ""),
    metadata_ids = c("A", "B")
  )
  expect_identical(other$Dataframe, "df_a")
  expect_identical(other$UnexpectedID, "C")
  expect_identical(other$Line, "2")

  empty <- util_int_datastructure_empty_record_set("Segment")
  expect_identical(
    names(empty),
    c(
      "Level", "NUM_int_sts_setrc", "PCT_int_sts_setrc",
      "GRADING"
    )
  )
  expect_identical(nrow(empty), 0L)
})

test_that("util_int_datastructure dataframe record sets skip ID guardrails", {
  meta_data_dataframe <- data.frame(
    DF_NAME = c("no_id", "multi_id"),
    DF_ID_VARS = c("", "id | visit"),
    DF_RECORD_CHECK = c("exact", "subset"),
    stringsAsFactors = FALSE
  )
  meta_data_dataframe[[DF_ID_REF_TABLE]] <- I(list(
    data.frame(ID = "A", stringsAsFactors = FALSE),
    data.frame(ID = "A", stringsAsFactors = FALSE)
  ))
  study_data_list <- list(
    no_id = data.frame(id = "A", stringsAsFactors = FALSE),
    multi_id = data.frame(
      id = "A",
      visit = 1L,
      stringsAsFactors = FALSE
    )
  )

  warnings <- capture_warnings(
    result <- util_int_datastructure_df_record_set(
      meta_data_dataframe,
      study_data_list
    )
  )

  expect_true(any(grepl("No .+DF_ID_VARS", warnings)))
  expect_true(any(grepl("multiple IDs", warnings, fixed = TRUE)))
  expect_equal(nrow(result$DataframeTable), 0L)
  expect_equal(nrow(result$Other), 0L)
})

test_that("util_int_datastructure_add_summary_columns copies available pairs", {
  source_data <- data.frame(
    `Duplicate IDs (Number)` = 2,
    `Duplicate IDs (Percentage (0 to 100))` = 25,
    `Duplicate IDs (Grading)` = 1,
    check.names = FALSE
  )
  summary_data <- util_int_datastructure_add_summary_columns(
    summary_data = data.frame(Segment = "LAB"),
    source_data = source_data,
    label = "Duplicate IDs"
  )

  expect_identical(as.vector(summary_data[["Duplicate IDs N (%)"]]), "2 (25)")
  expect_identical(summary_data[["Duplicate IDs (Grading)"]], 1)

  unchanged <- util_int_datastructure_add_summary_columns(
    summary_data = data.frame(Segment = "LAB"),
    source_data = data.frame(),
    label = "Duplicate IDs"
  )
  expect_identical(names(unchanged), "Segment")
})

test_that(
  "util_int_datastructure dataframe record-set helper handles inapplicable IDs",
  {
    meta_data_dataframe <- data.frame(
      DF_NAME = c("df_a", "df_b"),
      DF_RECORD_CHECK = c("exact", "subset"),
      DF_ID_VARS = c(NA_character_, "id"),
      stringsAsFactors = FALSE
    )
    meta_data_dataframe[[DF_ID_REF_TABLE]] <- I(list(
      data.frame(id = "A", stringsAsFactors = FALSE),
      data.frame(id = c("B", "C"), stringsAsFactors = FALSE)
    ))
    study_data_list <- list(
      df_a = data.frame(id = "A"),
      df_b = data.frame(id = "B", visit = 1)
    )

    result <- suppressWarnings(util_int_datastructure_df_record_set(
      meta_data_dataframe,
      study_data_list,
      id_vars_list = list(df_a = character(0), df_b = "id")
    ))

    expect_equal(result$DataframeTable$DF_NAME, "df_b")
    expect_equal(result$DataframeTable$NUM_int_sts_setrc, 0)
    expect_equal(result$DataframeTable$PCT_int_sts_setrc, 0)
    expect_equal(nrow(result$Other), 0)
  }
)

test_that(
  paste(
    "util_int_datastructure dataframe record-set helper handles",
    "no applicable IDs"
  ),
  {
    meta_data_dataframe <- data.frame(
      DF_NAME = c("df_a", "df_b"),
      DF_RECORD_CHECK = c("exact", "subset"),
      DF_ID_VARS = c(NA_character_, NA_character_),
      stringsAsFactors = FALSE
    )
    meta_data_dataframe[[DF_ID_REF_TABLE]] <- I(list(
      data.frame(id = "A", stringsAsFactors = FALSE),
      data.frame(id = "B", stringsAsFactors = FALSE)
    ))
    study_data_list <- list(
      df_a = data.frame(id = "A"),
      df_b = data.frame(id = "B")
    )

    result <- suppressWarnings(util_int_datastructure_df_record_set(
      meta_data_dataframe,
      study_data_list,
      id_vars_list = list(df_a = character(0), df_b = character(0))
    ))

    expect_equal(nrow(result$DataframeTable), 0)
    expect_named(result$DataframeTable, c(
      "Level",
      DF_NAME,
      "NUM_int_sts_setrc",
      "PCT_int_sts_setrc",
      "GRADING"
    ))
    expect_equal(nrow(result$Other), 0)
  }
)

test_that(
  "util_int_datastructure dataframe record-set skips multiple IDs",
  {
    meta_data_dataframe <- data.frame(
      DF_NAME = "df_multi",
      DF_RECORD_CHECK = "exact",
      DF_ID_VARS = "id | visit",
      stringsAsFactors = FALSE
    )
    meta_data_dataframe[[DF_ID_REF_TABLE]] <- I(list(data.frame(
      id = "A",
      visit = 1L,
      stringsAsFactors = FALSE
    )))
    study_data_list <- list(
      df_multi = data.frame(id = "A", visit = 1L)
    )

    expect_warning(
      result <- util_int_datastructure_df_record_set(
        meta_data_dataframe,
        study_data_list,
        id_vars_list = list(df_multi = c("id", "visit"))
      ),
      "multiple IDs is not currently supported"
    )

    expect_equal(nrow(result$DataframeTable), 0)
    expect_equal(nrow(result$Other), 0)
  }
)

test_that("util_int_datastructure handles missing dataframe duplicate IDs", {
  meta_data_dataframe <- data.frame(
    DF_NAME = "df_missing",
    DF_ID_VARS = "missing_id",
    DF_UNIQUE_ID = "1",
    stringsAsFactors = FALSE
  )
  study_data_list <- list(
    df_missing = data.frame(value = 1L)
  )

  expect_message(
    duplicates <- util_int_datastructure_df_duplicate_ids(
      meta_data_dataframe,
      study_data_list,
      id_vars_list = list(df_missing = "missing_id")
    ),
    "None of the ID variables"
  )
  expect_equal(duplicates$DataframeTable[[DF_NAME]], "df_missing")
  expect_true(is.na(duplicates$DataframeTable$NUM_int_sts_dupl_ids))
  expect_null(duplicates$Other$df_missing)
})

test_that("util_int_datastructure dataframe wrappers use cached metadata", {
  duplicate_ids <- with_dataframe_environment(quote({
    df_a <- data.frame(
      id = c("A", "A", "B"),
      value = c(1L, 2L, 3L),
      stringsAsFactors = FALSE
    )
    dataframe_level <- util_int_datastructure_df_metadata(
      identifier_name_list = "df_a",
      id_vars_list = list(df_a = "id"),
      repetitions = c(df_a = "1")
    )
    prep_add_data_frames(df_a = df_a, dataframe_level = dataframe_level)
    util_int_duplicate_ids_dataframe()
  }))

  expect_equal(duplicate_ids$DataframeTable[[DF_NAME]], "df_a")
  expect_equal(duplicate_ids$DataframeTable$NUM_int_sts_dupl_ids, 1)
  expect_equal(duplicate_ids$Other$df_a[[1]], "1 | 2")

  duplicate_content <- with_dataframe_environment(quote({
    df_a <- data.frame(
      id = c("A", "A", "B"),
      value = c(1L, 1L, 2L),
      stringsAsFactors = FALSE
    )
    dataframe_level <- util_int_datastructure_df_metadata(
      identifier_name_list = "df_a",
      id_vars_list = list(df_a = "id"),
      unique_rows = c(df_a = "true")
    )
    prep_add_data_frames(df_a = df_a, dataframe_level = dataframe_level)
    util_int_duplicate_content_dataframe()
  }))

  expect_equal(duplicate_content$DataframeTable[[DF_NAME]], "df_a")
  expect_equal(
    duplicate_content$DataframeTable$NUM_int_sts_dupl_content,
    1
  )

  record_set <- with_dataframe_environment(quote({
    df_a <- data.frame(id = c("A", "B"), stringsAsFactors = FALSE)
    valid_ids <- data.frame(ID = c("A", "B"), stringsAsFactors = FALSE)
    dataframe_level <- util_int_datastructure_df_metadata(
      identifier_name_list = "df_a",
      id_vars_list = list(df_a = "id"),
      valid_id_table_list = list(df_a = valid_ids),
      meta_data_record_check_list = c(df_a = "subset")
    )
    prep_add_data_frames(df_a = df_a, dataframe_level = dataframe_level)
    suppressMessages(util_int_unexp_records_set_dataframe())
  }))

  expect_equal(nrow(record_set$DataframeTable), 0L)
  expect_equal(nrow(record_set$Other), 0L)
})

test_that("util_int_datastructure segment wrappers use cached metadata", {
  duplicate_ids <- with_dataframe_environment(quote({
    study_data <- data.frame(
      id = c("A", "A", "B"),
      value = c(1L, 2L, 3L),
      stringsAsFactors = FALSE
    )
    item_level <- prep_create_meta(
      VAR_NAMES = c("id", "value"),
      LABEL = c("ID", "Value"),
      DATA_TYPE = c(DATA_TYPES$STRING, DATA_TYPES$INTEGER),
      SCALE_LEVEL = c(SCALE_LEVELS$NOMINAL, SCALE_LEVELS$RATIO),
      STUDY_SEGMENT = "LAB",
      MISSING_LIST = SPLIT_CHAR,
      JUMP_LIST = SPLIT_CHAR
    )
    segment_level <- util_int_datastructure_segment_metadata(
      identifier_name_list = "LAB",
      id_vars_list = list(LAB = "id"),
      repetitions = c(LAB = "1")
    )
    prep_add_data_frames(segment_level = segment_level)
    testthat::with_mocked_bindings(
      prep_prepare_dataframes = function(...) {
        assign("ds1", study_data, parent.frame())
        invisible(study_data)
      },
      suppressMessages(util_int_duplicate_ids_segment(
        study_data = study_data,
        meta_data = item_level
      ))
    )
  }))

  expect_equal(duplicate_ids$SegmentTable$Segment, "LAB")
  expect_equal(duplicate_ids$SegmentTable$NUM_int_sts_dupl_ids, 1)
  expect_equal(duplicate_ids$Other$LAB[[1]], "1 | 2")

  duplicate_content <- with_dataframe_environment(quote({
    study_data <- data.frame(
      id = c("A", "A", "B"),
      value = c(1L, 1L, 2L),
      stringsAsFactors = FALSE
    )
    item_level <- prep_create_meta(
      VAR_NAMES = c("id", "value"),
      LABEL = c("ID", "Value"),
      DATA_TYPE = c(DATA_TYPES$STRING, DATA_TYPES$INTEGER),
      SCALE_LEVEL = c(SCALE_LEVELS$NOMINAL, SCALE_LEVELS$RATIO),
      STUDY_SEGMENT = "LAB",
      MISSING_LIST = SPLIT_CHAR,
      JUMP_LIST = SPLIT_CHAR
    )
    segment_level <- util_int_datastructure_segment_metadata(
      identifier_name_list = "LAB",
      id_vars_list = list(LAB = "id"),
      unique_rows = c(LAB = "true")
    )
    prep_add_data_frames(segment_level = segment_level)
    testthat::with_mocked_bindings(
      prep_prepare_dataframes = function(...) {
        assign("ds1", study_data, parent.frame())
        invisible(study_data)
      },
      suppressMessages(util_int_duplicate_content_segment(
        study_data = study_data,
        meta_data = item_level
      ))
    )
  }))

  expect_equal(duplicate_content$SegmentTable$Segment, "LAB")
  expect_equal(
    duplicate_content$SegmentTable$NUM_int_sts_dupl_content,
    1
  )
  expect_equal(
    duplicate_content$SegmentTable$PCT_int_sts_dupl_content,
    33.333
  )

  no_label_col_record_set <- with_dataframe_environment(quote({
    study_data <- data.frame(
      id = c("A", "B", "X"),
      value = c(1L, 2L, 3L),
      stringsAsFactors = FALSE
    )
    item_level <- prep_create_meta(
      VAR_NAMES = c("id", "value"),
      LABEL = c("ID", "Value"),
      DATA_TYPE = c(DATA_TYPES$STRING, DATA_TYPES$INTEGER),
      SCALE_LEVEL = c(SCALE_LEVELS$NOMINAL, SCALE_LEVELS$RATIO),
      STUDY_SEGMENT = "LAB",
      MISSING_LIST = SPLIT_CHAR,
      JUMP_LIST = SPLIT_CHAR
    )
    segment_level <- data.frame(
      STUDY_SEGMENT = "LAB",
      SEGMENT_RECORD_CHECK = "subset",
      SEGMENT_ID_VARS = "id",
      SEGMENT_ID_REF_TABLE = "ref_ids",
      stringsAsFactors = FALSE
    )
    names(segment_level) <- c(
      STUDY_SEGMENT, SEGMENT_RECORD_CHECK, SEGMENT_ID_VARS,
      SEGMENT_ID_REF_TABLE
    )
    prep_add_data_frames(
      segment_level = segment_level,
      ref_ids = data.frame(ID = c("A", "B"), stringsAsFactors = FALSE)
    )
    testthat::with_mocked_bindings(
      prep_prepare_dataframes = function(...) {
        assign("ds1", study_data, parent.frame())
        invisible(study_data)
      },
      suppressMessages(util_int_unexp_records_set_segment(
        study_data = study_data,
        meta_data = item_level
      ))
    )
  }))

  expect_equal(no_label_col_record_set$SegmentTable$Segment, "LAB")
  expect_equal(no_label_col_record_set$SegmentTable$NUM_int_sts_setrc, 1)
  expect_equal(no_label_col_record_set$Other$UnexpectedID, "X")

  record_set <- with_dataframe_environment(quote({
    study_data <- data.frame(
      id = c("A", "B", "X"),
      value = c(1L, 2L, 3L),
      stringsAsFactors = FALSE
    )
    item_level <- prep_create_meta(
      VAR_NAMES = c("id", "value"),
      LABEL = c("ID", "Value"),
      DATA_TYPE = c(DATA_TYPES$STRING, DATA_TYPES$INTEGER),
      SCALE_LEVEL = c(SCALE_LEVELS$NOMINAL, SCALE_LEVELS$RATIO),
      STUDY_SEGMENT = "LAB",
      MISSING_LIST = SPLIT_CHAR,
      JUMP_LIST = SPLIT_CHAR
    )
    segment_level <- data.frame(
      STUDY_SEGMENT = "LAB",
      SEGMENT_RECORD_CHECK = "subset",
      SEGMENT_ID_VARS = "id",
      SEGMENT_ID_REF_TABLE = "ref_ids",
      stringsAsFactors = FALSE
    )
    names(segment_level) <- c(
      STUDY_SEGMENT, SEGMENT_RECORD_CHECK, SEGMENT_ID_VARS,
      SEGMENT_ID_REF_TABLE
    )
    prep_add_data_frames(
      segment_level = segment_level,
      ref_ids = data.frame(ID = c("A", "B"), stringsAsFactors = FALSE)
    )
    testthat::with_mocked_bindings(
      prep_prepare_dataframes = function(...) {
        assign("ds1", study_data, parent.frame())
        invisible(study_data)
      },
      suppressMessages(util_int_unexp_records_set_segment(
        study_data = study_data,
        meta_data = item_level,
        label_col = VAR_NAMES
      ))
    )
  }))

  expect_equal(record_set$SegmentTable$Segment, "LAB")
  expect_equal(record_set$SegmentTable$NUM_int_sts_setrc, 1)
  expect_equal(record_set$SegmentTable$PCT_int_sts_setrc, 20)
  expect_equal(names(record_set$Other), c("Segment", "Line", "UnexpectedID"))
  expect_equal(record_set$Other$UnexpectedID, "X")
})

test_that("util_int_datastructure maps parsed ID labels to metadata names", {
  meta_data <- prep_create_meta(
    VAR_NAMES = c("person_id", "visit_id"),
    LABEL = c("Person", "Visit"),
    DATA_TYPE = DATA_TYPES$STRING,
    MISSING_LIST = SPLIT_CHAR,
    JUMP_LIST = SPLIT_CHAR
  )
  df_metadata <- data.frame(
    DF_NAME = "study",
    DF_ID_VARS = "person_id | visit_id",
    stringsAsFactors = FALSE
  )

  id_vars <- util_int_datastructure_id_vars(
    df_metadata,
    name_col = DF_NAME,
    id_col = DF_ID_VARS,
    map_meta_data = meta_data,
    label_col = LABEL
  )

  expect_equal(unname(id_vars$study), c("Person", "Visit"))
})

test_that(
  paste0(
    "util_int_datastructure segment record-set helper ",
    "keeps unexpected-first output"
  ),
  {
    study_data <- data.frame(
      id = c("A", "B", "X"),
      value = c(1L, 2L, 3L),
      stringsAsFactors = FALSE
    )
    meta_data <- prep_create_meta(
      VAR_NAMES = c("id", "value"),
      DATA_TYPE = c(DATA_TYPES$STRING, DATA_TYPES$INTEGER),
      MISSING_LIST = SPLIT_CHAR,
      JUMP_LIST = SPLIT_CHAR,
      STUDY_SEGMENT = "LAB"
    )
    meta_data_segment <- data.frame(
      STUDY_SEGMENT = "LAB",
      SEGMENT_RECORD_CHECK = "subset",
      SEGMENT_ID_VARS = "id",
      stringsAsFactors = FALSE
    )
    meta_data_segment[[SEGMENT_ID_REF_TABLE]] <- I(list(data.frame(
      LAB = c("A", "B"),
      stringsAsFactors = FALSE
    )))

    result <- util_int_datastructure_segment_record_set(
      meta_data_segment,
      study_data,
      meta_data,
      label_col = VAR_NAMES,
      id_vars_list = list(LAB = "id"),
      other_column_order = "unexpected_first"
    )

    expect_equal(result$SegmentTable$Segment, "LAB")
    expect_equal(result$SegmentTable$NUM_int_sts_setrc, 1)
    expect_equal(names(result$Other), c("Segment", "UnexpectedID", "Line"))
    expect_equal(result$Other$UnexpectedID, "X")
  }
)

test_that("util_int_datastructure segment record sets skip ID guardrails", {
  study_data <- data.frame(
    id = c("A", "B"),
    visit = c(1L, 2L),
    value = c(1L, 2L),
    stringsAsFactors = FALSE
  )
  meta_data <- prep_create_meta(
    VAR_NAMES = c("id", "visit", "value"),
    LABEL = c("ID", "Visit", "Value"),
    DATA_TYPE = c(
      DATA_TYPES$STRING,
      DATA_TYPES$INTEGER,
      DATA_TYPES$INTEGER
    ),
    SCALE_LEVEL = c(
      SCALE_LEVELS$NOMINAL,
      SCALE_LEVELS$RATIO,
      SCALE_LEVELS$RATIO
    ),
    STUDY_SEGMENT = "LAB",
    MISSING_LIST = SPLIT_CHAR,
    JUMP_LIST = SPLIT_CHAR
  )

  build_segment_metadata <- function(id_vars, segment = "LAB") {
    meta_data_segment <- data.frame(
      STUDY_SEGMENT = segment,
      SEGMENT_ID_VARS = id_vars,
      SEGMENT_RECORD_CHECK = "exact",
      stringsAsFactors = FALSE
    )
    meta_data_segment[[SEGMENT_ID_REF_TABLE]] <- I(list(data.frame(
      ID = "A",
      stringsAsFactors = FALSE
    )))
    meta_data_segment
  }

  missing_id_warnings <- capture_warnings(
    missing_id_result <- util_int_datastructure_segment_record_set(
      build_segment_metadata(""),
      study_data,
      meta_data,
      label_col = VAR_NAMES,
      id_vars_list = list(LAB = character(0))
    )
  )
  multiple_id_warnings <- capture_warnings(
    multiple_id_result <- util_int_datastructure_segment_record_set(
      build_segment_metadata("id | visit"),
      study_data,
      meta_data,
      label_col = VAR_NAMES,
      id_vars_list = list(LAB = c("id", "visit"))
    )
  )
  mismatch_warnings <- capture_warnings(
    mismatch_result <- util_int_datastructure_segment_record_set(
      build_segment_metadata("id", segment = "OTHER"),
      study_data,
      meta_data,
      label_col = VAR_NAMES,
      id_vars_list = list(OTHER = "id")
    )
  )

  expect_true(any(grepl("No .+SEGMENT_ID_VARS", missing_id_warnings)))
  expect_true(any(grepl("multiple IDs", multiple_id_warnings, fixed = TRUE)))
  expect_true(any(grepl("do not match", mismatch_warnings, fixed = TRUE)))
  expect_equal(nrow(missing_id_result$SegmentData), 0L)
  expect_equal(nrow(multiple_id_result$SegmentData), 0L)
  expect_equal(nrow(mismatch_result$SegmentTable), 0L)
})

test_that(
  "util_int_datastructure segment duplicate IDs handles absent ID variables",
  {
    study_data <- data.frame(value = c(1L, 2L), stringsAsFactors = FALSE)
    meta_data <- prep_create_meta(
      VAR_NAMES = "value",
      DATA_TYPE = DATA_TYPES$INTEGER,
      MISSING_LIST = SPLIT_CHAR,
      JUMP_LIST = SPLIT_CHAR,
      STUDY_SEGMENT = "LAB"
    )
    meta_data_segment <- data.frame(
      STUDY_SEGMENT = "LAB",
      SEGMENT_ID_VARS = "missing_id",
      SEGMENT_UNIQUE_ID = "1",
      stringsAsFactors = FALSE
    )

    result <- suppressWarnings(util_int_datastructure_segment_duplicate_ids(
      meta_data_segment,
      study_data,
      meta_data,
      label_col = VAR_NAMES,
      id_vars_list = list(LAB = "missing_id")
    ))

    expect_equal(result$SegmentTable$Segment, "LAB")
    expect_true(is.na(result$SegmentTable$NUM_int_sts_dupl_ids))
    expect_true(is.na(result$SegmentTable$PCT_int_sts_dupl_ids))
    expect_true(is.na(result$SegmentTable$GRADING))
    expect_null(result$Other$LAB)
  }
)

test_that("util_int_datastructure metadata constructors fill defaults", {
  df_metadata <- util_int_datastructure_df_metadata(
    identifier_name_list = c("df_a", "df_b"),
    id_vars_list = list("id", c("id", "visit")),
    valid_id_table_list = list(
      df_b = data.frame(id = "B"),
      df_a = data.frame(id = "A")
    ),
    meta_data_record_check_list = c(df_b = "subset", df_a = "exact")
  )

  expect_equal(df_metadata[[DF_NAME]], c("df_a", "df_b"))
  expect_equal(df_metadata[[DF_ID_VARS]], c("id", "id | visit"))
  expect_true(all(is.na(df_metadata[[DF_UNIQUE_ID]])))
  expect_equal(df_metadata[[DF_RECORD_CHECK]], c("exact", "subset"))
  expect_equal(df_metadata[[DF_ID_REF_TABLE]][[1]]$id, "A")

  segment_metadata <- util_int_datastructure_segment_metadata(
    identifier_name_list = c("LAB", "INTRO"),
    id_vars_list = list("id", "person | visit"),
    unique_rows = c(INTRO = "no_id", LAB = "true")
  )

  expect_equal(segment_metadata[[STUDY_SEGMENT]], c("LAB", "INTRO"))
  expect_equal(segment_metadata[[SEGMENT_ID_VARS]], c("id", "person | visit"))
  expect_true(all(is.na(segment_metadata[[SEGMENT_RECORD_CHECK]])))
  expect_equal(segment_metadata[[SEGMENT_UNIQUE_ROWS]], c("true", "no_id"))
})
