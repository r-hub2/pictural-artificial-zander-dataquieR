skip_on_cran()

test_that("dataframe-level study data discovery uses input_dir", {
  prep_purge_data_frame_cache()
  withr::defer(prep_purge_data_frame_cache())

  input_dir <- tempfile()
  dir.create(input_dir)
  study_data <- data.frame(id = 1:2, x = c(10, 20))
  utils::write.csv(study_data,
    file.path(input_dir, "study_data.csv"),
    row.names = FALSE
  )

  found <- util_find_study_data_from_dataframe_level(
    data.frame(DF_NAME = "study_data.csv"),
    input_dir = input_dir
  )

  expect_equal(
    found$name_of_study_data,
    file.path(input_dir, "study_data.csv")
  )
  expect_equal(found$study_data, study_data, ignore_attr = TRUE)
})

test_that("dq_report_by study-data references use shared input_dir handling", {
  input_dir <- tempfile()
  dir.create(input_dir)

  refs <- util_report_by_study_data_refs(
    study_data = c(
      "study_data.csv",
      "https://example.org/study_data.csv",
      file.path(input_dir, "absolute.csv")
    ),
    study_data_expr = "study_data",
    input_dir = input_dir
  )

  expect_equal(
    refs,
    c(
      file.path(input_dir, "study_data.csv"),
      "https://example.org/study_data.csv",
      file.path(input_dir, "absolute.csv")
    )
  )
})

test_that("dq_report_by study-data collection keeps lazy header loading", {
  prep_purge_data_frame_cache()
  withr::defer(prep_purge_data_frame_cache())

  input_dir <- tempfile()
  dir.create(input_dir)
  first_data <- data.frame(id = 1:2, x = c(10, 20))
  second_data <- data.frame(id = 1:2, y = c(100, 200))
  utils::write.csv(first_data,
    file.path(input_dir, "first.csv"),
    row.names = FALSE
  )
  utils::write.csv(second_data,
    file.path(input_dir, "second.csv"),
    row.names = FALSE
  )

  collected <- util_report_by_collect_study_data(
    study_data = c("first.csv", "second.csv"),
    study_data_expr = "study_data",
    input_dir = input_dir
  )

  expect_equal(
    collected$study_data,
    file.path(input_dir, c("first.csv", "second.csv"))
  )
  expect_null(collected$name_of_study_data)
  expect_equal(
    collected$list_sd_columns,
    setNames(
      list(c("id", "x"), c("id", "y")),
      file.path(input_dir, c("first.csv", "second.csv"))
    )
  )

  explicit_data <- data.frame(id = 1:2, z = c(3, 4))
  collected <- util_report_by_collect_study_data(
    study_data = explicit_data,
    study_data_expr = "explicit_data"
  )

  expect_equal(collected$name_of_study_data, "explicit_data")
  expect_equal(collected$list_sd_columns, list(explicit_data = c("id", "z")))
  expect_equal(prep_get_data_frame("explicit_data"),
    explicit_data,
    ignore_attr = TRUE
  )

  collect_with_captured_name <- function(study_data) {
    util_report_by_collect_study_data(
      study_data = study_data,
      study_data_expr = util_report_by_study_data_expr(
        substitute(study_data)
      )
    )
  }
  collected_by_do_call <- do.call(
    collect_with_captured_name,
    list(study_data = explicit_data)
  )

  expect_equal(collected_by_do_call$name_of_study_data, "study_data")
  expect_equal(names(collected_by_do_call$list_sd_columns), "study_data")
})

test_that("dq_report_by selects dataframe metadata by segment columns", {
  meta_data <- data.frame(
    VAR_NAMES = c("id", "a", "b", "c"),
    segment = c("shared", "baseline", "follow_up", "follow_up")
  )
  dataframe_names <- c("baseline.csv", "follow_up.csv")
  meta_data_dataframe <- util_dataframe_metadata_for_names(dataframe_names)
  list_sd_columns <- list(
    "baseline.csv" = c("id", "a"),
    "follow_up.csv" = c("id", "b", "c")
  )

  selected <- util_report_by_dataframes_by_segment(
    segment_names = c("baseline", "follow_up"),
    meta_data = meta_data,
    segment_column = "segment",
    list_sd_columns = list_sd_columns,
    dataframe_names = dataframe_names,
    meta_data_dataframe = meta_data_dataframe
  )

  expect_equal(names(selected), c("baseline", "follow_up"))
  expect_equal(selected$baseline[[DF_NAME]], "baseline.csv")
  expect_equal(selected$follow_up[[DF_NAME]], "follow_up.csv")
})

test_that("dq_report_by resolves segment dataframe names", {
  meta_data <- data.frame(
    VAR_NAMES = c("id", "a", "b"),
    DATAFRAMES = c("base", "base", "follow")
  )
  list_sd_columns <- list(
    "baseline.csv" = c("id", "a"),
    "follow_up.csv" = c("id", "b")
  )
  study_data_withcode <- data.frame(
    DF_NAME = c("baseline.csv", "follow_up.csv"),
    DF_CODE = c("base", "follow")
  )

  expect_equal(
    util_report_by_segment_dataframe_names(
      vars_in_segment = c("id", "b"),
      meta_data = meta_data,
      list_sd_columns = list_sd_columns
    ),
    c("baseline.csv", "follow_up.csv")
  )
  expect_equal(
    util_report_by_segment_dataframe_names(
      vars_in_segment = "b",
      meta_data = meta_data,
      list_sd_columns = NULL,
      study_data_withcode = study_data_withcode
    ),
    "follow_up.csv"
  )
})

test_that("dq_report_by filters segment dataframes by item metadata", {
  dataframes <- list(
    baseline = data.frame(
      id = 1:2, shared = 3:4, base_only = 5:6,
      follow_only = 7:8
    ),
    follow = data.frame(
      id = 1:2, shared = 9:10, base_only = 11:12,
      follow_only = 13:14
    )
  )
  meta_data <- data.frame(
    VAR_NAMES = c("shared", "base_only", "follow_only"),
    DATAFRAMES = c("baseline|follow", "baseline", "follow")
  )
  study_data_withcode <- data.frame(
    DF_NAME = c("baseline", "follow"),
    DF_CODE = c("baseline", "follow")
  )
  dfr_in_segment <- data.frame(
    DF_NAME = c("baseline", "follow"),
    DF_CODE = c("baseline", "follow"),
    DF_ID_VARS = c("id", "id")
  )

  filtered <- util_report_by_filter_segment_dataframes(
    dataframes = dataframes,
    dataframe_names = c("baseline", "follow"),
    dfr_in_segment = dfr_in_segment,
    meta_data = meta_data,
    vars_in_segment = c("shared", "base_only", "follow_only"),
    study_data_withcode = study_data_withcode,
    id_vars = character(0)
  )

  expect_equal(names(filtered$baseline), c("id", "shared", "base_only"))
  expect_equal(names(filtered$follow), c("id", "shared", "follow_only"))
})

test_that("dq_report_by subgroup filtering is isolated", {
  study_data <- data.frame(id = 1:3, x = c(1, 2, 3))
  meta_data <- data.frame(
    VAR_NAMES = c("id", "x"),
    LABEL = c("ID", "X"),
    DATA_TYPE = DATA_TYPES$INTEGER,
    SCALE_LEVEL = SCALE_LEVELS$RATIO,
    MISSING_LIST = "|",
    JUMP_LIST = "|"
  )

  expect_equal(
    util_report_by_filter_subgroup(study_data, meta_data, NULL),
    study_data
  )
  expect_equal(
    util_report_by_filter_subgroup(study_data, meta_data, "[x] > 1")$id,
    2:3
  )
  expect_warning(
    util_report_by_filter_subgroup(study_data, meta_data, "[x] > 0"),
    "The number of cases did not change"
  )
  expect_error(
    util_report_by_filter_subgroup(study_data, meta_data, "[unknown] > 0"),
    "The subgroup rule.+not acceptable"
  )
})

test_that("study data merging by ID variables is shared", {
  first_data <- data.frame(id = 1:2, x = c(10, 20))
  second_data <- data.frame(id = 2:3, y = c(200, 300))

  merged <- util_merge_study_data_by_id_vars(
    list(first = first_data, second = second_data),
    "id"
  )

  expect_equal(
    merged[order(merged$id), ],
    data.frame(id = 1:3, x = c(10, 20, NA), y = c(NA, 200, 300))
  )
})

test_that("dataframe-level study data discovery prefers study_data", {
  prep_purge_data_frame_cache()
  withr::defer(prep_purge_data_frame_cache())

  input_dir <- tempfile()
  dir.create(input_dir)
  other_data <- data.frame(id = 1:2, other = c(1, 2))
  study_data <- data.frame(id = 1:2, x = c(10, 20))
  utils::write.csv(other_data,
    file.path(input_dir, "other.csv"),
    row.names = FALSE
  )
  utils::write.csv(study_data,
    file.path(input_dir, "study_data.csv"),
    row.names = FALSE
  )

  found <- util_find_study_data_from_dataframe_level(
    data.frame(
      DF_NAME = c("other.csv", "study_data.csv"),
      DF_CODE = c("other", NA_character_)
    ),
    input_dir = input_dir
  )

  expect_equal(
    found$name_of_study_data,
    file.path(input_dir, "study_data.csv")
  )
  expect_equal(found$study_data, study_data, ignore_attr = TRUE)
})

test_that("dataframe-level study data discovery can merge by DF_ID_VARS", {
  prep_purge_data_frame_cache()
  withr::defer(prep_purge_data_frame_cache())

  input_dir <- tempfile()
  dir.create(input_dir)
  first_data <- data.frame(id = 1:2, x = c(10, 20))
  second_data <- data.frame(id = 2:3, y = c(200, 300))
  utils::write.csv(first_data,
    file.path(input_dir, "first.csv"),
    row.names = FALSE
  )
  utils::write.csv(second_data,
    file.path(input_dir, "second.csv"),
    row.names = FALSE
  )

  found <- util_find_study_data_from_dataframe_level(
    data.frame(
      DF_NAME = c("first.csv", "second.csv"),
      DF_ID_VARS = c("id", "id")
    ),
    input_dir = input_dir
  )

  expect_equal(
    found$name_of_study_data,
    paste(file.path(input_dir, c("first.csv", "second.csv")),
      collapse = ", "
    )
  )
  expect_equal(
    found$study_data[order(found$study_data$id), ],
    data.frame(id = 1:3, x = c(10, 20, NA), y = c(NA, 200, 300))
  )
})

test_that("dataframe-level study data discovery helper keeps remote refs", {
  refs <- c("study_data.csv", "https://example.test/study_data.csv", "")

  expect_identical(
    util_add_input_dir_to_data_frame_refs(refs),
    refs
  )
  expect_identical(
    util_add_input_dir_to_data_frame_refs(refs, input_dir = "input"),
    c(file.path("input", "study_data.csv"), refs[2:3])
  )
  expect_identical(
    util_parse_dataframe_level_id_vars(c(NA_character_, "")),
    character(0)
  )
})

test_that("dataframe-level study data discovery reports unmergeable metadata", {
  prep_purge_data_frame_cache()
  on.exit(prep_purge_data_frame_cache(), add = TRUE)

  expect_message(
    found <- util_find_study_data_from_dataframe_level(data.frame(
      DF_NAME = c("first", "second"),
      DF_ID_VARS = c(NA_character_, NA_character_)
    )),
    "could not merge them automatically"
  )

  expect_null(found)
})

test_that("dataframe-level study data discovery reports missing cache frames", {
  prep_purge_data_frame_cache()
  on.exit(prep_purge_data_frame_cache(), add = TRUE)

  expect_message(
    found <- util_find_study_data_from_dataframe_level(data.frame(
      DF_NAME = c("missing_first", "missing_second"),
      DF_ID_VARS = c("id", "id")
    )),
    "Could not load all study data frames"
  )

  expect_null(found)
})

test_that("dataframe-level study data discovery reports missing merge IDs", {
  prep_purge_data_frame_cache()
  on.exit(prep_purge_data_frame_cache(), add = TRUE)

  prep_add_data_frames(
    first = data.frame(id = 1:2, x = c(10, 20)),
    second = data.frame(y = c(200, 300))
  )

  expect_message(
    found <- util_find_study_data_from_dataframe_level(data.frame(
      DF_NAME = c("first", "second"),
      DF_ID_VARS = c("id", "id")
    )),
    "ID variable"
  )

  expect_null(found)
})

test_that("zzz preps can use study data from dataframe-level metadata", {
  prep_purge_data_frame_cache()
  withr::defer(prep_purge_data_frame_cache())

  input_dir <- tempfile()
  dir.create(input_dir)
  study_data <- data.frame(id = 1:3, x = c(1, NA, 3))
  study_data_file <- file.path(input_dir, "study_data.csv")
  utils::write.csv(study_data, study_data_file, row.names = FALSE)
  item_level <- data.frame(
    VAR_NAMES = c("id", "x"),
    DATA_TYPE = c("integer", "integer"),
    LABEL = c("ID", "X"),
    MISSING_LIST = c("|", "|"),
    JUMP_LIST = c("|", "|"),
    stringsAsFactors = FALSE
  )
  dataframe_level <- data.frame(
    DF_NAME = study_data_file,
    DF_ID_VARS = "id",
    stringsAsFactors = FALSE
  )
  prep_add_data_frames(
    item_level = item_level,
    dataframe_level = dataframe_level
  )

  result_env <- new.env(parent = emptyenv())
  withr::local_options(list(dataquieR.test_decorator = TRUE))
  expect_message(
    result_env$res <- com_item_missingness(
      resp_vars = "x",
      label_col = "LABEL",
      threshold_value = 0,
      include_sysmiss = TRUE
    ),
    "dataframe level metadata"
  )
  res <- result_env$res
  expect_type(res, "list")
  expect_setequal(names(res), c(
    "SummaryData", "SummaryTable", "SummaryPlot",
    "ReportSummaryTable"
  ))
})

test_that("report-by strata rows retain regular and missing groups", {
  study_data <- data.frame(
    stratum = c("north", NA_character_, "south", "north"),
    stringsAsFactors = FALSE
  )

  expect_identical(
    util_report_by_strata_rows(study_data, NULL),
    list(all_observations = 1:4)
  )
  expect_identical(
    util_report_by_strata_rows(study_data, "absent"),
    list(all_observations = 1:4)
  )
  expect_identical(
    util_report_by_strata_rows(study_data, "stratum"),
    list(north = c(1L, 4L), south = 3L, NAs_group = 2L)
  )
})

test_that("report-by study-data refs reject unsupported inputs", {
  expect_error(
    util_report_by_study_data_refs(
      study_data = 42,
      study_data_expr = "study_data"
    ),
    "study_data argument"
  )
})

test_that("report-by study-data collection tolerates unreadable refs", {
  refs <- c("missing.csv", "loaded.csv")

  testthat::local_mocked_bindings(
    prep_get_data_frame = function(nm, column_names_only = FALSE,
      keep_types = FALSE) {
      expect_true(column_names_only)
      expect_true(keep_types)
      if (identical(nm, "missing.csv")) {
        stop("not available")
      }
      data.frame(id = integer(), value = numeric())
    }
  )

  collected <- util_report_by_collect_study_data(
    study_data = refs,
    study_data_expr = "study_data"
  )

  expect_null(collected$name_of_study_data)
  expect_named(collected$list_sd_columns, refs)
  expect_null(collected$list_sd_columns[["missing.csv"]])
  expect_equal(collected$list_sd_columns[["loaded.csv"]], c("id", "value"))
})

test_that("report-by segment ID variables warn when metadata is empty", {
  dfr_in_segment <- data.frame(
    DF_NAME = "study_data.csv",
    DF_ID_VARS = NA_character_,
    stringsAsFactors = FALSE
  )

  expect_warning(
    id_vars <- util_report_by_segment_id_vars(
      dfr_in_segment,
      id_vars = "fallback_id"
    ),
    "DF_ID_VARS"
  )

  expect_equal(id_vars, "fallback_id")
})

test_that("report-by dataframe merges handle empty and unkeyed inputs", {
  expect_null(util_merge_study_data_by_id_vars(list()))

  dataframes <- list(
    first = data.frame(id = 1:2, a = c("a", "b")),
    second = data.frame(id = 2:3, b = c("c", "d"))
  )

  expect_warning(
    merged <- util_report_by_merge_segment_dataframes(
      dataframes,
      id_vars = character(0)
    ),
    "no id variable"
  )

  expect_s3_class(merged, "data.frame")
  expect_true(all(c("id", "a", "b") %in% colnames(merged)))
})

test_that("dataframe-level study-data merge requires ID metadata", {
  meta_data_dataframe <- data.frame(
    DF_NAME = c("first.csv", "second.csv"),
    DF_ID_VARS = NA_character_,
    stringsAsFactors = FALSE
  )

  expect_message(
    merged <- util_merge_dataframe_level_study_data(
      meta_data_dataframe,
      dataframe_names = meta_data_dataframe[[DF_NAME]]
    ),
    "DF_ID_VARS"
  )

  expect_null(merged)
})
