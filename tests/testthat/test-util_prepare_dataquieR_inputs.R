skip_on_cran()

test_that("util_prepare_dataquieR_inputs applies cached missing-code rules once", { # nolint: line_length_linter.
  prep_purge_data_frame_cache()
  withr::defer(prep_purge_data_frame_cache())

  study_data <- data.frame(
    sex = c(1L, 2L, 1L),
    pregnant = c(NA_integer_, NA_integer_, 5L)
  )
  meta_data <- data.frame(
    VAR_NAMES = c("sex", "pregnant"),
    LABEL = c("sex", "pregnant"),
    DATA_TYPE = c(DATA_TYPES$INTEGER, DATA_TYPES$INTEGER),
    SCALE_LEVEL = c(SCALE_LEVELS$NOMINAL, SCALE_LEVELS$METRIC),
    MISSING_LIST = c("", ""),
    JUMP_LIST = c("", "")
  )
  rules <- data.frame(
    resp_vars = "pregnant",
    CODE_CLASS = "JUMP",
    CODE_LABEL = "not applicable in males",
    CODE_VALUE = "9999",
    RULE = "[sex]=1"
  )

  prep_add_data_frames(
    data_frame_list = setNames(list(rules), MISSING_CODE_RULES)
  )

  prepared <- util_prepare_dataquieR_inputs(
    study_data = study_data,
    meta_data = meta_data,
    label_col = LABEL
  )

  expect_equal(prepared$study_data$pregnant, c(9999L, NA_integer_, 5L))
  expect_true(JUMP_LIST %in% names(prepared$meta_data))
  expect_match(
    prepared$meta_data[
      prepared$meta_data[[VAR_NAMES]] == "pregnant",
      JUMP_LIST
    ],
    "9999"
  )
  expect_equal(
    prep_get_data_frame("study_data")$pregnant,
    c(9999L, NA_integer_, 5L)
  )
  expect_true(util_dataquieR_inputs_prepared(
    prep_get_data_frame("study_data"),
    prepared$meta_data
  ))
  expect_false(any(startsWith(
    prep_list_dataframes(),
    "..dataquieR_study_data_"
  )))

  prepared_again <- util_prepare_dataquieR_inputs(
    study_data = prepared$study_data,
    meta_data = prepared$meta_data,
    label_col = LABEL
  )

  expect_equal(prepared_again$study_data, prepared$study_data,
    ignore_attr = TRUE
  )
  expect_equal(prepared_again$meta_data, prepared$meta_data,
    ignore_attr = TRUE
  )

  rebuilt <- prep_prepare_dataframes(
    .study_data = prepared$study_data,
    .meta_data = prepared$meta_data,
    .label_col = LABEL,
    .replace_missings = FALSE,
    .replace_hard_limits = FALSE,
    .adjust_data_type = TRUE,
    .amend_scale_level = FALSE
  )

  expect_equal(rebuilt$pregnant, c(9999L, NA_integer_, 5L))
  expect_equal(
    util_attr(rebuilt, "study_data", exact = TRUE)$pregnant,
    c(9999L, NA_integer_, 5L)
  )
})

test_that("util_clean_relevant_var_names keeps only usable names", {
  expect_equal(
    util_clean_relevant_var_names(list("a", c("b", ""), 1L, NA_character_)),
    c("a", "b")
  )
})

test_that("util_filter_cross_item_metadata keeps selected variable rules", {
  skip_on_cran()

  meta_data <- data.frame(
    VAR_NAMES = c("a", "b", "c"),
    LABEL = c("Alpha", "Beta", "Gamma")
  )
  meta_data_cross_item <- data.frame(
    VARIABLE_LIST = c("Alpha | Beta", "Gamma"),
    CHECK_LABEL = c("a and b", "c")
  )

  filtered <- util_filter_cross_item_metadata(
    meta_data_cross_item = meta_data_cross_item,
    resp_vars = "b",
    meta_data = meta_data,
    label_col = LABEL
  )

  expect_equal(filtered$CHECK_LABEL, "a and b")
  expect_equal(
    util_filter_cross_item_metadata(
      meta_data_cross_item = meta_data_cross_item,
      resp_vars = character(0),
      meta_data = meta_data,
      label_col = LABEL
    ),
    meta_data_cross_item
  )
})

test_that("util_ensure_variable_roles defaults missing and invalid roles", {
  skip_on_cran()

  meta_data <- data.frame(
    VAR_NAMES = c("a", "b", "c"),
    LABEL = c("Alpha", "Beta", "Gamma"),
    VARIABLE_ROLE = c(VARIABLE_ROLES$PRIMARY, NA_character_, "unknown")
  )

  expect_message(
    {
      prepared <- util_ensure_variable_roles(meta_data, label_col = LABEL)
    },
    "invalid"
  )
  expect_equal(prepared[[VARIABLE_ROLE]], rep(VARIABLE_ROLES$PRIMARY, 3))
  expect_equal(
    util_ensure_variable_roles(meta_data[c(VAR_NAMES, LABEL)], label_col = LABEL)[[VARIABLE_ROLE]], # nolint: line_length_linter.
    rep(VARIABLE_ROLES$PRIMARY, 3)
  )

  input_meta_data <- data.frame(
    VAR_NAMES = "a",
    LABEL = "Alpha",
    DATA_TYPE = DATA_TYPES$INTEGER,
    SCALE_LEVEL = SCALE_LEVELS$RATIO,
    MISSING_LIST = "|",
    JUMP_LIST = "|",
    VARIABLE_ROLE = "unknown"
  )
  prepared_inputs <- util_prepare_dataquieR_inputs(
    study_data = data.frame(a = 1L),
    meta_data = input_meta_data,
    label_col = LABEL,
    update_registry = FALSE
  )
  expect_equal(
    as.character(prepared_inputs$meta_data[[VARIABLE_ROLE]]),
    VARIABLE_ROLES$PRIMARY
  )
})

test_that("util_ensure_cross_item_metadata supplies stable defaults", {
  skip_on_cran()

  defaulted <- util_ensure_cross_item_metadata(NULL)
  expect_equal(names(defaulted), c(VARIABLE_LIST, CHECK_LABEL))
  expect_equal(nrow(defaulted), 0)

  cross_item <- data.frame(
    VARIABLE_LIST = "Alpha | Beta",
    CHECK_LABEL = "comparison",
    row.names = "rule_1"
  )
  expect_equal(rownames(util_ensure_cross_item_metadata(cross_item)), "1")
})

test_that("util_ensure_segment_metadata preserves report-specific options", {
  skip_on_cran()

  meta_data <- data.frame(
    VAR_NAMES = c("a", "b"),
    STUDY_SEGMENT = c("baseline", "follow_up")
  )
  defaulted <- util_ensure_segment_metadata(NULL, meta_data)
  expect_equal(defaulted[[STUDY_SEGMENT]], c("baseline", "follow_up"))
  expect_false(SEGMENT_ID_VARS %in% names(defaulted))

  with_id_vars <- util_ensure_segment_metadata(
    meta_data_segment = data.frame(STUDY_SEGMENT = "baseline"),
    meta_data = meta_data,
    validate_study_segment = TRUE,
    add_segment_id_vars = TRUE
  )
  expect_equal(with_id_vars[[SEGMENT_ID_VARS]], NA_character_)

  no_segment_column <- util_ensure_segment_metadata(
    meta_data_segment = NULL,
    meta_data = meta_data[VAR_NAMES],
    validate_study_segment = TRUE,
    add_segment_id_vars = TRUE
  )
  expect_equal(nrow(no_segment_column), 0)
  expect_equal(names(no_segment_column), c(STUDY_SEGMENT, SEGMENT_ID_VARS))
})

test_that("dataframe metadata defaults keep optional columns explicit", {
  skip_on_cran()

  defaulted <- util_dataframe_metadata_for_names(c("study_data", "lookup"))
  expect_equal(defaulted[[DF_NAME]], c("study_data", "lookup"))
  expect_equal(defaulted[[DF_CODE]], rep(NA_character_, 2))
  expect_equal(defaulted[[DF_ID_VARS]], rep(NA_character_, 2))

  name_only <- util_dataframe_metadata_for_names(
    "study_data",
    include_df_code = FALSE,
    include_df_id_vars = FALSE
  )
  expect_equal(names(name_only), DF_NAME)

  empty <- util_empty_dataframe_metadata()
  expect_equal(nrow(empty), 0)
  expect_equal(names(empty), c(DF_NAME, DF_CODE, DF_ID_VARS))

  null_names <- util_dataframe_metadata_for_names(NULL)
  expect_equal(nrow(null_names), 0)
  expect_equal(names(null_names), c(DF_NAME, DF_CODE, DF_ID_VARS))
})

test_that("util_map_relevant_var_names maps labels to variable names", {
  meta_data <- data.frame(
    VAR_NAMES = c("a", "b"),
    LABEL = c("Alpha", "Beta"),
    LONG_LABEL = c("Long alpha", "Long beta")
  )

  expect_equal(
    util_map_relevant_var_names(
      relevant_var_names = list("Alpha", "b", 42L),
      meta_data = meta_data,
      label_col = LABEL
    ),
    c("a", "b")
  )
})

test_that("util_prepare_relevant_meta_data validates only relevant metadata", {
  meta_data <- data.frame(
    VAR_NAMES = c("a", "b", "a"),
    LABEL = c("Alpha 1", "Beta", "Alpha 2"),
    LONG_LABEL = c("Long alpha 1", "Long beta", "Long alpha 2")
  )

  prepared <- util_prepare_relevant_meta_data(
    meta_data = meta_data,
    relevant_var_names = "Beta",
    label_col = LABEL
  )
  expect_equal(prepared$relevant_var_names, "b")
  expect_equal(prepared$meta_data, meta_data)

  expect_error(
    util_prepare_relevant_meta_data(
      meta_data = meta_data,
      relevant_var_names = "a",
      label_col = LABEL
    ),
    "Found duplicated"
  )
})

test_that("util_fill_empty_label_columns fills mapped label columns", {
  meta_data <- data.frame(
    VAR_NAMES = c("a", "b"),
    LABEL = c("", "Beta"),
    LONG_LABEL = c("Long alpha", NA_character_),
    OTHER = c("", "")
  )

  expect_equal(
    util_fill_empty_label_columns(meta_data, label_col = LABEL)[[LABEL]],
    c("a", "Beta")
  )
  expect_equal(
    util_fill_empty_label_columns(meta_data, label_col = LABEL)[[LONG_LABEL]],
    c("Long alpha", "b")
  )
  expect_equal(
    util_fill_empty_label_columns(meta_data, label_col = LABEL)[["OTHER"]],
    c("", "")
  )
})

test_that("decorated standalone calls see cached missing-code rules", {
  prep_purge_data_frame_cache()
  withr::defer(prep_purge_data_frame_cache())
  withr::local_options(list(dataquieR.test_decorator = TRUE))

  study_data <- data.frame(
    sex = c(1L, 2L, 1L),
    pregnant = c(NA_integer_, NA_integer_, 5L)
  )
  meta_data <- data.frame(
    VAR_NAMES = c("sex", "pregnant"),
    LABEL = c("sex", "pregnant"),
    DATA_TYPE = c(DATA_TYPES$INTEGER, DATA_TYPES$INTEGER),
    SCALE_LEVEL = c(SCALE_LEVELS$NOMINAL, SCALE_LEVELS$METRIC),
    MISSING_LIST = c("|", "|"),
    JUMP_LIST = c("|", "|")
  )
  rules <- data.frame(
    resp_vars = "pregnant",
    CODE_CLASS = "JUMP",
    CODE_LABEL = "not applicable in males",
    CODE_VALUE = "9999",
    RULE = "[sex]=1"
  )

  prep_add_data_frames(
    data_frame_list = setNames(list(rules), MISSING_CODE_RULES)
  )

  report_prepared <- util_prepare_dataquieR_inputs(
    study_data = study_data,
    meta_data = meta_data,
    label_col = LABEL,
    update_registry = FALSE
  )

  expect_equal(
    report_prepared$study_data$pregnant,
    c(9999L, NA_integer_, 5L)
  )
  expect_match(
    report_prepared$meta_data[
      report_prepared$meta_data[[VAR_NAMES]] == "pregnant",
      JUMP_LIST
    ],
    "9999"
  )

  result <- com_item_missingness(
    resp_vars = "pregnant",
    study_data = study_data,
    meta_data = meta_data,
    label_col = LABEL,
    threshold_value = 100,
    include_sysmiss = TRUE
  )

  expect_type(result, "list")
  expect_equal(
    prep_get_data_frame("study_data")$pregnant,
    report_prepared$study_data$pregnant
  )
})

test_that("decorated standalone calls see item-computation metadata", {
  prep_purge_data_frame_cache()
  withr::defer(prep_purge_data_frame_cache())
  withr::local_options(list(dataquieR.test_decorator = TRUE))

  study_data <- data.frame(
    a = c(1L, 2L, 3L),
    b = c(3L, 4L, 5L)
  )
  meta_data <- data.frame(
    VAR_NAMES = c("a", "b", "sum_ab"),
    LABEL = c("a", "b", "sum_ab"),
    DATA_TYPE = c(
      DATA_TYPES$INTEGER, DATA_TYPES$INTEGER,
      DATA_TYPES$INTEGER
    ),
    SCALE_LEVEL = c(
      SCALE_LEVELS$RATIO, SCALE_LEVELS$RATIO,
      SCALE_LEVELS$RATIO
    ),
    MISSING_LIST = c("|", "|", "|"),
    JUMP_LIST = c("|", "|", "|")
  )
  item_computation_level <- data.frame(
    VAR_NAMES = "sum_ab",
    COMPUTATION_RULE = "[a] + [b]"
  )
  prep_add_data_frames(item_computation_level = item_computation_level)

  result <- com_item_missingness(
    resp_vars = "sum_ab",
    study_data = study_data,
    meta_data = meta_data,
    label_col = LABEL,
    threshold_value = 100,
    include_sysmiss = TRUE
  )

  expect_type(result, "list")
  expect_equal(prep_get_data_frame("study_data")$sum_ab, c(4L, 6L, 8L))
})

test_that("util_prepare_dataquieR_inputs skips already applied computed variables", { # nolint: line_length_linter.
  prep_purge_data_frame_cache()
  withr::defer(prep_purge_data_frame_cache())

  study_data <- data.frame(
    a = c(1L, 2L),
    b = c(3L, 4L)
  )
  meta_data <- data.frame(
    VAR_NAMES = c("a", "b", "sum_ab"),
    LABEL = c("a", "b", "sum_ab"),
    DATA_TYPE = c(
      DATA_TYPES$INTEGER, DATA_TYPES$INTEGER,
      DATA_TYPES$INTEGER
    ),
    SCALE_LEVEL = c(
      SCALE_LEVELS$RATIO, SCALE_LEVELS$RATIO,
      SCALE_LEVELS$RATIO
    ),
    MISSING_LIST = c("", "", ""),
    JUMP_LIST = c("", "", "")
  )
  computation <- data.frame(
    VAR_NAMES = "sum_ab",
    COMPUTATION_RULE = "[a] + [b]"
  )

  prepared <- util_prepare_dataquieR_inputs(
    study_data = study_data,
    meta_data = meta_data,
    label_col = LABEL,
    meta_data_item_computation = computation
  )

  expect_equal(prepared$study_data$sum_ab, c(4L, 6L))
  expect_false(is.null(util_attr(
    prepared$study_data,
    "dataquieR_computed_variables_applied",
    exact = TRUE
  )))

  only_computed <- prepared$study_data["sum_ab"]
  attr(only_computed, "dataquieR_computed_variables_applied") <-
    util_attr(prepared$study_data,
      "dataquieR_computed_variables_applied",
      exact = TRUE
    )
  expect_warning(
    prepared_again <- util_prepare_dataquieR_inputs(
      study_data = only_computed,
      meta_data = prepared$meta_data,
      label_col = LABEL,
      meta_data_item_computation = computation
    ),
    "Column 'VAR_NAMES' in 'rules' must match the predicate"
  )

  expect_equal(prepared_again$study_data$sum_ab, c(4L, 6L))

  uaci <- util_add_computed_internals(
    prepared$meta_data_item_computation,
    data.frame(),
    prepared$meta_data,
    LABEL
  )
  computed_signature <- rlang::hash(list(
    rules = uaci$meta_data_item_computation,
    meta_data = uaci$meta_data
  ))
  attr(prepared$study_data, "dataquieR_computed_variables_applied") <-
    computed_signature

  expect_warning(
    prepared_again_full <- suppressMessages(
      util_apply_computed_variables_once(
        study_data = prepared$study_data,
        meta_data = prepared$meta_data,
        label_col = LABEL,
        meta_data_cross_item = data.frame(),
        meta_data_item_computation = prepared$meta_data_item_computation
      )
    ),
    NA
  )

  expect_equal(prepared_again_full$study_data$sum_ab, c(4L, 6L))
  expect_identical(
    util_attr(
      prepared_again_full$study_data,
      "dataquieR_computed_variables_applied",
      exact = TRUE
    ),
    computed_signature
  )
})

test_that("computed variables tolerate duplicate display labels", {
  skip_on_cran()
  prep_purge_data_frame_cache()
  withr::defer(prep_purge_data_frame_cache())

  study_data <- data.frame(
    a = c(1L, 2L),
    b = c(3L, 4L)
  )
  meta_data <- data.frame(
    VAR_NAMES = c("a", "b", "sum_ab"),
    LABEL = c("Repeated label", "Repeated label", "Computed total"),
    DATA_TYPE = rep(DATA_TYPES$INTEGER, 3),
    SCALE_LEVEL = rep(SCALE_LEVELS$RATIO, 3),
    MISSING_LIST = rep("", 3),
    JUMP_LIST = rep("", 3)
  )
  computation <- data.frame(
    VAR_NAMES = "sum_ab",
    COMPUTATION_RULE = "[a] + [b]"
  )

  prepared <- suppressWarnings(util_prepare_dataquieR_inputs(
    study_data = study_data,
    meta_data = meta_data,
    label_col = LABEL,
    meta_data_item_computation = computation,
    update_registry = FALSE
  ))

  expect_equal(prepared$study_data$sum_ab, c(4L, 6L))
  expect_length(unique(prepared$meta_data[[LABEL]]), 3)
  expect_match(prepared$label_modification_text, "duplicated")
  expect_gt(nrow(prepared$label_modification_table), 0)
})

test_that("util_apply_missing_code_rules_once skips already applied rules", {
  prep_purge_data_frame_cache()
  withr::defer(prep_purge_data_frame_cache())

  study_data <- data.frame(x = c(1L, 999L))
  meta_data <- data.frame(
    VAR_NAMES = "x",
    LABEL = "x",
    DATA_TYPE = DATA_TYPES$INTEGER,
    SCALE_LEVEL = SCALE_LEVELS$RATIO,
    MISSING_LIST = "",
    JUMP_LIST = "",
    stringsAsFactors = FALSE
  )
  rules <- data.frame(
    resp_vars = "x",
    CODE_CLASS = "MISSING",
    CODE_LABEL = "missing",
    CODE_VALUE = "999",
    RULE = "TRUE"
  )
  prep_add_data_frames(data_frame_list = setNames(
    list(rules),
    MISSING_CODE_RULES
  ))
  missing_code_rules <- MISSING_CODE_RULES
  util_expect_data_frame(missing_code_rules)

  attr(study_data, "dataquieR_missing_code_rules_applied") <-
    rlang::hash(missing_code_rules)

  result <- util_apply_missing_code_rules_once(
    study_data = study_data,
    meta_data = meta_data,
    label_col = LABEL
  )

  expect_identical(result$study_data, study_data)
  expect_identical(result$meta_data, meta_data)
})

test_that("missing-code rule errors preserve report inputs", {
  prep_purge_data_frame_cache()
  withr::defer(prep_purge_data_frame_cache())

  study_data <- data.frame(x = c(1L, 999L))
  meta_data <- data.frame(
    VAR_NAMES = "x",
    LABEL = "x",
    DATA_TYPE = DATA_TYPES$INTEGER,
    SCALE_LEVEL = SCALE_LEVELS$RATIO,
    MISSING_LIST = "",
    JUMP_LIST = "",
    stringsAsFactors = FALSE
  )
  rules <- data.frame(
    resp_vars = "x",
    CODE_CLASS = "MISSING",
    CODE_LABEL = "missing",
    CODE_VALUE = "999",
    RULE = "invalid rule"
  )
  prep_add_data_frames(data_frame_list = setNames(
    list(rules),
    MISSING_CODE_RULES
  ))
  testthat::local_mocked_bindings(
    prep_add_missing_codes = function(...) stop("synthetic rule failure")
  )

  result <- NULL
  expect_warning(
    result <- util_apply_missing_code_rules_once(
      study_data = study_data,
      meta_data = meta_data,
      label_col = LABEL
    ),
    "Could not amend missing codes.*synthetic rule failure"
  )

  expect_identical(result$study_data, study_data)
  expect_identical(result$meta_data, meta_data)
})

test_that("prepared report inputs are marked for dq_report2", {
  study_data <- data.frame(a = c(1L, 2L))
  meta_data <- data.frame(
    VAR_NAMES = "a",
    LABEL = "a",
    DATA_TYPE = DATA_TYPES$INTEGER,
    SCALE_LEVEL = SCALE_LEVELS$RATIO
  )

  prepared <- util_prepare_dataquieR_inputs(
    study_data = study_data,
    meta_data = meta_data,
    label_col = LABEL,
    update_registry = FALSE
  )

  expect_true(util_dataquieR_inputs_prepared(
    prepared$study_data,
    prepared$meta_data
  ))
  expect_false(util_dataquieR_inputs_prepared(
    prepared$study_data,
    meta_data
  ))
})

test_that("util_prepare_dataquieR_inputs reuses normalized metadata", {
  study_data <- data.frame(a = c(1L, 2L))
  meta_data <- data.frame(
    VAR_NAMES = "a",
    LABEL = "a",
    DATA_TYPE = DATA_TYPES$INTEGER,
    SCALE_LEVEL = SCALE_LEVELS$RATIO
  )
  attr(meta_data, "normalized") <- TRUE
  attr(meta_data, "version") <- 2

  testthat::local_mocked_bindings(
    prep_meta_data_v1_to_item_level_meta_data = function(...) {
      stop("normalized metadata should not be converted again")
    }
  )

  prepared <- util_prepare_dataquieR_inputs(
    study_data = study_data,
    meta_data = meta_data,
    label_col = LABEL,
    update_registry = FALSE
  )

  expect_true(util_dataquieR_inputs_prepared(
    prepared$study_data,
    prepared$meta_data
  ))
  expect_equal(unname(as.character(prepared$meta_data[[VAR_NAMES]])), "a")
})

test_that("util_prepare_item_level_metadata owns item-level normalization", {
  meta_data <- data.frame(
    VAR_NAMES = c("a", "a"),
    LABEL = c("Alpha", "Alpha"),
    DATA_TYPE = c(DATA_TYPES$INTEGER, DATA_TYPES$INTEGER),
    SCALE_LEVEL = c(SCALE_LEVELS$RATIO, SCALE_LEVELS$RATIO),
    MISSING_LIST = c(
      "99980 = Missing - other reason",
      "99980 = Missing - other reason"
    ),
    JUMP_LIST = c("88880 = JUMP 88880", "88880 = JUMP 88880")
  )
  rownames(meta_data) <- c("custom_row", "duplicated_custom_row")

  prepared <- util_prepare_item_level_metadata(
    meta_data = meta_data,
    label_col = LABEL
  )

  expect_equal(nrow(prepared), 1L)
  expect_identical(
    .row_names_info(prepared, type = 0L),
    c(NA_integer_, -1L)
  )
  expect_true(identical(util_attr(prepared, "normalized", exact = TRUE), TRUE))
  expect_equal(util_attr(prepared, "version", exact = TRUE), 2)
  expect_true(all(c(MISSING_LIST, JUMP_LIST) %in% names(prepared)))
})

test_that("util_prepare_item_level_metadata keeps code-list table normalization", { # nolint: line_length_linter.
  prep_purge_data_frame_cache()
  withr::defer(prep_purge_data_frame_cache())

  prep_add_data_frames(code_table = data.frame(
    CODE_VALUE = "99980",
    CODE_LABEL = "Missing - other reason",
    CODE_CLASS = "MISSING",
    stringsAsFactors = FALSE
  ))
  meta_data <- data.frame(
    VAR_NAMES = "a",
    LABEL = "Alpha",
    DATA_TYPE = DATA_TYPES$INTEGER,
    SCALE_LEVEL = SCALE_LEVELS$RATIO,
    MISSING_LIST_TABLE = "",
    VALUE_LABEL_TABLE = "",
    CODE_LIST_TABLE = "code_table",
    stringsAsFactors = FALSE
  )
  attr(meta_data, "normalized") <- TRUE
  attr(meta_data, "version") <- 2

  testthat::local_mocked_bindings(
    prep_meta_data_v1_to_item_level_meta_data = function(...) {
      stop("normalized metadata should not be converted again")
    }
  )

  prepared <- util_prepare_item_level_metadata(
    meta_data = meta_data,
    label_col = LABEL
  )

  expect_false(CODE_LIST_TABLE %in% names(prepared))
  expect_equal(prepared[[MISSING_LIST_TABLE]], "code_table")
  expect_equal(prepared[[VALUE_LABEL_TABLE]], "")
})

test_that("util_prepare_item_level_metadata passes optional cause labels", {
  meta_data <- data.frame(VAR_NAMES = "a", LABEL = "Alpha")
  cause_label_df <- data.frame(CODE_VALUE = "99980")
  observed <- new.env(parent = emptyenv())
  observed$value <- NULL

  testthat::local_mocked_bindings(
    prep_meta_data_v1_to_item_level_meta_data = function(...,
      cause_label_df) {
      observed$value <- cause_label_df
      meta_data
    },
    util_normalize_clt = function(meta_data) meta_data
  )

  prepared <- util_prepare_item_level_metadata(
    meta_data = meta_data,
    label_col = LABEL,
    cause_label_df = cause_label_df
  )

  expect_equal(prepared, meta_data)
  expect_equal(observed$value, cause_label_df)
})

test_that("study-data cache distinguishes preparation signatures", {
  util_purge_study_data_cache()
  withr::defer(util_purge_study_data_cache())

  study_data <- data.frame(a = c(1L, 2L))
  meta_data <- data.frame(
    VAR_NAMES = "a",
    LABEL = "Alpha",
    DATA_TYPE = DATA_TYPES$INTEGER,
    SCALE_LEVEL = SCALE_LEVELS$RATIO,
    MISSING_LIST = "",
    JUMP_LIST = ""
  )

  suppressWarnings(suppressMessages(util_populate_study_data_cache(
    study_data = study_data,
    meta_data = meta_data,
    label_col = LABEL,
    quick = FALSE
  )))
  keys_by_label <- ls(.study_data_cache)

  expect_gt(length(keys_by_label), 1)
  expect_length(unique(keys_by_label), length(keys_by_label))

  cached_calls <- lapply(keys_by_label, function(key) {
    util_attr(.study_data_cache[[key]], "call", exact = TRUE)
  })
  call_args <- lapply(cached_calls, rlang::call_args)
  # The exhaustive cache fill stores variants for different
  # prep_prepare_dataframes() switches. The key must keep those variants
  # separate so later direct and report calls can retrieve the right one.
  prep_signatures <- vapply(call_args, function(args) {
    paste(
      vapply(.to_combine, function(arg) isTRUE(args[[arg]]), logical(1)),
      collapse = ""
    )
  }, character(1))

  expect_gt(length(unique(prep_signatures)), 1)

  util_purge_study_data_cache()
  suppressWarnings(suppressMessages(util_populate_study_data_cache(
    study_data = study_data,
    meta_data = meta_data,
    label_col = VAR_NAMES,
    quick = FALSE
  )))
  keys_by_var_names <- ls(.study_data_cache)

  expect_false(identical(sort(keys_by_label), sort(keys_by_var_names)))
})

test_that("preparation signatures include study data and metadata", {
  util_purge_study_data_cache()
  withr::defer(util_purge_study_data_cache())

  study_data <- data.frame(a = c(1L, 2L))
  meta_data <- data.frame(
    VAR_NAMES = "a",
    LABEL = "Alpha",
    DATA_TYPE = DATA_TYPES$INTEGER,
    SCALE_LEVEL = SCALE_LEVELS$RATIO,
    MISSING_LIST = "",
    JUMP_LIST = ""
  )

  prepare_minimal <- function(.study_data = study_data,
    .meta_data = meta_data) {
    suppressWarnings(suppressMessages(prep_prepare_dataframes(
      .study_data = .study_data,
      .meta_data = .meta_data,
      .label_col = LABEL,
      .replace_hard_limits = FALSE,
      .replace_missings = FALSE,
      .adjust_data_type = FALSE,
      .amend_scale_level = FALSE,
      .apply_factor_metadata = FALSE,
      .apply_factor_metadata_inadm = FALSE
    )))
  }

  base_signature <- util_attr(
    prepare_minimal(),
    "dataquieR_preparation_signature",
    exact = TRUE
  )
  changed_study_data_signature <- util_attr(
    prepare_minimal(.study_data = data.frame(a = c(1L, 3L))),
    "dataquieR_preparation_signature",
    exact = TRUE
  )
  meta_data_with_other_label <- meta_data
  meta_data_with_other_label$LABEL <- "Other label"
  changed_meta_data_signature <- util_attr(
    prepare_minimal(.meta_data = meta_data_with_other_label),
    "dataquieR_preparation_signature",
    exact = TRUE
  )

  expect_length(unique(c(
    base_signature,
    changed_study_data_signature,
    changed_meta_data_signature
  )), 3)
})

test_that("preparation signatures include missing-code rule effects", {
  prep_purge_data_frame_cache()
  util_purge_study_data_cache()
  withr::defer(prep_purge_data_frame_cache())
  withr::defer(util_purge_study_data_cache())

  study_data <- data.frame(
    sex = c(1L, 2L, 1L),
    pregnant = c(NA_integer_, NA_integer_, 5L)
  )
  meta_data <- data.frame(
    VAR_NAMES = c("sex", "pregnant"),
    LABEL = c("sex", "pregnant"),
    DATA_TYPE = c(DATA_TYPES$INTEGER, DATA_TYPES$INTEGER),
    SCALE_LEVEL = c(SCALE_LEVELS$NOMINAL, SCALE_LEVELS$METRIC),
    MISSING_LIST = c("", ""),
    JUMP_LIST = c("", "")
  )

  prepare_with_rule <- function(code_value) {
    prep_add_data_frames(
      data_frame_list = setNames(list(data.frame(
        resp_vars = "pregnant",
        CODE_CLASS = "JUMP",
        CODE_LABEL = paste("not applicable", code_value),
        CODE_VALUE = code_value,
        RULE = "[sex]=1"
      )), MISSING_CODE_RULES)
    )
    prepared_inputs <- util_prepare_dataquieR_inputs(
      study_data = study_data,
      meta_data = meta_data,
      label_col = LABEL,
      update_registry = FALSE
    )
    prepared_ds <- suppressWarnings(suppressMessages(prep_prepare_dataframes(
      .study_data = prepared_inputs$study_data,
      .meta_data = prepared_inputs$meta_data,
      .label_col = LABEL,
      .replace_hard_limits = FALSE,
      .replace_missings = FALSE,
      .adjust_data_type = FALSE,
      .amend_scale_level = FALSE,
      .apply_factor_metadata = FALSE,
      .apply_factor_metadata_inadm = FALSE
    )))
    util_attr(prepared_ds, "dataquieR_preparation_signature", exact = TRUE)
  }

  first_signature <- prepare_with_rule("9999")
  second_signature <- prepare_with_rule("8888")

  expect_false(identical(first_signature, second_signature))
})

test_that("repeated identical preparation hits the study-data cache", {
  util_purge_study_data_cache()
  withr::defer(util_purge_study_data_cache())

  metrics <- new.env(parent = emptyenv())
  withr::local_options(list(
    dataquieR.study_data_cache_metrics = TRUE,
    dataquieR.study_data_cache_metrics_env = metrics
  ))

  study_data <- data.frame(a = c(1L, 2L))
  meta_data <- data.frame(
    VAR_NAMES = "a",
    LABEL = "Alpha",
    DATA_TYPE = DATA_TYPES$INTEGER,
    SCALE_LEVEL = SCALE_LEVELS$RATIO,
    MISSING_LIST = "",
    JUMP_LIST = ""
  )

  prepare_minimal <- function() {
    suppressWarnings(suppressMessages(prep_prepare_dataframes(
      .study_data = study_data,
      .meta_data = meta_data,
      .label_col = LABEL,
      .replace_hard_limits = FALSE,
      .replace_missings = FALSE,
      .adjust_data_type = FALSE,
      .amend_scale_level = FALSE,
      .apply_factor_metadata = FALSE,
      .apply_factor_metadata_inadm = FALSE
    )))
  }

  prepared_once <- prepare_minimal()
  signature <- util_attr(
    prepared_once,
    "dataquieR_preparation_signature",
    exact = TRUE
  )

  expect_true(signature %in% ls(.study_data_cache))
  expect_null(metrics$usage)

  prepared_twice <- prepare_minimal()

  expect_identical(
    util_attr(prepared_twice, "dataquieR_preparation_signature", exact = TRUE),
    signature
  )
  expect_equal(metrics$usage[[signature]], 1)
})

test_that("cache metrics recover from a locked metrics environment", {
  util_purge_study_data_cache()
  withr::defer(util_purge_study_data_cache())

  key <- "locked-metrics-environment"
  .study_data_cache[[key]] <- util_as_prepared_data_frame(data.frame(x = 1))

  metrics <- new.env(parent = emptyenv())
  metrics$usage <- list()
  lockBinding("usage", metrics)
  withr::local_options(list(
    dataquieR.study_data_cache_metrics = TRUE,
    dataquieR.study_data_cache_metrics_env = metrics
  ))

  expect_warning(
    cached <- util_prepare_dataframes_cache_hit(key),
    "expects an unlocked environment",
    fixed = TRUE
  )

  expect_s3_class(cached, "dataquieR_data_frame_prepared")
  expect_equal(cached$x, 1)
})

test_that("repeated identical preparation can hit the early input cache", {
  util_purge_study_data_cache()
  withr::defer(util_purge_study_data_cache())

  metrics <- new.env(parent = emptyenv())
  withr::local_options(list(
    dataquieR.study_data_cache_metrics = TRUE,
    dataquieR.study_data_cache_metrics_env = metrics
  ))

  study_data <- data.frame(a = c(1L, 2L))
  meta_data <- data.frame(
    VAR_NAMES = "a",
    LABEL = "Alpha",
    DATA_TYPE = DATA_TYPES$INTEGER,
    SCALE_LEVEL = SCALE_LEVELS$RATIO,
    MISSING_LIST = "",
    JUMP_LIST = ""
  )

  prepare_minimal <- function() {
    suppressWarnings(suppressMessages(prep_prepare_dataframes(
      .study_data = study_data,
      .meta_data = meta_data,
      .label_col = LABEL,
      .replace_hard_limits = FALSE,
      .replace_missings = FALSE,
      .adjust_data_type = FALSE,
      .amend_scale_level = FALSE,
      .apply_factor_metadata = FALSE,
      .apply_factor_metadata_inadm = FALSE
    )))
  }

  prepared_once <- prepare_minimal()
  signature <- util_attr(
    prepared_once,
    "dataquieR_preparation_signature",
    exact = TRUE
  )

  expect_true(signature %in% ls(.study_data_cache))
  expect_length(ls(.study_data_cache_input_keys), 1)

  prepared_twice <- prepare_minimal()

  expect_identical(
    util_attr(prepared_twice, "dataquieR_preparation_signature", exact = TRUE),
    signature
  )
  expect_equal(metrics$usage[[signature]], 1)
  expect_equal(metrics$early_usage[[signature]], 1)
})

test_that("early input cache preserves internal caller assignments", {
  util_purge_study_data_cache()
  withr::defer(util_purge_study_data_cache())

  metrics <- new.env(parent = emptyenv())
  withr::local_options(list(
    dataquieR.study_data_cache_metrics = TRUE,
    dataquieR.study_data_cache_metrics_env = metrics
  ))

  study_data <- data.frame(a = c(1L, 2L))
  meta_data <- data.frame(
    VAR_NAMES = "a",
    LABEL = "Alpha",
    DATA_TYPE = DATA_TYPES$INTEGER,
    SCALE_LEVEL = SCALE_LEVELS$RATIO,
    MISSING_LIST = "",
    JUMP_LIST = ""
  )

  internal_prepare <- function(study_data, meta_data, label_col = LABEL) {
    prep_prepare_dataframes(
      .replace_hard_limits = FALSE,
      .replace_missings = FALSE,
      .adjust_data_type = FALSE,
      .amend_scale_level = FALSE,
      .apply_factor_metadata = FALSE,
      .apply_factor_metadata_inadm = FALSE
    )
    list(
      ds1 = ds1,
      study_data = study_data,
      meta_data = meta_data,
      label_col = label_col
    )
  }
  environment(internal_prepare) <- asNamespace("dataquieR")

  first <- suppressWarnings(suppressMessages(
    internal_prepare(study_data, meta_data, LABEL)
  ))
  signature <- util_attr(first$ds1,
    "dataquieR_preparation_signature",
    exact = TRUE)

  second <- suppressWarnings(suppressMessages(
    internal_prepare(study_data, meta_data, LABEL)
  ))

  expect_identical(
    util_attr(second$ds1, "dataquieR_preparation_signature", exact = TRUE),
    signature
  )
  expect_equal(metrics$early_usage[[signature]], 1)
  expect_true(is.data.frame(second$study_data))
  expect_true(is.data.frame(second$meta_data))
  expect_identical(second$label_col, LABEL)
})

test_that("on-demand preparation does not export study-data cache to workers", {
  util_purge_study_data_cache()
  withr::defer(util_purge_study_data_cache())

  expect_false(getOption(
    "dataquieR.precomputeStudyData",
    dataquieR.precomputeStudyData_default
  ))

  study_data <- data.frame(a = c(1L, 2L))
  meta_data <- data.frame(
    VAR_NAMES = "a",
    LABEL = "Alpha",
    DATA_TYPE = DATA_TYPES$INTEGER,
    SCALE_LEVEL = SCALE_LEVELS$RATIO,
    MISSING_LIST = "",
    JUMP_LIST = ""
  )

  suppressWarnings(suppressMessages(prep_prepare_dataframes(
    .study_data = study_data,
    .meta_data = meta_data,
    .label_col = LABEL,
    .replace_hard_limits = FALSE,
    .replace_missings = FALSE,
    .adjust_data_type = FALSE,
    .amend_scale_level = FALSE,
    .apply_factor_metadata = FALSE,
    .apply_factor_metadata_inadm = FALSE
  )))

  expect_gt(length(ls(.study_data_cache)), 0)

  withr::local_options(dataquieR.precomputeStudyData = FALSE)
  on_demand_payload <- util_worker_cache_payload()
  expect_equal(on_demand_payload$cache_as_list, list())
  expect_equal(on_demand_payload$study_data_cache, list())

  precomputed_payload <- withr::with_options(
    list(dataquieR.precomputeStudyData = TRUE),
    util_worker_cache_payload()
  )
  expect_gt(length(precomputed_payload$study_data_cache), 0)
})

test_that("study-data cache quick fill tries prepared variants", {
  skip_on_cran()

  calls <- new.env(parent = emptyenv())
  calls$args <- list()
  testthat::local_mocked_bindings(
    prep_prepare_dataframes = function(...) {
      calls$args[[length(calls$args) + 1L]] <- list(...)
      data.frame(a = 1L)
    }
  )

  study_data <- data.frame(a = 1L)
  meta_data <- data.frame(
    VAR_NAMES = "a",
    LABEL = "Alpha",
    DATA_TYPE = DATA_TYPES$INTEGER,
    SCALE_LEVEL = SCALE_LEVELS$RATIO,
    MISSING_LIST = "",
    JUMP_LIST = ""
  )

  util_populate_study_data_cache(study_data, meta_data, LABEL, quick = TRUE)

  expect_length(calls$args, 6L)
  expect_true(calls$args[[1L]]$.replace_hard_limits)
  expect_true(calls$args[[1L]]$.replace_missings)
  expect_true(calls$args[[1L]]$.adjust_data_type)
  expect_true(calls$args[[1L]]$.amend_scale_level)
  expect_false(calls$args[[2L]]$.replace_missings)
  expect_equal(calls$args[[3L]][c(".study_data", ".meta_data", ".label_col")],
    list(.study_data = study_data, .meta_data = meta_data, .label_col = LABEL)
  )
  expect_true(calls$args[[4L]]$.allow_empty)
  expect_true(calls$args[[5L]]$.replace_hard_limits)
  expect_false(calls$args[[6L]]$.replace_missings)
  expect_false(calls$args[[6L]]$.adjust_data_type)
})

test_that("worker cache payload restores raw study-data attributes", {
  util_purge_study_data_cache()
  withr::defer(util_purge_study_data_cache())

  study_data <- data.frame(a = c("1", "2"))
  meta_data <- data.frame(
    VAR_NAMES = "a",
    LABEL = "Alpha",
    DATA_TYPE = DATA_TYPES$INTEGER,
    SCALE_LEVEL = SCALE_LEVELS$RATIO,
    MISSING_LIST = "",
    JUMP_LIST = ""
  )

  suppressWarnings(suppressMessages(prep_prepare_dataframes(
    .study_data = study_data,
    .meta_data = meta_data,
    .label_col = LABEL,
    .replace_hard_limits = FALSE,
    .replace_missings = FALSE,
    .adjust_data_type = TRUE,
    .amend_scale_level = FALSE,
    .apply_factor_metadata = FALSE,
    .apply_factor_metadata_inadm = FALSE
  )))

  original_cache <- as.list(.study_data_cache)
  payload <- withr::with_options(
    list(dataquieR.precomputeStudyData = TRUE),
    util_worker_cache_payload()
  )

  expect_gt(length(payload$study_data_cache), 0)
  expect_gt(length(payload$study_data_cache_study_data_attrs), 0)
  expect_true(all(vapply(
    payload$study_data_cache,
    function(ds1) is.null(util_attr(ds1, "study_data", exact = TRUE)),
    FUN.VALUE = logical(1)
  )))

  restored_cache <- util_worker_cache_restore_study_data_attrs(
    study_data_cache = payload$study_data_cache,
    study_data_attrs = payload$study_data_cache_study_data_attrs
  )

  for (key in names(restored_cache)) {
    expect_identical(
      util_attr(restored_cache[[key]], "study_data", exact = TRUE),
      util_attr(original_cache[[key]], "study_data", exact = TRUE)
    )
    expect_null(util_attr(
      restored_cache[[key]],
      "dataquieR_study_data_cache_raw_key",
      exact = TRUE
    ))
  }
})

test_that("preparation signatures include referenced metadata tables", {
  prep_purge_data_frame_cache()
  util_purge_study_data_cache()
  withr::defer(prep_purge_data_frame_cache())
  withr::defer(util_purge_study_data_cache())

  study_data <- data.frame(a = c(1L, 2L))
  meta_data <- data.frame(
    VAR_NAMES = "a",
    LABEL = "Alpha",
    DATA_TYPE = DATA_TYPES$INTEGER,
    SCALE_LEVEL = SCALE_LEVELS$RATIO,
    MISSING_LIST = "",
    JUMP_LIST = "",
    SIGNATURE_TABLE = "signature_table"
  )

  prepare_minimal <- function(table_version) {
    prep_add_data_frames(signature_table = data.frame(version = table_version))
    suppressWarnings(suppressMessages(prep_prepare_dataframes(
      .study_data = study_data,
      .meta_data = meta_data,
      .label_col = LABEL,
      .replace_hard_limits = FALSE,
      .replace_missings = FALSE,
      .adjust_data_type = FALSE,
      .amend_scale_level = FALSE,
      .apply_factor_metadata = FALSE,
      .apply_factor_metadata_inadm = FALSE
    )))
  }

  first_signature <- util_attr(
    prepare_minimal(1L),
    "dataquieR_preparation_signature",
    exact = TRUE
  )
  second_signature <- util_attr(
    prepare_minimal(2L),
    "dataquieR_preparation_signature",
    exact = TRUE
  )

  expect_false(identical(first_signature, second_signature))
})

test_that("early input cache includes referenced metadata table contents", {
  prep_purge_data_frame_cache()
  util_purge_study_data_cache()
  withr::defer(prep_purge_data_frame_cache())
  withr::defer(util_purge_study_data_cache())

  metrics <- new.env(parent = emptyenv())
  withr::local_options(list(
    dataquieR.study_data_cache_metrics = TRUE,
    dataquieR.study_data_cache_metrics_env = metrics
  ))

  study_data <- data.frame(a = c(1L, 2L))
  meta_data <- data.frame(
    VAR_NAMES = "a",
    LABEL = "Alpha",
    DATA_TYPE = DATA_TYPES$INTEGER,
    SCALE_LEVEL = SCALE_LEVELS$RATIO,
    MISSING_LIST = "",
    JUMP_LIST = "",
    SIGNATURE_TABLE = "signature_table"
  )

  prepare_minimal <- function(table_version) {
    prep_add_data_frames(signature_table = data.frame(version = table_version))
    suppressWarnings(suppressMessages(prep_prepare_dataframes(
      .study_data = study_data,
      .meta_data = meta_data,
      .label_col = LABEL,
      .replace_hard_limits = FALSE,
      .replace_missings = FALSE,
      .adjust_data_type = FALSE,
      .amend_scale_level = FALSE,
      .apply_factor_metadata = FALSE,
      .apply_factor_metadata_inadm = FALSE
    )))
  }

  first_signature <- util_attr(
    prepare_minimal(1L),
    "dataquieR_preparation_signature",
    exact = TRUE
  )

  second_signature <- util_attr(
    prepare_minimal(2L),
    "dataquieR_preparation_signature",
    exact = TRUE
  )

  expect_false(identical(first_signature, second_signature))
  expect_null(metrics$early_usage)
})

test_that("early input cache does not hide metadata prediction warnings", {
  prep_purge_data_frame_cache()
  util_purge_study_data_cache()
  withr::defer(prep_purge_data_frame_cache())
  withr::defer(util_purge_study_data_cache())

  metrics <- new.env(parent = emptyenv())
  withr::local_options(list(
    dataquieR.study_data_cache_metrics = TRUE,
    dataquieR.study_data_cache_metrics_env = metrics
  ))

  study_data <- data.frame(a = c(1L, 2L))
  meta_data <- data.frame(
    VAR_NAMES = "a",
    LABEL = "Alpha",
    DATA_TYPE = NA_character_,
    SCALE_LEVEL = SCALE_LEVELS$RATIO,
    MISSING_LIST = "",
    JUMP_LIST = ""
  )

  prepare_minimal <- function() {
    suppressMessages(prep_prepare_dataframes(
      .study_data = study_data,
      .meta_data = meta_data,
      .label_col = LABEL,
      .replace_hard_limits = FALSE,
      .replace_missings = FALSE,
      .adjust_data_type = FALSE,
      .amend_scale_level = FALSE,
      .apply_factor_metadata = FALSE,
      .apply_factor_metadata_inadm = FALSE
    ))
  }

  expect_warning(
    prepare_minimal(),
    "I've predicted the.+DATA_TYPE.+study_data",
    perl = TRUE
  )
  expect_warning(
    prepare_minimal(),
    "I've predicted the.+DATA_TYPE.+study_data",
    perl = TRUE
  )
  expect_null(metrics$early_usage)
})
