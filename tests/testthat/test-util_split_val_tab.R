skip_on_cran()

test_that("util_split_val_tab works", {
  skip_on_cran()
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  prep_purge_data_frame_cache()

  test <- tibble::tribble(
    ~CODE_VALUE, ~CODE_LABEL, ~CODE_CLASS, ~CODE_INTERPRET, ~CODE_ORDER,
    ~VALUE_LABEL_TABLE, ~MISSING_LIST_TABLE,
    1234, "Test1", "MISSING", "P", 1, "vtab1", "mtab1",
    2234, "Test2", "JUMP", "P", 1, "vtab1", "mtab1",
    3234, "Test3", "VALUE", NA, 1, "vtab1", "vtab1",
    4234, "Test4", "MISSING", "P", 1, "vtab1", "mtab2",
    5234, "Test5", "MISSING", "P", 1, "vtab1", "mtab2",
    3234, "Test999", "VALUE", NA, 1, "vtab2", NA
  )
  expect_warning(util_split_val_tab(test))
  expect_equal(
    sort(prep_list_dataframes()),
    c("mtab1", "mtab2", "vtab1", "vtab2")
  )
  prep_purge_data_frame_cache()

  test <- tibble::tribble(
    ~CODE_VALUE, ~CODE_LABEL, ~CODE_CLASS, ~CODE_INTERPRET, ~CODE_ORDER,
    ~VALUE_LABEL_TABLE, ~MISSING_LIST_TABLE,
    1234, "Test1", "MISSING", "P", 1, "vtab1", "mtab1",
    2234, "Test2", "JUMP", "P", 1, "vtab1", "mtab1",
    3234, "Test3", "VALUE", NA, 1, "vtab1", "",
    4234, "Test4", "MISSING", "P", 1, "vtab1", "mtab2",
    5234, "Test5", "MISSING", "P", 1, "vtab1", "mtab2",
    3234, "Test999", "VALUE", NA, 1, "vtab2", NA
  )
  util_split_val_tab(test)
  expect_equal(
    sort(prep_list_dataframes()),
    c("mtab1", "mtab2", "vtab1", "vtab2")
  )
  prep_purge_data_frame_cache()

  test <- tibble::tribble(
    ~CODE_VALUE, ~CODE_LABEL, ~CODE_CLASS, ~CODE_INTERPRET, ~CODE_ORDER,
    ~CODE_LIST_TABLE,
    1234, "Test1", "MISSING", "P", 1, "tab1",
    2234, "Test2", "JUMP", "P", 1, "tab1",
    3234, "Test3", "VALUE", NA, 1, "tab1",
    4234, "Test4", "MISSING", "P", 1, "tab1",
    5234, "Test5", "MISSING", "P", 1, "tab1",
    3234, "Test999", "VALUE", NA, 1, "tab2",
    3235, "Test99x9", "VALUE", NA, 2, "tab2",
    3236, "Test999a", "MISSING", NA, 1, "tab4",
    3237, "Test99x9b", "JUMP", NA, 2, "tab4",
    0, "no", "VALUE", NA, 1, "tab9",
    1, "yes", "VALUE", NA, 2, "tab9",
    3234, "MISSING", "MISSING", "P", 1, "tab9"
  )
  expect_warning(util_split_val_tab(test))
  expect_equal(prep_list_dataframes(), c("tab1", "tab2", "tab4", "tab9"))

  # Use prep_purge_data_frame_cache(), prep_load_workbook_like_file(),
  # prep_get_data_frame(), util_normalize_value_labels(),
  # prep_add_data_frames(), prep_get_labels(), and dq_report2() locally for
  # this cache smoke check.
})

test_that("value label normalization is idempotent for stable generated names", { # nolint: line_length_linter.
  skip_on_cran()
  prep_purge_data_frame_cache()
  value_labels <- paste(
    "0 = never",
    "1 = 1-2d a week",
    "2 = 3-4d a week",
    "3 = 5-6d a week",
    "4 = daily",
    sep = " < "
  )
  table_name <- .util_generate_value_label_table_name(value_labels)
  prep_add_data_frames(data_frame_list = setNames(list(data.frame(
    CODE_VALUE = as.character(0:4),
    CODE_LABEL = c(
      "never", "1-2d a week", "3-4d a week",
      "5-6d a week", "daily"
    ),
    stringsAsFactors = FALSE
  )), table_name))
  meta_data <- data.frame(
    VAR_NAMES = "v00023",
    LABEL = "MEAT_CONS_0",
    VALUE_LABELS = value_labels,
    stringsAsFactors = FALSE
  )

  util_normalize_value_labels(meta_data)
  util_normalize_value_labels(meta_data)

  expect_equal(prep_get_data_frame(table_name), data.frame(
    CODE_VALUE = as.character(0:4),
    CODE_LABEL = c(
      "never", "1-2d a week", "3-4d a week",
      "5-6d a week", "daily"
    ),
    CODE_ORDER = 1:5,
    stringsAsFactors = FALSE
  ))
})

test_that("value label normalization leaves unrelated metadata unchanged", {
  meta_data <- data.frame(
    VAR_NAMES = "v1",
    LABEL = "Variable 1",
    stringsAsFactors = FALSE
  )

  expect_equal(util_normalize_value_labels(meta_data), meta_data)
})

test_that("value label normalization validates maximum label length", {
  meta_data <- data.frame(
    VAR_NAMES = "v1",
    VALUE_LABELS = "1 = yes",
    stringsAsFactors = FALSE
  )

  expect_error(
    util_normalize_value_labels(meta_data, max_value_label_len = "short"),
    "must be a numeric value"
  )
})

test_that("value label normalization combines explicit and generated tables", {
  prep_purge_data_frame_cache()
  withr::defer(prep_purge_data_frame_cache())

  prep_add_data_frames(data_frame_list = list(
    existing_labs = data.frame(
      CODE_VALUE = "1",
      CODE_LABEL = "one",
      stringsAsFactors = FALSE
    )
  ))
  meta_data <- data.frame(
    VAR_NAMES = "v1",
    VALUE_LABELS = "2 = two",
    VALUE_LABEL_TABLE = "existing_labs",
    stringsAsFactors = FALSE
  )

  expect_warning(
    normalized <- util_normalize_value_labels(meta_data),
    "Cannot mix"
  )

  expect_false(VALUE_LABELS %in% names(normalized))
  expect_equal(normalized[[VALUE_LABEL_TABLE]], "existing_labs_VALUE_LABELS")
  expect_equal(
    prep_get_data_frame("existing_labs_VALUE_LABELS"),
    data.frame(
      CODE_VALUE = c("1", "2"),
      CODE_LABEL = c("one", "two"),
      stringsAsFactors = FALSE
    )
  )
})

test_that("value label normalization labels long code-only tables", {
  prep_purge_data_frame_cache()
  withr::defer(prep_purge_data_frame_cache())

  prep_add_data_frames(data_frame_list = list(
    long_labs = data.frame(
      CODE_VALUE = c(
        "a very long display value",
        "another very long display value"
      ),
      stringsAsFactors = FALSE
    )
  ))
  meta_data <- data.frame(
    VAR_NAMES = "v1",
    VALUE_LABEL_TABLE = "long_labs",
    stringsAsFactors = FALSE
  )

  normalized <- util_normalize_value_labels(meta_data,
    max_value_label_len = 12
  )
  normalized_labs <- prep_get_data_frame(normalized[[VALUE_LABEL_TABLE]])

  expect_true(CODE_LABEL %in% colnames(normalized_labs))
  expect_true(all(nchar(normalized_labs[[CODE_LABEL]]) <= 12))
})

test_that("value label table combination merges duplicate labels and metadata", { # nolint: line_length_linter.
  vlt1 <- data.frame(
    CODE_VALUE = c("1", "1", "2"),
    CODE_LABEL = c("one", "single", "two"),
    EXTRA = c("", "preferred", "kept"),
    stringsAsFactors = FALSE
  )
  vlt2 <- data.frame(
    CODE_VALUE = c("2", "3"),
    CODE_LABEL = c("deux", "three"),
    EXTRA = c("", "new"),
    stringsAsFactors = FALSE
  )

  combined <- util_combine_value_label_tables(vlt1, vlt2)

  expect_equal(combined[[CODE_VALUE]], c("1", "2", "3"))
  expect_equal(combined[[CODE_LABEL]], c(
    "one | single", "two | deux",
    "three"
  ))
  expect_equal(combined$EXTRA, c("preferred", "kept", "new"))
})

test_that("prep_unsplit_val_tabs works", {
  skip_on_cran()
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  prep_purge_data_frame_cache()
  prep_load_workbook_like_file("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx") # nolint: line_length_linter.
  prep_unsplit_val_tabs(val_tab = CODE_LIST_TABLE)
  # Use prep_open_in_excel() to edit CODE_LIST_TABLE in Excel locally.
  clt <- prep_get_data_frame(CODE_LIST_TABLE)
  # Use View() to inspect clt interactively when updating the snapshot.
  expect_snapshot(clt)

  prep_purge_data_frame_cache()
  util_split_val_tab(clt)
  expect_snapshot(prep_list_dataframes())
})

test_that(
  "prep_unsplit_val_tabs combines cached local value and missing tables",
  {
    skip_on_cran()
    prep_purge_data_frame_cache()
    withr::defer(prep_purge_data_frame_cache())

    prep_add_data_frames(
      value_tab = data.frame(
        CODE_VALUE = c("0", "1"),
        CODE_LABEL = c("no", "yes"),
        stringsAsFactors = FALSE
      ),
      missing_tab = data.frame(
        CODE_VALUE = "99",
        CODE_LABEL = "missing",
        CODE_INTERPRET = "NA",
        stringsAsFactors = FALSE
      )
    )
    meta_data <- data.frame(
      VAR_NAMES = c("x", "y"),
      LABEL = c("X", "Y"),
      DATA_TYPE = DATA_TYPES$STRING,
      SCALE_LEVEL = SCALE_LEVELS$NOMINAL,
      VALUE_LABEL_TABLE = c("value_tab", NA_character_),
      MISSING_LIST_TABLE = c(NA_character_, "missing_tab"),
      MISSING_LIST = SPLIT_CHAR,
      JUMP_LIST = SPLIT_CHAR,
      stringsAsFactors = FALSE
    )

    combined <- suppressWarnings(prep_unsplit_val_tabs(meta_data))

    expect_equal(as.character(combined[[CODE_VALUE]]), c("99", "0", "1"))
    expect_equal(as.character(combined[[CODE_CLASS]]),
      c("MISSING", "VALUE", "VALUE"))
    expect_equal(as.character(combined[[VALUE_LABEL_TABLE]]),
      c(NA_character_, "value_tab", "value_tab"))
    expect_equal(as.character(combined[[MISSING_LIST_TABLE]]),
      c("missing_tab_Y", NA_character_, NA_character_))
    expect_equal(attr(combined, "meta_data", exact = TRUE)[[VAR_NAMES]],
      c("x", "y"))
  }
)

test_that("split value tables prefer explicit split columns over code list", {
  skip_on_cran()
  prep_purge_data_frame_cache()
  withr::defer(prep_purge_data_frame_cache())

  mixed_table <- data.frame(
    CODE_VALUE = c("1", "99"),
    CODE_LABEL = c("yes", "missing"),
    CODE_CLASS = c(CODE_CLASSES$VALUE, CODE_CLASSES$MISSING),
    CODE_INTERPRET = c(NA_character_, "NA"),
    CODE_LIST_TABLE = "ignored_code_list",
    VALUE_LABEL_TABLE = c("value_tab", NA_character_),
    MISSING_LIST_TABLE = c(NA_character_, "missing_tab"),
    stringsAsFactors = FALSE
  )

  expect_warning(
    util_split_val_tab(mixed_table),
    "ignoring"
  )

  expect_equal(sort(prep_list_dataframes()), c("missing_tab", "value_tab"))
  expect_false(CODE_LIST_TABLE %in% names(prep_get_data_frame("value_tab")))
  expect_false(CODE_LIST_TABLE %in% names(prep_get_data_frame("missing_tab")))
})

test_that("prep_unsplit_val_tabs retains one cached value table", {
  skip_on_cran()
  prep_purge_data_frame_cache()
  withr::defer(prep_purge_data_frame_cache())

  prep_add_data_frames(
    value_tab = data.frame(
      CODE_VALUE = c("0", "1"),
      CODE_LABEL = c("no", "yes"),
      stringsAsFactors = FALSE
    )
  )
  meta_data <- data.frame(
    VAR_NAMES = "answer",
    DATA_TYPE = DATA_TYPES$INTEGER,
    SCALE_LEVEL = SCALE_LEVELS$NOMINAL,
    VALUE_LABEL_TABLE = "value_tab",
    MISSING_LIST = SPLIT_CHAR,
    JUMP_LIST = SPLIT_CHAR,
    stringsAsFactors = FALSE
  )

  combined <- prep_unsplit_val_tabs(meta_data)

  expect_s3_class(combined, "data.frame")
  expect_equal(as.character(combined[[CODE_VALUE]]), c("0", "1"))
  expect_true(all(combined[[CODE_CLASS]] == CODE_CLASSES$VALUE))
})

test_that("prep_unsplit_val_tabs handles metadata without table columns", {
  skip_on_cran()
  prep_purge_data_frame_cache()
  withr::defer(prep_purge_data_frame_cache())

  meta_data <- data.frame(
    VAR_NAMES = "answer",
    LABEL = "Answer",
    DATA_TYPE = DATA_TYPES$STRING,
    SCALE_LEVEL = SCALE_LEVELS$NOMINAL,
    MISSING_LIST = SPLIT_CHAR,
    JUMP_LIST = SPLIT_CHAR,
    stringsAsFactors = FALSE
  )

  combined <- suppressWarnings(prep_unsplit_val_tabs(meta_data))

  expect_equal(dim(combined), c(0L, 0L))
  normalized_meta <- attr(combined, "meta_data", exact = TRUE)
  expect_true(all(c(VALUE_LABEL_TABLE, MISSING_LIST_TABLE) %in%
        names(normalized_meta)))
  expect_equal(normalized_meta[[VAR_NAMES]], "answer")
})

test_that("prep_unsplit_val_tabs warns before overwriting cached tables", {
  skip_on_cran()
  prep_purge_data_frame_cache()
  withr::defer(prep_purge_data_frame_cache())

  prep_add_data_frames(data_frame_list = list(
    value_tab = data.frame(
      CODE_VALUE = "1",
      CODE_LABEL = "yes",
      stringsAsFactors = FALSE
    ),
    missing_tab = data.frame(
      CODE_VALUE = "99",
      CODE_LABEL = "missing",
      CODE_INTERPRET = "NA",
      stringsAsFactors = FALSE
    ),
    CODE_LIST_TABLE = data.frame(
      CODE_VALUE = "old",
      CODE_LABEL = "old",
      stringsAsFactors = FALSE
    )
  ))
  meta_data <- data.frame(
    VAR_NAMES = c("x", "y"),
    LABEL = c("X", "Y"),
    DATA_TYPE = DATA_TYPES$STRING,
    SCALE_LEVEL = SCALE_LEVELS$NOMINAL,
    VALUE_LABEL_TABLE = c("value_tab", NA_character_),
    MISSING_LIST_TABLE = c(NA_character_, "missing_tab"),
    MISSING_LIST = SPLIT_CHAR,
    JUMP_LIST = SPLIT_CHAR,
    stringsAsFactors = FALSE
  )

  expect_warning(
    expect_warning(
      combined <- prep_unsplit_val_tabs(meta_data, val_tab = CODE_LIST_TABLE),
      "will overwrite"
    ),
    "Should not have"
  )

  expect_equal(as.character(combined[[CODE_VALUE]]), c("99", "1"))
  expect_equal(
    as.character(prep_get_data_frame(CODE_LIST_TABLE)[[CODE_VALUE]]),
    c("99", "1")
  )
})

test_that("util_handle_val_tab splits and removes the combined cache table", {
  skip_on_cran()
  prep_purge_data_frame_cache()
  withr::defer(prep_purge_data_frame_cache())

  prep_add_data_frames(data_frame_list = setNames(list(data.frame(
    CODE_VALUE = c("1", "99"),
    CODE_LABEL = c("yes", "missing"),
    CODE_CLASS = c(CODE_CLASSES$VALUE, CODE_CLASSES$MISSING),
    CODE_INTERPRET = c(NA_character_, "NA"),
    CODE_LIST_TABLE = "shared_tab",
    stringsAsFactors = FALSE
  )), CODE_LIST_TABLE))

  expect_warning(util_handle_val_tab(), "missing codes")

  expect_equal(prep_list_dataframes(), "shared_tab")
  expect_equal(
    as.character(prep_get_data_frame("shared_tab")[[CODE_VALUE]]),
    c("99", "1")
  )
})
