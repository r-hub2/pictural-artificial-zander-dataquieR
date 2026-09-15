skip_on_cran()

test_that("default behavior returns list of resultsets", {
  skip_on_cran() # slow
  skip_if_not_installed("stringdist")
  study_data <- data.frame(
    id = 1:4,
    CENTER = c("A", "A", "B", "B"),
    val1 = c(1, NA, 3, 4),
    val2 = c(5, 6, NA, 8)
  )
  meta_data <- data.frame(
    VAR_NAMES = c("val1", "val2", "CENTER"),
    LABEL = c("Variable 1", "Variable 2", "Center"),
    DATA_TYPE = c("integer", "integer", "string"),
    MISSING_LIST = c("", "", ""),
    VALUE_LABELS = c(NA, NA, "A|B"),
    stringsAsFactors = FALSE
  )

  rep <- dq_report_by(study_data, meta_data,
    label_col = "LABEL",
    strata_column = "CENTER", segment_column = NULL,
    filter_indicator_functions = "int_datatype_matrix", cores = NULL
  )
  expect_true(is.list(rep))
  expect_named(rep$all_variables, c("Center_A", "Center_B"))
  expect_true(all(sapply(rep$all_variables, inherits, "dataquieR_resultset2")))
})

test_that("segment_column = NULL returns flat structure", {
  skip_on_cran() # slow
  skip_if_not_installed("stringdist")
  study_data <- data.frame(
    id = 1:4,
    CENTER = c("A", "A", "B", "B"),
    val1 = c(1, NA, 3, 4),
    val2 = c(5, 6, NA, 8)
  )
  meta_data <- data.frame(
    VAR_NAMES = c("val1", "val2", "CENTER"),
    LABEL = c("Variable 1", "Variable 2", "Center"),
    DATA_TYPE = c("integer", "integer", "string"),
    MISSING_LIST = c("", "", ""),
    VALUE_LABELS = c(NA, NA, "A|B"),
    stringsAsFactors = FALSE
  )
  rep2 <- dq_report_by(study_data, meta_data,
    label_col = "LABEL",
    strata_column = "CENTER", segment_column = NULL,
    filter_indicator_functions = "int_datatype_matrix", cores = NULL
  )
  expect_true(all(sapply(rep2, function(r) is.list(r) && is.null(r$segment))))
})

test_that("missing strata are returned as a separate report", {
  skip_on_cran() # slow
  skip_if_not_installed("stringdist")

  study_data <- data.frame(
    id = 1:5,
    CENTER = c("A", NA, "B", NA, "A"),
    val1 = c(1, NA, 3, 4, 5),
    val2 = c(5, 6, NA, 8, 9)
  )
  meta_data <- data.frame(
    VAR_NAMES = c("val1", "val2", "CENTER"),
    LABEL = c("Variable 1", "Variable 2", "Center"),
    DATA_TYPE = c("integer", "integer", "string"),
    MISSING_LIST = c("", "", ""),
    VALUE_LABELS = c(NA, NA, "A|B"),
    stringsAsFactors = FALSE
  )

  report <- suppressMessages(suppressWarnings(dq_report_by(
    study_data,
    meta_data,
    label_col = "LABEL",
    strata_column = "CENTER",
    segment_column = NULL,
    filter_indicator_functions = "int_datatype_matrix",
    cores = NULL
  )))

  expect_named(
    report$all_variables,
    c("Center_A", "Center_B", "Center_NAs_group")
  )
  expect_true(all(vapply(
    report$all_variables,
    inherits,
    logical(1),
    what = "dataquieR_resultset2"
  )))
})

test_that("regex strata selection keeps only matching reports", {
  skip_on_cran() # slow
  skip_if_not_installed("stringdist")

  study_data <- data.frame(
    id = 1:4,
    CENTER = c("Alpha", "Alpha", "Beta", "Beta"),
    val1 = c(1, NA, 3, 4),
    val2 = c(5, 6, NA, 8)
  )
  meta_data <- data.frame(
    VAR_NAMES = c("val1", "val2", "CENTER"),
    LABEL = c("Variable 1", "Variable 2", "Center"),
    DATA_TYPE = c("integer", "integer", "string"),
    MISSING_LIST = c("", "", ""),
    VALUE_LABELS = c(NA, NA, "Alpha|Beta"),
    stringsAsFactors = FALSE
  )

  report <- suppressMessages(suppressWarnings(dq_report_by(
    study_data,
    meta_data,
    label_col = "LABEL",
    strata_column = "CENTER",
    strata_select = "^Al",
    selection_type = "regex",
    segment_column = NULL,
    filter_indicator_functions = "int_datatype_matrix",
    cores = NULL
  )))

  expect_named(report$all_variables, "Center_Alpha")
})

test_that("disable_plotly disables plotly in HTML output", {
  skip_on_cran() # slow

  skip_if_not_installed("DT")

  study_data <- data.frame(
    id = 1:4,
    CENTER = c("A", "A", "B", "B"),
    val1 = c(1, NA, 3, 4),
    val2 = c(5, 6, NA, 8)
  )
  meta_data <- data.frame(
    VAR_NAMES = c("val1", "val2", "CENTER"),
    LABEL = c("Variable 1", "Variable 2", "Center"),
    DATA_TYPE = c("integer", "integer", "string"),
    MISSING_LIST = c("", "", ""),
    VALUE_LABELS = c(NA, NA, "A|B"),
    stringsAsFactors = FALSE
  )
  td <- tempfile()
  withr::defer(unlink(td, recursive = TRUE))
  result <- dq_report_by(study_data, meta_data,
    label_col = "LABEL",
    strata_column = "CENTER",
    output_dir = td, also_print = TRUE, view = FALSE,
    disable_plotly = TRUE, segment_column = NULL,
    filter_indicator_functions = "int_datatype_matrix",
    cores = NULL
  )
  html_files <- list.files(td,
    pattern = "\\.html$", full.names = TRUE,
    all.files = TRUE, recursive = TRUE
  )
  expect_gt(length(html_files), 0)
  expect_false(any(basename(html_files) == "sunburst.html"))
  html_contents <- paste(unlist(lapply(
    html_files,
    function(f) readLines(f, warn = FALSE)
  )), collapse = "\n")

  expect_false(grepl("plotly",
      gsub("disable_plotly", "", html_contents, fixed = TRUE),
      fixed = TRUE
    ))
  index_html <- paste(readLines(file.path(td, "index.html"), warn = FALSE),
    collapse = "\n"
  )
  expect_true(grepl("Items: Summary table", index_html, fixed = TRUE))
  expect_true(grepl("Dashboard", index_html, fixed = TRUE))
  expect_false(grepl("Sunburst", index_html, fixed = TRUE))
})

test_that("HTML progress error page does not depend on view", {
  td <- withr::local_tempdir()
  content_file <- file.path(td, "index.html")

  local({
    .hi <- .hp <- .hm <- NULL
    list2env(
      util_init_html_progress(
        output_dir = td,
        content_file = content_file,
        title = "Test report",
        view = FALSE,
        rep_id = "test"
      ),
      envir = environment()
    )
    util_call_progress_hooks(
      type = "progress",
      n = 4,
      percent = 50,
      status = "Rendering",
      msg = "halfway"
    )
    progress_html <- paste(readLines(content_file, warn = FALSE),
      collapse = "\n"
    )
    expect_true(grepl("Rendering", progress_html, fixed = TRUE))
    expect_true(grepl("halfway", progress_html, fixed = TRUE))
    expect_true(file.exists(file.path(td, ".report", "renderinfo.js")))
  })

  html <- paste(readLines(content_file, warn = FALSE), collapse = "\n")
  expect_true(grepl("Report was not created", html, fixed = TRUE))
  expect_false(file.exists(file.path(td, ".report", "renderinfo.js")))
})

test_that("also_print writes html files and returns a result list", {
  skip_on_cran() # slow

  skip_if_not_installed("DT")
  study_data <- data.frame(
    id = 1:4,
    CENTER = c("A", "A", "B", "B"),
    val1 = c(1, NA, 3, 4),
    val2 = c(5, 6, NA, 8)
  )
  meta_data <- data.frame(
    VAR_NAMES = c("val1", "val2", "CENTER"),
    LABEL = c("Variable 1", "Variable 2", "Center"),
    DATA_TYPE = c("integer", "integer", "string"),
    MISSING_LIST = c("", "", ""),
    VALUE_LABELS = c(NA, NA, "A|B"),
    stringsAsFactors = FALSE
  )
  td <- tempfile()
  withr::defer(unlink(td, recursive = TRUE))
  result <- dq_report_by(study_data, meta_data,
    label_col = "LABEL",
    strata_column = "CENTER",
    output_dir = td, also_print = TRUE, segment_column = NULL,
    filter_indicator_functions = "int_datatype_matrix", cores = NULL
  )
  expect_type(result, "list")
  expect_true(file.exists(file.path(td, "report_by_meta.RDS")))
  report_by_meta <- readRDS(file.path(td, "report_by_meta.RDS"))
  expect_named(report_by_meta, c(
    "strata_column", "segment_column", "strata_column_label", "subgroup",
    "mod_label", "disable_plotly", "advanced_options", "html_table_backend",
    "title", "start_time", "rep_id", "subtitle", "author", "user_info",
    "by_call", "call_report_by", "call_report_by_overview", "report_files"
  ))
  html_files <- list.files(td,
    pattern = "\\.html$", full.names = TRUE,
    all.files = TRUE, recursive = TRUE
  )
  expect_gt(length(html_files), 0)
  html_contents <- paste(unlist(lapply(
    html_files,
    function(f) readLines(f, warn = FALSE)
  )), collapse = "\n")
  expect_false(grepl("NA/.report", html_contents, fixed = TRUE))
  expect_true(grepl("dq-report-overview-back", html_contents, fixed = TRUE))
  js_files <- list.files(td,
    pattern = "script_toplevel[.]js$",
    full.names = TRUE, all.files = TRUE,
    recursive = TRUE
  )
  js_contents <- paste(unlist(lapply(
    js_files,
    function(f) readLines(f, warn = FALSE)
  )), collapse = "\n")
  expect_true(grepl("dq-breadcrumb-overview", js_contents, fixed = TRUE))
  expect_true(grepl("floatbarTop", js_contents, fixed = TRUE))
  expect_true(grepl("float_menus()", js_contents, fixed = TRUE))
  expect_true(grepl(
    "report_study_data_Center_A_all_variables/.report/VAR_Variable1.html#Variable1.int_datatype_matrix", # nolint: line_length_linter.
    html_contents,
    fixed = TRUE
  ))
})

test_that("missing label_col is handled gracefully", {
  skip_on_cran() # slow

  skip_if_not_installed("stringdist")
  study_data <- data.frame(
    id = 1:4,
    CENTER = c("A", "A", "B", "B"),
    val1 = c(1, NA, 3, 4),
    val2 = c(5, 6, NA, 8)
  )
  meta_data <- data.frame(
    VAR_NAMES = c("val1", "val2", "CENTER"),
    DATA_TYPE = c("integer", "integer", "string"),
    MISSING_LIST = c("", "", ""),
    VALUE_LABELS = c(NA, NA, "A|B"),
    stringsAsFactors = FALSE
  )

  result <- dq_report_by(
    study_data, meta_data,
    label_col = "LABEL",
    strata_column = "CENTER",
    segment_column = NULL,
    filter_indicator_functions = "int_datatype_matrix",
    cores = NULL
  )

  expect_type(result, "list")
})

test_that("prepared study data use raw variable names in dq_report_by", {
  skip_on_cran() # slow

  study_data <- data.frame(
    CENTER = c("A", "A"),
    val = c("1", "x"),
    stringsAsFactors = FALSE
  )
  meta_data <- data.frame(
    VAR_NAMES = c("CENTER", "val"),
    LABEL = c("Center", "Value"),
    DATA_TYPE = c("string", "integer"),
    SCALE_LEVEL = c("nominal", "metric"),
    MISSING_LIST = c("", ""),
    JUMP_LIST = c("", ""),
    HARD_LIMITS = c("", ""),
    VARIABLE_ROLE = c("primary", "primary"),
    VALUE_LABELS = c("A", NA),
    stringsAsFactors = FALSE
  )

  prepared <- suppressWarnings(suppressMessages(prep_prepare_dataframes(
    study_data,
    meta_data,
    LABEL,
    .replace_missings = FALSE,
    .replace_hard_limits = FALSE,
    .adjust_data_type = TRUE
  )))

  result <- dq_report_by(
    prepared,
    meta_data,
    label_col = "LABEL",
    strata_column = "CENTER",
    segment_column = NULL,
    dimensions = "int",
    filter_indicator_functions = "int_datatype_matrix",
    cores = NULL,
    view = FALSE
  )

  expect_named(result$all_variables, "Center_A")
  expect_true(any(grepl(
    "int_datatype_matrix",
    names(result$all_variables$Center_A),
    fixed = TRUE
  )))
})

test_that("dq_report_by repairs duplicate labels before dq_report2", {
  skip_on_cran() # slow

  study_data <- data.frame(
    CENTER = c("A", "A", "B", "B"),
    val1 = c(1, 2, 3, 4),
    val2 = c(5, 6, 7, 8)
  )
  meta_data <- data.frame(
    VAR_NAMES = c("CENTER", "val1", "val2"),
    LABEL = c("Center", "Duplicate label", "Duplicate label"),
    DATA_TYPE = c("string", "integer", "integer"),
    SCALE_LEVEL = c("nominal", "metric", "metric"),
    MISSING_LIST = c("", "", ""),
    JUMP_LIST = c("", "", ""),
    HARD_LIMITS = c("", "", ""),
    VARIABLE_ROLE = c("primary", "primary", "primary"),
    VALUE_LABELS = c("A|B", NA, NA),
    stringsAsFactors = FALSE
  )

  warnings <- new.env(parent = emptyenv())
  warnings$values <- character()
  result <- withCallingHandlers(
    dq_report_by(
      study_data,
      meta_data,
      label_col = "LABEL",
      strata_column = "CENTER",
      segment_column = NULL,
      dimensions = "int",
      filter_indicator_functions = "int_datatype_matrix",
      cores = NULL,
      view = FALSE
    ),
    warning = function(w) {
      warnings$values <- c(warnings$values, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  expect_true(any(grepl(
    "Some variables have duplicated labels in .+LABEL",
    warnings$values
  )))
  expect_false(any(grepl(
    "duplicated in the metadata and cannot be used as label",
    warnings$values,
    fixed = TRUE
  )))
  expect_true(inherits(
    result$all_variables$Center_A,
    "dataquieR_resultset2"
  ))
  expect_match(
    util_attr(result$all_variables$Center_A,
      "label_modification_text",
      exact = TRUE
    ),
    "duplicated labels"
  )
})
