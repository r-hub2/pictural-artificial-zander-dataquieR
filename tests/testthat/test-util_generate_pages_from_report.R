skip_on_cran()

# Coverage-focused tests live in the matching coverage file.

.get_mini_report <- function() {
  env <- testthat::test_env()

  if (!is.null(env$.mini_report)) {
    return(env$.mini_report)
  }

  skip_on_cran() # slow, errors unlikely
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")

  dataquieR::prep_load_workbook_like_file(
    "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx" # nolint: line_length_linter.
  )

  withr::defer(
    dataquieR::prep_purge_data_frame_cache(),
    envir = env
  )

  env$.mini_report <- dataquieR::dq_report2(
    "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData", # nolint: line_length_linter.
    resp_vars = c("SEX_0", "AGE_0"),
    dimensions = c("int"),
    label_col = LABEL,
    cores = NULL
  )

  env$.mini_report
}

.call_pages <- function(report, disable_plotly = TRUE, my_dashboard = NULL,
  ...) {
  skip_if_not_installed("DT")
  skip_if_not_installed("markdown")

  dataquieR:::util_generate_pages_from_report( # nolint
    report = report,
    template = "default",
    disable_plotly = disable_plotly,

    # required + avoid recursive defaults
    progress = function(pct) invisible(NULL),
    progress_msg = function(...) invisible(NULL),
    block_load_factor = 1,
    dir = tempdir(),
    my_dashboard = my_dashboard,
    ...
  )
}

.get_local_segment_only_report <- function() {
  skip_on_cran()

  prep_purge_data_frame_cache()
  withr::defer(prep_purge_data_frame_cache())

  study_data <- data.frame(
    a = c(1, NA, 3, 4),
    b = c(1, 2, NA, 4)
  )
  item_level <- data.frame(
    VAR_NAMES = names(study_data),
    LABEL = c("Segment A item", "Segment B item"),
    DATA_TYPE = DATA_TYPES$INTEGER,
    SCALE_LEVEL = SCALE_LEVELS$RATIO,
    STUDY_SEGMENT = c("A", "B"),
    VARIABLE_ROLE = VARIABLE_ROLES$PRIMARY,
    MISSING_LIST = "",
    JUMP_LIST = "",
    stringsAsFactors = FALSE
  )
  segment_level <- data.frame(
    STUDY_SEGMENT = c("A", "B"),
    stringsAsFactors = FALSE
  )

  suppressWarnings(suppressMessages(dq_report2(
    study_data = study_data,
    item_level = item_level,
    segment_level = segment_level,
    dimensions = "Completeness",
    filter_indicator_functions = "^com_segment_missingness$",
    cores = NULL
  )))
}

.get_local_repeated_group_only_report <- function() {
  skip_on_cran()

  prep_purge_data_frame_cache()
  withr::defer(prep_purge_data_frame_cache())

  study_data <- data.frame(
    c1 = c(1, 2, 3, 4),
    c2 = c(1, 3, 2, 5)
  )
  item_level <- prep_study2meta(study_data)
  item_level[[JUMP_LIST]] <- SPLIT_CHAR
  cross_item_level <- data.frame(
    VARIABLE_LIST = "c1 | c2",
    CHECK_ID = "repeated",
    CHECK_LABEL = "Repeated-measurement group",
    REPEATED_MEASURES_METRIC = "rmse",
    REPEATED_MEASURES_REFERENCE = "c1",
    stringsAsFactors = FALSE
  )

  suppressWarnings(suppressMessages(dq_report2(
    study_data = study_data,
    item_level = item_level,
    cross_item_level = cross_item_level,
    dimensions = "Accuracy",
    filter_indicator_functions = "^acc_repeated_measurements$",
    cores = NULL
  )))
}

.page_html <- function(pages) {
  htmltools::renderTags(htmltools::tagList(pages))$html
}

.get_local_com_report <- function() {
  skip_if_not_installed("stringdist")

  prep_purge_data_frame_cache()
  withr::defer(prep_purge_data_frame_cache())

  study_data <- data.frame(x = c(1, NA, 3))
  meta_data <- data.frame(
    VAR_NAMES = "x",
    LABEL = "x",
    DATA_TYPE = DATA_TYPES$INTEGER,
    SCALE_LEVEL = SCALE_LEVELS$RATIO,
    MISSING_LIST = SPLIT_CHAR,
    JUMP_LIST = SPLIT_CHAR,
    stringsAsFactors = FALSE
  )

  suppressWarnings(suppressMessages(dq_report2(
    study_data = study_data,
    item_level = meta_data,
    dimensions = "Completeness",
    filter_indicator_functions = "^com_item_missingness$",
    filter_result_slots = "^SummaryTable$",
    cores = NULL
  )))
}

.get_local_ssi_report <- function(filter_indicator_functions = character(0)) {
  skip_on_cran() # report rendering is slow

  prep_purge_data_frame_cache()
  withr::defer(prep_purge_data_frame_cache())

  study_data <- data.frame(
    x1 = c(1, 2, 3, 4),
    x2 = c(1, NA, 3, 4),
    x3 = c(2, 2, 2, 2),
    x4 = c(3, 3, 3, 3)
  )
  meta_data <- data.frame(
    VAR_NAMES = names(study_data),
    LABEL = names(study_data),
    DATA_TYPE = DATA_TYPES$INTEGER,
    SCALE_LEVEL = SCALE_LEVELS$RATIO,
    VALUE_LABELS = "",
    JUMP_LIST = SPLIT_CHAR,
    MISSING_LIST = SPLIT_CHAR,
    LONG_LABEL = names(study_data),
    stringsAsFactors = FALSE
  )
  cross_item_level <- data.frame(
    VARIABLE_LIST = "x1 | x2 | x3 | x4",
    CHECK_LABEL = "ssi_test",
    MISS_RESP = "[;2)",
    stringsAsFactors = FALSE
  )

  suppressWarnings(suppressMessages(dq_report2(
    study_data = study_data,
    item_level = meta_data,
    cross_item_level = cross_item_level,
    dimensions = "Int",
    filter_indicator_functions = filter_indicator_functions,
    cores = NULL
  )))
}

.get_local_variable_group_report <- function() {
  env <- testthat::test_env()
  if (!is.null(env$.local_variable_group_report)) {
    return(env$.local_variable_group_report)
  }

  skip_on_cran() # report computation and page rendering are intentionally broad
  skip_if_not_installed("stringdist")

  prep_purge_data_frame_cache()
  withr::defer(prep_purge_data_frame_cache(), envir = env)

  study_data <- data.frame(
    a1 = c(1, 2, 3, 4, 5, 1, 2, 3),
    a2 = c(1, NA, 3, 4, 5, 1, NA, 3),
    b1 = seq_len(8),
    b2 = c(2, 1, 4, 3, 7, 5, 8, 6),
    c1 = c(1, 2, 3, 4, 1, 2, 3, 4),
    c2 = c(2, 4, 6, 7, 2, 5, 3, 8)
  )
  item_level <- data.frame(
    variable = names(study_data),
    label = paste("Questionnaire item", names(study_data)),
    data_type = DATA_TYPES$INTEGER,
    scale_level = SCALE_LEVELS$RATIO,
    value_labels = "",
    jump_list = SPLIT_CHAR,
    missing_list = SPLIT_CHAR,
    long_label = paste("Questionnaire item", names(study_data)),
    stringsAsFactors = FALSE
  )
  colnames(item_level) <- c(
    VAR_NAMES, LABEL, DATA_TYPE, SCALE_LEVEL, VALUE_LABELS,
    JUMP_LIST, MISSING_LIST, LONG_LABEL
  )

  cross_item_level <- data.frame(
    variable_list = c("a1 | a2", "b1 | b2", "c1 | c2"),
    check_label = c(
      "Contradiction group",
      "Long-string group",
      "Repeated-measurement group"
    ),
    miss_resp = c("[;2)", "[;2)", ""),
    maximum_long_string = c("", "[;3)", ""),
    mahalanobis_threshold = c("", "TRUE", ""),
    repeated_measures_metric = c("", "", "rmse"),
    repeated_measures_reference = c("", "", "c1"),
    contradiction_term = c("[a1] < 0", "", ""),
    contradiction_type = c("LOGICAL", "", ""),
    stringsAsFactors = FALSE
  )
  colnames(cross_item_level) <- c(
    VARIABLE_LIST, CHECK_LABEL, MISS_RESP, MAXIMUM_LONG_STRING,
    MAHALANOBIS_THRESHOLD, "REPEATED_MEASURES_METRIC",
    "REPEATED_MEASURES_REFERENCE", CONTRADICTION_TERM,
    CONTRADICTION_TYPE
  )

  env$.local_variable_group_report <- suppressWarnings(suppressMessages(
    dq_report2(
      study_data = study_data,
      item_level = item_level,
      cross_item_level = cross_item_level,
      dimensions = c("Integrity", "Completeness", "Consistency", "Accuracy"),
      filter_indicator_functions = paste0(
        "^(",
        paste(c(
          "com_item_missingness",
          "con_ssi_range_check",
          "con_contradictions_redcap",
          "acc_mahalanobis_ratio",
          "acc_repeated_measurements"
        ), collapse = "|"),
        ")$"
      ),
      cores = NULL
    )
  ))
  env$.local_variable_group_report
}

.get_local_attention_check_report <- function() {
  skip_on_cran()
  skip_if_not_installed("stringdist")

  prep_purge_data_frame_cache()
  withr::defer(prep_purge_data_frame_cache())

  study_data <- data.frame(
    scaleA1 = c(1, 2, 1, NA),
    scaleA2 = c(2, 2, 2, 1),
    scaleA3 = c(1, 1, 2, 1)
  )
  item_level <- prep_study2meta(study_data)
  item_level[[ITEM_TYPE]] <- c(
    "BOGUS: 1|2",
    "INSTRUCTED: 2",
    "BOGUS: 1"
  )
  item_level[[JUMP_LIST]] <- SPLIT_CHAR

  cross_item_level <- data.frame(
    VARIABLE_LIST = "scaleA1 | scaleA2 | scaleA3",
    CHECK_LABEL = "Group3",
    CHECK_ID = "Group3",
    SUM_ATTENTION_CHECK_ITEMS = "[0;1]",
    stringsAsFactors = FALSE
  )

  suppressWarnings(suppressMessages(dq_report2(
    study_data = study_data,
    item_level = item_level,
    cross_item_level = cross_item_level,
    dimensions = "Consistency",
    filter_indicator_functions = "^con_attention_check_items$",
    cores = NULL
  )))
}

.get_local_contradiction_rule_report <- function(n_rules = 40L) {
  skip_on_cran()
  skip_if_not_installed("stringdist")

  study_data <- data.frame(
    x = c(0, 2, 1, 3),
    y = c(1, 1, 1, 1)
  )
  item_level <- data.frame(
    VAR_NAMES = names(study_data),
    LABEL = names(study_data),
    DATA_TYPE = DATA_TYPES$INTEGER,
    SCALE_LEVEL = SCALE_LEVELS$RATIO,
    VALUE_LABELS = "",
    JUMP_LIST = SPLIT_CHAR,
    MISSING_LIST = SPLIT_CHAR,
    stringsAsFactors = FALSE
  )
  cross_item_level <- data.frame(
    CHECK_ID = sprintf("rule_%02d", seq_len(n_rules)),
    CHECK_LABEL = sprintf("Contradiction rule %02d", seq_len(n_rules)),
    VARIABLE_LIST = rep("x | y", n_rules),
    CONTRADICTION_TERM = rep("[x] > [y]", n_rules),
    CONTRADICTION_TYPE = rep("LOGICAL", n_rules),
    DATA_PREPARATION = rep(LABEL, n_rules),
    stringsAsFactors = FALSE
  )

  suppressWarnings(suppressMessages(dq_report2(
    study_data = study_data,
    item_level = item_level,
    cross_item_level = cross_item_level,
    dimensions = "Consistency",
    filter_indicator_functions = "^con_contradictions_redcap$",
    cores = NULL
  )))
}

.get_local_mixed_shared_group_report <- function() {
  skip_on_cran()

  set.seed(2)
  study_data <- data.frame(
    a = rnorm(25),
    b = rnorm(25),
    c = rnorm(25)
  )
  item_level <- prep_study2meta(study_data)
  item_level[[JUMP_LIST]] <- SPLIT_CHAR
  cross_item_level <- data.frame(
    "a | b | c",
    "Group X",
    "gx",
    "[a] > [b]",
    "LOGICAL",
    "TRUE",
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  colnames(cross_item_level) <- c(
    VARIABLE_LIST,
    CHECK_LABEL,
    CHECK_ID,
    CONTRADICTION_TERM,
    CONTRADICTION_TYPE,
    MULTIVARIATE_OUTLIER_CHECK
  )

  suppressWarnings(suppressMessages(dq_report2(
    study_data = study_data,
    item_level = item_level,
    cross_item_level = cross_item_level,
    dimensions = c("Consistency", "Accuracy"),
    filter_indicator_functions =
      "^(con_contradictions_redcap|acc_multivariate_outlier)$",
    cores = NULL
  )))
}

test_that("util_generate_pages_from_report(): renders pages for a mini report", { # nolint: line_length_linter.
  skip_if_not_installed("stringdist")
  report <- .get_mini_report()

  pages <- .call_pages(report)
  expect_true(is.list(pages))
  expect_true(length(pages) >= 1)

  html <- .page_html(pages)
  expect_true(nchar(html) > 0)
  expect_match(html, "Marino2022", fixed = TRUE)
  expect_match(html, "BibTeX copied to clipboard.", fixed = TRUE)
})

test_that("overview float menu uses in-page anchors", {
  overview <- readLines(system.file(
    "templates", "default", "overview.html",
    package = "dataquieR"
  ))

  expect_true(any(grepl('"#comat"', overview, fixed = TRUE)))
  expect_false(any(grepl('"report.html#comat"', overview, fixed = TRUE)))
})

test_that("study-data counts separate SSI computed variables", {
  meta_data <- data.frame(
    VAR_NAMES = c("original", "derived", "metadata_only"),
    COMPUTED_VARIABLE_ROLE = c(NA, "MISS_RESP", "MAXIMUM_LONG_STRING")
  )

  variables <- util_generate_pages_partition_study_variables(
    c("original", "without_metadata", "derived"),
    meta_data
  )

  expect_identical(
    variables,
    list(
      original = c("original", "without_metadata"),
      computed = "derived"
    )
  )
})

test_that("metadata marks generated variables for client-side filtering", {
  skip_if_not_installed("DT")

  meta_data <- data.frame(
    VAR_NAMES = c("item", "user_computed", "generated_computed"),
    STUDY_SEGMENT = c("study", "study", ".COMPUTED__ssi"),
    stringsAsFactors = FALSE
  )

  internal <- util_generate_pages_internal_computed_var_names(meta_data)
  table <- util_html_table(
    data.frame(VAR_NAMES = meta_data[[VAR_NAMES]]),
    fixed_header = TRUE
  )
  toggle <- util_generate_pages_internal_computed_toggle(
    table,
    internal
  )
  toggle_html <- paste(as.character(toggle), collapse = "")

  expect_identical(internal, "generated_computed")
  expect_match(
    toggle_html,
    "generated_computed",
    fixed = TRUE
  )
  expect_match(
    toggle_html,
    "dataquieRInternalComputedToggleAction",
    fixed = TRUE
  )
  expect_match(
    toggle_html,
    "dataquieRInternalComputedToggleInit",
    fixed = TRUE
  )
  expect_match(toggle_html, '"rootId":"dq-internal-computed-metadata"',
    fixed = TRUE
  )
  expect_false(grepl("<style", toggle_html, fixed = TRUE))
  expect_false(grepl("MutationObserver", toggle_html, fixed = TRUE))
  expect_match(
    toggle_html,
    "Show automatically generated variables",
    fixed = TRUE
  )
  expect_match(
    toggle_html,
    'id="dq-internal-computed-metadata"',
    fixed = TRUE
  )
  expect_match(
    toggle_html,
    "dq-internal-computed-toggle-control",
    fixed = TRUE
  )
})

test_that("util_generate_pages_from_report(): reuses a precomputed summary", {
  skip_if_not_installed("stringdist")
  report <- .get_mini_report()
  repsum <- summary(report)

  pages <- .call_pages(report, repsum = repsum)
  expect_true(is.list(pages))
  expect_true(length(pages) >= 1)
})

test_that("unused statistical settings are omitted", {
  skip_if_not_installed("DT")
  skip_if_not_installed("markdown")

  prep_purge_data_frame_cache()
  withr::defer(prep_purge_data_frame_cache())

  prep_add_data_frames(
    statistical_settings = data.frame(
      SETTING_ID = "rm_default",
      METHOD = "rmse",
      stringsAsFactors = FALSE
    )
  )

  study_data <- data.frame(x = c(1, 2, 3))
  meta_data <- data.frame(
    VAR_NAMES = "x",
    LABEL = "x",
    DATA_TYPE = DATA_TYPES$INTEGER,
    SCALE_LEVEL = SCALE_LEVELS$RATIO,
    MISSING_LIST = SPLIT_CHAR,
    JUMP_LIST = SPLIT_CHAR,
    stringsAsFactors = FALSE
  )

  report <- suppressWarnings(suppressMessages(dq_report2(
    study_data = study_data,
    item_level = meta_data,
    dimensions = "Completeness",
    filter_indicator_functions = "^com_item_missingness$",
    filter_result_slots = "^SummaryTable$",
    cores = NULL
  )))

  pages <- .call_pages(report)
  html <- .page_html(pages)
  expect_false("statisticalsettings.html" %in% names(pages))
  expect_false(grepl("rm_default", html, fixed = TRUE))
})

test_that("statistical settings survive cache purge and non-disclosure", {
  skip_on_cran()
  skip_if_not_installed("stringdist")

  prep_purge_data_frame_cache()
  withr::defer(prep_purge_data_frame_cache())
  prep_add_data_frames(
    statistical_settings = data.frame(
      SETTING_ID = "custom_rmse",
      METRIC = "rmse",
      MIN_N = 2L,
      stringsAsFactors = FALSE
    )
  )

  study_data <- data.frame(
    c1 = c(1, 2, 3, 4),
    c2 = c(1, 3, 2, 5)
  )
  item_level <- prep_study2meta(study_data)
  item_level[[JUMP_LIST]] <- SPLIT_CHAR
  cross_item_level <- data.frame(
    VARIABLE_LIST = "c1 | c2",
    CHECK_ID = "repeated",
    CHECK_LABEL = "Repeated-measurement group",
    REPEATED_MEASURES_METRIC = "rmse",
    REPEATED_MEASURES_METRIC_SETTING = "custom_rmse",
    stringsAsFactors = FALSE
  )

  report <- suppressWarnings(suppressMessages(dq_report2(
    study_data = study_data,
    item_level = item_level,
    cross_item_level = cross_item_level,
    dimensions = "Accuracy",
    filter_indicator_functions = "^acc_repeated_measurements$",
    cores = NULL
  )))
  referred_tables <- util_attr(report, "referred_tables", exact = TRUE)
  expect_true("statistical_settings" %in% names(referred_tables))
  expect_true("custom_rmse" %in%
      referred_tables$statistical_settings$setting_id)

  prep_purge_data_frame_cache()
  pages <- .call_pages(report)
  expect_true("statisticalsettings.html" %in% names(pages))
  expect_match(
    .page_html(pages["dim_acc_acc_repeated_measurements.html"]),
    paste0(
      "statisticalsettings.html?dq_filter_col=SETTING_ID&amp;",
      "dq_filter_value=custom_rmse"
    ),
    fixed = TRUE
  )
  expect_match(
    .page_html(pages["statisticalsettings.html"]),
    "custom_rmse",
    fixed = TRUE
  )

  undisclosed <- util_undisclose(report)
  undisclosed_pages <- .call_pages(undisclosed)
  expect_false("statisticalsettings.html" %in% names(undisclosed_pages))
})

test_that("util_generate_pages_from_report(): local report metadata variants", {
  report <- .get_local_com_report()
  report2 <- report
  attr(report2, "label_modification_text") <- "One label was normalized."
  attr(report2, "label_modification_table") <- data.frame(
    Original = "old label",
    Modified = "new label",
    stringsAsFactors = FALSE
  )
  attr(report2, "properties") <- NULL
  attr(report2, "dt_adjust") <- FALSE
  issue <- rlang::warning_cnd(message = "metadata issue before the pipeline")
  attr(issue, "varname") <- character(0)
  attr(issue, "integrity_indicator") <- "unknown_indicator"
  attr(report2, "integrity_issues_before_pipeline") <- list(issue, issue)

  pages <- .call_pages(report2)
  html <- .page_html(pages)

  expect_match(html, "Label modifications", fixed = TRUE)
  expect_match(html, "old label", fixed = TRUE)
  expect_match(html, "Report information", fixed = TRUE)
  expect_match(
    html,
    "computed with the dt_adjust-option set to FALSE",
    fixed = TRUE
  )
  expect_match(html, "Integrity Issues", fixed = TRUE)
  expect_equal(
    lengths(regmatches(
      html,
      gregexpr("metadata issue before the pipeline", html, fixed = TRUE)
    )),
    1L
  )
})

test_that(
  "util_generate_pages_from_report(): reports study variables without metadata",
  {
    report <- .get_local_com_report()
    attr(report, "study_data_dimnames") <- list(
      as.character(seq_len(3)),
      c("x", "only_in_study_data")
    )

    pages <- .call_pages(report)
    html <- .page_html(pages)

    expect_match(html, "Variables without metadata", fixed = TRUE)
    expect_match(html, "only_in_study_data", fixed = TRUE)
  }
)

test_that("util_setup_dashboard(): keeps intermediate columns available", {
  skip_if_not_installed("stringdist")
  report <- .get_mini_report()
  repsum <- summary(report)

  table <- dataquieR:::util_setup_dashboard( # nolint
    report,
    return_table_only = TRUE,
    repsum = repsum
  )

  expect_s3_class(table, "data.frame")
  cols <- dataquieR:::util_untranslated_colnames(table) # nolint
  expect_true(all(c(
    "values_raw",
    "call_names",
    "function_name",
    "indicator_metric"
  ) %in% cols))
})

test_that(
  paste0(
    "util_generate_pages_from_report(): label ",
    "modifications section (if present) is rendered"
  ),
  {
    skip_if_not_installed("stringdist")
    report <- .get_mini_report()

    pages <- .call_pages(report)
    html <- .page_html(pages)

    txt <- util_attr(report, "label_modification_text", exact = TRUE)
    if (!is.null(txt) && length(txt) == 1L && nchar(txt) > 0) {
      expect_match(html, "Label modifications", fixed = TRUE)
    } else {
      expect_true(TRUE) # branch executed, but nothing to assert
    }
  }
)

test_that(
  "util_generate_pages_from_report(): renders label modification details",
  {
    skip_if_not_installed("stringdist")
    report <- .get_mini_report()
    report2 <- report
    attr(report2, "label_modification_text") <- "One label was normalized."
    attr(report2, "label_modification_table") <- data.frame(
      Original = "old label",
      Modified = "new label",
      stringsAsFactors = FALSE
    )

    pages <- .call_pages(report2)
    html <- .page_html(pages)

    expect_match(html, "Label modifications", fixed = TRUE)
    expect_match(html, "old label", fixed = TRUE)
    expect_match(html, "new label", fixed = TRUE)
  }
)

test_that("util_generate_pages_from_report(): integrity issues before pipeline (if present) are rendered", { # nolint: line_length_linter.
  skip_if_not_installed("stringdist")
  report <- .get_mini_report()

  pages <- .call_pages(report)
  html <- .page_html(pages)

  issues <- util_attr(report, "integrity_issues_before_pipeline", exact = TRUE)
  if (!is.null(issues) && length(issues) > 0) {
    expect_match(html, "Integrity", fixed = FALSE)
  } else {
    expect_true(TRUE)
  }
})

test_that(
  "util_generate_pages_from_report(): normalizes integrity conditions",
  {
    skip_if_not_installed("stringdist")
    report <- .get_mini_report()
    report2 <- report
    issue <- rlang::warning_cnd(message = "metadata issue before the pipeline")
    attr(issue, "varname") <- character(0)
    attr(issue, "integrity_indicator") <- "unknown_indicator"
    attr(report2, "integrity_issues_before_pipeline") <- list(issue, issue)

    pages <- .call_pages(report2)
    html <- .page_html(pages)

    expect_match(html, "Integrity Issues", fixed = TRUE)
    expect_match(html, "metadata issue before the pipeline", fixed = TRUE)
    expect_equal(
      lengths(regmatches(
        html,
        gregexpr("metadata issue before the pipeline", html, fixed = TRUE)
      )),
      1L
    )
  }
)

test_that("util_generate_pages_from_report(): falls back when properties are missing", { # nolint: line_length_linter.
  skip_if_not_installed("stringdist")
  report <- .get_mini_report()

  # modify a copy to avoid polluting the cached object
  report2 <- report
  attr(report2, "properties") <- NULL

  pages <- .call_pages(report2)
  html <- .page_html(pages)

  expect_match(html, "Report information", fixed = TRUE)
  expect_match(html, "Data quality report", fixed = TRUE)
})

test_that("util_generate_pages_from_report(): aligns general results", { # nolint: line_length_linter.
  report <- .get_local_ssi_report()

  pages <- .call_pages(report)
  html <- .page_html(pages)

  expect_match(
    html,
    "Item-level assessment scope</span>",
    fixed = TRUE
  )
  expect_match(html, "Study data summary", fixed = TRUE)
  expect_match(html, "Metadata summary", fixed = TRUE)
  expect_match(
    html,
    "Study variables with item-level metadata",
    fixed = TRUE
  )
  expect_match(
    html,
    "Study variables (item-level metadata)",
    fixed = TRUE
  )
  expect_match(html, "Computed variable-group results", fixed = TRUE)
  expect_false(grepl("Scales metrics computed", html, fixed = TRUE))
  expect_match(html, "Assessment scope", fixed = TRUE)
  expect_match(html, "Assessment scope follows the DQ_OBS concept",
    fixed = TRUE
  )
  expect_false(grepl("Goldammer et al. (2024)", html, fixed = TRUE))
  expect_match(
    html,
    "Variable-group assessment scope</span>",
    fixed = TRUE
  )
  expect_match(html, "Missing", fixed = TRUE)
  expect_match(html, "Data Quality Summary", fixed = TRUE)
  expect_match(
    html,
    "Data Quality Summary for variable groups",
    fixed = TRUE
  )
  meta_data_html <- .page_html(pages["meta_data.html"])
  expect_match(
    meta_data_html,
    "Show automatically generated variables",
    fixed = TRUE
  )
  expect_match(
    meta_data_html,
    "dq-internal-computed-metadata",
    fixed = TRUE
  )
  expect_equal(
    lengths(regmatches(
      html,
      gregexpr("class=\"dq-general-result-grid\"", html, fixed = TRUE)
    )),
    2L
  )
  expect_match(
    html,
    "grid-template-columns:repeat(auto-fit,minmax(min(100%,28em),1fr));",
    fixed = TRUE
  )
})

test_that("segment-only reports omit empty item-level overview pages", {
  skip_if_not_installed("plotly")

  report <- .get_local_segment_only_report()
  dashboard <- htmltools::tagList(htmltools::div("Unused item dashboard"))
  pages <- .call_pages(
    report,
    disable_plotly = FALSE,
    my_dashboard = dashboard
  )
  report_page_names <- names(pages[["report.html"]])

  expect_false("Item-level data quality summary" %in% report_page_names)
  expect_false("sunburst.html" %in% names(pages))
  expect_false("dashboard.html" %in% names(pages))
  expect_true("dim_com.html" %in% names(pages))
  expect_match(
    .page_html(pages["report.html"]),
    "The selected checks target segments.",
    fixed = TRUE
  )
  expect_match(
    .page_html(pages["report.html"]),
    "The absence of an overview is not a data-quality rating.",
    fixed = TRUE
  )
})

test_that("unclassified group-only reports explain omitted charts", {
  skip_if_not_installed("plotly")

  report <- .get_local_repeated_group_only_report()
  pages <- .call_pages(report, disable_plotly = FALSE)
  report_page_names <- names(pages[["report.html"]])
  group_summary_html <- .page_html(pages["variable_group_summary.html"])

  expect_false("Item-level data quality summary" %in% report_page_names)
  expect_false("sunburst.html" %in% names(pages))
  expect_true("variable_group_summary.html" %in% names(pages))
  expect_true("variable_group_dashboard.html" %in% names(pages))
  expect_false("variable_group_chart.html" %in% names(pages))
  expect_match(
    group_summary_html,
    paste(
      "Variable-group results were calculated, but no grading rule",
      "produced a classification."
    ),
    fixed = TRUE
  )
  expect_match(
    .page_html(pages["report.html"]),
    "Variable-group assessment scope",
    fixed = TRUE
  )
})

test_that("variable-group report pages expose groups and metric overviews", {
  skip_if_not_installed("plotly")

  report <- .get_local_variable_group_report()
  repsum <- summary(report)
  repsum_this <- util_attr(repsum, "this", exact = TRUE)
  # Exercise the direct-result fallback as well as the generic result table.
  direct_group_rows <-
    !util_empty(repsum_this$result[[CHECK_ID]]) &
    repsum_this$result[["function_name"]] == "acc_mahalanobis_ratio"
  repsum_this$result[["function_name"]][direct_group_rows] <-
    "con_ssi_range_check"
  attr(repsum, "this") <- repsum_this
  pages <- .call_pages(report, repsum = repsum, disable_plotly = FALSE)
  pages_without_plotly <- .call_pages(
    report,
    repsum = repsum,
    disable_plotly = TRUE
  )
  html <- .page_html(pages)

  expect_true(length(pages) > 5L)
  expect_false(
    "variable_group_chart.html" %in% names(pages_without_plotly)
  )
  expect_match(html, "Variable-group data quality summary", fixed = TRUE)
  expect_match(html, "Contradiction checks", fixed = TRUE)
  expect_match(
    html,
    "not the share of observations that violated a rule",
    fixed = TRUE
  )
  expect_match(html, "Variable-group data quality chart", fixed = TRUE)
  expect_match(html, "Variable-group data quality dashboard", fixed = TRUE)
  expect_match(html, "Contradiction group", fixed = TRUE)
  expect_match(html, "Long-string group", fixed = TRUE)
  expect_match(html, "Repeated-measurement group", fixed = TRUE)
  expect_match(html, "Mahalanobis", fixed = TRUE)
  expect_match(html, "Maximum Long String", fixed = TRUE)
  expect_match(html, "Repeated measurements", fixed = TRUE)

  page_names <- names(pages)
  page_dropdowns <- unlist(lapply(pages, function(file_pages) {
    vapply(file_pages, function(page) {
      util_attr(page, "dropdown", exact = TRUE)
    }, character(1))
  }), use.names = FALSE)
  expect_true(VARIABLE_GROUP_REPORT_MENU %in% page_dropdowns)
  expect_false(any(page_dropdowns %in% c("Scales", "Variable groups")))
  menu_html <- as.character(.menu_env$menu(pages))
  expect_equal(
    lengths(regmatches(
      menu_html,
      gregexpr('id="Variablegroupsscales"', menu_html, fixed = TRUE)
    )),
    1L
  )
  expect_true(any(grepl("variable_group_summary", page_names, fixed = TRUE)))
  expect_true(any(grepl("variable_group_chart", page_names, fixed = TRUE)))
  expect_true(any(grepl("variable_group_dashboard", page_names, fixed = TRUE)))
  expect_true(any(grepl("long.?string", page_names, ignore.case = TRUE)))
  expect_match(
    .page_html(pages["variable_group_dashboard.html"]),
    "showDataquieRResult",
    fixed = TRUE
  )
  group_overviews <- .page_html(pages[c(
    "variable_group_summary.html",
    "variable_group_chart.html",
    "variable_group_dashboard.html"
  )])
  expect_match(
    group_overviews,
    paste0(
      "This chart summarizes evaluated variable-group results other\\s+",
      "than"
    )
  )
  expect_match(
    group_overviews,
    "Each slice represents one evaluated contradiction check",
    fixed = TRUE
  )
  group_chart_html <- .page_html(pages["variable_group_chart.html"])
  expect_match(group_chart_html, "Other group checks", fixed = TRUE)
  expect_match(group_chart_html, "Contradiction checks", fixed = TRUE)
  expect_match(group_chart_html, "All group-level results", fixed = TRUE)
  expect_match(group_chart_html, "dq-sunburst-mode-switch", fixed = TRUE)
  expect_equal(
    lengths(regmatches(
      group_chart_html,
      gregexpr(
        "data-dq-sunburst-mode-button",
        group_chart_html,
        fixed = TRUE
      )
    )),
    3L
  )
  expect_equal(
    lengths(regmatches(
      group_chart_html,
      gregexpr(
        "data-dq-sunburst-mode-panel",
        group_chart_html,
        fixed = TRUE
      )
    )),
    3L
  )
  expect_match(
    group_chart_html,
    paste0(
      "[0-9]+ contradiction checks? across [0-9]+ variable groups? ",
      "available"
    )
  )
  expect_match(group_chart_html, 'aria-selected="true"', fixed = TRUE)
  expect_match(group_chart_html, 'aria-live="polite"', fixed = TRUE)
  expect_match(
    group_chart_html,
    paste0(
      "Sector area, label size, and color emphasize more critical checks.\\s+",
      "The count in the selector and notice is the number of evaluated"
    )
  )
  expect_equal(
    lengths(regmatches(
      .page_html(pages["report.html"]),
      gregexpr(
        "variable_group_summary.html",
        .page_html(pages["report.html"]),
        fixed = TRUE
      )
    )),
    1L
  )

  variable_group_rows <- util_filter_repsum(
    repsum_this$result,
    vars_to_include = "variable_group",
    meta_data = repsum_this$meta_data,
    rownames_of_report = repsum_this$rownames_of_report,
    label_col = repsum_this$label_col,
    variable_group_call_names = repsum_this$variable_group_call_names
  )
  is_variable_group_row <- rownames(repsum_this$result) %in%
    rownames(variable_group_rows)
  is_contradiction_row <- util_summary_metrics_in_concept(
    repsum_this$result[["indicator_metric"]],
    "con_con"
  )
  contradiction_only <- repsum
  contradiction_only_this <- rlang::env_clone(repsum_this)
  contradiction_only_this$result <- repsum_this$result[
    !is_variable_group_row | is_contradiction_row, , drop = FALSE
  ]
  attr(contradiction_only, "this") <- contradiction_only_this
  contradiction_only_pages <- suppressWarnings(
    .call_pages(
      report,
      repsum = contradiction_only,
      disable_plotly = FALSE
    )
  )
  contradiction_only_overview <- .page_html(
    contradiction_only_pages["report.html"]
  )
  expect_match(
    contradiction_only_overview,
    "Contradiction checks",
    fixed = TRUE
  )
  expect_false(grepl(
    "Data Quality Summary for variable groups",
    contradiction_only_overview,
    fixed = TRUE
  ))
  contradiction_only_chart <- .page_html(
    contradiction_only_pages["variable_group_chart.html"]
  )
  expect_match(
    contradiction_only_chart,
    "Contradiction checks",
    fixed = TRUE
  )
  expect_false(grepl(
    "Other group checks",
    contradiction_only_chart,
    fixed = TRUE
  ))
  expect_false(grepl(
    "data-dq-sunburst-mode-button",
    contradiction_only_chart,
    fixed = TRUE
  ))

  other_groups_only <- repsum
  other_groups_only_this <- rlang::env_clone(repsum_this)
  other_groups_only_this$result <- repsum_this$result[
    !is_variable_group_row | !is_contradiction_row, , drop = FALSE
  ]
  attr(other_groups_only, "this") <- other_groups_only_this
  other_groups_only_pages <- .call_pages(
    report,
    repsum = other_groups_only,
    disable_plotly = FALSE
  )
  other_groups_only_overview <- .page_html(
    other_groups_only_pages["report.html"]
  )
  expect_match(
    other_groups_only_overview,
    "Data Quality Summary for variable groups",
    fixed = TRUE
  )
  expect_false(grepl(
    "Contradiction checks",
    other_groups_only_overview,
    fixed = TRUE
  ))
  other_groups_only_chart <- .page_html(
    other_groups_only_pages["variable_group_chart.html"]
  )
  expect_match(
    other_groups_only_chart,
    "Other group checks",
    fixed = TRUE
  )
  expect_false(grepl(
    'data-dq-sunburst-mode-panel="contradiction"',
    other_groups_only_chart,
    fixed = TRUE
  ))
  expect_false(grepl(
    "data-dq-sunburst-mode-button",
    other_groups_only_chart,
    fixed = TRUE
  ))
  expect_false(grepl("GROUPCALL_", group_overviews, fixed = TRUE))
  expect_match(
    group_overviews,
    "Repeated-measurementgroup.html",
    fixed = TRUE
  )

  contradiction_file <- paste0(
    "GROUPCALL_",
    prep_link_escape("Contradictions"),
    ".html"
  )
  repeated_file <- paste0(
    "GROUPCALL_",
    prep_link_escape("Repeated measurements"),
    ".html"
  )
  expect_false(contradiction_file %in% page_names)
  expect_false(repeated_file %in% page_names)
  expect_true("dim_acc_acc_repeated_measurements.html" %in% page_names)

  group_pages <- pages[grepl(
    paste(c(
      "Contradictiongroup",
      "Long-stringgroup",
      "Repeated-measurementgroup"
    ), collapse = "|"),
    page_names
  )]
  expect_gt(length(group_pages), 0L)
  group_html <- .page_html(group_pages)
  expect_match(group_html, ">Summary</h5>", fixed = TRUE)
  expect_match(group_html, "Variable-group metadata", fixed = TRUE)
  expect_match(group_html, VARIABLE_LIST, fixed = TRUE)
  expect_match(group_html, 'data-nm="variable_group.', fixed = TRUE)
  expect_match(group_html, 'data-popup-nm="variable_group.', fixed = TRUE)
  expect_match(group_html, "Mahalanobis Distance", fixed = TRUE)
  expect_match(
    group_html,
    'data-nm="con_ssi_range_check.',
    fixed = TRUE
  )
  expect_match(
    group_html,
    'data-nm="acc_mahalanobis_ratio.',
    fixed = TRUE
  )
  contradiction_html <- .page_html(pages["Contradictiongroup.html"])
  expect_match(
    contradiction_html,
    "Invalid or missing responses",
    fixed = TRUE
  )
  expect_false(grepl(
    "Logical contradictions",
    contradiction_html,
    fixed = TRUE
  ))

  figure_tags <- unlist(
    lapply(pages, util_collect_tag_attrs),
    recursive = FALSE
  )
  figure_files <- vapply(figure_tags, function(figure) {
    attr(figure, "html_file", exact = TRUE)
  }, character(1))
  expect_gt(length(figure_files), 1L)
  expect_identical(anyDuplicated(figure_files), 0L)
  expect_true(any(grepl(
    "Missingresponses[.]Contradictiongroup[.]html$",
    figure_files
  )))
  expect_true(any(grepl(
    "Missingresponses[.]Long-stringgroup[.]html$",
    figure_files
  )))
})

test_that("attention-check results have no duplicate call overview", {
  report <- .get_local_attention_check_report()
  pages <- .call_pages(report)
  page_names <- names(pages)
  metric_file <- "SSIGROUP_SUM_ATTENTION_CHECK_ITEMS.html"
  group_file <- "Group3.html"

  expect_true(metric_file %in% page_names)
  expect_true(group_file %in% page_names)
  expect_false(any(startsWith(page_names, "GROUPCALL_")))

  menu_html <- as.character(.menu_env$menu(pages))
  expect_false(grepl(
    "Attention check items (variable groups)",
    menu_html,
    fixed = TRUE
  ))
  expect_equal(
    lengths(regmatches(
      menu_html,
      gregexpr(metric_file, menu_html, fixed = TRUE)
    )),
    1L
  )

  metric_html <- .page_html(pages[metric_file])
  expect_match(metric_html, "Sum of attention check items", fixed = TRUE)
  expect_match(metric_html, "Group3", fixed = TRUE)
  expect_match(metric_html, "Cases with incorrect responses", fixed = TRUE)
})

test_that("variable-group pages ignore summary rows without CHECK_ID", {
  report <- .get_local_variable_group_report()
  repsum <- summary(report)
  this <- util_attr(repsum, "this", exact = TRUE)
  group_rows <- which(!util_empty(this$result[[CHECK_ID]]))
  expect_gt(length(group_rows), 0L)
  this$result[[CHECK_ID]][group_rows[[1]]] <- NA_character_
  attr(repsum, "this") <- this

  expect_no_error(.call_pages(report, repsum = repsum))
})

test_that("many contradiction-only rules share one report page", {
  report <- .get_local_contradiction_rule_report(40L)
  repsum <- summary(report)
  pages <- .call_pages(report, repsum = repsum)
  page_names <- names(pages)

  contradiction_result <- report[[grep(
    "^con_contradictions_redcap",
    names(report)
  )[[1]]]]
  expect_equal(nrow(contradiction_result$VariableGroupTable), 40L)
  expect_equal(nrow(contradiction_result$SummaryPlot$data), 40L)
  expect_true("dim_con.html" %in% page_names)
  expect_true("Contradictions" %in% names(pages[["dim_con.html"]]))
  expect_false(any(startsWith(page_names, "GROUPCALL_Contradictions")))
  expect_false(any(grepl("Contradictionrule", page_names, fixed = TRUE)))

  cross_item <- util_attr(repsum, "this", exact = TRUE)$meta_data_cross_item
  expect_length(
    util_attr(cross_item, "contradiction_only_check_ids", exact = TRUE),
    40L
  )
  expect_identical(
    unique(util_cross_item_hrefs(cross_item[[CHECK_ID]], cross_item)),
    "dim_con.html#Contradictions"
  )
})

test_that("mixed shared groups keep their concrete result page", {
  report <- .get_local_mixed_shared_group_report()
  pages <- .call_pages(report)
  page_names <- names(pages)

  expect_true("dim_con.html" %in% page_names)
  expect_true("dim_acc_acc_multivariate_outlier.html" %in% page_names)
  expect_true("GroupX.html" %in% page_names)

  group_html <- .page_html(pages["GroupX.html"])
  expect_match(group_html, "Multivariate outliers", fixed = TRUE)
  expect_match(group_html, "Multivariate outliers (Number)", fixed = TRUE)
  expect_false(grepl("Logical contradictions", group_html, fixed = TRUE))
  expect_false(grepl("con_contradictions_redcap", group_html, fixed = TRUE))
})

test_that("util_generate_pages_from_report(): omits empty item-level general results", { # nolint: line_length_linter.
  report <- .get_local_ssi_report(
    filter_indicator_functions = "^con_ssi_range_check$"
  )

  pages <- .call_pages(report)
  html <- .page_html(pages)

  expect_false(
    grepl(">Scope of item-level data quality assessment</h2>",
      html,
      fixed = TRUE
    )
  )
  expect_false(grepl(">Data Quality Summary</h2>", html, fixed = TRUE))
  expect_match(
    html,
    "Variable-group assessment scope</span>",
    fixed = TRUE
  )
  expect_false(grepl("Data Quality Summary for scales", html, fixed = TRUE))
})

test_that("util_generate_pages_from_report(): normalizes report properties", {
  skip_if_not_installed("stringdist")
  report <- .get_mini_report()
  report2 <- report
  attr(report2, "properties") <- list(
    report_call = quote(dq_report2(study_data)),
    created_at = as.POSIXct("2026-07-22 18:55:00", tz = "UTC"),
    title = "Property normalization"
  )

  pages <- .call_pages(report2)
  html <- .page_html(pages)

  expect_match(html, "dq_report2", fixed = TRUE)
  expect_match(html, "Property normalization", fixed = TRUE)
})

test_that("util_generate_pages_from_report(): renders legacy meta_data_cross reports", { # nolint: line_length_linter.
  skip_if_not_installed("stringdist")
  report <- .get_mini_report()

  report2 <- report
  attr(report2, "meta_data_cross") <-
    util_attr(report2, "meta_data_cross_item", exact = TRUE)
  attr(report2, "meta_data_cross_item") <- NULL

  pages <- .call_pages(report2)

  expect_true(is.list(pages))
  expect_true(length(pages) >= 1)
})

test_that("cross-item metadata display summarizes variable-group items", {
  compact <- dataquieR:::util_generate_pages_compact_variable_group_items( # nolint
    c(
      paste0(
        "scaleA1: Long label without useful whitespace ",
        SPLIT_CHAR,
        " scaleA2: Another long label"
      ),
      paste(
        paste0("item", seq_len(14), ": label"),
        collapse = paste0(" ", SPLIT_CHAR, " ")
      )
    )
  )

  expect_identical(compact[[1]], paste(c("scaleA1", "scaleA2"),
      collapse = sprintf(" %s ", SPLIT_CHAR)))
  expect_match(compact[[2]], paste(c("item1", "item2"),
      collapse = sprintf(" %s ", SPLIT_CHAR)), fixed = TRUE)
  expect_match(compact[[2]], "...", fixed = TRUE)
  expect_false(grepl("Long label", compact[[1]], fixed = TRUE))

  var_names <- dataquieR:::util_generate_pages_compact_variable_group_items( # nolint
    "scaleA1|scaleA2|scaleA3"
  )
  expect_identical(var_names[[1]], paste(
    c("scaleA1", "scaleA2", "scaleA3"),
    collapse = sprintf(" %s ", SPLIT_CHAR)
  ))
})

test_that("variable-group metadata tables hide internals and compact items", {
  skip_if_not_installed("DT")

  expect_identical(
    util_generate_pages_normalize_assignment_lists(c(
      "item1|item2",
      "item1 |item2",
      "item1| item2",
      "item1 | item2"
    )),
    rep(paste(c("item1", "item2"),
        collapse = sprintf(" %s ", SPLIT_CHAR)), 4)
  )
  expect_identical(
    util_generate_pages_normalize_assignment_lists(c(
      "LABEL | cutoff = x < 5",
      "x < 5|y >= 2",
      'quoted = "a=b"|plain',
      SPLIT_CHAR,
      " | ",
      NA_character_
    )),
    c(
      "LABEL | cutoff = x < 5",
      "x < 5 | y >= 2",
      'quoted = "a=b" | plain',
      SPLIT_CHAR,
      SPLIT_CHAR,
      NA_character_
    )
  )

  meta_data_cross_item <- data.frame(
    CHECK_ID = c("group-1", "group-2"),
    CHECK_LABEL = c("Group one", "Group two"),
    VARIABLE_LIST = c(
      paste0("item1: First item ", SPLIT_CHAR, " item2: Second item"),
      "item3: Third item"
    ),
    VARIABLE_LIST_ORDER = c("item1| item2", "item3"),
    DATA_PREPARATION = c("MISSING_NA  |LIMITS", "MISSING_NA"),
    CONTRADICTION_TYPE = c("hard| soft", "hard"),
    MULTIVARIATE_OUTLIER_CHECKTYPE = c("MCD |IQR", "MCD"),
    CONTRADICTION_TERM = c('[item1] = "A|B"', ""),
    custom_text = c("left|right", ""),
    .internal = c("hidden", "hidden"),
    stringsAsFactors = FALSE
  )

  table <- util_generate_pages_variable_group_metadata(
    meta_data_cross_item,
    "group-1"
  )
  html <- paste(as.character(table), collapse = "")

  expect_match(html, "Group one", fixed = TRUE)
  expect_match(html, paste(c("item1", "item2"),
      collapse = sprintf(" %s ", SPLIT_CHAR)), fixed = TRUE)
  expect_match(html, paste(c("MISSING_NA", "LIMITS"),
      collapse = sprintf(" %s ", SPLIT_CHAR)), fixed = TRUE)
  expect_match(html, paste(c("hard", "soft"),
      collapse = sprintf(" %s ", SPLIT_CHAR)), fixed = TRUE)
  expect_match(html, paste(c("MCD", "IQR"),
      collapse = sprintf(" %s ", SPLIT_CHAR)), fixed = TRUE)
  expect_match(html, "A|B", fixed = TRUE)
  expect_false(grepl("A | B", html, fixed = TRUE))
  expect_match(html, "left|right", fixed = TRUE)
  expect_match(html, "Variable-group_Metadata_group-1", fixed = TRUE)
  expect_false(grepl(".internal", html, fixed = TRUE))
  expect_false(grepl("hidden", html, fixed = TRUE))
  expect_null(util_generate_pages_variable_group_metadata(NULL, "group-1"))
  expect_null(util_generate_pages_variable_group_metadata(
    data.frame(other = "value"),
    "group-1"
  ))
  expect_null(util_generate_pages_variable_group_metadata(
    meta_data_cross_item,
    "missing"
  ))
})

test_that("report cross-item metadata helpers use canonical attribute first", {
  report <- .get_mini_report()

  canonical <- data.frame(CHECK_ID = "canonical")
  legacy <- data.frame(CHECK_ID = "legacy")
  attr(report, "meta_data_cross_item") <- canonical
  attr(report, "meta_data_cross") <- legacy

  expect_equal(
    dataquieR:::util_report_meta_data_cross_item(report), # nolint
    canonical
  )
  expect_false(
    "meta_data_cross" %in%
      dataquieR:::util_report_meta_data_frames(report) # nolint
  )

  attr(report, "meta_data_cross_item") <- NULL
  expect_equal(
    dataquieR:::util_report_meta_data_cross_item(report), # nolint
    legacy
  )
  expect_true(
    "meta_data_cross" %in%
      dataquieR:::util_report_meta_data_frames(report) # nolint
  )
})

test_that(
  paste0(
    "util_generate_pages_from_report(): renders with ",
    "optional SSI metadata sheets"
  ),
  {
    skip_if_not_installed("stringdist")
    report <- .get_mini_report()

    cases <- list(
      all_optional_sheets = list(
        keep_item_computation = TRUE,
        keep_cross_item = TRUE
      ),
      no_item_computation = list(
        keep_item_computation = FALSE,
        keep_cross_item = TRUE
      ),
      no_cross_item = list(
        keep_item_computation = TRUE,
        keep_cross_item = FALSE
      ),
      no_optional_ssi_sheets = list(
        keep_item_computation = FALSE,
        keep_cross_item = FALSE
      )
    )

    for (case_name in names(cases)) {
      report2 <- report

      if (!isTRUE(cases[[case_name]]$keep_item_computation)) {
        attr(report2, "meta_data_item_computation") <- NULL
      }
      if (!isTRUE(cases[[case_name]]$keep_cross_item)) {
        attr(report2, "meta_data_cross_item") <- NULL
        attr(report2, "meta_data_cross") <- NULL
      }

      pages <- .call_pages(report2)

      expect_true(is.list(pages), info = case_name)
      expect_true(length(pages) >= 1, info = case_name)
    }
  }
)

test_that("SSI result tables tolerate empty summary slots", {
  curr_roles <- c(score = "CRONBACH_ALPHA")
  mapping <- c(CRONBACH_ALPHA = "Cronbach's alpha")

  expect_null(dataquieR:::util_generate_pages_ssi_result_data( # nolint
    tb = NULL,
    curr_roles = curr_roles,
    mapping_names_labels_ssi = mapping
  ))
  expect_null(dataquieR:::util_generate_pages_ssi_result_data( # nolint
    tb = character(0),
    curr_roles = curr_roles,
    mapping_names_labels_ssi = mapping
  ))
  expect_null(dataquieR:::util_generate_pages_ssi_result_data( # nolint
    tb = data.frame(Variables = character(0)),
    curr_roles = curr_roles,
    mapping_names_labels_ssi = mapping
  ))

  tb <- dataquieR:::util_generate_pages_ssi_result_data( # nolint
    tb = data.frame(Variables = "score", Value = 0.8),
    curr_roles = curr_roles,
    mapping_names_labels_ssi = mapping
  )

  expect_equal(colnames(tb), c("Metrics", "Value"))
  expect_equal(tb$Metrics, "Cronbach's alpha")
})

test_that("SSI summary tables compact Mahalanobis columns", {
  tb <- data.frame(
    Labels = c("all_questionnaire", "all_questionnaire"),
    Variables = c(
      "MISS_RESP_all_questionnaire",
      "MAHALANOBIS_all_questionnaire"
    ),
    `Admissible range` = c("[0;5)", NA),
    mahalanobis_threshold = c(NA, 0.975),
    `Above range N (%)` = c("0 (0)", NA),
    `MD_outliers (N)` = c(NA, 1),
    `MD_outliers (%)` = c(NA, 2.1),
    observational_units_removed = c(NA, 2),
    check.names = FALSE
  )

  compact <- dataquieR:::util_generate_pages_compact_ssi_summary(tb) # nolint

  expect_equal(compact$Admissible, c(
    "[0;5)",
    "\u2264 \u03c7\u00b2 quantile (p = 0.975)"
  ))
  expect_equal(compact$`Above range N (%)`, c("0 (0)", "1 (2.1)"))
  expect_false("Admissible range" %in% colnames(compact))
  expect_false("mahalanobis_threshold" %in% colnames(compact))
  expect_false("MD_outliers (N)" %in% colnames(compact))
  expect_false("MD_outliers (%)" %in% colnames(compact))
  expect_equal(compact$`Observational units removed`, c(NA, 2))
  expect_false("observational_units_removed" %in% colnames(compact))
  expect_match(
    attr(compact, "description")[["Observational units removed"]],
    "excluded",
    fixed = TRUE
  )
  expect_equal(colnames(compact)[seq_len(4)], c(
    "Labels",
    "Variables",
    "Admissible",
    "Above range N (%)"
  ))
})

test_that("summary table rules coalesce and remove technical sources", {
  rules <- list(list(
    sources = "technical_source",
    target = "Readable target",
    formatter = util_summary_table_identity
  ))
  tb <- data.frame(
    technical_source = c("unused", "replacement"),
    `Readable target` = c("existing", NA_character_),
    check.names = FALSE
  )

  transformed <- util_apply_summary_table_rules(tb, rules)

  expect_identical(
    transformed[["Readable target"]],
    c("existing", "replacement")
  )
  expect_false("technical_source" %in% colnames(transformed))
  expect_identical(
    util_apply_summary_table_rules(tb, list(list(
      sources = "missing_source",
      target = "unused",
      formatter = util_summary_table_identity
    ))),
    tb
  )
})

test_that("SSI metric summaries hide technical variable names", {
  tb <- data.frame(
    Labels = c("all_questionnaire", "Page1"),
    Variables = c(
      "MAXIMUM_LONG_STRING_all_questionnaire",
      "MAXIMUM_LONG_STRING_Page1"
    ),
    `Admissible range` = c("[0;25)", "[0;25)"),
    check.names = FALSE
  )

  compact <- util_generate_pages_compact_ssi_metric_summary(tb)

  expect_true("Variables" %in% colnames(compact))
  expect_identical(util_attr(compact, "hideCols", exact = TRUE), "Variables")
  expect_equal(compact$Labels, c("all_questionnaire", "Page1"))
  expect_equal(compact$Admissible, c("[0;25)", "[0;25)"))
})

test_that("SSI summary metrics link to their detailed sections", {
  tb <- data.frame(
    Metrics = c("Missing responses", "Mahalanobis Distance"),
    N = c(50L, 48L)
  )

  linked <- util_generate_pages_link_ssi_summary_metrics(
    tb,
    metric_roles = c("MISS_RESP", "MAHALANOBIS_RATIO"),
    section_prefix = "all_questionnaire"
  )

  expect_true(isTRUE(attr(linked, "is_html_escaped")))
  expect_match(
    linked$Metrics[[1]],
    'href="#all_questionnaire.MISS_RESP"',
    fixed = TRUE
  )
  expect_match(
    linked$Metrics[[2]],
    'href="#all_questionnaire.MAHALANOBIS_RATIO"',
    fixed = TRUE
  )
})

test_that("SSI sections build anchors, information, and page navigation", {
  skip_if_not_installed("htmltools")

  section <- dataquieR:::util_generate_pages_ssi_section( # nolint
    id = "MISS_RESP.all_questionnaire",
    title = "Missing responses",
    content = htmltools::span("result"),
    description = "Metric description"
  )
  page <- dataquieR:::util_generate_pages_ssi_sections(list(section)) # nolint
  html <- as.character(page)

  expect_match(html, 'class="floatbar"', fixed = TRUE)
  expect_match(html, 'href="#MISS_RESP.all_questionnaire"', fixed = TRUE)
  expect_match(html, 'id="MISS_RESP.all_questionnaire"', fixed = TRUE)
  expect_match(html, 'class="infobutton"', fixed = TRUE)
  expect_match(html, "Metric description", fixed = TRUE)
})

test_that("SSI cross-item page titles use scale metadata", {
  titles <- dataquieR:::util_generate_pages_ssi_cross_item_titles( # nolint
    c(
      CHECK_ID = "scale_1",
      CHECK_LABEL = "scale_check",
      SCALE_NAME = "Scale One",
      SCALE_ACRONYM = "S1"
    )
  )

  expect_equal(titles[["short_title"]], "scale_check")
  expect_equal(titles[["long_title"]], "scale_check (Scale One -- S1)")
})

test_that("SSI cross-item page titles fall back to check metadata", {
  titles <- dataquieR:::util_generate_pages_ssi_cross_item_titles( # nolint
    c(
      CHECK_ID = "scale_1",
      CHECK_LABEL = "scale_check",
      SCALE_NAME = NA_character_,
      SCALE_ACRONYM = ""
    )
  )

  expect_equal(titles[["short_title"]], "scale_check")
  expect_equal(titles[["long_title"]], "scale_check")
})

test_that("variable-group roles are read from normalized result rows", {
  results <- data.frame(
    placeholder = seq_len(3),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  results[["placeholder"]] <- NULL
  results[[VAR_NAMES]] <- c("group-1", "group-2", "group-3")
  results[[LABEL]] <- c("Group One", "Group Two", "Group Three")
  results[[COMPUTED_VARIABLE_ROLE]] <- c(
    "MAXIMUM_LONG_STRING",
    "MISS_RESP",
    NA_character_
  )
  results[["Metric"]] <- c(
    "Maximum Long String",
    "Missing responses",
    "Association"
  )
  results[["value"]] <- c("0", "1", "0.5")
  results[["Class"]] <- c("Ok", "Important", "Ok")

  roles <- dataquieR:::util_generate_pages_variable_group_roles(results) # nolint
  metric_results <-
    dataquieR:::util_generate_pages_variable_group_results( # nolint
      results,
      role = "MAXIMUM_LONG_STRING",
      label_col = LABEL
    )

  expect_setequal(roles, c("MAXIMUM_LONG_STRING", "MISS_RESP"))
  expect_equal(nrow(metric_results), 1)
  expect_equal(metric_results[["Variable group"]], "Group One")
  expect_equal(metric_results[["Indicator Metric"]], "Maximum Long String")
})

test_that("variable-group roles support translated dashboard columns", {
  results <- data.frame(
    VAR_NAMES = "group-1",
    LABEL = "Group One",
    COMPUTED_VARIABLE_ROLE = "MAXIMUM_LONG_STRING",
    stringsAsFactors = FALSE
  )
  translated <- util_translate(colnames(results), ns = "dashboard_table")
  util_translated_colnames(results) <- translated

  expect_identical(
    dataquieR:::util_generate_pages_variable_group_roles(results), # nolint
    "MAXIMUM_LONG_STRING"
  )
})

test_that("variable-group role helpers tolerate unavailable result data", {
  expect_identical(
    dataquieR:::util_generate_pages_variable_group_roles(NULL), # nolint
    character()
  )
  expect_null(dataquieR:::util_generate_pages_variable_group_results( # nolint
    NULL,
    role = "MISS_RESP",
    label_col = LABEL
  ))
})

test_that("non-SSI variable-group calls get their own result inventories", {
  results <- data.frame(
    placeholder = seq_len(3),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  results[["placeholder"]] <- NULL
  results[[VAR_NAMES]] <- c("group-1", "group-2", "group-3")
  results[[LABEL]] <- c(
    "<a href='group-1.html'>Group One</a>",
    "<a href='group-2.html'>Group Two</a>",
    "<a href='group-3.html'>Group Three</a>"
  )
  results[[COMPUTED_VARIABLE_ROLE]] <- c(
    "MAXIMUM_LONG_STRING",
    "MISS_RESP",
    "MAXIMUM_LONG_STRING"
  )
  results[["function_name"]] <- c(
    "con_ssi_range_check",
    "acc_repeated_measurements",
    "con_contradictions_redcap"
  )
  results[["Call"]] <- c(
    "Limits",
    "Repeated measurements",
    "Contradictions"
  )
  results[["Metric"]] <- c(
    "Maximum Long String",
    "Disagreement",
    "Logical contradictions"
  )
  results[["value"]] <- c("0", "2.08", "0")
  results[["Class"]] <- c("Ok", "Important", "Ok")

  calls <- dataquieR:::util_generate_pages_variable_group_calls(results) # nolint
  call_results <-
    dataquieR:::util_generate_pages_variable_group_call_results( # nolint
      results,
      call = "Repeated measurements",
      label_col = LABEL
    )

  expect_identical(calls, "Repeated measurements")
  expect_identical(
    util_generate_pages_variable_group_call_functions(
      results,
      "Repeated measurements"
    ),
    "acc_repeated_measurements"
  )
  expect_equal(nrow(call_results), 1)
  expect_match(call_results[["Variable group"]], "group-2.html", fixed = TRUE)
  expect_equal(call_results[["Indicator Metric"]], "Disagreement")
})

test_that("variable-group call helpers tolerate unavailable result data", {
  expect_identical(
    dataquieR:::util_generate_pages_variable_group_calls(NULL), # nolint
    character()
  )
  expect_identical(
    util_generate_pages_variable_group_call_functions(
      data.frame(Call = "Contradictions"),
      "Contradictions"
    ),
    character()
  )
  expect_identical(
    util_generate_pages_variable_group_call_functions(
      data.frame(function_name = "con_contradictions_redcap"),
      "Contradictions"
    ),
    character()
  )
  expect_null(
    dataquieR:::util_generate_pages_variable_group_call_results( # nolint
      NULL,
      call = "Contradictions",
      label_col = LABEL
    )
  )
})

test_that("variable-group inventory keeps one row per stable group ID", {
  results <- data.frame(
    CHECK_ID = c("8", "8", "2"),
    LABEL = c("technical row one", "technical row two", "fallback label"),
    stringsAsFactors = FALSE
  )
  cross_item <- data.frame(
    CHECK_ID = c("2", "8"),
    CHECK_LABEL = c("Empirical contradictions", "Repeated measurements"),
    SCALE_NAME = c(NA_character_, "Scale H response group"),
    SCALE_ACRONYM = c(NA_character_, "H"),
    stringsAsFactors = FALSE
  )

  inventory <- dataquieR:::util_generate_pages_variable_group_inventory( # nolint
    results,
    cross_item,
    label_col = LABEL
  )

  expect_equal(as.character(inventory[[CHECK_ID]]), c("8", "2"))
  expect_equal(
    as.character(inventory[[CHECK_LABEL]]),
    c("Repeated measurements", "Empirical contradictions")
  )
})

test_that("actual variable-group results use stable metadata identities", {
  cross_item <- data.frame(
    CHECK_ID = c("6", "7"),
    CHECK_LABEL = c("Scale F response group", "Scale G association group"),
    stringsAsFactors = FALSE
  )
  result_call <- quote(des_scatterplot_matrix(study_data, meta_data))
  result <- structure(
    list(
      VariableGroupTable = data.frame(
        CHECK_ID = "7",
        CHECK_LABEL = "Scale G association group",
        max_cor = 0.72,
        stringsAsFactors = FALSE
      ),
      VariableGroupPlotList = list(
        "7" = list(plot = TRUE)
      )
    ),
    class = "dataquieR_result",
    call = result_call
  )

  ids <- dataquieR:::util_generate_pages_result_group_ids( # nolint
    result,
    cross_item
  )
  subset <- dataquieR:::util_generate_pages_variable_group_result_subset( # nolint
    result,
    cross_item[2, , drop = FALSE]
  )

  expect_identical(ids, "7")
  expect_equal(nrow(subset[["VariableGroupTable"]]), 1L)
  expect_identical(names(subset[["VariableGroupPlotList"]]),
    "7")
  expect_identical(util_attr(subset, "call", exact = TRUE), result_call)
  expect_null(dataquieR:::util_generate_pages_variable_group_result_subset( # nolint
    result,
    cross_item[1, , drop = FALSE]
  ))
})

test_that("variable-group pages are routed by result type", {
  expect_setequal(
    dataquieR:::util_generate_pages_shared_variable_group_functions(), # nolint
    c(
      "con_contradictions",
      "con_contradictions_redcap",
      "des_scatterplot_matrix"
    )
  )
  scatterplot <- structure(
    list(VariableGroupTable = data.frame(CHECK_ID = "7")),
    class = "dataquieR_result"
  )
  attr(scatterplot, "dq_report_function_name") <- "des_scatterplot_matrix"

  expect_null(
    dataquieR:::util_generate_pages_variable_group_menu( # nolint
      generic_group_functions = character(),
      actual_group_results = list(des_scatterplot_matrix = scatterplot),
      has_ssi_results = FALSE
    )
  )
  expect_identical(
    dataquieR:::util_generate_pages_variable_group_menu( # nolint
      generic_group_functions = "des_scatterplot_matrix",
      actual_group_results = list(scatterplot),
      has_ssi_results = TRUE
    ),
    VARIABLE_GROUP_REPORT_MENU
  )

  attr(scatterplot, "dq_report_function_name") <- "con_contradictions"
  expect_null(
    dataquieR:::util_generate_pages_variable_group_menu( # nolint
      generic_group_functions = character(),
      actual_group_results = list(scatterplot),
      has_ssi_results = FALSE
    )
  )
  expect_identical(
    dataquieR:::util_generate_pages_variable_group_menu( # nolint
      generic_group_functions = "acc_repeated_measurements",
      actual_group_results = list(),
      has_ssi_results = FALSE
    ),
    VARIABLE_GROUP_REPORT_MENU
  )
  expect_identical(
    dataquieR:::util_generate_pages_variable_group_menu( # nolint
      generic_group_functions = c(
        "des_scatterplot_matrix",
        "acc_repeated_measurements"
      ),
      actual_group_results = list(),
      has_ssi_results = FALSE
    ),
    VARIABLE_GROUP_REPORT_MENU
  )
})

test_that("variable-group display helpers reject incomplete result tables", {
  no_call <- data.frame(
    LABEL = "Group One",
    COMPUTED_VARIABLE_ROLE = "MISS_RESP",
    stringsAsFactors = FALSE
  )
  no_role <- data.frame(
    LABEL = "Group One",
    Call = "Contradictions",
    stringsAsFactors = FALSE
  )
  no_display_columns <- data.frame(
    Call = "Contradictions",
    COMPUTED_VARIABLE_ROLE = "MISS_RESP",
    stringsAsFactors = FALSE
  )

  expect_null(
    dataquieR:::util_generate_pages_variable_group_call_results( # nolint
      no_call,
      call = "Contradictions",
      label_col = LABEL
    )
  )
  expect_null(
    dataquieR:::util_generate_pages_variable_group_call_results( # nolint
      no_role,
      call = "Missing call",
      label_col = LABEL
    )
  )
  call_only <-
    dataquieR:::util_generate_pages_variable_group_call_results( # nolint
      no_display_columns,
      call = "Contradictions",
      label_col = LABEL
    )
  expect_identical(colnames(call_only), "Call")
  expect_null(dataquieR:::util_generate_pages_variable_group_results( # nolint
    no_role,
    role = "MISS_RESP",
    label_col = LABEL
  ))
  expect_null(dataquieR:::util_generate_pages_variable_group_results( # nolint
    no_call,
    role = "MAXIMUM_LONG_STRING",
    label_col = LABEL
  ))
  role_only <- dataquieR:::util_generate_pages_variable_group_results( # nolint
    no_display_columns,
    role = "MISS_RESP",
    label_col = LABEL
  )
  expect_identical(nrow(role_only), 1L)
})

test_that("variable-group inventory handles incomplete metadata", {
  empty <- dataquieR:::util_generate_pages_variable_group_inventory( # nolint
    NULL,
    NULL,
    label_col = LABEL
  )
  expect_identical(nrow(empty), 0L)
  expect_setequal(
    colnames(empty),
    c(CHECK_ID, CHECK_LABEL, SCALE_NAME, SCALE_ACRONYM)
  )

  without_ids <- data.frame(LABEL = "Only a label", stringsAsFactors = FALSE)
  expect_identical(
    nrow(dataquieR:::util_generate_pages_variable_group_inventory( # nolint
      without_ids,
      NULL,
      label_col = LABEL
    )),
    0L
  )

  empty_ids <- data.frame(CHECK_ID = c("", NA), stringsAsFactors = FALSE)
  expect_identical(
    nrow(dataquieR:::util_generate_pages_variable_group_inventory( # nolint
      empty_ids,
      NULL,
      label_col = LABEL
    )),
    0L
  )

  results <- data.frame(
    CHECK_ID = c("group-1", "group-2"),
    LABEL = c("Group One", "Group Two"),
    stringsAsFactors = FALSE
  )
  inventory <- dataquieR:::util_generate_pages_variable_group_inventory( # nolint
    results,
    list(not = "metadata"),
    label_col = LABEL,
    columns = c(CHECK_ID, CHECK_LABEL, "EXTRA")
  )
  expect_identical(as.character(inventory[[CHECK_ID]]), c("group-1", "group-2"))
  expect_identical(
    as.character(inventory[[CHECK_LABEL]]),
    c("Group One", "Group Two")
  )
  expect_true(all(is.na(inventory[["EXTRA"]])))
})

test_that("variable-group inventory matches IDs and preserves fallbacks", {
  results <- data.frame(
    CHECK_ID = c("new-id", "known-id", "unmatched-id"),
    LABEL = c("Known label", "", "Fallback label"),
    stringsAsFactors = FALSE
  )
  cross_item <- data.frame(
    CHECK_ID = c("different-id", "known-id"),
    CHECK_LABEL = c("Known label", NA_character_),
    SCALE_NAME = c("Matched by label", "Matched by ID"),
    stringsAsFactors = FALSE
  )

  inventory <- dataquieR:::util_generate_pages_variable_group_inventory( # nolint
    results,
    cross_item,
    label_col = LABEL
  )

  expect_identical(
    as.character(inventory[[CHECK_ID]]),
    c("new-id", "known-id", "unmatched-id")
  )
  expect_identical(
    as.character(inventory[[CHECK_LABEL]]),
    c("Known label", NA_character_, "Fallback label")
  )
  expect_identical(
    as.character(inventory[[SCALE_NAME]]),
    c(NA_character_, "Matched by ID", NA_character_)
  )
})

test_that("report result helpers keep only non-empty result objects", {
  result <- structure(list(value = 1), class = "dataquieR_result")
  null_result <- structure(
    list(value = 2),
    class = c("dataquieR_NULL", "dataquieR_result")
  )
  empty_result <- structure(list(), class = "dataquieR_result")

  expect_identical(
    dataquieR:::util_generate_pages_function_results(NULL, "fun"), # nolint
    list()
  )
  report <- data.frame(other = 1)
  expect_identical(
    dataquieR:::util_generate_pages_function_results(report, "fun"), # nolint
    list()
  )

  report <- data.frame(fun = I(list(result, null_result, empty_result)))
  kept <- dataquieR:::util_generate_pages_function_results(report, "fun") # nolint
  expect_length(kept, 1L)
  expect_identical(kept[[1]], result)

  single <- data.frame(fun = I(list(result)))
  single[["fun"]] <- result
  expect_identical(
    dataquieR:::util_generate_pages_function_results(single, "fun"), # nolint
    list(result)
  )
})

test_that("result group IDs require explicit stable identities", {
  cross_item <- data.frame(
    CHECK_ID = c("1", "2", "3"),
    CHECK_LABEL = c("Call group", "Table group", "Plot group"),
    SCALE_NAME = c("Scale one", "Scale two", "Scale three"),
    stringsAsFactors = FALSE
  )
  call <- structure(list(), CHECK_ID = "1")
  result <- structure(
    list(
      VariableGroupTable = data.frame(
        CHECK_ID = "2",
        stringsAsFactors = FALSE
      ),
      VariableGroupPlotList = list(`3` = list(plot = TRUE)),
      ignored = list(`Call group` = TRUE)
    ),
    class = "dataquieR_result",
    call = call
  )

  expect_identical(
    dataquieR:::util_generate_pages_result_group_ids(result, cross_item), # nolint
    c("1", "2", "3")
  )
  expect_identical(
    dataquieR:::util_generate_pages_result_group_ids(list(), cross_item), # nolint
    character()
  )
  expect_identical(
    dataquieR:::util_generate_pages_result_group_ids(result, NULL), # nolint
    character()
  )
  expect_identical(
    dataquieR:::util_generate_pages_result_group_ids(
      result,
      data.frame(CHECK_LABEL = "Call group")
    ), # nolint
    character()
  )

  unmatched <- structure(list(value = 1), class = "dataquieR_result")
  expect_identical(
    dataquieR:::util_generate_pages_result_group_ids(unmatched, cross_item), # nolint
    character()
  )

  attributed <- structure(
    list(VariableGroupData = data.frame(value = 1)),
    class = "dataquieR_result",
    CHECK_ID = "1"
  )
  subset <- dataquieR:::util_generate_pages_variable_group_result_subset( # nolint
    attributed,
    cross_item[1, , drop = FALSE]
  )
  expect_identical(util_attr(subset, CHECK_ID, exact = TRUE), "1")
  expect_identical(
    util_attr(subset, CHECK_LABEL, exact = TRUE),
    "Call group"
  )
})

test_that("requested SSI groups exclude cross-item rows without requests", {
  cross_item <- data.frame(
    CHECK_ID = as.character(1:4),
    CHECK_LABEL = paste("Group", 1:4),
    MISS_RESP = c("[0;1]", NA, "", NA),
    MAXIMUM_LONG_STRING = c(NA, "[0;2]", "", NA),
    stringsAsFactors = FALSE
  )

  groups <- dataquieR:::util_generate_pages_requested_ssi_groups( # nolint
    cross_item,
    c(MISS_RESP, MAXIMUM_LONG_STRING),
    columns = c(CHECK_ID, CHECK_LABEL)
  )

  expect_identical(groups[[CHECK_ID]], c("1", "2"))
  expect_true(all(c(MISS_RESP, MAXIMUM_LONG_STRING) %in% names(groups)))
})

test_that("unclassified variable-group summaries still render a pie", {
  skip_on_cran()
  skip_if_not_installed("htmltools")

  rendered <- new.env(parent = emptyenv())
  rendered$data <- NULL
  testthat::local_mocked_bindings(
    prep_render_pie_chart_from_summaryclasses_ggplot2 = function(data, ...) {
      rendered$data <- data
      htmltools::span("unclassified pie")
    }
  )

  plot <- util_generate_pages_unclassified_group_summary(
    c("Group A", "Group B", "Group A", NA, ""),
    use_plotly = FALSE,
    meta_data = data.frame()
  )

  expect_s3_class(plot, "shiny.tag")
  expect_identical(rendered$data$value, 2L)
  expect_true(is.na(rendered$data$class))
  expect_match(rendered$data$note, "Group A<br>Group B", fixed = TRUE)
  expect_identical(
    attr(rendered$data, "vars_to_include", exact = TRUE),
    "variable_group"
  )
  expect_null(util_generate_pages_unclassified_group_summary(
    c(NA, ""),
    use_plotly = FALSE,
    meta_data = data.frame()
  ))
})

test_that(
  "util_generate_pages_from_report(): errors if parallelMap.mode is not local",
  {
    skip_if_not_installed("stringdist")
    report <- .get_mini_report()
    withr::local_options(list(parallelMap.mode = "snow"))

    expect_error(
      .call_pages(report),
      "parallel rendering of reports is not supported",
      fixed = TRUE
    )
  }
)

test_that("unclassified group summaries select the requested chart backend", {
  skip_on_cran()
  skip_if_not_installed("htmltools")
  skip_if_not_installed("plotly")

  plot <- util_generate_pages_unclassified_group_summary(
    c("Group A", "Group B"),
    use_plotly = TRUE,
    meta_data = data.frame()
  )

  expect_s3_class(plot, "shiny.tag")
  expect_s3_class(plot$children[[1]], "plotly")
  expect_match(htmltools::renderTags(plot)$html,
    "0 of 2 available variable groups classified",
    fixed = TRUE
  )
})

test_that("plotly title wrapping retains empty and escaped titles", {
  skip_if_not_installed("htmltools")

  empty <- util_plotly_wrap_text("")
  escaped <- util_plotly_wrap_text("Group <A> & Group B", width = 8L)

  expect_identical(as.character(empty), "")
  expect_identical(attr(empty, "line_count", exact = TRUE), 1L)
  expect_match(as.character(escaped), "&lt;", fixed = TRUE)
  expect_match(as.character(escaped), "&amp;", fixed = TRUE)
  expect_gt(attr(escaped, "line_count", exact = TRUE), 1L)
})

test_that("variable-group pages resolve display-only group rows", {
  report <- .get_local_variable_group_report()
  repsum <- summary(report)
  report2 <- report
  issue <- rlang::warning_cnd(
    message = "condition without a DQ indicator mapping"
  )
  attr(report2, "integrity_issues_before_pipeline") <- list(issue)

  original_dashboard <- util_setup_dashboard
  testthat::local_mocked_bindings(
    util_get_cores_safe = function() NA_real_,
    util_setup_dashboard = function(
      report,
      make_links = FALSE,
      return_table_only = FALSE,
      repsum = NULL,
      vars_to_include = "study"
    ) {
      if (!identical(vars_to_include, "variable_group")) {
        return(original_dashboard(
          report,
          make_links = make_links,
          return_table_only = return_table_only,
          repsum = repsum,
          vars_to_include = vars_to_include
        ))
      }
      data.frame(
        LABEL = c(
          "Contradiction group",
          "Long-string group",
          "Repeated-measurement group"
        ),
        .variable_group_result_label = c(
          "Logical contradictions",
          "Maximum long string",
          "Disagreement"
        ),
        Call = c(
          "Contradictions",
          "Limits",
          "Repeated measurements"
        ),
        Metric = c(
          "Logical contradictions",
          "Maximum Long String",
          "Disagreement"
        ),
        value = c("0", "0", "2.08"),
        Class = c("Ok", "Ok", "Important"),
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
    }
  )

  expect_no_warning(
    pages <- .call_pages(report2, repsum = repsum)
  )
  html <- .page_html(pages)

  expect_match(html, "Contradiction group", fixed = TRUE)
  expect_match(html, "Long-string group", fixed = TRUE)
  expect_match(html, "Repeated-measurement group", fixed = TRUE)
  expect_match(html, "Logical contradictions", fixed = TRUE)
  expect_match(html, "Disagreement", fixed = TRUE)
  expect_true("statisticalsettings.html" %in% names(pages))
  repeated_measurements_html <- .page_html(
    pages["dim_acc_acc_repeated_measurements.html"]
  )
  expect_match(
    repeated_measurements_html,
    paste0(
      "statisticalsettings.html?dq_filter_col=SETTING_ID&amp;",
      "dq_filter_value=rm_rmse_default"
    ),
    fixed = TRUE
  )
  settings_html <- .page_html(pages["statisticalsettings.html"])
  expect_match(settings_html, "rm_rmse_default", fixed = TRUE)
  expect_false(grepl(
    "condition without a DQ indicator mapping",
    html,
    fixed = TRUE
  ))

  matrix <- print(
    repsum,
    grouped_by = "indicator_metric",
    vars_to_include = "variable_group"
  )
  matrix_columns <- names(attr(matrix, "repsum_wide", exact = TRUE))
  expect_true("Variable group" %in% matrix_columns)
  expect_false("Variables" %in% matrix_columns)

  fallback_repsum <- repsum
  fallback_this <- new.env(parent = parent.env(attr(repsum, "this")))
  list2env(
    as.list.environment(attr(repsum, "this"), all.names = TRUE),
    envir = fallback_this
  )
  fallback_this$labels <- list(cat1 = "Only")
  fallback_this$colors <- list(cat1 = "#111111")
  attr(fallback_repsum, "this") <- fallback_this
  fallback_matrix <- NULL
  expect_warning(
    fallback_matrix <- util_render_table_dataquieR_summary(
      fallback_repsum,
      grouped_by = "indicator_metric",
      vars_to_include = "variable_group"
    ),
    "Could not find enough categories"
  )
  expect_s3_class(fallback_matrix, "shiny.tag.list")

  expect_s3_class(util_report_scope_item_coverage(report), "data.frame")
})
