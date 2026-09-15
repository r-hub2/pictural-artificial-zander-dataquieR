local_segment_missingness_options <- function() {
  withr::local_options(
    dataquieR.CONDITIONS_WITH_STACKTRACE = FALSE,
    dataquieR.ERRORS_WITH_CALLER = FALSE,
    dataquieR.WARNINGS_WITH_CALLER = FALSE,
    dataquieR.MESSAGES_WITH_CALLER = FALSE
  )
}

local_segment_missingness_bar_data <- function(plot) {
  bar_layer <- which(vapply(plot$layers, function(layer) {
    inherits(layer$geom, "GeomBar")
  }, logical(1)))
  stopifnot(length(bar_layer) == 1L)
  ggplot2::ggplot_build(plot)$data[[bar_layer]]
}

local_segment_missingness_fixture <- function() {
  study_data <- data.frame(
    PART_a = c(1, NA, 3, 4),
    a2 = c(NA, 2, 3, 4),
    b = c(NA, 2, NA, 4),
    c = c(1, NA, NA, 4),
    strata = c("s1", NA, "s2", "s2"),
    stringsAsFactors = FALSE
  )
  meta_data <- data.frame(
    VAR_NAMES = c("PART_a", "a2", "b", "c", "strata"),
    LABEL = c("PART_a", "a2", "b", "c", "strata"),
    DATA_TYPE = DATA_TYPES$STRING,
    SCALE_LEVEL = SCALE_LEVELS$NOMINAL,
    STUDY_SEGMENT = c("a", "a", "b", "b", "b"),
    MISSING_LIST = "",
    JUMP_LIST = "",
    stringsAsFactors = FALSE
  )
  list(study_data = study_data, meta_data = meta_data)
}

local_segment_missingness_threshold_fixture <- function() {
  study_data <- data.frame(
    a1 = c(rep(NA, 2), seq_len(8)),
    a2 = c(rep(NA, 2), seq_len(8)),
    b1 = c(rep(NA, 6), seq_len(4)),
    b2 = c(rep(NA, 6), seq_len(4))
  )
  meta_data <- data.frame(
    VAR_NAMES = c("a1", "a2", "b1", "b2"),
    LABEL = c("a1", "a2", "b1", "b2"),
    DATA_TYPE = DATA_TYPES$FLOAT,
    SCALE_LEVEL = SCALE_LEVELS$RATIO,
    STUDY_SEGMENT = c("a", "a", "b", "b"),
    VARIABLE_ROLE = VARIABLE_ROLES$PRIMARY,
    MISSING_LIST = "",
    JUMP_LIST = "",
    stringsAsFactors = FALSE
  )
  list(study_data = study_data, meta_data = meta_data)
}

full_segment_missingness_fixture <- local({
  fixture <- NULL

  function() {
    if (is.null(fixture)) {
      meta_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data.RData") # nolint: line_length_linter.
      study_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData", keep_types = TRUE) # nolint: line_length_linter.
      meta_data2 <- prep_scalelevel_from_data_and_metadata(
        study_data = study_data,
        meta_data = meta_data
      )
      meta_data[[SCALE_LEVEL]] <- setNames(
        meta_data2[[SCALE_LEVEL]],
        nm = meta_data2[[VAR_NAMES]]
      )[meta_data[[VAR_NAMES]]]
      fixture <- list(study_data = study_data, meta_data = meta_data)
    }

    fixture
  }
})

capture_segment_missingness_warnings <- function(expr) {
  captured <- new.env(parent = emptyenv())
  captured$warnings <- character()
  value <- withCallingHandlers(
    expr,
    warning = function(w) {
      captured$warnings <- c(captured$warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  list(value = value, warnings = captured$warnings)
}

test_that("segment missingness distinguishes grading and legacy thresholds", {
  skip_on_cran()

  fixture <- local_segment_missingness_threshold_fixture()
  local_segment_missingness_options()

  graded <- suppressWarnings(suppressMessages(com_segment_missingness(
    study_data = fixture$study_data,
    meta_data = fixture$meta_data,
    label_col = VAR_NAMES,
    expected_observations = "ALL",
    exclude_roles = character()
  )))
  graded_data <- unclass(graded)[["ResultData"]]
  expect_identical(
    util_attr(graded_data[[GRADING_RULESET]], DATA_TYPE, exact = TRUE),
    DATA_TYPES$INTEGER
  )
  expect_named(
    graded,
    c("ResultData", "ReportSummaryTable", "SummaryPlot")
  )
  expect_s3_class(graded$ReportSummaryTable, "ReportSummaryTable")
  expect_equal(
    graded$ReportSummaryTable$`No. missing segments`,
    c(2, 6)
  )
  expect_equal(graded$ReportSummaryTable$N, c(10, 10))
  expect_null(util_report_summary_table_continuous(
    graded$ReportSummaryTable
  ))
  expect_null(util_report_summary_table_colcode(graded$ReportSummaryTable))
  expect_type(
    util_attr(graded$ReportSummaryTable, "grading_context", exact = TRUE),
    "list"
  )
  graded_bar_context <- util_attr(
    graded$ReportSummaryTable,
    "segment_missingness_bar",
    exact = TRUE
  )
  expect_identical(graded_bar_context$color_mode, "grading_rules")
  expect_false(any(c("colors", "class_labels") %in% names(
    graded_bar_context
  )))
  expect_true(util_report_summary_table_relative(graded$ReportSummaryTable))
  expect_false(any(c("threshold", "direction") %in% names(graded_data)))
  expect_equal(as.numeric(graded_data$`(%) of missing segments`), c(20, 60))
  graded_plot_promise <- dq_lazy_unwrap(graded$SummaryPlot)
  expect_s3_class(graded_plot_promise, "dq_lazy_ggplot")
  expect_null(util_report_summary_table_colcode(
    unclass(graded_plot_promise)$env$repsumtab
  ))
  graded_plot <- prep_realize_ggplot(graded$SummaryPlot)
  graded_plot_build <- ggplot2::ggplot_build(graded_plot)
  expect_true(any(vapply(graded_plot$layers, function(layer) {
    inherits(layer$geom, "GeomBar")
  }, logical(1))))
  expect_null(graded_plot_build$plot$scales$get_scales("x")$name)
  graded_bar_data <- local_segment_missingness_bar_data(graded_plot)
  expect_equal(graded_bar_data$y, c(0.2, 0.6))
  expect_equal(
    graded_bar_data$fill,
    rep(unname(util_get_colors()[["2"]]), 2)
  )
  graded_fill_scale <- graded_plot_build$plot$scales$get_scales("fill")
  expect_identical(graded_fill_scale$get_labels(), "Unclear")
  expect_false(any(grepl("%", graded_fill_scale$get_labels(), fixed = TRUE)))
  expect_identical(graded_plot$theme$legend.position, "bottom")
  expect_identical(graded_plot$theme$axis.text.x$angle, 0)
  expect_null(util_report_summary_table_colcode(graded$ReportSummaryTable))

  invalid <- suppressWarnings(suppressMessages(com_segment_missingness(
    study_data = fixture$study_data,
    meta_data = fixture$meta_data,
    label_col = VAR_NAMES,
    threshold_value = NA_real_,
    expected_observations = "ALL",
    exclude_roles = character()
  )))
  invalid_data <- unclass(invalid)[["ResultData"]]
  expect_s3_class(invalid$ReportSummaryTable, "ReportSummaryTable")
  expect_false(any(c("threshold", "direction") %in% names(invalid_data)))
  expect_s3_class(
    dq_lazy_unwrap(invalid$SummaryPlot),
    "dq_lazy_ggplot"
  )

  legacy <- suppressWarnings(suppressMessages(com_segment_missingness(
    study_data = fixture$study_data,
    meta_data = fixture$meta_data,
    label_col = VAR_NAMES,
    threshold_value = 50,
    color_gradient_direction = "above",
    expected_observations = "ALL",
    exclude_roles = character()
  )))
  legacy_data <- unclass(legacy)[["ResultData"]]
  expect_named(
    legacy,
    c("ResultData", "ReportSummaryTable", "SummaryPlot")
  )
  expect_s3_class(legacy$ReportSummaryTable, "ReportSummaryTable")
  expect_false(util_report_summary_table_continuous(
    legacy$ReportSummaryTable
  ))
  expect_true(util_report_summary_table_relative(legacy$ReportSummaryTable))
  expect_null(util_attr(
    legacy$ReportSummaryTable,
    "grading_context",
    exact = TRUE
  ))
  legacy_bar_context <- util_attr(
    legacy$ReportSummaryTable,
    "segment_missingness_bar",
    exact = TRUE
  )
  expect_identical(legacy_bar_context$color_mode, "legacy_threshold")
  expect_equal(legacy_bar_context$colors, c("#2166AC", "#7f0000"))
  expect_equal(legacy_bar_context$class_labels, c("Normal", "Critical"))
  expect_equal(as.numeric(legacy_data$threshold), c(50, 50))
  expect_equal(as.character(legacy_data$direction), c("above", "above"))
  expect_identical(
    util_attr(legacy$SummaryPlot,
      "segment_missingness_colors",
      exact = TRUE
    ),
    "legacy_threshold"
  )
  legacy_plot_build <- ggplot2::ggplot_build(legacy$SummaryPlot)
  expect_true(any(vapply(legacy$SummaryPlot$layers, function(layer) {
    inherits(layer$geom, "GeomBar")
  }, logical(1))))
  expect_null(legacy_plot_build$plot$scales$get_scales("x")$name)
  legacy_bar_data <- local_segment_missingness_bar_data(legacy$SummaryPlot)
  expect_equal(legacy_bar_data$y, c(0.2, 0.6))
  expect_equal(legacy_bar_data$fill, c("#2166AC", "#7f0000"))
  legacy_fill_scale <- legacy_plot_build$plot$scales$get_scales("fill")
  expect_equal(legacy_fill_scale$get_labels(), c("Normal", "Critical"))
  expect_false(any(grepl("%", legacy_fill_scale$get_labels(), fixed = TRUE)))

  grouped_fixture <- fixture
  grouped_fixture$study_data$group <- rep(c("g1", "g2"), each = 5)
  grouped_fixture$study_data$stratum <- rep(c("s1", "s2"), 5)
  grouped_fixture$meta_data <- rbind(
    grouped_fixture$meta_data,
    data.frame(
      VAR_NAMES = c("group", "stratum"),
      LABEL = c("group", "stratum"),
      DATA_TYPE = DATA_TYPES$STRING,
      SCALE_LEVEL = SCALE_LEVELS$NOMINAL,
      STUDY_SEGMENT = c("a", "b"),
      VARIABLE_ROLE = VARIABLE_ROLES$PROCESS,
      MISSING_LIST = "",
      JUMP_LIST = "",
      stringsAsFactors = FALSE
    )
  )
  grouped <- suppressWarnings(suppressMessages(com_segment_missingness(
    study_data = grouped_fixture$study_data,
    meta_data = grouped_fixture$meta_data,
    label_col = VAR_NAMES,
    group_vars = "group",
    expected_observations = "ALL"
  )))
  grouped_plot_promise <- dq_lazy_unwrap(grouped$SummaryPlot)
  expect_s3_class(grouped_plot_promise, "dq_lazy_ggplot")
  expect_false(any(startsWith(
    names(unclass(grouped_plot_promise)$env$res_df),
    ".dq_grading"
  )))
  expect_s3_class(prep_realize_ggplot(grouped$SummaryPlot), "ggplot")
  expect_null(grouped$ReportSummaryTable)

  stratified <- suppressWarnings(suppressMessages(com_segment_missingness(
    study_data = grouped_fixture$study_data,
    meta_data = grouped_fixture$meta_data,
    label_col = VAR_NAMES,
    group_vars = "group",
    strata_vars = "stratum",
    expected_observations = "ALL"
  )))
  stratified_plot_promise <- dq_lazy_unwrap(stratified$SummaryPlot)
  expect_s3_class(stratified_plot_promise, "dq_lazy_ggplot")
  expect_false(any(startsWith(
    names(unclass(stratified_plot_promise)$env$res_df),
    ".dq_grading"
  )))
  expect_s3_class(prep_realize_ggplot(stratified$SummaryPlot), "ggplot")
  expect_null(stratified$ReportSummaryTable)
})

test_that("segment missingness resolves grading colors when rendered", {
  skip_on_cran()

  fixture <- local_segment_missingness_threshold_fixture()
  local_segment_missingness_options()
  result <- suppressWarnings(suppressMessages(com_segment_missingness(
    study_data = fixture$study_data,
    meta_data = fixture$meta_data,
    label_col = VAR_NAMES,
    expected_observations = "ALL",
    exclude_roles = character()
  )))
  rst <- result$ReportSummaryTable
  expect_null(util_report_summary_table_colcode(rst))

  withr::local_options(
    dataquieR.test_segment_missingness_classes = c(1, 1)
  )
  testthat::local_mocked_bindings(
    util_metrics_to_classes = function(rs_table_long, ...) {
      rs_table_long$class <- getOption(
        "dataquieR.test_segment_missingness_classes"
      )
      rs_table_long
    }
  )

  first_plot <- print.ReportSummaryTable(rst, view = FALSE)
  first_build <- ggplot2::ggplot_build(first_plot)
  expect_true(any(vapply(first_plot$layers, function(layer) {
    inherits(layer$geom, "GeomBar")
  }, logical(1))))
  first_bar_data <- local_segment_missingness_bar_data(first_plot)
  expect_equal(first_bar_data$y, c(0.2, 0.6))
  expect_equal(
    first_bar_data$fill,
    unname(util_get_colors()[c("1", "1")])
  )
  expect_identical(
    first_build$plot$scales$get_scales("fill")$get_labels(),
    unname(util_get_labels_grading_class()[["1"]])
  )
  first_summary_plot <- prep_realize_ggplot(result$SummaryPlot)
  expect_equal(
    local_segment_missingness_bar_data(first_summary_plot)$fill,
    unname(util_get_colors()[c("1", "1")])
  )

  options(dataquieR.test_segment_missingness_classes = c(2, 1))
  second_plot <- print.ReportSummaryTable(rst, view = FALSE)
  second_build <- ggplot2::ggplot_build(second_plot)
  second_bar_data <- local_segment_missingness_bar_data(second_plot)
  expect_equal(second_bar_data$y, c(0.2, 0.6))
  expect_equal(
    second_bar_data$fill,
    unname(util_get_colors()[c("2", "1")])
  )
  expect_equal(
    second_build$plot$scales$get_scales("fill")$get_labels(),
    unname(util_get_labels_grading_class()[c("1", "2")])
  )
  second_summary_plot <- prep_realize_ggplot(result$SummaryPlot)
  expect_equal(
    local_segment_missingness_bar_data(second_summary_plot)$fill,
    unname(util_get_colors()[c("2", "1")])
  )
  expect_null(util_report_summary_table_colcode(rst))
})

test_that("segment missingness uses only segment-level grading rulesets", {
  skip_on_cran()

  fixture <- local_segment_missingness_threshold_fixture()
  fixture$meta_data[[GRADING_RULESET]] <- c(
    "item-a", "item-b", "item-c", "item-d"
  )
  segment_level <- data.frame(
    STUDY_SEGMENT = c("a", "b"),
    GRADING_RULESET = c("strict", ""),
    stringsAsFactors = FALSE
  )
  local_segment_missingness_options()

  testthat::local_mocked_bindings(
    util_metrics_to_classes = function(rs_table_long, meta_data, ...) {
      rulesets <- setNames(
        meta_data[[GRADING_RULESET]],
        meta_data[[VAR_NAMES]]
      )
      rs_table_long$class <- ifelse(
        rulesets[rs_table_long[[VAR_NAMES]]] == "strict",
        1,
        2
      )
      rs_table_long
    }
  )

  direct <- suppressWarnings(suppressMessages(com_segment_missingness(
    study_data = fixture$study_data,
    meta_data = fixture$meta_data,
    meta_data_segment = segment_level,
    label_col = VAR_NAMES,
    expected_observations = "ALL",
    exclude_roles = character()
  )))

  expect_identical(
    as.character(direct$ResultData[[GRADING_RULESET]]),
    c("strict", "0")
  )
  grading_context <- util_attr(
    direct$ReportSummaryTable,
    "grading_context",
    exact = TRUE
  )
  expect_identical(grading_context$grading_rule_sets, c("strict", "0"))
  direct_plot <- prep_realize_ggplot(direct$SummaryPlot)
  direct_plot_build <- ggplot2::ggplot_build(direct_plot)
  expect_equal(
    local_segment_missingness_bar_data(direct_plot)$fill,
    unname(util_get_colors()[c("1", "2")])
  )

  report <- suppressWarnings(suppressMessages(dq_report2(
    study_data = fixture$study_data,
    meta_data = fixture$meta_data,
    meta_data_segment = segment_level,
    label_col = VAR_NAMES,
    dimensions = "Completeness",
    filter_indicator_functions = "^com_segment_missingness$",
    cores = NULL
  )))
  expect_identical(
    as.character(report[[1L]]$ResultData[[GRADING_RULESET]]),
    c("strict", "0")
  )
})

test_that("segment grading rejects ambiguous segment-level rulesets", {
  skip_on_cran()

  segment_level <- data.frame(
    STUDY_SEGMENT = c("a", "a"),
    GRADING_RULESET = c("first", "second"),
    stringsAsFactors = FALSE
  )

  expect_error(
    util_segment_grading_rulesets("a", segment_level),
    "More than one.+GRADING_RULESET.+segment"
  )
  expect_identical(
    util_segment_grading_rulesets(
      c("a", "b"),
      data.frame(
        STUDY_SEGMENT = "a",
        GRADING_RULESET = "same",
        stringsAsFactors = FALSE
      )
    ),
    c("same", "0")
  )
})

test_that("segment missingness uses a readable focused percentage axis", {
  skip_on_cran()

  expect_equal(
    util_segment_missingness_axis_spec(c(0.0384, 0.0544, 0.1135))$limit,
    0.15
  )
  expect_equal(
    util_segment_missingness_axis_spec(c(0, 0.0005, 0.0065))$limit,
    0.01
  )
  full_axis <- util_segment_missingness_axis_spec(c(0.8, 0.9))
  expect_equal(full_axis$limit, 1)
  expect_false(full_axis$truncated)
  expect_identical(
    util_segment_missingness_axis_labels(full_axis),
    full_axis$labels
  )
  expect_false(any(grepl(
    "<b>",
    util_segment_missingness_axis_labels(full_axis, plotly = TRUE),
    fixed = TRUE
  )))

  focused_axis <- util_segment_missingness_axis_spec(c(0.05, 0.10))
  static_labels <- util_segment_missingness_axis_labels(focused_axis)
  interactive_labels <- util_segment_missingness_axis_labels(
    focused_axis,
    plotly = TRUE
  )
  expect_true(is.expression(static_labels))
  expect_match(deparse(static_labels[[length(static_labels)]]), "bold")
  expect_match(interactive_labels[[length(interactive_labels)]], "<b>")
})

test_that("segment missingness backgrounds follow grading intervals", {
  skip_on_cran()

  x <- util_new_report_summary_table(data.frame(
    Variables = c("a", "b"),
    `No. missing segments` = c(3, 6),
    N = c(100, 100),
    check.names = FALSE
  ))
  attr(x, "segment_missingness_bar") <- list(
    value_column = "No. missing segments",
    color_mode = "grading_rules"
  )
  attr(x, "grading_context") <- list(
    indicator_metric = "PCT_com_crm_mv",
    entity = "SEGMENT",
    values_raw = c(3, 6),
    var_names = c("a", "b"),
    grading_rule_sets = c("0", "0")
  )
  testthat::local_mocked_bindings(
    util_get_thresholds = function(...) {
      list(
        a = c("1" = "[0;5]", "3" = "[4;8]", "2" = "[10;20]"),
        b = c("2" = "[0;Inf]")
      )
    }
  )

  bands <- util_segment_missingness_grading_bands(x, 0.15)
  a_bands <- bands[bands$Variables == "a", , drop = FALSE]
  expect_equal(a_bands$lower, c(0, 0.04, 0.05, 0.10))
  expect_equal(a_bands$upper, c(0.04, 0.05, 0.08, 0.15))
  expect_identical(a_bands$class_label, c(
    "Ok", "Moderate", "Moderate", "Unclear"
  ))
  expect_false(any(a_bands$lower < 0.10 & a_bands$upper > 0.08))
  b_bands <- bands[bands$Variables == "b", c("lower", "upper")]
  rownames(b_bands) <- NULL
  expect_equal(b_bands, data.frame(lower = 0, upper = 0.15))

  original_hsv <- grDevices::rgb2hsv(
    grDevices::col2rgb(util_get_colors()),
    maxColorValue = 255
  )
  band_hsv <- grDevices::rgb2hsv(
    grDevices::col2rgb(util_segment_missingness_band_colors(
      util_get_colors()
    )),
    maxColorValue = 255
  )
  expect_true(all(
    band_hsv["s", , drop = TRUE] < original_hsv["s", , drop = TRUE]
  ))
  expect_true(all(band_hsv["v", , drop = TRUE] >= 0.98 - 1e-8))
})

test_that("segment missingness legends use grading-class order", {
  skip_on_cran()

  x <- util_new_report_summary_table(data.frame(
    Variables = c("critical", "ok", "moderate"),
    `No. missing segments` = c(12, 1, 6),
    N = rep(100, 3),
    check.names = FALSE
  ))
  attr(x, "segment_missingness_bar") <- list(
    value_column = "No. missing segments",
    color_mode = "grading_rules",
    colors = unname(util_get_colors()[c("5", "1", "3")]),
    class_labels = unname(
      util_get_labels_grading_class()[c("5", "1", "3")]
    )
  )

  plot <- util_plot_segment_missingness_report_summary_table(x)
  expect_identical(
    ggplot2::ggplot_build(plot)$plot$scales$get_scales("fill")$get_labels(),
    unname(util_get_labels_grading_class()[c("1", "3", "5")])
  )
  if (!is.null(util_attr(plot, "py", exact = TRUE))) {
    built_py <- plotly::plotly_build(util_attr(plot, "py", exact = TRUE))
    visible_traces <- built_py$x$data[vapply(
      built_py$x$data,
      function(trace) isTRUE(trace$showlegend),
      logical(1)
    )]
    expect_identical(
      vapply(visible_traces, function(trace) trace$name, character(1)),
      unname(util_get_labels_grading_class()[c("1", "3", "5")])
    )
  }
})

test_that("focused segment axes have an explicit end boundary", {
  skip_on_cran()

  fixture <- local_segment_missingness_threshold_fixture()
  local_segment_missingness_options()
  focused <- suppressWarnings(suppressMessages(com_segment_missingness(
    study_data = fixture$study_data,
    meta_data = fixture$meta_data,
    label_col = VAR_NAMES,
    expected_observations = "ALL",
    exclude_roles = character()
  )))
  focused_plot <- prep_realize_ggplot(focused$SummaryPlot)
  expect_true(any(vapply(focused_plot$layers, function(layer) {
    inherits(layer$geom, "GeomSegment")
  }, logical(1))))
  if (!is.null(util_attr(focused_plot, "py", exact = TRUE))) {
    focused_py <- plotly::plotly_build(util_attr(
      focused_plot,
      "py",
      exact = TRUE
    ))
    expect_length(focused_py$x$layout$shapes, 1L)
  }

  full <- util_new_report_summary_table(data.frame(
    Variables = c("a", "b"),
    `No. missing segments` = c(80, 90),
    N = c(100, 100),
    check.names = FALSE
  ))
  attr(full, "segment_missingness_bar") <- list(
    value_column = "No. missing segments",
    color_mode = "legacy_threshold",
    colors = c("#7f0000", "#7f0000"),
    class_labels = c("Critical", "Critical"),
    threshold_value = 50,
    direction = "above"
  )
  full_plot <- util_plot_segment_missingness_report_summary_table(full)
  expect_false(any(vapply(full_plot$layers, function(layer) {
    inherits(layer$geom, "GeomSegment")
  }, logical(1))))
  if (!is.null(util_attr(full_plot, "py", exact = TRUE))) {
    full_py <- plotly::plotly_build(util_attr(full_plot, "py", exact = TRUE))
    expect_length(full_py$x$layout$shapes, 0L)
  }
})

test_that("segment missingness Plotly legend shows compact class labels", {
  skip_on_cran()
  skip_if_not_installed("plotly")

  fixture <- local_segment_missingness_threshold_fixture()
  local_segment_missingness_options()
  graded <- suppressWarnings(suppressMessages(com_segment_missingness(
    study_data = fixture$study_data,
    meta_data = fixture$meta_data,
    label_col = VAR_NAMES,
    expected_observations = "ALL",
    exclude_roles = character()
  )))
  graded_plot <- prep_realize_ggplot(graded$SummaryPlot)
  graded_plot_build <- ggplot2::ggplot_build(graded_plot)
  graded_py <- util_attr(graded_plot, "py", exact = TRUE)
  graded_built_py <- plotly::plotly_build(graded_py)
  graded_visible_traces <- graded_built_py$x$data[vapply(
    graded_built_py$x$data,
    function(trace) isTRUE(trace$showlegend),
    logical(1)
  )]
  expect_identical(
    vapply(
      graded_visible_traces,
      function(trace) trace$name,
      character(1)
    ),
    "Unclear"
  )
  expect_true(graded_built_py$x$layout$showlegend)
  expect_identical(graded_built_py$x$layout$yaxis$title, "")
  expect_equal(
    graded_plot_build$plot$scales$get_scales("y")$limits,
    c(0, 0.75)
  )
  expect_equal(graded_built_py$x$layout$xaxis$range, c(0, 0.75))
  expect_equal(graded_built_py$x$layout$legend$x, 0.5)
  expect_equal(graded_built_py$x$layout$legend$y, -0.34)
  expect_identical(
    as.character(graded_built_py$x$layout$yaxis$categoryarray),
    as.character(
      graded_plot_build$layout$panel_params[[1L]]$y$get_labels()
    )
  )

  result <- suppressWarnings(suppressMessages(com_segment_missingness(
    study_data = fixture$study_data,
    meta_data = fixture$meta_data,
    label_col = VAR_NAMES,
    threshold_value = 50,
    color_gradient_direction = "above",
    expected_observations = "ALL",
    exclude_roles = character()
  )))

  py <- util_attr(result$SummaryPlot, "py", exact = TRUE)
  expect_s3_class(py, "plotly")
  built_py <- plotly::plotly_build(py)
  visible_traces <- built_py$x$data[vapply(
    built_py$x$data,
    function(trace) isTRUE(trace$showlegend),
    logical(1)
  )]
  trace_names <- vapply(
    visible_traces,
    function(trace) trace$name,
    character(1)
  )
  expect_equal(trace_names, c("Normal", "Critical"))
  expect_false(any(grepl("#|%", trace_names)))
  expect_equal(
    unlist(lapply(visible_traces, function(trace) trace$x)),
    c(0.2, 0.6)
  )
  expect_equal(
    unlist(lapply(visible_traces, function(trace) trace$text)),
    c("20.00%", "60.00%")
  )
  expect_true(all(vapply(
    visible_traces,
    function(trace) identical(trace$orientation, "h"),
    logical(1)
  )))
  expect_identical(built_py$x$layout$barmode, "overlay")
  expect_identical(built_py$x$layout$legend$orientation, "h")
  expect_identical(built_py$x$layout$yaxis$title, "")
  expect_true(built_py$x$layout$showlegend)
  expect_lt(built_py$x$layout$legend$y, 0)
  expect_gte(built_py$x$layout$margin$b, 100)
})

test_that("dq_report2 preserves effective segment missingness colors", {
  skip_on_cran()

  fixture <- local_segment_missingness_threshold_fixture()
  local_segment_missingness_options()
  report_args <- list(
    study_data = fixture$study_data,
    meta_data = fixture$meta_data,
    label_col = VAR_NAMES,
    cores = NULL,
    dimensions = "Completeness",
    filter_indicator_functions = "^com_segment_missingness$",
    filter_result_slots = c(
      "^ResultData$",
      "^ReportSummaryTable$",
      "^SummaryPlot$"
    )
  )

  graded_report <- suppressWarnings(suppressMessages(do.call(
    dq_report2,
    report_args
  )))
  graded <- graded_report[[1L]]
  graded_data <- unclass(graded)[["ResultData"]]
  expect_named(
    graded,
    c("ResultData", "ReportSummaryTable", "SummaryPlot")
  )
  expect_s3_class(graded$ReportSummaryTable, "ReportSummaryTable")
  expect_s3_class(dq_lazy_unwrap(graded$SummaryPlot), "dq_lazy_ggplot")
  expect_null(util_report_summary_table_colcode(graded$ReportSummaryTable))
  expect_false(any(c("threshold", "direction") %in% names(graded_data)))
  graded_plot <- print.ReportSummaryTable(
    graded$ReportSummaryTable,
    view = FALSE
  )
  expect_equal(
    local_segment_missingness_bar_data(graded_plot)$fill,
    rep(unname(util_get_colors()[["2"]]), 2)
  )

  report_args$specific_args <- list(
    com_segment_missingness = list(
      threshold_value = 50,
      direction = "above",
      expected_observations = "ALL",
      exclude_roles = character()
    )
  )
  legacy_report <- suppressWarnings(suppressMessages(do.call(
    dq_report2,
    report_args
  )))
  legacy <- legacy_report[[1L]]
  legacy_data <- unclass(legacy)[["ResultData"]]
  expect_named(
    legacy,
    c("ResultData", "ReportSummaryTable")
  )
  expect_s3_class(legacy$ReportSummaryTable, "ReportSummaryTable")
  expect_equal(as.numeric(legacy_data$threshold), c(50, 50))
  expect_equal(as.character(legacy_data$direction), c("above", "above"))
  legacy_plot <- print.ReportSummaryTable(
    legacy$ReportSummaryTable,
    view = FALSE
  )
  expect_equal(
    local_segment_missingness_bar_data(legacy_plot)$fill,
    c("#2166AC", "#7f0000")
  )
})

test_that(
  "com_segment_missingness falls back for missing participation metadata",
  {
    skip_on_cran()

    study_data <- data.frame(
      a = c(1, NA, 3),
      b = c(NA, NA, 2),
      c = c(1, 2, NA),
      d = c(NA, 4, NA),
      stringsAsFactors = FALSE
    )
    meta_data <- data.frame(
      VAR_NAMES = c("a", "b", "c", "d"),
      LABEL = c("a", "b", "c", "d"),
      DATA_TYPE = DATA_TYPES$FLOAT,
      SCALE_LEVEL = SCALE_LEVELS$RATIO,
      STUDY_SEGMENT = c("seg1", "seg1", "seg2", "seg2"),
      VARIABLE_ROLE = c("", "", "", ""),
      MISSING_LIST = "",
      JUMP_LIST = "",
      stringsAsFactors = FALSE
    )

    result <- suppressWarnings(suppressMessages(com_segment_missingness(
      study_data = study_data,
      meta_data = meta_data,
      label_col = VAR_NAMES,
      threshold_value = 10,
      color_gradient_direction = "above",
      expected_observations = "SEGMENT",
      exclude_roles = character()
    )))

    expect_named(
      result,
      c("ResultData", "ReportSummaryTable", "SummaryPlot")
    )
    expect_s3_class(result$ResultData, "data.frame")
    expect_s3_class(result$SummaryPlot, "ggplot")
    expect_equal(dim(result$ResultData), c(2L, 7L))
    expect_equal(
      as.character(result$ResultData$Examinations),
      c("seg1", "seg2")
    )
  }
)

test_that("com_segment_missingness handles local parameter guardrails", {
  skip_on_cran()

  fixture <- local_segment_missingness_fixture()
  local_segment_missingness_options()

  suppressWarnings(expect_error(
    com_segment_missingness(
      study_data = fixture$study_data,
      meta_data = fixture$meta_data,
      label_col = VAR_NAMES,
      threshold_value = 10,
      color_gradient_direction = "above",
      direction = "low",
      expected_observations = "SEGMENT",
      exclude_roles = FALSE
    ),
    regexp = paste(
      "Conflicting options for .+color_gradient_direction.+",
      "and .+direction.+"
    ),
    perl = TRUE
  ))

  captured <- capture_segment_missingness_warnings(
    suppressMessages(com_segment_missingness(
      study_data = fixture$study_data,
      meta_data = fixture$meta_data,
      label_col = VAR_NAMES,
      threshold_value = 10,
      color_gradient_direction = "above",
      direction = "high",
      expected_observations = "SEGMENT",
      exclude_roles = FALSE
    ))
  )
  result <- captured$value
  expect_true(any(grepl("direction.+deprecated", captured$warnings)))
  expect_s3_class(result$ResultData, "data.frame")

  captured <- capture_segment_missingness_warnings(
    suppressMessages(com_segment_missingness(
      study_data = fixture$study_data,
      meta_data = fixture$meta_data,
      label_col = VAR_NAMES,
      threshold_value = 10,
      color_gradient_direction = "above",
      direction = "sideways",
      expected_observations = "SEGMENT",
      exclude_roles = FALSE
    ))
  )
  result_invalid_direction <- captured$value
  expect_true(any(grepl("direction.+deprecated", captured$warnings)))
  expect_s3_class(result_invalid_direction$SummaryPlot, "ggplot")
})

test_that("com_segment_missingness handles local metadata fallbacks", {
  skip_on_cran()

  fixture <- local_segment_missingness_fixture()
  local_segment_missingness_options()

  expect_message2(
    result <- suppressWarnings(com_segment_missingness(
      study_data = fixture$study_data,
      meta_data = fixture$meta_data,
      label_col = VAR_NAMES,
      threshold_value = 10,
      color_gradient_direction = "above",
      expected_observations = "SEGMENT",
      exclude_roles = VARIABLE_ROLES$PROCESS
    )),
    regexp = paste(
      "VARIABLE_ROLE has not been defined in the metadata,",
      "therefore all variables within segments are used."
    )
  )
  expect_equal(
    as.character(result$ResultData$Examinations),
    c("a", "b")
  )

  meta_data_with_roles <- fixture$meta_data
  meta_data_with_roles[[VARIABLE_ROLE]] <- c(
    VARIABLE_ROLES$PROCESS,
    "",
    "",
    "",
    ""
  )
  captured <- capture_segment_missingness_warnings(
    suppressMessages(com_segment_missingness(
      study_data = fixture$study_data,
      meta_data = meta_data_with_roles,
      threshold_value = 10,
      color_gradient_direction = "above",
      expected_observations = "SEGMENT",
      exclude_roles = c(VARIABLE_ROLES$PROCESS, "unknown")
    ))
  )
  result_mixed_roles <- captured$value
  expect_true(any(grepl("Specified VARIABLE_ROLE.s.: .+unknown.+",
        captured$warnings
      )))
  expect_false("PART_a" %in% result_mixed_roles$ResultData$Examinations)

  expect_message2(
    result_strata <- suppressWarnings(com_segment_missingness(
      study_data = fixture$study_data,
      meta_data = fixture$meta_data,
      label_col = VAR_NAMES,
      strata_vars = "strata",
      threshold_value = 10,
      color_gradient_direction = "above",
      expected_observations = "SEGMENT",
      exclude_roles = FALSE
    )),
    regexp = "Some observations in strata are NA and were removed."
  )
  expect_true(all(!is.na(result_strata$ResultData$strata)))
})

test_that("com_segment_missingness works", {
  skip_on_cran()

  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  local_segment_missingness_options()
  fixture <- full_segment_missingness_fixture()
  meta_data <- fixture$meta_data
  study_data <- fixture$study_data
  expect_message2(
    r <- com_segment_missingness(study_data, meta_data,
      label_col = LABEL,
      threshold_value = NA, color_gradient_direction = "above",
      exclude_roles = VARIABLE_ROLES$PROCESS
    ),
    regexp = sprintf(
      "%s|%s",
      paste(
        "Study variables: .+ARM_CUFF_0.+,",
        ".+USR_VO2_0.+, .+USR_BP_0.+,",
        ".+EXAM_DT_0.+, .+DEV_NO_0.+,",
        ".+LAB_DT_0.+, .+USR_SOCDEM_0.+,",
        ".+INT_DT_0.+, .+QUEST_DT_0.+",
        "are not considered due to their",
        "VARIABLE_ROLE."
      ),
      paste(
        "threshold_value should be a single number between",
        "0 and 100. The invalid threshold is ignored and",
        "grading rules are used."
      )
    )
  )

  expect_message2(
    r <- com_segment_missingness(study_data, meta_data,
      label_col = LABEL,
      threshold_value = NA, color_gradient_direction = "above"
    ),
    regexp = sprintf(
      "%s|%s|%s",
      paste(
        "Study variables: .+ARM_CUFF_0.+,",
        ".+USR_VO2_0.+, .+USR_BP_0.+,",
        ".+EXAM_DT_0.+, .+DEV_NO_0.+,",
        ".+LAB_DT_0.+, .+USR_SOCDEM_0.+,",
        ".+INT_DT_0.+, .+QUEST_DT_0.+",
        "are not considered due to their",
        "VARIABLE_ROLE."
      ),
      paste(
        "threshold_value should be a single number between",
        "0 and 100. The invalid threshold is ignored and",
        "grading rules are used."
      ),
      paste(
        "Formal exclude_roles is used with default:",
        "all process variables are not included here."
      )
    )
  )

  expect_message2(
    r <- com_segment_missingness(study_data, meta_data,
      threshold_value = NA, color_gradient_direction = "above"
    ),
    regexp = sprintf(
      "%s|%s|%s",
      paste(
        "Study variables: .+v00010.+,",
        ".+v00011.+, .+v00012.+,",
        ".+v00013.+, .+v00016.+,",
        ".+v00017.+, .+v00032.+,",
        ".+v00033.+, .+v00042.+",
        "are not considered due to their",
        "VARIABLE_ROLE."
      ),
      paste(
        "threshold_value should be a single number between",
        "0 and 100. The invalid threshold is ignored and",
        "grading rules are used."
      ),
      paste(
        "Formal exclude_roles is used with default:",
        "all process variables are not included here."
      )
    )
  )

  expect_message2(
    r <- com_segment_missingness(study_data, meta_data,
      label_col = LABEL,
      threshold_value = NA, color_gradient_direction = "above",
      strata_vars = "CENTER_0"
    ),
    regexp = sprintf(
      "%s|%s|%s",
      paste(
        "Study variables: .+ARM_CUFF_0.+,",
        ".+USR_VO2_0.+, .+USR_BP_0.+,",
        ".+EXAM_DT_0.+, .+DEV_NO_0.+,",
        ".+LAB_DT_0.+, .+USR_SOCDEM_0.+,",
        ".+INT_DT_0.+, .+QUEST_DT_0.+",
        "are not considered due to their",
        "VARIABLE_ROLE."
      ),
      paste(
        "threshold_value should be a single number between",
        "0 and 100. The invalid threshold is ignored and",
        "grading rules are used."
      ),
      paste(
        "Formal exclude_roles is used with default:",
        "all process variables are not included here."
      )
    )
  )

  meta_data2 <- meta_data
  meta_data2$KEY_STUDY_SEGMENT <- NULL
  meta_data2$STUDY_SEGMENT <- NULL
  expect_error(
    suppressWarnings(suppressMessages(
      r <- com_segment_missingness(study_data, meta_data2,
        threshold_value = 10, color_gradient_direction = "above",
        exclude_roles = c(
          VARIABLE_ROLES$PROCESS,
          "invalid"
        )
      )
    )),
    regexp = paste(
      ".*Metadata do not contain",
      "the column STUDY_SEGMENT"
    ),
    perl = TRUE
  )

  meta_data2 <- meta_data
  meta_data2$LONG_LABEL <- NA
  expect_warning(
    r <- com_segment_missingness(study_data, meta_data2,
      threshold_value = 10, color_gradient_direction = "above",
      exclude_roles = c(
        VARIABLE_ROLES$PROCESS,
        "invalid"
      )
    ),
    regexp = sprintf(
      "%s|%s",
      paste(
        "Specified VARIABLE_ROLE.s.:",
        ".+invalid.+ was not found in metadata, only:",
        ".+process.+ is used."
      ),
      paste(
        "Study variables: .+v00010.+, .+v00011.+,",
        ".+v00012.+, .+v00013.+, .+v00016.+, .+v00017.+,",
        ".+v00032.+, .+v00033.+, .+v00042.+ are not",
        "considered due to their VARIABLE_ROLE."
      )
    ),
    perl = TRUE
  )

  expect_warning(
    r <- com_segment_missingness(study_data, meta_data,
      threshold_value = 10, color_gradient_direction = "above",
      exclude_roles = c(
        VARIABLE_ROLES$PROCESS,
        "invalid"
      )
    ),
    regexp = sprintf(
      "%s|%s",
      paste(
        "Specified VARIABLE_ROLE.s.:",
        ".+invalid.+ was not found in metadata, only:",
        ".+process.+ is used."
      ),
      paste(
        "Study variables: .+v00010.+, .+v00011.+,",
        ".+v00012.+, .+v00013.+, .+v00016.+, .+v00017.+,",
        ".+v00032.+, .+v00033.+, .+v00042.+ are not",
        "considered due to their VARIABLE_ROLE."
      )
    ),
    perl = TRUE
  )

  expect_warning(
    r <- com_segment_missingness(study_data, meta_data,
      label_col = LABEL,
      threshold_value = 10, color_gradient_direction = "above",
      exclude_roles = c(
        VARIABLE_ROLES$PROCESS,
        "invalid"
      )
    ),
    regexp = sprintf(
      "%s|%s",
      paste(
        "Specified VARIABLE_ROLE.s.:",
        ".+invalid.+ was not found in metadata, only:",
        ".+process.+ is used."
      ),
      paste(
        "Study variables: .+ARM_CUFF_0.+,",
        ".+USR_VO2_0.+, .+USR_BP_0.+,",
        ".+EXAM_DT_0.+, .+DEV_NO_0.+,",
        ".+LAB_DT_0.+, .+USR_SOCDEM_0.+,",
        ".+INT_DT_0.+, .+QUEST_DT_0.+",
        "are not considered due to their",
        "VARIABLE_ROLE."
      )
    ),
    perl = TRUE
  )

  expect_error(
    suppressWarnings(
      r <- com_segment_missingness(study_data, meta_data,
        label_col = LABEL,
        threshold_value = 10, color_gradient_direction = "invalid",
        exclude_roles = VARIABLE_ROLES$PROCESS
      )
    ),
    regexp = paste(
      "Parameter .+color_gradient_direction.+ should be either .+above.+ or",
      ".+below.+, but not .+invalid.+."
    ),
    perl = TRUE
  )

  expect_error(
    suppressMessages(
      r <- com_segment_missingness(study_data, meta_data,
        label_col = LABEL,
        threshold_value = 10, color_gradient_direction = 1:2,
        exclude_roles = VARIABLE_ROLES$PROCESS
      )
    ),
    regexp = paste(
      "Parameter .+color_gradient_direction.+ should be of length",
      "1, but not 2."
    ),
    perl = TRUE
  )

  expect_message2(
    r <- com_segment_missingness(study_data, meta_data,
      label_col = LABEL,
      threshold_value = 10, color_gradient_direction = "above",
      exclude_roles = VARIABLE_ROLES$PROCESS
    ),
    regexp = paste(
      "Study variables: .+ARM_CUFF_0.+,",
      ".+USR_VO2_0.+, .+USR_BP_0.+,",
      ".+EXAM_DT_0.+, .+DEV_NO_0.+,",
      ".+LAB_DT_0.+, .+USR_SOCDEM_0.+,",
      ".+INT_DT_0.+, .+QUEST_DT_0.+",
      "are not considered due to their",
      "VARIABLE_ROLE."
    )
  )
  expect_message2(
    r <- com_segment_missingness(study_data, meta_data,
      label_col = LABEL,
      threshold_value = 10, color_gradient_direction = "below",
      exclude_roles = VARIABLE_ROLES$PROCESS
    ),
    regexp = paste(
      "Study variables: .+ARM_CUFF_0.+,",
      ".+USR_VO2_0.+, .+USR_BP_0.+,",
      ".+EXAM_DT_0.+, .+DEV_NO_0.+,",
      ".+LAB_DT_0.+, .+USR_SOCDEM_0.+,",
      ".+INT_DT_0.+, .+QUEST_DT_0.+",
      "are not considered due to their",
      "VARIABLE_ROLE."
    )
  )
  expect_equal(
    length(intersect(
      names(r),
      c("ResultData", "ReportSummaryTable", "SummaryPlot")
    )), length(union(
      names(r),
      c("ResultData", "ReportSummaryTable", "SummaryPlot")
    ))
  )
  expect_true(abs(suppressWarnings(sum(as.numeric(as.matrix(
    r$ResultData
  )), na.rm = TRUE)) - 15288.63) < 2)

  skip_on_cran()
  skip_if_not_installed("vdiffr")
  expect_doppelganger2(
    "segment missingness plot ok",
    r$SummaryPlot
  )
})

test_that("com_segment_missingness skips part-var validation for ALL", {
  skip_on_cran()

  fixture <- local_segment_missingness_fixture()
  local_segment_missingness_options()

  expect_warning(
    suppressMessages(com_segment_missingness(
      study_data = fixture$study_data,
      meta_data = fixture$meta_data,
      label_col = VAR_NAMES,
      threshold_value = 10,
      color_gradient_direction = "above",
      expected_observations = "ALL",
      exclude_roles = FALSE
    )),
    NA
  )

  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")

  fixture <- full_segment_missingness_fixture()
  meta_data <- fixture$meta_data
  study_data <- fixture$study_data

  testthat::local_mocked_bindings(
    int_part_vars_structure = function(...) {
      stop("ALL must not call int_part_vars_structure()")
    }
  )

  expect_no_error(suppressMessages(com_segment_missingness(
    study_data = study_data,
    meta_data = meta_data,
    label_col = LABEL,
    threshold_value = 10,
    color_gradient_direction = "above",
    expected_observations = "ALL"
  )))
})

test_that("com_segment_missingness works w/g (group|strata)_vars", {
  skip_on_cran() # slow and not frequently used

  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  local_segment_missingness_options()
  fixture <- full_segment_missingness_fixture()
  meta_data <- fixture$meta_data
  study_data <- fixture$study_data
  expect_message2(
    {
      r1 <- com_segment_missingness(study_data, meta_data,
        strata_vars = "CENTER_0",
        threshold_value = 5,
        color_gradient_direction = "above",
        exclude_roles = VARIABLE_ROLES$PROCESS
      )
      r2 <- com_segment_missingness(study_data, meta_data,
        strata_vars = "CENTER_0",
        group_vars = "SEX_0",
        threshold_value = 5,
        color_gradient_direction = "above",
        exclude_roles = VARIABLE_ROLES$PROCESS
      )
      r3 <- com_segment_missingness(study_data, meta_data,
        group_vars = "SEX_0",
        threshold_value = 5,
        color_gradient_direction = "above",
        exclude_roles = VARIABLE_ROLES$PROCESS
      )
    },
    regexp = "Study variables: .+ are not considered due to their VARIABLE_ROLE.", # nolint: line_length_linter.
    perl = TRUE
  )
  testthat::local_edition(3)
  expect_snapshot_value(
    style = "deparse",
    r1$ResultData
  )
  expect_snapshot_value(
    style = "deparse",
    r2$ResultData
  )
  expect_snapshot_value(
    style = "deparse",
    r3$ResultData
  )
  skip_on_cran()
  skip_if_not_installed("vdiffr")
  expect_doppelganger2(
    "segment missingness plot r1 ok",
    r1$SummaryPlot
  )
  expect_doppelganger2(
    "segment missingness plot r2 ok",
    r2$SummaryPlot
  )
  expect_doppelganger2(
    "segment missingness plot r3 ok",
    r3$SummaryPlot
  )
})
