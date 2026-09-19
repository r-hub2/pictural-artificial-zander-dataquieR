test_that("pretty-print layout helpers use attributes and concept defaults", {
  skip_on_cran()

  result <- structure(list(), class = c("dataquieR_result", "list"))
  expect_identical(util_get_result_layout(result), "default")

  marked <- util_mark_result_layout(result, "2-columns-fig-left")
  expect_s3_class(marked, "dataquieR_result")
  expect_identical(util_get_result_layout(marked), "2-columns-fig-left")
  expect_error(util_mark_result_layout(result, "sideways"))

  result_from_concept <- result
  attr(result_from_concept, "function_name") <- "acc_test"
  testthat::local_mocked_bindings(
    util_get_concept_info = function(...) {
      "1-column-fig-top"
    }
  )

  expect_identical(
    util_get_result_layout(result_from_concept),
    "1-column-fig-top"
  )
})

test_that("pretty-print result wrapper keeps call and condition metadata", {
  skip_on_cran()
  skip_if_not_installed("htmltools")

  dqr <- structure(list(), class = c("dataquieR_result", "list"))
  attr(dqr, "call") <- quote(acc_test(study_data, meta_data))

  wrapped <- util_wrap_dqr_result(
    htmltools::span("inner"),
    nm = "acc_test.var1",
    popup_nm = "variable_group.12",
    dqr = dqr,
    errors = "error line",
    warnings = "warning line",
    messages = "message line",
    extra_classes = c("compact", NA_character_, "")
  )

  expect_s3_class(wrapped, "shiny.tag")
  expect_identical(
    htmltools::tagGetAttribute(wrapped, "class"),
    "dataquieR_result compact"
  )
  expect_identical(
    htmltools::tagGetAttribute(wrapped, "data-nm"),
    "acc_test.var1"
  )
  expect_identical(
    htmltools::tagGetAttribute(wrapped, "data-popup-nm"),
    "variable_group.12"
  )
  expect_match(
    htmltools::tagGetAttribute(wrapped, "data-call"),
    "acc_test\\("
  )
  expect_identical(
    htmltools::tagGetAttribute(wrapped, "data-stderr"),
    "error line warning line message line"
  )
})

test_that("pretty-print result wrapper retains literal stored calls", {
  skip_on_cran()
  skip_if_not_installed("htmltools")

  dqr <- structure(list(), class = c("dataquieR_result", "list"))
  attr(dqr, "call") <- "result restored from a checkpoint"

  wrapped <- util_wrap_dqr_result(
    htmltools::span("inner"),
    nm = "restored.result",
    dqr = dqr,
    errors = character(),
    warnings = character(),
    messages = character()
  )

  expect_identical(
    htmltools::tagGetAttribute(wrapped, "data-call"),
    "result restored from a checkpoint"
  )
  expect_identical(
    htmltools::tagGetAttribute(wrapped, "data-stderr"),
    ""
  )
})

test_that("pretty-print shows flagged data only outside a pipeline", {
  skip_on_cran()
  skip_if_not_installed("htmltools")

  dqr <- structure(
    list(
      SummaryData = data.frame(Variables = "var1", value = 1),
      FlaggedStudyData = data.frame(flagged = "var1")
    ),
    class = c("dataquieR_result", "list")
  )
  attr(dqr, "call") <- quote(acc_test(study_data, meta_data))
  testthat::local_mocked_bindings(
    util_all_ind_functions = function() "acc_test",
    util_get_concept_info = function(...) {
      data.frame(Reportoutputs = "")
    },
    util_html_table = function(tb, ...) {
      htmltools::span(paste(names(tb), collapse = "|"))
    },
    util_generate_anchor_tag = function(...) htmltools::span(),
    util_generate_anchor_link = function(...) htmltools::span(),
    util_link_result_references = identity
  )

  render <- function() {
    out <- util_pretty_print(
      dqr,
      nm = "acc_test.var1",
      is_single_var = TRUE,
      meta_data = data.frame(VAR_NAMES = "var1"),
      label_col = VAR_NAMES,
      use_plot_ly = FALSE,
      dir = tempdir()
    )
    paste(as.character(out), collapse = "")
  }

  expect_match(without_pipeline(render()), "flagged")
  expect_no_match(with_pipeline(render()), "flagged")
})

test_that("pretty-print filters result slots using concept report outputs", {
  skip_on_cran()
  skip_if_not_installed("htmltools")

  dqr <- structure(
    list(
      SummaryData = data.frame(Variables = "var1", value = 1),
      OtherData = data.frame(Variables = "var1", value = 2)
    ),
    class = c("dataquieR_result", "list")
  )
  attr(dqr, "call") <- quote(acc_test(study_data, meta_data))

  testthat::local_mocked_bindings(
    util_all_ind_functions = function() "acc_test",
    util_get_concept_info = function(...) {
      data.frame(Reportoutputs = "SummaryData(V)")
    },
    util_html_table = function(tb, ...) {
      htmltools::span(class = "mock-table", paste(names(tb), collapse = "|"))
    },
    util_generate_anchor_tag = function(...) {
      htmltools::span(class = "mock-anchor")
    },
    util_generate_anchor_link = function(...) {
      htmltools::a(href = "#mock", class = "mock-link")
    },
    util_link_result_references = identity
  )

  out <- util_pretty_print(
    dqr,
    nm = "acc_test.var1",
    is_single_var = TRUE,
    meta_data = data.frame(VAR_NAMES = "var1"),
    label_col = VAR_NAMES,
    use_plot_ly = FALSE,
    dir = tempdir()
  )

  html <- paste(as.character(out), collapse = "")
  expect_match(html, "Variables\\|value")
  expect_equal(length(gregexpr("mock-table", html, fixed = TRUE)[[1]]), 1)
})

test_that("pretty-print keeps plot names separate from group popup names", {
  skip_on_cran()
  skip_if_not_installed("htmltools")

  dqr <- structure(
    list(SummaryData = data.frame(Variables = "group-12", value = 1)),
    class = c("dataquieR_result", "list")
  )
  attr(dqr, "call") <- quote(acc_mahalanobis_ratio(study_data, meta_data))

  testthat::local_mocked_bindings(
    util_all_ind_functions = function() "acc_mahalanobis_ratio",
    util_get_concept_info = function(...) {
      data.frame(Reportoutputs = "")
    },
    util_html_table = function(...) htmltools::span(class = "mock-table"),
    util_generate_anchor_tag = function(...) htmltools::span(),
    util_generate_anchor_link = function(...) htmltools::span(),
    util_link_result_references = identity
  )

  out <- util_pretty_print(
    dqr,
    nm = "acc_mahalanobis_ratio.variable_group.12",
    is_single_var = FALSE,
    meta_data = data.frame(VAR_NAMES = "group-12"),
    label_col = VAR_NAMES,
    use_plot_ly = FALSE,
    dir = tempdir(),
    popup_nm = "variable_group.12"
  )
  html <- paste(as.character(out), collapse = "")

  expect_match(
    html,
    'data-nm="acc_mahalanobis_ratio.variable_group.12"',
    fixed = TRUE
  )
  expect_match(
    html,
    'data-popup-nm="variable_group.12"',
    fixed = TRUE
  )
})

test_that("pretty-print can filter away all configured result outputs", {
  skip_on_cran()
  skip_if_not_installed("htmltools")

  dqr <- structure(
    list(SummaryData = data.frame(Variables = "var1", value = 1)),
    class = c("dataquieR_result", "list")
  )
  attr(dqr, "call") <- quote(acc_test(study_data, meta_data))

  testthat::local_mocked_bindings(
    util_all_ind_functions = function() "acc_test",
    util_get_concept_info = function(...) {
      data.frame(Reportoutputs = "OtherData(V)")
    },
    util_message = function(...) invisible(NULL),
    util_html_table = function(...) {
      stop("filtered output should not be rendered", call. = FALSE)
    }
  )

  out <- util_pretty_print(
    dqr,
    nm = "acc_test.var1",
    is_single_var = TRUE,
    meta_data = data.frame(VAR_NAMES = "var1"),
    label_col = VAR_NAMES,
    use_plot_ly = FALSE,
    dir = tempdir()
  )

  expect_null(out)
})

test_that("pretty-print omits empty result data from report sections", {
  skip_on_cran()
  skip_if_not_installed("htmltools")

  dqr <- structure(
    list(SummaryData = data.frame(
      Variables = character(),
      value = numeric()
    )),
    class = c("dataquieR_result", "list")
  )
  attr(dqr, "call") <- quote(acc_test(study_data, meta_data))
  attr(dqr, "warning") <- list(simpleWarning("No observations remained"))

  testthat::local_mocked_bindings(
    util_all_ind_functions = function() "acc_test",
    util_get_concept_info = function(...) {
      data.frame(Reportoutputs = "")
    },
    util_generate_anchor_tag = function(...) {
      htmltools::span(class = "mock-anchor")
    },
    util_generate_anchor_link = function(...) {
      htmltools::a(href = "#mock", class = "mock-link")
    },
    util_link_result_references = identity
  )

  out <- util_pretty_print(
    dqr,
    nm = "acc_test.var1",
    is_single_var = TRUE,
    meta_data = data.frame(VAR_NAMES = "var1"),
    label_col = VAR_NAMES,
    use_plot_ly = FALSE,
    dir = tempdir()
  )
  expect_null(out)
})

test_that("pretty-print passes hidden result columns to the table renderer", {
  skip_on_cran()
  skip_if_not_installed("htmltools")

  summary_data <- data.frame(Variables = "var1", value = 1)
  attr(summary_data, "hideCols") <- "Variables"
  dqr <- structure(
    list(SummaryData = summary_data),
    class = c("dataquieR_result", "list")
  )
  attr(dqr, "call") <- quote(acc_test(study_data, meta_data))
  state <- new.env(parent = emptyenv())

  testthat::local_mocked_bindings(
    util_all_ind_functions = function() "acc_test",
    util_get_concept_info = function(...) {
      data.frame(Reportoutputs = "")
    },
    util_html_table = function(tb, hideCols, ...) {
      state$columns <- colnames(tb)
      state$hide_cols <- hideCols
      htmltools::span(class = "mock-table")
    },
    util_generate_anchor_tag = function(...) htmltools::span(),
    util_generate_anchor_link = function(...) htmltools::span(),
    util_link_result_references = identity
  )

  util_pretty_print(
    dqr,
    nm = "acc_test.var1",
    is_single_var = TRUE,
    meta_data = data.frame(VAR_NAMES = "var1"),
    label_col = VAR_NAMES,
    use_plot_ly = FALSE,
    dir = tempdir()
  )

  expect_true("Variables" %in% state$columns)
  expect_identical(state$hide_cols, "Variables")
})

test_that("pretty-print shows variable-group labels instead of identifiers", {
  skip_on_cran()
  skip_if_not_installed("htmltools")

  dqr <- structure(
    list(SummaryData = data.frame(Variables = "12", value = 1)),
    class = c("dataquieR_result", "list"),
    CHECK_ID = "12",
    CHECK_LABEL = "Blood pressure checks"
  )
  attr(dqr, "call") <- quote(des_scatterplot_matrix(study_data, meta_data))

  testthat::local_mocked_bindings(
    util_all_ind_functions = function() "variable_group",
    util_get_concept_info = function(...) {
      data.frame(Reportoutputs = "")
    },
    util_generate_anchor_tag = function(...) htmltools::a(id = "anchor"),
    util_generate_anchor_link = function(..., title = "12") {
      htmltools::a(href = "#result", title)
    },
    util_html_table = function(...) htmltools::span(class = "mock-table"),
    util_link_result_references = identity,
    util_cross_item_hrefs = function(...) "group.html#group"
  )

  out <- util_pretty_print(
    dqr,
    nm = "variable_group.12",
    is_single_var = FALSE,
    meta_data = data.frame(VAR_NAMES = "SBP_0", LABEL = "Systolic"),
    meta_data_cross_item = data.frame(
      CHECK_ID = "12",
      CHECK_LABEL = "Blood pressure checks"
    ),
    label_col = LABEL,
    use_plot_ly = FALSE,
    dir = tempdir()
  )

  html <- paste(as.character(out), collapse = "")
  expect_match(html, ">Blood pressure checks</a>", fixed = TRUE)
  expect_identical(
    lengths(regmatches(
      html,
      gregexpr(">Blood pressure checks</a>", html, fixed = TRUE)
    )),
    1L
  )
  expect_false(grepl(">12</a>", html, fixed = TRUE))
  expect_false(grepl("#result", html, fixed = TRUE))
  expect_match(html, 'id="anchor"', fixed = TRUE)
})

test_that("pretty-print summarizes one-cell report summary tables", {
  skip_on_cran()
  skip_if_not_installed("htmltools")

  make_result <- function(table) {
    dqr <- structure(
      list(ReportSummaryTable = table),
      class = c("dataquieR_result", "list")
    )
    attr(dqr, "call") <- quote(acc_test(study_data, meta_data))
    dqr
  }
  render_result <- function(dqr) {
    paste(as.character(util_pretty_print(
      dqr,
      nm = "acc_test.var1",
      is_single_var = TRUE,
      meta_data = data.frame(VAR_NAMES = "var1"),
      label_col = VAR_NAMES,
      use_plot_ly = FALSE,
      dir = tempdir()
    )), collapse = "")
  }

  counted <- util_new_report_summary_table(data.frame(
    Variables = "var1",
    N = 10L,
    metric = 2
  ))

  labelled <- util_new_report_summary_table(data.frame(
    Variables = "var1",
    N = 10L,
    metric = 2
  ))
  labelled <- util_set_report_summary_table_level_names(
    labelled,
    c(`2` = "two")
  )

  testthat::local_mocked_bindings(
    util_all_ind_functions = function() "acc_test",
    util_get_concept_info = function(...) {
      data.frame(Reportoutputs = "")
    },
    util_generate_anchor_tag = function(...) {
      htmltools::span(class = "mock-anchor")
    },
    util_generate_anchor_link = function(...) {
      htmltools::a(href = "#mock", class = "mock-link")
    },
    util_link_result_references = identity
  )

  counted_html <- render_result(make_result(counted))
  expect_match(counted_html, "metric")
  expect_match(counted_html, "20%")
  expect_match(counted_html, "out of")

  labelled_html <- render_result(make_result(labelled))
  expect_match(labelled_html, "two")

  uncounted <- structure(
    data.frame(Variables = "var1", metric = 2),
    class = c("ReportSummaryTable", "data.frame")
  )

  uncounted_html <- render_result(make_result(uncounted))
  expect_match(uncounted_html, "metric")
  expect_match(uncounted_html, "2")
  expect_false(grepl("out of", uncounted_html, fixed = TRUE))
  expect_false(grepl("%", uncounted_html, fixed = TRUE))
})

test_that("pretty-print suppresses degenerate report summary tables", {
  skip_on_cran()
  skip_if_not_installed("htmltools")

  make_result <- function(table) {
    dqr <- structure(
      list(ReportSummaryTable = table),
      class = c("dataquieR_result", "list")
    )
    attr(dqr, "call") <- quote(acc_test(study_data, meta_data))
    dqr
  }
  render_result <- function(table) {
    util_pretty_print(
      make_result(table),
      nm = "acc_test.var1",
      is_single_var = TRUE,
      meta_data = data.frame(VAR_NAMES = "var1"),
      label_col = VAR_NAMES,
      use_plot_ly = FALSE,
      dir = tempdir()
    )
  }

  testthat::local_mocked_bindings(
    util_all_ind_functions = function() "acc_test",
    util_get_concept_info = function(...) {
      data.frame(Reportoutputs = "")
    },
    util_generate_anchor_tag = function(...) {
      htmltools::span(class = "mock-anchor")
    },
    util_generate_anchor_link = function(...) {
      htmltools::a(href = "#mock", class = "mock-link")
    },
    util_link_result_references = identity
  )

  no_categories <- structure(
    data.frame(Variables = "var1"),
    class = c("ReportSummaryTable", "data.frame")
  )
  no_variables <- structure(
    data.frame(Variables = character(), metric = numeric()),
    class = c("ReportSummaryTable", "data.frame")
  )

  no_categories_html <- paste(as.character(render_result(no_categories)),
    collapse = ""
  )
  no_variables_html <- paste(as.character(render_result(no_variables)),
    collapse = ""
  )

  expect_false(grepl("dataquieR_result", no_categories_html, fixed = TRUE))
  expect_false(grepl("dataquieR_result", no_variables_html, fixed = TRUE))
  expect_false(grepl("metric", no_variables_html, fixed = TRUE))
})

test_that("pretty-print renders multiple plot-list entries separately", {
  skip_on_cran()
  skip_if_not_installed("htmltools")

  dqr <- structure(
    list(
      SummaryPlotList = list(
        first = htmltools::span(class = "mock-plot-first", "first"),
        second = htmltools::span(class = "mock-plot-second", "second")
      )
    ),
    class = c("dataquieR_result", "list")
  )
  attr(dqr, "call") <- quote(con_limit_deviations(study_data, meta_data))

  testthat::local_mocked_bindings(
    util_all_ind_functions = function() "con_limit_deviations",
    util_get_concept_info = function(...) {
      data.frame(Reportoutputs = "")
    },
    util_generate_anchor_tag = function(...) {
      htmltools::span(class = "mock-anchor")
    },
    util_generate_anchor_link = function(...) {
      htmltools::a(href = "#mock", class = "mock-link")
    },
    util_link_result_references = identity
  )

  out <- util_pretty_print(
    dqr,
    nm = "con_limit_deviations.variable_group.12",
    is_single_var = TRUE,
    meta_data = data.frame(VAR_NAMES = "var1"),
    label_col = VAR_NAMES,
    use_plot_ly = FALSE,
    dir = tempdir(),
    popup_nm = "variable_group.12"
  )

  html <- paste(as.character(out), collapse = "")
  expect_match(html, "mock-plot-first")
  expect_match(html, "mock-plot-second")
  expect_match(
    html,
    "con_limit_deviations.variable_group.12.first",
    fixed = TRUE
  )
  expect_match(
    html,
    "con_limit_deviations.variable_group.12.second",
    fixed = TRUE
  )
  expect_match(html, 'data-popup-nm="variable_group.12"', fixed = TRUE)
})

test_that("pretty-print tolerates empty plot-list slots", {
  skip_on_cran()
  skip_if_not_installed("htmltools")

  dqr <- structure(
    list(SummaryPlotList = list()),
    class = c("dataquieR_result", "list")
  )
  attr(dqr, "call") <- quote(con_limit_deviations(study_data, meta_data))

  testthat::local_mocked_bindings(
    util_all_ind_functions = function() "con_limit_deviations",
    util_get_concept_info = function(...) {
      data.frame(Reportoutputs = "")
    },
    util_generate_anchor_tag = function(...) {
      htmltools::span(class = "mock-anchor")
    },
    util_generate_anchor_link = function(...) {
      htmltools::a(href = "#mock", class = "mock-link")
    }
  )

  out <- util_pretty_print(
    dqr,
    nm = "con_limit_deviations.var1",
    is_single_var = TRUE,
    meta_data = data.frame(VAR_NAMES = "var1"),
    label_col = VAR_NAMES,
    use_plot_ly = FALSE,
    dir = tempdir()
  )

  html <- paste(as.character(out), collapse = "")
  expect_match(html, "mock-anchor")
  expect_false(grepl("mock-plot", html, fixed = TRUE))
})

test_that("pretty-print explains unsupported primary result objects", {
  skip_on_cran()
  skip_if_not_installed("htmltools")

  dqr <- structure(
    list(SummaryData = structure("opaque", class = "opaque_result")),
    class = c("dataquieR_result", "list")
  )
  attr(dqr, "call") <- quote(acc_test(study_data, meta_data))

  testthat::local_mocked_bindings(
    util_all_ind_functions = function() "acc_test",
    util_get_concept_info = function(...) {
      data.frame(Reportoutputs = "")
    },
    util_generate_anchor_tag = function(...) {
      htmltools::span(class = "mock-anchor")
    },
    util_generate_anchor_link = function(...) {
      htmltools::a(href = "#mock", class = "mock-link")
    },
    util_link_result_references = identity
  )

  out <- util_pretty_print(
    dqr,
    nm = "acc_test.var1",
    is_single_var = TRUE,
    meta_data = data.frame(VAR_NAMES = "var1"),
    label_col = VAR_NAMES,
    use_plot_ly = FALSE,
    dir = tempdir()
  )

  html <- paste(as.character(out), collapse = "")
  expect_match(html, "Cannot display objects of class")
  expect_match(html, "opaque_result")
})

test_that("pretty-print can wrap plot and data slots in two columns", {
  skip_on_cran()
  skip_if_not_installed("htmltools")

  dqr <- structure(
    list(
      PlotlyPlot = structure(list(), class = "plotly"),
      SummaryData = data.frame(Variables = "var1", value = 1)
    ),
    class = c("dataquieR_result", "list")
  )
  attr(dqr, "call") <- quote(acc_test(study_data, meta_data))
  dqr <- util_mark_result_layout(dqr, "2-columns-fig-left")

  testthat::local_mocked_bindings(
    util_all_ind_functions = function() "acc_test",
    util_get_concept_info = function(...) {
      data.frame(Reportoutputs = "")
    },
    util_fix_sizing_hints = function(dqr, x) {
      list(dqr = dqr, x = x)
    },
    util_plot_figure_no_plotly = function(x, sizing_hints = NULL) {
      htmltools::span(class = "mock-plot", "plot")
    },
    util_iframe_it_if_needed = function(it, ...) {
      it
    },
    util_html_table = function(tb, ...) {
      htmltools::span(class = "mock-table", "table")
    },
    util_generate_anchor_tag = function(...) {
      htmltools::span(class = "mock-anchor")
    },
    util_generate_anchor_link = function(...) {
      htmltools::a(href = "#mock", class = "mock-link")
    },
    util_link_result_references = identity
  )

  out <- util_pretty_print(
    dqr,
    nm = "acc_test.var1",
    is_single_var = TRUE,
    meta_data = data.frame(VAR_NAMES = "var1"),
    label_col = VAR_NAMES,
    use_plot_ly = FALSE,
    dir = tempdir()
  )

  html <- paste(as.character(out), collapse = "")
  expect_match(html, "dq-plot-table-grid")
  expect_match(html, "dq-plot-table-result")
  expect_match(html, "mock-plot")
  expect_match(html, "mock-table")
})

test_that("pretty-print renders result group headings before tables", {
  skip_on_cran()
  skip_if_not_installed("htmltools")

  dqr <- structure(
    list(
      SummaryData = data.frame(Variables = "var1", value = 1)
    ),
    class = c("dataquieR_result", "list")
  )
  attr(dqr, "call") <- quote(acc_test(study_data, meta_data))
  attr(dqr, "dq_result_title") <- "Result group title"

  testthat::local_mocked_bindings(
    util_all_ind_functions = function() "acc_test",
    util_get_concept_info = function(...) {
      data.frame(Reportoutputs = "")
    },
    util_html_table = function(tb, ...) {
      htmltools::span(class = "mock-table", "table")
    },
    util_generate_anchor_tag = function(...) {
      htmltools::span(class = "mock-anchor")
    },
    util_generate_anchor_link = function(...) {
      htmltools::a(href = "#mock", class = "mock-link")
    },
    util_link_result_references = identity
  )

  out <- util_pretty_print(
    dqr,
    nm = "acc_test.var1",
    is_single_var = TRUE,
    meta_data = data.frame(VAR_NAMES = "var1"),
    label_col = VAR_NAMES,
    use_plot_ly = FALSE,
    dir = tempdir()
  )

  html <- paste(as.character(out), collapse = "")
  expect_match(html, "<h5>Result group title</h5>", fixed = TRUE)
  expect_lt(
    regexpr("<h5>Result group title</h5>", html, fixed = TRUE)[[1]],
    regexpr("mock-table", html, fixed = TRUE)[[1]]
  )
})

test_that("pretty-print adds SSI group descriptions and local anchors", {
  skip_on_cran()
  skip_if_not_installed("htmltools")

  dqr <- structure(
    list(SummaryData = data.frame(Variables = "var1", value = 1)),
    class = c("dataquieR_result", "list")
  )
  attr(dqr, "call") <- quote(acc_test(study_data, meta_data))
  attr(dqr, "dq_result_title") <- "Result group title"
  attr(dqr, "dq_result_description") <- "Result group description"
  attr(dqr, "dq_result_anchor") <- "group.METRIC"

  testthat::local_mocked_bindings(
    util_all_ind_functions = function() "acc_test",
    util_get_concept_info = function(...) {
      data.frame(Reportoutputs = "")
    },
    util_html_table = function(tb, ...) {
      htmltools::span(class = "mock-table", "table")
    },
    util_generate_anchor_tag = function(...) {
      htmltools::span(class = "mock-anchor")
    },
    util_generate_anchor_link = function(...) {
      htmltools::a(href = "#mock", class = "mock-link")
    },
    util_link_result_references = identity
  )

  out <- util_pretty_print(
    dqr,
    nm = "acc_test.var1",
    is_single_var = TRUE,
    meta_data = data.frame(VAR_NAMES = "var1"),
    label_col = VAR_NAMES,
    use_plot_ly = FALSE,
    dir = tempdir(),
    is_ssi = TRUE
  )
  html <- paste(as.character(out), collapse = "")

  expect_match(html, 'id="group.METRIC"', fixed = TRUE)
  expect_match(html, 'href="#group.METRIC"', fixed = TRUE)
  expect_match(html, 'class="infobutton"', fixed = TRUE)
  expect_match(html, "Result group description", fixed = TRUE)
})

test_that("pretty-print explains unsupported secondary result objects", {
  skip_on_cran()
  skip_if_not_installed("htmltools")

  dqr <- structure(
    list(
      SummaryPlot = htmltools::span(class = "mock-plot", "plot"),
      SummaryData = structure("opaque", class = "opaque_secondary")
    ),
    class = c("dataquieR_result", "list")
  )
  attr(dqr, "call") <- quote(acc_test(study_data, meta_data))

  testthat::local_mocked_bindings(
    util_all_ind_functions = function() "acc_test",
    util_get_concept_info = function(...) {
      data.frame(Reportoutputs = "")
    },
    util_fix_sizing_hints = function(dqr, x) {
      list(dqr = dqr, x = x)
    },
    util_plot_figure_no_plotly = function(x, sizing_hints = NULL) {
      htmltools::span(class = "mock-plot", "plot")
    },
    util_iframe_it_if_needed = function(it, ...) {
      it
    },
    util_generate_anchor_tag = function(...) {
      htmltools::span(class = "mock-anchor")
    },
    util_generate_anchor_link = function(...) {
      htmltools::a(href = "#mock", class = "mock-link")
    }
  )

  out <- util_pretty_print(
    dqr,
    nm = "acc_test.var1",
    is_single_var = TRUE,
    meta_data = data.frame(VAR_NAMES = "var1"),
    label_col = VAR_NAMES,
    use_plot_ly = FALSE,
    dir = tempdir()
  )

  html <- paste(as.character(out), collapse = "")
  expect_match(html, "mock-plot")
  expect_match(html, "Cannot display objects of class")
  expect_match(html, "opaque_secondary")
})

test_that("pretty-print captions multi-variable pages with metadata labels", {
  skip_on_cran()
  skip_if_not_installed("htmltools")

  dqr <- structure(
    list(
      SummaryPlot = htmltools::span(class = "mock-plot", "plot"),
      SummaryData = data.frame(
        Variables = c("Displayed label", "Displayed label"),
        value = c(1, 2)
      )
    ),
    class = c("dataquieR_result", "list")
  )
  attr(dqr, "call") <- quote(acc_test(study_data, meta_data))

  testthat::local_mocked_bindings(
    util_all_ind_functions = function() "acc_test",
    util_get_concept_info = function(...) {
      data.frame(Reportoutputs = "")
    },
    util_html_table = function(tb, ...) {
      htmltools::span(
        class = "mock-table",
        paste(names(tb), collapse = "|")
      )
    },
    util_generate_anchor_tag = function(...) {
      htmltools::span(class = "mock-anchor")
    },
    util_generate_anchor_link = function(...) {
      htmltools::a(href = "#mock", class = "mock-link")
    },
    util_link_result_references = identity
  )

  out <- util_pretty_print(
    dqr,
    nm = "acc_test.Displayed label",
    is_single_var = FALSE,
    meta_data = data.frame(
      VAR_NAMES = "v1",
      LABEL = "Displayed label",
      LONG_LABEL = "Long displayed label",
      stringsAsFactors = FALSE
    ),
    label_col = LABEL,
    use_plot_ly = FALSE,
    dir = tempdir()
  )

  html <- paste(as.character(out), collapse = "")
  expect_match(html, "Displayed label")
  expect_match(html, "Long displayed label")
  expect_match(html, "title=\"v1\"", fixed = TRUE)
  expect_match(html, "mock-table")
  expect_false(grepl("Variables|value", html, fixed = TRUE))
})

test_that("pretty-print suppresses anchors for SSI result pages", {
  skip_on_cran()
  skip_if_not_installed("htmltools")

  dqr <- structure(
    list(SummaryData = data.frame(Variables = "var1", value = 1)),
    class = c("dataquieR_result", "list")
  )
  attr(dqr, "call") <- quote(acc_test(study_data, meta_data))

  testthat::local_mocked_bindings(
    util_all_ind_functions = function() "acc_test",
    util_get_concept_info = function(...) {
      data.frame(Reportoutputs = "")
    },
    util_html_table = function(tb, ...) {
      htmltools::span(class = "mock-table", "table")
    },
    util_generate_anchor_tag = function(...) {
      htmltools::span(class = "mock-anchor")
    },
    util_generate_anchor_link = function(...) {
      htmltools::a(href = "#mock", class = "mock-link")
    },
    util_link_result_references = identity
  )

  out <- util_pretty_print(
    dqr,
    nm = "acc_test.var1",
    is_single_var = TRUE,
    meta_data = data.frame(VAR_NAMES = "var1"),
    label_col = VAR_NAMES,
    use_plot_ly = FALSE,
    dir = tempdir(),
    is_ssi = TRUE
  )

  html <- paste(as.character(out), collapse = "")
  expect_match(html, "mock-table")
  expect_false(grepl("mock-anchor", html, fixed = TRUE))
  expect_false(grepl("mock-link", html, fixed = TRUE))
})

test_that("pretty-print muffles known plotly conversion chatter", {
  skip_on_cran()
  skip_if_not_installed("htmltools")

  dqr <- structure(
    list(PlotlyPlot = structure(list(), class = "plotly")),
    class = c("dataquieR_result", "list")
  )
  attr(dqr, "call") <- quote(acc_test(study_data, meta_data))
  attr(dqr, "dont_util_adjust_geom_text_for_plotly") <- TRUE

  testthat::local_mocked_bindings(
    util_all_ind_functions = function() "acc_test",
    util_get_concept_info = function(...) {
      data.frame(Reportoutputs = "")
    },
    util_fix_sizing_hints = function(dqr, x) {
      list(dqr = dqr, x = x)
    },
    util_as_plotly_from_res = function(res) {
      warning("'bar' objects don't have these attributes: 'mode'")
      message("the mode has been inferred")
      structure(list(x = list()), class = "plotly")
    },
    util_iframe_it_if_needed = function(it, ...) {
      htmltools::span(class = "mock-plotly", "plotly")
    },
    util_generate_anchor_tag = function(...) {
      htmltools::span(class = "mock-anchor")
    },
    util_generate_anchor_link = function(...) {
      htmltools::a(href = "#mock", class = "mock-link")
    }
  )

  out <- expect_no_condition(util_pretty_print(
    dqr,
    nm = "acc_test.var1",
    is_single_var = TRUE,
    meta_data = data.frame(VAR_NAMES = "var1"),
    label_col = VAR_NAMES,
    use_plot_ly = TRUE,
    dir = tempdir()
  ))

  html <- paste(as.character(out), collapse = "")
  expect_match(html, "mock-plotly")
})

test_that("plotly modebar helper appends custom buttons", {
  skip_on_cran()

  py <- list(x = list())
  buttons <- list(list(name = "first"), list(name = "second"))

  out <- util_plotly_add_modebar_buttons(py, buttons)

  expect_identical(out$x$config$modeBarButtonsToAdd, buttons)

  out <- util_plotly_add_modebar_buttons(out, list(list(name = "third")))

  expect_identical(
    vapply(
      out$x$config$modeBarButtonsToAdd,
      `[[`,
      "name",
      FUN.VALUE = character(1)
    ),
    c("first", "second", "third")
  )
})

test_that("plotly decoration is skipped outside pipeline mode", {
  skip_on_cran()

  py <- structure(list(x = list()), class = "plotly")

  expect_identical(util_decorate_plotly(py), py)
})

test_that("plotly decoration adds pipeline modebar configuration", {
  skip_on_cran()
  skip_if_not_installed("htmlwidgets")
  skip_if_not_installed("plotly")

  py <- plotly::plot_ly(
    data.frame(x = 1, y = 1),
    x = ~x,
    y = ~y,
    type = "scatter",
    mode = "markers"
  )

  decorated <- with_pipeline(util_decorate_plotly(py))

  expect_s3_class(decorated, "plotly")
  expect_true(decorated$x$config$responsive)
  expect_true(decorated$x$config$autosizable)
  expect_true(decorated$x$config$fillFrame)
  expect_false(decorated$x$config$displaylogo)
  expect_identical(decorated$x$config$modeBarButtonsToRemove, list("toImage"))
  expect_identical(
    vapply(
      tail(decorated$x$config$modeBarButtonsToAdd, 4),
      `[[`,
      "name",
      FUN.VALUE = character(1)
    ),
    c(
      "Save as image",
      "Restore initial Size",
      "Print plot",
      "Download as PDF"
    )
  )
})
