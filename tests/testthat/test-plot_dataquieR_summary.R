skip_on_cran()

test_that("plot.dataquieR_summary rejects non-html summary plot objects", {
  skip_on_cran()

  summary <- structure(
    data.frame(dummy = 1),
    class = c("dataquieR_summary", "data.frame")
  )
  attr(summary, "this") <- rlang::env(
    result = data.frame(
      VAR_NAMES = "x",
      indicator_metric = "NUM_metric",
      class = 1L,
      value = "1",
      stringsAsFactors = FALSE
    ),
    meta_data = data.frame(
      VAR_NAMES = "x",
      LABEL = "x",
      stringsAsFactors = FALSE
    ),
    rownames_of_report = "x",
    label_col = VAR_NAMES
  )

  filtered_summary <- data.frame(
    VAR_NAMES = "x",
    indicator_metric = "NUM_metric",
    class = 1L,
    value = "1",
    stringsAsFactors = FALSE
  )
  attr(filtered_summary, "rownames_of_report") <- "x"

  testthat::local_mocked_bindings(
    util_ensure_suggested = function(...) {
      TRUE
    },
    util_reclassify_dataquieR_summary = function(x) {
      x
    },
    util_filter_repsum = function(...) {
      filtered_summary
    },
    prep_render_pie_chart_from_summaryclasses_plotly = function(...) {
      list("not-html")
    }
  )

  expect_error(
    suppressWarnings(plot.dataquieR_summary(
      summary,
      dont_plot = TRUE
    )),
    "Not all summaryplots are html htmlwidgets",
    fixed = TRUE
  )
})

test_that("plot.dataquieR_summary validates unused y argument", {
  skip_on_cran()

  summary <- structure(NA, class = "dataquieR_summary")

  testthat::local_mocked_bindings(
    util_ensure_suggested = function(...) TRUE
  )

  expect_error(
    plot.dataquieR_summary(summary, y = "unused"),
    "y is not used for plotting summaries",
    fixed = TRUE
  )
})

test_that("plot.dataquieR_summary returns empty HTML for empty summaries", {
  skip_on_cran()
  skip_if_not_installed("htmltools")

  summary <- structure(NA, class = "dataquieR_summary")
  attr(summary, "this") <- rlang::env(
    result = data.frame(
      VAR_NAMES = character(),
      indicator_metric = character(),
      class = integer(),
      value = character(),
      stringsAsFactors = FALSE
    ),
    meta_data = data.frame(
      VAR_NAMES = character(),
      LABEL = character(),
      stringsAsFactors = FALSE
    ),
    rownames_of_report = character(),
    label_col = VAR_NAMES
  )
  empty_summary <- data.frame(
    VAR_NAMES = character(),
    indicator_metric = character(),
    class = integer(),
    value = character(),
    stringsAsFactors = FALSE
  )
  attr(empty_summary, "rownames_of_report") <- character()

  testthat::local_mocked_bindings(
    util_ensure_suggested = function(...) TRUE,
    util_reclassify_dataquieR_summary = function(x) x,
    util_filter_repsum = function(...) empty_summary
  )

  result <- plot.dataquieR_summary(summary, dont_plot = TRUE)

  expect_s3_class(result, "html")
  expect_identical(as.character(result), "")
})

test_that("plot.dataquieR_summary omits all-unclassified pies", {
  skip_on_cran()
  skip_if_not_installed("htmltools")

  summary <- structure(NA, class = "dataquieR_summary")
  filtered_summary <- data.frame(
    VAR_NAMES = c("group_a", "group_b"),
    indicator_metric = c("metric_a", "metric_b"),
    call_names = c("metric_call_a", "metric_call_b"),
    class = c(NA_integer_, NA_integer_),
    value = c("1", "2"),
    stringsAsFactors = FALSE
  )
  attr(filtered_summary, "rownames_of_report") <- filtered_summary$VAR_NAMES
  attr(summary, "this") <- rlang::env(
    result = filtered_summary,
    meta_data = data.frame(
      VAR_NAMES = filtered_summary$VAR_NAMES,
      LABEL = c("Group A", "Group B"),
      stringsAsFactors = FALSE
    ),
    rownames_of_report = filtered_summary$VAR_NAMES,
    label_col = LABEL
  )

  testthat::local_mocked_bindings(
    util_ensure_suggested = function(...) TRUE,
    util_reclassify_dataquieR_summary = function(x) x,
    util_filter_repsum = function(...) filtered_summary,
    prep_render_pie_chart_from_summaryclasses_ggplot2 = function(...) {
      stop("all-unclassified summary reached the pie renderer")
    }
  )

  result <- plot.dataquieR_summary(
    summary,
    vars_to_include = "variable_group",
    dont_plot = TRUE,
    disable_plotly = TRUE
  )

  expect_s3_class(result, "html")
  expect_identical(as.character(result), "")
})

test_that("plot.dataquieR_summary omits unclassified strata", {
  skip_on_cran()
  skip_if_not_installed("htmltools")

  summary <- structure(NA, class = "dataquieR_summary")
  filtered_summary <- data.frame(
    VAR_NAMES = c("x", "y", "x", "y"),
    indicator_metric = rep(c("unclassified", "classified"), each = 2L),
    class = c(NA_integer_, NA_integer_, 1L, 2L),
    value = c("1", "2", "3", "4"),
    stringsAsFactors = FALSE
  )
  attr(filtered_summary, "rownames_of_report") <- c("x", "y")
  attr(summary, "this") <- rlang::env(
    result = filtered_summary,
    meta_data = data.frame(
      VAR_NAMES = c("x", "y"),
      LABEL = c("X", "Y"),
      stringsAsFactors = FALSE
    ),
    rownames_of_report = c("x", "y"),
    label_col = LABEL
  )
  rendered <- new.env(parent = emptyenv())
  rendered$data <- NULL

  testthat::local_mocked_bindings(
    util_ensure_suggested = function(...) TRUE,
    util_reclassify_dataquieR_summary = function(x) x,
    util_filter_repsum = function(...) filtered_summary,
    prep_render_pie_chart_from_summaryclasses_ggplot2 = function(data, ...) {
      rendered$data <- data
      htmltools::span("classified pie")
    }
  )

  result <- plot.dataquieR_summary(
    summary,
    stratify_by = "indicator_metric",
    dont_plot = TRUE,
    disable_plotly = TRUE
  )

  expect_s3_class(result, "shiny.tag")
  expect_identical(unique(rendered$data$indicator_metric), "classified")
})

test_that("plot.dataquieR_summary validates requested stratification columns", {
  skip_on_cran()

  summary <- structure(NA, class = "dataquieR_summary")
  attr(summary, "this") <- rlang::env(
    result = data.frame(),
    meta_data = data.frame(VAR_NAMES = "x", LABEL = "x"),
    rownames_of_report = "x",
    label_col = VAR_NAMES
  )
  filtered_summary <- data.frame(
    VAR_NAMES = "x",
    indicator_metric = "NUM_metric",
    class = 1L,
    value = "1",
    stringsAsFactors = FALSE
  )
  attr(filtered_summary, "rownames_of_report") <- "x"

  testthat::local_mocked_bindings(
    util_ensure_suggested = function(...) TRUE,
    util_reclassify_dataquieR_summary = function(x) x,
    util_filter_repsum = function(...) filtered_summary
  )

  expect_error(
    plot.dataquieR_summary(
      summary,
      dont_plot = TRUE,
      disable_plotly = TRUE,
      stratify_by = "missing_group"
    ),
    "Cannot stratify summary by"
  )
})

test_that("plot.dataquieR_summary combines multiple html summary plots", {
  skip_on_cran()
  skip_if_not_installed("htmltools")

  summary <- structure(NA, class = "dataquieR_summary")
  attr(summary, "this") <- rlang::env(
    result = data.frame(),
    meta_data = data.frame(VAR_NAMES = "x", LABEL = "x"),
    rownames_of_report = "x",
    label_col = VAR_NAMES
  )
  filtered_summary <- data.frame(
    VAR_NAMES = "x",
    indicator_metric = "NUM_metric",
    class = 1L,
    value = "1",
    stringsAsFactors = FALSE
  )
  attr(filtered_summary, "rownames_of_report") <- "x"

  testthat::local_mocked_bindings(
    util_ensure_suggested = function(...) TRUE,
    util_reclassify_dataquieR_summary = function(x) x,
    util_filter_repsum = function(...) filtered_summary,
    prep_get_labels = function(...) "x",
    prep_render_pie_chart_from_summaryclasses_ggplot2 = function(...) {
      list(htmltools::span("one"), htmltools::span("two"))
    }
  )

  result <- suppressWarnings(plot.dataquieR_summary(
    summary,
    dont_plot = TRUE,
    disable_plotly = TRUE
  ))

  html <- paste(as.character(result), collapse = "")
  expect_s3_class(result, "shiny.tag.list")
  expect_match(html, "one")
  expect_match(html, "two")
})

test_that("plot.dataquieR_summary applies caller filters before rendering", {
  skip_on_cran()
  skip_if_not_installed("htmltools")

  summary <- structure(NA, class = "dataquieR_summary")
  attr(summary, "this") <- rlang::env(
    result = data.frame(),
    meta_data = data.frame(VAR_NAMES = c("x", "y"), LABEL = c("x", "y")),
    rownames_of_report = c("x", "y"),
    label_col = VAR_NAMES
  )
  filtered_summary <- data.frame(
    VAR_NAMES = c("x", "y"),
    indicator_metric = c("NUM_metric", "NUM_metric"),
    class = c(1L, 2L),
    value = c("keep", "drop"),
    stringsAsFactors = FALSE
  )
  attr(filtered_summary, "rownames_of_report") <- c("x", "y")
  rendered_rows <- new.env(parent = emptyenv())
  rendered_rows$n <- NA_integer_

  testthat::local_mocked_bindings(
    util_ensure_suggested = function(...) TRUE,
    util_reclassify_dataquieR_summary = function(x) x,
    util_filter_repsum = function(...) filtered_summary,
    prep_get_labels = function(...) "x",
    prep_render_pie_chart_from_summaryclasses_ggplot2 = function(tab, ...) {
      rendered_rows$n <- nrow(tab)
      htmltools::span("filtered")
    }
  )

  result <- plot.dataquieR_summary(
    summary,
    filter = value == "keep",
    dont_plot = TRUE,
    disable_plotly = TRUE
  )

  expect_s3_class(result, "shiny.tag")
  expect_identical(rendered_rows$n, 1L)
})

test_that("plot.dataquieR_summary labels variable-group summaries clearly", {
  skip_on_cran()
  skip_if_not_installed("htmltools")

  summary <- structure(NA, class = "dataquieR_summary")
  filtered_summary <- data.frame(
    VAR_NAMES = c(
      "MAHALANOBIS_RATIO_Page1",
      "MAXIMUM_LONG_STRING_all_questionnaire",
      "MISS_RESP_bogus"
    ),
    indicator_metric = c("PCT_ssc_mah", "PCT_ssc_mls", "PCT_ssc_miss"),
    class = c(1L, 5L, NA_integer_),
    value = c("0.00%", "20.00%", NA_character_),
    stringsAsFactors = FALSE
  )
  attr(filtered_summary, "rownames_of_report") <- filtered_summary$VAR_NAMES
  attr(summary, "this") <- rlang::env(
    result = filtered_summary,
    meta_data = data.frame(
      VAR_NAMES = filtered_summary$VAR_NAMES,
      LABEL = c(
        "Mahalanobis Distance_1",
        "Maximum Long String",
        "Missing responses_2"
      ),
      COMPUTED_VARIABLE_ROLE = c(
        "MAHALANOBIS_RATIO",
        "MAXIMUM_LONG_STRING",
        "MISS_RESP"
      ),
      CHECK_ID = c("2", "1", "3"),
      stringsAsFactors = FALSE
    ),
    rownames_of_report = filtered_summary$VAR_NAMES,
    label_col = VAR_NAMES
  )
  rendered <- new.env(parent = emptyenv())
  rendered$tab <- NULL

  testthat::local_mocked_bindings(
    util_ensure_suggested = function(...) TRUE,
    util_reclassify_dataquieR_summary = function(x) x,
    util_filter_repsum = function(...) filtered_summary,
    util_get_concept_info = function(x, ...) {
      if (identical(x, "ssi")) {
        return(list(
          SSI_METRICS = c(
            "MAHALANOBIS_RATIO",
            "MAXIMUM_LONG_STRING",
            "MISS_RESP"
          ),
          menu_label = c(
            "Mahalanobis distance",
            "Maximum long string",
            "Missing responses"
          )
        ))
      }
      util_get_concept_info(x, ...)
    },
    prep_render_pie_chart_from_summaryclasses_ggplot2 = function(tab, ...) {
      rendered$tab <- tab
      htmltools::span("ssi-summary")
    }
  )

  result <- plot.dataquieR_summary(
    summary,
    vars_to_include = "ssi",
    dont_plot = TRUE,
    disable_plotly = TRUE
  )

  expect_s3_class(result, "shiny.tag")
  expect_true(all(rendered$tab$X ==
        "2 of 3 computed variable-group results classified"))
  expect_true(any(grepl(
    "Mahalanobis dis...",
    rendered$tab$note
  )))
  expect_true(any(grepl(
    "Maximum long st...",
    rendered$tab$note
  )))
  expect_true(any(grepl("\\.\\.\\.", rendered$tab$note)))
  expect_false(any(grepl(
    "\\[.*_(Page1|all_questionnaire|bogus)\\]", rendered$tab$note
  )))
})

test_that("plot.dataquieR_summary counts group-metric results separately", {
  skip_on_cran()
  skip_if_not_installed("htmltools")

  summary <- structure(NA, class = "dataquieR_summary")
  filtered_summary <- data.frame(
    VAR_NAMES = c("group_a", "group_a"),
    indicator_metric = c("max_cor", "in_range"),
    call_names = c("des_scatterplot_matrix", "des_scatterplot_matrix"),
    class = c(1L, NA_integer_),
    value = c(0.8, 1),
    stringsAsFactors = FALSE
  )
  attr(filtered_summary, "rownames_of_report") <- "group_a"
  attr(summary, "this") <- rlang::env(
    result = filtered_summary,
    meta_data = data.frame(
      VAR_NAMES = "group_a",
      LABEL = "Blood-pressure association group",
      stringsAsFactors = FALSE
    ),
    rownames_of_report = "group_a",
    label_col = LABEL
  )
  rendered <- new.env(parent = emptyenv())
  rendered$tab <- NULL

  testthat::local_mocked_bindings(
    util_ensure_suggested = function(...) TRUE,
    util_reclassify_dataquieR_summary = function(x) x,
    util_filter_repsum = function(...) filtered_summary,
    prep_render_pie_chart_from_summaryclasses_ggplot2 = function(tab, ...) {
      rendered$tab <- tab
      htmltools::span("variable-group-summary")
    }
  )

  plot.dataquieR_summary(
    summary,
    vars_to_include = "variable_group",
    dont_plot = TRUE,
    disable_plotly = TRUE
  )

  expect_true(all(
    rendered$tab$X ==
      "1 of 1 available variable groups classified (1 requested)"
  ))
  expect_true(any(grepl(
    "Blood-pressure ...",
    rendered$tab$note,
    fixed = TRUE
  )))
  expect_false(any(grepl("group_a|max_cor|in_range", rendered$tab$note)))

  plot.dataquieR_summary(
    summary,
    vars_to_include = "variable_group",
    dont_plot = TRUE,
    disable_plotly = TRUE,
    summary_title = "1 contradiction check across 1 variable group",
    summary_subtitle = "checks, not observations",
    summary_unit_label = "contradiction checks"
  )
  expect_true(all(
    rendered$tab$X == "1 contradiction check across 1 variable group"
  ))
  expect_identical(
    attr(rendered$tab, "summary_subtitle", exact = TRUE),
    "checks, not observations"
  )
  expect_identical(
    attr(rendered$tab, "summary_unit_label", exact = TRUE),
    "contradiction checks"
  )
})

test_that("plot.dataquieR_summary returns empty HTML for empty hierarchy", {
  skip_on_cran()
  skip_if_not_installed("htmltools")

  summary <- structure(NA, class = "dataquieR_summary")
  filtered_summary <- data.frame(
    VAR_NAMES = character(),
    indicator_metric = character(),
    class = integer(),
    value = character(),
    stringsAsFactors = FALSE
  )
  attr(filtered_summary, "rownames_of_report") <- character()
  attr(summary, "this") <- rlang::env(
    result = filtered_summary,
    meta_data = data.frame(VAR_NAMES = "x", LABEL = "x"),
    rownames_of_report = "x",
    label_col = VAR_NAMES
  )

  testthat::local_mocked_bindings(
    util_ensure_suggested = function(...) TRUE,
    util_reclassify_dataquieR_summary = function(x) x,
    util_filter_repsum = function(...) filtered_summary
  )

  empty_result <- plot.dataquieR_summary(
    summary,
    hierarchy = "DQ_OBS",
    dont_plot = TRUE
  )

  expect_s3_class(empty_result, "html")
  expect_identical(as.character(empty_result), "")
})

test_that("plot.dataquieR_summary routes hierarchy plots through sunburst", {
  skip_on_cran()
  skip_if_not_installed("htmltools")

  summary <- structure(NA, class = "dataquieR_summary")
  filtered_summary <- data.frame(
    VAR_NAMES = "x",
    indicator_metric = "NUM_metric",
    class = 1L,
    value = "1",
    stringsAsFactors = FALSE
  )
  attr(filtered_summary, "rownames_of_report") <- "x"
  attr(summary, "this") <- rlang::env(
    result = filtered_summary,
    meta_data = data.frame(VAR_NAMES = "x", LABEL = "x"),
    rownames_of_report = "x",
    label_col = VAR_NAMES
  )

  testthat::local_mocked_bindings(
    util_ensure_suggested = function(...) TRUE,
    util_reclassify_dataquieR_summary = function(x) x,
    util_filter_repsum = function(...) filtered_summary,
    util_render_sunburst_from_summary_classes = function(rs, ...) {
      expect_identical(
        util_attr(rs, "this", exact = TRUE)$result,
        filtered_summary
      )
      htmltools::span("sunburst")
    }
  )

  sunburst <- plot.dataquieR_summary(
    summary,
    hierarchy = "DQ_OBS",
    dont_plot = TRUE
  )

  expect_s3_class(sunburst, "shiny.tag")
  expect_match(paste(as.character(sunburst), collapse = ""), "sunburst")
})

test_that("variable-group plot helpers provide readable fallbacks", {
  meta_data <- data.frame(
    VAR_NAMES = c("group_a", "group_b"),
    stringsAsFactors = FALSE
  )

  expect_identical(
    util_summary_plot_group_labels(
      c("group_a", "group_b"), meta_data, "variable_group"
    ),
    c("group_a", "group_b")
  )
  expect_identical(
    util_summary_plot_group_labels("group_a", meta_data, "item"),
    NA_character_
  )

  note_labels <- util_summary_plot_note_labels(
    var_names = "group_a",
    meta_data = meta_data,
    vars_to_include = "variable_group",
    indicator_metrics = "unknown_metric",
    call_names = "con_demo"
  )
  expect_identical(note_labels, "con_demo: group_a")

  ssi_info <- util_get_concept_info("ssi")
  expect_identical(
    util_summary_plot_variable_group_metric_label(
      ssi_info[["SSI_METRICS"]][[1]], NA_character_
    ),
    unname(ssi_info[["menu_label"]][[1]])
  )
  expect_identical(
    util_summary_plot_variable_group_metric_label(
      "custom_metric", NA_character_
    ),
    "custom metric"
  )
  expect_identical(
    util_first_non_empty_character("", "Group A", "group"),
    "Group A"
  )
})

test_that("summary concept helpers split contradiction metrics generically", {
  metrics <- c(
    "NUM_con_con",
    "PCT_con_con_contc",
    "PCT_con_con_contu",
    "PCT_con_cdtc",
    "CAT_applicability"
  )
  expect_identical(
    util_summary_metrics_in_concept(metrics, "con_con"),
    c(TRUE, TRUE, TRUE, FALSE, FALSE)
  )

  summary <- structure(NA, class = "dataquieR_summary")
  rows <- data.frame(
    VAR_NAMES = paste0("group_", seq_along(metrics)),
    indicator_metric = metrics,
    stringsAsFactors = FALSE
  )
  original_this <- rlang::env(result = rows)
  attr(summary, "this") <- original_this

  contradiction_summary <- util_summary_subset_metric_concept(
    summary, "con_con"
  )
  other_summary <- util_summary_subset_metric_concept(
    summary, "con_con", include = FALSE
  )

  expect_identical(
    util_attr(contradiction_summary, "this", exact = TRUE)$result$
      indicator_metric,
    metrics[1:3]
  )
  expect_identical(
    util_attr(other_summary, "this", exact = TRUE)$result$indicator_metric,
    metrics[4:5]
  )
  expect_identical(original_this$result, rows)
})

test_that("contradiction titles distinguish checks from variable groups", {
  results <- data.frame(
    VAR_NAMES = c("rule_a", "rule_b"),
    CHECK_ID = c("rule_a", "rule_b"),
    indicator_metric = c("PCT_con_con_contc", "PCT_con_con_contc"),
    value = c(1, 2),
    stringsAsFactors = FALSE
  )
  cross_item <- data.frame(
    CHECK_ID = c("rule_a", "rule_b"),
    VARIABLE_LIST = c("x", "y"),
    VARIABLE_LIST_ORDER = c("x | y", "y|x"),
    stringsAsFactors = FALSE
  )

  expect_identical(
    util_summary_check_count_title(
      results, cross_item, "contradiction"
    ),
    "2 contradiction checks across 1 variable group"
  )
  expect_identical(util_summary_evaluated_check_count(results), 2L)

  results[[CHECK_ID]] <- NULL
  results[[".variable_group_result_label"]] <- c("Rule A", "Rule B")
  cross_item[[CHECK_LABEL]] <- c("Rule A", "Rule B")
  expect_identical(
    util_summary_check_count_title(
      results, cross_item, "contradiction"
    ),
    "2 contradiction checks across 1 variable group"
  )
})

test_that("variable-group sunburst controls expose only useful modes", {
  empty <- htmltools::HTML("")
  other <- htmltools::HTML('<div data-chart="other"></div>')
  contradiction <- htmltools::HTML(
    '<div data-chart="contradiction"></div>'
  )
  all_results <- htmltools::HTML('<div data-chart="all"></div>')

  mixed <- as.character(util_render_variable_group_sunburst_switch(
    other_chart = other,
    contradiction_chart = contradiction,
    all_chart = all_results,
    contradiction_count = 2L,
    contradiction_notice =
      "2 contradiction checks across 1 variable group"
  ))
  expect_equal(
    lengths(regmatches(
      mixed,
      gregexpr("data-dq-sunburst-mode-button", mixed, fixed = TRUE)
    )),
    3L
  )
  expect_match(mixed, "Contradiction checks (2)", fixed = TRUE)
  expect_match(mixed, "All group-level results", fixed = TRUE)
  expect_match(
    mixed,
    "2 contradiction checks across 1 variable group available",
    fixed = TRUE
  )
  expect_false(grepl("available. available", mixed, fixed = TRUE))
  expect_match(mixed, 'aria-live="polite"', fixed = TRUE)

  contradiction_only <- as.character(
    util_render_variable_group_sunburst_switch(
      other_chart = empty,
      contradiction_chart = contradiction
    )
  )
  expect_match(contradiction_only, "Contradiction checks", fixed = TRUE)
  expect_false(grepl(
    "data-dq-sunburst-mode-button",
    contradiction_only,
    fixed = TRUE
  ))
  expect_false(grepl(
    "dq-sunburst-mode-notice",
    contradiction_only,
    fixed = TRUE
  ))
})
