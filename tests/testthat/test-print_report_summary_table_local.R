test_that("print.ReportSummaryTable handles local structural edge cases", {
  skip_on_cran()

  no_metric <- util_new_report_summary_table(data.frame(
    Variables = "SBP",
    N = 1L
  ))
  expect_error(
    print.ReportSummaryTable(no_metric, drop = NA, view = FALSE),
    "Argument drop must not contain NAs"
  )

  empty_result <- print.ReportSummaryTable(no_metric, view = FALSE)
  expect_s3_class(empty_result, "dataquieR_result")
  expect_s3_class(empty_result, "ggplot")
  expect_identical(attr(empty_result, "warning", exact = TRUE), "Empty result")
  expect_true(util_attr(empty_result, "from_ReportSummaryTable", exact = TRUE))

  skip_if_not_installed("htmltools")
  skip_if_not_installed("DT")
  empty_html <- print.ReportSummaryTable(no_metric, dt = TRUE, view = FALSE)
  expect_s3_class(empty_html, "dataquieR_result")
  expect_s3_class(empty_html, "html")
  expect_identical(attr(empty_html, "warning", exact = TRUE), "Empty result")

  empty_metric <- util_new_report_summary_table(data.frame(
    Variables = character(0),
    N = integer(0),
    Metric = numeric(0)
  ))
  empty_plot <- print.ReportSummaryTable(empty_metric, view = FALSE)
  expect_s3_class(empty_plot, "ReportSummaryTable")
  expect_equal(nrow(empty_plot), 0L)

  skip_if_not_installed("htmlwidgets")
  empty_widget <- print.ReportSummaryTable(empty_metric,
    dt = TRUE,
    view = FALSE
  )
  expect_s3_class(empty_widget, "ReportSummaryTable")

  all_missing <- util_new_report_summary_table(data.frame(
    Variables = "SBP",
    N = 1L,
    Metric = NA_real_
  ))
  all_missing_plot <- suppressWarnings(
    print.ReportSummaryTable(all_missing, view = FALSE)
  )
  expect_s3_class(all_missing_plot, "ggplot")

  infinite_bar <- util_new_report_summary_table(data.frame(
    Variables = "SBP",
    N = 1L,
    Metric = Inf
  ))
  infinite_bar <- util_set_report_summary_table_relative(infinite_bar, FALSE)
  bar_plot <- print.ReportSummaryTable(infinite_bar, view = FALSE)
  expect_s3_class(bar_plot, "ggplot")
  expect_true(util_attr(bar_plot, "from_ReportSummaryTable", exact = TRUE))
  expect_identical(
    util_attr(bar_plot, "sizing_hints", exact = TRUE)$number_of_bars,
    0
  )

  categorical <- util_new_report_summary_table(data.frame(
    Variables = c("SBP", "DBP"),
    N = c(1L, 1L),
    acc_local = c(1, 2),
    stringsAsFactors = FALSE
  ))
  categorical <- util_set_report_summary_table_continuous(categorical, FALSE)
  categorical <- util_set_report_summary_table_relative(categorical, FALSE)
  categorical <- util_set_report_summary_table_colcode(categorical,
    c("1" = "#2166AC", "2" = "#B2182B")
  )
  categorical_plot <- print.ReportSummaryTable(categorical, view = FALSE)
  expect_s3_class(categorical_plot, "ReportSummaryTable")
  expect_equal(categorical_plot$acc_local, c(1, 2))
})

test_that("print.ReportSummaryTable renders non-empty DT widgets locally", {
  skip_on_cran()
  skip_if_not_installed("DT")
  skip_if_not_installed("htmlwidgets")

  summary_table <- util_new_report_summary_table(data.frame(
    Variables = c("SBP", "DBP"),
    N = c(10L, 20L),
    int_ok = c(1, 2),
    acc_ok = c(3, 4),
    stringsAsFactors = FALSE
  ))
  summary_table <- util_set_report_summary_table_relative(
    summary_table,
    FALSE
  )
  summary_table$int_missing <- c(NA_real_, 0.5)

  widget <- print.ReportSummaryTable(
    summary_table,
    dt = TRUE,
    displayValues = TRUE,
    fillContainer = TRUE,
    view = FALSE
  )
  html <- paste(as.character(htmltools::tagList(widget)), collapse = "")

  expect_s3_class(widget, "htmlwidget")
  expect_true(util_attr(widget, "from_ReportSummaryTable", exact = TRUE))
  expect_match(html, "ReportSummaryTable", fixed = TRUE)
  expect_match(html, "matrixTable", fixed = TRUE)
  expect_match(html, "dq-matrix-fill-container", fixed = TRUE)
  expect_match(html, "int_ok", fixed = TRUE)
  expect_match(html, "acc_ok", fixed = TRUE)
})

test_that("print.ReportSummaryTable plotly bars use ggplot fill colors", {
  skip_on_cran()
  skip_if_not_installed("plotly")

  summary_table <- util_new_report_summary_table(data.frame(
    Variables = c("Segment A", "Segment B"),
    N = c(10L, 10L),
    missingness = c(2, 8),
    stringsAsFactors = FALSE
  ))
  summary_table <- util_set_report_summary_table_relative(
    summary_table,
    FALSE
  )
  summary_table <- util_set_report_summary_table_colscale(
    summary_table,
    c("#B2182B", "#92C5DE", "#2166AC")
  )

  worse_summary <- util_set_report_summary_table_higher_means(
    summary_table,
    "worse"
  )
  worse_plot <- print.ReportSummaryTable(worse_summary, view = FALSE)
  worse_marker <- util_attr(worse_plot, "py", exact = TRUE)$x$data[[1]]$marker
  worse_ggplot_fill <- ggplot2::ggplot_build(worse_plot)$data[[1]]$fill

  expect_equal(as.vector(worse_marker$color), worse_ggplot_fill)
  expect_equal(worse_marker$line$color, "white")
  expect_equal(worse_marker$line$width, 0.8)

  better_summary <- util_set_report_summary_table_higher_means(
    summary_table,
    "better"
  )
  better_plot <- print.ReportSummaryTable(better_summary, view = FALSE)
  better_marker <- util_attr(better_plot, "py", exact = TRUE)$x$data[[1]]$marker
  better_ggplot_fill <- ggplot2::ggplot_build(better_plot)$data[[1]]$fill

  expect_equal(as.vector(better_marker$color), better_ggplot_fill)
  expect_equal(better_marker$line$color, "white")
  expect_equal(better_marker$line$width, 0.8)
})

test_that(
  "print.ReportSummaryTable colors categorical DT cells by level names",
  {
    skip_on_cran()
    skip_if_not_installed("DT")
    skip_if_not_installed("htmlwidgets")

    summary_table <- util_new_report_summary_table(data.frame(
      Variables = c("SBP", "DBP"),
      N = c(10L, 20L),
      acc_code = c(1, 2),
      stringsAsFactors = FALSE
    ))
    summary_table <- util_set_report_summary_table_continuous(
      summary_table,
      FALSE
    )
    summary_table <- util_set_report_summary_table_relative(
      summary_table,
      FALSE
    )
    summary_table <- util_set_report_summary_table_colcode(summary_table,
      c(low = "#2166AC", high = "#B2182B")
    )
    summary_table <- util_set_report_summary_table_level_names(summary_table,
      c(`1` = "low", `2` = "high")
    )

    widget <- print.ReportSummaryTable(
      summary_table,
      dt = TRUE,
      displayValues = TRUE,
      view = FALSE
    )
    html <- paste(as.character(htmltools::tagList(widget)), collapse = "")

    expect_s3_class(widget, "htmlwidget")
    expect_match(html, "acc_code", fixed = TRUE)
    expect_match(html, "low", fixed = TRUE)
    expect_match(html, "high", fixed = TRUE)
  }
)

test_that("print.ReportSummaryTable colors categorical DT cells by code", {
  skip_on_cran()
  skip_if_not_installed("DT")
  skip_if_not_installed("htmlwidgets")

  summary_table <- util_new_report_summary_table(data.frame(
    Variables = c("SBP", "DBP"),
    N = c(10L, 20L),
    acc_code = c(1, 2),
    stringsAsFactors = FALSE
  ))
  summary_table <- util_set_report_summary_table_continuous(
    summary_table,
    FALSE
  )
  summary_table <- util_set_report_summary_table_relative(
    summary_table,
    FALSE
  )
  summary_table <- util_set_report_summary_table_colcode(summary_table,
    c("1" = "#2166AC", "2" = "#B2182B")
  )

  widget <- print.ReportSummaryTable(
    summary_table,
    dt = TRUE,
    displayValues = TRUE,
    view = FALSE
  )
  html <- paste(as.character(htmltools::tagList(widget)), collapse = "")

  expect_s3_class(widget, "htmlwidget")
  expect_match(html, "acc_code", fixed = TRUE)
})

test_that("droplevels.ReportSummaryTable removes empty metric columns", {
  skip_on_cran()

  summary_table <- data.frame(
    Variables = c("age", "bmi"),
    N = c(10L, 10L),
    empty_zero = c(0, 0),
    empty_na = c(NA_real_, NA_real_),
    mixed = c(0, 1),
    stringsAsFactors = FALSE
  )
  class(summary_table) <- c("ReportSummaryTable", class(summary_table))

  dropped <- droplevels.ReportSummaryTable(summary_table)

  expect_s3_class(dropped, "ReportSummaryTable")
  expect_named(dropped, c("Variables", "N", "mixed"))
  expect_identical(dropped$mixed, c(0, 1))

  expect_error(droplevels.ReportSummaryTable(data.frame(x = 1)))
})
