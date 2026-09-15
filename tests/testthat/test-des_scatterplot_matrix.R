skip_on_cran()

test_that("des_scatterplot_matrix no-data plotly fallback prints as HTML", {
  skip_if_not_installed("plotly")

  p <- ggplot2::ggplot() +
    ggplot2::annotate(
      "text",
      x = 0,
      y = 0,
      label = "No data available to create the plot"
    )
  attr(p, "plotly") <- htmltools::div(
    htmltools::h3("No data available to create the plot")
  )

  dqr <- list(
    VariableGroupPlotList = list(all = p),
    VariableGroupTable = data.frame(
      VARIABLE_LIST = "x",
      max_cor = NA_real_
    ),
    VariableGroupData = data.frame(VARIABLE_LIST = "x")
  )
  class(dqr) <- c("dataquieR_result", "master_result", "list")
  attr(dqr, "as_plotly") <- "util_as_plotly_des_scatterplot_matrix"
  attr(dqr, "function_name") <- "des_scatterplot_matrix"
  attr(dqr, "cn") <- "des_scatterplot_matrix"
  attr(dqr, "call") <- structure(
    quote(des_scatterplot_matrix()),
    entity_name = "[ALL]",
    label_col = "LABEL"
  )
  attr(dqr, "message") <- list()
  attr(dqr, "warning") <- list()
  attr(dqr, "error") <- list()

  expect_silent(print.dataquieR_result(dqr, dont_print = TRUE))
})

test_that("des_scatterplot_matrix plotly fallback handles multiple groups", {
  skip_on_cran()
  skip_if_not_installed("plotly")

  dqr <- list(
    VariableGroupPlotList = list(
      first = ggplot2::ggplot(),
      second = ggplot2::ggplot()
    )
  )

  expect_output(
    plot <- util_as_plotly_des_scatterplot_matrix(dqr),
    "FIXME"
  )
  expect_s3_class(plot, "plotly")
})

test_that("des_scatterplot_matrix plotly uses recursive SummaryPlot", {
  skip_on_cran()
  skip_if_not_installed("plotly")

  plotly_plot <- plotly::plot_ly(x = 1, y = 1)
  summary_plot <- ggplot2::ggplot()
  attr(summary_plot, "plotly") <- plotly_plot

  plot <- util_as_plotly_des_scatterplot_matrix(list(
    SummaryPlot = summary_plot
  ))

  expect_identical(plot, plotly_plot)
})

test_that("des_scatterplot_matrix computes a local correlation group", {
  skip_on_cran()
  skip_if_not_installed("plotly")

  study_data <- data.frame(
    a = c(1, 2, 3, 4),
    b = c(1, 2, 4, 8),
    c = c(4, 3, 2, 1)
  )
  meta_data <- data.frame(
    VAR_NAMES = c("a", "b", "c"),
    LABEL = c("A", "B", "C"),
    DATA_TYPE = DATA_TYPES$FLOAT,
    SCALE_LEVEL = SCALE_LEVELS$RATIO,
    MISSING_LIST = "",
    JUMP_LIST = "",
    stringsAsFactors = FALSE
  )
  cross_item <- data.frame(
    CHECK_LABEL = "group_one",
    VARIABLE_LIST = "a | b | c",
    ASSOCIATION_METRIC = "pearson",
    ASSOCIATION_RANGE = "[-1;1]",
    stringsAsFactors = FALSE
  )

  result <- suppressMessages(suppressWarnings(des_scatterplot_matrix(
    study_data = study_data,
    meta_data = meta_data,
    meta_data_cross_item = cross_item,
    label_col = VAR_NAMES
  )))

  expect_named(result, c(
    "VariableGroupPlotList",
    "VariableGroupTable",
    "VariableGroupData"
  ))
  expect_named(result$VariableGroupPlotList, "group_one")
  expect_s3_class(result$VariableGroupPlotList[[1]], "util_pairs_ggplot_panels")
  expect_equal(
    util_attr(result$VariableGroupPlotList[[1]], "sizing_hints", exact = TRUE),
    list(figure_type_id = "pairs_plot", number_of_vars = 3L)
  )
  expect_equal(
    as.character(result$VariableGroupTable$VARIABLE_LIST),
    "a | b | c"
  )
  expect_identical(
    unname(as.character(result$VariableGroupTable[[CHECK_ID]])),
    "1"
  )
  expect_identical(
    unname(as.character(result$VariableGroupData[[CHECK_ID]])),
    "1"
  )
  expect_true(result$VariableGroupTable$in_range)
  expect_match(result$VariableGroupData$cors, "cor\\(a, b\\)")
})

test_that("des_scatterplot_matrix reports no-data groups from computation", {
  skip_on_cran()
  skip_if_not_installed("plotly")

  study_data <- data.frame(
    a = c(NA_real_, NA_real_),
    b = c(NA_real_, NA_real_)
  )
  meta_data <- data.frame(
    VAR_NAMES = c("a", "b"),
    LABEL = c("A", "B"),
    DATA_TYPE = DATA_TYPES$FLOAT,
    SCALE_LEVEL = SCALE_LEVELS$RATIO,
    MISSING_LIST = "",
    JUMP_LIST = "",
    stringsAsFactors = FALSE
  )
  cross_item <- data.frame(
    CHECK_LABEL = "empty_group",
    VARIABLE_LIST = "a | b",
    ASSOCIATION_METRIC = "spearman",
    ASSOCIATION_RANGE = "[-1;1]",
    stringsAsFactors = FALSE
  )

  result <- suppressWarnings(suppressMessages(des_scatterplot_matrix(
    study_data = study_data,
    meta_data = meta_data,
    meta_data_cross_item = cross_item,
    label_col = VAR_NAMES
  )))

  expect_named(result$VariableGroupPlotList, "empty_group")
  expect_equal(
    util_attr(result$VariableGroupPlotList[[1]], "sizing_hints", exact = TRUE),
    list(figure_type_id = "pairs_plot", number_of_vars = 0)
  )
  expect_equal(as.character(result$VariableGroupTable$cors), "|")
  expect_false(result$VariableGroupTable$in_range)
})
