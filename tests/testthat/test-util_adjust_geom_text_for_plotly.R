test_that("util_adjust_geom_text_for_plotly moves text traces to the right", {
  skip_on_cran()
  skip_if_not_installed("plotly")

  plot <- ggplot2::ggplot(
    data.frame(
      x = 1:2,
      y = 2:3,
      label = c("first", "second")
    ),
    ggplot2::aes(x = x, y = y, label = label)
  ) +
    ggplot2::geom_text()

  adjusted <- util_adjust_geom_text_for_plotly(plotly::ggplotly(plot))

  text_traces <- vapply(
    adjusted$x$data,
    function(trace) {
      identical(trace$type, "scatter") &&
        identical(trace$mode, "text")
    },
    logical(1)
  )
  text_positions <- vapply(
    adjusted$x$data[text_traces],
    function(trace) trace$textposition,
    character(1)
  )

  expect_s3_class(adjusted, "plotly")
  expect_true(any(text_traces))
  expect_true(all(text_positions == "right"))
})

test_that("util_adjust_geom_text_for_plotly ignores incomplete traces", {
  skip_on_cran()
  skip_if_not_installed("plotly")

  plotly_text <- plotly::plotly_build(
    plotly::plot_ly(
      x = 1,
      y = 1,
      type = "scatter",
      mode = "text",
      text = "label"
    )
  )
  plotly_text$x$data[[2]] <- list(x = 2, y = 2, mode = "text")
  plotly_text$x$data[[3]] <- list(x = 3, y = 3, type = "scatter")
  styled <- new.env(parent = emptyenv())

  testthat::local_mocked_bindings(
    util_plotly_build = function(...) plotly_text
  )
  testthat::with_mocked_bindings(
    .package = "plotly",
    style = function(p, textposition, traces) {
      styled$textposition <- textposition
      styled$traces <- traces
      p
    },
    adjusted <- util_adjust_geom_text_for_plotly(plotly_text)
  )

  expect_s3_class(adjusted, "plotly")
  expect_identical(styled$textposition, "right")
  expect_identical(styled$traces, c(TRUE, FALSE, FALSE))
})

test_that("util_adjust_geom_text_for_plotly validates plotly input", {
  skip_on_cran()
  skip_if_not_installed("plotly")

  expect_error(
    util_adjust_geom_text_for_plotly(list()),
    "inherits\\(plotly, \"plotly\"\\)"
  )
})

test_that("util_adjust_geom_text_for_plotly suppresses known diagnostics", {
  skip_on_cran()
  skip_if_not_installed("plotly")

  plotly_text <- plotly::plotly_build(
    plotly::plot_ly(
      x = 1,
      y = 1,
      type = "scatter",
      mode = "text",
      text = "label"
    )
  )

  testthat::local_mocked_bindings(
    util_plotly_build = function(...) {
      warning("'bar' objects don't have these attributes: 'mode'")
      message("the mode attribute is not used for this trace")
      plotly_text
    }
  )

  expect_warning(
    expect_message(
      adjusted <- util_adjust_geom_text_for_plotly(plotly_text),
      NA
    ),
    NA
  )
  expect_s3_class(adjusted, "plotly")
  expect_identical(adjusted$x$data[[1]]$textposition, "right")
})

test_that("util_adjust_geom_text_for_plotly suppresses box diagnostics", {
  skip_on_cran()
  skip_if_not_installed("plotly")

  plotly_text <- plotly::plotly_build(
    plotly::plot_ly(
      x = 1,
      y = 1,
      type = "scatter",
      mode = "text",
      text = "label"
    )
  )

  testthat::local_mocked_bindings(
    util_plotly_build = function(...) {
      warning("'box' objects don't have these attributes: 'mode'")
      message("'box' objects don't have these attributes: 'mode'")
      plotly_text
    }
  )

  expect_warning(
    expect_message(
      adjusted <- util_adjust_geom_text_for_plotly(plotly_text),
      NA
    ),
    NA
  )
  expect_s3_class(adjusted, "plotly")
  expect_identical(adjusted$x$data[[1]]$textposition, "right")
})

test_that("util_adjust_geom_text_for_plotly suppresses style diagnostics", {
  skip_on_cran()
  skip_if_not_installed("plotly")

  plotly_text <- plotly::plotly_build(
    plotly::plot_ly(
      x = 1,
      y = 1,
      type = "scatter",
      mode = "text",
      text = "label"
    )
  )

  testthat::local_mocked_bindings(
    util_plotly_build = function(...) plotly_text
  )
  testthat::with_mocked_bindings(
    .package = "plotly",
    style = function(p, ...) {
      warning("'bar' objects don't have these attributes: 'mode'")
      message("the mode attribute is not used for this trace")
      p$x$data[[1]]$textposition <- "right"
      p
    },
    expect_warning(
      expect_message(
        adjusted <- util_adjust_geom_text_for_plotly(plotly_text),
        NA
      ),
      NA
    )
  )

  expect_s3_class(adjusted, "plotly")
  expect_identical(adjusted$x$data[[1]]$textposition, "right")
})

test_that("util_adjust_geom_text_for_plotly suppresses alternate diagnostics", {
  skip_on_cran()
  skip_if_not_installed("plotly")

  plotly_text <- plotly::plotly_build(
    plotly::plot_ly(
      x = 1,
      y = 1,
      type = "scatter",
      mode = "text",
      text = "label"
    )
  )

  testthat::local_mocked_bindings(
    util_plotly_build = function(...) {
      message("'bar' objects don't have these attributes: 'mode'")
      plotly_text
    }
  )

  expect_message(
    adjusted <- util_adjust_geom_text_for_plotly(plotly_text),
    NA
  )
  expect_identical(adjusted$x$data[[1]]$textposition, "right")

  testthat::local_mocked_bindings(
    util_plotly_build = function(...) plotly_text
  )
  testthat::with_mocked_bindings(
    .package = "plotly",
    style = function(p, ...) {
      warning("'box' objects don't have these attributes: 'mode'")
      message("'box' objects don't have these attributes: 'mode'")
      p$x$data[[1]]$textposition <- "right"
      p
    },
    expect_warning(
      expect_message(
        adjusted <- util_adjust_geom_text_for_plotly(plotly_text),
        NA
      ),
      NA
    )
  )

  expect_identical(adjusted$x$data[[1]]$textposition, "right")
})
