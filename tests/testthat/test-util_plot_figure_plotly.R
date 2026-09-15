test_that("util_plot_figure_plotly works", {
  skip_if_not_installed("plotly")
  skip_on_cran()
  # Create test ggplot
  p1 <- ggplot(mtcars) +
    geom_point(aes(mpg, disp))

  result <- util_plot_figure_plotly(p1)

  # Check that the result is a plotly object
  expect_s3_class(result, "plotly")

  # Create a test patchwork
  p2 <- ggplot(mtcars) +
    geom_boxplot(aes(gear, disp, group = gear))
  p3 <- p1 + p2

  result_2 <- util_plot_figure_plotly(p3)

  # Check that the result is an HTML plot tag
  expect_s3_class(result_2, "shiny.tag")
  expect_match(
    htmltools::renderTags(result_2)$html,
    "<img ",
    fixed = TRUE
  )
})

test_that("util_plot_figure_plotly falls back for non-interactive plots", {
  skip_on_cran()

  fallback <- structure(list(kind = "static"), class = "static_plot")
  testthat::local_mocked_bindings(
    util_plot_figure_no_plotly = function(x, sizing_hints = NULL) {
      expect_equal(x, "not a ggplot")
      expect_equal(sizing_hints, list(width = "small"))
      fallback
    }
  )

  expect_identical(
    util_plot_figure_plotly("not a ggplot",
      sizing_hints = list(width = "small")
    ),
    fallback
  )
})

test_that("util_plot_figure_plotly realizes lazy plots before fallback", {
  skip_on_cran()

  lazy_plot <- structure(list(id = "lazy"), class = "dq_lazy_ggplot")
  fallback <- structure(list(kind = "static"), class = "static_plot")
  testthat::local_mocked_bindings(
    prep_realize_ggplot = function(x) {
      expect_identical(x, lazy_plot)
      "realized plot"
    },
    util_plot_figure_no_plotly = function(x, sizing_hints = NULL) {
      expect_equal(x, "realized plot")
      expect_equal(sizing_hints, list(width = "small"))
      fallback
    }
  )

  expect_identical(
    util_plot_figure_plotly(lazy_plot,
      sizing_hints = list(width = "small")
    ),
    fallback
  )
})

test_that("util_plot_figure_plotly keeps simple ggplot titles", {
  skip_on_cran()
  skip_if_not_installed("plotly")

  plot <- ggplot(mtcars) +
    geom_point(aes(mpg, disp)) +
    ggtitle("Mileage by displacement")

  rendered <- util_plot_figure_plotly(plot)

  expect_s3_class(rendered, "plotly")
  expect_equal(rendered$x$layout$title$text, "Mileage by displacement")
})

test_that("util_plot_figure_plotly keeps ggplot subtitles", {
  skip_on_cran()
  skip_if_not_installed("plotly")

  plot <- ggplot(mtcars) +
    geom_point(aes(mpg, disp)) +
    labs(
      title = "Mileage by displacement",
      subtitle = "Small local fixture"
    )

  rendered <- util_plot_figure_plotly(plot)

  expect_s3_class(rendered, "plotly")
  expect_match(rendered$x$layout$title$text, "Mileage by displacement")
  expect_match(rendered$x$layout$title$text, "Small local fixture")
})

test_that("util_ggplotly returns text fallback when conversion fails", {
  skip_on_cran()
  skip_if_not_installed("plotly")
  withr::local_options(show.error.messages = FALSE)

  fallback <- structure(
    list(x = list(visdat = "large", attrs = "large", cur_data = "large")),
    class = "plotly"
  )
  testthat::local_mocked_bindings(
    ggplotly = function(...) {
      stop("ggplotly failed")
    },
    .package = "plotly"
  )
  testthat::local_mocked_bindings(
    util_plotly_text = function(text) {
      expect_match(text, "ggplotly failed")
      fallback
    }
  )

  result <- util_ggplotly(ggplot(mtcars) + geom_point(aes(mpg, disp)))

  expect_s3_class(result, "plotly")
  expect_null(result$x$visdat)
  expect_null(result$x$attrs)
  expect_null(result$x$cur_data)
})

test_that("util_as_plotly_from_res validates stored plotly slots", {
  skip_on_cran()

  expect_error(
    util_as_plotly_from_res(list(SummaryPlot = "not interactive")),
    "res w/o PlotlyPlot"
  )

  skip_if_not_installed("plotly")

  expect_error(
    util_as_plotly_from_res(list(PlotlyPlot = "not a plotly object")),
    "PlotlyPlot should be a plotly"
  )

  plotly_obj <- plotly::plot_ly(x = 1:2, y = 2:1)
  expect_identical(
    util_as_plotly_from_res(list(PlotlyPlot = plotly_obj)),
    plotly_obj
  )
})

test_that("util_plotly_build realizes lazy plots", {
  skip_on_cran()
  skip_if_not_installed("plotly")

  lazy_plot <- structure(list(id = "lazy"), class = "dq_lazy_ggplot")
  realized <- plotly::plot_ly(x = 1:2, y = 2:1)
  built <- structure(list(id = "built"), class = "plotly_built")
  testthat::local_mocked_bindings(
    prep_realize_ggplot = function(x) {
      expect_identical(x, lazy_plot)
      realized
    }
  )
  testthat::local_mocked_bindings(
    plotly_build = function(p, ...) {
      expect_identical(p, realized)
      built
    },
    .package = "plotly"
  )

  expect_identical(util_plotly_build(lazy_plot), built)
})
