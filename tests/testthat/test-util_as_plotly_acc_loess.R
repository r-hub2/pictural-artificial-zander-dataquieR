test_that("util_as_plotly_acc_loess maps scale labels and title", {
  skip_on_cran()
  skip_if_not_installed("plotly")

  plot <- ggplot2::ggplot(
    data.frame(
      time = c(1, 2, 1, 2),
      value = c(1, 2, 2, 3),
      group = c("a", "a", "b", "b")
    ),
    ggplot2::aes(time, value, colour = group)
  ) +
    ggplot2::geom_line() +
    ggplot2::scale_colour_manual(
      values = c(a = "red", b = "blue"),
      labels = c(a = "Alpha", b = "Beta")
    ) +
    ggplot2::ggtitle("LOESS title")

  testthat::local_mocked_bindings(
    util_ggplotly = function(...) {
      py <- plotly::plot_ly()
      py$x$data <- list(list(name = "a"), list(name = "b"))
      py
    }
  )

  converted <- util_as_plotly_acc_loess(list(SummaryPlot = plot))

  expect_s3_class(converted, "plotly")
  expect_identical(
    vapply(converted$x$data, `[[`, character(1), "name"),
    c("Alpha", "Beta")
  )
  expect_true(any(vapply(converted$x$layoutAttrs, function(x) {
    identical(x$title, "LOESS title")
  }, logical(1))))
})

test_that("util_as_plotly_acc_loess handles list plots with subtitles", {
  skip_on_cran()
  skip_if_not_installed("plotly")

  plot <- ggplot2::ggplot(
    data.frame(
      time = c(1, 2, 1, 2),
      value = c(1, 2, 2, 3),
      group = c("a", "a", "b", "b")
    ),
    ggplot2::aes(time, value, colour = group)
  ) +
    ggplot2::geom_line() +
    ggplot2::scale_colour_manual(
      values = c(a = "red", b = "blue"),
      labels = c(a = "Alpha", b = "Beta")
    ) +
    ggplot2::labs(title = "LOESS title", subtitle = "LOESS subtitle")

  testthat::local_mocked_bindings(
    util_ggplotly = function(...) {
      py <- plotly::plot_ly()
      py$x$data <- list(list(name = "a"), list(name = "b"))
      py
    }
  )

  converted <- util_as_plotly_acc_loess(list(SummaryPlotList = list(plot)))

  expect_s3_class(converted, "plotly")
  expect_identical(
    vapply(converted$x$data, `[[`, character(1), "name"),
    c("Alpha", "Beta")
  )
  title_text <- unlist(lapply(converted$x$layoutAttrs, function(x) {
    x$title$text
  }), use.names = FALSE)
  expect_true(any(grepl("LOESS title", title_text, fixed = TRUE)))
  expect_true(any(grepl("LOESS subtitle", title_text, fixed = TRUE)))
})
