test_that(
  paste0(
    "util_as_plotly_acc_distributions lays out one plot ",
    "and muffles stack warnings"
  ),
  {
    skip_on_cran()
    skip_if_not_installed("plotly")

    plot <- ggplot2::ggplot(
      data.frame(x = c("a", "b"), y = c(1, 2)),
      ggplot2::aes(x, y)
    ) +
      ggplot2::geom_col()

    testthat::local_mocked_bindings(
      util_ggplotly = function(...) {
        warning("position_stack requires non-overlapping x intervals")
        plotly::plot_ly()
      }
    )

    converted <- expect_warning(
      util_as_plotly_acc_distributions(list(SummaryPlotList = list(plot))),
      NA
    )

    expect_s3_class(converted, "plotly")
    expect_true(any(vapply(converted$x$layoutAttrs, function(x) {
      identical(x$xaxis$tickangle, "auto")
    }, logical(1))))
  }
)

test_that("util_as_plotly_acc_distributions reports multiple plots", {
  skip_on_cran()
  skip_if_not_installed("plotly")

  seen <- new.env(parent = emptyenv())
  testthat::local_mocked_bindings(
    util_ggplotly = function(plot, ...) {
      seen$plot <- plot
      plotly::plot_ly()
    }
  )

  converted <- util_as_plotly_acc_distributions(
    list(SummaryPlotList = list("a", "b"))
  )

  expect_s3_class(converted, "plotly")
  expect_s3_class(seen$plot, "ggplot")
  expect_match(
    ggplot2::ggplot_build(seen$plot)$data[[1]]$label,
    "Internal error"
  )
})
