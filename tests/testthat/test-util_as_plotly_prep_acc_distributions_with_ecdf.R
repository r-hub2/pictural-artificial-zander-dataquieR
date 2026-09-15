test_that(
  "util_as_plotly_prep_acc_distributions_with_ecdf lays out simple plots",
  {
    skip_on_cran()
    skip_if_not_installed("plotly")

    plot <- ggplot2::ggplot(
      data.frame(x = 1:2, y = 1:2),
      ggplot2::aes(x, y)
    ) +
      ggplot2::geom_point()

    testthat::local_mocked_bindings(
      util_ggplotly = function(...) {
        plotly::plot_ly()
      }
    )

    converted <- util_as_plotly_prep_acc_distributions_with_ecdf(
      list(SummaryPlot = plot)
    )

    expect_s3_class(converted, "plotly")
    expect_true(any(vapply(converted$x$layoutAttrs, function(x) {
      identical(x$xaxis$tickangle, "auto")
    }, logical(1))))
  }
)

test_that(
  "util_as_plotly_prep_acc_distributions_with_ecdf reports multiple plots",
  {
    skip_on_cran()
    skip_if_not_installed("plotly")

    seen <- new.env(parent = emptyenv())
    testthat::local_mocked_bindings(
      util_ggplotly = function(plot, ...) {
        seen$plot <- plot
        plotly::plot_ly()
      }
    )

    converted <- util_as_plotly_prep_acc_distributions_with_ecdf(
      list(SummaryPlot = list("a", "b"))
    )

    expect_s3_class(converted, "plotly")
    expect_s3_class(seen$plot, "ggplot")
    expect_match(
      ggplot2::ggplot_build(seen$plot)$data[[1]]$label,
      "Internal error"
    )
  }
)

test_that(
  "util_as_plotly_prep_acc_distributions_with_ecdf reports patchwork size",
  {
    skip_on_cran()
    skip_if_not_installed("patchwork")
    skip_if_not_installed("plotly")

    plot <- ggplot2::ggplot(
      data.frame(x = 1:2, y = 1:2),
      ggplot2::aes(x, y)
    ) +
      ggplot2::geom_point()

    seen <- new.env(parent = emptyenv())
    testthat::local_mocked_bindings(
      util_ggplotly = function(plot, ...) {
        seen$plot <- plot
        plotly::plot_ly()
      }
    )

    converted <- util_as_plotly_prep_acc_distributions_with_ecdf(
      list(SummaryPlot = plot + plot + plot)
    )

    expect_s3_class(converted, "plotly")
    expect_s3_class(seen$plot, "ggplot")
    expect_match(
      ggplot2::ggplot_build(seen$plot)$data[[1]]$label,
      "exactly 2 patchwork panels",
      fixed = TRUE
    )
  }
)

test_that(
  "util_as_plotly_prep_acc_distributions_with_ecdf combines patchwork panels",
  {
    skip_on_cran()
    skip_if_not_installed("patchwork")
    skip_if_not_installed("plotly")

    plot <- ggplot2::ggplot(
      data.frame(x = 1:2, y = 1:2),
      ggplot2::aes(x, y)
    ) +
      ggplot2::geom_point()
    calls <- new.env(parent = emptyenv())
    calls$n <- 0L

    testthat::local_mocked_bindings(
      util_ggplotly = function(plot, ...) {
        calls$n <- calls$n + 1L
        plotly::plot_ly(x = 1:2, y = 1:2)
      }
    )

    converted <- util_as_plotly_prep_acc_distributions_with_ecdf(
      list(SummaryPlot = plot + plot)
    )

    expect_s3_class(converted, "plotly")
    expect_identical(calls$n, 2L)
    expect_identical(
      vapply(converted$x$layout$annotations, `[[`, character(1), "text"),
      c("A", "B")
    )
  }
)
