test_that("util_as_plotly_acc_margins converts simple ggplot margin plots", {
  skip_on_cran()
  skip_if_not_installed("plotly")

  plot <- ggplot2::ggplot(
    data.frame(group = c("a", "b"), estimate = c(1, 2)),
    ggplot2::aes(group, estimate)
  ) +
    ggplot2::geom_point()

  res <- util_attach_attr(
    list(SummaryPlot = plot),
    sizing_hints = list(
      figure_type_id = "marg_plot",
      n_groups = 2L,
      no_char_x = 1L,
      no_char_y = 1L,
      type_plot = "point_plot"
    )
  )

  converted <- util_as_plotly_acc_margins(res)

  expect_s3_class(converted, "plotly")
  expect_true(length(converted$x$data) >= 1)
})

test_that("util_as_plotly_acc_margins combines patchwork children", {
  skip_on_cran()
  skip_if_not_installed("patchwork")
  skip_if_not_installed("plotly")

  distribution_plot <- ggplot2::ggplot(
    data.frame(
      group = rep(c("a", "b"), each = 3),
      estimate = c(1, 2, 3, 2, 3, 4)
    ),
    ggplot2::aes(group, estimate)
  ) +
    ggplot2::geom_boxplot() +
    ggplot2::geom_point()
  summary_plot <- ggplot2::ggplot(
    data.frame(group = c("a", "b"), estimate = c(2, 3)),
    ggplot2::aes(group, estimate)
  ) +
    ggplot2::geom_point()
  patchwork_plot <- (distribution_plot + summary_plot) +
    patchwork::plot_layout(widths = c(2, 1), nrow = 1) +
    patchwork::plot_annotation(
      title = "Margins",
      subtitle = "Test",
      caption = "note"
    )

  converted <- util_as_plotly_acc_margins(list(SummaryPlot = patchwork_plot))

  expect_s3_class(converted, "plotly")
  expect_gte(length(converted$x$data), 3)
})

test_that("util_as_plotly_acc_margins accepts one-child patchwork plots", {
  skip_on_cran()
  skip_if_not_installed("patchwork")
  skip_if_not_installed("plotly")

  child_plot <- ggplot2::ggplot(
    data.frame(
      group = rep(c("a", "b"), each = 3),
      estimate = c(1, 2, 3, 2, 3, 4)
    ),
    ggplot2::aes(group, estimate)
  ) +
    ggplot2::geom_boxplot() +
    ggplot2::geom_point()
  patchwork_plot <- child_plot +
    patchwork::plot_layout(widths = 1, nrow = 1) +
    patchwork::plot_annotation(
      title = "Margins",
      subtitle = "Test",
      caption = "note"
    )

  converted <- util_as_plotly_acc_margins(list(SummaryPlot = patchwork_plot))

  expect_s3_class(converted, "plotly")
  expect_gte(length(converted$x$data), 2)
})

test_that("util_as_plotly_acc_margins accepts a one-trace first child", {
  skip_on_cran()
  skip_if_not_installed("patchwork")
  skip_if_not_installed("plotly")

  first_child <- ggplot2::ggplot(
    data.frame(group = rep(c("a", "b"), each = 3), value = 1:6),
    ggplot2::aes(group, value)
  ) +
    ggplot2::geom_boxplot()
  second_child <- ggplot2::ggplot(
    data.frame(group = c("a", "b"), value = c(2, 3)),
    ggplot2::aes(group, value)
  ) +
    ggplot2::geom_point()
  patchwork_plot <- first_child + second_child +
    patchwork::plot_layout(widths = c(2, 1), nrow = 1)

  converted <- util_as_plotly_acc_margins(list(SummaryPlot = patchwork_plot))

  expect_s3_class(converted, "plotly")
  expect_gte(length(converted$x$data), 2)
})

test_that(
  paste0(
    "util_as_plotly_acc_margins returns diagnostic text ",
    "for patchwork conversion errors"
  ),
  {
    skip_on_cran()
    skip_if_not_installed("patchwork")
    skip_if_not_installed("plotly")

    plot <- ggplot2::ggplot(
      data.frame(group = c("a", "b"), estimate = c(1, 2)),
      ggplot2::aes(group, estimate)
    ) +
      ggplot2::geom_point()

    res <- list(SummaryPlot = plot + plot)

    testthat::local_mocked_bindings(
      util_ggplotly = function(...) {
        structure(
          "mock plotly conversion failed",
          class = "try-error",
          condition = simpleError("mock plotly conversion failed")
        )
      }
    )

    converted <- util_as_plotly_acc_margins(res)

    expect_s3_class(converted, "plotly")
    expect_true(any(grepl("mock plotly conversion failed",
          unlist(converted$x$attrs),
          fixed = TRUE
        )))
  }
)

test_that(
  paste0(
    "util_as_plotly_acc_margins returns diagnostic text ",
    "for second patchwork conversion errors"
  ),
  {
    skip_on_cran()
    skip_if_not_installed("patchwork")
    skip_if_not_installed("plotly")

    plot <- ggplot2::ggplot(
      data.frame(group = c("a", "b"), estimate = c(1, 2)),
      ggplot2::aes(group, estimate)
    ) +
      ggplot2::geom_point()

    res <- list(SummaryPlot = plot + plot)
    calls <- new.env(parent = emptyenv())
    calls$n <- 0L

    testthat::local_mocked_bindings(
      util_ggplotly = function(...) {
        calls$n <- calls$n + 1L
        if (identical(calls$n, 1L)) {
          structure(
            list(x = list(data = list(
              list(mode = "markers"),
              list(mode = "markers")
            ))),
            class = "plotly"
          )
        } else {
          structure(
            "mock second plotly conversion failed",
            class = "try-error",
            condition = simpleError("mock second plotly conversion failed")
          )
        }
      }
    )

    converted <- util_as_plotly_acc_margins(res)

    expect_s3_class(converted, "plotly")
    expect_true(any(grepl("mock second plotly conversion failed",
          unlist(converted$x$attrs),
          fixed = TRUE
        )))
  }
)
