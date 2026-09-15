skip_on_cran()

.guide_axis_params <- function(plot, aesthetic = "x") {
  guides <- plot@guides[["guides"]]
  guide <- guides[[aesthetic]]
  if (is.null(guide)) {
    return(NULL)
  }

  guide[["params"]]
}

.long_discrete_x_axis_plot <- function() {
  ggplot2::ggplot(
    data.frame(
      x = factor(sprintf("long_label_%02d_with_extra_text", seq_len(50))),
      y = 1
    ),
    ggplot2::aes(x = x, y = y)
  ) +
    ggplot2::geom_point()
}

test_that("util_gg_stagger_x_axis_labels does not dodge rotated x labels", {
  p <- .long_discrete_x_axis_plot()

  rotated <- p +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 9),
      axis.text.y = ggplot2::element_text(size = 8)
    )

  rotated_out <- util_gg_stagger_x_axis_labels(
    rotated,
    inner_width = 4,
    theme_base_size = 11
  )

  expect_null(.guide_axis_params(rotated_out))
  rotated_theme <- ggplot2::theme_get() + rotated_out$theme
  expect_equal(rotated_theme$axis.text.x$size, 9)
  expect_equal(rotated_theme$axis.text.y$size, 8)

  horizontal_out <- util_gg_stagger_x_axis_labels(
    p,
    inner_width = 4,
    theme_base_size = 11
  )

  expect_gt(.guide_axis_params(horizontal_out)[["n.dodge"]], 1)
})

test_that("util_gg_stagger_x_axis_labels does not dodge flipped discrete x labels", { # nolint: line_length_linter.
  flipped <- .long_discrete_x_axis_plot() +
    ggplot2::coord_flip()

  flipped_out <- util_gg_stagger_x_axis_labels(
    flipped,
    inner_width = 4,
    theme_base_size = 11
  )

  expect_null(.guide_axis_params(flipped_out, "x"))
  expect_null(.guide_axis_params(flipped_out, "y"))
})

test_that(
  "util_gg_stagger_x_axis_labels leaves invalid or ineligible plots unchanged",
  {
    p <- .long_discrete_x_axis_plot()

    expect_identical(
      util_gg_stagger_x_axis_labels(p, min_size = -1),
      p
    )

    continuous <- ggplot2::ggplot(
      data.frame(x = seq_len(30), y = seq_len(30)),
      ggplot2::aes(x = x, y = y)
    ) +
      ggplot2::geom_point()
    expect_identical(util_gg_stagger_x_axis_labels(continuous), continuous)

    short_discrete <- ggplot2::ggplot(
      data.frame(x = factor(letters[1:8]), y = 1),
      ggplot2::aes(x = x, y = y)
    ) +
      ggplot2::geom_point()
    expect_identical(util_gg_stagger_x_axis_labels(short_discrete),
      short_discrete)
  }
)

test_that("util_gg_stagger_x_axis_labels preserves incomplete plot objects", {
  expect_null(util_gg_stagger_x_axis_labels(NULL))

  malformed <- structure(list(), class = c("gg", "ggplot"))
  expect_identical(util_gg_stagger_x_axis_labels(malformed), malformed)

  plot <- .long_discrete_x_axis_plot()
  fake_build <- function(mode) {
    scales <- new.env(parent = emptyenv())
    scales$get_scales <- function(...) {
      if (identical(mode, "no-scale")) {
        return(NULL)
      }
      structure(list(), class = "ScaleDiscrete")
    }
    x_scale <- new.env(parent = emptyenv())
    x_scale$get_labels <- function() {
      if (identical(mode, "no-labels")) {
        return(NULL)
      }
      ""
    }
    panel_params <- if (identical(mode, "no-panel")) {
      list()
    } else {
      list(list(x = x_scale))
    }
    list(
      plot = list(scales = scales),
      layout = list(panel_params = panel_params)
    )
  }

  for (mode in c("no-scale", "no-panel", "no-labels", "empty-labels")) {
    testthat::with_mocked_bindings(
      .package = "ggplot2",
      ggplot_build = function(...) fake_build(mode),
      expect_identical(util_gg_stagger_x_axis_labels(plot), plot)
    )
  }
})

test_that("layout scaling failures warn and preserve the patchwork", {
  patch <- structure(
    list(patches = list(plots = list(), annotation = list(theme = NULL))),
    class = "patchwork"
  )
  out <- NULL

  testthat::with_mocked_bindings(
    .package = "ggplot2",
    theme = function(...) stop("synthetic layout failure"),
    expect_warning(
      out <- util_gg_stagger_x_axis_labels(patch),
      "failed; returning input unchanged"
    )
  )

  expect_identical(out, patch)
})

test_that("util_gg_stagger_x_axis_labels caps automatic x-axis dodging", {
  p <- ggplot2::ggplot(
    data.frame(
      x = factor(sprintf("very_long_label_%03d_with_extra_text", seq_len(150))),
      y = 1
    ),
    ggplot2::aes(x = x, y = y)
  ) +
    ggplot2::geom_point()

  expect_message(
    out <- util_gg_stagger_x_axis_labels(
      p,
      inner_width = 4,
      max_dodge = 2,
      theme_base_size = 11,
      verbose = TRUE
    ),
    "n_labels=150"
  )

  expect_equal(.guide_axis_params(out)[["n.dodge"]], 2)
})

test_that("util_gg_stagger_x_axis_labels scales medium label sets", {
  p <- ggplot2::ggplot(
    data.frame(
      x = factor(sprintf("label_%02d_with_text", seq_len(20))),
      y = 1
    ),
    ggplot2::aes(x = x, y = y)
  ) +
    ggplot2::geom_point()

  out <- util_gg_stagger_x_axis_labels(
    p,
    inner_width = 8,
    theme_base_size = 10
  )
  th <- ggplot2::theme_get() + out$theme

  expect_equal(th$axis.text.x$size, 9.6)
  expect_null(.guide_axis_params(out))

  no_dodge <- util_gg_stagger_x_axis_labels(
    .long_discrete_x_axis_plot(),
    inner_width = 4,
    auto_dodge = FALSE,
    theme_base_size = 11
  )
  expect_null(.guide_axis_params(no_dodge))
})

test_that("util_gg_stagger_x_axis_labels uses local theme fallbacks", {
  p <- ggplot2::ggplot(
    data.frame(
      x = factor(sprintf("medium_label_%02d", seq_len(24))),
      y = 1
    ),
    ggplot2::aes(x = x, y = y)
  ) +
    ggplot2::geom_point() +
    ggplot2::theme(text = ggplot2::element_text(size = 13))

  out <- util_gg_stagger_x_axis_labels(
    p,
    inner_width = 3.2
  )
  th <- ggplot2::theme_get() + out$theme

  expect_lt(th$axis.text.x$size, 13)
  expect_gte(th$axis.text.x$size, 4.2)
  expect_null(.guide_axis_params(out))
})

test_that("util_gg_stagger_x_axis_labels handles dense and invalid angles", {
  dense <- ggplot2::ggplot(
    data.frame(
      x = factor(sprintf("dense_label_%03d_with_long_text", seq_len(190))),
      y = 1
    ),
    ggplot2::aes(x = x, y = y)
  ) +
    ggplot2::geom_point()

  dense_out <- util_gg_stagger_x_axis_labels(
    dense,
    inner_width = 3,
    theme_base_size = 11
  )
  dense_theme <- ggplot2::theme_get() + dense_out$theme

  expect_equal(.guide_axis_params(dense_out)[["n.dodge"]], 3)
  expect_equal(dense_theme$axis.text.x$size, 4.2)

  invalid_angle_out <- util_gg_stagger_x_axis_labels(
    .long_discrete_x_axis_plot(),
    inner_width = 3,
    angle = NA_real_,
    theme_base_size = 11
  )

  expect_null(.guide_axis_params(invalid_angle_out))

  angled_out <- util_gg_stagger_x_axis_labels(
    dense,
    inner_width = 3,
    angle = 0,
    theme_base_size = 11
  )
  expect_equal(.guide_axis_params(angled_out)[["angle"]], 0)
})

test_that("util_gg_stagger_x_axis_labels recurses into patchworks", {
  skip_on_cran()
  skip_if_not_installed("patchwork")

  p <- ggplot2::ggplot(
    data.frame(
      x = factor(sprintf("patch_label_%03d_with_extra_text", seq_len(80))),
      y = 1
    ),
    ggplot2::aes(x = x, y = y)
  ) +
    ggplot2::geom_point()

  patch <- patchwork::wrap_plots(list(p, p), nrow = 1) +
    patchwork::plot_annotation(title = "Overview")

  out <- util_gg_stagger_x_axis_labels(
    patch,
    inner_width = 4,
    patchwork_widths = c(1, 2),
    patchwork_heights = c(1, 2),
    theme_base_size = 11
  )

  expect_s3_class(out, "patchwork")
  expect_equal(out$patches$annotation$theme$plot.title$size, 7.26)
  expect_gte(.guide_axis_params(out$patches$plots[[1]])[["n.dodge"]], 2)
})

test_that("util_gg_stagger_x_axis_labels normalizes patchwork weights", {
  skip_on_cran()
  skip_if_not_installed("patchwork")

  p <- ggplot2::ggplot(
    data.frame(
      x = factor(sprintf("patch_label_%03d_with_extra_text", seq_len(90))),
      y = 1
    ),
    ggplot2::aes(x = x, y = y)
  ) +
    ggplot2::geom_point()

  patch <- patchwork::wrap_plots(list(p, p, p), nrow = 1) +
    patchwork::plot_annotation(title = "Overview")

  out <- util_gg_stagger_x_axis_labels(
    patch,
    inner_width = 6,
    inner_height = 3,
    patchwork_widths = 1,
    patchwork_heights = 1,
    theme_base_size = 11
  )

  expect_s3_class(out, "patchwork")
  expect_gte(.guide_axis_params(out$patches$plots[[1]])[["n.dodge"]], 2)
})

test_that("util_gg_stagger_x_axis_labels scales empty patchwork annotations", {
  patch <- structure(
    list(patches = list(plots = list(), annotation = list(theme = NULL))),
    class = "patchwork"
  )

  out <- util_gg_stagger_x_axis_labels(
    patch,
    theme_base_size = 12
  )

  expect_s3_class(out, "patchwork")
  expect_equal(out$patches$annotation$theme$plot.title$size, 7.92)
})
