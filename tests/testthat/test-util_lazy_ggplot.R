skip_on_cran()

test_that("lazy ggplots materialize on demand and use the cache option", {
  skip_on_cran()
  skip_if_not_installed("ggplot2")

  old_compat <- getOption("dataquieR.lazy_plots_gg_compatibility")
  old_cache <- getOption("dataquieR.lazy_plots_cache")
  on.exit(options(
    dataquieR.lazy_plots_gg_compatibility = old_compat,
    dataquieR.lazy_plots_cache = old_cache
  ), add = TRUE)
  options(
    dataquieR.lazy_plots_gg_compatibility = FALSE,
    dataquieR.lazy_plots_cache = TRUE
  )
  util_forget_lazy_ggplots()

  counter <- new.env(parent = emptyenv())
  counter$n <- 0L
  env <- new.env(parent = baseenv())
  env$counter <- counter
  env$data <- data.frame(x = 1:2, y = 3:4)
  env$ggplot2 <- asNamespace("ggplot2")

  lazy <- dq_lazy_ggplot(
    quote({
      counter$n <- counter$n + 1L
      ggplot2$ggplot(data, ggplot2$aes(x, y)) + ggplot2$geom_point()
    }),
    env = env,
    id = "cache-test"
  )

  expect_s3_class(lazy, "dq_lazy_ggplot")
  expect_s3_class(prep_realize_ggplot(lazy), "ggplot")
  expect_s3_class(prep_realize_ggplot(lazy), "ggplot")
  expect_identical(counter$n, 1L)

  util_forget_lazy_ggplots()
  options(dataquieR.lazy_plots_cache = FALSE)
  expect_s3_class(prep_realize_ggplot(lazy), "ggplot")
  expect_s3_class(prep_realize_ggplot(lazy), "ggplot")
  expect_identical(counter$n, 3L)
})

test_that("lazy ggplot generated ids are stable opaque identifiers", {
  id1 <- util_lazy_ggplot_next_id()
  id2 <- util_lazy_ggplot_next_id()

  expect_match(id1, "^dq_lazy_[0-9a-f]{8}$")
  expect_match(id2, "^dq_lazy_[0-9a-f]{8}$")
  expect_false(identical(id1, id2))
})

test_that("lazy ggplot helpers delegate to materialized ggplots", {
  skip_on_cran()
  skip_if_not_installed("ggplot2")

  old_compat <- getOption("dataquieR.lazy_plots_gg_compatibility")
  old_cache <- getOption("dataquieR.lazy_plots_cache")
  on.exit(options(
    dataquieR.lazy_plots_gg_compatibility = old_compat,
    dataquieR.lazy_plots_cache = old_cache
  ), add = TRUE)
  options(
    dataquieR.lazy_plots_gg_compatibility = FALSE,
    dataquieR.lazy_plots_cache = TRUE
  )

  base_plot <- ggplot2::ggplot(data.frame(x = 1:2, y = 3:4), ggplot2::aes(x, y))
  lazy <- util_create_lean_ggplot(
    {
      base_plot + ggplot2::geom_point()
    },
    base_plot = base_plot,
    .lazy = TRUE
  )

  expect_s3_class(util_realize_if_lazy(lazy), "ggplot")
  expect_identical(util_realize_if_lazy("plain"), "plain")
  expect_s3_class(ggplot2::ggplot_build(lazy), "ggplot_built")
  expect_s3_class(ggplot2::ggplotGrob(lazy), "gtable")
  expect_s3_class(as_grob.dq_lazy_ggplot(lazy), "gtable")
  expect_s3_class(lazy$data, "data.frame")
  expect_type(lazy[["layers"]], "list")
  expect_length(lazy[["layers"]], 1L)
})

test_that("lazy ggplot plotly methods delegate to materialized ggplots", {
  skip_on_cran()
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("plotly")

  old_compat <- getOption("dataquieR.lazy_plots_gg_compatibility")
  old_lazy <- getOption("dataquieR.lazy_plots")
  on.exit(options(
    dataquieR.lazy_plots_gg_compatibility = old_compat,
    dataquieR.lazy_plots = old_lazy
  ), add = TRUE)
  options(
    dataquieR.lazy_plots_gg_compatibility = FALSE,
    dataquieR.lazy_plots = TRUE
  )

  base_plot <- ggplot2::ggplot(data.frame(x = 1:2, y = 2:3),
    ggplot2::aes(x, y))
  lazy <- util_create_lean_ggplot(
    {
      base_plot + ggplot2::geom_point()
    },
    base_plot = base_plot,
    .lazy = TRUE
  )

  expect_s3_class(ggplotly.dq_lazy_ggplot(lazy), "plotly")
  expect_s3_class(plotly_build.dq_lazy_ggplot(lazy), "plotly")
})

test_that("lazy ggplot print materializes ordinary ggplots", {
  skip_on_cran()
  skip_if_not_installed("ggplot2")

  old_compat <- getOption("dataquieR.lazy_plots_gg_compatibility")
  old_cache <- getOption("dataquieR.lazy_plots_cache")
  on.exit(options(
    dataquieR.lazy_plots_gg_compatibility = old_compat,
    dataquieR.lazy_plots_cache = old_cache
  ), add = TRUE)
  options(
    dataquieR.lazy_plots_gg_compatibility = FALSE,
    dataquieR.lazy_plots_cache = TRUE
  )

  base_plot <- ggplot2::ggplot(data.frame(x = 1:2, y = 2:3),
    ggplot2::aes(x, y))
  lazy <- util_create_lean_ggplot(
    {
      base_plot + ggplot2::geom_point()
    },
    base_plot = base_plot,
    .lazy = TRUE
  )

  grDevices::pdf(tempfile(fileext = ".pdf"))
  withr::defer(grDevices::dev.off())
  printed <- withVisible(print(lazy))

  expect_false(printed$visible)
  expect_identical(printed$value, lazy)
})

test_that("lazy ggplot methods materialize for composition and grobs", {
  skip_on_cran()
  skip_if_not_installed("ggplot2")

  old_lazy <- getOption("dataquieR.lazy_plots")
  old_compat <- getOption("dataquieR.lazy_plots_gg_compatibility")
  on.exit(options(
    dataquieR.lazy_plots = old_lazy,
    dataquieR.lazy_plots_gg_compatibility = old_compat
  ), add = TRUE)
  options(
    dataquieR.lazy_plots = TRUE,
    dataquieR.lazy_plots_gg_compatibility = FALSE
  )

  base_plot <- ggplot2::ggplot(
    data.frame(x = 1:2, y = 3:4),
    ggplot2::aes(x, y)
  )
  lazy_points <- base_plot %lean_lazy+% ggplot2::geom_point()
  lazy_line <- util_create_lean_ggplot(
    {
      base_plot + ggplot2::geom_line()
    },
    base_plot = base_plot,
    .lazy = TRUE
  )

  combined <- lazy_points + lazy_line
  expect_s3_class(combined, "ggplot")
  expect_s3_class(combined$data, "data.frame")

  direct_grob <- ggplotGrob.dq_lazy_ggplot(lazy_points)
  expect_s3_class(direct_grob, "gtable")
})

test_that("invalid lazy ggplot cache option falls back with a warning", {
  skip_on_cran()
  skip_if_not_installed("ggplot2")

  old_compat <- getOption("dataquieR.lazy_plots_gg_compatibility")
  old_cache <- getOption("dataquieR.lazy_plots_cache")
  on.exit(options(
    dataquieR.lazy_plots_gg_compatibility = old_compat,
    dataquieR.lazy_plots_cache = old_cache
  ), add = TRUE)
  options(
    dataquieR.lazy_plots_gg_compatibility = FALSE,
    dataquieR.lazy_plots_cache = NA
  )

  lazy <- util_create_lean_ggplot(
    {
      ggplot2::ggplot(data.frame(x = 1, y = 1), ggplot2::aes(x, y))
    },
    .lazy = TRUE
  )

  expect_warning(
    realized <- prep_realize_ggplot(lazy),
    "Cannot use option dataquieR.lazy_plots_cache"
  )
  expect_s3_class(realized, "ggplot")
})

test_that("lean ggplot composition follows lazy plot options", {
  skip_on_cran()
  skip_if_not_installed("ggplot2")

  old_lazy <- getOption("dataquieR.lazy_plots")
  old_compat <- getOption("dataquieR.lazy_plots_gg_compatibility")
  on.exit(options(
    dataquieR.lazy_plots = old_lazy,
    dataquieR.lazy_plots_gg_compatibility = old_compat
  ), add = TRUE)
  options(dataquieR.lazy_plots_gg_compatibility = FALSE)

  base_plot <- ggplot2::ggplot(data.frame(x = 1:2, y = 2:3),
    ggplot2::aes(x, y))
  layer <- ggplot2::geom_point()

  options(dataquieR.lazy_plots = FALSE)
  eager <- base_plot %lean+% layer
  expect_s3_class(eager, "ggplot")
  expect_length(eager$layers, 1L)

  options(dataquieR.lazy_plots = TRUE)
  lazy <- base_plot %lean+% layer
  expect_s3_class(lazy, "dq_lazy_ggplot")
  expect_s3_class(prep_realize_ggplot(lazy), "ggplot")

  explicit_lazy <- base_plot %lean_lazy+% layer
  expect_s3_class(explicit_lazy, "dq_lazy_ggplot")
})

test_that("invalid lean ggplot option falls back with a warning", {
  skip_on_cran()
  skip_if_not_installed("ggplot2")

  old_lazy <- getOption("dataquieR.lazy_plots")
  old_compat <- getOption("dataquieR.lazy_plots_gg_compatibility")
  on.exit(options(
    dataquieR.lazy_plots = old_lazy,
    dataquieR.lazy_plots_gg_compatibility = old_compat
  ), add = TRUE)

  base_plot <- ggplot2::ggplot(data.frame(x = 1, y = 1), ggplot2::aes(x, y))
  options(
    dataquieR.lazy_plots = "maybe",
    dataquieR.lazy_plots_gg_compatibility = FALSE
  )

  expect_warning(
    plot <- base_plot %lean+% ggplot2::geom_point(),
    "Cannot use option dataquieR.lazy_plots"
  )
  if (inherits(plot, "dq_lazy_ggplot")) {
    expect_s3_class(plot, "dq_lazy_ggplot")
    expect_s3_class(prep_realize_ggplot(plot), "ggplot")
  } else {
    expect_s3_class(plot, "ggplot")
  }
})

test_that("lazy ggplot patchwork operators materialize both sides", {
  skip_on_cran()
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("patchwork")

  old_compat <- getOption("dataquieR.lazy_plots_gg_compatibility")
  old_cache <- getOption("dataquieR.lazy_plots_cache")
  on.exit(options(
    dataquieR.lazy_plots_gg_compatibility = old_compat,
    dataquieR.lazy_plots_cache = old_cache
  ), add = TRUE)
  options(
    dataquieR.lazy_plots_gg_compatibility = FALSE,
    dataquieR.lazy_plots_cache = TRUE
  )

  p1 <- ggplot2::ggplot(data.frame(x = 1:2, y = 1:2),
    ggplot2::aes(x, y)) +
    ggplot2::geom_point()
  p2 <- ggplot2::ggplot(data.frame(x = 1:2, y = 2:1),
    ggplot2::aes(x, y)) +
    ggplot2::geom_line()

  env <- environment()
  lazy_1 <- dq_lazy_ggplot(quote(p1), env = env, id = "operator-left")
  lazy_2 <- dq_lazy_ggplot(quote(p2), env = env, id = "operator-right")

  expect_s3_class(lazy_1 - lazy_2, "patchwork")
  expect_s3_class(lazy_1 / lazy_2, "patchwork")
  expect_s3_class(lazy_1 | lazy_2, "patchwork")
  expect_s3_class(lazy_1 * lazy_2, "patchwork")
  expect_s3_class(lazy_1 & lazy_2, "patchwork")
})

test_that("S7 lazy ggplot adapters delegate to the payload", {
  skip_on_cran()
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("grImport2")
  skip_if_not_installed("rsvg")

  old_compat <- getOption("dataquieR.lazy_plots_gg_compatibility")
  old_cache <- getOption("dataquieR.lazy_plots_cache")
  on.exit(options(
    dataquieR.lazy_plots_gg_compatibility = old_compat,
    dataquieR.lazy_plots_cache = old_cache
  ), add = TRUE)
  options(
    dataquieR.lazy_plots_gg_compatibility = TRUE,
    dataquieR.lazy_plots_cache = TRUE
  )

  base_plot <- ggplot2::ggplot(data.frame(x = 1:2, y = 2:3),
    ggplot2::aes(x, y)) +
    ggplot2::geom_point()
  env <- environment()
  lazy <- dq_lazy_ggplot(quote(base_plot), env = env, id = "s7-adapter")

  skip_if_not(inherits(lazy, "dq_lazy_ggplot_s7"))

  expect_s3_class(ggplot2::ggplot_build(lazy), "ggplot_built")
  expect_s3_class(ggplot2::ggplotGrob(lazy), "gtable")
  expect_identical(util_realize_if_lazy(lazy), lazy)
  expect_s3_class(lazy$data, "data.frame")
  expect_type(lazy[["layers"]], "list")
  expect_s3_class(util_undisclose.dq_lazy_ggplot_s7(lazy), "svg_plot_proxy")
})

test_that("S7 lazy ggplot plotly and print methods delegate to payload", {
  skip_on_cran()
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("plotly")
  skip_if_not_installed("S7")

  old_compat <- getOption("dataquieR.lazy_plots_gg_compatibility")
  old_cache <- getOption("dataquieR.lazy_plots_cache")
  old_ready <- .dq_lazy_state$s7_ready
  old_class <- .dq_lazy_state$s7_class
  withr::defer({
    options(
      dataquieR.lazy_plots_gg_compatibility = old_compat,
      dataquieR.lazy_plots_cache = old_cache
    )
    .dq_lazy_state$s7_ready <- old_ready
    .dq_lazy_state$s7_class <- old_class
  })
  options(
    dataquieR.lazy_plots_gg_compatibility = TRUE,
    dataquieR.lazy_plots_cache = TRUE
  )
  .dq_lazy_state$s7_ready <- FALSE
  .dq_lazy_state$s7_class <- NULL

  base_plot <- ggplot2::ggplot(data.frame(x = 1:2, y = 2:3),
    ggplot2::aes(x, y)) +
    ggplot2::geom_point()
  env <- environment()
  lazy <- suppressMessages(
    dq_lazy_ggplot(quote(base_plot), env = env, id = "s7-plotly-direct")
  )
  skip_if_not(inherits(lazy, "dq_lazy_ggplot_s7"))

  expect_s3_class(ggplotly.dq_lazy_ggplot_s7(lazy), "plotly")
  expect_s3_class(plotly_build.dq_lazy_ggplot_s7(lazy), "plotly")

  grDevices::pdf(tempfile(fileext = ".pdf"))
  withr::defer(grDevices::dev.off())
  printed <- withVisible(print.dq_lazy_ggplot_s7(lazy))
  expect_false(printed$visible)
  expect_s3_class(ggplotGrob.dq_lazy_ggplot_s7(lazy), "gtable")
})

test_that("S7 lazy ggplot operators materialize both sides", {
  skip_on_cran()
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("patchwork")
  skip_if_not_installed("S7")

  old_compat <- getOption("dataquieR.lazy_plots_gg_compatibility")
  old_cache <- getOption("dataquieR.lazy_plots_cache")
  old_ready <- .dq_lazy_state$s7_ready
  old_class <- .dq_lazy_state$s7_class
  withr::defer({
    options(
      dataquieR.lazy_plots_gg_compatibility = old_compat,
      dataquieR.lazy_plots_cache = old_cache
    )
    .dq_lazy_state$s7_ready <- old_ready
    .dq_lazy_state$s7_class <- old_class
  })
  options(
    dataquieR.lazy_plots_gg_compatibility = TRUE,
    dataquieR.lazy_plots_cache = TRUE
  )

  .dq_lazy_state$s7_ready <- FALSE
  .dq_lazy_state$s7_class <- NULL

  p1 <- ggplot2::ggplot(data.frame(x = 1:2, y = 1:2),
    ggplot2::aes(x, y)) +
    ggplot2::geom_point()
  p2 <- ggplot2::ggplot(data.frame(x = 1:2, y = 2:1),
    ggplot2::aes(x, y)) +
    ggplot2::geom_line()

  env <- environment()
  lazy_1 <- suppressMessages(
    dq_lazy_ggplot(quote(p1), env = env, id = "s7-operator-left")
  )
  lazy_2 <- suppressMessages(
    dq_lazy_ggplot(quote(p2), env = env, id = "s7-operator-right")
  )

  skip_if_not(inherits(lazy_1, "dq_lazy_ggplot_s7"))

  expect_s3_class(lazy_1 + lazy_2, "patchwork")
  expect_s3_class(lazy_1 - lazy_2, "patchwork")
  expect_s3_class(lazy_1 / lazy_2, "patchwork")
  expect_s3_class(lazy_1 | lazy_2, "patchwork")
  expect_s3_class(lazy_1 * lazy_2, "patchwork")
  expect_s3_class(lazy_1 & lazy_2, "patchwork")
})

test_that("S7 lazy ggplot registration is guarded and initializes state", {
  skip_on_cran()
  skip_if_not_installed("S7")

  old_ready <- .dq_lazy_state$s7_ready
  old_class <- .dq_lazy_state$s7_class
  withr::defer({
    .dq_lazy_state$s7_ready <- old_ready
    .dq_lazy_state$s7_class <- old_class
  })

  sentinel_class <- function(...) {
    list(...)
  }
  .dq_lazy_state$s7_ready <- TRUE
  .dq_lazy_state$s7_class <- sentinel_class
  expect_true(dq_lazy_register_s7())
  expect_identical(.dq_lazy_state$s7_class, sentinel_class)

  .dq_lazy_state$s7_ready <- FALSE
  .dq_lazy_state$s7_class <- NULL
  expect_true(suppressMessages(dq_lazy_register_s7()))
  expect_true(.dq_lazy_state$s7_ready)
  expect_true(is.function(.dq_lazy_state$s7_class))
})

test_that("S7 lazy ggplot registration is unavailable without S7", {
  skip_on_cran()

  old_ready <- .dq_lazy_state$s7_ready
  old_class <- .dq_lazy_state$s7_class
  withr::defer({
    .dq_lazy_state$s7_ready <- old_ready
    .dq_lazy_state$s7_class <- old_class
  })
  .dq_lazy_state$s7_ready <- FALSE
  .dq_lazy_state$s7_class <- NULL

  testthat::with_mocked_bindings(
    .package = "base",
    requireNamespace = function(package, quietly = FALSE, ...) {
      if (identical(package, "S7")) FALSE else base::requireNamespace(
        package,
        quietly = quietly,
        ...
      )
    },
    {
      expect_false(dq_lazy_register_s7())
    }
  )
})
