skip_on_cran()

test_that("util_compress_ggplots_in_res drops ReportSummaryTable plots", {
  plot <- ggplot2::ggplot(
    data.frame(x = 1:2, y = 3:4),
    ggplot2::aes(x = x, y = y)
  ) +
    ggplot2::geom_point()
  attr(plot, "from_ReportSummaryTable") <- TRUE

  expect_null(util_compress_ggplots_in_res(plot))
})

test_that(
  "util_compress_ggplots_in_res removes null plot entries recursively",
  {
    plot <- ggplot2::ggplot(
      data.frame(x = 1:2, y = 3:4),
      ggplot2::aes(x = x, y = y)
    ) +
      ggplot2::geom_point()
    marked <- plot
    attr(marked, "from_ReportSummaryTable") <- TRUE

    result <- util_compress_ggplots_in_res(list(
      keep_value = 1,
      marked_plot = marked,
      nested = list(plot = plot, marked = marked)
    ))

    expect_identical(names(result), c("keep_value", "nested"))
    expect_identical(names(result$nested), "plot")
    expect_s3_class(result$nested$plot, "ggplot")
  }
)

test_that("util_compress_ggplots_in_res keeps mapped plot data only", {
  plot <- ggplot2::ggplot(
    data.frame(x = 1:2, y = 3:4, unused = 5:6),
    ggplot2::aes(x = .data[["x"]], y = .data[["y"]])
  ) +
    ggplot2::geom_point()

  compressed <- util_compress_ggplots_in_res(plot)

  expect_identical(names(compressed$data), c("x", "y", "unused"))
  expect_identical(compressed$plot_env, emptyenv())
})

test_that("util_compress_ggplots_in_res keeps layer and facet columns", {
  plot <- ggplot2::ggplot(
    data.frame(
      x = 1:4,
      y = 2:5,
      label = letters[1:4],
      facet = rep(c("A", "B"), each = 2),
      unused = 10:13,
      stringsAsFactors = FALSE
    ),
    ggplot2::aes(x = x, y = y)
  ) +
    ggplot2::geom_text(ggplot2::aes(label = label)) +
    ggplot2::facet_wrap(ggplot2::vars(facet))

  compressed <- util_compress_ggplots_in_res(plot)

  expect_true(all(c("x", "y", "label", "facet") %in% names(compressed$data)))
  expect_identical(compressed$plot_env, emptyenv())
})

test_that("util_compress_ggplots_in_res keeps explicit facet vars", {
  plot <- list(
    data = data.frame(
      x = 1:3,
      y = 2:4,
      facet_extra = c("a", "b", "c"),
      unused = 5:7,
      stringsAsFactors = FALSE
    ),
    mapping = list(rlang::quo(x), rlang::quo(y)),
    layers = list(),
    facet = list(vars = function() "facet_extra")
  )

  testthat::local_mocked_bindings(
    util_is_gg_plot = function(x) identical(x, plot)
  )
  compressed <- util_compress_ggplots_in_res(plot)

  expect_identical(
    names(compressed$data),
    c("x", "y", "facet_extra")
  )
})

test_that("util_compress_ggplots_in_res keeps .data layer columns", {
  plot <- ggplot2::ggplot(
    data.frame(
      x = 1:3,
      y = 2:4,
      label = letters[1:3],
      unused = 5:7,
      stringsAsFactors = FALSE
    ),
    ggplot2::aes(x = x, y = y)
  ) +
    ggplot2::geom_text(ggplot2::aes(label = .data[["label"]]))

  compressed <- util_compress_ggplots_in_res(plot)

  expect_true(all(c("x", "y", "label") %in% names(compressed$data)))
  expect_true("unused" %in% names(compressed$data))
  expect_identical(compressed$plot_env, emptyenv())
})

test_that("util_compress_ggplots_in_res leaves lazy ggplot lists unchanged", {
  lazy <- structure(
    list(
      plot = structure(list(), class = c("gg", "ggplot")),
      marked_plot = NULL
    ),
    class = c("dq_lazy_ggplot", "list")
  )

  expect_identical(util_compress_ggplots_in_res(lazy), lazy)
})
