test_that("util_pairs_ggplot builds panel matrices for numeric columns", {
  skip_on_cran()

  data <- data.frame(
    a = c(1, 2, 3, 4),
    b = c(4, 3, 2, 1),
    group = letters[1:4],
    stringsAsFactors = FALSE
  )

  panels <- util_pairs_ggplot(
    data,
    columns = c("a", "b", "group"),
    diag = "histogram",
    correlation_method = "spearman",
    columnLabels = c("Alpha", "Beta"),
    title = "Pairs"
  )

  expect_s3_class(panels, "util_pairs_ggplot_panels")
  expect_equal(panels$layout_dim, 2L)
  expect_length(panels$panels, 4L)
  expect_equal(panels$title, "Pairs")
  expect_true(all(vapply(panels$panels, inherits, logical(1), "ggplot")))
})

test_that("util_pairs_ggplot requires two numeric columns", {
  skip_on_cran()

  data <- data.frame(
    a = c(1, 2, 3),
    group = letters[1:3],
    stringsAsFactors = FALSE
  )

  expect_error(
    util_pairs_ggplot(data, columns = c("a", "group")),
    "At least two numeric columns"
  )
})

test_that("util_pairs_plotly builds annotated pair matrices", {
  skip_on_cran()
  skip_if_not_installed("plotly")

  data <- data.frame(
    a = c(1, 2, 3, 4),
    b = c(4, 3, 2, 1),
    stringsAsFactors = FALSE
  )

  fig <- util_pairs_plotly(
    data,
    diag = "histogram",
    title = "Interactive pairs",
    columnLabels = c("Alpha", "Beta")
  )

  expect_s3_class(fig, "plotly")
  expect_length(fig$x$data, 3L)
  expect_true(is.list(fig$x$layout$annotations))
  expect_true(length(fig$x$layout$annotations) >= 1L)
  expect_false(isTRUE(fig$x$layout$showlegend))
})

test_that("util_pairs_ggplot panels print with optional titles", {
  skip_on_cran()

  data <- data.frame(
    a = c(1, 2, 3, 4),
    b = c(4, 3, 2, 1),
    stringsAsFactors = FALSE
  )

  panels <- util_pairs_ggplot(data, title = "Pairs")

  expect_silent(print(panels))
  expect_identical(grid::grid.draw(panels), panels)
})

test_that("util_pairs_plotly samples large scatter panels deterministically", {
  skip_on_cran()
  skip_if_not_installed("plotly")

  data <- data.frame(
    a = seq_len(3001),
    b = rev(seq_len(3001))
  )

  testthat::local_mocked_bindings(
    util_subsample_cases = function(df, x, y, nmax, seed) {
      expect_identical(nmax, 3000)
      expect_identical(seed, 1)
      seq_len(nmax)
    }
  )

  fig <- util_pairs_plotly(data, diag = "histogram")
  trace_lengths <- vapply(fig$x$data, function(trace) {
    length(trace$x)
  }, FUN.VALUE = integer(1))

  expect_s3_class(fig, "plotly")
  expect_true(3000L %in% trace_lengths)
  expect_true(all(trace_lengths <= nrow(data)))
})
