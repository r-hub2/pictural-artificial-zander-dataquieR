test_that("svg plot proxies keep raw SVG and identify undisclosed plots", {
  skip_on_cran()

  svg_file <- tempfile(fileext = ".svg")
  writeLines(
    c(
      '<svg xmlns="http://www.w3.org/2000/svg" width="10" height="10">',
      '<circle cx="5" cy="5" r="3" />',
      "</svg>"
    ),
    svg_file
  )
  on.exit(unlink(svg_file), add = TRUE)

  proxy <- util_svg_plot_proxy(svg_file)

  expect_s3_class(proxy, "svg_plot_proxy")
  expect_type(proxy$svg, "raw")
  expect_true(util_is_svg_object(proxy))
  expect_false(util_is_svg_object(data.frame(x = 1)))
})

test_that("svg plot proxies draw through grid methods", {
  skip_on_cran()
  skip_if_not_installed("grImport2")

  svg_file <- tempfile(fileext = ".svg")
  writeLines(
    c(
      '<svg xmlns="http://www.w3.org/2000/svg" width="10" height="10">',
      '<circle cx="5" cy="5" r="3" />',
      "</svg>"
    ),
    svg_file
  )
  on.exit(unlink(svg_file), add = TRUE)

  proxy <- util_svg_plot_proxy(svg_file)

  expect_silent(suppressWarnings(grid::grid.draw(proxy)))
  expect_output(suppressWarnings(print(proxy)), NA)
})

test_that("util_plot2svg_object converts a simple plot expression", {
  skip_on_cran()
  skip_if_not_installed("grImport2")
  skip_if_not_installed("rsvg")

  proxy <- suppressWarnings(
    util_plot2svg_object(
      rlang::expr(plot(1:3, 1:3)),
      sizing_hints = list(figure_type_id = "histogram")
    )
  )

  expect_s3_class(proxy, "svg_plot_proxy")
  expect_true(length(proxy$svg) > 0)
  expect_equal(
    attr(proxy, "sizing_hints", exact = TRUE),
    list(figure_type_id = "histogram")
  )
})

test_that("util_plotly2svg_object reports save_image failures", {
  skip_on_cran()

  testthat::local_mocked_bindings(
    util_ensure_suggested = function(pkg, ...) !identical(pkg, "units")
  )

  expect_error(
    util_plotly2svg_object(
      plotly = list(),
      sizing_hints = list(w = "1cm", h = "2cm")
    ),
    "Could not use .*plotly::save_image"
  )
})

test_that("util_plotly2svg_object keeps sizing hints on SVG proxies", {
  skip_on_cran()
  skip_if_not_installed("plotly")
  skip_if_not_installed("rsvg")

  testthat::local_mocked_bindings(
    util_ensure_suggested = function(...) TRUE
  )
  testthat::local_mocked_bindings(
    save_image = function(p, file, width, height) {
      writeLines(
        c(
          '<svg xmlns="http://www.w3.org/2000/svg">',
          '<rect width="10" height="10" />',
          "</svg>"
        ),
        file
      )
      file
    },
    .package = "plotly"
  )
  testthat::local_mocked_bindings(
    rsvg_svg = function(svg, file) {
      writeBin(svg, file)
      invisible(file)
    },
    .package = "rsvg"
  )

  hints <- list(w = "1cm", h = "2cm")
  proxy <- util_plotly2svg_object(plotly = list(), sizing_hints = hints)

  expect_s3_class(proxy, "svg_plot_proxy")
  expect_true(length(proxy$svg) > 0)
  expect_equal(attr(proxy, "sizing_hints", exact = TRUE), hints)
})
