test_that("util_plot_figure_no_plotly works", {
  skip_on_cran()
  # Create a test plot
  test_plot <- plot(cars)
  result <- util_plot_figure_no_plotly(test_plot)

  # Check that the result is an HTML plot tag
  expect_s3_class(result, "shiny.tag")
})

test_that("util_plot_figure_no_plotly falls back if Cairo is unavailable", {
  skip_on_cran()
  original_capabilities <- base::capabilities
  testthat::local_mocked_bindings(
    capabilities = function(what = NULL) {
      if (identical(what, "cairo")) {
        return(FALSE)
      }
      original_capabilities(what)
    },
    .package = "base"
  )

  result <- util_plot_figure_no_plotly(plot(cars))

  expect_s3_class(result, "shiny.tag")
  expect_identical(result$name, "img")
  expect_identical(result$attribs$class, "dataquieRfigure")
})
