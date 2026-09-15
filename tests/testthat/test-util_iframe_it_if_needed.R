test_that("util_get_restricted_size_args_for_figure preserves small figures", {
  skip_on_cran()

  sizing_hints <- list(w_in_cm = 10, h_in_cm = 5)
  args <- util_get_restricted_size_args_for_figure(
    MAX_SIZE = 4 * 1024 * 1024,
    max_w_in_cm = 20,
    max_h_in_cm = 20,
    sizing_hints = sizing_hints
  )

  expect_equal(args$sizing_hints_updated, sizing_hints)
  expect_equal(args$width, 10 / 2.54)
  expect_equal(args$height, 5 / 2.54)
  expect_gte(args$dpi, 72)
})

test_that("util_get_restricted_size_args_for_figure scales large figures", {
  skip_on_cran()

  args <- util_get_restricted_size_args_for_figure(
    MAX_SIZE = 4 * 1024 * 1024,
    max_w_in_cm = 20,
    max_h_in_cm = 10,
    sizing_hints = list(w_in_cm = 40, h_in_cm = 10)
  )

  expect_equal(args$sizing_hints_updated$w_in_cm, 20)
  expect_equal(args$sizing_hints_updated$h_in_cm, 5)
  expect_equal(args$width, 20 / 2.54)
  expect_equal(args$height, 5 / 2.54)
})

test_that("util_get_restricted_size_args_for_figure handles invalid sizes", {
  skip_on_cran()

  expect_warning(
    args <- util_get_restricted_size_args_for_figure(
      MAX_SIZE = 4 * 1024 * 1024,
      max_w_in_cm = 20,
      max_h_in_cm = 20,
      sizing_hints = list(w_in_cm = 0, h_in_cm = 5)
    ),
    "Invalid physical"
  )

  expect_equal(args$dpi, 72)
  expect_equal(args$width, 0)
  expect_equal(args$height, 5 / 2.54)
})

test_that("util_get_cores_safe reports default-cluster size", {
  skip_on_cran()

  expect_equal(util_get_cores_safe(), 1L)

  cl <- structure(list(1, 2), class = c("SOCKcluster", "cluster"))
  withr::defer(try(parallel::setDefaultCluster(NULL), silent = TRUE))
  parallel::setDefaultCluster(cl)

  expect_equal(util_get_cores_safe(), 2L)
})

test_that("util_iframe_it_if_needed attaches iframe metadata for images", {
  skip_on_cran()
  skip_if_not_installed("htmltools")
  skip_if_not_installed("jsonlite")
  skip_if_not_installed("rmarkdown")

  out_dir <- tempfile("iframe-dir")
  dir.create(out_dir)
  html_img <- htmltools::tags$img(src = "plot.png")

  framed <- util_iframe_it_if_needed(
    html_img,
    dir = out_dir,
    nm = "",
    fkt = "example_indicator",
    sizing_hints = list(w = "10cm", h = "5cm"),
    ggthumb = NULL
  )

  expect_s3_class(framed, "shiny.tag")
  expect_match(basename(attr(framed, "html_file")), "^FIG_.*[.]html$")
  expect_s3_class(attr(framed, "html_inner"), "shiny.tag.list")
  expect_null(attr(framed, "thumbnail_path", exact = TRUE))
  expect_match(framed$attribs$style, "min-width:")
})

test_that("util_iframe_it_if_needed attaches thumbnail metadata", {
  skip_on_cran()
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("htmltools")
  skip_if_not_installed("jsonlite")
  skip_if_not_installed("rmarkdown")

  out_dir <- tempfile("iframe-dir")
  dir.create(out_dir)
  html_img <- htmltools::tags$img(src = "plot.png")
  thumb <- ggplot2::ggplot(
    data.frame(x = 1:2, y = 2:3),
    ggplot2::aes(x, y)
  ) +
    ggplot2::geom_point()

  framed <- util_iframe_it_if_needed(
    html_img,
    dir = out_dir,
    nm = "Summary plot",
    fkt = "example_indicator",
    sizing_hints = list(
      figure_type_id = "scatter",
      w = "60cm",
      h = "40cm"
    ),
    ggthumb = thumb
  )

  expect_match(basename(attr(framed, "html_file")),
    "^FIG_Summaryplot[.]html$")
  expect_match(basename(attr(framed, "thumbnail_path")), "[.]png$")
  expect_s3_class(attr(framed, "ggthumb"), "compressed")
  expect_named(attr(framed, "thumbnail_args"),
    c("width", "height", "dpi", "figure_type_id", "rotated"))
})

test_that("util_iframe_it_if_needed handles non-framed outputs", {
  skip_on_cran()
  skip_if_not_installed("htmltools")

  plain <- util_iframe_it_if_needed(
    "plain text",
    dir = tempdir(),
    nm = "plain",
    fkt = "example_indicator",
    sizing_hints = list(w = "10cm", h = "5cm"),
    ggthumb = NULL
  )

  expect_s3_class(plain, "shiny.tag")
  expect_identical(plain$name, "div")
  expect_null(attr(plain, "html_file", exact = TRUE))
})

test_that("util_iframe_it_if_needed records missing jsonlite fallback", {
  skip_on_cran()
  skip_if_not_installed("htmltools")
  skip_if_not_installed("rmarkdown")

  out_dir <- tempfile("iframe-dir")
  dir.create(out_dir)
  html_img <- htmltools::tags$img(src = "plot.png")

  testthat::local_mocked_bindings(
    util_ensure_suggested = function(pkg, goal, err = TRUE) {
      if (pkg %in% c("jsonlite", "units") && identical(err, FALSE)) {
        return(FALSE)
      }
      TRUE
    }
  )

  framed <- util_iframe_it_if_needed(
    html_img,
    dir = out_dir,
    nm = "jsonlite fallback",
    fkt = "example_indicator",
    sizing_hints = list(w = "10cm", h = "0cm"),
    ggthumb = NULL
  )
  html_inner <- attr(framed, "html_inner", exact = TRUE)
  rendered <- paste(as.character(html_inner), collapse = "\n")

  expect_match(rendered, "No figure size hints")
  expect_match(framed$attribs$style, "min-height:")
})
