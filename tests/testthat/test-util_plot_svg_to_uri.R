test_that("util_plot_svg_to_uri returns an img tag with encoded SVG data", {
  skip_on_cran()
  skip_if_not_installed("htmltools")

  img <- util_plot_svg_to_uri(
    quote({
      old_par <- par(mar = c(1, 1, 1, 1))
      on.exit(par(old_par), add = TRUE)
      plot(c(1, 2), c(3, 4), type = "b")
    }),
    w = 360,
    h = 240
  )

  expect_s3_class(img, "shiny.tag")
  expect_equal(img$name, "img")
  expect_equal(img$attribs$class, "dataquieRfigure")
  expect_true(startsWith(
    img$attribs$src,
    "data:image/svg+xml;charset=utf-8,"
  ))

  decoded <- utils::URLdecode(sub(
    "^data:image/svg\\+xml;charset=utf-8,",
    "",
    img$attribs$src
  ))

  expect_true(startsWith(decoded, "<svg "))
  expect_match(decoded, 'preserveAspectRatio="none"', fixed = TRUE)
  expect_false(grepl("<?xml", decoded, fixed = TRUE))
  expect_false(grepl("<!DOCTYPE", decoded, fixed = TRUE))
})
