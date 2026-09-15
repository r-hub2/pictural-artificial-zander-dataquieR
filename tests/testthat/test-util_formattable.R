test_that("util_formattable renders styled numeric HTML tables", {
  skip_on_cran()

  table <- data.frame(
    Low = c(0, 1),
    High = c(10, NA),
    check.names = FALSE
  )
  hover <- data.frame(
    Low = c("<b>low</b>", "one"),
    High = c("ten", "missing"),
    check.names = FALSE
  )

  html <- htmltools::renderTags(util_formattable(
    table,
    min_color = c(0, 0, 0),
    max_color = c(255, 255, 255),
    soften = identity,
    style_header = c("font-weight: bold;", "font-style: italic;"),
    hover_texts = hover
  ))$html

  expect_match(html, "<table")
  expect_match(html, "font-style: italic", fixed = TRUE)
  expect_match(html, "background-color:", fixed = TRUE)
  expect_match(html, "title=\"&amp;lt;b&amp;gt;low&amp;lt;/b&amp;gt;\"",
    fixed = TRUE)
})

test_that("util_formattable supports grayscale text and trusted HTML content", {
  skip_on_cran()

  table <- data.frame(
    Label = c("<strong>A</strong>"),
    check.names = FALSE
  )

  html <- htmltools::renderTags(util_formattable(
    table,
    min_val = 0,
    max_val = 1,
    style_header = c("font-weight: bold;", "ignored: true;"),
    text_color_mode = "gs",
    escape_all_content = FALSE
  ))$html

  expect_match(html, "font-weight: bold", fixed = TRUE)
  expect_match(html, "<strong>A</strong>", fixed = TRUE)
  expect_false(grepl("&lt;strong&gt;", html, fixed = TRUE))
})

test_that("util_formattable falls back for degenerate color scales", {
  skip_on_cran()

  table <- data.frame(
    A = c(1, 1),
    B = c(1, 1),
    check.names = FALSE
  )

  html <- htmltools::renderTags(util_formattable(
    table,
    soften = identity
  ))$html

  expect_match(html, "background-color: white", fixed = TRUE)
  expect_match(html, "color: #000000", fixed = TRUE)
  expect_equal(
    length(gregexpr("background-color: white", html, fixed = TRUE)[[1]]),
    4L
  )
})
