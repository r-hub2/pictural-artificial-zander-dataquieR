test_that("util_float_index_menu renders table rows as floating links", {
  skip_on_cran()
  skip_if_not_installed("htmltools")

  menu <- util_float_index_menu(data.frame(
    links = c("#first", "#second"),
    hovers = c("First section", "Second section"),
    texts = c("First", "Second")
  ))

  html <- htmltools::renderTags(menu)$html

  expect_match(html, "class=\"floatbar\"", fixed = TRUE)
  expect_match(html, "class=\"floatmenu\"", fixed = TRUE)
  expect_match(html, "title=\"Jump to section\"", fixed = TRUE)
  expect_match(html, "href=\"#first\"", fixed = TRUE)
  expect_match(html, "title=\"First section\"", fixed = TRUE)
  expect_match(html, ">First</a>", fixed = TRUE)
  expect_match(html, "href=\"#second\"", fixed = TRUE)
  expect_match(html, "title=\"Second section\"", fixed = TRUE)
  expect_match(html, ">Second</a>", fixed = TRUE)
})

test_that("util_float_index_menu accepts a prebuilt object", {
  skip_on_cran()
  skip_if_not_installed("htmltools")

  menu <- util_float_index_menu(
    object = htmltools::tagList(
      htmltools::tags$li(htmltools::a(href = "#direct", "Direct"))
    )
  )

  html <- htmltools::renderTags(menu)$html

  expect_match(html, "href=\"#direct\"", fixed = TRUE)
  expect_match(html, ">Direct</a>", fixed = TRUE)
})

test_that("util_float_index_menu omits empty menus", {
  skip_on_cran()
  skip_if_not_installed("htmltools")

  expect_null(util_float_index_menu(object = htmltools::tagList()))
})

test_that("util_float_index_menu requires only one input mode", {
  skip_on_cran()
  skip_if_not_installed("htmltools")

  expect_error(
    util_float_index_menu(
      index_menu_table = data.frame(links = "#x", hovers = "X", texts = "X"),
      object = htmltools::tags$li("x")
    ),
    "index_menu_table"
  )
})
