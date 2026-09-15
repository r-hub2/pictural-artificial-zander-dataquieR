test_that("page-generation HTML helpers classify wrappers and empty content", {
  skip_on_cran()
  skip_if_not_installed("htmltools")

  wrapped <- util_apply_full_page_table_class(htmltools::span("table"))

  expect_s3_class(wrapped, "shiny.tag")
  expect_equal(wrapped$attribs$class, "fullpage-table")
  expect_false(util_is_empty_html(wrapped))
  expect_true(util_is_empty_html(NULL))
  expect_true(util_is_empty_html(htmltools::HTML("   ")))
  expect_false(util_is_empty_html(htmltools::HTML("x")))
})
