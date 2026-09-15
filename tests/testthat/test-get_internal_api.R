test_that(".get_internal_api works", {
  skip_on_cran() # internal use, only
  expect_true(".get_internal_api" %in%
      getNamespaceExports("dataquieR"))
  x <- .get_internal_api("util_html_table", version = "0.0.1", TRUE)
  expect_identical(x, util_html_table)
  expect_warning(
    .get_internal_api("util_html_table"),
    "should not omit the API version"
  )
  expect_error(.get_internal_api("util_html_table", version = "0.0.1", FALSE))
  expect_error(.get_internal_api("util_html_table", version = "10", FALSE))
  expect_error(.get_internal_api("util_html_table", version = "10", TRUE))
})

test_that(".get_internal_api validates developer API access", {
  skip_on_cran()

  expect_identical(
    .get_internal_api(util_html_table, version = API_VERSION),
    util_html_table
  )

  expect_warning(
    x <- .get_internal_api(util_html_table),
    "should not omit the API version"
  )
  expect_identical(x, util_html_table)

  expect_error(
    .get_internal_api("util_html_table", version = "not-a-version"),
    "version number"
  )
  expect_error(
    .get_internal_api("definitely_not_exported", version = API_VERSION)
  )
})
