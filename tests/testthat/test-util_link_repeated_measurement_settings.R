test_that("util_link_repeated_measurement_settings leaves unrelated tables", {
  skip_on_cran()

  tb <- data.frame(label = "plain")

  linked <- util_link_repeated_measurement_settings(tb)

  expect_equal(linked, tb)
  expect_null(attr(linked, "is_html_escaped", exact = TRUE))
})

test_that("util_link_repeated_measurement_settings escapes empty settings", {
  skip_on_cran()

  tb <- data.frame(
    `Repeated-measurement setting` = c("", NA_character_),
    detail = c("<unsafe>", "plain"),
    check.names = FALSE
  )

  linked <- util_link_repeated_measurement_settings(tb)

  expect_true(attr(linked, "is_html_escaped", exact = TRUE))
  expect_identical(linked[["Repeated-measurement setting"]], c("", NA))
  expect_identical(linked$detail, c("&lt;unsafe&gt;", "plain"))
})

test_that("util_link_repeated_measurement_settings links setting IDs", {
  skip_on_cran()
  skip_if_not_installed("htmltools")

  tb <- data.frame(
    `Repeated-measurement setting` = c("setting 1", ""),
    detail = c("<unsafe>", "plain"),
    check.names = FALSE
  )

  linked <- util_link_result_references(tb)

  expect_true(attr(linked, "is_html_escaped", exact = TRUE))
  expect_match(
    linked[["Repeated-measurement setting"]][[1]],
    "statisticalsettings.html?dq_filter_col=SETTING_ID",
    fixed = TRUE
  )
  expect_match(
    linked[["Repeated-measurement setting"]][[1]],
    "dq_filter_value=setting%201",
    fixed = TRUE
  )
  expect_match(
    linked[["Repeated-measurement setting"]][[1]],
    ">setting 1</a>",
    fixed = TRUE
  )
  expect_identical(linked[["Repeated-measurement setting"]][[2]], "")
  expect_identical(linked$detail, c("&lt;unsafe&gt;", "plain"))
})
