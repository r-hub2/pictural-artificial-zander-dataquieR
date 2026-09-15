test_that("util_get_var_att_names_of_level respects requirement levels", {
  skip_on_cran()

  required <- util_get_var_att_names_of_level(
    VARATT_REQUIRE_LEVELS$REQUIRED,
    cumulative = FALSE
  )
  optional_cumulative <- util_get_var_att_names_of_level(
    VARATT_REQUIRE_LEVELS$OPTIONAL,
    cumulative = TRUE
  )

  expect_true(all(required %in% optional_cumulative))
  expect_true(VAR_NAMES %in% required)
  expect_true(DATA_TYPE %in% required)
  expect_false(LABEL %in% required)
  expect_true(LABEL %in% optional_cumulative)
  expect_equal(
    util_get_var_att_names_of_level(character(0)),
    character(0)
  )
  expect_error(
    util_get_var_att_names_of_level("not_a_requirement_level"),
    "should be one of"
  )
})

test_that("util_order_of_indicator_metrics orders known metrics first", {
  skip_on_cran()

  metrics <- c(
    "unknown_metric",
    "PCT_con_con",
    "NUM_int_sts",
    "PCT_int_sts"
  )

  order <- util_order_of_indicator_metrics(metrics)

  expect_equal(order[["PCT_int_sts"]], 1L)
  expect_equal(order[["NUM_int_sts"]], 2L)
  expect_equal(order[["PCT_con_con"]], 3L)
  expect_equal(order[["unknown_metric"]], 4L)
})

test_that("util_get_concept_links creates external concept links", {
  skip_on_cran()
  skip_if_not_installed("htmltools")

  links <- util_get_concept_links("int_all_datastructure_dataframe")
  html <- htmltools::renderTags(links)$html

  expect_s3_class(links, "shiny.tag")
  expect_match(html, "<ul", fixed = TRUE)
  expect_match(html, "target=\"_blank\"", fixed = TRUE)
  expect_match(
    html,
    "https://dataquality.qihs.uni-greifswald.de/id/#",
    fixed = TRUE
  )
})

test_that("util_get_concept_links handles functions without concept links", {
  skip_on_cran()
  skip_if_not_installed("htmltools")

  links <- util_get_concept_links("not_a_dataquier_function")
  html <- htmltools::renderTags(links)$html

  expect_s3_class(links, "shiny.tag")
  expect_match(html, "<ul", fixed = TRUE)
  expect_false(grepl("target=\"_blank\"", html, fixed = TRUE))
})
