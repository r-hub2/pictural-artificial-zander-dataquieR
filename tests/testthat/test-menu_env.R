skip_on_cran()

test_that("report menu renders explicit separators", {
  skip_if_not_installed("htmltools")
  skip_if_not_installed("markdown")

  pages <- list(
    scales.html = list(
      Metric = dataquieR:::util_attach_attr( # nolint
        htmltools::div(id = "Metric"),
        dropdown = VARIABLE_GROUP_REPORT_MENU
      )
    ),
    groups.html = list(
      Group = dataquieR:::util_attach_attr( # nolint
        htmltools::div(id = "Group"),
        dropdown = VARIABLE_GROUP_REPORT_MENU,
        menu_separator_before = TRUE
      )
    )
  )

  html <- htmltools::renderTags(dataquieR:::.menu_env$menu(pages))$html # nolint

  expect_match(html, "<hr class=\"dropdown-divider\"/>", fixed = TRUE)
  expect_match(html, "scales.html#Metric", fixed = TRUE)
  expect_match(html, "groups.html#Group", fixed = TRUE)
  expect_equal(
    lengths(regmatches(html, gregexpr('class="dropdown"', html, fixed = TRUE))),
    1L
  )
  expect_match(html, 'id="Variablegroupsscales"', fixed = TRUE)
})

test_that("variable groups and scales share a report-specific description", {
  skip_if_not_installed("htmltools")
  skip_if_not_installed("markdown")

  pages <- list(groups.html = list(
    `Repeated measurements` = dataquieR:::util_attach_attr( # nolint
      htmltools::div(id = "Repeated measurements"),
      dropdown = VARIABLE_GROUP_REPORT_MENU
    )
  ))

  html <- htmltools::renderTags(dataquieR:::.menu_env$menu(pages))$html # nolint

  expect_match(html, "group-level analyses", fixed = TRUE)
  expect_match(html, "scale metrics and other", fixed = TRUE)
  expect_match(html, "Variable groups / scales", fixed = TRUE)
  expect_false(grepl("No unique description", html, fixed = TRUE))
})

test_that("Single Variables menu labels are shortened without changing links", {
  skip_if_not_installed("htmltools")
  skip_if_not_installed("markdown")

  long_title <- paste0(
    "ITEM_A_01: QuestionnaireItemLabelWithoutAnyWhitespaceDesignedToStress",
    "TablesTooltipsChartsAndVariableGroupSummaries"
  )
  pages <- list(single.html = setNames(list(
    dataquieR:::util_attach_attr( # nolint
      htmltools::div(id = long_title),
      dropdown = "Single Variables"
    )
  ), long_title))

  html <- htmltools::renderTags(dataquieR:::.menu_env$menu(pages))$html # nolint

  expect_match(
    html,
    paste0("single.html#", htmltools::urlEncodePath(long_title)),
    fixed = TRUE
  )
  expect_match(html, sprintf('title="%s"', long_title), fixed = TRUE)
  expect_false(grepl(paste0(">", long_title, "</a>"), html, fixed = TRUE))
  expect_match(html, "...", fixed = TRUE)
})
