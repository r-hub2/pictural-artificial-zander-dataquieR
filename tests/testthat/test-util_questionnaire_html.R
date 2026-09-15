questionnaire_html_test_result <- function(variable_name = "SCALE_A",
  metric = "PCT_test_metric",
  value = 25,
  rule_set = "0",
  title = "Scale A") {
  call <- quote(test_indicator(resp_vars = "Scale A"))
  attr(call, VAR_NAMES) <- setNames(variable_name, title)
  attr(call, GRADING_RULESET) <- setNames(rule_set, title)
  summary_data <- data.frame(
    VAR_NAMES = variable_name,
    class = NA_character_,
    indicator_metric = metric,
    value = paste0(value, "%"),
    values_raw = value,
    n_classes = 5L,
    STUDY_SEGMENT = ".COMPUTED__ssi",
    call_names = "test_indicator",
    function_name = "test_indicator",
    stringsAsFactors = FALSE
  )
  result <- structure(
    list(SummaryTable = data.frame(Variables = title)),
    function_name = "test_indicator",
    call = call,
    r_summary = summary_data,
    dq_result_title = title,
    class = c("dataquieR_result", "master_result")
  )
  result
}

test_that("questionnaire sections retain anchors and grading metadata", {
  skip_on_cran()

  first <- questionnaire_html_test_result(
    variable_name = "SCALE_A",
    rule_set = "custom"
  )
  second <- questionnaire_html_test_result(
    variable_name = "SCALE_B",
    title = "Scale B"
  )
  results <- util_questionnaire_add_section_anchors(list(
    first = first,
    second = second
  ))

  expect_identical(
    vapply(results, util_attr,
      which = "dq_result_anchor",
      exact = TRUE,
      FUN.VALUE = character(1)
    ),
    c(
      first = "questionnaire-section-01",
      second = "questionnaire-section-02"
    )
  )
  expect_identical(
    util_questionnaire_result_grading_meta_data(first),
    data.frame(
      VAR_NAMES = "SCALE_A",
      GRADING_RULESET = "custom",
      stringsAsFactors = FALSE
    )
  )

  stored <- data.frame(
    VAR_NAMES = "STORED",
    GRADING_RULESET = "stored-rule"
  )
  attr(first, "dq_questionnaire_grading_meta_data") <- stored
  expect_identical(
    util_questionnaire_result_grading_meta_data(first),
    stored
  )

  attr(second, "call") <- NULL
  fallback <- util_questionnaire_result_grading_meta_data(second)
  expect_identical(fallback[[VAR_NAMES]], "SCALE_B")
  expect_identical(fallback[[GRADING_RULESET]], "0")
  expect_identical(
    util_questionnaire_result_grading_meta_data(structure(
      list(),
      class = "dataquieR_result"
    )),
    data.frame()
  )
})

test_that("questionnaire HTML summary uses current grading and deep links", {
  skip_on_cran()

  result <- questionnaire_html_test_result(value = 25)
  category_row <- util_attr(result, "r_summary", exact = TRUE)
  category_row[["indicator_metric"]] <- "CAT_error"
  category_row[["value"]] <- "1"
  category_row[["values_raw"]] <- "1"
  attr(result, "r_summary") <- util_rbind(
    util_attr(result, "r_summary", exact = TRUE),
    category_row
  )
  questionnaire <- util_questionnaire_results(list(scale = result))
  sections <- util_questionnaire_html_sections(questionnaire)

  state <- new.env(parent = emptyenv())
  testthat::local_mocked_bindings(
    util_metrics_to_classes = function(rs_table_long, meta_data, ...) {
      state$meta_data <- meta_data
      rs_table_long$class <- c("5", "1")
      rs_table_long
    },
    util_get_colors = function() c(`1` = "#eeeeee", `5` = "#aa0000"),
    util_get_labels_grading_class = function() {
      c(`1` = "Ok", `5` = "Critical")
    },
    util_translate_indicator_metrics = function(x, ...) paste("Metric", x),
    .package = "dataquieR"
  )

  summary_data <- util_questionnaire_summary_data(sections)
  expect_identical(nrow(summary_data), 1L)
  expect_identical(summary_data$class_label, "Critical")
  expect_identical(summary_data$color, "#aa0000")
  expect_identical(summary_data$anchor, "questionnaire-section-01")
  expect_identical(state$meta_data[[GRADING_RULESET]], "0")

  html <- as.character(util_questionnaire_summary_html(sections))
  expect_match(html, "background:#aa0000", fixed = TRUE)
  expect_match(html, "Critical: 25%", fixed = TRUE)
  expect_match(html, 'href="#questionnaire-section-01"', fixed = TRUE)
  expect_match(html, 'id="questionnaire-summary"', fixed = TRUE)
})

test_that("questionnaire HTML content adds navigation for every section", {
  skip_on_cran()

  first <- questionnaire_html_test_result(title = "Scale A")
  second <- questionnaire_html_test_result(
    variable_name = "SCALE_B",
    title = "Scale B"
  )
  attr(second, "function_name") <- "second_indicator"
  questionnaire <- util_questionnaire_results(list(
    first = first,
    second = second
  ))

  testthat::local_mocked_bindings(
    util_pretty_print = function(dqr, is_ssi, link_variables, ...) {
      expect_true(is_ssi)
      expect_false(link_variables)
      expect_null(util_attr(dqr, "dq_result_anchor", exact = TRUE))
      htmltools::div(class = "rendered-result")
    },
    .package = "dataquieR"
  )

  rendered <- util_questionnaire_html_content(
    questionnaire,
    dir = withr::local_tempdir(),
    use_plot_ly = FALSE
  )
  html <- paste0(
    as.character(rendered$menu),
    as.character(rendered$content)
  )
  expect_match(html, "floatbar", fixed = TRUE)
  expect_match(html, 'id="questionnaire-section-01"', fixed = TRUE)
  expect_match(html, 'href="#questionnaire-section-02"', fixed = TRUE)
  expect_match(html, "Scale A", fixed = TRUE)
  expect_match(html, "Scale B", fixed = TRUE)

  summary_menu <- util_questionnaire_navigation_menu(
    util_questionnaire_html_sections(questionnaire),
    have_summary = TRUE
  )
  expect_match(
    as.character(summary_menu),
    'href="#questionnaire-summary"',
    fixed = TRUE
  )

  empty_menu <- util_questionnaire_navigation_menu(
    util_questionnaire_html_sections(first),
    have_summary = FALSE
  )
  expect_false(grepl("questionnaire-summary", as.character(empty_menu)))
})

test_that("questionnaire summary helpers handle empty legacy results", {
  skip_on_cran()

  result <- questionnaire_html_test_result()
  attr(result, "r_summary") <- NULL
  sections <- util_questionnaire_html_sections(result)
  expect_identical(util_questionnaire_summary_data(sections), data.frame())
  expect_null(util_questionnaire_summary_html(sections))

  attr(result, "r_summary") <- data.frame(unexpected = 1)
  sections <- util_questionnaire_html_sections(result)
  expect_identical(util_questionnaire_summary_data(sections), data.frame())

  duplicated <- util_questionnaire_add_section_anchors(list(
    first = result,
    second = result
  ))
  attr(duplicated[[2]], "dq_result_anchor") <- "questionnaire-section-01"
  repaired <- util_questionnaire_add_missing_section_anchors(duplicated)
  expect_identical(
    util_attr(repaired[[2]], "dq_result_anchor", exact = TRUE),
    "questionnaire-section-02"
  )
})
