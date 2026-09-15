skip_on_cran()

test_that("test_render_report_summary_1", {
  withr::local_options(
    dataquieR.CONDITIONS_WITH_STACKTRACE = TRUE,
    dataquieR.ERRORS_WITH_CALLER = TRUE,
    dataquieR.WARNINGS_WITH_CALLER = TRUE,
    dataquieR.MESSAGES_WITH_CALLER = TRUE
  )
  skip_on_cran() # slow test
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  target <- withr::local_tempdir("testrendersummary")


  sd0 <- head(prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData", keep_types = TRUE), n = 20) # nolint: line_length_linter.
  sd0 <- sd0[, 1:15]

  md0 <- prep_get_data_frame(
    "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx | item_level" # nolint: line_length_linter.
  )

  md0 <- md0[md0$VAR_NAMES %in% colnames(sd0), ]

  r1 <- suppressWarnings(dq_report2(
    study_data = sd0,
    cores = NULL,
    meta_data = md0,
    dimensions = c("Completeness")
  ))

  sumrep <- summary(r1)

  skip_if_not_installed("htmltools")
  skip_if_not_installed("DT")
  skip_if_not_installed("rmarkdown")
  skip_if_not_installed("markdown")

  calnames <- print(sumrep, grouped_by = "call_names")
  indic <- print(sumrep, grouped_by = "indicator_metric")
  combined <- print(sumrep, grouped_by = c("call_names", "indicator_metric"))

  calnames_wide <- attr(calnames, "repsum_wide", exact = TRUE)
  indic_wide <- attr(indic, "repsum_wide", exact = TRUE)
  combined_wide <- attr(combined, "repsum_wide", exact = TRUE)

  expect_true(all(c(
    "Variables",
    "Descr stats-Cat",
    "Descr stats-Cont",
    "Data type error",
    "Invalid encoding",
    "Miss values-Item",
    "Resp-rates-Item",
    "Total"
  ) %in% names(calnames_wide)))
  expect_false(any(grepl("EMPTY_OUTPUT", trimws(names(calnames_wide)),
        fixed = TRUE
      )))

  indicator_summary_columns <-
    trimws(names(indic_wide))

  expect_equal(
    indicator_summary_columns[1:6],
    c(
      "Variables",
      "Data type mismatch (%)",
      "Data type mismatch (cat.)",
      "Inadmissible data format (%)",
      "Inadmissible data format (N)",
      "Uncertain missingness status (%)"
    )
  )
  expect_true("No regular output" %in% indicator_summary_columns)
  expect_false(any(grepl("EMPTY_OUTPUT", indicator_summary_columns,
        fixed = TRUE
      )))
  indicator_summary_html <- as.character(indic)
  expect_match(indicator_summary_html, '"grading_cols":[', fixed = TRUE)
  expect_match(indicator_summary_html, '"grading_order"', fixed = TRUE)
  expect_false(any(grepl("NA is a descriptor, only.",
        indicator_summary_html,
        fixed = TRUE
      )))
  expect_false(any(grepl("<h4>NA of", indicator_summary_html,
        fixed = TRUE
      )))
  expect_true("Total" %in% indicator_summary_columns)
  expect_false(any(grepl("EMPTY_OUTPUT", trimws(names(combined_wide)),
        fixed = TRUE
      )))
  expect_true(any(grepl("No regular output", trimws(names(combined_wide)),
        fixed = TRUE
      )))
  dependency_names <- vapply(htmltools::findDependencies(indic), `[[`, "name",
    FUN.VALUE = character(1)
  )
  expect_true("tippy" %in% dependency_names)
  expect_false("menu" %in% dependency_names)
})

test_that(
  "util_render_table_dataquieR_summary renders empty synthetic summaries",
  {
    skip_on_cran()
    skip_if_not_installed("htmltools")

    summary <- structure(NA, class = "dataquieR_summary")
    this <- new.env(parent = emptyenv())
    this$result <- data.frame(
      VAR_NAMES = character(0),
      indicator_metric = character(0),
      class = character(0),
      stringsAsFactors = FALSE
    )
    this$meta_data <- data.frame(
      VAR_NAMES = c("study_var", "ssi_var"),
      LABEL = c("Study variable", "SSI variable"),
      COMPUTED_VARIABLE_ROLE = c(NA_character_, "scale"),
      stringsAsFactors = FALSE
    )
    this$rownames_of_report <- c("Study variable", "SSI variable")
    this$label_col <- LABEL
    this$labels <- list(cat1 = "Ok")
    this$ordered_call_names <- character(0)
    this$ordered_var_names <- c(
      "Study variable" = "study_var",
      "SSI variable" = "ssi_var"
    )
    attr(summary, "this") <- this

    rendered <- util_render_table_dataquieR_summary(
      summary,
      grouped_by = "call_names",
      vars_to_include = c("study", "ssi")
    )
    wide <- attr(rendered, "repsum_wide", exact = TRUE)

    expect_s3_class(rendered, "shiny.tag.list")
    expect_equal(as.character(rendered), "<p>Empty summary</p>")
    expect_equal(wide, data.frame(), ignore_attr = TRUE)
    expect_identical(attr(wide, "label_col", exact = TRUE), VAR_NAMES)
  }
)

test_that("summary display values hide missing-value tokens", {
  expect_identical(
    util_summary_display_value_missing(c(NA, "NA", " NaN ", "", "0")),
    c(TRUE, TRUE, TRUE, TRUE, FALSE)
  )
})

test_that("summary popup handlers open a result-only iframe", {
  handler <- as.character(util_summary_popup_handler(
    "group.html#nm=variable_group.check_1",
    "group.html#Group one",
    "Group one: Contradictions"
  ))

  expect_match(handler, "showDataquieRResult", fixed = TRUE)
  expect_match(handler, "#nm=variable_group.check_1", fixed = TRUE)
  expect_match(handler, "group.html#Group one", fixed = TRUE)
  expect_false(grepl("navigateDataquieRFrame", handler, fixed = TRUE))

  raw_handler <- util_summary_popup_handler(
    "group.html#nm=variable_group.check_1",
    "group.html#Group one",
    "Group one: Contradictions",
    escape = FALSE
  )
  expect_match(raw_handler, '"group.html#Group one"', fixed = TRUE)
  expect_false(grepl("&quot;", raw_handler, fixed = TRUE))
})

test_that("summary links require both navigation targets", {
  expect_identical(
    util_summary_link_available(
      c("result.html", NA, "result.html", "", "report/.report/NA"),
      c("popup.html", "popup.html", NA, "popup.html", "popup.html")
    ),
    c(TRUE, FALSE, FALSE, FALSE, FALSE)
  )
})

test_that("variable-group summary rows require output or diagnostics", {
  expect_identical(
    util_summary_display_row_available(
      value = c(NA, "NA", "2.08", NA, NA),
      indicator_metric = c("metric", "metric", "metric", "EMPTY_OUTPUT",
        "metric"),
      messages = c("", "", "", "", "diagnostic"),
      classification = c(NA, NA, NA, NA, NA)
    ),
    c(FALSE, FALSE, TRUE, TRUE, TRUE)
  )
})

test_that("variable-group summary matrix renders absent metrics as empty", {
  matrix_values <- data.frame(
    VAR_NAMES = "group_a",
    metric_a = NA_character_,
    metric_b = "2.08",
    stringsAsFactors = FALSE
  )
  value_columns <- setdiff(colnames(matrix_values), VAR_NAMES)

  for (value_column in value_columns) {
    missing_value <- util_summary_display_value_missing(
      matrix_values[[value_column]]
    )
    matrix_values[[value_column]][missing_value] <- ""
  }

  expect_identical(matrix_values$metric_a, "")
  expect_identical(matrix_values$metric_b, "2.08")
})

test_that("summary metrics map to DQ_OBS dimensions", {
  dimensions <- util_summary_metric_dimensions(c(
    "PCT_con_con_contc",
    "NUM_ssc_mls",
    "PCT_ssc_mah",
    "NUM_acc_drm_gold_repeated_call",
    "int_datatype.PCT_datatype",
    "com_item_missingness.PCT_miss",
    "EMPTY_OUTPUT"
  ))

  expect_identical(unname(dimensions), c(
    "Consistency",
    "Consistency",
    "Accuracy",
    "Accuracy",
    "Integrity",
    "Completeness",
    NA_character_
  ))
})

test_that("group summaries prefer available descendant metrics", {
  results <- data.frame(
    row_id = c(
      "parent", "parent-shadow", "logical", "logical-shadow",
      "parent-only", "logical-sibling", "empirical-sibling",
      "classified-parent-shadow", "available-child"
    ),
    VAR_NAMES = c("a", "a", "a", "a", "b", "c", "c", "d", "d"),
    STUDY_SEGMENT = "STUDY",
    call_names = "con_contradictions_redcap",
    indicator_metric = c(
      "NUM_con_con", "NUM_con_con", "NUM_con_con_contc",
      "NUM_con_con_contc", "NUM_con_con", "NUM_con_con_contc",
      "NUM_con_con_contu", "NUM_con_con", "NUM_con_con_contc"
    ),
    values_raw = c(5, NA, 5, NA, 2, 1, 3, NA, 4),
    value = c("5", NA, "5", NA, "2", "1", "3", NA, "4"),
    class = c(NA, NA, 4, NA, 2, 1, 5, 5, 2),
    stringsAsFactors = FALSE
  )

  selected <- util_summary_most_specific_group_metrics(results)

  expect_false(any(selected$row_id %in% c(
    "parent", "parent-shadow", "classified-parent-shadow"
  )))
  expect_true(all(c(
    "logical", "logical-shadow", "parent-only",
    "logical-sibling", "empirical-sibling", "available-child"
  ) %in% selected$row_id))
})

test_that("group metric specificity is separated by metric kind", {
  results <- data.frame(
    VAR_NAMES = "a",
    STUDY_SEGMENT = "STUDY",
    call_names = "con_contradictions_redcap",
    indicator_metric = c("NUM_con_con_contc", "PCT_con_con"),
    values_raw = c(5, 2.5),
    value = c("5", "2.50"),
    class = c(4, 2),
    stringsAsFactors = FALSE
  )

  selected <- util_summary_most_specific_group_metrics(results)

  expect_identical(selected$indicator_metric, results$indicator_metric)
})

test_that("group metric specificity leaves inapplicable inputs unchanged", {
  without_metric <- data.frame(VAR_NAMES = "a")
  unknown_metric <- data.frame(
    VAR_NAMES = "a",
    indicator_metric = "NUM_not_in_dq_obs",
    value = "1"
  )
  diagnostic_only <- data.frame(
    VAR_NAMES = "a",
    indicator_metric = "NUM_con_con",
    value = NA_character_
  )

  expect_identical(
    util_summary_most_specific_group_metrics(without_metric),
    without_metric
  )
  expect_identical(
    util_summary_most_specific_group_metrics(unknown_metric),
    unknown_metric
  )
  expect_identical(
    util_summary_most_specific_group_metrics(diagnostic_only),
    diagnostic_only
  )
})

test_that("multiple group results use an explicit result count", {
  links <- c(
    '<a href="group.html" style="color:#000;">2.08</a>',
    "3.42",
    '<a href="second.html" style="color:#fff;">4.00</a>'
  )

  expect_identical(
    util_summary_replace_link_text(
      links,
      c("3 results", "2 results", "5 results")
    ),
    c(
      '<a href="group.html" style="color:#000;">3 results</a>',
      "2 results",
      '<a href="second.html" style="color:#fff;">5 results</a>'
    )
  )
})

test_that("group result state distinguishes unique and tied worst results", {
  state <- util_summary_group_result_state(
    code = c("a", "a", "a", "b", "b", "c", "c"),
    class_num = c("1", "3", "2", "2", "2", NA, NA),
    has_result = c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE)
  )

  expect_identical(as.integer(state[, "result_count"]),
    c(3L, 3L, 3L, 2L, 2L, 0L, 0L)
  )
  expect_identical(as.integer(state[, "worst_count"]),
    c(1L, 1L, 1L, 2L, 2L, 0L, 0L)
  )
  expect_identical(as.integer(state[, "has_classification"]),
    c(1L, 1L, 1L, 1L, 1L, 0L, 0L)
  )
})

test_that("group summary primary rows partition repeated results exactly", {
  code <- c("a", "a", "a", "b", "b")
  values <- c("2.08", "2.08", "3.00", "0", "1")
  has_result <- c(FALSE, TRUE, TRUE, TRUE, TRUE)
  primary_rows <- util_summary_primary_result_rows(code, has_result)
  remaining_rows <- setdiff(seq_along(code), primary_rows)

  expect_identical(primary_rows, c(2L, 4L))
  expect_identical(values[primary_rows], c("2.08", "0"))
  expect_identical(values[remaining_rows], c("2.08", "3.00", "1"))
  expect_identical(sort(c(primary_rows, remaining_rows)), seq_along(code))
})
