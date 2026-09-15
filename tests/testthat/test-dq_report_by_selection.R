test_that("dq_report_by selects and excludes strata by values and labels", {
  skip_on_cran()
  skip_if_not_installed("stringdist")

  study_data <- data.frame(
    CENTER = c("A", "A", "B", "B", "C", "C"),
    val = 1:6
  )
  meta_data <- data.frame(
    VAR_NAMES = c("val", "CENTER"),
    LABEL = c("Value", "Center"),
    DATA_TYPE = c("integer", "string"),
    SCALE_LEVEL = c("metric", "nominal"),
    MISSING_LIST = c("", ""),
    JUMP_LIST = c("", ""),
    VALUE_LABELS = c(NA, "A = Alpha | B = Beta | C = Gamma"),
    VARIABLE_ROLE = "primary",
    stringsAsFactors = FALSE
  )

  selected_by_value <- suppressMessages(suppressWarnings(dq_report_by(
    study_data,
    meta_data,
    label_col = "LABEL",
    strata_column = "CENTER",
    strata_select = "B",
    selection_type = "value",
    segment_column = NULL,
    dimensions = "int",
    filter_indicator_functions = "int_datatype_matrix",
    cores = NULL,
    view = FALSE
  )))
  selected_by_label <- suppressMessages(suppressWarnings(dq_report_by(
    study_data,
    meta_data,
    label_col = "LABEL",
    strata_column = "CENTER",
    strata_select = "Beta",
    selection_type = "v_label",
    segment_column = NULL,
    dimensions = "int",
    filter_indicator_functions = "int_datatype_matrix",
    cores = NULL,
    view = FALSE
  )))
  excluded_by_value <- suppressMessages(suppressWarnings(dq_report_by(
    study_data,
    meta_data,
    label_col = "LABEL",
    strata_column = "CENTER",
    strata_exclude = "B",
    selection_type = "value",
    segment_column = NULL,
    dimensions = "int",
    filter_indicator_functions = "int_datatype_matrix",
    cores = NULL,
    view = FALSE
  )))
  excluded_by_label <- suppressMessages(suppressWarnings(dq_report_by(
    study_data,
    meta_data,
    label_col = "LABEL",
    strata_column = "CENTER",
    strata_exclude = "Beta",
    selection_type = "v_label",
    segment_column = NULL,
    dimensions = "int",
    filter_indicator_functions = "int_datatype_matrix",
    cores = NULL,
    view = FALSE
  )))
  selected_by_detected_label <- suppressMessages(suppressWarnings(dq_report_by(
    study_data,
    meta_data,
    label_col = "LABEL",
    strata_column = "CENTER",
    strata_select = "et",
    segment_column = NULL,
    dimensions = "int",
    filter_indicator_functions = "int_datatype_matrix",
    cores = NULL,
    view = FALSE
  )))
  selected_by_detected_value <- suppressMessages(suppressWarnings(dq_report_by(
    study_data,
    meta_data,
    label_col = "LABEL",
    strata_column = "CENTER",
    strata_select = "B",
    segment_column = NULL,
    dimensions = "int",
    filter_indicator_functions = "int_datatype_matrix",
    cores = NULL,
    view = FALSE
  )))
  selected_by_detected_regex <- suppressMessages(suppressWarnings(dq_report_by(
    study_data,
    meta_data,
    label_col = "LABEL",
    strata_column = "CENTER",
    strata_select = "mm",
    segment_column = NULL,
    dimensions = "int",
    filter_indicator_functions = "int_datatype_matrix",
    cores = NULL,
    view = FALSE
  )))
  excluded_by_detected_label <- suppressMessages(suppressWarnings(dq_report_by(
    study_data,
    meta_data,
    label_col = "LABEL",
    strata_column = "CENTER",
    strata_exclude = "Beta",
    segment_column = NULL,
    dimensions = "int",
    filter_indicator_functions = "int_datatype_matrix",
    cores = NULL,
    view = FALSE
  )))
  excluded_by_detected_value <- suppressMessages(suppressWarnings(dq_report_by(
    study_data,
    meta_data,
    label_col = "LABEL",
    strata_column = "CENTER",
    strata_exclude = "B",
    segment_column = NULL,
    dimensions = "int",
    filter_indicator_functions = "int_datatype_matrix",
    cores = NULL,
    view = FALSE
  )))
  excluded_by_detected_regex <- suppressMessages(suppressWarnings(dq_report_by(
    study_data,
    meta_data,
    label_col = "LABEL",
    strata_column = "CENTER",
    strata_exclude = "mm",
    segment_column = NULL,
    dimensions = "int",
    filter_indicator_functions = "int_datatype_matrix",
    cores = NULL,
    view = FALSE
  )))

  expect_named(selected_by_value$all_variables, "Center_B")
  expect_named(selected_by_label$all_variables, "Center_B")
  expect_named(selected_by_detected_label$all_variables, "Center_B")
  expect_named(selected_by_detected_value$all_variables, "Center_B")
  expect_named(selected_by_detected_regex$all_variables, "Center_C")
  expect_named(excluded_by_value$all_variables, c("Center_A", "Center_C"))
  expect_named(excluded_by_label$all_variables, c("Center_A", "Center_C"))
  expect_named(
    excluded_by_detected_label$all_variables,
    c("Center_A", "Center_C")
  )
  expect_named(
    excluded_by_detected_value$all_variables,
    c("Center_A", "Center_C")
  )
  expect_named(
    excluded_by_detected_regex$all_variables,
    c("Center_A", "Center_B")
  )
})

test_that("dq_report_by rejects unmatched explicit strata selections", {
  skip_on_cran()
  skip_if_not_installed("stringdist")

  study_data <- data.frame(
    CENTER = c("A", "A", "B", "B", "C", "C"),
    val = 1:6
  )
  meta_data <- data.frame(
    VAR_NAMES = c("val", "CENTER"),
    LABEL = c("Value", "Center"),
    DATA_TYPE = c("integer", "string"),
    SCALE_LEVEL = c("metric", "nominal"),
    MISSING_LIST = c("", ""),
    JUMP_LIST = c("", ""),
    VALUE_LABELS = c(NA, "A = Alpha | B = Beta | C = Gamma"),
    VARIABLE_ROLE = "primary",
    stringsAsFactors = FALSE
  )
  base_args <- list(
    study_data = study_data,
    meta_data = meta_data,
    label_col = "LABEL",
    strata_column = "CENTER",
    segment_column = NULL,
    dimensions = "int",
    filter_indicator_functions = "int_datatype_matrix",
    cores = NULL,
    view = FALSE
  )

  expect_error(
    suppressMessages(do.call(dq_report_by, c(base_args, list(
      strata_select = "Z",
      selection_type = "value"
    )))),
    "No values in the variable correspond"
  )
  expect_error(
    suppressMessages(do.call(dq_report_by, c(base_args, list(
      strata_select = "Z",
      selection_type = "v_label"
    )))),
    "No value label in the variable correspond"
  )
  expect_error(
    suppressMessages(do.call(dq_report_by, c(base_args, list(
      strata_exclude = ".",
      selection_type = "regex"
    )))),
    "No strata_column stratum remains"
  )
  expect_error(
    suppressMessages(do.call(dq_report_by, c(base_args, list(
      strata_exclude = "Z"
    )))),
    "does not corresponds to any strata"
  )
})

test_that("dq_report_by uses STUDY_SEGMENT as default split metadata", {
  skip_on_cran()
  skip_if_not_installed("stringdist")

  study_data <- data.frame(
    CENTER = c("A", "A", "B", "B", "C", "C"),
    val = 1:6
  )
  meta_data <- data.frame(
    VAR_NAMES = c("val", "CENTER"),
    LABEL = c("Value", "Center"),
    DATA_TYPE = c("integer", "string"),
    SCALE_LEVEL = c("metric", "nominal"),
    MISSING_LIST = c("", ""),
    JUMP_LIST = c("", ""),
    VALUE_LABELS = c(NA, "A = Alpha | B = Beta | C = Gamma"),
    VARIABLE_ROLE = "primary",
    STUDY_SEGMENT = c("main", "ids"),
    stringsAsFactors = FALSE
  )

  report <- suppressMessages(suppressWarnings(dq_report_by(
    study_data,
    meta_data,
    label_col = "LABEL",
    dimensions = "int",
    filter_indicator_functions = "int_datatype_matrix",
    cores = NULL,
    view = FALSE
  )))

  expect_named(report, c("ids", "main"))
  expect_named(report$ids, "all_observations")
  expect_named(report$main, "all_observations")
  expect_s3_class(report$ids$all_observations, "dataquieR_resultset2")
  expect_s3_class(report$main$all_observations, "dataquieR_resultset2")
  expect_equal(
    util_attr(report$ids$all_observations, "rn", exact = TRUE),
    "Center"
  )
  expect_equal(
    util_attr(report$main$all_observations, "rn", exact = TRUE),
    "Value"
  )
})

test_that("dq_report_by selects and excludes STUDY_SEGMENT levels", {
  skip_on_cran()
  skip_if_not_installed("stringdist")

  study_data <- data.frame(
    CENTER = c("A", "A", "B", "B"),
    val = 1:4
  )
  meta_data <- data.frame(
    VAR_NAMES = c("val", "CENTER"),
    LABEL = c("Value", "Center"),
    DATA_TYPE = c("integer", "string"),
    SCALE_LEVEL = c("metric", "nominal"),
    MISSING_LIST = c("", ""),
    JUMP_LIST = c("", ""),
    VALUE_LABELS = c(NA, "A = Alpha | B = Beta"),
    VARIABLE_ROLE = "primary",
    STUDY_SEGMENT = c("main", "ids"),
    stringsAsFactors = FALSE
  )

  selected <- suppressMessages(suppressWarnings(dq_report_by(
    study_data,
    meta_data,
    label_col = "LABEL",
    segment_select = "main",
    dimensions = "int",
    filter_indicator_functions = "int_datatype_matrix",
    cores = NULL,
    view = FALSE
  )))
  excluded <- suppressMessages(suppressWarnings(dq_report_by(
    study_data,
    meta_data,
    label_col = "LABEL",
    segment_exclude = "ids",
    dimensions = "int",
    filter_indicator_functions = "int_datatype_matrix",
    cores = NULL,
    view = FALSE
  )))

  expect_named(selected, "main")
  expect_named(excluded, "main")
  expect_named(selected$main, "all_observations")
  expect_named(excluded$main, "all_observations")
})

test_that("dq_report_by records subgroup rules in unsplit report subtitles", {
  skip_on_cran()
  skip_if_not_installed("stringdist")

  study_data <- data.frame(
    CENTER = c("A", "A", "B", "B"),
    val = 1:4
  )
  meta_data <- data.frame(
    VAR_NAMES = c("val", "CENTER"),
    LABEL = c("Value", "Center"),
    DATA_TYPE = c("integer", "string"),
    SCALE_LEVEL = c("metric", "nominal"),
    MISSING_LIST = c("", ""),
    JUMP_LIST = c("", ""),
    VALUE_LABELS = c(NA, "A = Alpha | B = Beta"),
    VARIABLE_ROLE = "primary",
    stringsAsFactors = FALSE
  )

  report <- suppressMessages(suppressWarnings(dq_report_by(
    study_data,
    meta_data,
    label_col = "LABEL",
    subgroup = "[val] > 2",
    segment_column = NULL,
    dimensions = "int",
    filter_indicator_functions = "int_datatype_matrix",
    cores = NULL,
    view = FALSE
  )))

  resultset <- report$all_variables$all_observations

  expect_s3_class(resultset, "dataquieR_resultset2")
  expect_match(
    util_attr(resultset, "subtitle", exact = TRUE),
    "Subgroup: \\[val\\] > 2"
  )
})

test_that(
  "dq_report_by requires explicit unsplit reports without split metadata",
  {
    skip_on_cran()
    skip_if_not_installed("stringdist")

    study_data <- data.frame(val = 1:3)
    meta_data <- data.frame(
      VAR_NAMES = "val",
      LABEL = "Value",
      DATA_TYPE = "integer",
      SCALE_LEVEL = "metric",
      MISSING_LIST = "",
      JUMP_LIST = "",
      VARIABLE_ROLE = "primary",
      stringsAsFactors = FALSE
    )

    expect_error(
      suppressMessages(dq_report_by(
        study_data,
        meta_data,
        label_col = "LABEL",
        dimensions = "int",
        filter_indicator_functions = "int_datatype_matrix",
        cores = NULL,
        view = FALSE
      )),
      "No information for split provided"
    )
  }
)

test_that("dq_report_by rejects unknown split metadata early", {
  skip_on_cran()
  skip_if_not_installed("stringdist")

  study_data <- data.frame(
    CENTER = c("A", "B"),
    val = 1:2
  )
  meta_data <- data.frame(
    VAR_NAMES = c("val", "CENTER"),
    LABEL = c("Value", "Center"),
    DATA_TYPE = c("integer", "string"),
    SCALE_LEVEL = c("metric", "nominal"),
    MISSING_LIST = c("", ""),
    JUMP_LIST = c("", ""),
    VALUE_LABELS = c(NA, "A|B"),
    VARIABLE_ROLE = "primary",
    stringsAsFactors = FALSE
  )
  base_args <- list(
    study_data = study_data,
    meta_data = meta_data,
    label_col = "LABEL",
    dimensions = "int",
    filter_indicator_functions = "int_datatype_matrix",
    cores = NULL,
    view = FALSE
  )

  expect_error(
    suppressMessages(do.call(dq_report_by, c(base_args, list(
      segment_column = "NOPE"
    )))),
    "No metadata attribute"
  )
  expect_error(
    suppressMessages(do.call(dq_report_by, c(base_args, list(
      segment_column = "NOPE",
      strata_column = "CENTER"
    )))),
    "No metadata attribute"
  )
  expect_error(
    suppressMessages(do.call(dq_report_by, c(base_args, list(
      segment_column = NULL,
      strata_column = "NOPE"
    )))),
    "strata_column provided does not correpond"
  )
  expect_error(
    suppressMessages(do.call(dq_report_by, c(base_args, list(
      segment_column = "VARIABLE_ROLE",
      strata_column = "NOPE"
    )))),
    "strata_column provided does not correpond"
  )
})

test_that("dq_report_by rejects inconsistent split arguments early", {
  skip_on_cran()

  expect_error(
    dq_report_by(strata_select = "A"),
    "strata_column is needed for selecting the strata",
    fixed = TRUE
  )
  expect_error(
    dq_report_by(strata_exclude = "A"),
    "strata_column is needed for excluding the strata",
    fixed = TRUE
  )
  expect_error(
    dq_report_by(selection_type = "value"),
    "selection_type can only be specified",
    fixed = TRUE
  )
  expect_error(
    dq_report_by(
      strata_column = "CENTER",
      strata_select = "A",
      selection_type = "label"
    ),
    'The selection_type can only be "value", "v_label", or "regex"',
    fixed = TRUE
  )
  expect_error(
    dq_report_by(subgroup = c("[a] == 1", "[b] == 2")),
    regexp = "Need exactly one element in argument subgroup"
  )
})

test_that("dq_report_by validates report path arguments before loading data", {
  skip_on_cran()

  expect_error(
    dq_report_by(
      input_dir = file.path(tempdir(), "missing-qif-input-dir")
    ),
    "does not exist. Provide an 'input_dir'",
    fixed = TRUE
  )
  expect_error(
    dq_report_by(
      also_print = TRUE,
      output_dir = c(tempdir(), tempdir())
    ),
    regexp = "Need exactly one element in argument output_dir"
  )
  expect_error(
    dq_report_by(
      also_print = TRUE,
      output_dir = tempdir(),
      force_overwrite = c(TRUE, FALSE)
    ),
    regexp = "Need exactly one element in argument force_overwrite"
  )
})
