test_that("schema dependency diagnostics use the requested format", {
  skip_on_cran()
  prep_purge_data_frame_cache()
  withr::defer(prep_purge_data_frame_cache())

  study_data <- data.frame(id = "1", json = '{"a":1}')
  meta_data <- data.frame(
    VAR_NAMES = c("id", "json"),
    DATA_TYPE = c("string", "string"),
    STUDY_SEGMENT = c("A", "A"),
    STRUCTURED_TEXT_DATA_TYPE = c(NA_character_, "json"),
    STRUCTURED_TEXT_SCHEMA = c(NA_character_, '{"type":"object"}'),
    stringsAsFactors = FALSE
  )
  meta_data_segment <- data.frame(
    STUDY_SEGMENT = "A",
    SEGMENT_ID_VARS = "id"
  )
  called <- new.env(parent = emptyenv())
  called$goal <- NULL
  testthat::local_mocked_bindings(
    util_ensure_suggested = function(pkg, goal) {
      if (identical(pkg, "jsonvalidate")) {
        called$goal <- goal
        util_error("Dependency check reached")
      }
      TRUE
    }
  )

  invisible(try(suppressWarnings(con_schema_nonadherence(
    resp_vars = "json",
    study_data = study_data,
    meta_data = meta_data,
    meta_data_segment = meta_data_segment,
    label_col = VAR_NAMES
  )), silent = TRUE))

  expect_identical(called$goal, "To check for schema adherence in json")
})

test_that("schema validation categories map to the intended indicators", {
  skip_on_cran()

  categories <- c(
    type = "int_vfe_type",
    maxItems = "int_sts_countel",
    minItems = "int_sts_countel",
    maxLength = "int_vfe_inhom",
    minLength = "int_vfe_inhom",
    maxProperties = "int_sts_countel",
    minProperties = "int_sts_countel",
    additionalItems = "int_sts_countel",
    additionalProperties = "int_sts_element",
    dependencies = "con_con_contc",
    format = "int_vfe_type",
    maximum = "con_rvv_inum",
    minimum = "con_rvv_inum",
    exclusiveMaximum = "con_rvv_inum",
    exclusiveMinimum = "con_rvv_inum",
    multipleOf = "con_rvv_inum",
    pattern = "int_vfe_inhom",
    required = "int_sts_structure",
    propertyNames = "int_sts_element",
    enum = "con_rvv_icat",
    SyntaxErrors = "syntax error"
  )

  for (category in names(categories)) {
    expect_identical(
      .util_classify_validation_error("json", category),
      unname(categories[[category]]),
      info = category
    )
  }
  expect_identical(
    .util_classify_validation_error("json", "unknown"),
    "uncategorized error"
  )
  expect_identical(
    .util_classify_validation_error("xml", "FoundSyntaxErrors"),
    "syntax error"
  )
  expect_identical(
    .util_classify_validation_error("xml", "unknown"),
    "uncategorized error"
  )
})

test_that("JSON schema checks retain names for one response variable", {
  skip_on_cran()
  skip_if_not_installed("jsonvalidate")
  prep_purge_data_frame_cache()
  withr::defer(prep_purge_data_frame_cache())

  study_data <- data.frame(
    id = c("1", "2"),
    json = c('{"a":1}', '{"a":"not a number"}'),
    stringsAsFactors = FALSE
  )
  schema <- paste0(
    '{"type":"object","properties":',
    '{"a":{"type":"number"}},"required":["a"]}'
  )
  meta_data <- data.frame(
    VAR_NAMES = c("id", "json"),
    DATA_TYPE = c("string", "string"),
    STUDY_SEGMENT = c("A", "A"),
    STRUCTURED_TEXT_DATA_TYPE = c(NA_character_, "json"),
    STRUCTURED_TEXT_SCHEMA = c(NA_character_, schema),
    stringsAsFactors = FALSE
  )
  meta_data_segment <- data.frame(
    STUDY_SEGMENT = "A",
    SEGMENT_ID_VARS = "id"
  )

  result <- suppressWarnings(con_schema_nonadherence(
    resp_vars = "json",
    study_data = study_data,
    meta_data = meta_data,
    meta_data_segment = meta_data_segment,
    label_col = VAR_NAMES
  ))

  summary <- as.data.frame(result$SummaryTable)
  expect_equal(summary[["Variables"]], "json", ignore_attr = TRUE)
  expect_equal(summary[["NUM_total_errors"]], 1, ignore_attr = TRUE)
  expect_equal(summary[["PCT_total_errors"]], 50, ignore_attr = TRUE)
  expect_equal(summary[["NUM_int_vfe_type"]], 1, ignore_attr = TRUE)
  expect_s3_class(result$FlaggedStudyData, "FlaggedStudyData")
  expect_equal(nrow(result$FlaggedStudyData), 1)

  study_data$json[[2]] <- '{"a":2}'
  valid_result <- suppressWarnings(con_schema_nonadherence(
    study_data = study_data,
    meta_data = meta_data,
    meta_data_segment = meta_data_segment,
    label_col = VAR_NAMES
  ))
  expect_equal(valid_result$SummaryTable[["NUM_total_errors"]], 0,
    ignore_attr = TRUE)
  expect_equal(nrow(valid_result$FlaggedStudyData), 0)
})

test_that("JSON schema checks count malformed documents separately", {
  skip_on_cran()
  skip_if_not_installed("jsonvalidate")
  prep_purge_data_frame_cache()
  withr::defer(prep_purge_data_frame_cache())

  study_data <- data.frame(
    id = c("1", "2"),
    json = c('{"a":1}', '{"a":'),
    stringsAsFactors = FALSE
  )
  meta_data <- data.frame(
    VAR_NAMES = c("id", "json"),
    DATA_TYPE = c("string", "string"),
    STUDY_SEGMENT = c("A", "A"),
    STRUCTURED_TEXT_DATA_TYPE = c(NA_character_, "json"),
    STRUCTURED_TEXT_SCHEMA = c(NA_character_, '{"type":"object"}'),
    STRUCTURED_TEXT_SCHEMA_REFERENCE = c(NA_character_, NA_character_),
    stringsAsFactors = FALSE
  )
  meta_data_segment <- data.frame(
    STUDY_SEGMENT = "A",
    SEGMENT_ID_VARS = "id"
  )

  result <- suppressWarnings(suppressMessages(con_schema_nonadherence(
    resp_vars = "json",
    study_data = study_data,
    meta_data = meta_data,
    meta_data_segment = meta_data_segment,
    label_col = VAR_NAMES
  )))

  expect_equal(result$SummaryTable[["NUM_total_errors"]], 1,
    ignore_attr = TRUE)
  expect_equal(result$SummaryTable[["PCT_total_errors"]], 100,
    ignore_attr = TRUE)
  expect_equal(nrow(result$FlaggedStudyData), 1)
})

test_that("schema checks distinguish missing and unsupported metadata", {
  skip_on_cran()
  prep_purge_data_frame_cache()
  withr::defer(prep_purge_data_frame_cache())

  study_data <- data.frame(
    id = "1",
    json = '{"a":1}',
    without_schema = '{"a":1}',
    unsupported = "a: 1"
  )
  schema <- '{"type":"object"}'
  meta_data <- data.frame(
    VAR_NAMES = names(study_data),
    DATA_TYPE = rep("string", 4),
    STUDY_SEGMENT = rep("A", 4),
    STRUCTURED_TEXT_DATA_TYPE = c(NA, "json", "json", "yaml"),
    STRUCTURED_TEXT_SCHEMA = c(NA, schema, NA, schema)
  )
  meta_data_segment <- data.frame(
    STUDY_SEGMENT = "A", SEGMENT_ID_VARS = "id"
  )
  check <- function(vars, metadata = meta_data) {
    con_schema_nonadherence(
      resp_vars = vars,
      study_data = study_data,
      meta_data = metadata,
      meta_data_segment = meta_data_segment,
      label_col = VAR_NAMES
    )
  }

  no_schema_column <- meta_data
  no_schema_column[[STRUCTURED_TEXT_SCHEMA]] <- NULL
  expect_error(suppressWarnings(check("json", no_schema_column)),
    "no attached validation schema")
  expect_error(suppressWarnings(check("unsupported")),
    "incorrect data type")
  expect_error(suppressWarnings(check("without_schema")),
    "No schema given")

  skip_if_not_installed("jsonvalidate")
  result <- suppressMessages(suppressWarnings(check(
    c("json", "without_schema", "unsupported")
  )))
  expect_equal(as.character(result$SummaryTable[["Variables"]]), "json")
  expect_equal(result$SummaryTable[["NUM_total_errors"]], 0,
    ignore_attr = TRUE)
})

test_that("XML schema checks classify invalid documents", {
  skip_on_cran()
  skip_if_not_installed("xml2")
  prep_purge_data_frame_cache()
  withr::defer(prep_purge_data_frame_cache())

  study_data <- data.frame(
    id = c("1", "2"),
    xml = c("<a>1</a>", "<a>not-a-number</a>")
  )
  schema <- paste0(
    '<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">',
    '<xs:element name="a" type="xs:integer"/>',
    "</xs:schema>"
  )
  meta_data <- data.frame(
    VAR_NAMES = c("id", "xml"),
    DATA_TYPE = c("string", "string"),
    STUDY_SEGMENT = c("A", "A"),
    STRUCTURED_TEXT_DATA_TYPE = c(NA, "xml"),
    STRUCTURED_TEXT_SCHEMA = c(NA, schema)
  )
  meta_data_segment <- data.frame(
    STUDY_SEGMENT = "A", SEGMENT_ID_VARS = "id"
  )

  result <- suppressWarnings(con_schema_nonadherence(
    resp_vars = "xml",
    study_data = study_data,
    meta_data = meta_data,
    meta_data_segment = meta_data_segment,
    label_col = VAR_NAMES
  ))
  expect_equal(result$SummaryTable[["NUM_total_errors"]], 1,
    ignore_attr = TRUE)
  expect_equal(nrow(result$FlaggedStudyData), 1)
})
