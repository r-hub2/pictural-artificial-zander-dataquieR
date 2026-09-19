test_that("structured text syntax checks count invalid JSON and XML", {
  skip_on_cran()
  skip_if_not_installed("jsonlite")
  skip_if_not_installed("xml2")
  prep_purge_data_frame_cache()
  withr::defer(prep_purge_data_frame_cache())

  study_data <- data.frame(
    json = c('{"a":1}', '{"a":'),
    xml = c("<a/>", "<a>"),
    stringsAsFactors = FALSE
  )
  meta_data <- data.frame(
    VAR_NAMES = c("json", "xml"),
    DATA_TYPE = c("string", "string"),
    STRUCTURED_TEXT_DATA_TYPE = c("json", "xml"),
    stringsAsFactors = FALSE
  )

  result <- suppressWarnings(int_invalid_syntax(
    resp_vars = c("json", "xml"),
    study_data = study_data,
    meta_data = meta_data,
    label_col = VAR_NAMES
  ))

  expect_length(attr(result, "error"), 0)
  summary <- as.data.frame(result$SummaryTable)
  expect_equal(summary[["Variables"]], c("json", "xml"))
  expect_equal(summary[["NUM_int_dsc_format"]], c(1, 1))
  expect_equal(summary[["PCT_int_dsc_format"]], c(50, 50))
})

test_that("structured text dependency diagnostics use the requested format", {
  skip_on_cran()
  prep_purge_data_frame_cache()
  withr::defer(prep_purge_data_frame_cache())

  study_data <- data.frame(yaml = "a: 1")
  meta_data <- data.frame(
    VAR_NAMES = "yaml",
    DATA_TYPE = "string",
    STRUCTURED_TEXT_DATA_TYPE = "yaml"
  )
  called <- new.env(parent = emptyenv())
  called$goal <- NULL
  testthat::local_mocked_bindings(
    util_ensure_suggested = function(pkg, goal) {
      if (identical(pkg, "yaml12")) {
        called$goal <- goal
        util_error("Dependency check reached")
      }
      TRUE
    }
  )

  invisible(try(suppressWarnings(int_invalid_syntax(
    resp_vars = "yaml",
    study_data = study_data,
    meta_data = meta_data,
    label_col = VAR_NAMES
  )), silent = TRUE))

  expect_identical(called$goal, "To check for syntax errors in yaml")
})

test_that("syntax checks exclude unconfigured and unsupported formats", {
  skip_on_cran()
  skip_if_not_installed("jsonlite")
  prep_purge_data_frame_cache()
  withr::defer(prep_purge_data_frame_cache())

  study_data <- data.frame(json = '{"a":1}', other = "plain text")
  meta_data <- data.frame(
    VAR_NAMES = c("json", "other"),
    DATA_TYPE = c("string", "string")
  )
  check <- function(vars, metadata = meta_data) {
    suppressWarnings(int_invalid_syntax(
      resp_vars = vars,
      study_data = study_data,
      meta_data = metadata,
      label_col = VAR_NAMES
    ))
  }

  expect_equal(nrow(check("json")$SummaryTable), 0)
  meta_data[["STRUCTURED_TEXT_DATA_TYPE"]] <- c("json", "plain")
  expect_error(check("other", meta_data), "incorrect data type")
  result <- suppressWarnings(check(c("json", "other"), meta_data))
  expect_equal(as.character(result$SummaryTable[["Variables"]]), "json")
  expect_equal(result$SummaryTable[["NUM_int_dsc_format"]], 0,
    ignore_attr = TRUE)
})

test_that("syntax checks read JSON files as well as inline JSON", {
  skip_on_cran()
  skip_if_not_installed("jsonlite")
  prep_purge_data_frame_cache()
  withr::defer(prep_purge_data_frame_cache())

  folder <- withr::local_tempdir()
  valid <- file.path(folder, "valid.json")
  invalid <- file.path(folder, "invalid.json")
  writeLines('{"a":1}', valid)
  writeLines('{"a":', invalid)
  study_data <- data.frame(json = c(valid, invalid))
  meta_data <- data.frame(
    VAR_NAMES = "json",
    DATA_TYPE = "string",
    STRUCTURED_TEXT_DATA_TYPE = "json"
  )

  result <- suppressWarnings(int_invalid_syntax(
    study_data = study_data,
    meta_data = meta_data,
    label_col = VAR_NAMES
  ))
  expect_equal(result$SummaryTable[["NUM_int_dsc_format"]], 1,
    ignore_attr = TRUE)
  expect_equal(result$SummaryTable[["PCT_int_dsc_format"]], 50,
    ignore_attr = TRUE)
})
