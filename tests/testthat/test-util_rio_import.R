skip_on_cran()

test_that("rio import warning helper detects metadata read with keep_types", {
  metadata_like <- data.frame(
    VAR_NAMES = "age",
    DATA_TYPE = DATA_TYPES$INTEGER,
    LABEL = "Age",
    VALUE_LABELS = NA_character_
  )

  expect_message(
    .util_warn_on_possible_study_data_read_with_keep_types_false_or_vv(
      metadata_like,
      fn = "metadata.xlsx",
      keep_types = TRUE
    ),
    "look like.*meta data"
  )
})

test_that(
  "rio import warning helper detects study data read as text metadata",
  {
    study_like <- data.frame(
      age = c("20", "40"),
      sbp = c("120", "130"),
      dbp = c("80", "90")
    )

    expect_message(
      .util_warn_on_possible_study_data_read_with_keep_types_false_or_vv(
        study_like,
        fn = "study_data.csv",
        keep_types = FALSE
      ),
      "look like.*meta data"
    )
  }
)

test_that("rio import warning helper recurses through workbook-like lists", {
  workbook <- list(
    metadata = data.frame(
      VAR_NAMES = "age",
      DATA_TYPE = DATA_TYPES$INTEGER,
      LABEL = "Age"
    ),
    study_data = data.frame(
      age = c("20", "40"),
      sbp = c("120", "130")
    )
  )

  expect_message(
    .util_warn_on_possible_study_data_read_with_keep_types_false_or_vv(
      workbook,
      fn = "workbook.xlsx",
      keep_types = TRUE
    ),
    "look like.*meta data"
  )
  expect_message(
    .util_warn_on_possible_study_data_read_with_keep_types_false_or_vv(
      workbook,
      fn = "workbook.xlsx",
      keep_types = FALSE
    ),
    "look like.*meta data"
  )
})

test_that("rio import converts Excel time-only POSIXct columns", {
  time_only <- as.POSIXct(
    c("1899-12-30 01:02:03", "1899-12-30 04:05:06", NA),
    tz = "UTC"
  )
  date_time <- as.POSIXct(
    c("2020-01-01 01:02:03", "2020-01-02 04:05:06", NA),
    tz = "UTC"
  )
  input <- data.frame(
    clock = time_only,
    stamp = date_time,
    value = 1:3
  )

  converted <- .util_convert_time_only_cols_df(input)

  expect_false(inherits(converted$clock, "POSIXct"))
  expect_equal(as.numeric(converted$clock), c(3723, 14706, NA))
  expect_s3_class(converted$stamp, "POSIXct")
  expect_identical(converted$value, input$value)
})

test_that("rio import leaves non-time-only inputs unchanged", {
  all_missing_time <- as.POSIXct(c(NA, NA), origin = "1970-01-01", tz = "UTC")
  input <- data.frame(clock = all_missing_time)

  expect_false(.util_is_excel_time_only(all_missing_time))
  expect_identical(.util_convert_time_only_cols_df(input), input)
  expect_identical(.util_convert_time_only_cols_df(1:3), 1:3)
  expect_identical(
    .util_convert_time_only_cols_df(data.frame()),
    data.frame()
  )
})

test_that("rio import time-only conversion falls back without hms", {
  time_only <- as.POSIXct(
    c("1899-12-30 01:00:00", NA),
    tz = "UTC"
  )

  testthat::with_mocked_bindings(
    .package = "base",
    requireNamespace = function(package, quietly = FALSE) FALSE,
    {
      converted <- .util_posixct_to_hms(time_only)
    }
  )

  expect_s3_class(converted, "difftime")
  expect_equal(as.numeric(converted), c(3600, NA))
  expect_identical(attr(converted, "units", exact = TRUE), "secs")
})

test_that("rio import wrapper normalizes local csv text reads", {
  skip_on_cran()

  csv <- tempfile(fileext = ".csv")
  writeLines(c(
    "a,b",
    "1,NA",
    "-,text",
    "\"\",n/a"
  ), csv)

  expect_message(
    text_read <- util_rio_import(csv, keep_types = FALSE),
    "Found only few"
  )
  expect_equal(text_read$a, c("1", NA_character_, NA_character_))
  expect_equal(text_read$b, c(NA_character_, "text", NA_character_))

  typed_read <- suppressMessages(util_rio_import(csv, keep_types = TRUE))
  expect_type(typed_read$a, "integer")
  expect_equal(typed_read$a, c(1L, NA_integer_, NA_integer_))
})

test_that("rio import lambda can re-adjust text-read csv data", {
  skip_on_cran()

  csv <- tempfile(fileext = ".csv")
  writeLines(c(
    "score,group,date",
    "1,a,2020-01-01",
    "2,b,2020-01-02"
  ), csv)

  withr::local_options(dataquieR.fix_column_type_on_read = TRUE)

  imported <- .util_rio_import_lambda(
    csv,
    keep_types = TRUE,
    lambda = rio::import
  )

  expect_type(imported$score, "double")
  expect_equal(imported$score, c(1, 2))
  expect_type(imported$group, "character")
  expect_s3_class(imported$date, "POSIXct")
})

test_that("rio import lambda reports columns with unknown adjusted types", {
  skip_on_cran()

  csv <- tempfile(fileext = ".csv")
  writeLines(c(
    "score,group",
    "1,a",
    "2,b"
  ), csv)

  withr::local_options(dataquieR.fix_column_type_on_read = TRUE)
  testthat::local_mocked_bindings(
    prep_robust_guess_data_type = function(...) NA
  )

  expect_warning(
    imported <- .util_rio_import_lambda(
      csv,
      keep_types = TRUE,
      lambda = rio::import
    ),
    "Could not adjust the data type"
  )

  expect_equal(imported$score, c("1", "2"))
  expect_equal(imported$group, c("a", "b"))
})

test_that("rio import list wrapper handles local csv files", {
  skip_on_cran()

  csv <- tempfile(fileext = ".csv")
  writeLines(c(
    "a,b",
    "1,text",
    "2,more"
  ), csv)

  expect_no_message(
    text_read <- util_rio_import_list(csv, keep_types = FALSE)
  )
  expect_length(text_read, 1)
  expect_s3_class(text_read[[1]], "data.frame")
  expect_equal(text_read[[1]]$a, c("1", "2"))
  expect_equal(text_read[[1]]$b, c("text", "more"))

  typed_read <- suppressMessages(util_rio_import_list(csv, keep_types = TRUE))
  expect_length(typed_read, 1)
  expect_s3_class(typed_read[[1]], "data.frame")
  expect_type(typed_read[[1]]$a, "integer")
  expect_equal(typed_read[[1]]$a, c(1L, 2L))
})

test_that("rio import list reports ODM files without converter", {
  skip_on_cran()

  odm_file <- tempfile(fileext = ".xml")
  writeLines(
    paste0(
      "<?xml version='1.0'?><ODM ",
      "xmlns='http://www.cdisc.org/ns/odm/v1.3'><Study /></ODM>"
    ),
    odm_file,
    useBytes = TRUE
  )

  testthat::local_mocked_bindings(
    odm_installed = FALSE
  )

  expect_error(
    util_rio_import_list(odm_file, keep_types = FALSE),
    "dataquieR2odm"
  )
})

test_that("rio import list accepts ODM converter output", {
  skip_on_cran()

  odm_file <- tempfile(fileext = ".xml")
  writeLines(
    paste0(
      "<?xml version='1.0'?><ODM ",
      "xmlns='http://www.cdisc.org/ns/odm/v1.3'><Study /></ODM>"
    ),
    odm_file,
    useBytes = TRUE
  )

  item_level <- data.frame(VAR_NAMES = "age", LABEL = "Age")
  code_list <- data.frame(CODE = "1", LABEL = "yes")

  testthat::local_mocked_bindings(
    odm_installed = TRUE,
    util_odm2dataquieR = function(fn) {
      expect_identical(fn, odm_file)
      list(
        item_level = item_level,
        CODE_LIST_TABLE = code_list,
        ignored_sheet = data.frame(x = 1)
      )
    }
  )

  imported <- util_rio_import_list(odm_file, keep_types = FALSE)

  expect_named(imported, c("item_level", CODE_LIST_TABLE))
  expect_equal(imported$item_level, item_level)
  expect_equal(imported[[CODE_LIST_TABLE]], code_list)
})

test_that("rio import lambda validates control arguments", {
  skip_on_cran()

  csv <- tempfile(fileext = ".csv")
  writeLines(c("a", "1"), csv)

  expect_error(
    .util_rio_import_lambda(csv, keep_types = "yes", lambda = rio::import),
    "keep_types"
  )
  expect_error(
    .util_rio_import_lambda(csv, keep_types = TRUE, lambda = identity),
    "invalid lambda"
  )
})

test_that("rio import retry helper retries transient URL errors", {
  skip_on_cran()

  state <- new.env(parent = emptyenv())
  state$attempts <- 0L
  withr::local_options(list(
    dataquieR.url_import_attempts = 2L,
    dataquieR.url_import_retry_delay = 0.001
  ))

  expect_message(
    result <- .util_rio_import_with_retry(
      "https://example.org/data.csv",
      function() {
        state$attempts <- state$attempts + 1L
        if (state$attempts == 1L) {
          return(try(stop("HTTP error 503"), silent = TRUE))
        }
        data.frame(x = 1)
      }
    ),
    "Transient network error"
  )

  expect_equal(state$attempts, 2L)
  expect_equal(result, data.frame(x = 1))
})
