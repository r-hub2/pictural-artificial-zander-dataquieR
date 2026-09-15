skip_on_cran()

test_that("prep_get_data_frame works", {
  skip_on_cran()
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  skip_on_cran()
  prep_purge_data_frame_cache()
  expect_equal(length(prep_list_dataframes()), 0)
  expect_error(
    prep_get_data_frame("", .data_frame_list = cars),
    "must be an environment"
  )
  expect_error(
    prep_get_data_frame("ship"),
    "shortcuts in test"
  )
  expect_error(
    prep_get_data_frame("ship_subset1"),
    "shortcuts in test"
  )
  expect_error(
    prep_get_data_frame("ship_meta_v2"),
    "shortcuts in test"
  )
  expect_error(
    prep_get_data_frame("ship_meta_dataframe"),
    "shortcuts in test"
  )
  expect_error(
    prep_get_data_frame("ship_meta"),
    "shortcuts in test"
  )
  expect_error(
    prep_get_data_frame("study_data|study_data"),
    "shortcuts in test"
  )
  expect_error(
    prep_get_data_frame("meta_data_v2"),
    "shortcuts in test"
  )
  expect_error(
    prep_get_data_frame("meta_data"),
    "shortcuts in test"
  )
})

test_that("prep_get_data_frame explains cached keep_types requests", {
  prep_purge_data_frame_cache()
  withr::defer(prep_purge_data_frame_cache())

  csv <- tempfile(fileext = ".csv")
  write.csv(
    data.frame(VAR_NAMES = "v1", LABEL = "label"),
    csv,
    row.names = FALSE
  )

  first <- prep_get_data_frame(csv)
  expect_false(util_attr(first, "dataquieR_data_frame_keep_types", exact = TRUE)) # nolint: line_length_linter.

  expect_message2(
    second <- prep_get_data_frame(csv, keep_types = TRUE),
    "Returning cached data frame.*keep_types.*fresh load"
  )
  expect_equal(second, first)
})

test_that("prep_get_data_frame explains unknown cached keep_types", {
  cache <- new.env(parent = emptyenv())
  cache$manual <- data.frame(x = 1)

  expect_message2(
    result <- prep_get_data_frame("manual", .data_frame_list = cache,
      keep_types = TRUE),
    "registered without a recorded keep_types setting"
  )

  expect_identical(result, cache$manual)
})

test_that("prep_get_data_frame can read headers without caching full data", {
  prep_purge_data_frame_cache()
  withr::defer(prep_purge_data_frame_cache())

  csv <- tempfile(fileext = ".csv")
  write.csv(
    data.frame(id = 1:2, value = c("a", "b")),
    csv,
    row.names = FALSE
  )

  header_only <- suppressMessages(prep_get_data_frame(
    csv,
    column_names_only = TRUE
  ))

  expect_named(header_only, c("id", "value"))
  expect_equal(nrow(header_only), 0)
  expect_false(csv %in% prep_list_dataframes())
})

test_that("prep_get_data_frame selects columns from a named RData table", {
  prep_purge_data_frame_cache()
  withr::defer(prep_purge_data_frame_cache())

  table <- data.frame(id = 1:2, keep = 3:4, drop = 5:6)
  rdata <- tempfile(fileext = ".RData")
  save(table, file = rdata)

  selected <- suppressWarnings(suppressMessages(prep_get_data_frame(
    paste(rdata, "table", "id+keep", sep = "|")
  )))

  expect_named(selected, c("id", "keep"))
  expect_equal(selected$id, table$id)
  expect_equal(selected$keep, table$keep)
  expect_identical(
    util_attr(selected, "dataquieR_data_frame_cache_name", exact = TRUE),
    paste(rdata, "table", "id+keep", sep = "|")
  )
  expect_identical(
    util_attr(selected, "dataquieR_data_frame_keep_types", exact = TRUE),
    FALSE
  )
})

test_that("prep_get_data_frame accepts numeric RData table selectors", {
  prep_purge_data_frame_cache()
  withr::defer(prep_purge_data_frame_cache())

  table <- data.frame(id = 1:2, value = 3:4)
  rdata <- tempfile(fileext = ".RData")
  save(table, file = rdata)

  selected <- suppressWarnings(suppressMessages(prep_get_data_frame(
    paste(rdata, "1", sep = "|")
  )))

  expect_equal(selected, table, ignore_attr = TRUE)
})

test_that("prep_get_data_frame reports missing selected RData columns", {
  prep_purge_data_frame_cache()
  withr::defer(prep_purge_data_frame_cache())

  table <- data.frame(id = 1:2, keep = 3:4)
  rdata <- tempfile(fileext = ".RData")
  save(table, file = rdata)

  expect_error(
    suppressWarnings(suppressMessages(prep_get_data_frame(
      paste(rdata, "table", "id+absent", sep = "|")
    ))),
    "does not contain all of the columns"
  )
})

test_that("prep_get_data_frame loads data package tables by URI", {
  skip_on_cran()

  cache <- new.env(parent = emptyenv())
  iris_species <- with_dataframe_environment(quote(
    prep_get_data_frame("data:datasets|iris|Species")
  ), env = cache)

  expect_s3_class(iris_species, "data.frame")
  expect_named(iris_species, "Species")
  expect_identical(iris_species$Species, datasets::iris$Species)
  expect_true("data:datasets|iris|Species" %in% ls(cache))
})

test_that("prep_get_data_frame loads package files by URI", {
  skip_on_cran()

  cache <- new.env(parent = emptyenv())
  rulesets <- suppressMessages(prep_get_data_frame(
    "package:dataquieR/grading_rulesets.xlsx",
    .data_frame_list = cache
  ))

  expect_s3_class(rulesets, "data.frame")
  expect_true(nrow(rulesets) > 0)
  expect_true("GRADING_RULESET" %in% names(rulesets))
  expect_true("package:dataquieR/grading_rulesets.xlsx" %in% ls(cache))
})

test_that("prep_get_data_frame resolves extdata package URIs", {
  skip_on_cran()

  observed <- new.env(parent = emptyenv())
  cache <- new.env(parent = emptyenv())

  testthat::local_mocked_bindings(
    util_ensure_suggested = function(...) {
      TRUE
    },
    util_system_file = function(...) {
      observed$system_file_args <- list(...)
      "/tmp/resolved-extdata.csv"
    },
    util_rio_import = function(fn, keep_types, which = NULL, ...) {
      observed$import_fn <- fn
      observed$keep_types <- keep_types
      observed$which <- which
      data.frame(x = 1)
    }
  )

  result <- prep_get_data_frame(
    "extdata:demoPkg/nested/data.csv|sheet1",
    keep_types = TRUE,
    .data_frame_list = cache
  )

  expect_equal(result$x, 1)
  expect_identical(
    observed$system_file_args,
    list("extdata", "nested", "data.csv", package = "demoPkg")
  )
  expect_identical(observed$import_fn, "/tmp/resolved-extdata.csv")
  expect_true(observed$keep_types)
  expect_identical(observed$which, "sheet1")
  expect_true("extdata:demoPkg/nested/data.csv|sheet1" %in% ls(cache))
})

test_that("prep_get_data_frame loads database tables by URI", {
  skip_on_cran()
  skip_if_not_installed("dbx")

  observed <- new.env(parent = emptyenv())
  cache <- new.env(parent = emptyenv())

  testthat::local_mocked_bindings(
    dbxConnect = function(...) {
      observed$connect_args <- list(...)
      structure(list(id = "connection"), class = "mock_db_connection")
    },
    dbxSelect = function(con, sql) {
      observed$select_connection <- con
      observed$sql <- sql
      data.frame(id = 1, value = "loaded")
    },
    .package = "dbx"
  )
  testthat::local_mocked_bindings(
    with_db_connection = function(con, code) {
      observed$with_connection <- con
      force(code)
    },
    .package = "withr"
  )

  old_options <- getOption("dataquieR")
  old_dbx_options <- getOption("dataquieR.dbx")
  withr::defer(options(
    dataquieR = old_options,
    dataquieR.dbx = old_dbx_options
  ))
  options(dataquieR = list(schema = "legacy option ignored"))
  options(dataquieR.dbx = list(schema = "used by dbxConnect mock"))

  result <- prep_get_data_frame(
    "dbx:sqlite:///:memory:|study_table|id",
    keep_types = TRUE,
    .data_frame_list = cache
  )

  expect_named(result, "id")
  expect_equal(result$id, 1)
  expect_identical(
    observed$connect_args,
    list(url = "sqlite:///:memory:", schema = "used by dbxConnect mock")
  )
  expect_s3_class(observed$with_connection, "mock_db_connection")
  expect_s3_class(observed$select_connection, "mock_db_connection")
  expect_identical(observed$sql, "SELECT id FROM study_table")
  expect_true("dbx:sqlite:///:memory:|study_table|id" %in% ls(cache))
})

test_that("prep_get_data_frame loads all database columns by default", {
  skip_on_cran()
  skip_if_not_installed("dbx")

  observed <- new.env(parent = emptyenv())

  testthat::local_mocked_bindings(
    dbxConnect = function(...) {
      observed$connect_args <- list(...)
      structure(list(id = "connection"), class = "mock_db_connection")
    },
    dbxSelect = function(con, sql) {
      observed$sql <- sql
      data.frame(id = 1, value = "loaded")
    },
    .package = "dbx"
  )
  testthat::local_mocked_bindings(
    with_db_connection = function(con, code) {
      force(code)
    },
    .package = "withr"
  )

  result <- prep_get_data_frame(
    "dbx:sqlite:///:memory:|study_table",
    .data_frame_list = new.env(parent = emptyenv())
  )

  expect_named(result, c("id", "value"))
  expect_identical(observed$connect_args$url, "sqlite:///:memory:")
  expect_identical(observed$sql, "SELECT * FROM study_table")
})

test_that("prep_get_data_frame prefers DBX-specific connection options", {
  skip_on_cran()
  skip_if_not_installed("dbx")

  observed <- new.env(parent = emptyenv())
  withr::local_options(list(
    dataquieR = list(driver = "legacy"),
    dataquieR.dbx = list(driver = "specific", schema = "dq")
  ))

  testthat::local_mocked_bindings(
    dbxConnect = function(...) {
      observed$connect_args <- list(...)
      structure(list(id = "connection"), class = "mock_db_connection")
    },
    dbxSelect = function(con, sql) {
      data.frame(id = 1)
    },
    .package = "dbx"
  )
  testthat::local_mocked_bindings(
    with_db_connection = function(con, code) {
      force(code)
    },
    .package = "withr"
  )

  result <- prep_get_data_frame(
    "dbx:sqlite:///:memory:|study_table",
    .data_frame_list = new.env(parent = emptyenv())
  )

  expect_named(result, "id")
  expect_identical(observed$connect_args$url, "sqlite:///:memory:")
  expect_identical(observed$connect_args$driver, "specific")
  expect_identical(observed$connect_args$schema, "dq")
})

test_that("prep_get_data_frame keeps legacy DBX connection options", {
  skip_on_cran()
  skip_if_not_installed("dbx")

  observed <- new.env(parent = emptyenv())
  withr::local_options(list(
    dataquieR = list(driver = "legacy", schema = "dq")
  ))

  testthat::local_mocked_bindings(
    dbxConnect = function(...) {
      observed$connect_args <- list(...)
      structure(list(id = "connection"), class = "mock_db_connection")
    },
    dbxSelect = function(con, sql) {
      data.frame(id = 1)
    },
    .package = "dbx"
  )
  testthat::local_mocked_bindings(
    with_db_connection = function(con, code) {
      force(code)
    },
    .package = "withr"
  )

  result <- prep_get_data_frame(
    "dbx:sqlite:///:memory:|study_table",
    .data_frame_list = new.env(parent = emptyenv())
  )

  expect_named(result, "id")
  expect_identical(observed$connect_args$url, "sqlite:///:memory:")
  expect_identical(observed$connect_args$driver, "legacy")
  expect_identical(observed$connect_args$schema, "dq")
})

test_that("prep_get_data_frame tries extdata suffix fallbacks", {
  skip_on_cran()

  observed <- new.env(parent = emptyenv())
  observed$import_calls <- character(0)
  fallback_file <- tempfile(fileext = ".RDS")
  writeBin(charToRaw("placeholder"), fallback_file)
  withr::defer(unlink(fallback_file))
  no_such_file <- try(stop("No such file"), silent = TRUE)

  testthat::local_mocked_bindings(
    util_system_file = function(...) {
      observed$system_file_args <- c(
        observed$system_file_args,
        list(list(...))
      )
      fallback_file
    },
    util_rio_import = function(fn, keep_types, ...) {
      observed$import_calls <- c(observed$import_calls, fn)
      if (length(observed$import_calls) == 1) {
        no_such_file
      } else {
        data.frame(x = 2)
      }
    }
  )

  result <- prep_get_data_frame(
    "named_fixture",
    .data_frame_list = new.env(parent = emptyenv())
  )

  expect_equal(result$x, 2)
  expect_identical(
    observed$import_calls,
    c("named_fixture", fallback_file)
  )
  expect_identical(
    observed$system_file_args[[1]],
    list("extdata", "named_fixture.RDS", package = "dataquieR")
  )
})

test_that("prep_get_data_frame resolves vocabulary aliases recursively", {
  skip_on_cran()

  testthat::local_mocked_bindings(
    util_get_voc_tab = function() {
      data.frame(
        voc = "iris_species",
        url = "data:datasets|iris|Species",
        stringsAsFactors = FALSE
      )
    }
  )

  cache <- new.env(parent = emptyenv())
  iris_species <- with_dataframe_environment(quote(
    prep_get_data_frame("voc:iris_species")
  ), env = cache)

  expect_named(iris_species, "Species")
  expect_identical(iris_species$Species, datasets::iris$Species)

  iris_species2 <- with_dataframe_environment(quote(
    prep_get_data_frame("<iris_species>")
  ), env = new.env(parent = emptyenv()))

  expect_named(iris_species2, "Species")
  expect_identical(iris_species2$Species, datasets::iris$Species)
})

test_that("prep_get_data_frame reports invalid data-package URIs", {
  skip_on_cran()

  expect_error(
    prep_get_data_frame(
      "data:datasets/iris|iris",
      .data_frame_list = new.env(parent = emptyenv())
    ),
    "Cannot read file"
  )
})

test_that("cache attributes and temporary dataframe environments stay scoped", {
  env <- new.env(parent = emptyenv())
  env$cached <- util_set_data_frame_cache_attrs(
    data.frame(x = 1),
    data_frame_name = "cached",
    source = "manual",
    keep_types = TRUE
  )

  old_env <- .dataframe_environment()
  result <- with_dataframe_environment(quote({
    cached <- prep_get_data_frame("cached")
    expect_equal(cached$x, 1)
    expect_identical(
      util_attr(cached, "dataquieR_data_frame_source", exact = TRUE),
      "manual"
    )
    util_set_data_frame_cache_attrs("not-a-data-frame", "x", "manual", FALSE)
  }), env = env)

  expect_identical(result, "not-a-data-frame")
  expect_identical(.dataframe_environment(), old_env)

  replacement <- new.env(parent = emptyenv())
  util_set_dataframe_environment(replacement)
  expect_identical(.dataframe_environment(), replacement)
  util_set_dataframe_environment(NULL)
  expect_identical(.dataframe_environment(), .global.dataframe_environment)
  util_set_dataframe_environment(old_env)
})

test_that("prep_get_data_frame explains missing files with cache hints", {
  skip_on_cran()

  prep_purge_data_frame_cache()
  withr::defer(prep_purge_data_frame_cache())

  no_such_file <- try(stop("No such file"), silent = TRUE)
  testthat::local_mocked_bindings(
    util_rio_import = function(...) {
      no_such_file
    }
  )

  expect_error(
    prep_get_data_frame("absent_source"),
    "Did you forget to call"
  )
})

test_that("prep_get_data_frame omits cache hints for non-empty caches", {
  skip_on_cran()

  cache <- new.env(parent = emptyenv())
  cache$other <- data.frame(x = 1)
  no_such_file <- try(stop("No such file"), silent = TRUE)
  testthat::local_mocked_bindings(
    util_rio_import = function(...) {
      no_such_file
    }
  )

  err <- with_dataframe_environment(quote(tryCatch(
    prep_get_data_frame("absent_source"),
    error = identity
  )), env = cache)
  expect_s3_class(err, "error")
  expect_false(
    grepl("Did you forget", conditionMessage(err), fixed = TRUE)
  )
})
