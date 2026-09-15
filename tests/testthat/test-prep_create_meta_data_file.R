test_that("multiplication works", {
  skip_on_cran()
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  d <- withr::local_tempdir()
  withr::local_dir(d)
  prep_create_meta_data_file("Test.xlsx",
    cars,
    open = FALSE,
    overwrite = TRUE
  )
  expect_gt(file.size("Test.xlsx"), 1000)
  md0 <- prep_get_data_frame("Test.xlsx|item_level")
  expect_snapshot(md0)
})

test_that(
  "prep_create_meta_data_file validates arguments before template load",
  {
    skip_on_cran()

    target <- tempfile(fileext = ".xlsx")
    writeLines("already here", target)

    existing_file_error <- try(
      prep_create_meta_data_file(target, open = FALSE, overwrite = FALSE),
      silent = TRUE
    )
    expect_true(util_is_try_error(existing_file_error))
    expect_error(
      prep_create_meta_data_file(target, open = NA, overwrite = FALSE),
      "open"
    )
    expect_error(
      prep_create_meta_data_file(target, open = FALSE, overwrite = NA),
      "overwrite"
    )
  }
)

test_that("prep_create_meta_data_file handles optional cli formatting", {
  skip_on_cran()

  target <- tempfile(fileext = ".xlsx")
  writeLines("already here", target)

  testthat::local_mocked_bindings(
    util_ensure_suggested = function(...) FALSE
  )

  expect_error(
    prep_create_meta_data_file(target, open = FALSE, overwrite = FALSE),
    "already exists"
  )
})

test_that("prep_create_meta_data_file reports export errors", {
  skip_on_cran()

  template <- list(
    item_level = data.frame(
      MISSING_LIST_TABLE = character(),
      stringsAsFactors = FALSE
    ),
    segment_level = data.frame(
      SEGMENT_ID_REF_TABLE = character(),
      stringsAsFactors = FALSE
    ),
    dataframe_level = data.frame(
      DF_ID_REF_TABLE = character(),
      stringsAsFactors = FALSE
    )
  )
  target <- tempfile(fileext = ".unsupported")

  testthat::local_mocked_bindings(
    util_rio_import_list = function(...) template
  )

  expect_error(
    prep_create_meta_data_file(target, open = FALSE, overwrite = TRUE),
    "Could not write"
  )
})

test_that("prep_create_meta_data_file tolerates absent guessed code tables", {
  skip_on_cran()

  template <- list(
    item_level = data.frame(
      MISSING_LIST_TABLE = "old_missing",
      stringsAsFactors = FALSE
    ),
    segment_level = data.frame(
      SEGMENT_ID_REF_TABLE = character(),
      stringsAsFactors = FALSE
    ),
    dataframe_level = data.frame(
      DF_ID_REF_TABLE = character(),
      stringsAsFactors = FALSE
    ),
    old_missing = data.frame(code = 1L)
  )
  guessed_item_level <- data.frame(
    VAR_NAMES = "x",
    LABEL = "x",
    MISSING_LIST_TABLE = "new_missing",
    MISSING_LIST = "new_missing",
    JUMP_LIST = NA_character_,
    stringsAsFactors = FALSE
  )
  target <- tempfile(fileext = ".xlsx")
  cache_state <- new.env(parent = emptyenv())
  cache_state$calls <- 0L
  assign("temporary_code_table", data.frame(code = 1L),
    envir = .dataframe_environment()
  )

  testthat::local_mocked_bindings(
    util_rio_import_list = function(...) template,
    prep_study2meta = function(...) guessed_item_level,
    util_prepare_item_level_metadata = function(meta_data, ...) meta_data,
    prep_get_data_frame = function(...) {
      stop("table not cached", call. = FALSE)
    },
    prep_list_dataframes = function() {
      cache_state$calls <- cache_state$calls + 1L
      if (cache_state$calls == 1L) {
        return(character())
      }
      "temporary_code_table"
    }
  )

  prep_create_meta_data_file(
    target,
    study_data = data.frame(x = 1L),
    open = FALSE,
    overwrite = TRUE
  )

  expect_gt(file.size(target), 0)
  expect_false(exists("temporary_code_table",
      envir = .dataframe_environment(),
      inherits = FALSE
    ))
})

test_that("prep_create_meta_data_file opens requested output when asked", {
  skip_on_cran()

  template <- list(
    item_level = data.frame(
      MISSING_LIST_TABLE = character(),
      stringsAsFactors = FALSE
    ),
    segment_level = data.frame(
      SEGMENT_ID_REF_TABLE = character(),
      stringsAsFactors = FALSE
    ),
    dataframe_level = data.frame(
      DF_ID_REF_TABLE = character(),
      stringsAsFactors = FALSE
    )
  )
  target <- tempfile(fileext = ".xlsx")
  opened <- new.env(parent = emptyenv())
  opened$file <- NA_character_

  testthat::local_mocked_bindings(
    util_rio_import_list = function(...) template,
    browseURL = function(url) {
      opened$file <- url
    }
  )

  prep_create_meta_data_file(target, open = TRUE, overwrite = TRUE)

  expect_equal(opened$file, target)
  expect_gt(file.size(target), 0)
})
