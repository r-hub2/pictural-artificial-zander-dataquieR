skip_on_cran()

test_that("prep_load_workbook_like_file classifies missing local files as not found", { # nolint: line_length_linter.
  missing_file <- file.path(tempdir(), "dataquieR-missing-workbook.xlsx")
  if (file.exists(missing_file)) {
    unlink(missing_file, force = TRUE)
  }

  testthat::local_mocked_bindings(
    util_rio_import_list = function(...) {
      list(invalid = "not a data frame")
    }
  )

  expect_error(
    prep_load_workbook_like_file(missing_file),
    regexp = "File not found"
  )
})

test_that(
  "prep_load_workbook_like_file caches local RData sheets by qualified names",
  {
    skip_on_cran()
    cache <- new.env(parent = emptyenv())
    workbook <- tempfile("tiny_workbook_", fileext = ".RData")
    item_level <- data.frame(VAR_NAMES = "age", DATA_TYPE = "integer")
    code_table <- data.frame(CODE_VALUE = "1", CODE_LABEL = "yes")
    save(item_level, code_table, file = workbook)

    with_dataframe_environment(quote({
      suppressWarnings(prep_load_workbook_like_file(workbook))

      workbook_base <- basename(workbook)
      workbook_stem <- sub("\\.RData$", "", workbook_base)
      expected_names <- c(
        "item_level",
        "code_table",
        paste(workbook_base, "item_level", sep = SPLIT_CHAR),
        paste(workbook_base, "code_table", sep = SPLIT_CHAR),
        paste(workbook_stem, "item_level", sep = SPLIT_CHAR),
        paste(workbook_stem, "code_table", sep = SPLIT_CHAR)
      )

      expect_true(all(expected_names %in% ls(.dataframe_environment())))
      expect_identical(
        prep_get_data_frame(
          paste(workbook_base, "item_level", sep = SPLIT_CHAR)
        ),
        item_level
      )
      expect_identical(
        prep_get_data_frame(
          paste(workbook_stem, "code_table", sep = SPLIT_CHAR)
        ),
        code_table
      )
    }), env = cache)
  }
)

test_that("prep_load_workbook_like_file rejects multiple non-ODM files", {
  skip_on_cran()
  one <- tempfile(fileext = ".RData")
  two <- tempfile(fileext = ".RData")
  sheet <- data.frame(x = 1)
  save(sheet, file = one)
  save(sheet, file = two)

  expect_error(
    prep_load_workbook_like_file(c(one, two)),
    "Only for ODM files"
  )
})

test_that("prep_load_workbook_like_file validates workbook-like imports", {
  skip_on_cran()
  local_file <- tempfile(fileext = ".rds")
  saveRDS(list(not_a_sheet = 1), local_file)

  testthat::local_mocked_bindings(
    util_rio_import_list = function(fn, keep_types, ...) {
      list(not_a_sheet = 1)
    }
  )

  expect_error(
    prep_load_workbook_like_file(local_file),
    "workbook-like structure"
  )
})

test_that("prep_load_workbook_like_file validates scalar control arguments", {
  skip_on_cran()

  expect_error(
    prep_load_workbook_like_file("x", keep_types = c(TRUE, FALSE)),
    "keep_types"
  )
  expect_error(
    prep_load_workbook_like_file("x", append = c(TRUE, FALSE)),
    "append"
  )
})

test_that("prep_load_workbook_like_file blocks remote shortcuts during tests", {
  skip_on_cran()

  expect_error(
    prep_load_workbook_like_file("meta_data_v2"),
    "no shortcuts in tests"
  )
})

test_that("prep_load_workbook_like_file reports folder inputs", {
  skip_on_cran()

  folder <- tempfile()
  dir.create(folder)

  testthat::local_mocked_bindings(
    util_rio_import_list = function(fn, keep_types, ...) {
      list(not_a_sheet = 1)
    }
  )

  expect_error(
    prep_load_workbook_like_file(folder),
    "It is a folder"
  )
})

test_that("prep_load_workbook_like_file reports missing inputs", {
  skip_on_cran()

  missing_file <- file.path(tempdir(), "missing-workbook-like-file.xlsx")
  testthat::local_mocked_bindings(
    util_rio_import_list = function(fn, keep_types, ...) {
      list(not_a_sheet = 1)
    }
  )

  expect_error(
    prep_load_workbook_like_file(missing_file),
    "File not found"
  )
})

test_that(
  "prep_load_workbook_like_file caches single-table csv files by basename",
  {
    skip_on_cran()
    cache <- new.env(parent = emptyenv())
    csv_file <- tempfile(fileext = ".csv")
    writeLines(c("x,y", "1,a", "2,b"), csv_file)

    with_dataframe_environment(quote({
      suppressWarnings(prep_load_workbook_like_file(csv_file))

      expected <- read.csv(csv_file, colClasses = "character")
      expect_identical(prep_get_data_frame(basename(csv_file)), expected)
      expect_identical(
        prep_get_data_frame(paste(basename(csv_file), basename(csv_file),
            sep = SPLIT_CHAR
          )),
        expected
      )
    }), env = cache)
  }
)

test_that("prep_load_workbook_like_file honors downloaded file names", {
  skip_on_cran()
  seen <- new.env(parent = emptyenv())
  cache <- new.env(parent = emptyenv())

  with_mocked_bindings(
    download.file = function(url, destfile, ...) {
      seen$url <- url
      seen$destfile <- destfile
      writeLines(c("x", "1"), destfile)
      0
    },
    {
      testthat::local_mocked_bindings(
        util_fetch_ext = function(file) {
          structure("csv", `file-name` = "server-name.csv")
        },
        util_rio_import_list = function(fn, keep_types, ...) {
          seen$imported <- fn
          list(sheet = data.frame(x = 1))
        }
      )

      with_dataframe_environment(quote({
        suppressWarnings(prep_load_workbook_like_file(
          "https://example.invalid/download?id=1",
          keep_types = FALSE
        ))

        expect_true(all(c("sheet", "server-name.csv|sheet") %in%
              ls(.dataframe_environment())))
      }), env = cache)
    },
    .package = "utils"
  )

  expect_identical(seen$url, "https://example.invalid/download?id=1")
  expect_identical(basename(seen$destfile), "server-name.csv")
  expect_identical(basename(seen$imported), "server-name.csv")
})

test_that("prep_load_workbook_like_file appends detected URL extensions", {
  skip_on_cran()
  seen <- new.env(parent = emptyenv())
  cache <- new.env(parent = emptyenv())

  with_mocked_bindings(
    download.file = function(url, destfile, ...) {
      seen$url <- url
      seen$destfile <- destfile
      writeLines(c("x", "1"), destfile)
      0
    },
    {
      testthat::local_mocked_bindings(
        util_fetch_ext = function(file) {
          structure("csv", `file-name` = NA_character_)
        },
        util_rio_import_list = function(fn, keep_types, ...) {
          seen$imported <- fn
          list(sheet = data.frame(x = 1))
        }
      )

      with_dataframe_environment(quote({
        suppressWarnings(prep_load_workbook_like_file(
          "https://example.invalid/download?id=1",
          keep_types = FALSE
        ))

        expect_true("download.csv|sheet" %in% ls(.dataframe_environment()))
      }), env = cache)
    },
    .package = "utils"
  )

  expect_identical(seen$url, "https://example.invalid/download?id=1")
  expect_identical(basename(seen$destfile), "download.csv")
  expect_identical(basename(seen$imported), "download.csv")
})

test_that(
  "prep_load_workbook_like_file keeps URL basename if file type is unknown",
  {
    skip_on_cran()
    seen <- new.env(parent = emptyenv())
    cache <- new.env(parent = emptyenv())

    with_mocked_bindings(
      download.file = function(url, destfile, ...) {
        seen$url <- url
        seen$destfile <- destfile
        writeLines(c("x", "1"), destfile)
        0
      },
      {
        testthat::local_mocked_bindings(
          util_fetch_ext = function(file) {
            simpleError("no content type")
          },
          util_rio_import_list = function(fn, keep_types, ...) {
            seen$imported <- fn
            list(sheet = data.frame(x = 1))
          }
        )

        with_dataframe_environment(quote({
          expect_warning(
            prep_load_workbook_like_file(
              "https://example.invalid/folder/download%20name?token=1#anchor",
              keep_types = FALSE
            ),
            "Could not determine the file type"
          )

          expect_true("download name|sheet" %in% ls(.dataframe_environment()))
        }), env = cache)
      },
      .package = "utils"
    )

    expect_identical(
      seen$url,
      "https://example.invalid/folder/download%20name?token=1#anchor"
    )
    expect_identical(basename(seen$destfile), "download name")
    expect_identical(basename(seen$imported), "download name")
  }
)

test_that(
  "prep_load_workbook_like_file reports condition-like file type failures",
  {
    skip_on_cran()
    seen <- new.env(parent = emptyenv())
    cache <- new.env(parent = emptyenv())

    with_mocked_bindings(
      download.file = function(url, destfile, ...) {
        seen$destfile <- destfile
        writeLines(c("x", "1"), destfile)
        0
      },
      {
        testthat::local_mocked_bindings(
          util_fetch_ext = function(file) {
            simpleError("header condition")
          },
          util_rio_import_list = function(fn, keep_types, ...) {
            seen$imported <- fn
            list(sheet = data.frame(x = 1))
          }
        )

        with_dataframe_environment(quote({
          expect_warning(
            prep_load_workbook_like_file(
              "https://example.invalid/folder/condition-name",
              keep_types = FALSE
            ),
            "header condition"
          )

          expect_true("condition-name|sheet" %in%
              ls(.dataframe_environment()))
        }), env = cache)
      },
      .package = "utils"
    )

    expect_identical(basename(seen$destfile), "condition-name")
    expect_identical(basename(seen$imported), "condition-name")
  }
)

test_that("prep_load_workbook_like_file reports ambiguous URL file types", {
  skip_on_cran()
  seen <- new.env(parent = emptyenv())
  cache <- new.env(parent = emptyenv())

  with_mocked_bindings(
    download.file = function(url, destfile, ...) {
      seen$destfile <- c(seen$destfile, basename(destfile))
      writeLines(c("x", "1"), destfile)
      0
    },
    {
      testthat::local_mocked_bindings(
        util_fetch_ext = function(file) c("csv", "txt"),
        util_rio_import_list = function(fn, keep_types, ...) {
          seen$imported <- c(seen$imported, basename(fn))
          list(sheet = data.frame(x = 1))
        }
      )

      with_dataframe_environment(quote({
        expect_warning(
          prep_load_workbook_like_file(
            "https://example.invalid/folder/ambiguous",
            keep_types = FALSE
          ),
          "unknown reason"
        )

        expect_true("ambiguous|sheet" %in% ls(.dataframe_environment()))
      }), env = cache)
    },
    .package = "utils"
  )

  expect_identical(seen$destfile, "ambiguous")
  expect_identical(tail(seen$imported, 1), "ambiguous")
})

test_that(
  "prep_load_workbook_like_file reports thrown URL file type failures",
  {
    skip_on_cran()
    seen <- new.env(parent = emptyenv())
    cache <- new.env(parent = emptyenv())

    with_mocked_bindings(
      download.file = function(url, destfile, ...) {
        seen$destfile <- basename(destfile)
        writeLines(c("x", "1"), destfile)
        0
      },
      {
        testthat::local_mocked_bindings(
          util_fetch_ext = function(file) {
            stop("header lookup failed", call. = FALSE)
          },
          util_rio_import_list = function(fn, keep_types, ...) {
            seen$imported <- basename(fn)
            list(sheet = data.frame(x = 1))
          }
        )

        with_dataframe_environment(quote({
          expect_warning(
            prep_load_workbook_like_file(
              "https://example.invalid/folder/thrown",
              keep_types = FALSE
            ),
            "header lookup failed"
          )

          expect_true("thrown|sheet" %in% ls(.dataframe_environment()))
        }), env = cache)
      },
      .package = "utils"
    )

    expect_identical(seen$destfile, "thrown")
    expect_identical(seen$imported, "thrown")
  }
)

test_that(
  "prep_load_workbook_like_file appends multiple local ODM-like files",
  {
    skip_on_cran()

    cache <- new.env(parent = emptyenv())
    first <- tempfile(fileext = ".xml")
    second <- tempfile(fileext = ".xml")
    writeLines("<ODM><Study /></ODM>", first)
    writeLines("<ODM><Study /></ODM>", second)

    imported <- new.env(parent = emptyenv())
    imported$files <- character(0)

    testthat::local_mocked_bindings(
      util_is_odm_xml = function(fn) TRUE,
      util_rio_import_list = function(fn, keep_types, ...) {
        imported$files <- c(imported$files, basename(fn))
        list(sheet = data.frame(
          source = basename(fn), stringsAsFactors = FALSE
        ))
      }
    )

    with_dataframe_environment(quote({
      suppressWarnings(prep_load_workbook_like_file(
        c(first, second),
        keep_types = FALSE
      ))

      cached_sheet <- prep_get_data_frame("sheet")
      expect_equal(as.vector(cached_sheet$source), basename(c(first, second)))
      expect_true(all(paste(basename(c(first, second)), "sheet",
            sep = SPLIT_CHAR
          ) %in% ls(.dataframe_environment())))
    }), env = cache)

    expect_true(all(basename(c(first, second)) %in% imported$files))
    expect_equal(
      as.integer(table(imported$files)[basename(c(first, second))]),
      c(2L, 2L)
    )
  }
)
