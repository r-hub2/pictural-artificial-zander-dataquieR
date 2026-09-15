test_that("prep_load_folder_with_metadata works", {
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  skip_if_not_installed("rvest")
  skip_on_cran()
  prep_purge_data_frame_cache()
  expect_equal(length(prep_list_dataframes()), 0)
  suppressWarnings(prep_load_folder_with_metadata("https://dataquality.qihs.uni-greifswald.de/extdata/fortests")) # nolint: line_length_linter.
  expect_gt(length(prep_list_dataframes()), 10)
})

test_that("prep_load_folder_with_metadata loads local folders into the cache", {
  skip_on_cran()

  cache <- new.env(parent = emptyenv())
  folder <- tempfile()
  dir.create(folder)
  sheet <- data.frame(x = 1)
  save(sheet, file = file.path(folder, "meta.RData"))
  writeLines("not a workbook", file.path(folder, "ignore.txt"))

  with_dataframe_environment(quote({
    suppressWarnings(prep_load_folder_with_metadata(folder))

    expect_true(all(c("sheet", "meta.RData|sheet", "meta|sheet") %in%
          ls(.dataframe_environment())))
    expect_identical(prep_get_data_frame("sheet"), sheet)
    expect_true("ignore.txt" %in% ls(.dataframe_environment()))

    expect_error(
      prep_load_folder_with_metadata(folder, full.names = FALSE),
      "full.names not supported"
    )
  }), env = cache)
})

test_that("prep_load_folder_with_metadata reports local folder problems", {
  skip_on_cran()

  expect_error(
    prep_load_folder_with_metadata(file.path(tempdir(), "missing-folder")),
    "Folder not found"
  )

  folder <- tempfile()
  dir.create(folder)
  writeLines("excel lock", file.path(folder, "~$open.xlsx"))
  writeLines("libreoffice lock", file.path(folder, ".~lock.meta.xlsx#"))

  testthat::local_mocked_bindings(
    prep_load_workbook_like_file = function(...) invisible(NULL)
  )

  seen <- new.env(parent = emptyenv())
  seen$warnings <- character()
  withCallingHandlers(
    suppressMessages(prep_load_folder_with_metadata(folder)),
    warning = function(cnd) {
      seen$warnings <- c(seen$warnings, conditionMessage(cnd))
      invokeRestart("muffleWarning")
    }
  )

  expect_true(any(grepl("Found files that look like Excel", seen$warnings)))
  expect_true(any(grepl("Found files that look like LibreOffice",
        seen$warnings)))
})

test_that("prep_load_folder_with_metadata warns for unreadable local files", {
  skip_on_cran()

  folder <- tempfile()
  dir.create(folder)
  writeLines("not loadable", file.path(folder, "broken.txt"))

  testthat::local_mocked_bindings(
    prep_load_workbook_like_file = function(...) stop("not a workbook"),
    prep_get_data_frame = function(...) stop("not a data frame")
  )

  expect_warning(
    result <- prep_load_folder_with_metadata(folder),
    "Could not load .*broken.txt"
  )
  expect_identical(result, .dataframe_environment())
})
