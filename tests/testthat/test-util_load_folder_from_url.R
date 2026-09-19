test_that("downloaded folders remain available to their caller", {
  skip_on_cran()
  skip_if_not_installed("rvest")

  source_dir <- withr::local_tempdir()
  source_file <- file.path(source_dir, "source.RData")
  source_data <- data.frame(x = 1)
  save(source_data, file = source_file)
  index_file <- file.path(source_dir, "index.html")
  writeLines(sprintf('<a href="file://%s">source</a>', source_file),
    index_file)

  downloaded <- suppressWarnings(util_load_folder_from_url(
    paste0("file://", index_file)
  ))
  withr::defer(unlink(downloaded, recursive = TRUE, force = TRUE))

  expect_true(dir.exists(downloaded))
  expect_true(file.exists(file.path(downloaded, "source.RData")))
})
