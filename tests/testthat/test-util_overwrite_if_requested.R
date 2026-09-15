skip_on_cran()

test_that("util_overwrite_if_requested creates and protects directories", {
  target <- file.path(tempdir(), "dq-overwrite-test")
  unlink(target, recursive = TRUE, force = TRUE)
  withr::defer(unlink(target, recursive = TRUE, force = TRUE))

  util_overwrite_if_requested(target, force_overwrite = FALSE)
  expect_true(dir.exists(target))

  writeLines("keep", file.path(target, "existing.txt"))
  expect_error(
    util_overwrite_if_requested(target, force_overwrite = FALSE),
    "already exists"
  )
  expect_true(file.exists(file.path(target, "existing.txt")))

  util_overwrite_if_requested(target, force_overwrite = TRUE)
  expect_true(dir.exists(target))
  expect_false(file.exists(file.path(target, "existing.txt")))
})

test_that("util_overwrite_if_requested rejects files as output directories", {
  target <- tempfile("dq-output-file")
  writeLines("not a directory", target)
  withr::defer(unlink(target, recursive = TRUE, force = TRUE))

  expect_error(
    util_overwrite_if_requested(target, force_overwrite = TRUE),
    "not a directory"
  )
})

test_that("util_overwrite_if_requested reports directory creation failures", {
  parent_file <- tempfile("dq-output-parent-file")
  writeLines("not a directory", parent_file)
  withr::defer(unlink(parent_file, recursive = TRUE, force = TRUE))

  target <- file.path(parent_file, "child")

  expect_error(
    util_overwrite_if_requested(target, force_overwrite = FALSE),
    "Could not create"
  )
})
