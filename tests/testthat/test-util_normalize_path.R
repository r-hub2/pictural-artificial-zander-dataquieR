skip_on_cran()

test_that("util_normalize_path resolves non-existing tails", {
  root <- tempfile("normalize-path-root")
  dir.create(file.path(root, "existing"), recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)

  path <- file.path(
    root,
    "existing",
    ".",
    "missing",
    "child",
    "..",
    "leaf.csv"
  )

  normalized <- util_normalize_path(path)

  expect_equal(
    normalized,
    file.path(normalizePath(file.path(root, "existing")), "missing", "leaf.csv")
  )
})

test_that("util_normalize_path keeps vector names and existing paths", {
  root <- tempfile("normalize-path-root")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)

  paths <- c(primary = root)

  normalized <- util_normalize_path(paths)

  expect_named(normalized, "primary")
  expect_equal(unname(normalized), normalizePath(root))
})

test_that("util_normalize_path resolves relative paths with empty tails", {
  root <- tempfile("normalize-path-root")
  dir.create(file.path(root, "existing"), recursive = TRUE)
  withr::local_dir(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)

  normalized <- util_normalize_path(file.path("existing", "missing", ".."))

  expect_equal(normalized, normalizePath(file.path(root, "existing")))
})

test_that("util_normalize_path honors Windows winslash for missing tails", {
  root <- tempfile("normalize-path-root")
  dir.create(file.path(root, "existing"), recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)

  mocked_platform <- base::.Platform
  mocked_platform[["OS.type"]] <- "windows"

  normalized <- testthat::with_mocked_bindings(
    util_normalize_path(
      file.path(root, "existing", "missing", "leaf.csv"),
      winslash = "\\"
    ),
    .Platform = mocked_platform,
    .package = "base"
  )

  expect_match(normalized, "\\\\missing\\\\leaf\\.csv$")
  expect_false(grepl("/", normalized, fixed = TRUE))
})

test_that("util_normalize_path passes missing paths to base normalizer", {
  normalized <- util_normalize_path(c(primary = NA_character_))

  expect_named(normalized, "primary")
  expect_true(is.na(normalized[[1L]]))
})

test_that("util_normalize_path reports missing paths according to mustWork", {
  root <- tempfile("normalize-path-root")
  missing_path <- file.path(root, "does-not-exist")

  expect_warning(
    warned <- util_normalize_path(missing_path, mustWork = NA),
    "No such file or directory"
  )
  expect_match(warned, "does-not-exist")

  expect_error(
    util_normalize_path(missing_path, mustWork = TRUE),
    "No such file or directory"
  )
})

test_that("util_normalize_path validates scalar arguments", {
  expect_error(util_normalize_path(1), "`path` must be a character vector")
  expect_error(util_normalize_path("x", winslash = c("/", "/")))
  expect_error(util_normalize_path("x", winslash = NA_character_))
  expect_error(util_normalize_path("x", mustWork = c(TRUE, FALSE)))
})
