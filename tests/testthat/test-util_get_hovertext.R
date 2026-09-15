test_that("util_get_hovertext reads and caches shipped hover text", {
  skip_on_cran()

  cache_key <- system.file("hovertext.rds", package = "dataquieR")
  expect_true(nzchar(cache_key))

  had_cached_value <- exists(cache_key, .concept_chache, mode = "list")
  if (had_cached_value) {
    cached_value <- get(cache_key, .concept_chache, mode = "list")
  }
  on.exit({
    if (exists(cache_key, .concept_chache, mode = "list")) {
      rm(list = cache_key, envir = .concept_chache)
    }
    if (had_cached_value) {
      assign(cache_key, cached_value, .concept_chache)
    }
  }, add = TRUE)

  if (exists(cache_key, .concept_chache, mode = "list")) {
    rm(list = cache_key, envir = .concept_chache)
  }

  hover <- util_get_hovertext("meta_data")
  expect_true(all(c(VAR_NAMES, LABEL, VALUE_LABELS) %in% names(hover)))
  expect_match(unname(hover[[VAR_NAMES]]), "variable names", fixed = TRUE)
  expect_true(exists(cache_key, .concept_chache, mode = "list"))

  assign(cache_key, list(meta_data = c(custom = "cached")), .concept_chache)
  expect_identical(util_get_hovertext("meta_data"), c(custom = "cached"))
  expect_null(util_get_hovertext("unknown_table"))
})

test_that("util_get_hovertext rejects unreadable hover text files", {
  skip_on_cran()

  cache_key <- system.file("hovertext.rds", package = "dataquieR")
  expect_true(nzchar(cache_key))

  had_cached_value <- exists(cache_key, .concept_chache, mode = "list")
  if (had_cached_value) {
    cached_value <- get(cache_key, .concept_chache, mode = "list")
  }
  on.exit({
    if (exists(cache_key, .concept_chache, mode = "list")) {
      rm(list = cache_key, envir = .concept_chache)
    }
    if (had_cached_value) {
      assign(cache_key, cached_value, .concept_chache)
    }
  }, add = TRUE)

  if (exists(cache_key, .concept_chache, mode = "list")) {
    rm(list = cache_key, envir = .concept_chache)
  }

  testthat::local_mocked_bindings(
    file.access = function(...) -1L,
    .package = "base"
  )

  expect_error(
    util_get_hovertext("meta_data"),
    "Cannot read file"
  )
})
