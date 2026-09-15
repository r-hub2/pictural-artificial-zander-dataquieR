test_that("worker cache helpers strip and restore raw study data attributes", {
  skip_on_cran()

  prepared <- data.frame(a = 1)
  raw_study_data <- data.frame(raw = 2)
  attr(prepared, "study_data") <- raw_study_data

  stripped <- util_worker_cache_strip_study_data_attrs(
    list(prepared = prepared)
  )
  stripped_prepared <- stripped$study_data_cache$prepared
  raw_key <- attr(
    stripped_prepared,
    "dataquieR_study_data_cache_raw_key",
    exact = TRUE
  )

  expect_null(attr(stripped_prepared, "study_data", exact = TRUE))
  expect_match(raw_key, "^study_data@")
  expect_identical(
    stripped$study_data_cache_study_data_attrs[[raw_key]],
    raw_study_data
  )

  restored <- util_worker_cache_restore_study_data_attrs(
    stripped$study_data_cache,
    stripped$study_data_cache_study_data_attrs
  )

  expect_identical(
    attr(restored$prepared, "study_data", exact = TRUE),
    raw_study_data
  )
  expect_null(attr(
    restored$prepared,
    "dataquieR_study_data_cache_raw_key",
    exact = TRUE
  ))
})

test_that("worker cache payload can be disabled for local evaluation", {
  skip_on_cran()

  withr::local_options(dataquieR.precomputeStudyData = FALSE)

  payload <- util_worker_cache_payload()

  expect_named(payload, c(
    "cache_as_list",
    "study_data_cache",
    "study_data_cache_study_data_attrs",
    "study_data_cache_input_keys",
    "study_data_cache_meta_data"
  ))
  expect_true(all(vapply(payload, length, integer(1)) == 0L))
})

test_that("worker cache payload strips raw study-data attrs when enabled", {
  skip_on_cran()

  ns <- asNamespace("dataquieR")
  cache <- get(".cache", envir = ns)
  study_data_cache <- get(".study_data_cache", envir = ns)
  input_keys <- get(".study_data_cache_input_keys", envir = ns)
  meta_data_cache <- get(".study_data_cache_meta_data", envir = ns)

  rm(list = ls(cache[[".cache"]]), envir = cache[[".cache"]])
  util_purge_study_data_cache()
  withr::defer(rm(list = ls(cache[[".cache"]]), envir = cache[[".cache"]]))
  withr::defer(util_purge_study_data_cache())

  prepared <- data.frame(a = 1)
  raw_study_data <- data.frame(raw = 2)
  attr(prepared, "study_data") <- raw_study_data
  study_data_cache[["prepared-key"]] <- prepared
  input_keys[["input-key"]] <- "prepared-key"
  meta_data_cache[["prepared-key"]] <- data.frame(VAR_NAMES = "a")
  cache[[".cache"]][["ordinary-cache-key"]] <- "ordinary-cache-value"

  payload <- util_worker_cache_payload(precompute = TRUE)

  expect_identical(
    payload$cache_as_list$`ordinary-cache-key`,
    "ordinary-cache-value"
  )
  expect_named(payload$study_data_cache, "prepared-key")
  expect_null(attr(payload$study_data_cache$`prepared-key`, "study_data",
      exact = TRUE))
  raw_key <- attr(
    payload$study_data_cache$`prepared-key`,
    "dataquieR_study_data_cache_raw_key",
    exact = TRUE
  )
  expect_match(raw_key, "^study_data@")
  expect_identical(
    payload$study_data_cache_study_data_attrs[[raw_key]],
    raw_study_data
  )
  expect_identical(payload$study_data_cache_input_keys$`input-key`,
    "prepared-key")
  expect_identical(payload$study_data_cache_meta_data$`prepared-key`,
    data.frame(VAR_NAMES = "a"))
})
