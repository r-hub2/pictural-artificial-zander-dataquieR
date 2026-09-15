skip_on_cran()

test_that("data frame cache attributes are attached only to data frames", {
  study_data <- data.frame(id = 1:2, value = c("a", "b"))
  cached <- util_set_data_frame_cache_attrs(
    study_data,
    data_frame_name = "study_data",
    source = "memory",
    keep_types = TRUE
  )

  expect_equal(attr(cached, "dataquieR_data_frame_cache_name"), "study_data")
  expect_equal(attr(cached, "dataquieR_data_frame_source"), "memory")
  expect_true(attr(cached, "dataquieR_data_frame_keep_types"))
  expect_s3_class(attr(cached, "dataquieR_data_frame_loaded_at"), "POSIXt")

  expect_identical(
    util_set_data_frame_cache_attrs(1:3, "not_df", "memory", FALSE),
    1:3
  )
})

test_that("prep_get_data_frame returns cached data frames from custom cache", {
  cache <- new.env(parent = emptyenv())
  cache$cached_data <- util_set_data_frame_cache_attrs(
    data.frame(id = 1:2, value = c("a", "b")),
    data_frame_name = "cached_data",
    source = "memory",
    keep_types = FALSE
  )

  expect_identical(
    prep_get_data_frame("cached_data", .data_frame_list = cache),
    cache$cached_data
  )

  expect_message(
    prep_get_data_frame("cached_data",
      .data_frame_list = cache,
      keep_types = TRUE
    ),
    "Returning cached data frame"
  )
})

test_that("cached data frame messages include default keep_types context", {
  cached <- util_set_data_frame_cache_attrs(
    data.frame(id = 1),
    data_frame_name = "cached_data",
    source = "memory",
    keep_types = FALSE
  )
  attr(cached, "dataquieR_data_frame_loaded_at") <-
    as.POSIXct("2026-07-31 07:00:00", tz = "UTC")

  expect_message2(
    result <- util_message_cached_data_frame(
      cached,
      data_frame_name = "cached_data",
      keep_types = TRUE,
      keep_types_missing = TRUE
    ),
    "Returning cached data frame.*memory.*default .*keep_types.*fresh load"
  )
  expect_identical(result, cached)
})

test_that("prep_get_data_frame normalizes pipe separators for cached names", {
  cache <- new.env(parent = emptyenv())
  cache[["workbook|sheet"]] <- data.frame(id = 1, value = "cached")

  expect_equal(
    prep_get_data_frame("workbook | sheet", .data_frame_list = cache),
    data.frame(id = 1, value = "cached")
  )
})

test_that("with_dataframe_environment restores the previous data frame cache", {
  original <- .dataframe_environment()
  local_cache <- new.env(parent = emptyenv())
  local_cache$local_table <- data.frame(x = 1)

  result <- with_dataframe_environment(
    {
      expect_identical(.dataframe_environment(), local_cache)
      prep_get_data_frame("local_table")
    },
    env = local_cache
  )

  expect_equal(result, data.frame(x = 1))
  expect_identical(.dataframe_environment(), original)
})

test_that("data frame cache environment rejects non-environments", {
  expect_error(util_set_dataframe_environment(list()), "is.environment")
  expect_error(
    prep_get_data_frame("cached_data", .data_frame_list = list()),
    ".data_frame_list"
  )
  expect_error(prep_get_data_frame("cached_data", keep_types = NA))
  expect_error(prep_get_data_frame("cached_data", column_names_only = NA))
})

test_that(
  "prep_get_data_frame imports local files with header and column selection",
  {
    skip_on_cran()
    skip_if_not_installed("rio")

    cache <- new.env(parent = emptyenv())
    csv_file <- tempfile(fileext = ".csv")
    utils::write.csv(
      data.frame(id = 1:2, value = c("a", "b"), extra = c("x", "y")),
      csv_file,
      row.names = FALSE
    )

    header_only <- prep_get_data_frame(
      csv_file,
      .data_frame_list = cache,
      column_names_only = TRUE
    )
    expect_equal(colnames(header_only), c("id", "value", "extra"))
    expect_equal(nrow(header_only), 0L)

    rdata_file <- tempfile(fileext = ".RData")
    table_in_file <- data.frame(
      id = 1:2,
      value = c("a", "b"),
      extra = c("x", "y"),
      stringsAsFactors = FALSE
    )
    save(table_in_file, file = rdata_file)

    cache <- new.env(parent = emptyenv())
    selected <- prep_get_data_frame(
      paste(rdata_file, "table_in_file", "value+extra", sep = SPLIT_CHAR),
      .data_frame_list = cache
    )
    expect_equal(colnames(selected), c("value", "extra"))
    expect_equal(unname(selected$value), c("a", "b"))
    expect_equal(unname(selected$extra), c("x", "y"))

    expect_error(
      prep_get_data_frame(
        paste(rdata_file, "table_in_file", "missing", sep = SPLIT_CHAR),
        .data_frame_list = new.env(parent = emptyenv())
      ),
      "does not contain all of the columns"
    )
  }
)

test_that("prep_get_data_frame loads package data with column selection", {
  skip_on_cran()

  cache <- new.env(parent = emptyenv())
  selected <- prep_get_data_frame(
    paste("data:datasets", "iris", "Species", sep = SPLIT_CHAR),
    .data_frame_list = cache
  )

  expect_equal(colnames(selected), "Species")
  expect_equal(nrow(selected), nrow(datasets::iris))
  expect_equal(
    as.character(utils::head(selected$Species)),
    as.character(utils::head(datasets::iris$Species))
  )
  expect_equal(
    attr(selected, "dataquieR_data_frame_cache_name"),
    "data:datasets|iris|Species"
  )
  expect_equal(
    attr(selected, "dataquieR_data_frame_source"),
    "data:datasets"
  )
})

test_that("prep_get_data_frame reports non-table imports", {
  skip_on_cran()
  skip_if_not_installed("rio")

  rds_file <- tempfile(fileext = ".rds")
  saveRDS(1:3, rds_file)
  expect_error(
    prep_get_data_frame(
      rds_file,
      .data_frame_list = new.env(parent = emptyenv())
    ),
    "did not contain a table"
  )
})
