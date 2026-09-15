test_that("prep_remove_from_cache removes single cached data frames", {
  skip_on_cran()

  prep_purge_data_frame_cache()
  withr::defer(prep_purge_data_frame_cache())

  prep_add_data_frames(tab_a = data.frame(x = 1))

  expect_message(
    prep_remove_from_cache("tab_a"),
    "tab_a have been removed from cache",
    fixed = TRUE
  )
  expect_false("tab_a" %in% prep_list_dataframes())

  expect_message(
    prep_remove_from_cache("tab_missing"),
    "tab_missing not present in the cache",
    fixed = TRUE
  )
})

test_that("prep_remove_from_cache removes vectors only when all names exist", {
  skip_on_cran()

  prep_purge_data_frame_cache()
  withr::defer(prep_purge_data_frame_cache())

  prep_add_data_frames(
    tab_a = data.frame(x = 1),
    tab_b = data.frame(y = 2),
    tab_c = data.frame(z = 3)
  )

  expect_message(
    prep_remove_from_cache(c("tab_a", "tab_b")),
    "tab_a & tab_b have been removed from cache",
    fixed = TRUE
  )
  expect_false(any(c("tab_a", "tab_b") %in% prep_list_dataframes()))
  expect_true("tab_c" %in% prep_list_dataframes())

  expect_message(
    prep_remove_from_cache(c("tab_c", "tab_missing")),
    "Not all the objects \\(tab_c, tab_missing\\) are present in the cache"
  )
  expect_true("tab_c" %in% prep_list_dataframes())
})
