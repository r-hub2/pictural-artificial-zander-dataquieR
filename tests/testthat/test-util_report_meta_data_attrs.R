test_that("util_report_meta_data_cross_item prefers current metadata names", {
  skip_on_cran()

  current_meta <- data.frame(VAR_NAMES = "current")
  legacy_meta <- data.frame(VAR_NAMES = "legacy")
  report <- structure(
    list(),
    meta_data_cross_item = current_meta,
    meta_data_cross = legacy_meta
  )

  expect_identical(util_report_meta_data_cross_item(report), current_meta)
  expect_identical(
    util_report_meta_data_cross_item(
      structure(list(), meta_data_cross = legacy_meta)
    ),
    legacy_meta
  )
  expect_identical(
    util_report_meta_data_cross_item(structure(list(), other = legacy_meta)),
    data.frame()
  )
})

test_that("util_report_meta_data_frames keeps legacy-only cross metadata", {
  skip_on_cran()

  report <- structure(
    list(),
    meta_data = data.frame(),
    meta_data_cross = data.frame()
  )

  expect_equal(
    util_report_meta_data_frames(report),
    c("meta_data", "meta_data_cross")
  )
  expect_equal(
    util_report_meta_data_frames(structure(list())),
    character(0)
  )
})

test_that("util_report_meta_data_frames hides legacy cross-item aliases", {
  skip_on_cran()

  report <- structure(
    list(),
    meta_data = data.frame(),
    meta_data_dataframe = data.frame(),
    meta_data_cross_item = data.frame(),
    meta_data_cross = data.frame(),
    unrelated_meta_data = data.frame()
  )

  expect_setequal(
    util_report_meta_data_frames(report),
    c("meta_data", "meta_data_dataframe", "meta_data_cross_item")
  )
  expect_false("meta_data_cross" %in% util_report_meta_data_frames(report))
})
