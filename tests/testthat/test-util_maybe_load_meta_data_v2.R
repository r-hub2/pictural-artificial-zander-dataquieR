test_that("util_maybe_load_meta_data_v2 requires a matching formal", {
  skip_on_cran()

  without_metadata_v2 <- function() {
    util_maybe_load_meta_data_v2()
  }

  expect_error(without_metadata_v2(), "Need a.*meta_data_v2.*formal")
})

test_that(
  "util_maybe_load_meta_data_v2 leaves missing metadata arguments alone",
  {
    skip_on_cran()

    with_metadata_v2 <- function(meta_data_v2) {
      util_maybe_load_meta_data_v2()
    }

    testthat::local_mocked_bindings(
      prep_purge_data_frame_cache = function(...) {
        fail("cache must not be purged")
      },
      prep_load_workbook_like_file = function(...) {
        fail("metadata must not be loaded")
      }
    )

    expect_silent(with_metadata_v2())
  }
)

test_that("util_maybe_load_meta_data_v2 reloads supplied metadata", {
  skip_on_cran()

  with_metadata_v2 <- function(meta_data_v2) {
    util_maybe_load_meta_data_v2()
  }
  calls <- new.env(parent = emptyenv())
  calls$purged <- 0L
  calls$loaded <- NULL

  testthat::local_mocked_bindings(
    prep_purge_data_frame_cache = function(...) {
      calls$purged <- calls$purged + 1L
    },
    prep_load_workbook_like_file = function(file, ...) {
      calls$loaded <- file
    }
  )

  with_dataframe_environment(quote({
    expect_message(
      expect_warning(
        with_metadata_v2("metadata-v2.xlsx"),
        "Did not find any sheet named"
      ),
      "Have.*meta_data_v2.*set"
    )
  }), env = new.env(parent = emptyenv()))
  expect_identical(calls$purged, 1L)
  expect_identical(calls$loaded, "metadata-v2.xlsx")
})
