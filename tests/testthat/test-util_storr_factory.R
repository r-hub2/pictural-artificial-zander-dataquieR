skip_on_cran()

fake_storr <- function(type = "rds", hashes_error = FALSE) {
  list_hashes <- if (hashes_error) {
    function() stop("broken hashes")
  } else {
    function() character()
  }
  structure(
    list(
      driver = list(
        type = function() type,
        reconnect = function() NULL
      ),
      list_hashes = list_hashes
    ),
    class = "storr"
  )
}

test_that("util_storr_factory handles null and factory attributes", {
  expect_null(util_storr_factory(NULL))
  expect_null(util_storr_factory())

  storr_object <- fake_storr()
  wrapped <- util_storr_factory(storr_object, function() fake_storr())
  expect_s3_class(wrapped, "storr")
  expect_true(is.function(attr(wrapped, "storr_factory")))

  from_attr <- util_storr_factory(wrapped)
  expect_s3_class(from_attr, "storr")
  expect_identical(attr(from_attr, "storr_factory"),
    attr(wrapped, "storr_factory"))
})

test_that("util_storr_factory replaces unusable factory functions", {
  storr_object <- fake_storr()
  with_argument <- function(path) {
    path
  }

  wrapped <- util_storr_factory(storr_object, with_argument)
  recreated <- attr(wrapped, "storr_factory", exact = TRUE)()

  expect_s3_class(wrapped, "storr")
  expect_s3_class(recreated, "storr")
  attr(recreated, "storr_factory") <- NULL
  expect_identical(recreated, storr_object)
  recreated <- attr(wrapped, "storr_factory", exact = TRUE)()
  expect_true(is.function(attr(recreated, "storr_factory", exact = TRUE)))

  expect_s3_class(
    util_storr_object(my_storr_factory = function() fake_storr()),
    "storr"
  )
})

test_that("util_storr_object creates the default environment storr", {
  skip_on_cran()
  skip_if_not_installed("storr")

  expect_warning(
    storr_object <- util_storr_object(),
    "storr classes other than RDS"
  )

  expect_s3_class(storr_object, "storr")
  expect_true(is.function(attr(storr_object, "storr_factory", exact = TRUE)))
})

test_that("util_storr_factory validates factory output", {
  expect_error(
    util_storr_factory(my_storr_factory = function() "not storr"),
    "storr factory should return"
  )

  storr_object <- fake_storr()
  wrapped <- util_storr_factory(storr_object, function() fake_storr())
  fixed <- util_fix_storr_object(wrapped)
  expect_s3_class(fixed, "storr")

  report <- structure(list(), my_storr_object = wrapped)
  expect_s3_class(util_get_storr_object_from_report(report), "storr")
})

test_that("util_storr_factory checks non-rds storr objects", {
  expect_warning(
    wrapped <- util_storr_factory(fake_storr(type = "memory"),
      function() fake_storr(type = "memory")),
    "storr classes other than RDS"
  )
  expect_s3_class(wrapped, "storr")

  expect_error(
    suppressWarnings(util_storr_factory(
      fake_storr(type = "memory", hashes_error = TRUE),
      function() fake_storr(type = "memory", hashes_error = TRUE)
    )),
    "storr object not working"
  )
})

test_that("storr helpers handle unavailable or broken recovery factories", {
  testthat::local_mocked_bindings(
    util_ensure_suggested = function(...) FALSE
  )
  expect_null(prep_create_storr_factory()())

  expect_error(
    suppressWarnings(util_storr_factory(
      fake_storr(type = "memory", hashes_error = TRUE),
      function() "not storr"
    )),
    "storr factory should return"
  )
})
