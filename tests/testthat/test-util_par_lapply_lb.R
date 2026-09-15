test_that("util_par_lapply_lb delegates and falls back to lapply", {
  skip_on_cran()

  expect_identical(
    util_par_lapply_lb(
      cl = NULL,
      X = c("a", "b"),
      fun = paste0,
      "!"
    ),
    list("a!", "b!")
  )

  testthat::local_mocked_bindings(
    parLapplyLB = function(cl, X, fun, ..., chunk.size) {
      list(
        values = vapply(X, function(x) fun(x, ...), integer(1)),
        chunk_size = if (missing(chunk.size)) NULL else chunk.size
      )
    },
    .package = "parallel"
  )
  cluster <- structure(list(), class = "cluster")

  expect_identical(
    util_par_lapply_lb(cluster, 1:2, function(x) x + 1L),
    list(values = c(2L, 3L), chunk_size = NULL)
  )
  expect_identical(
    util_par_lapply_lb(cluster, 1:2, function(x) x + 1L, chunk.size = 2L),
    list(values = c(2L, 3L), chunk_size = 2L)
  )
})
