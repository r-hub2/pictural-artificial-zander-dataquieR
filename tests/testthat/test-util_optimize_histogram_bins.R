test_that("util_optimize_histogram_bins handles finite and constant inputs", {
  skip_on_cran()

  filtered <- util_optimize_histogram_bins(c(1, 2, Inf, NA), nbins_max = 5)
  expect_equal(filtered, list(c(1, 2)))

  constant <- util_optimize_histogram_bins(rep(2, 5), nbins_max = 3)
  expect_length(constant, 1)
  expect_equal(constant[[1]], c(0, 2))

  expect_error(
    util_optimize_histogram_bins(c(NA_real_, Inf)),
    "Nothing to plot|Need at least one element"
  )
})

test_that("util_optimize_histogram_bins respects interval and cut segments", {
  skip_on_cran()

  breaks <- util_optimize_histogram_bins(
    1:10,
    interval_freedman_diaconis = "[3;7]",
    nbins_max = 4,
    cuts = 5
  )

  expect_length(breaks, 4)
  expect_equal(breaks[[1]], c(-1, 3))
  expect_equal(breaks[[2]], c(3, 5))
  expect_equal(breaks[[3]], c(5, 7))
  expect_equal(breaks[[4]], c(7, 11))

  outside <- util_optimize_histogram_bins(
    1:5,
    interval_freedman_diaconis = "[10;20]",
    nbins_max = 4
  )
  expect_length(outside, 2)
  expect_true(min(unlist(outside)) <= 1)
  expect_true(max(unlist(outside)) >= 20)
})

test_that("util_optimize_histogram_bins removes only near-duplicate cuts", {
  skip_on_cran()

  epsilon <- .Machine$double.eps
  breaks <- expect_silent(util_optimize_histogram_bins(
    c(0, 3),
    cuts = c(1, 1 + epsilon, 2),
    nbins_max = 10
  ))

  internal_limits <- vapply(
    breaks[-length(breaks)],
    tail,
    FUN.VALUE = numeric(1),
    n = 1
  )
  expect_equal(internal_limits, c(1, 2))
})

test_that("util_optimize_histogram_bins covers rounding errors outside cuts", {
  skip_on_cran()

  below <- 1 - .Machine$double.eps / 2
  breaks_below <- util_optimize_histogram_bins(
    c(below, 1.25, 1.5, 2),
    interval_freedman_diaconis = "[1;2]",
    cuts = c(1, 2),
    nbins_max = 100
  )

  expect_length(breaks_below, 2)
  expect_lte(breaks_below[[1]][1], below)
  expect_silent(hist(below, plot = FALSE, breaks = breaks_below[[1]]))

  above <- 0.5 + .Machine$double.eps / 2
  breaks_above <- util_optimize_histogram_bins(
    c(0, 0.25, 0.5, above),
    interval_freedman_diaconis = "[0;0.5]",
    cuts = c(0, 0.5),
    nbins_max = 100
  )

  expect_length(breaks_above, 2)
  expect_gte(tail(breaks_above[[2]], 1), above)
  expect_silent(hist(above, plot = FALSE, breaks = breaks_above[[2]]))
})

test_that("util_optimize_histogram_bins preserves datetime break classes", {
  skip_on_cran()

  x_ct <- as.POSIXct("2020-01-01 00:00:00", tz = "UTC") + 0:3 * 3600
  breaks_ct <- util_optimize_histogram_bins(x_ct, nbins_max = 3)
  expect_s3_class(breaks_ct[[1]], "POSIXct")
  expect_true(is.unsorted(breaks_ct[[1]], strictly = TRUE) == FALSE)
  expect_true(min(breaks_ct[[1]]) <= min(x_ct))
  expect_true(max(breaks_ct[[1]]) >= max(x_ct))

  cuts_lt <- as.POSIXlt(x_ct[2])
  breaks_with_cuts <- util_optimize_histogram_bins(
    x_ct,
    cuts = cuts_lt,
    nbins_max = 3
  )
  expect_true(all(vapply(
    breaks_with_cuts,
    inherits,
    logical(1),
    what = "POSIXct"
  )))
  expect_true(any(vapply(
    breaks_with_cuts,
    function(x) x_ct[2] %in% x,
    logical(1)
  )))

  x_lt <- as.POSIXlt(x_ct)
  expect_error(
    util_optimize_histogram_bins(x_lt, nbins_max = 3),
    "must match the predicate"
  )
})

test_that("util_optimize_histogram_bins accepts hms vectors", {
  skip_on_cran()
  skip_if_not_installed("hms")

  x <- hms::as_hms(c(0, 3600, 7200, NA))
  breaks <- util_optimize_histogram_bins(x, nbins_max = 3)

  expect_length(breaks, 1)
  expect_type(breaks[[1]], "double")
  expect_true(min(breaks[[1]]) <= 0)
  expect_true(max(breaks[[1]]) >= 7200)
})
