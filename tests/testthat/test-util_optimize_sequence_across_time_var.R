test_that(
  "util_optimize_sequence_across_time_var returns sorted unique times",
  {
    skip_on_cran()

    times <- as.POSIXct("2020-01-01 00:00:00") +
      c(120, NA, 0, 60, 60)

    sequence <- util_optimize_sequence_across_time_var(times, n_points = 5)

    expect_equal(
      as.numeric(sequence),
      sort(unique(as.numeric(na.omit(times))))
    )
  }
)

test_that("util_optimize_sequence_across_time_var reduces dense time data", {
  skip_on_cran()

  times <- as.POSIXct("2020-01-01 00:00:00") +
    c(0, 60, 120, 180, 240, 300, 300)

  sequence <- util_optimize_sequence_across_time_var(
    times,
    n_points = 4,
    prop_grid = 0.75
  )

  expect_gte(length(sequence), 3L)
  expect_lte(length(sequence), 4L)
  expect_equal(as.numeric(sequence[[1]]), min(as.numeric(times)))
  expect_equal(as.numeric(sequence[[length(sequence)]]), max(as.numeric(times)))
  expect_true(all(diff(as.numeric(sequence)) > 0))
})

test_that("util_optimize_sequence_across_time_var preserves UTC instants", {
  skip_on_cran()

  times <- as.POSIXct("2020-01-01 00:00:00", tz = "UTC") +
    c(0, 60, 120, 180, 240, 300)

  sequence <- util_optimize_sequence_across_time_var(
    times,
    n_points = 4,
    prop_grid = 0.75
  )

  expect_equal(as.numeric(sequence[[1]]), min(as.numeric(times)))
  expect_equal(as.numeric(sequence[[length(sequence)]]), max(as.numeric(times)))
  expect_true(all(diff(as.numeric(sequence)) > 0))
})

test_that("util_optimize_sequence_across_time_var handles subsecond data", {
  skip_on_cran()

  times <- as.POSIXct("2020-01-01 00:00:00", tz = "UTC") +
    seq(0, 0.8, by = 0.1)

  sequence <- util_optimize_sequence_across_time_var(
    times,
    n_points = 3,
    prop_grid = 1
  )

  expect_s3_class(sequence, "POSIXct")
  expect_equal(as.numeric(sequence[[1]]), min(as.numeric(times)))
  expect_equal(as.numeric(sequence[[length(sequence)]]), max(as.numeric(times)))
  expect_true(all(diff(as.numeric(sequence)) >= 0))
})

test_that("util_optimize_sequence_across_time_var adds distribution points", {
  skip_on_cran()

  times <- as.POSIXct("2020-01-01 00:00:00", tz = "UTC") +
    c(seq(0, 600, by = 60), rep(600, 4))

  sequence <- util_optimize_sequence_across_time_var(
    times,
    n_points = 8,
    prop_grid = 0.5
  )

  expect_s3_class(sequence, "POSIXct")
  expect_gte(length(sequence), 4L)
  expect_lte(length(sequence), 8L)
  expect_equal(as.numeric(sequence[[1]]), min(as.numeric(times)))
  expect_equal(as.numeric(sequence[[length(sequence)]]), max(as.numeric(times)))
})

test_that("util_optimize_sequence_across_time_var validates inputs", {
  skip_on_cran()

  times <- as.POSIXct("2020-01-01 00:00:00") + c(0, 60)

  expect_error(
    util_optimize_sequence_across_time_var(times, n_points = 2),
    "n_points"
  )
  expect_error(
    util_optimize_sequence_across_time_var(times, n_points = 3, prop_grid = 0),
    "prop_grid"
  )
  expect_error(
    util_optimize_sequence_across_time_var(times[1], n_points = 3),
    "Internal error in sequence optimization"
  )
})
