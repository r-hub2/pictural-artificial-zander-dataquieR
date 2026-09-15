skip_on_cran()

test_that("util_is_time_only classifies character and numeric time values", {
  skip_if_not_installed("hms")

  expect_true(util_is_time_only(hms::hms(seconds = 1)))
  expect_true(util_is_time_only(structure(0.5, class = "times")))
  expect_true(util_is_time_only(
    as.POSIXct("2026-07-20 03:04:05", tz = "UTC")
  ))

  expect_true(util_is_time_only(c("12:13:14", NA_character_)))
  expect_false(util_is_time_only(c("12:13:14", NA_character_), na_ok = FALSE))
  expect_false(util_is_time_only(c("12:13:14", "not a time")))

  expect_true(util_is_time_only(c(0.5, 0.25)))
  expect_true(util_is_time_only(c(100, 90000)))
  expect_false(util_is_time_only(c(-1, 100)))
})

test_that("util_is_time_only handles nested list inputs", {
  expect_true(util_is_time_only(list("01:02:03", c(0.5, NA))))
  expect_false(util_is_time_only(list("01:02:03", "bad")))
})

test_that("util_as_time_only normalizes common inputs to hms", {
  from_character <- util_as_time_only(c("12:13:14", NA_character_))
  expect_s3_class(from_character, "hms")
  expect_identical(as.character(from_character), c("12:13:14", NA))

  from_numeric <- util_as_time_only(c(0.5, 86401))
  expect_s3_class(from_numeric, "hms")
  expect_identical(as.character(from_numeric), c("12:00:00", "00:00:01"))

  from_list <- util_as_time_only(list("01:02:03", "bad"))
  expect_s3_class(from_list[[1]], "hms")
  expect_true(is.na(from_list[[2]]))
})

test_that("util_as_time_only normalizes classed time-like inputs", {
  skip_if_not_installed("hms")

  from_hms <- hms::hms(seconds = c(1, NA))
  expect_identical(util_as_time_only(from_hms), from_hms)

  from_times <- util_as_time_only(structure(0.5, class = "times"))
  expect_s3_class(from_times, "hms")
  expect_identical(as.character(from_times), "12:00:00")

  from_posix <- util_as_time_only(
    as.POSIXct("2026-07-20 03:04:05", tz = "UTC")
  )
  expect_s3_class(from_posix, "hms")
  expect_identical(as.character(from_posix), "03:04:05")

  from_difftime <- util_as_time_only(
    as.difftime(c(1, 86401), units = "secs")
  )
  expect_s3_class(from_difftime, "hms")
  expect_identical(as.character(from_difftime), c("00:00:01", "00:00:01"))

  from_factor <- util_as_time_only(factor(c("01:02:03", NA)))
  expect_s3_class(from_factor, "hms")
  expect_identical(as.character(from_factor), c("01:02:03", NA))

  expect_error(util_as_time_only(TRUE))
})
