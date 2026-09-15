skip_on_cran()

test_that(
  "util_normalize_time_only_columns keeps non-time-only columns stable",
  {
    study_data <- data.frame(
      t_text = c("09:10:00", "10:00:00"),
      t_posix = as.POSIXct(
        c("2020-01-01 09:00:00", "2020-01-01 10:00:00"),
        tz = "UTC"
      )
    )
    study_data$t_hms <- hms::hms(c(1, 2))

    normalized <- util_normalize_time_only_columns(study_data)

    expect_identical(normalized$t_text, study_data$t_text)
    expect_identical(normalized$t_posix, study_data$t_posix)
    expect_s3_class(normalized$t_hms, "hms")
    expect_equal(as.numeric(normalized$t_hms), c(1, 2))
  }
)

test_that("util_normalize_time_only_columns converts time-only vectors", {
  study_data <- data.frame(id = 1:2)
  study_data$t_times <- structure(c(0.5, 0.25), class = "times")
  study_data$t_itime <- structure(c(3600L, 7200L), class = "ITime")

  normalized <- util_normalize_time_only_columns(study_data)

  expect_s3_class(normalized$t_times, "hms")
  expect_equal(as.numeric(normalized$t_times), c(43200, 21600))
  expect_s3_class(normalized$t_itime, "hms")
  expect_equal(as.numeric(normalized$t_itime), c(3600, 7200))
})

test_that(
  "util_normalize_time_only_columns normalizes mixed time list-columns",
  {
    study_data <- data.frame(id = seq_len(4))
    study_data$t <- I(list(
      structure(0.5, class = "times"),
      hms::hms(3),
      NA,
      "not a time"
    ))

    expect_warning(
      normalized <- util_normalize_time_only_columns(study_data),
      "could not be casted to hms"
    )

    expect_s3_class(normalized$t, "hms")
    expect_equal(as.numeric(normalized$t), c(43200, 3, NA, NA))
  }
)

test_that("util_normalize_time_only_columns casts scalar list fallbacks", {
  skip_on_cran()

  study_data <- data.frame(id = seq_len(4))
  study_data$t <- I(list(
    structure(3600L, class = "ITime"),
    as.difftime(90, units = "secs"),
    120,
    as.POSIXct("2020-01-01 01:02:03", tz = "UTC")
  ))

  expect_warning(
    normalized <- util_normalize_time_only_columns(study_data),
    "could not be casted to hms"
  )

  expect_s3_class(normalized$t, "hms")
  expect_equal(as.numeric(normalized$t), c(3600, 90, 120, NA))
})

test_calcal_time_of_day <- function(hour, minute, second) {
  structure(
    data.frame(
      hour = hour,
      minute = minute,
      second = second
    ),
    class = c("time_of_day", "data.frame")
  )
}

test_that(
  "util_normalize_time_only_columns converts calcal time-of-day values",
  {
    skip_on_cran()

    study_data <- data.frame(id = 1:2)
    study_data$t <- test_calcal_time_of_day(
      hour = c(9, 10),
      minute = c(15, 30),
      second = c(0, 45)
    )

    normalized <- util_normalize_time_only_columns(study_data)

    expect_s3_class(normalized$t, "hms")
    expect_equal(as.numeric(normalized$t), c(33300, 37845))
  }
)

test_that(
  "util_normalize_time_only_columns keeps incomplete calcal records missing",
  {
    skip_on_cran()

    study_data <- data.frame(id = 1:2)
    study_data$t <- test_calcal_time_of_day(
      hour = c(9, NA_real_),
      minute = c(15, 30),
      second = c(0, 45)
    )

    normalized <- util_normalize_time_only_columns(study_data)

    expect_s3_class(normalized$t, "hms")
    expect_equal(as.numeric(normalized$t), c(33300, NA))
  }
)

test_that("util_normalize_time_only_columns handles calcal values in lists", {
  skip_on_cran()

  study_data <- data.frame(id = seq_len(3))
  study_data$t <- I(list(
    test_calcal_time_of_day(hour = 1, minute = 2, second = 3),
    test_calcal_time_of_day(hour = 4, minute = 5, second = 6),
    "plain text"
  ))

  expect_warning(
    normalized <- util_normalize_time_only_columns(study_data),
    "could not be casted to hms"
  )

  expect_s3_class(normalized$t, "hms")
  expect_equal(as.numeric(normalized$t), c(3723, 14706, NA))
})

test_that("util_normalize_time_only_columns handles pipeline list fallbacks", {
  skip_on_cran()

  study_data <- data.frame(id = seq_len(4))
  study_data$t <- I(list(
    structure(NA_real_, class = "times"),
    structure(NA_integer_, class = "ITime"),
    test_calcal_time_of_day(hour = 1:2, minute = 2:3, second = 3:4),
    hms::hms(5)
  ))

  testthat::local_mocked_bindings(
    .called_in_pipeline2 = function() TRUE
  )

  expect_message(
    normalized <- util_normalize_time_only_columns(study_data),
    "Found 3 non-hms"
  )

  expect_s3_class(normalized$t, "hms")
  expect_equal(as.numeric(normalized$t), c(NA, NA, NA, 5))
})
