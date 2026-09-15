skip_on_cran()

test_that("util_as_character preserves scalar vector semantics", {
  skip_if_not_installed("hms")

  expect_identical(
    util_as_character(hms::hms(seconds = c(1, NA, 3661.25))),
    c("00:00:01.00", NA_character_, "01:01:01.25")
  )
  expect_identical(
    util_as_character(as.difftime(c(1, NA, 3661.25), units = "secs")),
    c("00:00:01.00", NA_character_, "01:01:01.25")
  )

  times <- as.POSIXct(c("2026-07-09 12:34:56", NA), tz = "UTC")
  expect_identical(util_as_character(times), c(
    "2026-07-09 12:34:56",
    NA_character_
  ))
  expect_match(
    util_as_character(times[1], tz = "Europe/Berlin"),
    "^2026-07-09 14:34:56 CEST$"
  )

  expect_identical(util_as_character(factor(c("a", NA))), c("a", NA_character_))
  expect_identical(util_as_character(c("a", NA_character_)), c(
    "a",
    NA_character_
  ))
  expect_identical(util_as_character(c(1, NA_real_)), c("1", NA_character_))
  expect_identical(util_as_character(c(TRUE, FALSE, NA)), c(
    "TRUE",
    "FALSE", NA_character_
  ))
  expect_identical(util_as_character(numeric()), character())
  expect_identical(util_as_character(list()), character())

  fake_times <- structure(c(0.5, NA_real_), class = "times")
  expect_identical(
    util_as_character(fake_times),
    c("12:00:00", NA_character_)
  )

  expect_identical(
    util_as_character(as.Date("2026-07-20")),
    "2026-07-20"
  )
})

test_that(
  "util_as_character formats non-hms list-like values without recursion",
  {
    x <- list(
      as.POSIXct("2026-07-09 12:34:56", tz = "UTC"),
      factor("level"),
      structure(0.5, class = "times"),
      as.difftime(2, units = "secs"),
      "text",
      3,
      as.Date("2026-07-20"),
      list("nested"),
      NULL
    )

    expect_identical(
      util_as_character(x, tz = "Europe/Berlin"),
      c(
        "2026-07-09 14:34:56 CEST",
        "level",
        "12:00:00",
        "00:00:02",
        "text",
        "3",
        "2026-07-20",
        NA_character_,
        NA_character_
      )
    )

    expect_match(
      util_as_character(list(as.POSIXct(
        "2026-07-09 12:34:56",
        tz = "UTC"
      ))),
      "^2026-07-09 12:34:56 UTC$"
    )
  }
)

test_that(
  "util_as_character reports mixed hms values as messages in pipelines",
  {
    skip_if_not_installed("hms")

    testthat::local_mocked_bindings(
      .called_in_pipeline2 = function() TRUE
    )

    x <- list(
      hms::hms(seconds = 1),
      "not-a-time"
    )
    attr(x, "..cn") <- "clock"

    expect_message(
      out <- util_as_character(x),
      "Found 1 non-hms .+ values in .clock."
    )
    expect_identical(out, c("00:00:01", NA_character_))
  }
)

test_that(
  "util_as_character warns when mixed hms list values lose information",
  {
    skip_if_not_installed("hms")

    x <- list(
      hms::hms(seconds = 1),
      as.difftime(2, units = "secs"),
      as.POSIXct("2026-07-09 12:34:56", tz = "UTC"),
      "2",
      "not-a-time",
      list("nested"),
      NA_real_
    )
    attr(x, "..cn") <- "clock"

    expect_warning(
      out <- util_as_character(x),
      paste0(
        "Found 6 non-hms .+ values in .clock. -- 3 of which ",
        "could not be casted to hms"
      )
    )
    expect_identical(
      out,
      c(
        "00:00:01",
        "00:00:02",
        "12:34:56",
        NA_character_,
        NA_character_,
        NA_character_,
        NA_character_
      )
    )
  }
)

test_that("util_as_character handles broken hms data frame list columns", {
  skip_if_not_installed("hms")

  study_data <- data.frame(id = 1:4)
  study_data$clock <- I(list(
    hms::hms(seconds = 1),
    hms::hms(seconds = c(2, 3)),
    "not-a-time",
    NA_real_
  ))
  attr(study_data$clock, "..cn") <- "clock"

  expect_warning(
    out <- util_as_character(study_data$clock),
    paste0(
      "Found 2 non-hms .+ values in .clock. -- 1 of which ",
      "could not be casted to hms"
    )
  )
  expect_identical(
    out,
    c("00:00:01", NA_character_, NA_character_, NA_character_)
  )
})

test_that("util_as_character formats scalar time list entries", {
  skip_if_not_installed("hms")

  expect_message(
    out <- util_as_character(list(
      hms::hms(seconds = 1),
      as.difftime(2, units = "secs")
    )),
    "Found 1 non-hms"
  )
  expect_identical(out, c("00:00:01", "00:00:02"))
  expect_identical(.util_format_hms(hms::hms()), character(0))
})

test_that(
  "util_as_character handles mixed hms list edge cases deterministically",
  {
    skip_if_not_installed("hms")

    x <- list(
      hms::hms(seconds = 1),
      c("not", "scalar"),
      factor(NA_character_)
    )
    attr(x, "..cn") <- "clock"

    expect_warning(
      out <- util_as_character(x),
      paste0(
        "Found 2 non-hms .+ values in .clock. -- 1 of which ",
        "could not be casted to hms"
      )
    )
    expect_identical(
      out,
      c("00:00:01", NA_character_, NA_character_)
    )
  }
)
