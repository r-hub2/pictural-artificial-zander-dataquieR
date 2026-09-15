test_that("print.interval handles POSIXct bounds", {
  skip_on_cran()

  posix_interval <- structure(
    list(
      inc_l = TRUE,
      low = as.POSIXct("2020-01-01 12:00:00", tz = "UTC"),
      upp = as.POSIXct("2020-01-02 13:30:00", tz = "UTC"),
      inc_u = FALSE
    ),
    class = "interval"
  )

  expect_output(
    print(posix_interval),
    "[2020-01-01 12:00:00;2020-01-02 13:30:00)",
    fixed = TRUE
  )
  expect_identical(
    as.character(posix_interval),
    "[2020-01-01 12:00:00;2020-01-02 13:30:00)"
  )
})
