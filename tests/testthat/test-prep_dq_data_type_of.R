test_that("prep_dq_data_type_of works", {
  skip_on_cran()
  expect_equal(prep_dq_data_type_of(1:10), "integer")
  expect_null(prep_dq_data_type_of(cars))
})

test_that("prep_dq_data_type_of handles time and character inputs", {
  skip_on_cran()

  expect_equal(
    prep_dq_data_type_of(list(hms::as_hms("12:34:56"))),
    DATA_TYPES$TIME
  )
  expect_equal(
    prep_dq_data_type_of(list(structure(1, class = "times"))),
    DATA_TYPES$TIME
  )
  expect_equal(
    prep_dq_data_type_of(structure(1, class = "time_of_day")),
    DATA_TYPES$TIME
  )
  expect_equal(
    prep_dq_data_type_of(c("1", "2"), guess_character = TRUE),
    DATA_TYPES$INTEGER
  )
  expect_equal(
    prep_dq_data_type_of(c("1", "2"), guess_character = FALSE),
    DATA_TYPES$STRING
  )
  expect_error(
    prep_dq_data_type_of(1:3, guess_character = c(TRUE, FALSE)),
    "Need exactly one"
  )
})
