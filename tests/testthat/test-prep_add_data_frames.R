test_that("prep_add_data_frames names, appends, and resolves cached data", {
  skip_on_cran()

  cache <- new.env(parent = emptyenv())
  result <- with_dataframe_environment(quote({
    local_df <- data.frame(x = 1)
    prep_add_data_frames(local_df)
    expect_true("local_df" %in% ls(.dataframe_environment()))
    expect_identical(get("local_df", envir = .dataframe_environment()),
      local_df)

    prep_add_data_frames(named = data.frame(x = 2))
    expect_silent(
      prep_add_data_frames(named = data.frame(x = 3), append = TRUE)
    )
    expect_equal(as.vector(get("named", envir = .dataframe_environment())$x),
      c(2, 3))

    prep_add_data_frames(alias = "named")
    expect_equal(as.vector(get("alias", envir = .dataframe_environment())$x),
      c(2, 3))

    TRUE
  }), env = cache)

  expect_true(result)
})

test_that("prep_add_data_frames aligns names with dots only", {
  skip_on_cran()

  cache <- new.env(parent = emptyenv())
  result <- with_dataframe_environment(quote({
    first <- data.frame(x = 1)
    second <- data.frame(x = 2)
    expect_silent(prep_add_data_frames(
      first,
      data_frame_list = list(second = second),
      append = FALSE
    ))
    expect_setequal(ls(.dataframe_environment()), c("first", "second"))
    TRUE
  }), env = cache)

  expect_true(result)
})

test_that("prep_add_data_frames rejects invalid cache entries", {
  skip_on_cran()

  cache <- new.env(parent = emptyenv())
  with_dataframe_environment(quote({
    expect_error(prep_add_data_frames(bad = 1), "Not all entries")
    expect_error(prep_add_data_frames(data_frame_list = 1),
      "needs to be a list")
  }), env = cache)
})

test_that("prep_add_data_frames ignores nulls and coerces shaped objects", {
  skip_on_cran()

  cache <- new.env(parent = emptyenv())
  result <- with_dataframe_environment(quote({
    shaped <- matrix(1:4, ncol = 2)
    prep_add_data_frames(
      data_frame_list = list(
        shaped = shaped,
        empty = NULL
      )
    )

    expect_true("shaped" %in% ls(.dataframe_environment()))
    expect_false("empty" %in% ls(.dataframe_environment()))
    expect_s3_class(get("shaped", envir = .dataframe_environment()),
      "data.frame")
    expect_equal(
      as.vector(get("shaped", envir = .dataframe_environment())[[1]]),
      c(1, 2)
    )
    TRUE
  }), env = cache)

  expect_true(result)
})
