test_that("util_subsample_cases returns eligible cases without subsampling", {
  skip_on_cran()

  study_data <- data.frame(
    id = paste0("c", seq_len(10)),
    x = c(NA, NA, seq_len(8)),
    y = c(seq_len(8), NA, NA)
  )

  expect_equal(
    util_subsample_cases(study_data, x = "x", nmax = 20, case_id = "id"),
    paste0("c", 3:10)
  )

  expect_equal(
    util_subsample_cases(study_data, x = "x", y = "y", nmax = 20,
      case_id = "id"),
    paste0("c", 3:8)
  )

  expect_equal(
    util_subsample_cases(
      data.frame(id = paste0("c", 1:3), x = NA_real_),
      x = "x",
      case_id = "id"
    ),
    character(0)
  )
})

test_that("util_subsample_cases random sampling is reproducible", {
  skip_on_cran()

  study_data <- data.frame(id = paste0("c", seq_len(10)), x = seq_len(10))

  first_sample <- util_subsample_cases(
    study_data, x = "x", nmax = 4, random = TRUE, case_id = "id", seed = 123
  )
  second_sample <- util_subsample_cases(
    study_data, x = "x", nmax = 4, random = TRUE, case_id = "id", seed = 123
  )

  expect_equal(first_sample, second_sample)
  expect_length(first_sample, 4)
  expect_true(all(first_sample %in% study_data$id))
})

test_that("util_subsample_cases validates scalar control arguments", {
  skip_on_cran()

  study_data <- data.frame(id = seq_len(4), x = seq_len(4), y = seq_len(4))

  expect_error(
    util_subsample_cases(study_data, x = "x", nmax = 0),
    "nmax.+positive integer"
  )
  expect_error(
    util_subsample_cases(study_data, x = "x", pinc = 2),
    "pinc.+\\[0, 1\\]"
  )
  expect_error(
    util_subsample_cases(study_data, x = "x", resids = -0.1),
    "resids.+\\[0, 1\\]"
  )
  expect_error(
    util_subsample_cases(study_data, x = "x", random = c(TRUE, FALSE)),
    "Need exactly one"
  )
  expect_error(
    util_subsample_cases(study_data, x = "x", case_id = "missing_id"),
    "missing_id"
  )
})

test_that("util_subsample_cases keeps univariate support boundaries", {
  skip_on_cran()

  study_data <- data.frame(id = seq_len(20), x = seq_len(20))

  selected <- util_subsample_cases(
    study_data, x = "x", nmax = 5, pinc = 0.4, case_id = "id", seed = 1
  )

  expect_true(1 %in% selected)
  expect_true(20 %in% selected)
  expect_true(all(selected %in% study_data$id))
})

test_that("util_subsample_cases can include all remaining random-fill cases", {
  skip_on_cran()

  study_data <- data.frame(id = paste0("c", seq_len(6)), x = seq_len(6))

  selected <- util_subsample_cases(
    study_data,
    x = "x",
    nmax = 5,
    pinc = 0.5,
    case_id = "id",
    seed = 2
  )

  expect_setequal(selected, study_data$id)
})

test_that("util_subsample_cases handles constant univariate values", {
  skip_on_cran()

  study_data <- data.frame(id = paste0("c", seq_len(8)), x = rep(1, 8))

  selected <- util_subsample_cases(
    study_data,
    x = "x",
    nmax = 4,
    pinc = 0.5,
    case_id = "id",
    seed = 11
  )

  expect_true("c1" %in% selected)
  expect_true(length(selected) <= 4)
  expect_true(all(selected %in% study_data$id))
})

test_that("util_subsample_cases keeps bivariate boundaries and residuals", {
  skip_on_cran()

  study_data <- data.frame(
    id = seq_len(20),
    x = seq_len(20),
    y = c(seq_len(19), 100)
  )

  selected <- util_subsample_cases(
    study_data, x = "x", y = "y", nmax = 6, pinc = 0.35, resids = 0.2,
    case_id = "id", seed = 1
  )

  expect_true(all(c(1, 20) %in% selected))
  expect_true(all(selected %in% study_data$id))
})

test_that("util_subsample_cases can skip residual selection", {
  skip_on_cran()

  study_data <- data.frame(
    id = seq_len(12),
    x = seq_len(12),
    y = c(seq_len(11), 100)
  )

  selected <- util_subsample_cases(
    study_data,
    x = "x",
    y = "y",
    nmax = 5,
    pinc = 0.4,
    resids = 0,
    case_id = "id",
    seed = 7
  )

  expect_true(all(c(1, 12) %in% selected))
  expect_true(length(selected) <= 7)
  expect_true(all(selected %in% study_data$id))
})

test_that("util_subsample_cases handles constant bivariate axes", {
  skip_on_cran()

  selected_x_constant <- util_subsample_cases(
    data.frame(id = seq_len(6), x = 1, y = seq_len(6)),
    x = "x", y = "y", nmax = 3, pinc = 0.5, resids = 0.2,
    case_id = "id", seed = 1
  )

  selected_y_constant <- util_subsample_cases(
    data.frame(id = seq_len(6), x = seq_len(6), y = 1),
    x = "x", y = "y", nmax = 3, pinc = 0.5, resids = 0.2,
    case_id = "id", seed = 1
  )

  expect_true(all(c(1, 6) %in% selected_x_constant))
  expect_true(all(c(1, 6) %in% selected_y_constant))
})

test_that("util_subsample_cases handles fully constant bivariate data", {
  skip_on_cran()

  selected <- util_subsample_cases(
    data.frame(id = paste0("c", seq_len(6)), x = 1, y = 2),
    x = "x",
    y = "y",
    nmax = 3,
    pinc = 0.5,
    resids = 0.5,
    case_id = "id",
    seed = 1
  )

  expect_true("c1" %in% selected)
  expect_true(all(selected %in% paste0("c", seq_len(6))))
})

test_that("util_subsample_cases caps a negative bivariate random-fill budget", {
  skip_on_cran()

  study_data <- data.frame(
    id = seq_len(8),
    x = seq_len(8),
    y = c(seq_len(7), 30)
  )

  selected <- util_subsample_cases(
    study_data,
    x = "x",
    y = "y",
    nmax = 4,
    pinc = 0.8,
    resids = 0.8,
    case_id = "id",
    seed = 1
  )

  expect_true(all(selected %in% study_data$id))
  expect_true(all(c(1, 8) %in% selected))
})
