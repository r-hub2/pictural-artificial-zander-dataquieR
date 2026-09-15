skip_on_cran()

test_that("util_multinomial_ci handles documented alpha boundaries", {
  counts <- c(2L, 3L, 5L)

  expect_equal(
    util_multinomial_ci(counts, alpha = 0),
    matrix(c(0, 0, 0, 1, 1, 1),
      ncol = 2,
      dimnames = list(NULL, c("lower", "upper")),
      byrow = FALSE
    ),
    ignore_attr = TRUE
  )

  expect_equal(
    util_multinomial_ci(counts, alpha = 1),
    cbind(lower = counts / sum(counts), upper = counts / sum(counts)),
    ignore_attr = TRUE
  )
})

test_that("util_multinomial_ci returns bounded intervals around proportions", {
  counts <- c(3L, 5L, 7L)
  ci <- util_multinomial_ci(counts, alpha = 0.05)
  p_hat <- counts / sum(counts)

  expect_equal(dim(ci), c(3L, 2L))
  expect_true(all(ci[, 1] >= 0))
  expect_true(all(ci[, 2] <= 1))
  expect_true(all(ci[, 1] <= p_hat))
  expect_true(all(ci[, 2] >= p_hat))
})

test_that("util_multinomial_ci handles zero-count categories", {
  skip_on_cran()

  counts <- c(0L, 4L, 6L)
  ci <- util_multinomial_ci(counts, alpha = 0.05)
  p_hat <- counts / sum(counts)

  expect_equal(dim(ci), c(3L, 2L))
  expect_equal(ci[1, 1], 0)
  expect_true(all(ci[, 1] >= 0))
  expect_true(all(ci[, 2] <= 1))
  expect_true(all(ci[, 1] <= p_hat))
  expect_true(all(ci[, 2] >= p_hat))
})

test_that(
  paste0(
    "util_multinomial_ci verbose mode reports progress ",
    "without changing intervals"
  ),
  {
    skip_on_cran()

    counts <- c(3L, 5L, 7L)
    quiet <- util_multinomial_ci(counts, alpha = 0.05)

    withr::local_options(dataquieR.testthat_expect_message_active = TRUE)
    captured <- new.env(parent = emptyenv())
    captured$messages <- character(0)
    verbose <- withCallingHandlers(
      util_multinomial_ci(counts, alpha = 0.05, verbose = TRUE),
      message = function(cond) {
        captured$messages <- c(captured$messages, conditionMessage(cond))
        invokeRestart("muffleMessage")
      }
    )

    expect_true(any(grepl("Final: c =", captured$messages, fixed = TRUE)))
    expect_equal(verbose, quiet)
  }
)

test_that("util_binomial_ci matches prop.test intervals", {
  counts <- c(4L, 6L, 10L)
  alpha <- 0.1
  ci <- util_binomial_ci(counts, alpha = alpha)
  expected <- t(vapply(
    counts,
    function(xi) {
      stats::prop.test(
        x = xi,
        n = sum(counts),
        conf.level = 1 - alpha
      )$conf.int
    },
    numeric(2)
  ))

  expect_equal(ci, unname(expected))
})

test_that("util_binomial_ci handles zero-count categories", {
  counts <- c(0L, 10L)

  ci <- util_binomial_ci(counts, alpha = 0.05)

  expect_equal(dim(ci), c(2L, 2L))
  expect_equal(ci[1, 1], 0)
  expect_equal(ci[2, 2], 1)
  expect_true(all(ci >= 0))
  expect_true(all(ci <= 1))
})

test_that("multinomial CI helpers reject invalid input", {
  expect_error(
    suppressWarnings(util_multinomial_ci(alpha = 0.05)),
    "Argument 'x' is missing"
  )
  expect_error(
    suppressWarnings(util_multinomial_ci(c(1), alpha = 0.05)),
    "'x' must be a numeric vector of length >= 2"
  )
  expect_error(
    suppressWarnings(util_multinomial_ci(c(1, Inf), alpha = 0.05)),
    "'x' must contain only finite values"
  )
  expect_error(
    suppressWarnings(util_multinomial_ci(c(1, -1), alpha = 0.05)),
    "'x' must contain non-negative counts"
  )
  expect_error(
    suppressWarnings(util_multinomial_ci(c(1, 1.5), alpha = 0.05)),
    "'x' must contain integer counts"
  )
  expect_error(
    suppressWarnings(util_multinomial_ci(c(0, 0), alpha = 0.05)),
    "sum\\(x\\) must be positive"
  )
  expect_error(
    suppressWarnings(util_binomial_ci(c(0, 0), alpha = 0.05)),
    "sum\\(x\\) must be positive"
  )
  expect_error(
    suppressWarnings(util_multinomial_ci(c(1, 1), alpha = NA_real_)),
    "'alpha' must be a single real number in \\[0, 1\\]"
  )
  expect_error(
    suppressWarnings(util_multinomial_ci(c(1, 1), alpha = 0.05, verbose = NA)),
    "'verbose' must be TRUE or FALSE"
  )
})
