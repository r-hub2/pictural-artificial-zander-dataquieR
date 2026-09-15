test_that("util_gg_get handles defaults and plain containers", {
  skip_on_cran()

  expect_identical(util_gg_get(NULL, "x", default = "fallback"), "fallback")
  expect_identical(util_gg_get(list(x = 1), NULL, default = "fallback"),
    "fallback")
  expect_identical(util_gg_get(list(x = 1), "x", default = "fallback"), 1)
  expect_identical(util_gg_get(list(y = 1), "x", default = "fallback"),
    "fallback")

  attributed <- structure(1, custom_field = "from-attribute")
  expect_identical(util_gg_get(attributed, "custom_field"),
    "from-attribute")
})

test_that(
  "util_gg_get reads environments and normalizes known list-like fields",
  {
    skip_on_cran()

    env <- new.env(parent = emptyenv())
    env$value <- 42
    expect_identical(util_gg_get(env, "value"), 42)
    expect_identical(util_gg_get(env, "missing", default = "fallback"),
      "fallback")

    data_env <- new.env(parent = emptyenv())
    data_env$a <- 1
    holder <- new.env(parent = emptyenv())
    holder$data <- data_env

    expect_identical(util_gg_get(holder, "data"), list(a = 1))
  }
)

test_that(
  "util_gg_get reads ggproto fields without environment normalization",
  {
    skip_on_cran()
    skip_if_not_installed("ggplot2")

    probe <- ggplot2::ggproto("CoverageProbe", NULL,
      data = new.env(parent = emptyenv()),
      nullable = NULL,
      value = 7)
    probe$data$a <- 1

    expect_identical(util_gg_get(probe, "value"), 7)
    expect_true(is.environment(util_gg_get(probe, "data")))
    expect_null(util_gg_get(probe, "nullable"))
    expect_identical(util_gg_get(probe, "missing", default = "fallback"),
      "fallback")
  }
)
