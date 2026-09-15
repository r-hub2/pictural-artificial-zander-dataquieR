test_that("util_margins_ord rejects unsupported covariates early", {
  skip_on_cran()
  skip_if_not_installed("ordinal")

  ds1 <- data.frame(
    resp = ordered(rep(c("low", "mid", "high"), length.out = 9)),
    group = factor(rep(c("a", "b", "c"), each = 3)),
    covar = seq_len(9)
  )

  expect_error(
    util_margins_ord(
      resp_vars = "resp",
      group_vars = "group",
      co_vars = "covar",
      ds1 = ds1,
      label_col = "VAR_NAMES"
    ),
    "Covariate argument for ordinal regression"
  )
})

test_that("util_margins_ord rejects constant ordinal responses", {
  skip_on_cran()
  skip_if_not_installed("ordinal")

  ds1 <- data.frame(
    resp = ordered(rep("low", 9), levels = c("low", "mid", "high")),
    group = factor(rep(c("a", "b", "c"), each = 3))
  )

  expect_error(
    util_margins_ord(
      resp_vars = "resp",
      group_vars = "group",
      co_vars = NULL,
      ds1 = ds1,
      label_col = "VAR_NAMES"
    ),
    "response variable is constant"
  )
})
