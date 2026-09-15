test_that("nominal margins reject sparse response and group data early", {
  skip_on_cran()

  expect_error(
    util_margins_nom(
      resp_vars = "outcome",
      group_vars = "group",
      co_vars = character(),
      min_obs_in_subgroup = 1,
      min_obs_in_cat = 1,
      ds1 = data.frame(outcome = rep("a", 4), group = rep("A", 4)),
      label_col = LABEL
    ),
    "response variable is constant"
  )

  expect_error(
    util_margins_nom(
      resp_vars = "outcome",
      group_vars = "group",
      co_vars = character(),
      min_obs_in_subgroup = 1,
      min_obs_in_cat = 2,
      ds1 = data.frame(outcome = c("a", "a", "a", "b"), group = rep("A", 4)),
      label_col = LABEL
    ),
    "Not enough observations per category"
  )

  expect_error(
    util_margins_nom(
      resp_vars = "outcome",
      group_vars = "group",
      co_vars = character(),
      min_obs_in_subgroup = 3,
      min_obs_in_cat = 1,
      ds1 = data.frame(
        outcome = c("a", "b", "a", "b", "a", "b"),
        group = c("A", "A", "A", "A", "B", "B")
      ),
      label_col = LABEL
    ),
    "Not enough observations per group"
  )
})

test_that("nominal margins reject unsupported covariates early", {
  skip_on_cran()

  ds1 <- data.frame(
    outcome = rep(c("a", "b"), each = 4),
    group = rep(c("A", "B"), 4),
    covar = seq_len(8)
  )

  expect_error(
    util_margins_nom(
      resp_vars = "outcome",
      group_vars = "group",
      co_vars = "covar",
      min_obs_in_subgroup = 2,
      min_obs_in_cat = 2,
      ds1 = ds1,
      label_col = LABEL
    ),
    "only supported for factor variables"
  )
})

test_that("nominal margins return grouped probabilities and plots", {
  skip_on_cran()
  skip_if_not_installed("emmeans")
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("nnet")

  old_lazy <- getOption("dataquieR.lazy_plots")
  on.exit(options(dataquieR.lazy_plots = old_lazy), add = TRUE)
  options(dataquieR.lazy_plots = FALSE)

  ds1 <- data.frame(
    outcome = factor(rep(c("a", "b", "c"), each = 12)),
    group = factor(rep(c("A", "B", "C"), times = 12))
  )

  result <- util_margins_nom(
    resp_vars = "outcome",
    group_vars = "group",
    co_vars = character(),
    min_obs_in_subgroup = 5,
    min_obs_in_cat = 5,
    ds1 = ds1,
    label_col = LABEL,
    sort_group_var_levels = FALSE,
    title = "Nominal margins"
  )

  expect_named(result, c("plot_data", "plot"))
  expect_s3_class(result$plot_data, "data.frame")
  expect_s3_class(result$plot, "ggplot")
  expect_setequal(as.character(result$plot_data$group), c("A", "B", "C"))
  expect_setequal(as.character(result$plot_data$outcome), c("a", "b", "c"))
  expect_true(all(result$plot_data$sample_size == 4L))
  expect_true(all(result$plot_data$GRADING %in% c(0, 1)))
})
