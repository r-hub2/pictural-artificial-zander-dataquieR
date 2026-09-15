test_that("binary margins reject non-binary and sparse outcomes", {
  skip_on_cran()

  expect_error(
    util_margins_bin(
      resp_vars = "outcome",
      group_vars = "group",
      co_vars = character(),
      threshold_type = "none",
      threshold_value = 1,
      min_obs_in_subgroup = 1,
      min_obs_in_cat = 1,
      ds1 = data.frame(outcome = c(0, 1, 2), group = rep("A", 3)),
      label_col = LABEL
    ),
    "response variable is not binary"
  )

  expect_error(
    util_margins_bin(
      resp_vars = "outcome",
      group_vars = "group",
      co_vars = character(),
      threshold_type = "none",
      threshold_value = 1,
      min_obs_in_subgroup = 1,
      min_obs_in_cat = 2,
      ds1 = data.frame(outcome = c(0, 0, 0, 1), group = rep("A", 4)),
      label_col = LABEL
    ),
    "Not enough observations per category"
  )

  expect_error(
    util_margins_bin(
      resp_vars = "outcome",
      group_vars = "group",
      co_vars = character(),
      threshold_type = "none",
      threshold_value = 1,
      min_obs_in_subgroup = 3,
      min_obs_in_cat = 1,
      ds1 = data.frame(
        outcome = c(0, 1, 0, 1, 0, 1),
        group = c("A", "A", "A", "A", "B", "B")
      ),
      label_col = LABEL
    ),
    "Not enough observations per group"
  )
})

test_that("binary margins handle user thresholds without overall panel", {
  skip_on_cran()
  skip_if_not_installed("emmeans")
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("patchwork")

  old_lazy <- getOption("dataquieR.lazy_plots")
  on.exit(options(dataquieR.lazy_plots = old_lazy), add = TRUE)
  options(dataquieR.lazy_plots = FALSE)

  ds1 <- data.frame(
    outcome = rep(c(0, 1), 20),
    group = rep(c("A", "B"), each = 20),
    stringsAsFactors = FALSE
  )

  expect_message(
    result <- util_margins_bin(
      resp_vars = "outcome",
      group_vars = "group",
      co_vars = character(),
      threshold_type = "user",
      threshold_value = 0.5,
      min_obs_in_subgroup = 5,
      min_obs_in_cat = 5,
      ds1 = ds1,
      label_col = LABEL,
      no_geom_count_in_bin = TRUE,
      no_overall_in_bin = FALSE,
      include_numbers_in_figures = FALSE,
      title = "Binary margins"
    ),
    "Cannot have"
  )

  expect_named(result, c("plot_data", "plot"))
  expect_s3_class(result$plot_data, "data.frame")
  expect_s3_class(result$plot, "patchwork")
  expect_equal(result$plot_data$threshold, c(0.5, 0.5))
  expect_true(all(result$plot_data$GRADING %in% c(0, 1)))
})

test_that("binary margins handle none thresholds with overall panel", {
  skip_on_cran()
  skip_if_not_installed("emmeans")
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("patchwork")

  old_lazy <- getOption("dataquieR.lazy_plots")
  on.exit(options(dataquieR.lazy_plots = old_lazy), add = TRUE)
  options(dataquieR.lazy_plots = FALSE)

  ds1 <- data.frame(
    outcome = rep(c(0, 1), 30),
    group = rep(c("A", "B", "C"), each = 20),
    stringsAsFactors = FALSE
  )

  result <- util_margins_bin(
    resp_vars = "outcome",
    group_vars = "group",
    co_vars = character(),
    threshold_type = "none",
    threshold_value = 1,
    min_obs_in_subgroup = 5,
    min_obs_in_cat = 5,
    ds1 = ds1,
    label_col = LABEL,
    no_geom_count_in_bin = FALSE,
    no_overall_in_bin = FALSE,
    include_numbers_in_figures = TRUE,
    title = "Binary margins"
  )

  expect_named(result, c("plot_data", "plot"))
  expect_s3_class(result$plot_data, "data.frame")
  expect_s3_class(result$plot, "patchwork")
  expect_equal(as.character(result$plot_data$group), c("A", "B", "C"))
  expect_equal(result$plot_data$sample_size, c(20L, 20L, 20L))
  expect_equal(result$plot_data$threshold, c(1, 1, 1))
  expect_equal(result$plot_data$GRADING, c(0, 0, 0))
})
