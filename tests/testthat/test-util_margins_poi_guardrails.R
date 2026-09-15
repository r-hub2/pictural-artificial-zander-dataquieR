test_that("Poisson margins reject inapplicable response and group data", {
  skip_on_cran()

  expect_error(
    util_margins_poi(
      resp_vars = "outcome",
      group_vars = "group",
      co_vars = character(),
      threshold_type = "none",
      threshold_value = 1,
      min_obs_in_subgroup = 1,
      ds1 = data.frame(outcome = rep(1, 4), group = rep("A", 4)),
      label_col = LABEL
    ),
    "response variable is constant"
  )

  expect_error(
    util_margins_poi(
      resp_vars = "outcome",
      group_vars = "group",
      co_vars = character(),
      threshold_type = "none",
      threshold_value = 1,
      min_obs_in_subgroup = 3,
      ds1 = data.frame(
        outcome = c(1, 2, 3, 4, 5, 6),
        group = c("A", "A", "A", "A", "B", "B")
      ),
      label_col = LABEL
    ),
    "Not enough observations per group"
  )
})

test_that("Poisson margins return grouped rates and a plot", {
  skip_on_cran()

  ds1 <- data.frame(
    outcome = c(1L, 2L, 1L, 3L, 4L, 5L, 2L, 3L),
    group = factor(c("B", "B", "B", "B", "A", "A", "A", "A"))
  )

  result <- suppressMessages(util_margins_poi(
    resp_vars = "outcome",
    group_vars = "group",
    co_vars = character(),
    threshold_type = "none",
    threshold_value = 1,
    min_obs_in_subgroup = 2,
    ds1 = ds1,
    label_col = LABEL,
    sort_group_var_levels = TRUE,
    include_numbers_in_figures = FALSE
  ))

  expect_named(result, c("plot_data", "plot"))
  expect_s3_class(result$plot, "patchwork")
  expect_equal(as.character(result$plot_data$group), c("A", "B"))
  expect_equal(result$plot_data$sample_size, c(4L, 4L))
  expect_equal(result$plot_data$margins, c(3.5, 1.75), tolerance = 1e-8)
  expect_equal(result$plot_data$threshold, c(1, 1))
  expect_equal(result$plot_data$GRADING, c(0, 0))
})

test_that("Poisson margins handle user thresholds and count labels", {
  skip_on_cran()

  old_lazy <- getOption("dataquieR.lazy_plots")
  on.exit(options(dataquieR.lazy_plots = old_lazy), add = TRUE)
  options(dataquieR.lazy_plots = FALSE)

  ds1 <- data.frame(
    outcome = c(1L, 2L, 1L, 3L, 4L, 5L, 2L, 3L),
    group = factor(c("B", "B", "B", "B", "A", "A", "A", "A"))
  )

  result <- suppressMessages(util_margins_poi(
    resp_vars = "outcome",
    group_vars = "group",
    co_vars = character(),
    threshold_type = "user",
    threshold_value = 2.5,
    min_obs_in_subgroup = 2,
    ds1 = ds1,
    label_col = LABEL,
    sort_group_var_levels = FALSE,
    include_numbers_in_figures = TRUE,
    title = "Poisson margins"
  ))

  expect_named(result, c("plot_data", "plot"))
  expect_s3_class(result$plot, "patchwork")
  expect_equal(as.character(result$plot_data$group), c("A", "B"))
  expect_equal(result$plot_data$sample_size, c(4L, 4L))
  expect_equal(result$plot_data$margins, c(3.5, 1.75), tolerance = 1e-8)
  expect_equal(result$plot_data$threshold, c(2.5, 2.5))
  expect_equal(result$plot_data$GRADING, c(0, 0))
})
