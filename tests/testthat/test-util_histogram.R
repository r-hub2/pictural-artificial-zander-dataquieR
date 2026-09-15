skip_on_cran()

test_that("util_histogram builds plain and grouped histograms", {
  skip_on_cran()
  skip_if_not_installed("ggplot2")

  old_lazy <- getOption("dataquieR.lazy_plots")
  withr::defer(options(dataquieR.lazy_plots = old_lazy))
  options(dataquieR.lazy_plots = FALSE)

  plot_data <- data.frame(
    value = seq_len(12),
    group = factor(rep(c("a", "b", "c"), each = 4)),
    panel = factor(rep(c("left", "right"), each = 6))
  )

  plain <- util_histogram(plot_data["value"], nbins_max = 4)
  grouped <- util_histogram(
    plot_data,
    num_var = "value",
    fill_var = "group",
    facet_var = "panel",
    nbins_max = 4,
    colors = "#2166AC"
  )

  expect_s3_class(plain, "ggplot")
  expect_s3_class(grouped, "ggplot")
  expect_type(ggplot2::ggplot_build(plain)$data[[1]]$x, "double")
  expect_setequal(
    ggplot2::ggplot_build(grouped)$plot$data$panel,
    c("left", "right")
  )
})

test_that("util_histogram preserves time x classes", {
  skip_on_cran()
  skip_if_not_installed("ggplot2")

  old_lazy <- getOption("dataquieR.lazy_plots")
  withr::defer(options(dataquieR.lazy_plots = old_lazy))
  options(dataquieR.lazy_plots = FALSE)

  times <- data.frame(value = util_parse_time(sprintf("%02d:00", 1:6)))

  time_plot <- util_histogram(times, nbins_max = 3, is_time = TRUE)

  expect_s3_class(
    suppressWarnings(ggplot2::ggplot_build(time_plot)),
    "ggplot_built"
  )
  expect_true(inherits(time_plot$data$histogram_x, "hms"))
})

test_that("util_histogram preserves grouped time x classes", {
  skip_on_cran()
  skip_if_not_installed("ggplot2")

  old_lazy <- getOption("dataquieR.lazy_plots")
  withr::defer(options(dataquieR.lazy_plots = old_lazy))
  options(dataquieR.lazy_plots = FALSE)

  times <- data.frame(
    value = util_parse_time(sprintf("%02d:00", 1:8)),
    group = factor(rep(c("a", "b"), each = 4))
  )

  time_plot <- util_histogram(
    times,
    fill_var = "group",
    nbins_max = 4,
    is_time = TRUE
  )

  expect_s3_class(
    suppressWarnings(ggplot2::ggplot_build(time_plot)),
    "ggplot_built"
  )
  expect_true(inherits(time_plot$data$histogram_x, "hms"))
  expect_setequal(time_plot$data$group, c("a", "b"))
})

test_that("util_histogram accepts POSIXct data for datetime plots", {
  datetimes <- data.frame(
    value = as.POSIXct("2020-01-01 00:00:00", tz = "UTC") + 0:5 * 3600
  )

  histogram <- util_histogram(
    datetimes,
    nbins_max = 3,
    is_datetime = TRUE
  )

  expect_s3_class(prep_realize_ggplot(histogram), "ggplot")
})
