test_that("util_create_mahalanobis_ggplot returns plot data and sizing hints", {
  skip_on_cran()

  result <- util_create_mahalanobis_ggplot(
    md_ratio = c(0.5, 1.5, 0.1),
    mahalanobis_threshold = 0.975,
    df = 2
  )

  expect_named(result, c("plot_MD", "MD_values", "theoretical_q"))
  expect_s3_class(result$plot_MD, "ggplot")
  expect_equal(result$MD_values, sort(c(0.5, 1.5, 0.1)) * stats::qchisq(
    0.975,
    df = 2
  ))
  expect_equal(
    result$theoretical_q,
    stats::qchisq(stats::ppoints(3), df = 2)
  )

  hints <- attr(result$plot_MD, "sizing_hints", exact = TRUE)
  expect_equal(hints$figure_type_id, "mahalanobis_plot")
  expect_gt(hints$range_x, 0)
  expect_gt(hints$range_y, 0)
})
