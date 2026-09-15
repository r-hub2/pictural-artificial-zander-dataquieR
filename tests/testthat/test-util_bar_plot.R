skip_on_cran()

test_that("util_bar_plot renders relative labels and manual fill colors", {
  plot_data <- data.frame(
    category = c("A", "B"),
    proportion = c(0.2, 0.8),
    group = c("low", "high"),
    stringsAsFactors = FALSE
  )

  plot <- util_bar_plot(
    plot_data = plot_data,
    cat_var = "category",
    num_var = "proportion",
    relative = TRUE,
    fill_var = "group",
    colors = c(low = "#000000", high = "#ffffff"),
    show_color_legend = TRUE
  )
  text_layer <- ggplot2::layer_data(plot, 2)

  expect_equal(text_layer$label, c("20%", "80%"))
  expect_equal(text_layer$vjust, c(-0.5, 1.5))
  expect_equal(text_layer$hjust, c(0.5, 0.5))
})

test_that("util_bar_plot supports flipped numeric fill plots", {
  plot_data <- data.frame(
    category = c("A", "B"),
    count = c(2, 8),
    score = c(0.2, 0.8),
    stringsAsFactors = FALSE
  )

  plot <- util_bar_plot(
    plot_data = plot_data,
    cat_var = "category",
    num_var = "count",
    fill_var = "score",
    colors = c("#ffffff", "#000000"),
    flip = TRUE
  )
  text_layer <- ggplot2::layer_data(plot, 2)

  expect_equal(text_layer$label, c(" 2 ", " 8 "))
  expect_equal(text_layer$vjust, c(0.5, 0.5))
  expect_equal(text_layer$hjust, c(0, 1))
  expect_equal(text_layer$colour, c("black", "white"))
})

test_that("util_bar_plot supports unfilled plots without numbers", {
  plot_data <- data.frame(
    category = c("A", "B"),
    count = c(2, 8),
    stringsAsFactors = FALSE
  )

  plot <- util_bar_plot(
    plot_data = plot_data,
    cat_var = "category",
    num_var = "count",
    colors = "#123456",
    show_numbers = FALSE
  )
  bar_layer <- ggplot2::layer_data(plot, 1)

  expect_equal(bar_layer$fill, c("#123456", "#123456"))
  expect_equal(nrow(bar_layer), 2)
  expect_equal(length(plot$layers), 1)
})

test_that("util_bar_plot supports relative numeric fill legends", {
  plot_data <- data.frame(
    category = c("A", "B"),
    proportion = c(0.2, 0.8),
    score = c(0.25, 0.75),
    stringsAsFactors = FALSE
  )

  plot <- util_bar_plot(
    plot_data = plot_data,
    cat_var = "category",
    num_var = "proportion",
    relative = TRUE,
    fill_var = "score",
    colors = c("#ffffff", "#000000"),
    show_color_legend = TRUE
  )
  text_layer <- ggplot2::layer_data(plot, 2)

  expect_equal(text_layer$label, c("20%", "80%"))
  expect_true(length(plot$scales$scales) >= 2)
  expect_true(any(vapply(
    plot$scales$scales,
    function(scale) "fill" %in% scale$aesthetics,
    logical(1)
  )))
})
