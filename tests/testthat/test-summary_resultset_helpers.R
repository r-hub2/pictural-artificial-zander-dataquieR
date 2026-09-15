skip_on_cran()

test_that("util_compute_rowmaxes ignores process pseudo-metrics", {
  result <- data.frame(
    VAR_NAMES = c("v1", "v1", "v1", "v2", "v2"),
    indicator_metric = c("PCT_demo", "CAT_process", "MSG_process",
      "NUM_demo", "PCT_demo"),
    class = c("2", "5", "5", "1", "4"),
    stringsAsFactors = FALSE
  )

  labels <- as.list(stats::setNames(paste("cat", 1:5), paste0("cat", 1:5)))
  labels[["NA"]] <- "missing"
  colors <- as.list(stats::setNames(
    c("#ffffff", "#eeeeee", "#cccccc", "#999999", "#000000"),
    paste0("cat", 1:5)
  ))
  colors[["NA"]] <- "#ffffff00"
  order_of <- stats::setNames(c(seq_len(5), NA_integer_),
    c(paste0("cat", 1:5), "NA"))
  filter_of <- as.list(stats::setNames(c(paste("cat", 1:5), "missing"),
      c(paste0("cat", 1:5), "NA")))
  labels_of_var_names <- c(v1 = "Variable 1", v2 = "Variable 2")

  rowmaxes <- util_compute_rowmaxes(
    result = result,
    labels = labels,
    colors = colors,
    order_of = order_of,
    filter_of = filter_of,
    labels_of_var_names_in_report = labels_of_var_names
  )

  expect_equal(rowmaxes$VAR_NAMES, c("v1", "v2"))
  expect_equal(as.integer(rowmaxes$rowmax), c(2L, 4L))
  expect_equal(unname(rowmaxes$label), c("cat 2", "cat 4"))
  expect_equal(unname(rowmaxes$filter), c("cat 2", "cat 4"))
  expect_named(rowmaxes$cell_text, c("Variable 1", "Variable 2"))
  expect_true(all(grepl("background:", rowmaxes$cell_text, fixed = TRUE)))
})
