test_that("util_heatmap_1th validates local inputs", {
  skip_on_cran()

  df <- data.frame(
    group = "A",
    subgroup = "B",
    extra = "C",
    value = 1,
    text = "one",
    stringsAsFactors = FALSE
  )

  expect_error(
    util_heatmap_1th(
      df = df,
      cat_vars = c("group", "subgroup", "extra"),
      values = "value",
      threshold = 1,
      invert = FALSE
    ),
    "cat_vars can have 1 or 2 elements"
  )
  expect_error(
    util_heatmap_1th(
      df = df,
      cat_vars = "group",
      values = "text",
      threshold = 1,
      invert = FALSE
    ),
    "must be numeric"
  )
  expect_error(
    util_heatmap_1th(
      df = df,
      cat_vars = "group",
      values = "value",
      invert = FALSE
    ),
    "No threshold"
  )
})

test_that("util_heatmap_1th works", {
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  skip_on_cran() # will be remoed, currently used only by segment missingness which is also tested # nolint: line_length_linter.
  meta_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data.RData") # nolint: line_length_linter.
  study_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData", keep_types = TRUE) # nolint: line_length_linter.
  label_col <- LABEL
  prep_prepare_dataframes(.internal = TRUE)
  hm1 <- util_heatmap_1th(
    df = ds1,
    cat_vars = c("CENTER_0", "USR_BP_0"),
    values = "SBP_0",
    threshold = 100,
    invert = FALSE
  )
  expect_lt(
    abs(suppressWarnings(
      sum(as.numeric(as.matrix(hm1$SummaryPlot$data)), na.rm = TRUE)
    ) - 1134545),
    0.8
  )
  skip_on_cran()
  skip_if_not_installed("vdiffr")
  hm2 <- util_heatmap_1th(
    df = ds1,
    cat_vars = c("USR_BP_0"),
    values = "SBP_0",
    threshold = 100,
    invert = TRUE,
    strata = "CENTER_0"
  )
  hm3 <- util_heatmap_1th(
    df = ds1,
    cat_vars = c("USR_BP_0"),
    values = "SBP_0",
    threshold = 100,
    invert = TRUE,
    strata = "CENTER_0",
    right_intv = TRUE
  )
  suppressWarnings({
    expect_doppelganger2(
      "util_heatmap_1th_1",
      hm1$SummaryPlot
    )
    expect_doppelganger2(
      "util_heatmap_1th_2",
      hm2$SummaryPlot
    )
    expect_doppelganger2(
      "util_heatmap_1th_3",
      hm3$SummaryPlot
    )
  })
})
