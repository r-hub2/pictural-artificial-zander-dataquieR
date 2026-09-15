test_that("util_check_group_levels works", {
  skip_on_cran()
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")

  prep_load_workbook_like_file("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx") # nolint: line_length_linter.

  f <- function(study_data, meta_data = "item_level", label_col) {
    ds1 <- prep_prepare_dataframes(
      .study_data = study_data,
      .meta_data = meta_data,
      .label_col = LABEL,
      .internal = TRUE
    )

    dim(util_check_group_levels(ds1, "CENTER_0"))

    expect_equal(
      nrow(util_check_group_levels(ds1, "USR_BP_0", min_obs_in_subgroup = 400)),
      448
    )
  }
  environment(f) <- environment(acc_margins)
  expect_message2(f(
    study_data = "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData", # nolint: line_length_linter.
    label_col = LABEL
  ))
})

test_that("util_check_group_levels handles local subgroup constraints", {
  skip_on_cran()

  study_data <- data.frame(
    group = c("a", "a", "b", "b", "c"),
    value = seq_len(5),
    stringsAsFactors = FALSE
  )

  expect_message(
    filtered <- util_check_group_levels(
      study_data,
      "group",
      min_obs_in_subgroup = 2
    ),
    "Discarding 1 observations"
  )
  expect_equal(as.character(filtered$group), c("a", "a", "b", "b"))
  expect_false(attr(filtered, "TOO_MANY", exact = TRUE))

  expect_message(
    filtered_max <- util_check_group_levels(
      study_data,
      "group",
      max_obs_in_subgroup = 1
    ),
    "Discarding 4 observations"
  )
  expect_equal(as.character(filtered_max$group), "c")
  expect_false(attr(filtered_max, "TOO_MANY", exact = TRUE))

  too_many <- util_check_group_levels(study_data, "group", max_subgroups = 2)
  expect_true(attr(too_many, "TOO_MANY", exact = TRUE))
  expect_equal(nrow(too_many), nrow(study_data))

  expect_error(
    util_check_group_levels(study_data, "group", min_subgroups = 4),
    "Too few subgroups"
  )
  expect_error(
    util_check_group_levels(
      study_data,
      "group",
      min_obs_in_subgroup = 3,
      max_obs_in_subgroup = 2
    )
  )
})
