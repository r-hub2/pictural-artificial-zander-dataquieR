local_summary_stub <- function(title, var_name, label) {
  this <- new.env(parent = emptyenv())
  this$result <- data.frame(
    VAR_NAMES = var_name,
    indicator_metric = "metric",
    GRADING = 1,
    stringsAsFactors = FALSE
  )
  this$rowmaxes <- data.frame(
    VAR_NAMES = var_name,
    GRADING = 1,
    stringsAsFactors = FALSE
  )
  this$labels_of_var_names_in_report <- setNames(label, var_name)
  this$alias_names <- setNames(paste0(title, "_alias"), paste0(title, "_call"))
  this$stopped_functions <- setNames(character(0), character(0))
  this$colors <- c(cat1 = "#000000")
  this$labels <- list(cat1 = "Category 1")
  this$order_of <- list(cat1 = 1)
  this$filter_of <- list(cat1 = "cat1")
  this$ordered_call_names <- paste0(title, "_call")
  this$ordered_var_names <- setNames(var_name, label)
  this$rownames_of_report <- label
  this$colnames_of_report <- "metric"
  this$title <- title
  this$subtitle <- paste0(title, " subtitle")
  this$meta_data <- data.frame(
    VAR_NAMES = var_name,
    LABEL = label,
    VARIABLE_ROLE = "process",
    stringsAsFactors = FALSE
  )
  this$summary_meta_data <- this$meta_data
  this$meta_data_cross_item <- NULL
  this$variable_group_call_names <- character()
  this$label_col <- LABEL
  this$used_data_file <- paste0(title, ".csv")
  this$stratum <- "all_observations"
  this$segment <- "all_variables"

  util_attach_attr(
    structure(NA, class = "dataquieR_summary"),
    this = this
  )
}

test_that(
  "util_combine_list_report_summaries handles empty and singleton inputs",
  {
    skip_on_cran()

    testthat::local_mocked_bindings(
      util_get_rule_sets = function(...) list(),
      util_get_ruleset_formats = function(...) list()
    )

    empty <- util_combine_list_report_summaries(list())

    expect_s3_class(empty, "dataquieR_summary")
    expect_equal(as.data.frame(attr(empty, "repsum_wide", exact = TRUE)),
      data.frame(),
      ignore_attr = TRUE
    )
    expect_identical(attr(attr(empty, "repsum_wide", exact = TRUE),
        "label_col",
        exact = TRUE
      ), VAR_NAMES)
    expect_equal(attr(empty, "this", exact = TRUE)$result, data.frame())

    singleton <- local_summary_stub("first", "var_a", "Variable A")
    expect_identical(
      util_combine_list_report_summaries(list(singleton)),
      singleton
    )
    expect_error(util_combine_list_report_summaries(list(list())))
  }
)

test_that(
  "util_combine_list_report_summaries merges summaries with unique variables",
  {
    skip_on_cran()

    first <- local_summary_stub("first", "var_a", "Variable A")
    second <- local_summary_stub("second", "var_b", "Variable B")

    combined <- util_combine_list_report_summaries(
      list(first, second),
      type = "unique_vars"
    )
    this <- attr(combined, "this", exact = TRUE)

    expect_s3_class(combined, "dataquieR_summary")
    expect_equal(this$result[[VAR_NAMES]], c("var_a", "var_b"))
    expect_equal(this$result$origin, c("first", "second"))
    expect_equal(this$meta_data[[VAR_NAMES]], c("var_a", "var_b"))
    expect_true(isTRUE(attr(this$meta_data, "normalized", exact = TRUE)))
    expect_identical(as.character(this$label_col), "new_label")
    expect_identical(
      attr(this$label_col, "orig_label_col", exact = TRUE),
      LABEL
    )
    expect_equal(this$rownames_of_report, c("Variable A", "Variable B"))
    expect_equal(this$ordered_var_names, c(
      "Variable A" = "var_a",
      "Variable B" = "var_b"
    ))
  }
)

test_that("util_combine_list_report_summaries prefixes repeated variables", {
  skip_on_cran()

  first <- local_summary_stub("first", "var", "Variable")
  second <- local_summary_stub("second", "var", "Variable")

  combined <- util_combine_list_report_summaries(
    list(first, second),
    type = "repeated_vars"
  )
  this <- attr(combined, "this", exact = TRUE)

  expect_s3_class(combined, "dataquieR_summary")
  expect_identical(
    this$result[[VAR_NAMES]],
    c("first.csv-all_observations-var", "second.csv-all_observations-var")
  )
  expect_identical(
    this$result$..Origin,
    c("first.csv-all_observations", "second.csv-all_observations")
  )
  expect_identical(
    this$meta_data[[VAR_NAMES]],
    c("first.csv-all_observations-var", "second.csv-all_observations-var")
  )
})

test_that("combined summaries preserve variable-group context", {
  skip_on_cran()

  first <- local_summary_stub("first", "var_a", "Variable A")
  second <- local_summary_stub("second", "var_b", "Variable B")
  first_this <- util_attr(first, "this", exact = TRUE)
  second_this <- util_attr(second, "this", exact = TRUE)
  first_this$summary_meta_data <- util_rbind(data_frames_list = list(
    first_this$meta_data,
    data.frame(
      VAR_NAMES = "group_a",
      LABEL = "Group A",
      stringsAsFactors = FALSE
    )
  ))
  second_this$summary_meta_data <- util_rbind(data_frames_list = list(
    second_this$meta_data,
    data.frame(
      VAR_NAMES = "group_b",
      LABEL = "Group B",
      stringsAsFactors = FALSE
    )
  ))
  first_this$variable_group_call_names <- "con_contradictions_redcap"
  second_this$variable_group_call_names <- c(
    "con_contradictions_redcap", "acc_mahalanobis"
  )
  first_this$result[[CHECK_ID]] <- "group_a"
  second_this$result[[CHECK_ID]] <- "group_b"
  first_this$result$function_name <- "con_contradictions_redcap"
  second_this$result$function_name <- "acc_mahalanobis"
  first_this$meta_data_cross_item <- data.frame(
    CHECK_ID = "group_a",
    CHECK_LABEL = "Group A",
    stringsAsFactors = FALSE
  )
  second_this$meta_data_cross_item <- data.frame(
    CHECK_ID = "group_b",
    CHECK_LABEL = "Group B",
    stringsAsFactors = FALSE
  )

  singleton <- util_combine_list_report_summaries(list(first))
  expect_identical(
    util_attr(
      util_attr(singleton, "this", exact = TRUE)$meta_data_cross_item,
      "contradiction_only_check_ids",
      exact = TRUE
    ),
    "group_a"
  )

  combined <- util_combine_list_report_summaries(
    list(first, second),
    type = "unique_vars"
  )
  this <- util_attr(combined, "this", exact = TRUE)

  expect_identical(
    this$variable_group_call_names,
    c("con_contradictions_redcap", "acc_mahalanobis")
  )
  expect_identical(
    this$meta_data_cross_item[[CHECK_ID]],
    c("group_a", "group_b")
  )
  expect_identical(this$result[[CHECK_ID]], c("group_a", "group_b"))
  expect_identical(
    util_attr(
      this$meta_data_cross_item,
      "contradiction_only_check_ids",
      exact = TRUE
    ),
    "group_a"
  )
  expect_setequal(
    this$summary_meta_data[[VAR_NAMES]],
    c("var_a", "group_a", "var_b", "group_b")
  )
  expect_identical(this$summary_meta_data, this$meta_data)
})

test_that("util_combine_list_report_summaries handles empty CAT results", {
  skip_on_cran()

  first <- local_summary_stub("first", "var_a", "Variable A")
  second <- local_summary_stub("second", "var_b", "Variable B")
  first_this <- attr(first, "this", exact = TRUE)
  second_this <- attr(second, "this", exact = TRUE)
  first_this$result$indicator_metric <- "CAT_one"
  second_this$result$indicator_metric <- "MSG_one"
  first_this$labels_of_var_names_in_report <- character()
  second_this$labels_of_var_names_in_report <- character()

  combined <- util_combine_list_report_summaries(
    list(first, second),
    type = "unique_vars"
  )
  this <- attr(combined, "this", exact = TRUE)

  expect_identical(nrow(this$result), 0L)
  expect_identical(names(this$result), "origin")
  expect_identical(this$labels_of_var_names_in_report, character())
})

test_that("util_combine_list_report_summaries preserves shared titles", {
  skip_on_cran()

  first <- local_summary_stub("shared", "var_a", "Variable A")
  second <- local_summary_stub("shared", "var_b", "Variable B")

  combined <- util_combine_list_report_summaries(
    list(first, second),
    type = "unique_vars"
  )
  this <- attr(combined, "this", exact = TRUE)

  expect_identical(this$title, "shared")
  expect_null(attr(this$title, "default", exact = TRUE))
  expect_identical(this$subtitle, "shared subtitle")
  expect_null(attr(this$subtitle, "default", exact = TRUE))
})

test_that("util_combine_list_report_summaries warns on color conflicts", {
  skip_on_cran()

  first <- local_summary_stub("first", "var_a", "Variable A")
  second <- local_summary_stub("second", "var_b", "Variable B")
  first_this <- attr(first, "this", exact = TRUE)
  second_this <- attr(second, "this", exact = TRUE)
  second_this$colors <- c(cat1 = "#ffffff")

  expect_warning(
    combined <- util_combine_list_report_summaries(
      list(first, second),
      type = "unique_vars"
    ),
    "Colors of first report"
  )
  this <- attr(combined, "this", exact = TRUE)

  expect_identical(this$colors, first_this$colors)
})

test_that("util_combine_list_report_summaries builds segmented origins", {
  skip_on_cran()

  first <- local_summary_stub("first", "var", "Variable")
  second <- local_summary_stub("second", "var", "Variable")
  first_this <- attr(first, "this", exact = TRUE)
  second_this <- attr(second, "this", exact = TRUE)
  first_this$used_data_file <- file.path("nested", "first.csv")
  first_this$stratum <- "site_a"
  first_this$segment <- "female"
  second_this$used_data_file <- file.path("nested", "second.csv")
  second_this$stratum <- "site_b"
  second_this$segment <- "male"

  combined <- util_combine_list_report_summaries(
    list(first, second),
    type = "repeated_vars"
  )
  this <- attr(combined, "this", exact = TRUE)

  expect_identical(
    this$result$..Origin,
    c("first.csv-site_a-female", "second.csv-site_b-male")
  )
  expect_identical(
    this$result[[VAR_NAMES]],
    c("first.csv-site_a-female-var", "second.csv-site_b-male-var")
  )
})

test_that("util_combine_list_report_summaries rejects duplicate origins", {
  skip_on_cran()

  first <- local_summary_stub("first", "var", "Variable")
  second <- local_summary_stub("second", "var", "Variable")
  first_this <- attr(first, "this", exact = TRUE)
  second_this <- attr(second, "this", exact = TRUE)
  first_this$used_data_file <- "same.csv"
  second_this$used_data_file <- "same.csv"

  expect_error(
    util_combine_list_report_summaries(
      list(first, second),
      type = "repeated_vars"
    ),
    "same title"
  )
})

test_that("util_combine_list_report_summaries rejects label_col conflicts", {
  skip_on_cran()

  first <- local_summary_stub("first", "var_a", "Variable A")
  second <- local_summary_stub("second", "var_b", "Variable B")
  second_this <- attr(second, "this", exact = TRUE)
  second_this$meta_data$ALT_LABEL <- "Variable B alt"
  second_this$label_col <- "ALT_LABEL"

  expect_error(
    util_combine_list_report_summaries(
      list(first, second),
      type = "unique_vars"
    ),
    "different label_col"
  )
})
