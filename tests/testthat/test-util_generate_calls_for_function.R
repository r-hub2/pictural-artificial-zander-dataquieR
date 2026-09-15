test_that(
  "util_generate_calls_for_function fills one call per response variable",
  {
    skip_on_cran()

    meta_data <- data.frame(
      VAR_NAMES = c("x", "y"),
      LABEL = c("X", "Y"),
      VARIABLE_ROLE = c(VARIABLE_ROLES$PRIMARY, VARIABLE_ROLES$PRIMARY),
      stringsAsFactors = FALSE
    )

    calls <- util_generate_calls_for_function(
      fkt = "des_summary",
      meta_data = meta_data,
      label_col = LABEL,
      meta_data_segment = NULL,
      meta_data_dataframe = NULL,
      meta_data_cross_item = NULL,
      specific_args = list(des_summary = list(
        item_level = "custom",
        hard_limits_removal = FALSE
      )),
      arg_overrides = list(item_level = "override"),
      resp_vars = c("X", "Y"),
      ssi_functions = character(),
      non_ssi_functions = "des_summary"
    )

    expect_identical(names(calls), c("X", "Y"))
    expect_identical(calls$X$study_data, quote(study_data))
    expect_identical(calls$X$meta_data, quote(meta_data))
    expect_identical(calls$X$label_col, LABEL)
    expect_identical(calls$X$resp_vars, "X")
    expect_identical(calls$Y$resp_vars, "Y")
    expect_identical(calls$X$item_level, "custom")
    expect_false(calls$X$hard_limits_removal)
  }
)

test_that(
  "util_generate_calls_for_function creates cross-item variable groups",
  {
    skip_on_cran()

    meta_data <- data.frame(
      VAR_NAMES = c("x", "y"),
      LABEL = c("X", "Y"),
      stringsAsFactors = FALSE
    )
    meta_data_cross_item <- data.frame(
      CHECK_ID = "check_1",
      CHECK_LABEL = "comparison",
      VARIABLE_LIST = "X|Y",
      stringsAsFactors = FALSE
    )

    calls <- util_generate_calls_for_function(
      fkt = "acc_repeated_measurements",
      meta_data = meta_data,
      label_col = LABEL,
      meta_data_segment = NULL,
      meta_data_dataframe = NULL,
      meta_data_cross_item = meta_data_cross_item,
      specific_args = list(),
      arg_overrides = list(),
      resp_vars = NULL,
      ssi_functions = character(),
      non_ssi_functions = "acc_repeated_measurements"
    )

    expect_identical(names(calls), "comparison")
    expect_identical(calls$comparison$variable_group, c("X", "Y"))
    expect_identical(calls$comparison$study_data, quote(study_data))
    expect_identical(calls$comparison$meta_data, quote(meta_data))
    expect_identical(calls$comparison$label_col, LABEL)
  }
)

test_that(
  "util_generate_calls_for_function drops unlabeled cross-item rows",
  {
    skip_on_cran()

    meta_data <- data.frame(
      VAR_NAMES = c("x", "y"),
      LABEL = c("X", "Y"),
      stringsAsFactors = FALSE
    )
    meta_data_cross_item <- data.frame(
      CHECK_ID = c("check_1", "check_2"),
      CHECK_LABEL = c("comparison", NA_character_),
      VARIABLE_LIST = c("X|Y", "X|Y"),
      stringsAsFactors = FALSE
    )

    expect_message(
      calls <- util_generate_calls_for_function(
        fkt = "acc_repeated_measurements",
        meta_data = meta_data,
        label_col = LABEL,
        meta_data_segment = NULL,
        meta_data_dataframe = NULL,
        meta_data_cross_item = meta_data_cross_item,
        specific_args = list(),
        arg_overrides = list(),
        resp_vars = NULL,
        ssi_functions = character(),
        non_ssi_functions = "acc_repeated_measurements"
      ),
      "Removing rows from"
    )

    expect_identical(names(calls), "comparison")
    expect_identical(calls$comparison$variable_group, c("X", "Y"))
  }
)

test_that(
  "util_generate_calls_for_function creates one all-observations call",
  {
    skip_on_cran()

    calls <- util_generate_calls_for_function(
      fkt = "pro_applicability_matrix",
      meta_data = data.frame(VAR_NAMES = "x", LABEL = "X"),
      label_col = LABEL,
      meta_data_segment = data.frame(segment = "all"),
      meta_data_dataframe = data.frame(dataframe = "study"),
      meta_data_cross_item = NULL,
      specific_args = list(
        pro_applicability_matrix = list(max_vars_per_plot = 5L)
      ),
      arg_overrides = list(),
      resp_vars = NULL,
      ssi_functions = character(),
      non_ssi_functions = "pro_applicability_matrix"
    )

    expect_identical(names(calls), "[ALL]")
    expect_identical(calls[["[ALL]"]]$max_vars_per_plot, 5L)
    expect_identical(calls[["[ALL]"]]$study_data, quote(study_data))
    expect_identical(calls[["[ALL]"]]$meta_data, quote(meta_data))
    expect_identical(
      calls[["[ALL]"]]$meta_data_segment,
      quote(meta_data_segment)
    )
    expect_identical(
      calls[["[ALL]"]]$meta_data_dataframe,
      quote(meta_data_dataframe)
    )
  }
)

test_that(
  "util_generate_calls_for_function keeps cross-item metadata for resp vars",
  {
    skip_on_cran()

    meta_data <- data.frame(
      VAR_NAMES = c("x", "y"),
      LABEL = c("X", "Y"),
      VARIABLE_ROLE = VARIABLE_ROLES$PRIMARY,
      stringsAsFactors = FALSE
    )
    meta_data_cross_item <- data.frame(
      CHECK_ID = "check_1",
      CHECK_LABEL = "comparison",
      VARIABLE_LIST = "X|Y",
      stringsAsFactors = FALSE
    )

    calls <- util_generate_calls_for_function(
      fkt = "acc_mahalanobis_ratio",
      meta_data = meta_data,
      label_col = LABEL,
      meta_data_segment = NULL,
      meta_data_dataframe = NULL,
      meta_data_cross_item = meta_data_cross_item,
      specific_args = list(),
      arg_overrides = list(),
      resp_vars = c("X", "Y"),
      ssi_functions = character(),
      non_ssi_functions = "acc_mahalanobis_ratio"
    )

    expect_identical(names(calls), c("X", "Y"))
    expect_identical(calls$X$meta_data_cross_item,
      quote(meta_data_cross_item))
    expect_identical(calls$Y$meta_data_cross_item,
      quote(meta_data_cross_item))
    expect_identical(calls$X$resp_vars, "X")
    expect_identical(calls$Y$resp_vars, "Y")
  }
)

test_that(
  "util_generate_calls_for_function carries computed group identities",
  {
    skip_on_cran()

    meta_data <- data.frame(
      VAR_NAMES = "MISS_RESP_group_1",
      LABEL = "Missing responses",
      CHECK_ID = "group_1",
      VARIABLE_ROLE = VARIABLE_ROLES$PRIMARY,
      stringsAsFactors = FALSE
    )
    meta_data_cross_item <- data.frame(
      CHECK_ID = "group_1",
      CHECK_LABEL = "Questionnaire group",
      VARIABLE_LIST = "x | y",
      stringsAsFactors = FALSE
    )

    calls <- util_generate_calls_for_function(
      fkt = "con_ssi_range_check",
      meta_data = meta_data,
      label_col = LABEL,
      meta_data_segment = NULL,
      meta_data_dataframe = NULL,
      meta_data_cross_item = meta_data_cross_item,
      specific_args = list(),
      arg_overrides = list(),
      resp_vars = "Missing responses",
      ssi_functions = "con_ssi_range_check",
      non_ssi_functions = character()
    )

    expect_identical(util_attr(calls[[1]], CHECK_ID, exact = TRUE), "group_1")
    expect_identical(
      util_attr(calls[[1]], CHECK_LABEL, exact = TRUE),
      "Questionnaire group"
    )
  }
)
