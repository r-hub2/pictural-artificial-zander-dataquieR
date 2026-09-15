skip_on_cran()

test_that("util_meta_data_env works", {
  skip_on_cran() # slow
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  b <- util_meta_data_env(
    meta_data_v2 = "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/ship_meta_v2.xlsx", # nolint: line_length_linter.
    study_data = "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/ship.RDS" # nolint: line_length_linter.
  )
  xx <- b$call(acc_margins(
    resp_vars = "sbp1",
    group_vars = "DEV_BP_0"
  ))
  prep_purge_data_frame_cache()
  prep_load_workbook_like_file("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/ship_meta_v2.xlsx") # nolint: line_length_linter.
  yy <- acc_margins(
    resp_vars = "sbp1",
    group_vars = "DEV_BP_0",
    study_data = "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/ship.RDS", # nolint: line_length_linter.
    meta_data = "item_level",
    co_vars = c("SEX_0", "AGE_0"),
    label_col = LABEL
  )
  expect_equal(xx$ResultData, yy$ResultData, ignore_attr = TRUE)
  expect_equal(xx$SummaryPlot, yy$SummaryPlot, ignore_attr = TRUE)
  expect_equal(xx$SummaryTable[, -1, FALSE],
    yy$SummaryTable[, -1, FALSE],
    ignore_attr = TRUE
  )
})

test_that("decorator expands ambiguous metadata-derived group_vars", {
  prep_purge_data_frame_cache()
  withr::defer(prep_purge_data_frame_cache())

  study_data <- data.frame(
    SBP = c(1, 2, 3, 4, 5, 6),
    OBS = c("a", "a", "b", "b", "c", "c"),
    DEV = c("x", "y", "x", "y", "x", "y"),
    stringsAsFactors = FALSE
  )
  item_level <- data.frame(
    VAR_NAMES = c("SBP", "OBS", "DEV"),
    LABEL = c("SBP", "OBS", "DEV"),
    DATA_TYPE = c(DATA_TYPES$INTEGER, "string", "string"),
    SCALE_LEVEL = c("metric", "nominal", "nominal"),
    MISSING_LIST = c("", "", ""),
    JUMP_LIST = c("", "", ""),
    GROUP_VAR_OBSERVER = c("OBS", NA_character_, NA_character_),
    GROUP_VAR_DEVICE = c("DEV", NA_character_, NA_character_),
    stringsAsFactors = FALSE
  )

  meta_env <- util_meta_data_env(
    item_level = item_level,
    study_data = study_data,
    label_col = LABEL
  )
  expect_error(
    meta_env$provisionize_call(
      dataquieR::acc_margins(resp_vars = "SBP", threshold_type = "none")
    ),
    "you need to specify 'group_vars' explicitly"
  )

  expanded_calls <- meta_env$provisionize_call(
    dataquieR::acc_margins(resp_vars = "SBP", threshold_type = "none"),
    expand_ambiguous = TRUE
  )
  expect_true(util_attr(expanded_calls,
      "dataquieR_decorator_expanded",
      exact = TRUE
    ))
  expect_equal(
    names(expanded_calls),
    c(
      "group_vars=OBS (GROUP_VAR_OBSERVER)",
      "group_vars=DEV (GROUP_VAR_DEVICE)"
    )
  )
  expect_equal(
    unname(vapply(expanded_calls, function(x) as.character(x$group_vars),
        FUN.VALUE = character(1)
      )),
    c("OBS", "DEV")
  )

  withr::local_options(list(dataquieR.test_decorator = TRUE))
  result <- acc_margins(
    resp_vars = "SBP",
    study_data = study_data,
    meta_data = item_level,
    label_col = LABEL,
    threshold_type = "none"
  )

  expect_type(result, "list")
  expect_identical(class(result)[[1]], "list")
  expect_true(inherits(result, "master_result"))
  expect_equal(names(result), names(expanded_calls))
  expect_true(all(vapply(result, inherits, "dataquieR_result",
        FUN.VALUE = logical(1)
      )))
  expect_true(all(vapply(result, inherits, "master_result",
        FUN.VALUE = logical(1)
      )))
  expect_equal(
    unname(vapply(result, function(x) {
      as.character(util_attr(x, "call", exact = TRUE)$group_vars)
    }, FUN.VALUE = character(1))),
    c("OBS", "DEV")
  )
})

test_that("metadata call expansion labels unnamed ambiguous arguments", {
  expanded_calls <- util_expand_ambiguous_metadata_calls(
    quote(dataquieR::acc_margins(
      resp_vars = "SBP",
      group_vars = "GROUP"
    )),
    list(group_vars = c("GROUP_A", "GROUP_B"))
  )

  expect_true(util_attr(
    expanded_calls,
    "dataquieR_decorator_expanded",
    exact = TRUE
  ))
  expect_equal(
    names(expanded_calls),
    c("group_vars=GROUP_A", "group_vars=GROUP_B")
  )
  expect_equal(
    unname(vapply(
      expanded_calls,
      function(x) as.character(x$group_vars),
      FUN.VALUE = character(1)
    )),
    c("GROUP_A", "GROUP_B")
  )
})

test_that("decorator resolves call symbols from the supplied environment", {
  item_level <- data.frame(
    VAR_NAMES = c("SBP", "GRP", "AGE"),
    LABEL = c("SBP label", "Group label", "Age label"),
    DATA_TYPE = c(DATA_TYPES$INTEGER, "string", DATA_TYPES$INTEGER),
    SCALE_LEVEL = c("metric", "nominal", "metric"),
    MISSING_LIST = c("", "", ""),
    JUMP_LIST = c("", "", ""),
    VARIABLE_ROLE = c("Primary", "Secondary", "Secondary"),
    stringsAsFactors = FALSE
  )
  cross_item_level <- data.frame(
    CHECK_ID = "ci_bp",
    VARIABLE_LIST = "SBP | AGE",
    CHECK_LABEL = "Blood pressure pair",
    MAHALANOBIS_THRESHOLD = "0.9",
    stringsAsFactors = FALSE
  )
  meta_env <- util_meta_data_env(
    item_level = item_level,
    meta_data_cross_item = cross_item_level,
    label_col = LABEL
  )
  caller_env <- new.env(parent = emptyenv())
  caller_env$resp_vars <- "SBP label"
  caller_env$variable_group <- "ci_bp"

  item_call <- meta_env$provisionize_call(
    dataquieR::acc_margins(
      resp_vars = resp_vars,
      group_vars = "GRP",
      threshold_type = "none"
    ),
    env = caller_env
  )
  expect_identical(as.character(item_call$resp_vars), "SBP label")
  expect_identical(as.character(item_call$group_vars), "GRP")

  cross_item_call <- meta_env$provisionize_call(
    dataquieR::acc_mahalanobis(
      variable_group = variable_group,
      mahalanobis_threshold = 0.95
    ),
    env = caller_env
  )
  expect_identical(as.character(cross_item_call$variable_group), "ci_bp")
  expect_equal(as.numeric(cross_item_call$mahalanobis_threshold), 0.95)
})

test_that("decorator warns for unresolved explicit response variables", {
  item_level <- data.frame(
    VAR_NAMES = c("SBP", "GRP"),
    LABEL = c("SBP label", "Group label"),
    DATA_TYPE = c(DATA_TYPES$INTEGER, "string"),
    SCALE_LEVEL = c("metric", "nominal"),
    MISSING_LIST = c("", ""),
    JUMP_LIST = c("", ""),
    VARIABLE_ROLE = c("Primary", "Secondary"),
    stringsAsFactors = FALSE
  )
  meta_env <- util_meta_data_env(
    item_level = item_level,
    label_col = LABEL
  )

  expect_warning(
    item_call <- meta_env$provisionize_call(
      dataquieR::acc_margins(
        resp_vars = "missing label",
        group_vars = "GRP",
        threshold_type = "none"
      )
    ),
    "Could not find the following variables"
  )
  expect_identical(as.character(item_call$resp_vars), "missing label")

  expect_error(
    meta_env$provisionize_call(dataquieR::not_exported()),
    "not an exported function"
  )
})

test_that("repeated-measures cross-item metadata accessors work", {
  env <- dataquieR:::.meta_data_env
  old_target <- env$target_meta_data
  old_cross_item <- env$meta_data_cross_item
  withr::defer({
    env$target_meta_data <- old_target
    env$meta_data_cross_item <- old_cross_item
  })

  env$target_meta_data <- "cross-item_level"
  env$meta_data_cross_item <- data.frame(
    CHECK_ID = c("rm_bp", "other"),
    REPEATED_MEASURES_METRIC = c(" rmse | mean_absolute_difference ", ""),
    REPEATED_MEASURES_METRIC_SETTING = c(" rm_rmse_default | custom_mad ", ""),
    REPEATED_MEASURES_REFERENCE = c(" SBP_reference ", NA_character_),
    stringsAsFactors = FALSE
  )

  metric <- env$repeated_measures_metric("rm_bp")
  metric_setting <- env$repeated_measures_metric_setting("rm_bp")
  reference <- env$repeated_measures_reference("rm_bp")
  empty_metric <- env$repeated_measures_metric("other")
  empty_metric_setting <- env$repeated_measures_metric_setting("other")
  empty_reference <- env$repeated_measures_reference("other")

  expect_identical(
    unname(as.character(metric)),
    c("rmse", "mean_absolute_difference")
  )
  expect_identical(
    unname(as.character(metric_setting)),
    c("rm_rmse_default", "custom_mad")
  )
  expect_identical(unname(as.character(reference)), "SBP_reference")
  expect_identical(unname(as.character(empty_metric)), "")
  expect_identical(unname(as.character(empty_metric_setting)), "")
  expect_identical(unname(as.character(empty_reference)), "")
  expect_false(attr(metric, "explode"))
  expect_false(attr(metric_setting, "explode"))
  expect_false(attr(reference, "explode"))
  expect_false(attr(empty_metric, "explode"))
  expect_false(attr(empty_metric_setting, "explode"))
  expect_false(attr(empty_reference, "explode"))
})

test_that("cross-item outlier metadata accessors normalize settings", {
  env <- dataquieR:::.meta_data_env
  old_target <- env$target_meta_data
  old_cross_item <- env$meta_data_cross_item
  withr::defer({
    env$target_meta_data <- old_target
    env$meta_data_cross_item <- old_cross_item
  })

  env$target_meta_data <- "cross-item_level"
  env$meta_data_cross_item <- data.frame(
    CHECK_ID = c("ci_true", "ci_false", "ci_invalid", "ci_threshold"),
    CONTRADICTION_TERM = c("", "", "", ""),
    MULTIVARIATE_OUTLIER_CHECK = c("1", "0", "maybe", NA_character_),
    MAHALANOBIS_THRESHOLD = c("true", "2", "", "0.25"),
    stringsAsFactors = FALSE
  )

  withr::local_options(dataquieR.MULTIVARIATE_OUTLIER_CHECK = "false")

  expect_true(env$multivariate_outlier_check("ci_true"))
  expect_false(attr(env$multivariate_outlier_check("ci_true"), "explode"))
  expect_false(env$multivariate_outlier_check("ci_false"))
  expect_warning(
    expect_false(env$multivariate_outlier_check("ci_invalid")),
    "invalid entry"
  )

  expect_equal(
    unname(as.numeric(env$mahalanobis_threshold("ci_true"))),
    dataquieR.MAHALANOBIS_THRESHOLD_default
  )
  expect_false(attr(env$mahalanobis_threshold("ci_true"), "explode"))
  expect_warning(
    expect_equal(
      unname(as.numeric(env$mahalanobis_threshold("ci_false"))),
      dataquieR.MAHALANOBIS_THRESHOLD_default
    ),
    "invalid entry"
  )
  expect_equal(
    unname(as.numeric(env$mahalanobis_threshold("ci_threshold"))),
    0.25
  )
  expect_false(attr(env$mahalanobis_threshold("ci_threshold"), "explode"))

  env$meta_data_cross_item[[MAHALANOBIS_THRESHOLD]] <- NULL
  expect_identical(env$mahalanobis_threshold("ci_true"), "")
})

test_that("repeated-measurement settings is an expected metadata table", {
  verified <- .util_verify_names(
    observed_names = c(
      "item_level",
      "cross-item_level",
      "dataframe_level",
      "statistical_settings",
      "repeated_measurement_settings"
    )
  )

  expect_false("statistical_settings" %in% verified$dontknow)
  expect_false("statistical_settings" %in% verified$warn2)
  expect_false("repeated_measurement_settings" %in% verified$dontknow)
  expect_false("repeated_measurement_settings" %in% verified$warn2)
})

test_that("util_meta_data_env handles supported cross-item aliases explicitly", { # nolint: line_length_linter.
  alias_cross_item <- data.frame(
    CHECK_ID = "check-1",
    CHECK_LABEL = "Check 1",
    stringsAsFactors = FALSE
  )

  env <- util_meta_data_env(meta_data_cross = alias_cross_item)
  expect_equal(env$meta_data_cross_item, alias_cross_item)

  expect_error(
    util_meta_data_env(
      meta_data_cross_item = "cross-item_level",
      meta_data_cross = alias_cross_item
    ),
    "Please provide only one"
  )
})

test_that("util_meta_data_env reports explicit unresolved metadata data frames", { # nolint: line_length_linter.
  prep_purge_data_frame_cache()
  withr::defer(prep_purge_data_frame_cache())

  expect_error(
    util_meta_data_env(item_level = "missing_item_level"),
    "No data frame found for argument 'item_level': \"missing_item_level\"",
    fixed = TRUE
  )
  expect_error(
    util_meta_data_env(meta_data_segment = "missing_segment_level"),
    "No data frame found for argument 'meta_data_segment': \"missing_segment_level\"", # nolint: line_length_linter.
    fixed = TRUE
  )
  expect_error(
    util_meta_data_env(meta_data_cross_item = "missing_cross_item_level"),
    paste0(
      "No data frame found for argument 'meta_data_cross_item': ",
      "\"missing_cross_item_level\""
    ),
    fixed = TRUE
  )
  expect_error(
    util_meta_data_env(meta_data_cross = "missing_cross_item_level"),
    "No data frame found for argument 'meta_data_cross': \"missing_cross_item_level\"", # nolint: line_length_linter.
    fixed = TRUE
  )
  expect_error(
    util_meta_data_env(meta_data_dataframe = "missing_dataframe_level"),
    "No data frame found for argument 'meta_data_dataframe': \"missing_dataframe_level\"", # nolint: line_length_linter.
    fixed = TRUE
  )
  expect_error(
    util_meta_data_env(
      meta_data_item_computation = "missing_item_computation_level"
    ),
    paste0(
      "No data frame found for argument 'meta_data_item_computation': ",
      "\"missing_item_computation_level\""
    ),
    fixed = TRUE
  )

  expect_true(is.environment(util_meta_data_env()))
})

test_that("util_meta_data_env resolves item-computation metadata", {
  prep_purge_data_frame_cache()
  withr::defer(prep_purge_data_frame_cache())

  item_computation_level <- data.frame(
    VAR_NAMES = "sum_ab",
    COMPUTATION_RULE = "[a] + [b]",
    stringsAsFactors = FALSE
  )
  prep_add_data_frames(item_computation_level = item_computation_level)

  env <- util_meta_data_env()
  expect_equal(env$meta_data_item_computation, item_computation_level)

  env <- util_meta_data_env(
    meta_data_item_computation =
      item_computation_level
  )
  expect_equal(env$meta_data_item_computation, item_computation_level)

  env <- util_meta_data_env(item_computation_level = item_computation_level)
  expect_equal(env$meta_data_item_computation, item_computation_level)

  expect_error(
    util_meta_data_env(
      meta_data_item_computation = item_computation_level,
      item_computation_level = data.frame(VAR_NAMES = "other")
    ),
    "Please provide only one"
  )
})

test_that("decorator cross-item alias fallback avoids false conflicts", {
  alias_cross_item <- data.frame(
    CHECK_ID = "check-1",
    CHECK_LABEL = "Check 1",
    stringsAsFactors = FALSE
  )
  canonical_cross_item <- data.frame(
    CHECK_ID = "check-2",
    CHECK_LABEL = "Check 2",
    stringsAsFactors = FALSE
  )

  env_args <- formals(util_meta_data_env)
  env_args$meta_data_cross <- alias_cross_item

  resolved <- util_apply_meta_data_cross_fallback(
    env_args,
    what_i_have = "meta_data_cross"
  )
  expect_equal(resolved$meta_data_cross_item, alias_cross_item)
  expect_false("meta_data_cross" %in% names(resolved))

  env_args$meta_data_cross_item <- canonical_cross_item
  resolved <- util_apply_meta_data_cross_fallback(
    env_args,
    what_i_have = c("meta_data_cross_item", "meta_data_cross")
  )
  expect_equal(resolved$meta_data_cross_item, canonical_cross_item)
  expect_equal(resolved[["meta_data_cross"]], alias_cross_item)

  env_args <- formals(util_meta_data_env)
  resolved <- util_apply_meta_data_cross_fallback(
    env_args,
    what_i_have = character(0)
  )
  expect_identical(resolved, env_args)
})

test_that("decorator metadata alias helper maps explicit aliases", {
  dataframe_level <- data.frame(
    DF_NAME = "study_data",
    stringsAsFactors = FALSE
  )
  canonical_dataframe_level <- data.frame(
    DF_NAME = "other_data",
    stringsAsFactors = FALSE
  )
  cross_item_level <- data.frame(
    CHECK_ID = "check-1",
    CHECK_LABEL = "Check 1",
    stringsAsFactors = FALSE
  )

  env <- new.env(parent = emptyenv())
  env$dataframe_level <- dataframe_level
  env[["cross-item_level"]] <- cross_item_level

  # A call that explicitly uses a supported public alias should populate the
  # canonical metadata argument used by util_meta_data_env().
  env_args <- formals(util_meta_data_env)
  resolved <- util_apply_decorator_meta_data_alias(
    env_args,
    canonical = "meta_data_dataframe",
    aliases = "dataframe_level",
    call_arg_names = "dataframe_level",
    env = env
  )
  expect_equal(resolved$meta_data_dataframe, dataframe_level)
  expect_equal(resolved$.meta_data_dataframe_arg_name, "dataframe_level")

  # If the canonical argument was explicit as well, the helper must keep that
  # canonical value. Conflict handling happens later in util_meta_data_env().
  env_args$meta_data_dataframe <- canonical_dataframe_level
  resolved <- util_apply_decorator_meta_data_alias(
    env_args,
    canonical = "meta_data_dataframe",
    aliases = "dataframe_level",
    call_arg_names = c("meta_data_dataframe", "dataframe_level"),
    env = env
  )
  expect_equal(resolved$meta_data_dataframe, canonical_dataframe_level)
  expect_equal(resolved$.meta_data_dataframe_arg_name, "meta_data_dataframe")

  # The decorator also has to handle the supported non-syntactic sheet name
  # `cross-item_level`, because users can pass it via backticks.
  env_args <- formals(util_meta_data_env)
  resolved <- util_apply_decorator_meta_data_alias(
    env_args,
    canonical = "meta_data_cross_item",
    aliases = c("cross_item_level", "cross-item_level"),
    call_arg_names = "cross-item_level",
    env = env
  )
  expect_equal(resolved$meta_data_cross_item, cross_item_level)
  expect_equal(resolved$.meta_data_cross_item_arg_name, "cross-item_level")
})

test_that("decorator resolves label_col before input preparation", {
  item_level_with_label <- data.frame(
    VAR_NAMES = "v1",
    LABEL = "Variable 1",
    stringsAsFactors = FALSE
  )
  item_level_without_label <- data.frame(
    VAR_NAMES = "v1",
    stringsAsFactors = FALSE
  )

  expect_identical(
    util_resolve_decorator_label_col(
      label_col = quote(LABEL),
      what_i_have = character(0),
      item_level = item_level_with_label,
      env = emptyenv()
    ),
    LABEL
  )
  expect_identical(
    util_resolve_decorator_label_col(
      label_col = quote(LABEL),
      what_i_have = character(0),
      item_level = item_level_without_label,
      env = emptyenv()
    ),
    VAR_NAMES
  )
  expect_identical(
    util_resolve_decorator_label_col(
      label_col = "custom_label",
      what_i_have = "label_col",
      item_level = item_level_with_label,
      env = emptyenv()
    ),
    "custom_label"
  )

  caller_env <- new.env(parent = emptyenv())
  caller_env$label_col <- "caller_label"
  expect_identical(
    util_resolve_decorator_label_col(
      label_col = quote(LABEL),
      what_i_have = character(0),
      item_level = item_level_with_label,
      env = caller_env
    ),
    "caller_label"
  )
})

test_that("decorator maps meta_data alias before metadata env diagnostics", {
  withr::local_options(list(dataquieR.test_decorator = TRUE))
  prep_purge_data_frame_cache()
  withr::defer(prep_purge_data_frame_cache())

  study_data <- data.frame(
    SBP_0 = c(120, 130, 140, 150, 160, 170),
    GROUP_0 = rep(c("a", "b"), each = 3),
    stringsAsFactors = FALSE
  )
  x <- data.frame(
    VAR_NAMES = c("SBP_0", "GROUP_0"),
    LABEL = c("SBP_0", "GROUP_0"),
    SCALE_LEVEL = c(SCALE_LEVELS$INTERVAL, SCALE_LEVELS$NOMINAL),
    stringsAsFactors = FALSE
  )
  prep_add_data_frames(x = x)

  expect_error(
    acc_margins("SBP_0", study_data = "xx"),
    "No data frame found for argument 'study_data': \"xx\"",
    fixed = TRUE
  )
  expect_error(
    acc_margins("SBP_0", meta_data = "missing_meta_data"),
    "No data frame found for argument 'meta_data': \"missing_meta_data\"",
    fixed = TRUE
  )
  result <- try(
    acc_margins("SBP_0",
      meta_data = "x",
      group_vars = "GROUP_0",
      min_obs_in_subgroup = 1,
      min_obs_in_cat = 1
    ),
    silent = TRUE
  )
  if (util_is_try_error(result)) {
    expect_false(grepl(
      "No data frame found",
      conditionMessage(util_attr(result, "condition", exact = TRUE)),
      fixed = TRUE
    ))
  }
  result <- try(
    acc_margins("SBP_0",
      item_level = "x",
      group_vars = "GROUP_0",
      min_obs_in_subgroup = 1,
      min_obs_in_cat = 1
    ),
    silent = TRUE
  )
  if (util_is_try_error(result)) {
    expect_false(grepl(
      "You cannot provide both",
      conditionMessage(util_attr(result, "condition", exact = TRUE)),
      fixed = TRUE
    ))
    expect_false(grepl(
      "Please provide only one of",
      conditionMessage(util_attr(result, "condition", exact = TRUE)),
      fixed = TRUE
    ))
  }
})

test_that("decorator accepts cached metadata through either item-level alias", {
  skip_on_cran()
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")

  withr::local_options(list(dataquieR.test_decorator = TRUE))
  prep_purge_data_frame_cache()
  withr::defer(prep_purge_data_frame_cache())

  study_data <- prep_get_data_frame(
    "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData", # nolint: line_length_linter.
    keep_types = TRUE
  )
  x <- prep_get_data_frame(
    "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx|item_level" # nolint: line_length_linter.
  )
  prep_add_data_frames(x = x)

  for (metadata_arg in c("meta_data", "item_level")) {
    call_args <- list(
      resp_vars = "SBP_0",
      group_vars = "USR_BP_0",
      study_data = study_data,
      min_obs_in_subgroup = 1,
      min_obs_in_cat = 1
    )
    call_args[[metadata_arg]] <- "x"
    result <- try(suppressWarnings(do.call(acc_margins, call_args)),
      silent = TRUE
    )
    expect_false(util_is_try_error(result))
  }
})

test_that("decorator rejects explicit item-level alias conflicts", {
  withr::local_options(list(dataquieR.test_decorator = TRUE))

  study_data <- data.frame(
    SBP_0 = c(120, 130, 140, 150, 160, 170),
    GROUP_0 = rep(c("a", "b"), each = 3),
    stringsAsFactors = FALSE
  )
  item_level <- data.frame(
    VAR_NAMES = c("SBP_0", "GROUP_0"),
    LABEL = c("SBP_0", "GROUP_0"),
    SCALE_LEVEL = c(SCALE_LEVELS$INTERVAL, SCALE_LEVELS$NOMINAL),
    stringsAsFactors = FALSE
  )

  expect_error(
    acc_margins("SBP_0",
      group_vars = "GROUP_0",
      study_data = study_data,
      item_level = item_level,
      meta_data = item_level[1, , drop = FALSE],
      min_obs_in_subgroup = 1,
      min_obs_in_cat = 1
    ),
    "Please provide only one"
  )
})

test_that("decorator keeps missing meta_data_v2 as the primary error", {
  withr::local_options(list(dataquieR.test_decorator = TRUE))
  prep_purge_data_frame_cache()
  withr::defer(prep_purge_data_frame_cache())

  result <- try(
    acc_margins("SBP_0", meta_data_v2 = missing_meta_data_v2),
    silent = TRUE
  )
  expect_true(util_is_try_error(result))
  result_message <- conditionMessage(util_condition_from_try_error(result))
  expect_match(
    result_message,
    "Could not evaluate argument 'meta_data_v2'",
    fixed = TRUE
  )
  expect_match(
    result_message,
    "object 'missing_meta_data_v2' not found",
    fixed = TRUE
  )
  expect_false(grepl("Argument file is NULL", result_message, fixed = TRUE))
})

test_that("decorator reports explicit metadata level aliases in diagnostics", {
  withr::local_options(list(dataquieR.test_decorator = TRUE))
  prep_purge_data_frame_cache()
  withr::defer(prep_purge_data_frame_cache())

  study_data <- data.frame(
    a = c(1, 2, 3),
    b = c(2, 3, 4),
    stringsAsFactors = FALSE
  )
  item_level <- prep_create_meta(
    VAR_NAMES = c("a", "b"),
    DATA_TYPE = DATA_TYPES$INTEGER,
    LABEL = c("A", "B"),
    MISSING_LIST = ""
  )
  prep_add_data_frames(item_level = item_level, study_data = study_data)

  expect_error(
    int_all_datastructure_dataframe(
      dataframe_level = "missing_dataframe_level"
    ),
    "No data frame found for argument 'dataframe_level': \"missing_dataframe_level\"", # nolint: line_length_linter.
    fixed = TRUE
  )
  expect_error(
    int_all_datastructure_segment(
      study_data = study_data,
      item_level = item_level,
      segment_level = "missing_segment_level"
    ),
    "No data frame found for argument 'segment_level': \"missing_segment_level\"", # nolint: line_length_linter.
    fixed = TRUE
  )
  expect_error(
    con_contradictions_redcap(
      study_data = study_data,
      item_level = item_level,
      cross_item_level = "missing_cross_item"
    ),
    "No data frame found for argument 'cross_item_level': \"missing_cross_item\"", # nolint: line_length_linter.
    fixed = TRUE
  )
  expect_error(
    con_contradictions_redcap(
      study_data = study_data,
      item_level = item_level,
      `cross-item_level` = "missing_cross_item"
    ),
    "No data frame found for argument 'cross-item_level': \"missing_cross_item\"", # nolint: line_length_linter.
    fixed = TRUE
  )
})

test_that("decorated calls accept supported dataframe and segment aliases", {
  withr::local_options(list(dataquieR.test_decorator = TRUE))
  prep_purge_data_frame_cache()
  withr::defer(prep_purge_data_frame_cache())

  study_data <- data.frame(
    id = 1:3,
    txt = c("a", "b", "c"),
    stringsAsFactors = FALSE
  )
  item_level <- data.frame(
    VAR_NAMES = c("id", "txt"),
    LABEL = c("id", "txt"),
    DATA_TYPE = c("integer", "text"),
    SCALE_LEVEL = c("nominal", "nominal"),
    ENCODING = c("UTF-8", "UTF-8"),
    MISSING_LIST_TABLE = c("", ""),
    MISSING_LIST = c("", ""),
    JUMP_LIST = c("", ""),
    stringsAsFactors = FALSE
  )
  dataframe_level <- data.frame(
    DF_NAME = "study_data",
    DF_CODE = "study_data",
    stringsAsFactors = FALSE
  )
  prep_add_data_frames(df_alias = dataframe_level)

  canonical_dataframe <- suppressWarnings(suppressMessages(
    int_encoding_errors(
      resp_vars = "txt",
      study_data = study_data,
      item_level = item_level,
      label_col = LABEL,
      meta_data_dataframe = dataframe_level,
      ref_encs = c(txt = "UTF-8")
    )
  ))
  alias_dataframe <- suppressWarnings(suppressMessages(
    int_encoding_errors(
      resp_vars = "txt",
      study_data = study_data,
      item_level = item_level,
      label_col = LABEL,
      dataframe_level = "df_alias",
      ref_encs = c(txt = "UTF-8")
    )
  ))
  expect_equal(
    alias_dataframe$SummaryTable,
    canonical_dataframe$SummaryTable
  )

  prep_purge_data_frame_cache()
  study_data <- data.frame(
    id = 1:3,
    part1 = c(1L, 1L, 0L),
    part2 = c(1L, 0L, 1L),
    x = c(1, NA, 3),
    y = c(NA, 2, 3)
  )
  item_level <- data.frame(
    VAR_NAMES = c("id", "part1", "part2", "x", "y"),
    LABEL = c("id", "part1", "part2", "x", "y"),
    DATA_TYPE = rep("integer", 5),
    SCALE_LEVEL = c("nominal", "nominal", "nominal", "metric", "metric"),
    MISSING_LIST_TABLE = rep("", 5),
    MISSING_LIST = rep("", 5),
    JUMP_LIST = rep("", 5),
    STUDY_SEGMENT = c("S1", "S1", "S2", "S1", "S2"),
    PART_VAR = c("", "", "", "part1", "part2"),
    stringsAsFactors = FALSE
  )
  segment_level <- data.frame(
    STUDY_SEGMENT = c("S1", "S2"),
    SEGMENT_MISS = NA_character_,
    SEGMENT_PART_VARS = c("part1", "part2"),
    stringsAsFactors = FALSE
  )
  prep_add_data_frames(seg_alias = segment_level)

  canonical_segment <- suppressWarnings(suppressMessages(
    com_segment_missingness(
      study_data = study_data,
      item_level = item_level,
      label_col = LABEL,
      meta_data_segment = segment_level,
      threshold_value = 100,
      expected_observations = "SEGMENT"
    )
  ))
  alias_segment <- suppressWarnings(suppressMessages(
    com_segment_missingness(
      study_data = study_data,
      item_level = item_level,
      label_col = LABEL,
      segment_level = "seg_alias",
      threshold_value = 100,
      expected_observations = "SEGMENT"
    )
  ))
  expect_equal(alias_segment$ReportSummaryTable,
    canonical_segment$ReportSummaryTable,
    ignore_attr = TRUE
  )
})

test_that("metadata level aliases are synchronized without false conflicts", {
  collect_aliases <- function(meta_data_dataframe = "dataframe_level",
    dataframe_level,
    meta_data_cross_item = "cross-item_level",
    cross_item_level,
    `cross-item_level`,
    meta_data_item_computation =
      "item_computation_level",
    item_computation_level) {
    util_ck_arg_aliases()
    out <- list()
    for (nm in c(
      "meta_data_dataframe", "dataframe_level",
      "meta_data_cross_item", "cross_item_level",
      "cross-item_level", "meta_data_item_computation",
      "item_computation_level"
    )) {
      if (!eval(call("missing", as.symbol(nm)))) {
        out[[nm]] <- get(nm)
      }
    }
    out
  }

  dataframe_level <- data.frame(DF_NAME = character(0))
  cross_item_level <- data.frame(
    CHECK_ID = "check-1",
    stringsAsFactors = FALSE
  )
  item_computation_level <- data.frame(
    VAR_NAMES = "computed_1",
    stringsAsFactors = FALSE
  )

  res <- collect_aliases(dataframe_level = dataframe_level)
  expect_equal(res$meta_data_dataframe, dataframe_level)
  expect_equal(res$dataframe_level, dataframe_level)

  res <- collect_aliases(meta_data_dataframe = dataframe_level)
  expect_equal(res$meta_data_dataframe, dataframe_level)
  expect_equal(res$dataframe_level, dataframe_level)

  expect_error(
    collect_aliases(
      dataframe_level = dataframe_level,
      meta_data_dataframe = data.frame(DF_NAME = "other")
    ),
    "You cannot provide both"
  )

  res <- collect_aliases(cross_item_level = cross_item_level)
  expect_equal(res$meta_data_cross_item, cross_item_level)
  expect_equal(res$cross_item_level, cross_item_level)
  expect_equal(res$`cross-item_level`, cross_item_level)

  res <- collect_aliases(meta_data_cross_item = cross_item_level)
  expect_equal(res$meta_data_cross_item, cross_item_level)
  expect_equal(res$cross_item_level, cross_item_level)
  expect_equal(res$`cross-item_level`, cross_item_level)

  expect_error(
    collect_aliases(
      cross_item_level = cross_item_level,
      meta_data_cross_item = data.frame(CHECK_ID = "other")
    ),
    "You cannot provide both"
  )

  res <- collect_aliases(item_computation_level = item_computation_level)
  expect_equal(res$meta_data_item_computation, item_computation_level)
  expect_equal(res$item_computation_level, item_computation_level)

  res <- collect_aliases(meta_data_item_computation = item_computation_level)
  expect_equal(res$meta_data_item_computation, item_computation_level)
  expect_equal(res$item_computation_level, item_computation_level)

  expect_error(
    collect_aliases(
      item_computation_level = item_computation_level,
      meta_data_item_computation = data.frame(VAR_NAMES = "other")
    ),
    "You cannot provide both"
  )
})

test_that("metadata level aliases are mirrored in the data frame cache", {
  prep_purge_data_frame_cache()
  withr::defer(prep_purge_data_frame_cache())

  alias_cross_item <- data.frame(
    CHECK_ID = "check-1",
    CHECK_LABEL = "Check 1",
    stringsAsFactors = FALSE
  )
  prep_add_data_frames(cross_item_level = alias_cross_item)

  # The cache can be populated through dq_report*, explicit aliases, or the
  # decorator. util_ck_arg_aliases() keeps the supported underscore and
  # hyphenated spellings addressable under the same metadata table.
  mirror_alias_in_cache <- function(cross_item_level) {
    util_ck_arg_aliases()
  }
  mirror_alias_in_cache(cross_item_level = alias_cross_item)

  expect_equal(
    prep_get_data_frame("cross-item_level"),
    alias_cross_item
  )
})
