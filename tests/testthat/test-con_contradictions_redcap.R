skip_on_cran()

redcap_test_fixture <- local({
  fixture <- NULL

  function() {
    if (is.null(fixture)) {
      meta_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data.RData") # nolint: line_length_linter.
      study_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData", keep_types = TRUE) # nolint: line_length_linter.
      meta_data2 <- prep_scalelevel_from_data_and_metadata(
        study_data = study_data,
        meta_data = meta_data
      )
      meta_data[[SCALE_LEVEL]] <- setNames(
        meta_data2[[SCALE_LEVEL]],
        nm = meta_data2[[VAR_NAMES]]
      )[meta_data[[VAR_NAMES]]]
      fixture <- list(
        meta_data = meta_data,
        study_data = study_data,
        meta_data_cross_item = prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx|cross-item_level") # nolint: line_length_linter.
      )
    }

    fixture
  }
})

redcap_ship_fixture <- local({
  fixture <- NULL

  function() {
    if (is.null(fixture)) {
      prep_load_workbook_like_file("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/ship_meta_v2.xlsx") # nolint: line_length_linter.
      fixture <- list(
        study_data = prep_get_data_frame(
          "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/ship.RDS", # nolint: line_length_linter.
          keep_types = TRUE
        ),
        meta_data = prep_get_data_frame("item_level"),
        meta_data_cross_item = prep_get_data_frame("cross-item_level")
      )
    }

    fixture
  }
})

test_that("con_contradictions_redcap reports missing cross-item metadata clearly", { # nolint: line_length_linter.
  prep_purge_data_frame_cache()
  withr::defer(prep_purge_data_frame_cache())

  study_data <- data.frame(v1 = 1)
  meta_data <- data.frame(
    VAR_NAMES = "v1",
    LABEL = "Variable 1",
    DATA_TYPE = "integer",
    MISSING_LIST = "|",
    JUMP_LIST = "|",
    stringsAsFactors = FALSE
  )

  expect_error(
    con_contradictions_redcap(
      study_data = study_data,
      meta_data = meta_data,
      label_col = LABEL,
      threshold_value = 0,
      meta_data_cross_item = "cross-item_level"
    ),
    "No data frame found for argument 'meta_data_cross_item': \"cross-item_level\"", # nolint: line_length_linter.
    fixed = TRUE
  )

  expect_error(
    con_contradictions_redcap(
      study_data = study_data,
      meta_data = meta_data,
      label_col = LABEL,
      threshold_value = 0,
      meta_data_cross_item = data.frame()
    ),
    "No cross-item-level metadata given",
    fixed = TRUE
  )
})

test_that("con_contradictions_redcap rejects deprecated value-label argument", {
  skip_on_cran()

  expect_error(
    con_contradictions_redcap(use_value_labels = TRUE),
    "use_value_labels.*con_contradictions_redcap"
  )
})

test_that("one failed contradiction check keeps valid bars visible", {
  skip_on_cran()

  study_data <- data.frame(
    v0 = c(10, 20, 30),
    v1 = c(9, 25, 20)
  )
  meta_data <- data.frame(
    VAR_NAMES = c("v0", "v1"),
    LABEL = c("AGE_0", "AGE_1"),
    DATA_TYPE = "integer",
    SCALE_LEVEL = "ratio",
    VALUE_LABELS = "",
    MISSING_LIST_TABLE = "",
    stringsAsFactors = FALSE
  )
  meta_data_cross_item <- data.frame(
    CHECK_LABEL = c("Valid", "Invalid"),
    CONTRADICTION_TERM = c("[AGE_1] < [AGE_0]", "[MISSING] > 0"),
    CONTRADICTION_TYPE = "LOGICAL",
    DATA_PREPARATION = "",
    stringsAsFactors = FALSE
  )

  result <- suppressWarnings(suppressMessages(
    con_contradictions_redcap(
      study_data = study_data,
      meta_data = meta_data,
      label_col = LABEL,
      meta_data_cross_item = meta_data_cross_item,
      threshold_value = NA_real_
    )
  ))
  layers <- util_gg_get(result$SummaryPlot, "layers")
  bar_layer <- which(vapply(layers, function(layer) {
    inherits(util_gg_get(layer, "geom"), "GeomBar")
  }, FUN.VALUE = logical(1)))
  built <- ggplot2::ggplot_build(result$SummaryPlot)
  bar_values <- built$data[[bar_layer]][["y"]]

  expect_equal(result$VariableGroupTable$PCT_con_con, c(66.67, NA_real_))
  expect_equal(bar_values, c(NA_real_, 66.67))
})

test_that("contradictions without a type retain a renderable summary plot", {
  study_data <- data.frame(
    v0 = c(10, 20, 30),
    v1 = c(9, 25, 20)
  )
  meta_data <- data.frame(
    VAR_NAMES = c("v0", "v1"),
    LABEL = c("AGE_0", "AGE_1"),
    DATA_TYPE = "integer",
    SCALE_LEVEL = "ratio",
    VALUE_LABELS = "",
    MISSING_LIST_TABLE = "",
    stringsAsFactors = FALSE
  )
  meta_data_cross_item <- data.frame(
    CHECK_LABEL = "Age contradiction",
    CONTRADICTION_TERM = "[AGE_1] < [AGE_0]",
    DATA_PREPARATION = "",
    stringsAsFactors = FALSE
  )

  result <- suppressWarnings(suppressMessages(
    con_contradictions_redcap(
      study_data = study_data,
      meta_data = meta_data,
      label_col = LABEL,
      meta_data_cross_item = meta_data_cross_item
    )
  ))

  expect_equal(result$VariableGroupTable$PCT_con_con, 66.67)
  expect_true(is.na(result$VariableGroupTable[[CONTRADICTION_TYPE]]))
  expect_false(any(c(
    "PCT_con_con_contc", "PCT_con_con_contu",
    "NUM_con_con_contc", "NUM_con_con_contu"
  ) %in% colnames(result$VariableGroupTable)))
  expect_no_error(ggplot2::ggplot_build(result$SummaryPlot))

  if (requireNamespace("plotly", quietly = TRUE)) {
    expect_no_error(util_as_plotly_con_contradictions_redcap(result))
  }
})

test_that("contradiction type band labels adapt to short runs", {
  one_row <- util_con_contradiction_type_bands(data.frame(
    CONTRADICTION_TYPE = "LOGICAL"
  ))
  two_rows <- util_con_contradiction_type_bands(data.frame(
    CONTRADICTION_TYPE = c("EMPIRICAL", "EMPIRICAL")
  ))
  few_rows <- util_con_contradiction_type_bands(data.frame(
    CONTRADICTION_TYPE = rep("LOGICAL", 4)
  ))
  many_rows <- util_con_contradiction_type_bands(data.frame(
    CONTRADICTION_TYPE = rep("EMPIRICAL", 8)
  ))

  expect_equal(one_row$display_label, "Log.")
  expect_equal(two_rows$display_label, "Emp.")
  expect_equal(few_rows$display_label, "Logical")
  expect_equal(many_rows$display_label, "Empirical contradictions")
  expect_equal(few_rows$display_label_full, "Logical contradictions")
})

test_that("con_contradictions_redcap works", {
  skip_on_cran() # slow, redcap parser is tested anyway, errors in plots obvious

  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  withr::local_options(
    dataquieR.CONDITIONS_WITH_STACKTRACE = TRUE,
    dataquieR.ERRORS_WITH_CALLER = TRUE,
    dataquieR.WARNINGS_WITH_CALLER = TRUE,
    dataquieR.MESSAGES_WITH_CALLER = TRUE
  )
  require_english_locale_and_berlin_tz()
  fixture <- redcap_test_fixture()
  meta_data <- fixture$meta_data
  study_data <- fixture$study_data
  meta_data_cross_item <- fixture$meta_data_cross_item
  # ignore here 'special' data preparation steps, will be checked separately
  meta_data_cross_item$DATA_PREPARATION <- ""
  label_col <- "LABEL"
  threshold_value <- 1
  # expect_message2(
  expect_silent({
    default <- con_contradictions_redcap(
      study_data = study_data, meta_data = meta_data, label_col = label_col,
      threshold_value = threshold_value, meta_data_cross_item = meta_data_cross_item # nolint: line_length_linter.
    )
    off <- con_contradictions_redcap(
      study_data = study_data, meta_data = meta_data, label_col = label_col,
      threshold_value = threshold_value, meta_data_cross_item = meta_data_cross_item, # nolint: line_length_linter.
      summarize_categories = FALSE
    )
    on <- con_contradictions_redcap(
      study_data = study_data, meta_data = meta_data, label_col = label_col,
      threshold_value = threshold_value, meta_data_cross_item = meta_data_cross_item, # nolint: line_length_linter.
      summarize_categories = TRUE
    )
  })
  # )
  expect_equal(off$FlaggedStudyData, on$Other$all_checks$FlaggedStudyData)
  expect_equal(off$VariableGroupTable, on$Other$all_checks$VariableGroupTable)

  skip_on_cran()
  skip_if_not_installed("vdiffr")
  expect_doppelganger2(
    "summary contra_r pl1 ok",
    on$Other$all_checks$SummaryPlot
  )
  expect_doppelganger2(
    "summary contra_r pl2 ok",
    default$SummaryPlot
  )
  expect_doppelganger2(
    "summary contra_r pl3 ok",
    off$SummaryPlot
  )
  expect_doppelganger2(
    "one cat contra_r pl4 ok",
    on$Other$EMPIRICAL$SummaryPlot
  )
})

test_that("con_contradictions_redcap works with tiny inputs", {
  skip_on_cran() # slow, redcap parser is tested anyway, errors in plots obvious

  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  withr::local_options(
    dataquieR.CONDITIONS_WITH_STACKTRACE = TRUE,
    dataquieR.ERRORS_WITH_CALLER = TRUE,
    dataquieR.WARNINGS_WITH_CALLER = TRUE,
    dataquieR.MESSAGES_WITH_CALLER = TRUE
  )
  # catch if some objects will be reduced to scalars or vectors instead of dataframes or matrices # nolint: line_length_linter.
  require_english_locale_and_berlin_tz()
  fixture <- redcap_test_fixture()
  meta_data <- fixture$meta_data
  study_data <- fixture$study_data
  threshold_value <- 1

  meta_data_cross_item <- data.frame(
    "CONTRADICTION_TERM" = "[AGE_0] < 18",
    "CHECK_LABEL" = "age hard limits check for testing",
    "CONTRADICTION_TYPE" = "LOGICAL"
  )
  meta_data_cross_item2 <- data.frame(
    "CONTRADICTION_TERM" = "[v00003] < 18",
    "CHECK_LABEL" = "age hard limits check for testing",
    "CONTRADICTION_TYPE" = "LOGICAL"
  )
  meta_data_cross_item3 <- data.frame(
    "CONTRADICTION_TERM" = c("[v00003] < 18", "[v00003] > 130"),
    "CHECK_LABEL" = c("age hard limits check for testing", "age hard limits check for testing no. 2"), # nolint: line_length_linter.
    "CONTRADICTION_TYPE" = c("LOGICAL", "EMPIRICAL")
  )
  tiny_sd <- study_data[, which(colnames(study_data) == meta_data$VAR_NAMES[which(meta_data$LABEL == "AGE_0")]), drop = FALSE] # nolint: line_length_linter.
  tiny_md <- meta_data[which(meta_data$LABEL == "AGE_0"), , drop = FALSE]
  tiny_md[[JUMP_LIST]][util_empty(tiny_md[[JUMP_LIST]])] <- SPLIT_CHAR
  tiny_md[[MISSING_LIST]][util_empty(tiny_md[[MISSING_LIST]])] <- SPLIT_CHAR
  # expect_message2(
  expect_message2({
    check1 <- con_contradictions_redcap( # only one contradiction check -> nrow(meta_data_cross_item) is 1 # nolint: line_length_linter.
      study_data = tiny_sd, meta_data = tiny_md, label_col = "LABEL", # using VAR_NAMES and LABELs => two "needles" to generate the variable list # nolint: line_length_linter.
      threshold_value = threshold_value, meta_data_cross_item = meta_data_cross_item # nolint: line_length_linter.
    )
    check2 <- con_contradictions_redcap(
      study_data = tiny_sd, meta_data = tiny_md, label_col = "VAR_NAMES", # only one "needle" to generate the variable list # nolint: line_length_linter.
      threshold_value = threshold_value, meta_data_cross_item = meta_data_cross_item2 # nolint: line_length_linter.
    ) # only one contradiction check -> nrow(meta_data_cross_item2) is 1
    check3 <- con_contradictions_redcap(
      study_data = tiny_sd, meta_data = tiny_md, label_col = "VAR_NAMES", # only one "needle" to generate the variable list # nolint: line_length_linter.
      threshold_value = threshold_value, meta_data_cross_item = meta_data_cross_item3 # nolint: line_length_linter.
    ) # nrow(meta_data_cross_item3) is 2
  })
  # )
})

test_that("con_contradictions_redcap uses DATA_PREPARATION correctly", {
  skip_on_cran()
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  fixture <- redcap_test_fixture()
  meta_data <- fixture$meta_data
  study_data <- fixture$study_data
  meta_data_cross_item <- fixture$meta_data_cross_item

  # If nothing is specified in DATA_PREPARATION, missing value labels and values
  # outside hard limits should be replaced by NA.
  mdci <- meta_data_cross_item[1, ]
  res1 <- con_contradictions_redcap(
    study_data = study_data,
    meta_data = meta_data,
    label_col = LABEL,
    threshold_value = 1,
    meta_data_cross_item = mdci
  )
  mdci$DATA_PREPARATION <- "LIMITS | MISSING_NA"
  res2 <- con_contradictions_redcap(
    study_data = study_data,
    meta_data = meta_data,
    label_col = LABEL,
    threshold_value = 1,
    meta_data_cross_item = mdci
  )
  expect_equal(res1$FlaggedStudyData, res2$FlaggedStudyData)

  # check option MISSING_INTERPRET
  mdci$CHECK_LABEL <- "Inconsistent reason for missingness in number of children" # nolint: line_length_linter.
  mdci$CONTRADICTION_TERM <- "[N_BIRTH_0] > 0 and ([N_CHILD_0] in set('NE', 'P', 'NC'))" # nolint: line_length_linter.
  mdci$CONTRADICTION_TYPE <- "EMPIRICAL"
  mdci$DATA_PREPARATION <- "MISSING_INTERPRET"
  # We would expect these observations to be picked up:
  miss_tab <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx|missing_table") # nolint: line_length_linter.
  sel_miss_codes <- miss_tab$CODE_VALUE[which(miss_tab$CODE_INTERPRET %in%
        c("NE", "NC", "P"))]
  check_res <- sum(table(study_data[
    which(study_data$v00021 %in% sel_miss_codes &
        study_data$v00027 > 0 &
        study_data$v00027 < 8000),
    c("v00021", "v00027")
  ]))
  # NE = 99981, NC = 99983, P = 99988
  # Numerical variables are not yet supported here.
  expect_warning(
    {
      res3 <- con_contradictions_redcap(
        study_data = study_data,
        meta_data = meta_data,
        label_col = LABEL,
        threshold_value = 1,
        meta_data_cross_item = mdci
      )
    },
    regexp = "not yet supported for numerical variables"
  )

  # check that the function catches problems with variables on smoking
  # (replacing missing value codes by NA not specified, but should be done;
  # inadmissible categorical values on top;
  # 91 observations match the contradiction rule)
  mdci <- meta_data_cross_item[
    grepl(
      "Non-smokers inconsistency",
      meta_data_cross_item$CHECK_LABEL
    ), ,
    drop = FALSE
  ]
  expect_message2(
    {
      res4 <- con_contradictions_redcap(
        study_data = study_data,
        meta_data = meta_data,
        label_col = LABEL,
        threshold_value = 1,
        meta_data_cross_item = mdci
      )
    },
    regexp = sprintf(
      "(%s|%s)",
      "replace the missing codes by NA, too",
      "Number of levels in variable greater than in character string"
    )
  )
  expect_equal(res4$VariableGroupTable$NUM_con_con, 91)
})

test_that("con_contradictions_redcap keeps raw codes for empty DATA_PREPARATION", { # nolint: line_length_linter.
  withr::local_options(dataquieR.precomputeStudyData = FALSE)

  study_data <- data.frame(x = c(1, -99, 2))
  meta_data <- data.frame(
    VAR_NAMES = "x",
    LABEL = "x",
    DATA_TYPE = "integer",
    SCALE_LEVEL = "metric",
    VARIABLE_ROLE = "primary",
    MISSING_LIST = "missing = -99",
    JUMP_LIST = "",
    VALUE_LABELS = NA_character_,
    VALUE_LABEL_TABLE = NA_character_,
    HARD_LIMITS = "",
    stringsAsFactors = FALSE
  )
  meta_data_cross_item <- data.frame(
    VARIABLE_LIST = "x",
    CHECK_LABEL = "raw missing code is visible",
    CONTRADICTION_TERM = "[x] < 0",
    CONTRADICTION_TYPE = "LOGICAL",
    DATA_PREPARATION = "|",
    stringsAsFactors = FALSE
  )

  direct <- suppressWarnings(suppressMessages(con_contradictions_redcap(
    study_data = study_data,
    meta_data = meta_data,
    label_col = LABEL,
    meta_data_cross_item = meta_data_cross_item,
    threshold_value = 1
  )))

  report <- suppressWarnings(suppressMessages(dq_report2(
    study_data = study_data,
    meta_data = meta_data,
    meta_data_cross_item = meta_data_cross_item,
    dimensions = "Consistency",
    filter_indicator_functions = "con_contradictions_redcap",
    cores = NULL,
    mode = "parallel"
  )))
  report_result_name <- grep("con_contradictions_redcap", names(report),
    value = TRUE
  )

  expect_equal(unclass(direct)[["VariableGroupTable"]][["NUM_con_con"]], 1)
  expect_equal(
    unclass(report[[report_result_name]])[["VariableGroupTable"]][[
      "NUM_con_con"
    ]],
    1
  )
  expect_equal(
    unclass(direct)[["VariableGroupTable"]][["DATA_PREPARATION"]],
    unclass(report[[report_result_name]])[["VariableGroupTable"]][[
      "DATA_PREPARATION"
    ]]
  )

  withr::local_options(list(dataquieR.test_decorator = TRUE))
  underscore_alias <- suppressWarnings(suppressMessages(
    con_contradictions_redcap(
      study_data = study_data,
      meta_data = meta_data,
      label_col = LABEL,
      cross_item_level = meta_data_cross_item,
      threshold_value = 1
    )
  ))
  hyphen_alias <- suppressWarnings(suppressMessages(
    con_contradictions_redcap(
      study_data = study_data,
      meta_data = meta_data,
      label_col = LABEL,
      `cross-item_level` = meta_data_cross_item,
      threshold_value = 1
    )
  ))

  expect_equal(
    unclass(underscore_alias)[["VariableGroupTable"]][["NUM_con_con"]],
    unclass(direct)[["VariableGroupTable"]][["NUM_con_con"]]
  )
  expect_equal(
    unclass(hyphen_alias)[["VariableGroupTable"]][["NUM_con_con"]],
    unclass(direct)[["VariableGroupTable"]][["NUM_con_con"]]
  )
})

test_that("dq_report2 filters cross-item rules after pattern expansion", {
  withr::local_options(dataquieR.precomputeStudyData = FALSE)

  study_data <- data.frame(
    sbp1 = c(120, 80, 130),
    dbp1 = c(80, 90, 85),
    height = c(170, 165, 180),
    unrelated = c(1, 2, 3)
  )
  meta_data <- data.frame(
    VAR_NAMES = names(study_data),
    LABEL = names(study_data),
    DATA_TYPE = c("integer", "integer", "integer", "integer"),
    SCALE_LEVEL = c("metric", "metric", "metric", "metric"),
    VARIABLE_ROLE = c("primary", "primary", "primary", "primary"),
    stringsAsFactors = FALSE
  )
  meta_data_cross_item <- data.frame(
    CHECK_LABEL = c(
      "selected variables from token",
      "unrelated variable from token"
    ),
    VARIABLE_LIST = c("[ALL]", "unrelated"),
    CONTRADICTION_TERM = c("[sbp1] < [dbp1]", "[unrelated] > 1"),
    CONTRADICTION_TYPE = "LOGICAL",
    DATA_PREPARATION = "LABEL",
    stringsAsFactors = FALSE
  )

  report <- suppressWarnings(suppressMessages(dq_report2(
    resp_vars = c("sbp1", "dbp1", "height"),
    study_data = study_data,
    meta_data = meta_data,
    meta_data_cross_item = meta_data_cross_item,
    filter_indicator_functions = "con_contradictions_redcap",
    advanced_options = list(dataquieR.dt_adjust = FALSE),
    cores = NULL,
    mode = "parallel"
  )))
  result_name <- grep("con_contradictions_redcap", names(report), value = TRUE)
  result <- unclass(report[[result_name]])

  expect_length(attr(report[[result_name]], "error"), 0)
  expect_equal(
    result[["VariableGroupTable"]][[CHECK_LABEL]],
    "selected variables from token"
  )
  expect_equal(result[["VariableGroupTable"]][["NUM_con_con"]], 1)
})

test_that("con_contradictions_redcap marks percentage summary data numeric", {
  withr::local_options(dataquieR.precomputeStudyData = FALSE)
  prep_purge_data_frame_cache()
  withr::defer(prep_purge_data_frame_cache())

  study_data <- data.frame(x = c(1L, 2L, 3L), y = c(1L, 1L, 3L))
  meta_data <- data.frame(
    VAR_NAMES = c("x", "y"),
    LABEL = c("x", "y"),
    DATA_TYPE = DATA_TYPES$INTEGER,
    SCALE_LEVEL = "metric",
    VARIABLE_ROLE = "primary",
    MISSING_LIST = "",
    JUMP_LIST = "",
    VALUE_LABELS = NA_character_,
    VALUE_LABEL_TABLE = NA_character_,
    HARD_LIMITS = "",
    stringsAsFactors = FALSE
  )
  meta_data_cross_item <- data.frame(
    VARIABLE_LIST = "x|y",
    CHECK_LABEL = "x greater than y",
    CONTRADICTION_TERM = "[x] > [y]",
    CONTRADICTION_TYPE = "LOGICAL",
    DATA_PREPARATION = "",
    stringsAsFactors = FALSE
  )

  result <- suppressWarnings(suppressMessages(con_contradictions_redcap(
    study_data = study_data,
    meta_data = meta_data,
    label_col = LABEL,
    threshold_value = 1,
    meta_data_cross_item = meta_data_cross_item
  )))

  percentage <- result$VariableGroupData[[
    "Contradictions (Percentage (0 to 100))"
  ]]
  number <- result$VariableGroupData[["Contradictions (Number)"]]
  expect_equal(as.numeric(sub("%$", "", percentage)), 33.33)
  expect_equal(
    util_attr(percentage, DATA_TYPE, exact = TRUE),
    DATA_TYPES$FLOAT
  )
  expect_equal(number, 1, ignore_attr = TRUE)
  expect_equal(
    util_attr(number, DATA_TYPE, exact = TRUE),
    DATA_TYPES$INTEGER
  )
})


test_that("custom grading rulesets affect item and cross-item rendering", {
  withr::local_options(dataquieR.precomputeStudyData = FALSE)
  prep_purge_data_frame_cache()
  withr::defer(prep_purge_data_frame_cache())

  make_formats <- function() {
    data.frame(
      category = 1:5,
      label = paste0("test category ", 1:5),
      color = c("1 2 3", "4 5 6", "7 8 9", "10 11 12", "13 14 15"),
      stringsAsFactors = FALSE
    )
  }

  make_rules <- function() {
    make_rule <- function(ruleset, metric, category, interval = "[0; 100]") {
      rule <- data.frame(
        GRADING_RULESET = ruleset,
        indicator_metric = metric,
        Description = "test grading rule",
        dqi_cat_1 = NA_character_,
        dqi_cat_2 = NA_character_,
        dqi_cat_3 = NA_character_,
        dqi_cat_4 = NA_character_,
        dqi_cat_5 = NA_character_,
        stringsAsFactors = FALSE
      )
      rule[[paste0("dqi_cat_", category)]] <- interval
      rule
    }

    rbind(
      make_rule("0", "PCT_com_crm_mv", 5),
      make_rule("0", "NUM_con_con_contc", 5, "[2; 3)"),
      make_rule("0", "PCT_con_con_contu", 5),
      make_rule("1", "PCT_com_crm_mv", 1),
      make_rule("1", "NUM_con_con_contc", 1),
      make_rule("1", "PCT_con_con_contu", 1)
    )
  }

  prep_add_data_frames(data_frame_list = list(
    test_grading_formats = make_formats(),
    test_grading_rulesets = make_rules()
  ))

  meta_data <- local({
    meta_data <- data.frame(
      VAR_NAMES = c("first", "x", "y", "z"),
      LABEL = c("First", "X", "Y", "Z"),
      DATA_TYPE = "integer",
      SCALE_LEVEL = "metric",
      VARIABLE_ROLE = "primary",
      MISSING_LIST = "",
      JUMP_LIST = "",
      VALUE_LABELS = NA_character_,
      VALUE_LABEL_TABLE = NA_character_,
      HARD_LIMITS = "",
      GRADING_RULESET = c("1", "0", "0", "0"),
      stringsAsFactors = FALSE
    )
    meta_data
  })

  study_data <- data.frame(
    first = c(1, 2, 3, 4),
    x = c(1, 2, 3, 4),
    y = c(0, 2, 5, 0),
    z = c(1, 1, 1, 1)
  )
  meta_data_cross_item <- data.frame(
    VARIABLE_LIST = c("x | y", "z | x"),
    CHECK_LABEL = c("x greater y", "z lower x"),
    CONTRADICTION_TERM = c("[x] > [y]", "[z] < [x]"),
    CONTRADICTION_TYPE = c("LOGICAL", "EMPIRICAL"),
    GRADING_RULESET = c("1", "1"),
    DATA_PREPARATION = "|",
    stringsAsFactors = FALSE
  )

  withr::local_options(dataquieR.grading_rulesets = dataquieR.grading_rulesets_default) # nolint: line_length_linter.
  cross_item <- suppressWarnings(suppressMessages(con_contradictions_redcap(
    study_data = study_data,
    meta_data = meta_data,
    label_col = LABEL,
    meta_data_cross_item = meta_data_cross_item,
    summarize_categories = FALSE
  )))
  cross_item_categories <- suppressWarnings(suppressMessages(con_contradictions_redcap( # nolint: line_length_linter.
    study_data = study_data,
    meta_data = meta_data,
    label_col = LABEL,
    meta_data_cross_item = meta_data_cross_item,
    summarize_categories = TRUE
  )))

  withr::local_options(
    dataquieR.grading_formats = "test_grading_formats",
    dataquieR.grading_rulesets = "test_grading_rulesets"
  )

  report <- suppressWarnings(suppressMessages(dq_report2(
    study_data = study_data,
    meta_data = meta_data,
    dimensions = "Completeness",
    filter_indicator_functions = "com_item_missingness",
    cores = NULL
  )))
  report_summary <- summary(report)

  render_item_summary_color <- function(report_summary) {
    summary_with_ruleset <- util_reclassify_dataquieR_summary(report_summary)
    rowmaxes <- util_attr(summary_with_ruleset, "this", exact = TRUE)$rowmaxes
    rowmaxes$color[rowmaxes[[VAR_NAMES]] == "x"][[1]]
  }

  render_cross_item_color <- function() {
    util_make_cls_binding(
      cross_item$VariableGroupTable,
      meta_data
    )
  }

  render_cross_item_plot_fill <- function() {
    built <- ggplot2::ggplot_build(cross_item$SummaryPlot)$data
    unique(unlist(lapply(built, function(layer_data) {
      if ("y" %in% names(layer_data) && "fill" %in% names(layer_data)) {
        layer_data$fill
      } else {
        character(0)
      }
    }), use.names = FALSE))
  }

  render_cross_item_plotly_fill <- function() {
    py <- util_as_plotly_con_contradictions_redcap(cross_item)
    unique(unlist(lapply(py$x$data, function(trace) {
      if (is.null(trace$marker) || is.null(trace$marker$color)) {
        character(0)
      } else {
        trace$marker$color
      }
    }), use.names = FALSE))
  }

  render_cross_item_category_color <- function() {
    util_make_cls_binding(
      cross_item_categories$OtherTable,
      meta_data
    )
  }

  expect_equal(render_item_summary_color(report_summary), "#0d0e0f")
  expect_equal(render_cross_item_color(), rep("#010203", 2))
  expect_true("#010203" %in% render_cross_item_plot_fill())
  type_bands <- util_attr(cross_item,
    "contradiction_type_bands",
    exact = TRUE
  )
  expect_setequal(
    type_bands$display_label_full,
    c("Logical contradictions", "Empirical contradictions")
  )
  expect_setequal(type_bands$display_label, c("Log.", "Emp."))
  if (requireNamespace("plotly", quietly = TRUE)) {
    expect_true("rgba(1,2,3,1)" %in% render_cross_item_plotly_fill())
    py <- util_as_plotly_con_contradictions_redcap(cross_item)
    expect_true(length(py$x$layout$shapes) >= 2)
    expect_true(any(vapply(py$x$layout$annotations, function(annotation) {
      identical(annotation$text, "Log.")
    }, FUN.VALUE = logical(1))))
  }
  expect_true("#010203" %in% render_cross_item_category_color())

  cross_item_ruleset_0 <- suppressWarnings(suppressMessages(
    con_contradictions_redcap(
      study_data = study_data,
      meta_data = meta_data,
      label_col = LABEL,
      meta_data_cross_item = transform(
        meta_data_cross_item[1, ],
        GRADING_RULESET = "0"
      ),
      summarize_categories = FALSE
    )
  ))
  cross_item_ruleset_1 <- suppressWarnings(suppressMessages(
    con_contradictions_redcap(
      study_data = study_data,
      meta_data = meta_data,
      label_col = LABEL,
      meta_data_cross_item = transform(
        meta_data_cross_item[1, ],
        GRADING_RULESET = "1"
      ),
      summarize_categories = FALSE
    )
  ))

  expect_equal(
    cross_item_ruleset_0$VariableGroupTable[[GRADING_RULESET]],
    "0"
  )
  expect_equal(
    cross_item_ruleset_1$VariableGroupTable[[GRADING_RULESET]],
    "1"
  )
  expect_equal(
    util_make_cls_binding(cross_item_ruleset_0$VariableGroupTable, meta_data),
    "#0d0e0f"
  )
  expect_equal(
    util_make_cls_binding(cross_item_ruleset_1$VariableGroupTable, meta_data),
    "#010203"
  )

  study_data_ordered_bug <- data.frame(
    x = c(1, 2, 3, 4),
    y = c(0, 2, 5, 0),
    z = c(1, 1, 1, 1)
  )
  meta_data_cross_item_ordered_bug <- data.frame(
    VARIABLE_LIST = c("x | y", "z | x"),
    CHECK_LABEL = c("logical count should grade high", "empirical zero should grade low"), # nolint: line_length_linter.
    CONTRADICTION_TERM = c("[x] > [y]", "[z] < 0"),
    CONTRADICTION_TYPE = c("LOGICAL", "EMPIRICAL"),
    DATA_PREPARATION = "|",
    stringsAsFactors = FALSE
  )
  make_ordered_bug_rule <- function(metric, category, interval) {
    rule <- data.frame(
      GRADING_RULESET = "0",
      indicator_metric = metric,
      Description = "test grading rule",
      dqi_cat_1 = NA_character_,
      dqi_cat_2 = NA_character_,
      dqi_cat_3 = NA_character_,
      dqi_cat_4 = NA_character_,
      dqi_cat_5 = NA_character_,
      stringsAsFactors = FALSE
    )
    rule[[paste0("dqi_cat_", category)]] <- interval
    rule
  }
  prep_add_data_frames(data_frame_list = list(
    test_grading_rulesets_ordered_bug = rbind(
      make_ordered_bug_rule("NUM_con_con_contc", 5, "[2; 3)"),
      make_ordered_bug_rule("PCT_con_con_contu", 1, "[0; 1]")
    )
  ))
  withr::local_options(
    dataquieR.grading_rulesets = "test_grading_rulesets_ordered_bug"
  )
  cross_item_ordered_bug <- suppressWarnings(suppressMessages(
    con_contradictions_redcap(
      study_data = study_data_ordered_bug,
      meta_data = meta_data[meta_data[[VAR_NAMES]] %in% c("x", "y", "z"), ],
      label_col = LABEL,
      meta_data_cross_item = meta_data_cross_item_ordered_bug,
      summarize_categories = FALSE
    )
  ))
  ordered_bug_build <- ggplot2::ggplot_build(cross_item_ordered_bug$SummaryPlot)
  ordered_bug_fills <- unique(unlist(lapply(ordered_bug_build$data, function(layer_data) { # nolint: line_length_linter.
    if ("y" %in% names(layer_data) && "fill" %in% names(layer_data)) {
      layer_data$fill
    } else {
      character(0)
    }
  }), use.names = FALSE))
  expect_true("#010203" %in% ordered_bug_fills)
  expect_true("#0d0e0f" %in% ordered_bug_fills)
})

# Temporarily skipped due to ship moving to website
test_that("no regression, rule errors should not be missed", {
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  skip_on_cran()
  fixture <- redcap_ship_fixture()
  sd1 <- fixture$study_data
  md1 <- fixture$meta_data
  checks <- fixture$meta_data_cross_item
  meta_data2 <-
    prep_scalelevel_from_data_and_metadata(
      study_data = sd1,
      meta_data = md1
    )
  md1[[SCALE_LEVEL]] <-
    setNames(meta_data2[[SCALE_LEVEL]], nm = meta_data2[[VAR_NAMES]])[
      md1[[VAR_NAMES]]
    ]

  checks[3, 3] <- "[BODY_HEIGHT_0] < [OBS_SOMA_0]"
  checks[4, 3] <- "[BODY_HEIGHT_0] < [EXAM_DT_0]"
  checks[4, 3] <- "[BODY_HEIGHT] < 1.2"
  checks[5, 3] <- "[SEX] < 3"
  checks <- checks[-c(1:2), ]
  checks <- checks[-c(4:10), ]

  suppressMessages(suppressWarnings(expect_warning(
    any_contradictions <- con_contradictions_redcap(
      study_data = sd1,
      meta_data = md1,
      label_col = "LABEL",
      meta_data_cross_item = checks,
      threshold_value = 1
    ),
    regexp = "object.+SEX.+not found"
  )))

  expect_equal(
    any_contradictions$VariableGroupTable$NUM_con_con,
    c(2152, NA_real_, NA_real_)
  )
  # the first test is by default comparing lexicographically, since obs_soma is a factor and LABEL is default for DATA_PREPARATION # nolint: line_length_linter.
  # the other two tests always fail because of missing variables
  sd1 <- fixture$study_data
  md1 <- fixture$meta_data
  checks <- fixture$meta_data_cross_item

  suppressWarnings(suppressMessages(
    any_contradictions <- con_contradictions_redcap(
      study_data = sd1,
      meta_data = md1,
      label_col = "LABEL",
      meta_data_cross_item = checks,
      threshold_value = 1
    )
  ))

  expect_equal(any_contradictions$VariableGroupTable$NUM_con_con, c(
    35,
    0,
    0,
    63,
    12,
    0,
    0
  ))
  sd1 <- fixture$study_data
  md1 <- fixture$meta_data
  checks <- fixture$meta_data_cross_item

  checks[1, 3] <- "[sbp] < [dbp]" # variables not in dataset/metadata
  checks[2, 3] <- "sbp2 < dbp2" # omit brackets
  checks[6, 3] <-
    "[diab_known] = \"yes\" NOT [diab_age] > 0" # uses variable names instead of labels # nolint: line_length_linter.
  checks <- checks[-c(3:5), ]
  checks <- checks[-c(4:9), ]


  suppressMessages(suppressWarnings(expect_warning(
    {
      any_contradictions <- con_contradictions_redcap(
        study_data = sd1,
        meta_data = md1,
        label_col = "LABEL",
        meta_data_cross_item = checks,
        threshold_value = 1
      )
    },
    regexp = "Parser error"
  )))

  expect_true(all(is.na(any_contradictions$VariableGroupTable$NUM_con_con)))
})
