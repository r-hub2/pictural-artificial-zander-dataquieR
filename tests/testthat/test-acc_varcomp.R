test_that("acc_varcomp rejects deprecated threshold_value early", {
  skip_on_cran()

  expect_error(
    acc_varcomp(threshold_value = 0.5),
    "threshold_value.*acc_varcomp"
  )
})

test_that("acc_varcomp works without label_col", {
  skip_on_cran()
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  meta_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data.RData") # nolint: line_length_linter.
  study_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData", keep_types = TRUE) # nolint: line_length_linter.
  expect_error(
    {
      res1 <-
        acc_varcomp(
          resp_vars = "v00014", study_data = study_data,
          meta_data = meta_data
        )
    },
    regexp =
      paste("Argument group_vars is NULL"),
    perl = TRUE
  )

  expect_error(
    res1 <-
      acc_varcomp(
        resp_vars = c("CRP_0", "SBP_0"), study_data = study_data,
        meta_data = meta_data, group_vars = c("DEV_NO_0", "USR_BP_0"),
        label_col = LABEL
      ),
    regexp =
      sprintf(
        "(%s)",
        paste("Need exactly one element in argument resp_vars, got 2: .CRP_0, SBP_0.") # nolint: line_length_linter.
      ),
    perl = TRUE
  )

  suppressMessages(
    res1 <-
      acc_varcomp(
        resp_vars = c("DBP_0"), study_data = study_data,
        meta_data = meta_data, group_vars = c("USR_BP_0"),
        label_col = LABEL
      )
  )

  expect_true(all(
    c(
      "SummaryTable",
      "ScalarValue_max_icc",
      "ScalarValue_argmax_icc"
    ) %in% names(res1)
  ))

  expect_lt(
    suppressWarnings(abs(sum(
      as.numeric(
        as.matrix(res1$SummaryTable)
      ),
      na.rm = TRUE
    ) - 783.112)), 0.1
  )

  expect_true(res1$ScalarValue_max_icc == 0.112)
  expect_true(res1$ScalarValue_argmax_icc == "DBP_0")
})

test_that("acc_varcomp works with label_col", {
  skip_on_cran()
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  meta_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data.RData") # nolint: line_length_linter.
  study_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData", keep_types = TRUE) # nolint: line_length_linter.
  expect_error(
    {
      res1 <-
        acc_varcomp(
          resp_vars = "SBP_0", study_data = study_data,
          label_col = LABEL,
          meta_data = meta_data
        )
    },
    regexp = paste("Argument group_vars is NULL")
  )

  res1 <-
    acc_varcomp(
      resp_vars = "v00014", study_data = study_data,
      meta_data = meta_data, group_vars = "v00016"
    )

  expect_true(all(
    c(
      "SummaryTable",
      "ScalarValue_max_icc",
      "ScalarValue_argmax_icc"
    ) %in% names(res1)
  ))

  expect_lt(
    suppressWarnings(abs(sum(
      as.numeric(
        as.matrix(res1$SummaryTable)
      ),
      na.rm = TRUE
    ) - 2082.221)), 0.1
  )

  expect_equal(res1$ScalarValue_max_icc, 0.021)
  expect_equal(res1$ScalarValue_argmax_icc, "v00014")
})

test_that("acc_varcomp works illegal min_obs_in_subgroup/min_subgroups", {
  skip_on_cran()
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  meta_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data.RData") # nolint: line_length_linter.
  study_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData", keep_types = TRUE) # nolint: line_length_linter.
  suppressMessages(expect_error(
    res1 <-
      acc_varcomp(
        resp_vars = c("DBP_0"), study_data = study_data,
        meta_data = meta_data, group_vars = c("USR_BP_0"),
        label_col = LABEL, min_obs_in_subgroup = "k",
        min_subgroups = "x"
      ),
    regexp =
      sprintf(
        "(%s|%s|%s)",
        paste("min_obs_in_subgroup needs to be integer > 0"),
        paste(
          "Could not convert min_subgroups .+x.+ to a number.",
          "Set to standard value."
        ),
        paste(
          "Levels .+USR_559.+ were excluded",
          "due to fewer than 30 observations."
        )
      )
  ))

  suppressMessages(
    res1 <-
      acc_varcomp(
        resp_vars = c("DBP_0"), study_data = study_data,
        meta_data = meta_data, group_vars = c("USR_BP_0"),
        label_col = LABEL
      )
  )

  expect_true(all(
    c(
      "SummaryTable",
      "ScalarValue_max_icc",
      "ScalarValue_argmax_icc"
    ) %in% names(res1)
  ))

  expect_lt(
    suppressWarnings(abs(sum(
      as.numeric(
        as.matrix(res1$SummaryTable)
      ),
      na.rm = TRUE
    ) - 783.112)), 0.1
  )

  expect_true(res1$ScalarValue_max_icc == 0.112)
  expect_true(res1$ScalarValue_argmax_icc == "DBP_0")
})

test_that("acc_varcomp works without resp_vars", {
  skip_on_cran()
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  meta_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data.RData") # nolint: line_length_linter.
  study_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData", keep_types = TRUE) # nolint: line_length_linter.

  expect_error(
    res1 <-
      acc_varcomp(
        study_data = study_data,
        meta_data = meta_data,
        group_vars = c("v00016", "v00012")
      ),
    regexp =
      paste("Argument resp_vars is NULL")
  )

  res1 <-
    suppressMessages(acc_varcomp(
      study_data = study_data,
      meta_data = meta_data,
      resp_vars = "v00004",
      group_vars = "v00016"
    ))

  expect_true(all(
    c(
      "SummaryTable",
      "ScalarValue_max_icc",
      "ScalarValue_argmax_icc"
    ) %in% names(res1)
  ))

  expect_lt(
    suppressWarnings(abs(sum(
      as.numeric(
        as.matrix(res1$SummaryTable)
      ),
      na.rm = TRUE
    ) - 1932.009)), 0.1
  )

  expect_equal(res1$ScalarValue_max_icc, 0.008)
  expect_equal(res1$ScalarValue_argmax_icc, "v00004")
})

test_that("acc_varcomp stops on too few subgroups", {
  skip_on_cran()
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  meta_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data.RData") # nolint: line_length_linter.
  study_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData", keep_types = TRUE) # nolint: line_length_linter.

  expect_error(
    res1 <-
      acc_varcomp(
        resp_vars = "v00014",
        study_data = study_data,
        meta_data = meta_data,
        group_vars = "v00016",
        min_subgroups = 50
      ),
    regexp = "5 . 50 levels in .+v00016.+ Will not compute ICCs for .+v00014.+."
  )

  # Historical empty-result detail expectations removed here.
})

test_that("acc_varcomp handles nominal and ordinal response variables", {
  skip_on_cran()
  skip_if_not_installed("lme4")

  group <- factor(rep(c("first", "second", "third"), each = 12L))
  outcome_binary <- rep(rep(c(0L, 1L), each = 6L), 3L)
  outcome_nominal <- rep(rep(c("low", "medium", "high"), each = 4L), 3L)
  outcome_ordinal <- rep(rep(1:3, each = 4L), 3L)
  study_data <- data.frame(
    group = group,
    outcome_binary = outcome_binary,
    outcome_nominal = outcome_nominal,
    outcome_ordinal = outcome_ordinal,
    stringsAsFactors = FALSE
  )
  meta_data <- data.frame(
    VAR_NAMES = c(
      "group", "outcome_binary", "outcome_nominal", "outcome_ordinal"
    ),
    LABEL = c("group", "outcome_binary", "outcome_nominal", "outcome_ordinal"),
    DATA_TYPE = c(
      DATA_TYPES$STRING,
      DATA_TYPES$INTEGER,
      DATA_TYPES$STRING,
      DATA_TYPES$INTEGER
    ),
    SCALE_LEVEL = c(
      SCALE_LEVELS$NOMINAL,
      SCALE_LEVELS$RATIO,
      SCALE_LEVELS$NOMINAL,
      SCALE_LEVELS$ORDINAL
    ),
    MISSING_LIST = SPLIT_CHAR,
    JUMP_LIST = SPLIT_CHAR,
    stringsAsFactors = FALSE
  )

  nominal <- suppressMessages(acc_varcomp(
    resp_vars = "outcome_nominal",
    group_vars = "group",
    study_data = study_data,
    meta_data = meta_data,
    label_col = LABEL,
    min_obs_in_subgroup = 2L,
    min_subgroups = 2L
  ))
  binary <- suppressMessages(acc_varcomp(
    resp_vars = "outcome_binary",
    group_vars = "group",
    study_data = study_data,
    meta_data = meta_data,
    label_col = LABEL,
    min_obs_in_subgroup = 2L,
    min_subgroups = 2L
  ))
  ordinal <- suppressMessages(acc_varcomp(
    resp_vars = "outcome_ordinal",
    group_vars = "group",
    study_data = study_data,
    meta_data = meta_data,
    label_col = LABEL,
    min_obs_in_subgroup = 2L,
    min_subgroups = 2L,
    cut_off_linear_model_for_ord = 1L
  ))

  expect_named(nominal, c("SummaryTable", "SummaryData"))
  expect_equal(as.character(nominal$SummaryData$Variables), "outcome_nominal")
  expect_equal(nominal$SummaryData$Class.Number, 3)
  expect_named(binary, c("SummaryTable", "SummaryData"))
  expect_equal(as.character(binary$SummaryData$Variables), "outcome_binary")
  expect_equal(binary$SummaryData$Class.Number, 3)
  expect_named(ordinal, c("SummaryTable", "SummaryData"))
  expect_equal(as.character(ordinal$SummaryData$Variables), "outcome_ordinal")
  expect_equal(ordinal$SummaryData$Class.Number, 3)
})

test_that("acc_varcomp covers interval integer response variables", {
  skip_on_cran()
  skip_if_not_installed("lme4")

  group <- factor(rep(c("first", "second", "third"), each = 12L))
  study_data <- data.frame(
    group = group,
    outcome_interval = seq_len(36L),
    stringsAsFactors = FALSE
  )
  meta_data <- data.frame(
    VAR_NAMES = c("group", "outcome_interval"),
    LABEL = c("group", "outcome_interval"),
    DATA_TYPE = c(DATA_TYPES$STRING, DATA_TYPES$INTEGER),
    SCALE_LEVEL = c(SCALE_LEVELS$NOMINAL, SCALE_LEVELS$INTERVAL),
    MISSING_LIST = SPLIT_CHAR,
    JUMP_LIST = SPLIT_CHAR,
    stringsAsFactors = FALSE
  )

  interval <- suppressMessages(acc_varcomp(
    resp_vars = "outcome_interval",
    group_vars = "group",
    study_data = study_data,
    meta_data = meta_data,
    label_col = LABEL,
    min_obs_in_subgroup = 2L,
    min_subgroups = 2L
  ))

  expect_named(interval, c("SummaryTable", "SummaryData"))
  expect_equal(as.character(interval$SummaryData$Variables), "outcome_interval")
  expect_equal(interval$SummaryData$Class.Number, 3)
})

test_that("acc_varcomp applies explicit recoding for ratio responses", {
  skip_on_cran()
  skip_if_not_installed("lme4")

  group <- factor(rep(c("first", "second", "third"), each = 12L))
  study_data <- data.frame(
    group = group,
    outcome = rep(rep(c(1L, 2L, 3L), each = 4L), 3L),
    stringsAsFactors = FALSE
  )
  meta_data <- data.frame(
    VAR_NAMES = c("group", "outcome"),
    LABEL = c("group", "outcome"),
    DATA_TYPE = c(DATA_TYPES$STRING, DATA_TYPES$INTEGER),
    SCALE_LEVEL = c(SCALE_LEVELS$NOMINAL, SCALE_LEVELS$RATIO),
    MISSING_LIST = SPLIT_CHAR,
    JUMP_LIST = SPLIT_CHAR,
    HARD_LIMITS = "",
    RECODE_CASES = c("", "1 | 2"),
    RECODE_CONTROL = c("", "3"),
    stringsAsFactors = FALSE
  )

  seen <- new.env(parent = emptyenv())
  seen$recoded <- FALSE
  recoded <- expect_warning(withCallingHandlers(
    acc_varcomp(
      resp_vars = "outcome",
      group_vars = "group",
      study_data = study_data,
      meta_data = meta_data,
      label_col = LABEL,
      min_obs_in_subgroup = 2L,
      min_subgroups = 2L
    ),
    message = function(m) {
      if (grepl("Recoded 1 variable", conditionMessage(m), fixed = TRUE)) {
        seen$recoded <- TRUE
      }
      invokeRestart("muffleMessage")
    }
  ), NA)

  expect_named(recoded, c("SummaryTable", "SummaryData"))
  expect_equal(as.character(recoded$SummaryData$Variables), "outcome")
  expect_equal(as.character(recoded$SummaryData$ICC), "0")
  expect_true(seen$recoded)
  expect_null(attr(recoded, "warning", exact = TRUE))
})

test_that("acc_varcomp delegates float ratio and interval responses", {
  skip_on_cran()
  skip_if_not_installed("lme4")

  group <- factor(rep(c("first", "second", "third"), each = 12L))
  study_data <- data.frame(
    group = group,
    outcome_ratio = as.numeric(seq_len(36L)),
    outcome_interval = as.numeric(seq_len(36L)) / 10,
    stringsAsFactors = FALSE
  )
  meta_data <- data.frame(
    VAR_NAMES = c("group", "outcome_ratio", "outcome_interval"),
    LABEL = c("group", "outcome_ratio", "outcome_interval"),
    DATA_TYPE = c(DATA_TYPES$STRING, DATA_TYPES$FLOAT, DATA_TYPES$FLOAT),
    SCALE_LEVEL = c(
      SCALE_LEVELS$NOMINAL, SCALE_LEVELS$RATIO, SCALE_LEVELS$INTERVAL
    ),
    MISSING_LIST = SPLIT_CHAR,
    JUMP_LIST = SPLIT_CHAR,
    stringsAsFactors = FALSE
  )

  ratio <- suppressMessages(acc_varcomp(
    resp_vars = "outcome_ratio",
    group_vars = "group",
    study_data = study_data,
    meta_data = meta_data,
    label_col = LABEL,
    min_obs_in_subgroup = 2L,
    min_subgroups = 2L
  ))
  interval <- suppressMessages(acc_varcomp(
    resp_vars = "outcome_interval",
    group_vars = "group",
    study_data = study_data,
    meta_data = meta_data,
    label_col = LABEL,
    min_obs_in_subgroup = 2L,
    min_subgroups = 2L
  ))

  expect_named(
    ratio,
    c("SummaryTable", "SummaryData", "ScalarValue_max_icc",
      "ScalarValue_argmax_icc")
  )
  expect_equal(as.character(ratio$SummaryData$Variables), "outcome_ratio")
  expect_equal(as.character(interval$SummaryData$Variables), "outcome_interval")
  expect_equal(ratio$SummaryData$Class.Number, 3)
  expect_equal(interval$SummaryData$Class.Number, 3)
  expect_null(attr(ratio, "warning", exact = TRUE))
  expect_null(attr(interval, "warning", exact = TRUE))
})

test_that("acc_varcomp rejects sparse binary response categories", {
  skip_on_cran()

  study_data <- data.frame(
    group = factor(rep(c("first", "second", "third"), each = 10L)),
    outcome_binary = c(rep(0L, 28L), 1L, 1L),
    stringsAsFactors = FALSE
  )
  meta_data <- data.frame(
    VAR_NAMES = c("group", "outcome_binary"),
    LABEL = c("group", "outcome_binary"),
    DATA_TYPE = c(DATA_TYPES$STRING, DATA_TYPES$INTEGER),
    SCALE_LEVEL = c(SCALE_LEVELS$NOMINAL, SCALE_LEVELS$RATIO),
    MISSING_LIST = SPLIT_CHAR,
    JUMP_LIST = SPLIT_CHAR,
    stringsAsFactors = FALSE
  )

  expect_error(
    acc_varcomp(
      resp_vars = "outcome_binary",
      group_vars = "group",
      study_data = study_data,
      meta_data = meta_data,
      label_col = LABEL,
      min_obs_in_subgroup = 3L,
      min_subgroups = 2L
    ),
    "Too few observations with different values"
  )
})

test_that("acc_varcomp rejects constant and unsupported ratio responses", {
  skip_on_cran()

  constant_data <- data.frame(
    group = factor(rep(c("first", "second", "third"), each = 4L)),
    outcome_constant = 1L
  )
  constant_meta <- data.frame(
    VAR_NAMES = c("group", "outcome_constant"),
    LABEL = c("group", "outcome_constant"),
    DATA_TYPE = c(DATA_TYPES$STRING, DATA_TYPES$INTEGER),
    SCALE_LEVEL = c(SCALE_LEVELS$NOMINAL, SCALE_LEVELS$RATIO),
    MISSING_LIST = SPLIT_CHAR,
    JUMP_LIST = SPLIT_CHAR,
    stringsAsFactors = FALSE
  )

  expect_error(
    acc_varcomp(
      resp_vars = "outcome_constant",
      group_vars = "group",
      study_data = constant_data,
      meta_data = constant_meta,
      label_col = LABEL,
      min_obs_in_subgroup = 2L,
      min_subgroups = 2L
    ),
    "response variable is constant after data preparation"
  )

  text_data <- data.frame(
    group = factor(rep(c("first", "second", "third"), each = 6L)),
    outcome_text = rep(rep(c("a", "b", "c"), each = 2L), 3L)
  )
  text_meta <- data.frame(
    VAR_NAMES = c("group", "outcome_text"),
    LABEL = c("group", "outcome_text"),
    DATA_TYPE = c(DATA_TYPES$STRING, DATA_TYPES$STRING),
    SCALE_LEVEL = c(SCALE_LEVELS$NOMINAL, SCALE_LEVELS$RATIO),
    MISSING_LIST = SPLIT_CHAR,
    JUMP_LIST = SPLIT_CHAR,
    stringsAsFactors = FALSE
  )

  expect_error(
    acc_varcomp(
      resp_vars = "outcome_text",
      group_vars = "group",
      study_data = text_data,
      meta_data = text_meta,
      label_col = LABEL,
      min_obs_in_subgroup = 2L,
      min_subgroups = 2L
    ),
    "No method implemented for scale level ratio"
  )
})

test_that("util_acc_varcomp covers small internal guardrails", {
  skip_on_cran()
  skip_if_not_installed("lme4")

  study_data <- data.frame(
    group = rep(c("first", "second", "third"), each = 12L),
    outcome = seq_len(36L),
    outcome2 = seq_len(36L) + 1L,
    stringsAsFactors = FALSE
  )
  meta_data <- data.frame(
    VAR_NAMES = c("group", "outcome", "outcome2"),
    LABEL = c("group", "outcome", "outcome2"),
    DATA_TYPE = c(DATA_TYPES$STRING, DATA_TYPES$FLOAT, DATA_TYPES$FLOAT),
    SCALE_LEVEL = c(
      SCALE_LEVELS$NOMINAL, SCALE_LEVELS$RATIO, SCALE_LEVELS$RATIO
    ),
    MISSING_LIST = SPLIT_CHAR,
    JUMP_LIST = SPLIT_CHAR,
    stringsAsFactors = FALSE
  )

  repeated_group <- suppressMessages(util_acc_varcomp(
    resp_vars = c("outcome", "outcome2"),
    group_vars = "group",
    study_data = study_data,
    meta_data = meta_data,
    label_col = LABEL,
    min_obs_in_subgroup = 2L,
    min_subgroups = 2L
  ))

  expect_equal(
    as.character(repeated_group$SummaryData$Variables),
    c("outcome", "outcome2")
  )
  expect_equal(repeated_group$SummaryData$Object, c("group", "group"))
  expect_equal(repeated_group$SummaryData$Class.Number, c(3, 3))

  auto_resp_vars <- suppressMessages(util_acc_varcomp(
    resp_vars = character(0),
    group_vars = "group",
    study_data = study_data,
    meta_data = meta_data,
    label_col = LABEL,
    min_obs_in_subgroup = 2L,
    min_subgroups = 2L
  ))

  expect_equal(
    as.character(auto_resp_vars$SummaryData$Variables),
    c("outcome", "outcome2")
  )

  expect_error(
    suppressMessages(util_acc_varcomp(
      resp_vars = c("outcome", "outcome2"),
      group_vars = character(0),
      study_data = study_data,
      meta_data = meta_data,
      label_col = LABEL,
      min_obs_in_subgroup = 2L,
      min_subgroups = 2L
    )),
    "expects one group_var per resp_var"
  )
})

test_that("util_acc_varcomp returns empty results for local subgroup limits", {
  skip_on_cran()
  skip_if_not_installed("lme4")

  study_data <- data.frame(
    group = rep(c("first", "second", "third"), each = 12L),
    outcome = seq_len(36L),
    stringsAsFactors = FALSE
  )
  meta_data <- data.frame(
    VAR_NAMES = c("group", "outcome"),
    LABEL = c("group", "outcome"),
    DATA_TYPE = c(DATA_TYPES$STRING, DATA_TYPES$FLOAT),
    SCALE_LEVEL = c(SCALE_LEVELS$NOMINAL, SCALE_LEVELS$RATIO),
    MISSING_LIST = SPLIT_CHAR,
    JUMP_LIST = SPLIT_CHAR,
    stringsAsFactors = FALSE
  )

  too_few_subgroups <- suppressMessages(util_acc_varcomp(
    resp_vars = "outcome",
    group_vars = "group",
    study_data = study_data,
    meta_data = meta_data,
    label_col = LABEL,
    min_obs_in_subgroup = 2L,
    min_subgroups = 4L
  ))

  expect_equal(nrow(too_few_subgroups$SummaryData), 0L)
  expect_equal(too_few_subgroups$ScalarValue_max_icc, -Inf)
  expect_length(too_few_subgroups$ScalarValue_argmax_icc, 0L)

  invalid_minimums <- suppressMessages(util_acc_varcomp(
    resp_vars = "outcome",
    group_vars = "group",
    study_data = study_data,
    meta_data = meta_data,
    label_col = LABEL,
    min_obs_in_subgroup = "invalid",
    min_subgroups = "invalid"
  ))

  expect_equal(nrow(invalid_minimums$SummaryData), 0L)
  expect_equal(invalid_minimums$ScalarValue_max_icc, -Inf)
  expect_length(invalid_minimums$ScalarValue_argmax_icc, 0L)
})
