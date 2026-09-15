test_that("util_varcomp_robust works", {
  skip_if_not_installed("rankICC")
  skip_on_cran()
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  prep_load_workbook_like_file("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx") # nolint: line_length_linter.
  r <- util_varcomp_robust(
    resp_vars = "SBP_0",
    group_vars = "USR_BP_0",
    study_data = "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData", # nolint: line_length_linter.
    meta_data = "item_level"
  )
  expect_snapshot(r)
})

test_that("util_varcomp_robust reports local guardrail conditions", {
  skip_if_not_installed("rankICC")
  skip_on_cran()

  meta_data <- data.frame(
    VAR_NAMES = c("score", "group"),
    DATA_TYPE = c(DATA_TYPES$FLOAT, DATA_TYPES$STRING),
    SCALE_LEVEL = c(SCALE_LEVELS$RATIO, SCALE_LEVELS$NOMINAL),
    MISSING_LIST = c(SPLIT_CHAR, SPLIT_CHAR),
    JUMP_LIST = c(SPLIT_CHAR, SPLIT_CHAR),
    stringsAsFactors = FALSE
  )
  study_data <- data.frame(
    score = c(1, 2, 3, 4),
    group = factor(c("a", "a", "b", "b")),
    stringsAsFactors = FALSE
  )

  expect_error(
    util_varcomp_robust(
      resp_vars = "score",
      group_vars = "group",
      study_data = study_data,
      meta_data = meta_data,
      min_obs_in_subgroup = 1,
      min_subgroups = 3,
      label_col = VAR_NAMES
    ),
    "< 3 levels"
  )

  constant_data <- transform(study_data, score = 1)
  expect_error(
    util_varcomp_robust(
      resp_vars = "score",
      group_vars = "group",
      study_data = constant_data,
      meta_data = meta_data,
      min_obs_in_subgroup = 1,
      min_subgroups = 2,
      label_col = VAR_NAMES
    ),
    "response variable is constant"
  )

  sparse_data <- data.frame(
    score = c(1, 2, 3, 4),
    group = factor(c("a", "a", "b", "c")),
    stringsAsFactors = FALSE
  )
  expect_message(
    expect_error(
      util_varcomp_robust(
        resp_vars = "score",
        group_vars = "group",
        study_data = sparse_data,
        meta_data = meta_data,
        min_obs_in_subgroup = 2,
        min_subgroups = 2,
        label_col = VAR_NAMES
      ),
      "< 2 levels"
    ),
    "were excluded"
  )
})

test_that("util_varcomp_robust abbreviates many excluded levels", {
  skip_if_not_installed("rankICC")
  skip_on_cran()

  meta_data <- data.frame(
    VAR_NAMES = c("score", "group"),
    DATA_TYPE = c(DATA_TYPES$FLOAT, DATA_TYPES$STRING),
    SCALE_LEVEL = c(SCALE_LEVELS$RATIO, SCALE_LEVELS$NOMINAL),
    MISSING_LIST = c(SPLIT_CHAR, SPLIT_CHAR),
    JUMP_LIST = c(SPLIT_CHAR, SPLIT_CHAR),
    stringsAsFactors = FALSE
  )
  study_data <- data.frame(
    score = seq_len(18),
    group = factor(c(
      rep(c("keep1", "keep2"), each = 3),
      paste0("drop", seq_len(12))
    )),
    stringsAsFactors = FALSE
  )

  messages <- new.env(parent = emptyenv())
  messages$text <- character()

  result <- withCallingHandlers(
    util_varcomp_robust(
      resp_vars = "score",
      group_vars = "group",
      study_data = study_data,
      meta_data = meta_data,
      min_obs_in_subgroup = 2,
      min_subgroups = 2,
      label_col = VAR_NAMES
    ),
    message = function(m) {
      messages$text <- c(messages$text, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )

  expect_type(result, "double")
  expect_match(paste(messages$text, collapse = "\n"), "\\.\\.\\.")
})
