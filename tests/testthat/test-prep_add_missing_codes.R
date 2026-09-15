skip_on_cran()

test_that("prep_add_missing_codes evaluates simple rules without preparing data again", { # nolint: line_length_linter.
  study_data <- data.frame(
    target = c(NA, NA, 0),
    selector = c(1, 2, 1),
    other = c("x", "y", "y")
  )
  meta_data <- data.frame(
    VAR_NAMES = c("target", "selector", "other"),
    LABEL = c("target", "selector", "other"),
    DATA_TYPE = c("integer", "integer", "string"),
    MISSING_LIST = "",
    JUMP_LIST = "",
    stringsAsFactors = FALSE
  )
  rules <- data.frame(
    resp_vars = "target",
    CODE_CLASS = "JUMP",
    CODE_LABEL = "Selector one",
    CODE_VALUE = "9999",
    RULE = "[selector] = 1",
    stringsAsFactors = FALSE
  )

  testthat::local_mocked_bindings(
    util_eval_rule = function(...) {
      stop("legacy rule preparation path used", call. = FALSE)
    },
    .package = "dataquieR"
  )

  res <- prep_add_missing_codes(
    NA,
    study_data = study_data,
    meta_data = meta_data,
    label_col = "LABEL",
    rules = rules,
    use_value_labels = FALSE
  )

  expect_identical(
    as.character(res$ModifiedStudyData$target),
    c("9999", NA, "0")
  )
})

test_that("prep_add_missing_codes keeps prepared rule evaluation conservative", { # nolint: line_length_linter.
  expect_true(util_can_eval_missing_code_rule_prepared(
    util_parse_missing_code_data_preparation("")
  ))
  expect_false(util_can_eval_missing_code_rule_prepared(
    util_parse_missing_code_data_preparation("LABEL")
  ))
  expect_false(util_can_eval_missing_code_rule_prepared(
    util_parse_missing_code_data_preparation("LIMITS")
  ))
  expect_false(util_can_eval_missing_code_rule_prepared(
    util_parse_missing_code_data_preparation("MISSING_NA")
  ))
  expect_warning(
    expect_identical(
      util_parse_missing_code_data_preparation("MISSING_LABEL | MISSING_NA"),
      "MISSING_NA"
    ),
    "Invalid"
  )
})

test_that("missing-code DATA_PREPARATION tokens choose replacement modes", {
  skip_on_cran()

  expect_identical(
    util_missing_code_replacement_mode(character()),
    ""
  )
  expect_identical(
    util_missing_code_replacement_mode("MISSING_LABEL"),
    "LABEL"
  )
  expect_identical(
    util_missing_code_replacement_mode("MISSING_INTERPRET"),
    "INTERPRET"
  )
  expect_identical(
    util_missing_code_replacement_mode("MISSING_NA"),
    "NA"
  )
  expect_identical(
    util_missing_code_replacement_mode(c(
      "MISSING_LABEL",
      "MISSING_INTERPRET",
      "MISSING_NA"
    )),
    "NA"
  )
})

test_that("prepared REDCap rule evaluation keeps label aliases", {
  ds1 <- data.frame(
    target = c(NA, NA, 0),
    selector = c(1, 2, 1)
  )
  attr(ds1, "label_col") <- "LABEL"
  meta_data <- data.frame(
    VAR_NAMES = c("v1", "v2"),
    LABEL = c("target", "selector"),
    stringsAsFactors = FALSE
  )

  rule <- util_parse_redcap_rule("[v2] = 1")

  expect_identical(
    util_eval_prepared_redcap_rule(
      rule = rule,
      ds1 = ds1,
      meta_data = meta_data,
      label_col = "LABEL"
    ),
    c(TRUE, FALSE, TRUE)
  )
})

test_that("prep_add_missing_codes handles rule variable guardrails", {
  skip_on_cran()

  study_data <- data.frame(target = c(NA, 1))
  meta_data <- data.frame(
    VAR_NAMES = "target",
    LABEL = "target",
    DATA_TYPE = DATA_TYPES$INTEGER,
    MISSING_LIST = "",
    JUMP_LIST = "",
    stringsAsFactors = FALSE
  )
  base_rules <- data.frame(
    resp_vars = "target",
    CODE_CLASS = "JUMP",
    CODE_LABEL = "Target one",
    CODE_VALUE = "9999",
    RULE = "[target] = 1",
    stringsAsFactors = FALSE
  )
  conflicting_rules <- base_rules
  conflicting_rules[[VAR_NAMES]] <- "target"

  expect_error(
    prep_add_missing_codes(
      NA,
      study_data = study_data,
      meta_data = meta_data,
      label_col = LABEL,
      rules = conflicting_rules,
      use_value_labels = FALSE
    ),
    "give only one of these columns"
  )

  ignored_rules <- base_rules
  ignored_rules$resp_vars <- "unknown"

  expect_message(
    result <- prep_add_missing_codes(
      NA,
      study_data = study_data,
      meta_data = meta_data,
      label_col = LABEL,
      rules = ignored_rules,
      use_value_labels = FALSE
    ),
    "Ignoring these rules"
  )

  expect_identical(result$ModifiedStudyData$target, study_data$target)
  expect_identical(result$ModifiedMetaData[[VAR_NAMES]], meta_data[[VAR_NAMES]])
})

test_that("prep_add_missing_codes derives missing DATA_PREPARATION defaults", {
  skip_on_cran()

  study_data <- data.frame(
    target = c(NA, NA),
    selector = c(1L, 2L)
  )
  meta_data <- data.frame(
    VAR_NAMES = c("target", "selector"),
    LABEL = c("target", "selector"),
    DATA_TYPE = DATA_TYPES$INTEGER,
    VALUE_LABELS = c(NA_character_, "1 = yes | 2 = no"),
    MISSING_LIST = SPLIT_CHAR,
    JUMP_LIST = SPLIT_CHAR,
    stringsAsFactors = FALSE
  )
  base_rules <- data.frame(
    resp_vars = "target",
    CODE_CLASS = "JUMP",
    CODE_LABEL = "Selector matched",
    CODE_VALUE = "9999",
    RULE = "[selector] = 1",
    stringsAsFactors = FALSE
  )

  code_result <- prep_add_missing_codes(
    NA,
    study_data = study_data,
    meta_data = meta_data,
    label_col = LABEL,
    rules = base_rules,
    use_value_labels = FALSE
  )

  label_rules <- base_rules
  label_rules[[RULE]] <- '[selector] = "yes"'
  label_result <- prep_add_missing_codes(
    NA,
    study_data = study_data,
    meta_data = meta_data,
    label_col = LABEL,
    rules = label_rules,
    use_value_labels = NA
  )

  expect_identical(as.character(code_result$ModifiedStudyData$target), c(
    "9999",
    NA
  ))
  expect_identical(as.character(label_result$ModifiedStudyData$target), c(
    "9999",
    NA
  ))
})

test_that("prep_add_missing_codes works", {
  skip_on_cran() # remote data and snapshots are covered by CI
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  study_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData", keep_types = TRUE) # nolint: line_length_linter.
  meta_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data.RData") # nolint: line_length_linter.
  rules <- tibble::tribble(
    ~resp_vars, ~CODE_CLASS, ~CODE_LABEL, ~CODE_VALUE, ~RULE,
    "PREGNANT_0", "JUMP", "No pregnancies in males", "9999", "[SEX_0]=1",
  )
  r <- prep_add_missing_codes(NA,
    study_data = study_data, meta_data =
      meta_data,
    label_col = "LABEL", rules = rules, use_value_labels = FALSE
  )
  subset(r$ModifiedMetaData, LABEL == "PREGNANT_0", JUMP_LIST)
  subset(meta_data, LABEL == "PREGNANT_0", JUMP_LIST)
  vn <- subset(r$ModifiedMetaData, LABEL == "PREGNANT_0", VAR_NAMES)[[1]]
  expect_snapshot(table(study_data[[vn]], useNA = "always"))
  expect_snapshot(table(r$ModifiedStudyData[[vn]], useNA = "always"))
  r <- prep_add_missing_codes(NA,
    study_data = study_data, meta_data =
      meta_data, label_col = "LABEL", rules = rules, use_value_labels = FALSE,
    overwrite = TRUE
  )
  expect_snapshot(table(study_data[[vn]], useNA = "always"))
  expect_snapshot(table(r$ModifiedStudyData[[vn]], useNA = "always"))

  rules <- tibble::tribble(
    ~resp_vars, ~CODE_CLASS, ~CODE_LABEL, ~CODE_VALUE, ~RULE,
    "PREGNANT_0", "JUMP", "No pregnancies in males", "9999", '[SEX_0]="males"',
  )
  r <- prep_add_missing_codes(NA,
    study_data = study_data,
    meta_data = meta_data,
    label_col = "LABEL", rules = rules, use_value_labels = TRUE,
    overwrite = FALSE
  )
  expect_snapshot(table(study_data[[vn]], useNA = "always"))
  expect_snapshot(table(r$ModifiedStudyData[[vn]], useNA = "always"))

  rules <- tibble::tribble(
    ~resp_vars, ~CODE_CLASS, ~CODE_LABEL, ~CODE_VALUE, ~RULE,
    "PREGNANT_0", "JUMP", "No pregs in males", "9999", '[v00002]="males"',
  )
  r <- prep_add_missing_codes(NA,
    study_data = study_data,
    meta_data = meta_data,
    label_col = "LABEL", rules = rules, use_value_labels = TRUE,
    overwrite = FALSE
  )
  expect_snapshot(table(study_data[[vn]], useNA = "always"))
  expect_snapshot(table(r$ModifiedStudyData[[vn]], useNA = "always"))
  # Use devtools::load_all() locally before continuing this manual scenario.

  study_data$v00002 <- ifelse(study_data$v00002 == "0", "females", "males")
  meta_data[meta_data$LABEL == "SEX_0", "VALUE_LABELS"] <- "females|males"
  rules <- tibble::tribble(
    ~resp_vars, ~CODE_CLASS, ~CODE_LABEL, ~CODE_VALUE, ~RULE,
    "PREGNANT_0", "JUMP", "No pregnancies in males", "9999", '[v00002]="males"',
  )
  r <- prep_add_missing_codes(NA,
    study_data = study_data, meta_data =
      meta_data,
    label_col = "LABEL", rules = rules,
    use_value_labels = TRUE, overwrite = FALSE
  )
  expect_snapshot(table(study_data[[vn]], useNA = "always"))
  expect_snapshot(table(r$ModifiedStudyData[[vn]], useNA = "always"))
})
