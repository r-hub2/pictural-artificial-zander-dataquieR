test_that("util_eval_rule evaluates simple rules for dataframe-like inputs", {
  skip_on_cran()

  rule <- util_parse_redcap_rule("[x] > 1", debug = 0)
  study_data <- data.frame(x = c(1, 2, 3))

  expect_equal(
    util_eval_rule(rule, ds1 = study_data, use_value_labels = FALSE),
    c(FALSE, TRUE, TRUE)
  )
  expect_equal(
    util_eval_rule(rule, ds1 = as.list(study_data), use_value_labels = FALSE),
    c(FALSE, TRUE, TRUE)
  )

  env <- list2env(as.list(study_data), parent = emptyenv())
  expect_equal(
    util_eval_rule(rule, ds1 = env, use_value_labels = FALSE),
    c(FALSE, TRUE, TRUE)
  )
})

test_that("util_eval_rule rejects unsupported input and missing-code modes", {
  skip_on_cran()

  rule <- util_parse_redcap_rule("[x] > 1", debug = 0)
  study_data <- data.frame(x = c(1, 2))

  expect_error(
    util_eval_rule(rule,
      ds1 = 1, meta_data = data.frame(),
      use_value_labels = FALSE
    ),
    "environment or a list"
  )

  meta_data <- data.frame(
    var_names = "x",
    data_type = DATA_TYPES$INTEGER,
    stringsAsFactors = FALSE
  )
  colnames(meta_data) <- c(VAR_NAMES, DATA_TYPE)

  expect_error(
    util_eval_rule(rule,
      ds1 = study_data, meta_data = meta_data,
      replace_missing_by = "LABEL", replace_limits = FALSE
    ),
    "not yet supported for numerical variables"
  )
})

test_that("SSI IRV and Mahalanobis account for reverse-coded items", {
  skip_on_cran()

  study_data <- data.frame(
    Q1 = c(1, 2, 3, 4),
    Q2 = c(5, 3, 4, 1)
  )
  meta_data <- data.frame(
    VAR_NAMES = c("Q1", "Q2"),
    DATA_TYPE = DATA_TYPES$INTEGER,
    VALUE_LABELS = "1 = low | 2 = low-mid | 3 = high-mid | 4 = high | 5 = top",
    MISSING_LIST = SPLIT_CHAR,
    JUMP_LIST = SPLIT_CHAR,
    ITEM_TYPE = c(NA, "REVERSE"),
    stringsAsFactors = FALSE
  )

  irv_rule <- util_parse_redcap_rule("IRV([Q1], [Q2])", debug = 0)
  expect_equal(
    util_eval_rule(irv_rule,
      ds1 = study_data,
      meta_data = meta_data,
      use_value_labels = FALSE
    ),
    apply(cbind(study_data$Q1, 6 - study_data$Q2), 1, sd)
  )

  mahal_rule <- util_parse_redcap_rule("MAHALANOBIS([Q1], [Q2])", debug = 0)
  expected_data <- cbind(Q1 = study_data$Q1, Q2 = 6 - study_data$Q2)
  expected <- mahalanobis(
    expected_data,
    colMeans(expected_data),
    cov(expected_data)
  )
  expect_equal(
    util_eval_rule(mahal_rule,
      ds1 = study_data,
      meta_data = meta_data,
      use_value_labels = FALSE
    ),
    expected
  )
})

test_that("SSI IRV rejects attention-check items", {
  skip_on_cran()

  study_data <- data.frame(
    Q1 = c(1, 2, 1),
    Q2 = c(1, 2, 3)
  )
  meta_data <- data.frame(
    VAR_NAMES = c("Q1", "Q2"),
    DATA_TYPE = DATA_TYPES$INTEGER,
    MISSING_LIST = SPLIT_CHAR,
    JUMP_LIST = SPLIT_CHAR,
    ITEM_TYPE = c("BOGUS: 1|2", NA),
    stringsAsFactors = FALSE
  )
  irv_rule <- util_parse_redcap_rule("IRV([Q1], [Q2])", debug = 0)

  expect_error(
    util_eval_rule(irv_rule,
      ds1 = study_data,
      meta_data = meta_data,
      use_value_labels = FALSE
    ),
    "IRV cannot be calculated from attention-check variables"
  )
})

test_that("SSI IRV keeps metadata-free direct REDCap-env calls working", {
  skip_on_cran()

  irv <- util_get_redcap_rule_env()[["IRV"]]

  expect_equal(
    irv(c(1, 2, 3), c(2, 4, 6)),
    apply(cbind(c(1, 2, 3), c(2, 4, 6)), 1, sd)
  )
})

test_that("SSI row filtering preserves one-cell matrix dimensions", {
  skip_on_cran()

  rule_env <- util_get_redcap_rule_env()
  cases_na_below <- rule_env[["CASES_NA_BELOW"]]
  irv <- rule_env[["IRV"]]

  expect_identical(dim(cases_na_below(100, 1)), c(1L, 1L))
  expect_identical(dim(cases_na_below(0, NA_real_)), c(1L, 1L))
  expect_true(is.na(cases_na_below(0, NA_real_)[[1]]))
  expect_true(is.na(irv(1)))
  expect_identical(dim(cases_na_below(100)), c(0L, 0L))
})

test_that("SSI psychometric indices require complete selected item pairs", {
  skip_on_cran()

  syn <- util_get_redcap_rule_env()[["PSYCHOMETRIC_SYN"]]
  ant <- util_get_redcap_rule_env()[["PSYCHOMETRIC_ANT"]]
  a <- 1:5
  b <- 3:7
  c <- c(5, 6, NA, 8, 9)
  d <- 7:11

  expect_warning(
    expect_equal(syn(a, b, c, d), c(0.5, 0.5, NA, 0.5, 0.5)),
    "Only '6' pairs"
  )
  expect_warning(
    expect_equal(
      ant(a, 10 - a, c(19, 18, NA, 16, 15), a + 1),
      c(-0.7568358, -0.6868296, NA, -0.4551793, -0.2842676),
      tolerance = 1e-6
    ),
    "Only '4' pairs"
  )
})

test_that("util_eval_rule covers replacement and alias edge paths", {
  skip_on_cran()

  rule <- util_parse_redcap_rule("[x] > 1", debug = 0)
  study_data <- data.frame(x = c(1, 2, 3))

  expect_message(
    result <- util_eval_rule(rule,
      ds1 = study_data, meta_data = data.frame(),
      replace_missing_by = "", replace_limits = TRUE,
      use_value_labels = FALSE
    ),
    regexp = "Cannot replace hard limits"
  )
  expect_equal(result, c(FALSE, TRUE, TRUE))

  eval_in_parent <- function() {
    x <- c(1, 2, 3)
    util_eval_rule(rule, meta_data = data.frame(), use_value_labels = FALSE)
  }
  expect_equal(eval_in_parent(), c(FALSE, TRUE, TRUE))

  meta_data <- data.frame(
    var_names = "x",
    label = "Label X",
    long_label = "Long Label X",
    original_var_names = "orig_x",
    original_label = "Original Label X",
    stringsAsFactors = FALSE
  )
  names(meta_data) <- c(
    VAR_NAMES, LABEL, LONG_LABEL, "ORIGINAL_VAR_NAMES",
    "ORIGINAL_LABEL"
  )

  aliased <- util_add_redcap_rule_label_aliases(
    ds1 = study_data,
    meta_data = meta_data,
    label_col = VAR_NAMES
  )
  expect_true(all(c(
    "Label X", "Long Label X", "orig_x", "Original Label X"
  ) %in%
    colnames(aliased)))

  expect_equal(
    util_eval_prepared_redcap_rule(
      util_parse_redcap_rule("[Label X] > 1", debug = 0),
      ds1 = study_data,
      meta_data = meta_data,
      label_col = VAR_NAMES
    ),
    c(FALSE, TRUE, TRUE)
  )
})

test_that("util_eval_rule evaluates value-label table labels", {
  skip_on_cran()

  result <- with_dataframe_environment(quote({
    prep_add_data_frames(
      value_codes = data.frame(
        CODE_VALUE = c("1", "2"),
        CODE_LABEL = c("No", "Yes"),
        stringsAsFactors = FALSE
      )
    )

    study_data <- data.frame(x = c("1", "2", "1"), stringsAsFactors = FALSE)
    meta_data <- data.frame(
      var_names = "x",
      data_type = DATA_TYPES$STRING,
      scale_level = SCALE_LEVELS$NOMINAL,
      value_label_table = "value_codes",
      missing_list = SPLIT_CHAR,
      jump_list = SPLIT_CHAR,
      hard_limits = "",
      stringsAsFactors = FALSE
    )
    names(meta_data) <- c(
      VAR_NAMES, DATA_TYPE, SCALE_LEVEL,
      VALUE_LABEL_TABLE, MISSING_LIST, JUMP_LIST,
      HARD_LIMITS
    )

    util_eval_rule(
      util_parse_redcap_rule("[x] = \"Yes\"", debug = 0),
      ds1 = study_data,
      meta_data = meta_data,
      use_value_labels = TRUE,
      replace_limits = FALSE,
      replace_missing_by = ""
    )
  }))

  expect_equal(result, c(FALSE, TRUE, FALSE))
})

test_that(
  "util_eval_rule falls back to codes in incomplete value-label tables",
  {
    skip_on_cran()

    result <- with_dataframe_environment(quote({
      prep_add_data_frames(
        value_codes = data.frame(CODE_VALUE = c("1", "2"))
      )

      study_data <- data.frame(x = c("1", "2", "1"), stringsAsFactors = FALSE)
      attr(study_data, "MAPPED") <- TRUE
      attr(study_data, "Codes_to_NA") <- FALSE
      attr(study_data, "HL_viol_to_NA") <- FALSE
      attr(study_data, "Data_type_matches") <- TRUE
      attr(study_data, "label_col") <- VAR_NAMES
      meta_data <- data.frame(
        var_names = "x",
        label = "x",
        data_type = DATA_TYPES$STRING,
        scale_level = SCALE_LEVELS$NOMINAL,
        value_label_table = "value_codes",
        missing_list = SPLIT_CHAR,
        jump_list = SPLIT_CHAR,
        hard_limits = "",
        stringsAsFactors = FALSE
      )
      names(meta_data) <- c(
        VAR_NAMES, LABEL, DATA_TYPE, SCALE_LEVEL,
        VALUE_LABEL_TABLE, MISSING_LIST, JUMP_LIST,
        HARD_LIMITS
      )

      util_eval_rule(
        util_parse_redcap_rule("[x] = \"2\"", debug = 0),
        ds1 = study_data,
        meta_data = meta_data,
        use_value_labels = TRUE,
        replace_limits = FALSE,
        replace_missing_by = ""
      )
    }))

    expect_equal(result, c(FALSE, TRUE, FALSE))
  }
)

test_that(
  "util_eval_rule prioritizes missing-code interpretations over value labels",
  {
    skip_on_cran()

    result <- with_dataframe_environment(quote({
      prep_add_data_frames(
        value_codes = data.frame(
          CODE_VALUE = c("1", "9"),
          CODE_LABEL = c("Observed", "Missing code"),
          stringsAsFactors = FALSE
        ),
        missing_codes = data.frame(
          CODE_VALUE = "9",
          CODE_INTERPRET = "Unit nonresponse",
          stringsAsFactors = FALSE
        )
      )

      study_data <- data.frame(x = c("1", "9", "1"), stringsAsFactors = FALSE)
      attr(study_data, "MAPPED") <- TRUE
      attr(study_data, "Codes_to_NA") <- FALSE
      attr(study_data, "HL_viol_to_NA") <- FALSE
      attr(study_data, "Data_type_matches") <- TRUE
      attr(study_data, "label_col") <- VAR_NAMES
      meta_data <- data.frame(
        var_names = "x",
        label = "x",
        data_type = DATA_TYPES$STRING,
        scale_level = SCALE_LEVELS$NOMINAL,
        value_label_table = "value_codes",
        missing_list_table = "missing_codes",
        missing_list = "9=Missing code",
        jump_list = SPLIT_CHAR,
        hard_limits = "",
        stringsAsFactors = FALSE
      )
      names(meta_data) <- c(
        VAR_NAMES, LABEL, DATA_TYPE, SCALE_LEVEL,
        VALUE_LABEL_TABLE, MISSING_LIST_TABLE, MISSING_LIST,
        JUMP_LIST, HARD_LIMITS
      )

      expect_message(
        result <- util_eval_rule(
          util_parse_redcap_rule("[x] = \"Unit nonresponse\"", debug = 0),
          ds1 = study_data,
          meta_data = meta_data,
          replace_limits = FALSE,
          replace_missing_by = "INTERPRET"
        ),
        "interpretation wins"
      )
      old_value_label <- NULL
      expect_message(
        old_value_label <- util_eval_rule(
          util_parse_redcap_rule("[x] = \"Missing code\"", debug = 0),
          ds1 = study_data,
          meta_data = meta_data,
          replace_limits = FALSE,
          replace_missing_by = "INTERPRET"
        ),
        "interpretation wins"
      )
      list(result = result, old_value_label = old_value_label)
    }))

    expect_equal(result$result, c(FALSE, TRUE, FALSE))
    expect_equal(result$old_value_label, c(FALSE, FALSE, FALSE))
  }
)

test_that("util_eval_rule falls back to codes for unusable missing tables", {
  skip_on_cran()

  result <- with_dataframe_environment(quote({
    prep_add_data_frames(
      value_codes = data.frame(CODE_VALUE = c("1", "9")),
      incomplete_missing_codes = data.frame(CODE_VALUE = "9")
    )

    study_data <- data.frame(x = c("1", "9", "1"), stringsAsFactors = FALSE)
    attr(study_data, "MAPPED") <- TRUE
    attr(study_data, "Codes_to_NA") <- FALSE
    attr(study_data, "HL_viol_to_NA") <- FALSE
    attr(study_data, "Data_type_matches") <- TRUE
    attr(study_data, "label_col") <- VAR_NAMES
    meta_data <- data.frame(
      var_names = "x",
      label = "x",
      data_type = DATA_TYPES$STRING,
      scale_level = SCALE_LEVELS$NOMINAL,
      value_label_table = "value_codes",
      missing_list_table = "incomplete_missing_codes",
      missing_list = "9=Missing code",
      jump_list = SPLIT_CHAR,
      hard_limits = "",
      stringsAsFactors = FALSE
    )
    names(meta_data) <- c(
      VAR_NAMES, LABEL, DATA_TYPE, SCALE_LEVEL,
      VALUE_LABEL_TABLE, MISSING_LIST_TABLE, MISSING_LIST,
      JUMP_LIST, HARD_LIMITS
    )

    result <- NULL
    suppressWarnings(
      result <- util_eval_rule(
        util_parse_redcap_rule("[x] = \"9\"", debug = 0),
        ds1 = study_data,
        meta_data = meta_data,
        replace_limits = FALSE,
        replace_missing_by = "INTERPRET"
      )
    )
    result
  }))

  expect_equal(result, c(FALSE, TRUE, FALSE))
})

test_that("util_eval_rule can explicitly keep raw codes", {
  skip_on_cran()

  result <- with_dataframe_environment(quote({
    prep_add_data_frames(
      value_codes = data.frame(
        CODE_VALUE = c("1", "2"),
        CODE_LABEL = c("No", "Yes"),
        stringsAsFactors = FALSE
      )
    )

    study_data <- data.frame(x = c("1", "2", "1"), stringsAsFactors = FALSE)
    attr(study_data, "MAPPED") <- TRUE
    attr(study_data, "Codes_to_NA") <- FALSE
    attr(study_data, "HL_viol_to_NA") <- FALSE
    attr(study_data, "Data_type_matches") <- TRUE
    attr(study_data, "label_col") <- VAR_NAMES
    meta_data <- data.frame(
      var_names = "x",
      label = "x",
      data_type = DATA_TYPES$STRING,
      scale_level = SCALE_LEVELS$NOMINAL,
      value_label_table = "value_codes",
      missing_list = SPLIT_CHAR,
      jump_list = SPLIT_CHAR,
      hard_limits = "",
      stringsAsFactors = FALSE
    )
    names(meta_data) <- c(
      VAR_NAMES, LABEL, DATA_TYPE, SCALE_LEVEL,
      VALUE_LABEL_TABLE, MISSING_LIST, JUMP_LIST,
      HARD_LIMITS
    )

    util_eval_rule(
      util_parse_redcap_rule("[x] = \"2\"", debug = 0),
      ds1 = study_data,
      meta_data = meta_data,
      use_value_labels = FALSE,
      replace_limits = FALSE,
      replace_missing_by = ""
    )
  }))

  expect_equal(result, c(FALSE, TRUE, FALSE))
})

test_that("util_eval_rule can use ordinary missing labels", {
  skip_on_cran()

  result <- with_dataframe_environment(quote({
    prep_add_data_frames(
      value_codes = data.frame(
        CODE_VALUE = "1",
        CODE_LABEL = "Observed",
        stringsAsFactors = FALSE
      )
    )

    study_data <- data.frame(x = c("1", "9", "1"), stringsAsFactors = FALSE)
    attr(study_data, "MAPPED") <- TRUE
    attr(study_data, "Codes_to_NA") <- FALSE
    attr(study_data, "HL_viol_to_NA") <- FALSE
    attr(study_data, "Data_type_matches") <- TRUE
    attr(study_data, "label_col") <- VAR_NAMES
    meta_data <- data.frame(
      var_names = "x",
      label = "x",
      data_type = DATA_TYPES$STRING,
      scale_level = SCALE_LEVELS$NOMINAL,
      value_label_table = "value_codes",
      missing_list = "9=Missing code",
      jump_list = SPLIT_CHAR,
      hard_limits = "",
      stringsAsFactors = FALSE
    )
    names(meta_data) <- c(
      VAR_NAMES, LABEL, DATA_TYPE, SCALE_LEVEL,
      VALUE_LABEL_TABLE, MISSING_LIST, JUMP_LIST,
      HARD_LIMITS
    )

    util_eval_rule(
      util_parse_redcap_rule("[x] = \"Missing code\"", debug = 0),
      ds1 = study_data,
      meta_data = meta_data,
      replace_limits = FALSE,
      replace_missing_by = "LABEL"
    )
  }))

  expect_equal(result, c(FALSE, TRUE, FALSE))
})

test_that(
  "util_eval_rule accepts missing-code interpretation without a table",
  {
    skip_on_cran()

    result <- with_dataframe_environment(quote({
      prep_add_data_frames(
        value_codes = data.frame(
          CODE_VALUE = c("1", "9"),
          CODE_LABEL = c("Observed", "Missing code"),
          stringsAsFactors = FALSE
        )
      )

      study_data <- data.frame(x = c("1", "9", "1"), stringsAsFactors = FALSE)
      attr(study_data, "MAPPED") <- TRUE
      attr(study_data, "Codes_to_NA") <- FALSE
      attr(study_data, "HL_viol_to_NA") <- FALSE
      attr(study_data, "Data_type_matches") <- TRUE
      attr(study_data, "label_col") <- VAR_NAMES
      meta_data <- data.frame(
        var_names = "x",
        label = "x",
        data_type = DATA_TYPES$STRING,
        scale_level = SCALE_LEVELS$NOMINAL,
        value_label_table = "value_codes",
        missing_list = SPLIT_CHAR,
        jump_list = SPLIT_CHAR,
        hard_limits = "",
        stringsAsFactors = FALSE
      )
      names(meta_data) <- c(
        VAR_NAMES, LABEL, DATA_TYPE, SCALE_LEVEL,
        VALUE_LABEL_TABLE, MISSING_LIST, JUMP_LIST,
        HARD_LIMITS
      )

      util_eval_rule(
        util_parse_redcap_rule("[x] = \"Missing code\"", debug = 0),
        ds1 = study_data,
        meta_data = meta_data,
        replace_limits = FALSE,
        replace_missing_by = "INTERPRET"
      )
    }))

    expect_equal(result, c(FALSE, TRUE, FALSE))
  }
)

test_that("util_eval_rule aliases active labels and parses character results", {
  skip_on_cran()

  study_data <- data.frame(
    `Label X` = c(1, 2, 3),
    check.names = FALSE
  )
  meta_data <- data.frame(
    var_names = "x",
    label = "Label X",
    long_label = "Long Label X",
    original_var_names = "orig_x",
    original_label = "Original Label X",
    stringsAsFactors = FALSE
  )
  names(meta_data) <- c(
    VAR_NAMES, LABEL, LONG_LABEL, "ORIGINAL_VAR_NAMES",
    "ORIGINAL_LABEL"
  )

  aliased <- util_add_redcap_rule_label_aliases(
    ds1 = study_data,
    meta_data = meta_data,
    label_col = LABEL
  )

  expect_true(all(c("x", "Long Label X", "orig_x", "Original Label X") %in%
        colnames(aliased)))
  expect_equal(aliased$x, study_data[["Label X"]])

  expect_equal(
    util_eval_prepared_redcap_rule(
      util_parse_redcap_rule("[x] > 1", debug = 0),
      ds1 = study_data,
      meta_data = meta_data,
      label_col = LABEL
    ),
    c(FALSE, TRUE, TRUE)
  )

  expect_equal(
    util_eval_prepared_redcap_rule(
      quote("1"),
      ds1 = data.frame(row.names = 1),
      meta_data = data.frame(),
      label_col = VAR_NAMES
    ),
    1
  )

  expect_error(
    util_eval_prepared_redcap_rule(
      quote(TRUE),
      ds1 = 1,
      meta_data = data.frame(),
      label_col = VAR_NAMES
    ),
    "environment or a list"
  )
})
