skip_on_cran()

test_that("util_validate_known_meta works", {
  skip_on_cran() # slow
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  local({
    meta_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data.RData") # nolint: line_length_linter.
    md1 <- meta_data
    expect_silent(util_validate_known_meta(meta_data = md1))
    md1$VAR_NAMES <- NULL
    expect_silent(util_validate_known_meta(meta_data = md1))
    md1 <- meta_data
    md1$HARD_LIMITS[[10]] <- "[9; 1]"
    expect_warning(util_validate_known_meta(meta_data = md1),
      regexp = sprintf(
        "(%s|%s)",
        paste(
          "Invalid limits detected: Found HARD with",
          "lower limit > upper limit: \\[9; 1\\]"
        ),
        paste("Some code labels or -values are missing from .+meta_data.+")
      ),
      perl = TRUE
    )
    md1 <- meta_data
    md1$VALUE_LABELS[[53]] <- "0 = no"
    expect_warning(util_validate_known_meta(meta_data = md1),
      regexp = sprintf(
        "(%s|%s)",
        paste(
          "Suspicious value labels",
          ".only 1 level. detected:",
          ".+0 = no.+"
        ),
        paste(
          "Some code labels or -values are",
          "missing from .+meta_data.+"
        )
      ),
      perl = TRUE
    )
    md1 <- meta_data
    md1$MISSING_LIST[[44]] <- "x| 9"

    expect_warning(
      expect_warning(
        util_validate_known_meta(meta_data = md1),
        regexp = paste("Some missing codes are not numeric"),
        perl = TRUE
      ),
      regexp = paste(
        "Suspicious .+MISSING_LIST.+:",
        "not numeric/date/time/assignment"
      ),
      perl = TRUE
    )

    md1 <- meta_data
    md1$MISSING_LIST[[44]] <- list(1:10)
    expect_warning(
      expect_warning(
        util_validate_known_meta(meta_data = md1),
        regexp = paste("Some missing codes are not numeric"),
        perl = TRUE
      ),
      regexp = paste(
        "Suspicious .+MISSING_LIST.+:",
        "not numeric/date/time/assignment"
      ),
      perl = TRUE
    )

    md1 <- meta_data
    md1$MISSING_LIST[[44]] <- paste(md1$MISSING_LIST[[44]],
      md1$MISSING_LIST[[44]],
      collapse = SPLIT_CHAR
    )
    expect_warning(
      expect_warning(
        util_validate_known_meta(meta_data = md1),
        regexp = paste("Some missing codes are not numeric"),
        perl = TRUE
      ),
      regexp = paste("Duplicates in"),
      perl = TRUE
    )
  })
})

test_that("util_validate_known_meta can restrict hard duplicate checks", {
  meta_data <- data.frame(
    VAR_NAMES = c("a", "b", "extra", "extra"),
    DATA_TYPE = DATA_TYPES$FLOAT,
    LABEL = c("A", "B", "Extra 1", "Extra 2"),
    LONG_LABEL = c("A", "B", "Extra 1", "Extra 2"),
    MISSING_LIST = "",
    stringsAsFactors = FALSE
  )

  expect_error(
    util_validate_known_meta(meta_data),
    "Found duplicated"
  )
  expect_silent(
    util_validate_known_meta(meta_data, relevant_var_names = c("a", "b"))
  )

  meta_data <- data.frame(
    VAR_NAMES = c("a", "a", "extra_1", "extra_2"),
    DATA_TYPE = DATA_TYPES$FLOAT,
    LABEL = c("A 1", "A 2", "Extra 1", "Extra 2"),
    LONG_LABEL = c("A 1", "A 2", "Extra 1", "Extra 2"),
    MISSING_LIST = "",
    stringsAsFactors = FALSE
  )

  expect_error(
    util_validate_known_meta(meta_data, relevant_var_names = "a"),
    "Found duplicated"
  )

  meta_data <- data.frame(
    VAR_NAMES = c("a", "b", "extra_1", "extra_2"),
    DATA_TYPE = DATA_TYPES$FLOAT,
    LABEL = c("A", "B", "Extra", "Extra"),
    LONG_LABEL = c("A", "B", "Extra 1", "Extra 2"),
    MISSING_LIST = "",
    stringsAsFactors = FALSE
  )

  expect_error(
    util_validate_known_meta(meta_data),
    "Found duplicated"
  )
  expect_silent(
    util_validate_known_meta(meta_data, relevant_var_names = c("a", "b"))
  )
})

test_that("util_validate_known_meta removes identical duplicate rows", {
  meta_data <- data.frame(
    VAR_NAMES = c("a", "a"),
    DATA_TYPE = DATA_TYPES$FLOAT,
    LABEL = c("A", "A"),
    LONG_LABEL = c("A", "A"),
    MISSING_LIST = "",
    stringsAsFactors = FALSE
  )

  expect_silent(
    meta_data <- util_validate_known_meta(meta_data, relevant_var_names = "a")
  )
  expect_equal(nrow(meta_data), 1)
  expect_equal(meta_data[[VAR_NAMES]], "a")
})

test_that(
  "util_validate_known_meta repairs names and reports value-label tables",
  {
    skip_on_cran()

    meta_data_without_names <- data.frame(
      LABEL = c("", "Named"),
      DATA_TYPE = DATA_TYPES$INTEGER,
      MISSING_LIST = "",
      stringsAsFactors = FALSE
    )

    expect_silent(
      amended <- util_validate_known_meta(meta_data_without_names)
    )
    expect_equal(amended[[VAR_NAMES]], c("v1", "v2"))

    meta_data_with_empty_name <- data.frame(
      VAR_NAMES = c("", "named"),
      LABEL = c("Dummy", "Named"),
      DATA_TYPE = DATA_TYPES$INTEGER,
      MISSING_LIST = "",
      stringsAsFactors = FALSE
    )

    expect_warning(
      amended <- util_validate_known_meta(meta_data_with_empty_name),
      "Found variables w/o"
    )
    expect_equal(amended[[VAR_NAMES]], c("v1", "named"))

    expect_error(
      util_validate_relevant_meta_uniqueness(
        data.frame(VAR_NAMES = "a", stringsAsFactors = FALSE),
        relevant_var_names = 1
      ),
      "relevant_var_names"
    )

    prep_purge_data_frame_cache()
    withr::defer(prep_purge_data_frame_cache())

    prep_add_data_frames(
      no_code_value_table = data.frame(
        CODE_LABEL = c("No", "Yes"),
        stringsAsFactors = FALSE
      ),
      empty_code_value_table = data.frame(
        CODE_VALUE = character(),
        stringsAsFactors = FALSE
      ),
      one_code_value_table = data.frame(
        CODE_VALUE = "1",
        stringsAsFactors = FALSE
      ),
      ordered_but_nominal_table = data.frame(
        CODE_VALUE = c("1", "2"),
        CODE_ORDER = c(1, 2),
        stringsAsFactors = FALSE
      ),
      unordered_but_ordinal_table = data.frame(
        CODE_VALUE = c("1", "2"),
        stringsAsFactors = FALSE
      )
    )

    meta_data <- data.frame(
      VAR_NAMES = paste0("v", seq_len(5)),
      DATA_TYPE = DATA_TYPES$INTEGER,
      VALUE_LABEL_TABLE = c(
        "no_code_value_table",
        "empty_code_value_table",
        "one_code_value_table",
        "ordered_but_nominal_table",
        "unordered_but_ordinal_table"
      ),
      SCALE_LEVEL = c(
        SCALE_LEVELS$NOMINAL,
        SCALE_LEVELS$NOMINAL,
        SCALE_LEVELS$NOMINAL,
        SCALE_LEVELS$NOMINAL,
        SCALE_LEVELS$ORDINAL
      ),
      VALUE_LABELS = "",
      MISSING_LIST = "",
      stringsAsFactors = FALSE
    )

    warning_env <- new.env(parent = emptyenv())
    warning_env$messages <- character(0)
    withCallingHandlers(
      util_validate_known_meta(meta_data),
      warning = function(w) {
        warning_env$messages <- c(warning_env$messages, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    )

    expect_length(warning_env$messages, 1)
    expect_true(grepl("missing column", warning_env$messages, fixed = TRUE))
    expect_true(grepl("empty for", warning_env$messages, fixed = TRUE))
    expect_true(grepl("only one value", warning_env$messages, fixed = TRUE))
    expect_true(grepl("Ordinal variable with unordered", warning_env$messages,
        fixed = TRUE
      ))
    expect_true(grepl("Nominal variable with ordered", warning_env$messages,
        fixed = TRUE
      ))
  }
)

test_that(
  "util_abbreviate_unique keeps shortened unique labels distinguishable",
  {
    skip_on_cran()

    expect_equal(
      util_abbreviate_unique(
        c("abcdef first", "abcdef second", "beta long label"),
        max_value_label_len = 6
      ),
      c("abcdef", "abcde1", "beta l")
    )

    expect_equal(
      util_abbreviate_unique(
        c("same label", "same label"), max_value_label_len = 4
      ),
      c("same", "same")
    )
  }
)

test_that("util_validate_known_meta reports local metadata convention issues", {
  skip_on_cran()

  expect_equal(
    util_validate_relevant_meta_uniqueness(
      data.frame(LABEL = "A", stringsAsFactors = FALSE)
    ),
    data.frame(LABEL = "A", stringsAsFactors = FALSE)
  )

  meta_data <- data.frame(
    VAR_NAMES = c("missing_table", "invalid_scale", "bad_nominal_labels"),
    DATA_TYPE = DATA_TYPES$INTEGER,
    VALUE_LABEL_TABLE = c("does_not_exist_locally", "", ""),
    VALUE_LABELS = c("", "", "1 < 2"),
    SCALE_LEVEL = c(SCALE_LEVELS$NOMINAL, "not_a_scale", SCALE_LEVELS$NOMINAL),
    MISSING_LIST = "",
    stringsAsFactors = FALSE
  )

  warning_env <- new.env(parent = emptyenv())
  warning_env$messages <- character(0)
  withCallingHandlers(
    util_validate_known_meta(meta_data),
    warning = function(w) {
      warning_env$messages <- c(warning_env$messages, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  warnings <- paste(warning_env$messages, collapse = "\n")
  expect_match(warnings, "Cannot load")
  expect_match(warnings, "Found invalid scale levels")
  expect_match(warnings, "Value labels with")
  expect_match(warnings, "bad_nominal_labels")
})
