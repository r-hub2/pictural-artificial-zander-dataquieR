test_that("util_correct_variable_use works", {
  skip_on_cran() # slow and implicitly tested by other tests
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  acc_test <- function(resp_variable, aux_variable, time_variable,
    co_variables, group_vars, study_data, meta_data,
    label_col) {
    prep_prepare_dataframes()
    util_correct_variable_use(resp_variable)
    util_correct_variable_use("resp_variable")
    util_correct_variable_use("aux_variable")
    util_correct_variable_use("time_variable")
    if (missing(co_variables)) {
      co_variables <- NA
      warning("No co_variables were defined")
    }
    util_correct_variable_use("co_variables",
      allow_na = TRUE,
      allow_more_than_one = TRUE
    )
    util_correct_variable_use2(group_vars)
    co_variables <- na.omit(co_variables)
  }
  environment(acc_test) <- asNamespace("dataquieR")
  acc_test2 <- function(resp_variable, aux_variable, time_variable,
    co_variables, group_vars, study_data, meta_data,
    label_col) {
    prep_prepare_dataframes()
    expect_error(util_correct_variable_use(c(a, b)),
      regexp =
        paste(
          "argument arg_name must be of length 1, wrong use",
          "of util_correct_variable_use."
        )
    )
    expect_error(util_correct_variable_use(character(0)),
      regexp =
        paste(
          "argument arg_name must be of length 1, wrong use",
          "of util_correct_variable_use."
        )
    )

    expect_error(util_correct_variable_use(resp_variable, role = "invalid"),
      regexp =
        paste(
          "Unknown variable-argument role: invalid for",
          "argument resp_variable, wrong use",
          "of util_correct_variable_use."
        )
    )

    expect_error(util_correct_variable_use(invalid),
      regexp =
        paste(
          "Unknown function argument invalid checked,",
          "wrong use",
          "of util_correct_variable_use."
        )
    )
    resp_variable <- "v00000"
    label_col <- VAR_NAMES
    expect_silent(util_correct_variable_use(resp_variable))
    label_col <- "XXX"
    expect_silent(util_correct_variable_use(resp_variable))
    rm(label_col)
    expect_silent(util_correct_variable_use(resp_variable))
    delayedAssign("label_col", stop())
    expect_silent(util_correct_variable_use(resp_variable))
    label_col <- VAR_NAMES
  }
  environment(acc_test2) <- asNamespace("dataquieR")
  acc_test3 <- function(resp_variable, aux_variable, time_variable,
    co_variables, group_vars, study_data, meta_data) {
    prep_prepare_dataframes()
    expect_error(util_correct_variable_use(c(a, b)),
      regexp =
        paste(
          "argument arg_name must be of length 1, wrong use",
          "of util_correct_variable_use."
        )
    )
    expect_error(util_correct_variable_use(character(0)),
      regexp =
        paste(
          "argument arg_name must be of length 1, wrong use",
          "of util_correct_variable_use."
        )
    )

    expect_error(util_correct_variable_use(resp_variable, role = "invalid"),
      regexp =
        paste(
          "Unknown variable-argument role: invalid for",
          "argument resp_variable, wrong use",
          "of util_correct_variable_use."
        )
    )

    expect_error(util_correct_variable_use(invalid),
      regexp =
        paste(
          "Unknown function argument invalid checked,",
          "wrong use",
          "of util_correct_variable_use."
        )
    )
    resp_variable <- "v00000"
    expect_silent(util_correct_variable_use(resp_variable))
  }
  environment(acc_test3) <- asNamespace("dataquieR")
  acc_test_type <-
    function(resp_variable, aux_variable, time_variable, co_variables,
      group_vars, study_data, meta_data, need_type,
      label_col) {
      prep_prepare_dataframes()
      util_correct_variable_use(resp_variable,
        need_type = need_type,
        allow_more_than_one = TRUE
      )
    }
  environment(acc_test_type) <- asNamespace("dataquieR")
  meta_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data.RData") # nolint: line_length_linter.
  study_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData", keep_types = TRUE) # nolint: line_length_linter.
  meta_data2 <-
    prep_scalelevel_from_data_and_metadata(
      study_data = study_data,
      meta_data = meta_data
    )
  meta_data[[SCALE_LEVEL]] <-
    setNames(meta_data2[[SCALE_LEVEL]], nm = meta_data2[[VAR_NAMES]])[
      meta_data[[VAR_NAMES]]
    ]
  expect_warning(
    expect_error(
      acc_test(study_data = study_data, meta_data = meta_data),
      regexp = "Argument resp_variable is NULL"
    ),
    regexp = paste(
      "Missing argument .+resp_variable.+ without default value.",
      "Setting to NULL. As a dataquieR developer"
    )
  )
  expect_error(
    acc_test("v00001"),
    regexp = "Need study data as a data frame"
  )
  expect_warning(
    expect_error(
      acc_test("v00001", study_data = study_data, meta_data = meta_data),
      regexp = paste(
        "Argument aux_variable is NULL"
      ),
      perl = TRUE
    ),
    regexp = paste(
      "Missing argument .+aux_variable.+ without default value. Setting",
      "to NULL. As a dataquieR developer,"
    ),
    perl = TRUE
  )
  expect_warning(
    expect_error(
      acc_test("v00001", "v00001",
        study_data = study_data,
        meta_data = meta_data
      ),
      regexp = paste(
        "Argument time_variable is NULL"
      ),
      perl = TRUE
    ),
    regexp = paste(
      "Missing argument .+time_variable.+ without default value.",
      "Setting to NULL. As a dataquieR developer,"
    ),
    perl = TRUE
  )

  expect_error(
    acc_test("v00001", "v00001", "v00001",
      study_data = study_data,
      meta_data = meta_data, co_variables = 1:10
    ),
    regexp = paste(
      "Need character variable names in argument co_variables"
    ),
    perl = TRUE
  )

  expect_warning(
    expect_error(
      acc_test("v00001", "v00001", "v00001", "v00001",
        study_data = study_data,
        meta_data = meta_data
      ),
      regexp = paste(
        "Argument group_vars is NULL"
      ),
      perl = TRUE
    ),
    regexp = paste(
      "Missing argument .+group_vars.+ without default value.",
      "Setting to NULL. As a dataquieR developer,"
    ),
    perl = TRUE
  )

  expect_error(
    acc_test("v00001", "v00001", "v00001", "v00001",
      study_data = study_data, meta_data = meta_data,
      group_vars = c()
    ),
    regexp = paste(
      "Argument group_vars is NULL"
    ),
    perl = TRUE
  )

  expect_silent(
    acc_test("v00001", "v00001", "v00001", "v00001",
      study_data = study_data, meta_data = meta_data,
      group_vars = c("v00001")
    )
  )

  expect_error(
    acc_test("v00001", "v00001", "v00001", "v00001",
      study_data = study_data, meta_data = meta_data,
      group_vars = c("v00001", "v00002")
    ),
    regexp = paste(
      "Variable .+v00002.+ \\(group_vars\\) has NA observations,",
      "which is not allowed"
    ),
    perl = TRUE
  )

  expect_silent(
    acc_test("v00001", "v00001", "v00001", "v00001",
      study_data = study_data, meta_data = meta_data,
      group_vars = c("v00001", "v00000")
    )
  )

  expect_silent(
    acc_test_type(c("v00000", "v00001"),
      study_data = study_data,
      meta_data = meta_data, need_type = "integer|string"
    )
  )

  expect_error(
    acc_test_type(c("v00000", "v00001"),
      study_data = study_data,
      meta_data = meta_data, need_type = "integer|float"
    ),
    regexp = paste(
      "Argument .+resp_variable.+: Variable .+v00001.+ \\(string\\) does",
      "not have an allowed type \\(integer\\|float\\)"
    ),
    perl = TRUE
  )

  expect_error(
    acc_test_type(c("v00000", "v00001"),
      study_data = study_data,
      meta_data = meta_data, need_type = "integer|xxx"
    ),
    regexp = paste(
      "Internal error: .+resp_variable.+s .+need_type.+ contains invalid type",
      "names .+xxx.+allowed are .+integer.+,",
      ".+string.+, .+float.+, .+datetime.+.",
      "As a dataquieR developer, you should fix your call of",
      ".+acc_test_type+."
    ),
    perl = TRUE
  )

  expect_error(
    acc_test_type(c("v00000", "v00001"),
      study_data = study_data,
      meta_data = meta_data, need_type = "string"
    ),
    regexp = paste(
      "Argument .+resp_variable.+: Variable .+v00000.+ \\(integer\\) does",
      "not have an allowed type \\(string\\)"
    ),
    perl = TRUE
  )

  expect_error(
    acc_test_type(c("v00000", "v00001"),
      study_data = study_data,
      meta_data = meta_data, need_type = "!integer"
    ),
    regexp = paste(
      "Argument .+resp_variable.+: Variable .+v00000.+ \\(integer\\)",
      "has a disallowed type \\(.+integer.+\\)"
    ),
    perl = TRUE
  )

  expect_silent(
    acc_test_type(c("v00000", "v00001"),
      study_data = study_data,
      meta_data = meta_data, need_type = "!datetime"
    )
  )

  acc_test2("v00001", "v00001", "v00001", "v00001",
    study_data = study_data, meta_data = meta_data,
    group_vars = c("v00001", "v00000")
  )
  acc_test3("v00001", "v00001", "v00001", "v00001",
    study_data = study_data, meta_data = meta_data,
    group_vars = c("v00001", "v00000")
  )

  acc_test4 <- function(resp_variable, aux_variable, time_variable,
    co_variables, group_vars, study_data, meta_data,
    label_col) {
    prep_prepare_dataframes()
    util_correct_variable_use(resp_variable)
  }
  environment(acc_test4) <- asNamespace("dataquieR")
  delayedAssign("resp_variable", stop("Error"))

  suppressWarnings(expect_warning(expect_warning(
    expect_error(
      acc_test4(
        resp_variable = resp_variable,
        "v00001", "v00001", "v00001",
        study_data = study_data, meta_data = meta_data,
        group_vars = c("v00001", "v00000")
      ),
      regexp = "Argument resp_variable is NULL"
    ),
    regexp = paste(
      "Could not get value of argument",
      "resp_variable for unexpected reasons.",
      "Setting to NULL."
    ),
    perl = TRUE
  ), regexp = paste("Error")))

  resp_variable <- "v00001"

  acc_test5 <- function(resp_variable, aux_variable, time_variable,
    co_variables, group_vars, study_data, meta_data,
    label_col) {
    util_correct_variable_use(resp_variable)
  }
  environment(acc_test5) <- asNamespace("dataquieR")
  expect_error(
    acc_test5(resp_variable = "v00001", meta_data = meta_data),
    regexp = paste(
      "Did not find merged study data and metadata ds1.",
      "Wrong use of util_correct_variable_use?"
    )
  )

  acc_test6 <- function(resp_variable, aux_variable, time_variable,
    co_variables, group_vars, study_data, meta_data,
    label_col, cmd, ...) {
    prep_prepare_dataframes()
    cmd()
    util_correct_variable_use(resp_variable, ...)
  }
  environment(acc_test6) <- asNamespace("dataquieR")
  expect_error(
    acc_test6(
      resp_variable = "v00001", study_data = study_data,
      meta_data = meta_data,
      cmd = function() {
        rm(meta_data, envir = parent.frame())
      }
    ),
    regexp = paste(
      "Did not find metadata.",
      "Wrong use of util_correct_variable_use?"
    )
  )
  expect_error(
    acc_test6(
      resp_variable = "v00001", study_data = study_data,
      meta_data = meta_data,
      cmd = function() {
        assign("meta_data", 42, parent.frame())
      }
    ),
    regexp = paste(
      "meta_data does not provide a metadata data frame.",
      "Wrong use of util_correct_variable_use?"
    )
  )

  expect_error(
    acc_test6(
      resp_variable = "v00001", study_data = study_data,
      meta_data = meta_data,
      cmd = function() {
        assign("ds1", 42, parent.frame())
      }
    ),
    regexp = paste(
      "ds1 does not provide merged study data and metadata.",
      "Wrong use of util_correct_variable_use?"
    )
  )

  expect_error(
    acc_test6(
      resp_variable = character(0), study_data = study_data,
      meta_data = meta_data,
      cmd = function() {}, allow_more_than_one = TRUE
    ),
    regexp = paste("Need at least one element in argument resp_variable, got 0")
  )

  expect_error(
    acc_test6(
      resp_variable = letters, study_data = study_data,
      meta_data = meta_data,
      cmd = function() {}
    ),
    regexp = paste(
      "Need exactly one element in argument resp_variable, got",
      "26: .a, b, c, d,.+x, y, z."
    ),
    perl = TRUE
  )

  md0 <- meta_data
  md0$DATA_TYPE <- NA
  expect_warning(
    acc_test6(
      resp_variable = "v00001", study_data = study_data,
      meta_data = md0,
      cmd = function() {}, need_type = "string"
    ),
    regexp = paste("predicted the.+DATA_TYPE"),
    perl = TRUE
  )

  expect_error(
    acc_test6(
      resp_variable = "v00000", study_data = study_data,
      meta_data = meta_data,
      cmd = function() {}, need_type = "string",
      allow_more_than_one = TRUE
    ),
    regexp = paste(
      "Argument .+resp_variable.+: Variable .+v00000.+ .integer.",
      "does not have an allowed type .string."
    ),
    perl = TRUE
  )
})

test_that("util_correct_variable_use covers local edge paths", {
  skip_on_cran()

  check_vars <- function(resp_vars, meta_data, ds1, study_data = ds1, ...) {
    util_correct_variable_use(
      "resp_vars",
      allow_more_than_one = TRUE,
      ...
    )
    resp_vars
  }
  environment(check_vars) <- asNamespace("dataquieR")

  meta_data <- data.frame(
    VAR_NAMES = c("v1", "v2", "v3"),
    LABEL = c("First variable", "Second variable", "Third variable"),
    LONG_LABEL = c("Long first", "Long second", "Long third"),
    DATA_TYPE = c(DATA_TYPES$INTEGER, DATA_TYPES$FLOAT, DATA_TYPES$INTEGER),
    SCALE_LEVEL = c(SCALE_LEVELS$RATIO, SCALE_LEVELS$NOMINAL,
      SCALE_LEVELS$RATIO),
    VARIABLE_ROLE = c(VARIABLE_ROLES$PRIMARY, VARIABLE_ROLES$SECONDARY,
      VARIABLE_ROLES$PRIMARY),
    COMPUTED_VARIABLE_ROLE = c(COMPUTED_VARIABLE_ROLES$`NA`,
      COMPUTED_VARIABLE_ROLES$`NA`,
      COMPUTED_VARIABLE_ROLES$RELCOMPL_SPEED),
    stringsAsFactors = FALSE
  )
  names(meta_data) <- c(VAR_NAMES, LABEL, LONG_LABEL, DATA_TYPE, SCALE_LEVEL,
    VARIABLE_ROLE, COMPUTED_VARIABLE_ROLE)
  ds1 <- data.frame(
    v1 = c(1L, 2L, 3L),
    v2 = c(1, NA, 3),
    v3 = c(NA_integer_, NA_integer_, NA_integer_)
  )

  expect_error(
    check_vars("study_only", meta_data, ds1,
      study_data = cbind(ds1, study_only = 1:3)),
    regexp = "study data not covered by metadata",
    perl = TRUE
  )

  expect_message(
    result <- check_vars("missing_var", meta_data, ds1,
      remove_not_found = TRUE, allow_null = TRUE),
    regexp = "Variable .+missing_var.+ not found in metadata"
  )
  expect_identical(result, character(0))

  result <- check_vars(c("First variable", "Long second"), meta_data, ds1)
  expect_identical(result, c("v1", "v2"))

  expect_warning(
    result <- check_vars(c("v1", "v2"), meta_data, ds1,
      need_scale = SCALE_LEVELS$RATIO,
      do_not_stop = TRUE),
    regexp = "variables .+v2.+ were excluded",
    perl = TRUE
  )
  expect_identical(result, "v1")

  expect_warning(
    result <- check_vars(c("v1", "v2"), meta_data, ds1,
      need_role = VARIABLE_ROLES$PRIMARY,
      do_not_stop = TRUE),
    regexp = "variables .+v2.+ were excluded",
    perl = TRUE
  )
  expect_identical(result, "v1")

  expect_warning(
    result <- check_vars(c("v1", "v3"), meta_data, ds1,
      need_computed_role =
        COMPUTED_VARIABLE_ROLES$`NA`,
      do_not_stop = TRUE),
    regexp = "variables .+v3.+ were excluded",
    perl = TRUE
  )
  expect_identical(result, "v1")
})

test_that("util_correct_variable_use rejects invalid requirement names", {
  skip_on_cran()

  check_var <- function(resp_var, meta_data, ds1, ...) {
    util_correct_variable_use("resp_var", ...)
  }
  environment(check_var) <- asNamespace("dataquieR")

  meta_data <- data.frame(
    VAR_NAMES = "v1",
    DATA_TYPE = DATA_TYPES$INTEGER,
    SCALE_LEVEL = SCALE_LEVELS$RATIO,
    VARIABLE_ROLE = VARIABLE_ROLES$PRIMARY,
    COMPUTED_VARIABLE_ROLE = COMPUTED_VARIABLE_ROLES$`NA`
  )
  names(meta_data) <- c(
    VAR_NAMES,
    DATA_TYPE,
    SCALE_LEVEL,
    VARIABLE_ROLE,
    COMPUTED_VARIABLE_ROLE
  )
  ds1 <- data.frame(v1 = 1L)

  expect_error(
    check_var("v1", meta_data, ds1, need_type = "not_a_type"),
    "invalid type names"
  )
  expect_error(
    check_var("v1", meta_data, ds1, need_type = "!not_a_type"),
    "invalid !type names"
  )
  expect_error(
    check_var("v1", meta_data, ds1, need_scale = "not_a_scale"),
    "invalid scale-level names"
  )
  expect_error(
    check_var("v1", meta_data, ds1, need_scale = "!not_a_scale"),
    "invalid !scale-level names"
  )
  expect_error(
    check_var("v1", meta_data, ds1, need_role = "not_a_role"),
    "invalid variable role names"
  )
  expect_error(
    check_var("v1", meta_data, ds1, need_role = "!not_a_role"),
    "invalid !variable role names"
  )
  expect_error(
    check_var(
      "v1",
      meta_data,
      ds1,
      need_computed_role = "not_a_computed_role"
    ),
    "invalid computed variable role names"
  )
  expect_error(
    check_var(
      "v1",
      meta_data,
      ds1,
      need_computed_role = "!not_a_computed_role"
    ),
    "invalid !computed variable role names"
  )
})

test_that("util_correct_variable_use excludes unmet data requirements", {
  skip_on_cran()

  check_vars <- function(resp_vars, meta_data, ds1, ...) {
    util_correct_variable_use(
      "resp_vars",
      allow_more_than_one = TRUE,
      do_not_stop = TRUE,
      ...
    )
    resp_vars
  }
  environment(check_vars) <- asNamespace("dataquieR")

  meta_data <- data.frame(
    VAR_NAMES = c("v1", "v2", "v3"),
    DATA_TYPE = c(DATA_TYPES$INTEGER, DATA_TYPES$FLOAT, DATA_TYPES$INTEGER),
    SCALE_LEVEL = c(SCALE_LEVELS$RATIO, SCALE_LEVELS$NOMINAL,
      SCALE_LEVELS$RATIO),
    VARIABLE_ROLE = c(VARIABLE_ROLES$PRIMARY, VARIABLE_ROLES$SECONDARY,
      VARIABLE_ROLES$PRIMARY),
    COMPUTED_VARIABLE_ROLE = c(COMPUTED_VARIABLE_ROLES$`NA`,
      COMPUTED_VARIABLE_ROLES$`NA`, COMPUTED_VARIABLE_ROLES$RELCOMPL_SPEED)
  )
  names(meta_data) <- c(
    VAR_NAMES,
    DATA_TYPE,
    SCALE_LEVEL,
    VARIABLE_ROLE,
    COMPUTED_VARIABLE_ROLE
  )
  ds1 <- data.frame(
    v1 = c(1L, 2L, 3L),
    v2 = c(1, NA, 1),
    v3 = c(NA_integer_, NA_integer_, NA_integer_)
  )

  expect_warning(
    expect_message(
      result <- check_vars(
        c("v1", "v3"),
        meta_data,
        ds1,
        allow_all_obs_na = FALSE
      ),
      "only NA observations"
    ),
    "excluded"
  )
  expect_identical(result, "v1")

  expect_warning(
    expect_message(
      result <- check_vars(
        c("v1", "v2"),
        meta_data,
        ds1,
        allow_any_obs_na = FALSE
      ),
      "NA observations"
    ),
    "excluded"
  )
  expect_identical(result, "v1")

  expect_warning(
    expect_message(
      result <- check_vars(
        c("v1", "v2"),
        meta_data,
        ds1,
        min_distinct_values = 3
      ),
      "fewer distinct values"
    ),
    "excluded"
  )
  expect_identical(result, "v1")

  expect_warning(
    result <- check_vars(c("v1", "v2"), meta_data, ds1,
      need_type = "!integer"),
    "excluded"
  )
  expect_identical(result, "v2")

  expect_warning(
    result <- check_vars(c("v1", "v2"), meta_data, ds1,
      need_role = "!primary"),
    "excluded"
  )
  expect_identical(result, "v2")

  expect_warning(
    result <- check_vars(c("v1", "v3"), meta_data, ds1,
      need_computed_role = "!na"),
    "excluded"
  )
  expect_identical(result, "v3")
})

test_that("util_correct_variable_use reports incomplete type metadata", {
  skip_on_cran()

  check_var <- function(resp_var, meta_data, ds1, ...) {
    util_correct_variable_use("resp_var", ...)
    resp_var
  }
  environment(check_var) <- asNamespace("dataquieR")

  meta_data <- data.frame(
    VAR_NAMES = "v1",
    LABEL = "v1",
    LONG_LABEL = "First variable",
    stringsAsFactors = FALSE
  )
  names(meta_data) <- c(VAR_NAMES, LABEL, LONG_LABEL)
  ds1 <- data.frame(v1 = c(1L, 2L, 3L))

  expect_warning(
    result <- check_var("v1", meta_data, ds1,
      need_type = DATA_TYPES$INTEGER
    ),
    "not all variables have a type assigned"
  )
  expect_identical(result, "v1")
})

test_that("util_correct_variable_use rejects an all-excluded type set", {
  skip_on_cran()

  check_vars <- function(resp_vars, meta_data, ds1, ...) {
    util_correct_variable_use(
      "resp_vars",
      allow_more_than_one = TRUE,
      do_not_stop = TRUE,
      ...
    )
  }
  environment(check_vars) <- asNamespace("dataquieR")

  meta_data <- data.frame(
    VAR_NAMES = c("v1", "v2"),
    LABEL = c("v1", "v2"),
    LONG_LABEL = c("First variable", "Second variable"),
    DATA_TYPE = rep(DATA_TYPES$FLOAT, 2),
    stringsAsFactors = FALSE
  )
  names(meta_data) <- c(VAR_NAMES, LABEL, LONG_LABEL, DATA_TYPE)
  ds1 <- data.frame(v1 = c(1.1, 2.2), v2 = c(3.3, 4.4))

  expect_error(
    suppressMessages(check_vars(c("v1", "v2"), meta_data, ds1,
        need_type = DATA_TYPES$INTEGER
      )),
    "none of the specified variables matches the requirements"
  )
})

test_that("util_correct_variable_use reports incomplete scale metadata", {
  skip_on_cran()

  check_var <- function(resp_var, meta_data, ds1, ...) {
    util_correct_variable_use("resp_var", ...)
    resp_var
  }
  environment(check_var) <- asNamespace("dataquieR")

  meta_data <- data.frame(
    VAR_NAMES = "v1",
    LABEL = "v1",
    LONG_LABEL = "First variable",
    stringsAsFactors = FALSE
  )
  names(meta_data) <- c(VAR_NAMES, LABEL, LONG_LABEL)
  ds1 <- data.frame(v1 = c(1L, 2L, 3L))

  expect_warning(
    result <- check_var("v1", meta_data, ds1,
      need_scale = SCALE_LEVELS$RATIO
    ),
    "not all variables have a scale level assigned"
  )
  expect_identical(result, "v1")
})
