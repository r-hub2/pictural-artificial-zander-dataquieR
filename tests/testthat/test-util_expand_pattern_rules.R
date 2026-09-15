skip_on_cran()

test_that("literal bracketed variable names are kept", {
  valid_names <- c("AGE_0", "SEX_0")

  expect_equal(
    util_expand_pattern_rules("[AGE_0] > 55", valid_names),
    util_attach_attr("[AGE_0] > 55", mismatches = list())
  )
})

test_that("wildcards expand matching variable names", {
  valid_names <- c("SEX_0", "PREGNANT_0", "PREGNANT_1", "AGE_0")

  expect_equal(
    util_expand_pattern_rules("[PREGNANT_*]", valid_names),
    util_attach_attr(c("[PREGNANT_0]", "[PREGNANT_1]"), mismatches = list())
  )
})

test_that("digit wildcard expands one digit only", {
  valid_names <- c("PREGNANT_0", "PREGNANT_09", "PREGNANT_x")

  expect_equal(
    util_expand_pattern_rules("[PREGNANT_#]", valid_names),
    util_attach_attr("[PREGNANT_0]", mismatches = list())
  )
})

test_that("two digit wildcard expands exactly two digits", {
  valid_names <- c("PREGNANT_0", "PREGNANT_09", "PREGNANT_99")

  expect_equal(
    util_expand_pattern_rules("[PREGNANT_##]", valid_names),
    util_attach_attr(c("[PREGNANT_09]", "[PREGNANT_99]"), mismatches = list())
  )
})

test_that("one or two digit wildcard expands one or two digits", {
  valid_names <- c("PREGNANT_0", "PREGNANT_09", "PREGNANT_123")

  expect_equal(
    util_expand_pattern_rules("[PREGNANT_#?]", valid_names),
    util_attach_attr(c("[PREGNANT_0]", "[PREGNANT_09]"), mismatches = list())
  )
})

test_that("captures are reused across variables", {
  valid_names <- c(
    "PREGNANT_0", "PREGNANT_1",
    "AGE_0", "AGE_1", "AGE_2"
  )

  expect_equal(
    util_expand_pattern_rules(
      "[PREGNANT_{W:#}] = \"yes\" and [AGE_{W}] > 55",
      valid_names
    ),
    util_attach_attr(c(
      "[PREGNANT_0] = \"yes\" and [AGE_0] > 55",
      "[PREGNANT_1] = \"yes\" and [AGE_1] > 55"
    ), mismatches = list())
  )
})

test_that("captures do not create mixed combinations", {
  valid_names <- c("PREGNANT_0", "PREGNANT_1", "AGE_0")

  expect_equal(
    util_expand_pattern_rules(
      "[PREGNANT_{W:#}] = \"yes\" and [AGE_{W}] > 55",
      valid_names
    ),
    util_attach_attr("[PREGNANT_0] = \"yes\" and [AGE_0] > 55", mismatches = list()) # nolint: line_length_linter.
  )
})

test_that("capture arithmetic with minus offset works", {
  valid_names <- c("AGE_0", "AGE_1", "AGE_2")

  expect_equal(
    util_expand_pattern_rules("[AGE_{CE:#}] < [AGE_{CE-1}]", valid_names),
    util_attach_attr(c("[AGE_1] < [AGE_0]", "[AGE_2] < [AGE_1]"), mismatches = list()) # nolint: line_length_linter.
  )
})

test_that("capture arithmetic with plus offset works", {
  valid_names <- c("AGE_0", "AGE_1", "AGE_2")

  expect_equal(
    util_expand_pattern_rules("[AGE_{CE:#}] < [AGE_{CE+1}]", valid_names),
    util_attach_attr(c("[AGE_0] < [AGE_1]", "[AGE_1] < [AGE_2]"), mismatches = list()) # nolint: line_length_linter.
  )
})

test_that("invalid arithmetic results are dropped", {
  valid_names <- c("AGE_0", "AGE_1")

  expect_equal(
    util_expand_pattern_rules("[AGE_{CE:#}] < [AGE_{CE-1}]", valid_names),
    util_attach_attr("[AGE_1] < [AGE_0]", mismatches = list())
  )
})

test_that("arithmetic on non-integer captures drops generated rule", {
  valid_names <- c("AGE_A", "AGE_B")

  expect_equal(
    util_expand_pattern_rules("[AGE_{CE:?}] < [AGE_{CE-1}]", valid_names),
    util_attach_attr(character(0), mismatches = list())
  )
})

test_that("escaped mini-language characters are treated literally", {
  valid_names <- c("Pregnant? 0", "Pregnant* 0", "AGE_0")

  expect_equal(
    util_expand_pattern_rules(
      "[Pregnant\\? {W:#}] and [AGE_{W}]",
      valid_names
    ),
    util_attach_attr("[Pregnant? 0] and [AGE_0]", mismatches = list())
  )
})

test_that("escaped capture braces are treated as literal characters", {
  expect_equal(
    util_expand_pattern_rules("[AGE_\\{W\\}]", "AGE_{W}"),
    util_attach_attr("[AGE_{W}]", mismatches = list())
  )
})

test_that("escaped capture contents and repeated captures stay literal", {
  expect_equal(
    util_expand_pattern_rules("[AGE_{W:\\}}]", "AGE_}"),
    util_attach_attr("[AGE_}]", mismatches = list())
  )

  expect_equal(
    util_expand_pattern_rules(
      "[AGE_{W:#}] and [AGE_{W:##}]",
      c("AGE_1", "AGE_01")
    ),
    util_attach_attr("[AGE_1] and [AGE_1]", mismatches = list())
  )

  expect_equal(
    util_expand_pattern_rules("[AGE_\\]", "AGE_\\"),
    util_attach_attr("[AGE_\\]", mismatches = list())
  )
})

test_that("variable labels with spaces and punctuation work", {
  valid_names <- c("Age at exam 0", "Pregnant? 0")

  expect_equal(
    util_expand_pattern_rules(
      "[Pregnant\\? {W:#}] and [Age at exam {W}]",
      valid_names
    ),
    util_attach_attr("[Pregnant? 0] and [Age at exam 0]", mismatches = list())
  )
})

test_that("valid names with square brackets produce a message", {
  expect_message2(
    util_expand_pattern_rules("[AGE_0]", c("AGE_0", "AGE[1]")),
    "square brackets are reserved"
  )
})

test_that("capture syntax errors fail clearly", {
  expect_error(
    util_expand_pattern_rules("[AGE_{W:#]", "AGE_0"),
    "Unclosed capture"
  )

  expect_error(
    util_expand_pattern_rules("[AGE_{BAD-NAME}]", "AGE_0"),
    "Invalid capture reference"
  )
})

test_that("undefined capture references fail clearly", {
  expect_error(
    util_expand_pattern_rules("[AGE_{W}]", "AGE_0"),
    "used before being defined"
  )
})

test_that("unmatched rules return character zero", {
  expect_equal(
    util_expand_pattern_rules("[DOES_NOT_EXIST_*]", "AGE_0"),
    util_attach_attr(character(0), mismatches = list("DOES_NOT_EXIST_*"))
  )
  expect_equal(
    util_expand_pattern_rules("[]", "AGE_0"),
    util_attach_attr("[]", mismatches = list())
  )
})

test_that("metadata group tokens expand in pattern rules", {
  meta_data <- data.frame(
    VAR_NAMES = c("id", "age", "height", "lab_glucose", "lab_cholesterol"),
    STUDY_SEGMENT = c("core", "core", "core", "lab", "lab"),
    DATAFRAMES = c("baseline", "baseline", "baseline", "labs", "labs"),
    REPORT_NAME = c("a", "a", "b", "c", NA_character_),
    stringsAsFactors = FALSE
  )
  valid_names <- meta_data[[VAR_NAMES]]

  expect_equal(
    util_expand_pattern_rules(
      "[ALL]",
      valid_names,
      meta_data = meta_data
    ),
    util_attach_attr(paste0("[", valid_names, "]"), mismatches = list())
  )
  expect_equal(
    util_expand_pattern_rules(
      "[SEGMENT:lab]",
      valid_names,
      meta_data = meta_data
    ),
    util_attach_attr(
      c("[lab_glucose]", "[lab_cholesterol]"),
      mismatches = list()
    )
  )
  expect_equal(
    util_expand_pattern_rules(
      "[DATAFRAME]",
      valid_names,
      meta_data = meta_data,
      context = data.frame(DATAFRAMES = "baseline")
    ),
    util_attach_attr(c("[id]", "[age]", "[height]"), mismatches = list())
  )
  expect_equal(
    util_expand_pattern_rules(
      '[WHERE {[REPORT_NAME] in {"a", "b"}}]',
      valid_names,
      meta_data = meta_data
    ),
    util_attach_attr(c("[id]", "[age]", "[height]"), mismatches = list())
  )
})

test_that("metadata group tokens report missing metadata context", {
  valid_names <- c("id", "age")

  # Assumption: group tokens are convenience shorthands for metadata-backed
  # expansions. Without the required metadata, they are dropped with an
  # applicability warning instead of being treated as literal variable names.
  expect_equal(
    util_expand_pattern_rules(
      "[ALL]",
      valid_names,
      meta_data = data.frame(x = 1)
    ),
    util_attach_attr(c("[id]", "[age]"), mismatches = list())
  )
  expect_warning(
    out <- util_expand_pattern_rules("[SEGMENT:core]", valid_names),
    "needs item-level metadata"
  )
  expect_equal(
    out,
    util_attach_attr(character(0), mismatches = list("SEGMENT:core"))
  )

  expect_warning(
    out <- util_expand_pattern_rules(
      "[DATAFRAME]",
      valid_names,
      meta_data = data.frame(VAR_NAMES = valid_names),
      context = data.frame(DATAFRAMES = "baseline")
    ),
    "needs .DATAFRAMES. in item-level metadata"
  )
  expect_equal(
    out,
    util_attach_attr(character(0), mismatches = list("DATAFRAME"))
  )

  meta_data <- data.frame(
    VAR_NAMES = valid_names,
    STUDY_SEGMENT = c("core", "core"),
    DATAFRAMES = c("baseline", "baseline"),
    stringsAsFactors = FALSE
  )
  expect_warning(
    out <- util_expand_pattern_rules(
      "[SEGMENT]",
      valid_names,
      meta_data = meta_data,
      context = data.frame(other = "core")
    ),
    "needs .STUDY_SEGMENT. in the current metadata row"
  )
  expect_equal(
    out,
    util_attach_attr(character(0), mismatches = list("SEGMENT"))
  )

  expect_warning(
    out <- util_expand_pattern_rules(
      "[SEGMENT:other]",
      valid_names,
      meta_data = meta_data
    ),
    "does not match any variables"
  )
  expect_equal(
    out,
    util_attach_attr(character(0), mismatches = list("SEGMENT:other"))
  )

  expect_equal(
    util_expand_pattern_rules(
      "[DATAFRAME]",
      valid_names,
      meta_data = meta_data,
      context = list(DATAFRAMES = "baseline")
    ),
    util_attach_attr(c("[id]", "[age]"), mismatches = list())
  )
})

test_that("WHERE metadata tokens report invalid or unmatched rules", {
  valid_names <- c("id", "age")
  meta_data <- data.frame(
    VAR_NAMES = valid_names,
    REPORT_NAME = c("baseline", "followup"),
    stringsAsFactors = FALSE
  )

  # Assumption: WHERE expressions must parse as REDCap-like metadata rules and
  # evaluate to one logical value per metadata row.
  expect_warning(
    out <- util_expand_pattern_rules(
      "[WHERE {[REPORT_NAME] == }]",
      valid_names,
      meta_data = meta_data
    ),
    "could not be parsed"
  )
  expect_equal(
    out,
    util_attach_attr(
      character(0),
      mismatches = list("WHERE {[REPORT_NAME] == }")
    )
  )

  expect_warning(
    out <- util_expand_pattern_rules(
      "[WHERE {[MISSING] == \"x\"}]",
      valid_names,
      meta_data = meta_data
    ),
    "could not be evaluated"
  )
  expect_equal(
    out,
    util_attach_attr(
      character(0),
      mismatches = list("WHERE {[MISSING] == \"x\"}")
    )
  )

  expect_warning(
    out <- util_expand_pattern_rules(
      "[WHERE {TRUE}]",
      valid_names,
      meta_data = meta_data
    ),
    "one logical value per item-level metadata row"
  )
  expect_equal(
    out,
    util_attach_attr(character(0), mismatches = list("WHERE {TRUE}"))
  )

  expect_warning(
    out <- util_expand_pattern_rules(
      "[WHERE {[REPORT_NAME] == \"screening\"}]",
      valid_names,
      meta_data = meta_data
    ),
    "does not match any variables"
  )
  expect_equal(
    out,
    util_attach_attr(
      character(0),
      mismatches = list("WHERE {[REPORT_NAME] == \"screening\"}")
    )
  )

  expect_warning(
    out <- util_expand_pattern_rules(
      "[WHERE {[REPORT_NAME] == \"baseline\"}]",
      valid_names,
      meta_data = data.frame(REPORT_NAME = "baseline")
    ),
    "needs .VAR_NAMES. in item-level metadata"
  )
  expect_equal(
    out,
    util_attach_attr(
      character(0),
      mismatches = list("WHERE {[REPORT_NAME] == \"baseline\"}")
    )
  )

  expect_warning(
    out <- util_expand_pattern_rules(
      "[WHERE {[REPORT_NAME] == \"baseline\"}]",
      valid_names
    ),
    "needs item-level metadata"
  )
  expect_equal(
    out,
    util_attach_attr(
      character(0),
      mismatches = list("WHERE {[REPORT_NAME] == \"baseline\"}")
    )
  )
})

test_that("expanded cross-item pattern rules get unique IDs and labels", {
  meta_data <- data.frame(
    VAR_NAMES = c("sbp_0", "dbp_0", "age_0", "age_1", "age_2"),
    LABEL = c("sbp_0", "dbp_0", "age_0", "age_1", "age_2"),
    LONG_LABEL = c("sbp_0", "dbp_0", "age_0", "age_1", "age_2"),
    ORIGINAL_VAR_NAMES = c("sbp_0", "dbp_0", "age_0", "age_1", "age_2"),
    ORIGINAL_LABEL = c("sbp_0", "dbp_0", "age_0", "age_1", "age_2"),
    stringsAsFactors = FALSE
  )
  meta_data_cross_item <- data.frame(
    CHECK_ID = c("bloodpressure", "age"),
    CHECK_LABEL = c("bloodpressure", "age"),
    CONTRADICTION_TERM = c(
      "[sbp_0] > [dbp_0]",
      "[age_{W:#}] < [age_{W-1}]"
    ),
    stringsAsFactors = FALSE
  )

  out <- util_normalize_cross_item(
    meta_data = meta_data,
    meta_data_cross_item = meta_data_cross_item
  )

  expect_equal(out[[CHECK_ID]], c("bloodpressure", "age_1", "age_2"))
  expect_equal(out[[CHECK_LABEL]], c("bloodpressure", "age #1", "age #2"))
  expect_equal(
    out[[CONTRADICTION_TERM]],
    c("[sbp_0] > [dbp_0]", "[age_1] < [age_0]", "[age_2] < [age_1]")
  )
})
