test_that("util_normalize_cross_item returns a normalized empty table", {
  skip_on_cran()

  meta_data <- data.frame(
    VAR_NAMES = character(0),
    LABEL = character(0),
    stringsAsFactors = FALSE
  )
  meta_data_cross_item <- data.frame(stringsAsFactors = FALSE)

  out <- util_normalize_cross_item(
    meta_data = meta_data,
    meta_data_cross_item = meta_data_cross_item
  )

  expect_true(isTRUE(attr(out, "normalized", exact = TRUE)))
  expect_equal(nrow(out), 0L)
  expect_true(all(c(
    VARIABLE_LIST,
    CHECK_LABEL,
    CONTRADICTION_TERM,
    CHECK_ID,
    DATA_PREPARATION
  ) %in% colnames(out)))
})

test_that(
  "util_normalize_cross_item fixes duplicate IDs, labels, and preparation tags",
  {
    skip_on_cran()

    meta_data <- data.frame(
      VAR_NAMES = c("var_a", "var_b"),
      LABEL = c("Variable A", "Variable B"),
      LONG_LABEL = c("Variable A", "Variable B"),
      ORIGINAL_VAR_NAMES = c("var_a", "var_b"),
      ORIGINAL_LABEL = c("Variable A", "Variable B"),
      VALUE_LABELS = c("1 = yes", NA_character_),
      stringsAsFactors = FALSE
    )
    meta_data_cross_item <- data.frame(
      CHECK_ID = c("dup", "dup"),
      CHECK_LABEL = c("same", "same"),
      VARIABLE_LIST = c("var_a | var_b", "var_a"),
      CONTRADICTION_TERM = c("[var_a] > [var_b]", NA_character_),
      DATA_PREPARATION = c(
        "label | unknown | label", "missing_na | missing_label"
      ),
      stringsAsFactors = FALSE
    )

    out <- suppressMessages(util_normalize_cross_item(
      meta_data = meta_data,
      meta_data_cross_item = meta_data_cross_item
    ))

    expect_true(isTRUE(attr(out, "normalized", exact = TRUE)))
    expect_equal(out[[CHECK_ID]], c("1", "2"))
    expect_equal(out[[CHECK_LABEL]], c("same", "Check #2"))
    expect_equal(out[[VARIABLE_LIST]], c(
      "Variable A | Variable B",
      "Variable A"
    ))
    expect_equal(out[[VARIABLE_LIST_ORDER]], c("var_a|var_b", "var_a"))
    expect_equal(
      out[[DATA_PREPARATION]], c("LABEL", "LABEL | MISSING_NA | LIMITS")
    )
  }
)

test_that("util_normalize_cross_item supplies canonical group identities", {
  skip_on_cran()

  meta_data <- data.frame(
    VAR_NAMES = c("item_a", "item_b"),
    LABEL = c("Item A", "Item B"),
    stringsAsFactors = FALSE
  )
  raw_cross_item <- data.frame(
    VARIABLE_LIST = c("item_a | item_b", "item_b"),
    CONTRADICTION_TERM = NA_character_,
    stringsAsFactors = FALSE
  )

  normalized <- util_normalize_cross_item(
    meta_data = meta_data,
    meta_data_cross_item = raw_cross_item
  )

  expect_true(isTRUE(attr(normalized, "normalized", exact = TRUE)))
  expect_identical(normalized[[CHECK_ID]], c("1", "2"))
  expect_identical(
    normalized[[CHECK_LABEL]],
    c("1: Item A, Item B", "2: Item B")
  )
  expect_identical(
    normalized[[VARIABLE_LIST]],
    c("Item A | Item B", "Item B")
  )
})

test_that("util_normalize_cross_item repairs partial and trimmed identities", {
  skip_on_cran()

  meta_data <- data.frame(
    VAR_NAMES = c("item_a", "item_b"),
    LABEL = c("Item A", "Item B"),
    stringsAsFactors = FALSE
  )
  raw_cross_item <- data.frame(
    CHECK_ID = c("duplicate", " duplicate ", NA_character_, "valid"),
    CHECK_LABEL = c(" Named group ", "", "Check #2", NA_character_),
    VARIABLE_LIST = c("item_a", "item_b", "item_a", "item_a | item_b"),
    CONTRADICTION_TERM = NA_character_,
    stringsAsFactors = FALSE
  )

  expect_message(
    normalized <- util_normalize_cross_item(
      meta_data = meta_data,
      meta_data_cross_item = raw_cross_item
    ),
    "must be non-empty and unique"
  )

  expect_identical(normalized[[CHECK_ID]], c("1", "2", "3", "4"))
  expect_identical(
    normalized[[CHECK_LABEL]],
    c(
      "Named group",
      "2: Item B",
      "Check #2",
      "4: Item A, Item B"
    )
  )
})

test_that("util_normalize_cross_item prefers explicit and scale labels", {
  skip_on_cran()

  meta_data <- data.frame(
    VAR_NAMES = c("item_a", "item_b"),
    LABEL = c("Item A", "Item B"),
    stringsAsFactors = FALSE
  )
  raw_cross_item <- data.frame(
    CHECK_ID = c("explicit", "named_scale", "short_scale"),
    CHECK_LABEL = c("Explicit check", NA_character_, ""),
    SCALE_NAME = c("Ignored scale", "Named scale", NA_character_),
    SCALE_ACRONYM = c("IS", "NS", "SS"),
    VARIABLE_LIST = c("item_a", "item_a | item_b", "item_b"),
    CONTRADICTION_TERM = NA_character_,
    stringsAsFactors = FALSE
  )

  normalized <- util_normalize_cross_item(
    meta_data = meta_data,
    meta_data_cross_item = raw_cross_item
  )

  expect_identical(
    normalized[[CHECK_LABEL]],
    c("Explicit check", "Named scale", "SS")
  )
})

test_that("cross-item fallback labels use short item labels", {
  skip_on_cran()

  meta_data <- data.frame(
    VAR_NAMES = c("item_a", "item_b"),
    LABEL = c("Item A", "Item B"),
    LONG_LABEL = c(
      "A deliberately very long label for item A",
      "A deliberately very long label for item B"
    ),
    DATA_TYPE = DATA_TYPES$INTEGER,
    SCALE_LEVEL = SCALE_LEVELS$RATIO,
    MISSING_LIST = "",
    JUMP_LIST = "",
    stringsAsFactors = FALSE
  )
  raw_cross_item <- data.frame(
    CHECK_ID = "group_1",
    VARIABLE_LIST = paste(meta_data[[LONG_LABEL]], collapse = " | "),
    CONTRADICTION_TERM = NA_character_,
    stringsAsFactors = FALSE
  )

  normalized <- util_normalize_cross_item(
    meta_data = meta_data,
    meta_data_cross_item = raw_cross_item,
    label_col = LONG_LABEL
  )

  expect_identical(
    normalized[[CHECK_LABEL]],
    "group_1: Item A, Item B"
  )
})

test_that(
  paste0(
    "util_normalize_cross_item reports both extra and missing ",
    "variable-list items"
  ),
  {
    skip_on_cran()

    meta_data <- data.frame(
      VAR_NAMES = c("var_a", "var_b", "var_c"),
      LABEL = c("Variable A", "Variable B", "Variable C"),
      stringsAsFactors = FALSE
    )
    meta_data_cross_item <- data.frame(
      CHECK_ID = "check1",
      VARIABLE_LIST = "var_a | var_c",
      CONTRADICTION_TERM = "[var_a] > [var_b]",
      stringsAsFactors = FALSE
    )

    expect_warning(
      out <- util_normalize_cross_item(
        meta_data = meta_data,
        meta_data_cross_item = meta_data_cross_item
      ),
      "Variable C was not used.+Also.+Variable B was missing"
    )

    expect_equal(out[[VARIABLE_LIST]], "Variable A | Variable B")
  }
)

test_that("inferred variable lists retain a computation order", {
  skip_on_cran()

  meta_data <- data.frame(
    VAR_NAMES = c("AGE_0", "AGE_1"),
    LABEL = c("Age B/L", "Age F/U"),
    stringsAsFactors = FALSE
  )
  meta_data_cross_item <- data.frame(
    CHECK_ID = "age_follow_up",
    CHECK_LABEL = "Age follow-up",
    VARIABLE_LIST = NA_character_,
    CONTRADICTION_TERM = "[AGE_1] < [AGE_0]",
    stringsAsFactors = FALSE
  )

  out <- util_normalize_cross_item(
    meta_data = meta_data,
    meta_data_cross_item = meta_data_cross_item
  )

  expect_identical(out[[VARIABLE_LIST]], "Age B/L | Age F/U")
  expect_identical(out[[VARIABLE_LIST_ORDER]], "AGE_0 | AGE_1")
})

test_that(paste0(
  "util_normalize_cross_item reports bracket labels ",
  "and invalid limits"
), {
  skip_on_cran()

  withr::local_options(dataquieR.lang = "en")
  meta_data <- data.frame(
    VAR_NAMES = c("var_a", "var_b"),
    LABEL = c("Variable [A]", "Variable B"),
    LABEL_en = c("Variable [A]", "Variable B"),
    LONG_LABEL = c("Variable [A]", "Variable B"),
    ORIGINAL_VAR_NAMES = c("var_a", "var_b"),
    ORIGINAL_LABEL = c("Variable [A]", "Variable B"),
    stringsAsFactors = FALSE
  )
  meta_data_cross_item <- data.frame(
    CHECK_ID = "c1",
    CHECK_LABEL = "Check",
    VARIABLE_LIST = "var_a | var_b",
    CONTRADICTION_TERM = "[var_a] > [var_b]",
    HARD_LIMITS = "not a redcap rule",
    stringsAsFactors = FALSE
  )

  expect_message(
    expect_warning(
      out <- util_normalize_cross_item(
        meta_data = meta_data,
        meta_data_cross_item = meta_data_cross_item
      ),
      "Cannot parse"
    ),
    "square brackets"
  )

  expect_true(isTRUE(attr(out, "normalized", exact = TRUE)))
  expect_equal(out[[HARD_LIMITS]], NA_character_)
  expect_equal(out[[VARIABLE_LIST]], "Variable [A] | Variable B")
})

test_that("util_normalize_cross_item expands variable-list group tokens", {
  skip_on_cran()

  meta_data <- data.frame(
    VAR_NAMES = c("id", "age", "height", "lab_glucose", "lab_cholesterol"),
    LABEL = c("ID", "Age", "Height", "Glucose", "Cholesterol"),
    STUDY_SEGMENT = c("core", "core", "core", "lab", "lab"),
    DATAFRAMES = c("baseline", "baseline", "baseline", "labs", "labs"),
    REPORT_NAME = c("core", "core", "extra", "labs", "labs"),
    stringsAsFactors = FALSE
  )
  meta_data_cross_item <- data.frame(
    CHECK_ID = c(
      "all",
      "explicit_segment",
      "current_segment",
      "dataframe",
      "where",
      "wildcard"
    ),
    CHECK_LABEL = c(
      "All variables",
      "Lab variables",
      "Core variables",
      "Baseline dataframe",
      "Report variables",
      "Lab wildcard"
    ),
    STUDY_SEGMENT = c(
      NA_character_,
      NA_character_,
      "core",
      NA_character_,
      NA_character_,
      NA_character_
    ),
    VARIABLE_LIST = c(
      "[ALL]",
      "[SEGMENT:lab]",
      "[SEGMENT]",
      "[DATAFRAME:baseline]",
      '[WHERE {[REPORT_NAME] in {"core", "extra"}}]',
      "lab_*"
    ),
    stringsAsFactors = FALSE
  )

  out <- util_normalize_cross_item(
    meta_data = meta_data,
    meta_data_cross_item = meta_data_cross_item
  )

  expect_equal(out[[VARIABLE_LIST]], c(
    "ID | Age | Height | Glucose | Cholesterol",
    "Glucose | Cholesterol",
    "ID | Age | Height",
    "ID | Age | Height",
    "ID | Age | Height",
    "Cholesterol | Glucose"
  ))
  expect_equal(out[[VARIABLE_LIST_ORDER]], c(
    "id|age|height|lab_glucose|lab_cholesterol",
    "lab_glucose|lab_cholesterol",
    "id|age|height",
    "id|age|height",
    "id|age|height",
    "lab_cholesterol|lab_glucose"
  ))
})
