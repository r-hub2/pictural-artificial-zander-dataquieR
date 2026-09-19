test_that("prep_add_computed_variables handles empty and simple rules", {
  skip_on_cran()

  study_data <- data.frame(a = c(1, 2), b = c(10, 20))
  meta_data <- data.frame(
    var_names = c("a", "b"),
    data_type = c(DATA_TYPES$INTEGER, DATA_TYPES$INTEGER),
    missing_list = NA_character_,
    jump_list = NA_character_,
    stringsAsFactors = FALSE
  )
  colnames(meta_data) <- c(VAR_NAMES, DATA_TYPE, MISSING_LIST, JUMP_LIST)

  empty_result <- prep_add_computed_variables(
    study_data = study_data,
    meta_data = meta_data,
    label_col = VAR_NAMES,
    rules = data.frame(),
    use_value_labels = FALSE
  )
  expect_s3_class(
    empty_result$ModifiedStudyData,
    "dataquieR_data_frame_prepared"
  )
  expect_equal(empty_result$ModifiedStudyData$a, study_data$a)
  expect_equal(empty_result$ModifiedStudyData$b, study_data$b)

  rules <- data.frame(
    var_names = "sum_ab",
    computation_rule = "[a] + [b]",
    stringsAsFactors = FALSE
  )
  colnames(rules) <- c(VAR_NAMES, COMPUTATION_RULE)

  simple_result <- suppressWarnings(suppressMessages(
    prep_add_computed_variables(
      study_data = study_data,
      meta_data = meta_data,
      label_col = VAR_NAMES,
      rules = rules,
      use_value_labels = FALSE
    )))
  expect_equal(simple_result$ModifiedStudyData$sum_ab, c(11, 22))
})

test_that("file-computation ID checks use the mapped ID column", {
  skip_on_cran()
  folder <- withr::local_tempdir()
  writeLines('{"id":"1"}', file.path(folder, "obs-1.json"))
  writeLines('{"id":"3"}', file.path(folder, "obs-3.json"))

  study_data <- data.frame(id = c("1", "2"))
  meta_data <- data.frame(VAR_NAMES = c("id", "file"))
  rules <- data.frame(
    VAR_NAMES = "file",
    FILE_PATH = folder,
    ID_PATH_REGEX = "obs-([0-9]+)[.]json$",
    ID_ENTITY_PATH = NA_character_,
    MERGE_ID_VAR = "id"
  )
  withr::local_options(dataquieR.ELEMENT_MISSMATCH_CHECKTYPE = "subset_u")

  expect_warning(
    result <- .util_add_computed_file_paths(
      study_data, meta_data, VAR_NAMES, rules
    ),
    "Some IDs extracted"
  )
  expect_true(file.exists(result$ModifiedStudyData$file[[1]]))
  expect_true(is.na(result$ModifiedStudyData$file[[2]]))
})

test_that("file computation reports missing files for study-data IDs", {
  skip_on_cran()
  folder <- withr::local_tempdir()
  writeLines('{"id":"1"}', file.path(folder, "obs-1.json"))

  study_data <- data.frame(id = c("1", "2"))
  meta_data <- data.frame(VAR_NAMES = c("id", "file"))
  rules <- data.frame(
    VAR_NAMES = "file",
    FILE_PATH = folder,
    ID_PATH_REGEX = "obs-([0-9]+)[.]json$",
    ID_ENTITY_PATH = NA_character_,
    MERGE_ID_VAR = "id"
  )
  withr::local_options(dataquieR.ELEMENT_MISSMATCH_CHECKTYPE = "subset_m")

  expect_warning(
    result <- .util_add_computed_file_paths(
      study_data, meta_data, VAR_NAMES, rules
    ),
    "No file in folder"
  )
  expect_equal(basename(result$ModifiedStudyData$file[[1]]), "obs-1.json")
  expect_true(is.na(result$ModifiedStudyData$file[[2]]))
})

test_that("file computation reports hidden files and unmatched IDs", {
  skip_on_cran()
  folder <- withr::local_tempdir()
  writeLines('{"id":"1"}', file.path(folder, ".obs-1.json"))
  writeLines('{"id":"3"}', file.path(folder, "obs-3.json"))

  study_data <- data.frame(id = c("1", "2"))
  meta_data <- data.frame(VAR_NAMES = c("id", "file"))
  rules <- data.frame(
    VAR_NAMES = "file",
    FILE_PATH = folder,
    ID_PATH_REGEX = "obs-([0-9]+)[.]json$",
    ID_ENTITY_PATH = NA_character_,
    MERGE_ID_VAR = "id"
  )
  withr::local_options(dataquieR.ELEMENT_MISSMATCH_CHECKTYPE = "subset_u")

  expect_warning(
    result <- .util_add_computed_file_paths(
      study_data, meta_data, VAR_NAMES, rules
    ),
    "Some IDs extracted"
  )
  expect_equal(basename(result$ModifiedStudyData$file[[1]]), ".obs-1.json")
  expect_true(is.na(result$ModifiedStudyData$file[[2]]))
})

test_that("prep_add_computed_variables resolves file-only rules", {
  skip_on_cran()
  folder <- withr::local_tempdir()
  writeLines('{"id":"1"}', file.path(folder, "obs-1.json"))

  study_data <- data.frame(id = "1")
  meta_data <- data.frame(
    VAR_NAMES = c("id", "file"),
    DATA_TYPE = c(DATA_TYPES$STRING, DATA_TYPES$STRING),
    MISSING_LIST = NA_character_,
    JUMP_LIST = NA_character_
  )
  rules <- data.frame(
    VAR_NAMES = "file",
    COMPUTATION_RULE = NA_character_,
    FILE_PATH = folder,
    ID_PATH_REGEX = "obs-([0-9]+)[.]json$",
    ID_ENTITY_PATH = NA_character_,
    MERGE_ID_VAR = "id"
  )

  result <- suppressMessages(prep_add_computed_variables(
    study_data = study_data,
    meta_data = meta_data,
    label_col = VAR_NAMES,
    rules = rules,
    use_value_labels = FALSE
  ))
  expect_equal(basename(result$ModifiedStudyData$file), "obs-1.json")
})

test_that("file computation excludes files without extractable IDs", {
  skip_on_cran()
  folder <- withr::local_tempdir()
  writeLines('{"id":"1"}', file.path(folder, "obs-1.json"))
  writeLines('{"id":"unknown"}', file.path(folder, "obs-other.json"))

  study_data <- data.frame(id = "1")
  meta_data <- data.frame(VAR_NAMES = c("id", "file"))
  rules <- data.frame(
    VAR_NAMES = "file",
    FILE_PATH = folder,
    ID_PATH_REGEX = "obs-([0-9]+)[.]json$",
    ID_ENTITY_PATH = NA_character_,
    MERGE_ID_VAR = "id"
  )

  expect_warning(
    result <- .util_add_computed_file_paths(
      study_data, meta_data, VAR_NAMES, rules
    ),
    "Could not extract IDs"
  )
  expect_equal(basename(result$ModifiedStudyData$file), "obs-1.json")
})

test_that("file computation ignores rules without item metadata", {
  skip_on_cran()
  folder <- withr::local_tempdir()
  writeLines('{"id":"1"}', file.path(folder, "obs-1.json"))

  study_data <- data.frame(id = "1")
  meta_data <- data.frame(VAR_NAMES = c("id", "file"))
  rules <- data.frame(
    VAR_NAMES = c("file", "unknown_file"),
    FILE_PATH = folder,
    ID_PATH_REGEX = "obs-([0-9]+)[.]json$",
    ID_ENTITY_PATH = NA_character_,
    MERGE_ID_VAR = "id"
  )

  result <- suppressMessages(.util_add_computed_file_paths(
    study_data, meta_data, VAR_NAMES, rules
  ))
  expect_equal(basename(result$ModifiedStudyData$file), "obs-1.json")
  expect_false("unknown_file" %in% names(result$ModifiedStudyData))
})

test_that("file computation keeps majority format and regex ID precedence", {
  skip_on_cran()
  folder <- withr::local_tempdir()
  writeLines('{"id":"1"}', file.path(folder, "obs-1.json"))
  writeLines('{"id":"2"}', file.path(folder, "obs-2.json"))
  writeLines("<obs><id>3</id></obs>", file.path(folder, "obs-3.xml"))

  study_data <- data.frame(id = c("1", "2"))
  meta_data <- data.frame(VAR_NAMES = c("id", "file"))
  rules <- data.frame(
    VAR_NAMES = "file",
    FILE_PATH = folder,
    ID_PATH_REGEX = "obs-([0-9]+)[.]json$",
    ID_ENTITY_PATH = ".id",
    MERGE_ID_VAR = "id"
  )

  expect_warning(
    result <- suppressMessages(.util_add_computed_file_paths(
      study_data, meta_data, VAR_NAMES, rules
    )),
    "Both ID_PATH_REGEX and ID_ENTITY_PATH"
  )
  expect_equal(basename(result$ModifiedStudyData$file),
    c("obs-1.json", "obs-2.json"))

  rules[["ID_PATH_REGEX"]] <- NA_character_
  rules[["ID_ENTITY_PATH"]] <- NA_character_
  expect_error(suppressMessages(.util_add_computed_file_paths(
    study_data, meta_data, VAR_NAMES, rules
  )), "None of ID_PATH_REGEX and ID_ENTITY_PATH")
})

test_that("file computation selects XML when XML files are the majority", {
  skip_on_cran()
  folder <- withr::local_tempdir()
  writeLines('{"id":"1"}', file.path(folder, "obs-1.json"))
  writeLines("<obs><id>2</id></obs>", file.path(folder, "obs-2.xml"))
  writeLines("<obs><id>3</id></obs>", file.path(folder, "obs-3.xml"))

  study_data <- data.frame(id = c("2", "3"))
  meta_data <- data.frame(VAR_NAMES = c("id", "file"))
  rules <- data.frame(
    VAR_NAMES = "file",
    FILE_PATH = folder,
    ID_PATH_REGEX = "obs-([0-9]+)[.]xml$",
    ID_ENTITY_PATH = NA_character_,
    MERGE_ID_VAR = "id"
  )

  result <- suppressMessages(.util_add_computed_file_paths(
    study_data, meta_data, VAR_NAMES, rules
  ))
  expect_equal(basename(result$ModifiedStudyData$file),
    c("obs-2.xml", "obs-3.xml"))
})

test_that("prep_add_computed_variables applies DATA_PREPARATION split routes", {
  skip_on_cran()

  study_data <- data.frame(a = c(1, 2), b = c(10, 20))
  meta_data <- data.frame(
    var_names = c("a", "b"),
    data_type = c(DATA_TYPES$INTEGER, DATA_TYPES$INTEGER),
    missing_list = NA_character_,
    jump_list = NA_character_,
    stringsAsFactors = FALSE
  )
  colnames(meta_data) <- c(VAR_NAMES, DATA_TYPE, MISSING_LIST, JUMP_LIST)

  rules <- data.frame(
    var_names = "sum_ab",
    computation_rule = "[a] + [b]",
    data_preparation = "MISSING_NA",
    stringsAsFactors = FALSE
  )
  colnames(rules) <- c(VAR_NAMES, COMPUTATION_RULE, DATA_PREPARATION)

  result <- suppressWarnings(suppressMessages(prep_add_computed_variables(
    study_data = study_data,
    meta_data = meta_data,
    label_col = VAR_NAMES,
    rules = rules,
    use_value_labels = FALSE
  )))

  expect_equal(result$ModifiedStudyData$sum_ab, c(11, 22))
})

test_that("prep_add_computed_variables keeps metadata for new variables", {
  skip_on_cran()

  study_data <- data.frame(id = c(1, 2))
  meta_data <- data.frame(
    var_names = "computed_only",
    data_type = DATA_TYPES$INTEGER,
    missing_list = NA_character_,
    jump_list = NA_character_,
    stringsAsFactors = FALSE
  )
  colnames(meta_data) <- c(VAR_NAMES, DATA_TYPE, MISSING_LIST, JUMP_LIST)
  rules <- data.frame(
    var_names = "computed_only",
    computation_rule = "[pi]",
    stringsAsFactors = FALSE
  )
  colnames(rules) <- c(VAR_NAMES, COMPUTATION_RULE)
  observed <- new.env(parent = emptyenv())

  testthat::local_mocked_bindings(
    .util_add_computed_variables = function(ds1, meta_data, label_col, rules,
      use_value_labels) {
      observed$ds1_ncol <- ncol(ds1)
      observed$meta_vars <- meta_data[[label_col]]
      ds1[[rules[[VAR_NAMES]][[1L]]]] <- c(3L, 3L)
      list(ModifiedStudyData = ds1)
    },
    .package = "dataquieR"
  )

  result <- suppressWarnings(prep_add_computed_variables(
    study_data = study_data,
    meta_data = meta_data,
    label_col = VAR_NAMES,
    rules = rules,
    use_value_labels = FALSE
  ))

  expect_identical(observed$ds1_ncol, 0L)
  expect_equal(observed$meta_vars, "computed_only")
  expect_equal(result$ModifiedStudyData$computed_only, c(3L, 3L))
})

test_that(
  "prep_add_computed_variables resolves conflicting missing strategies",
  {
    skip_on_cran()

    study_data <- data.frame(a = c(1, 2), b = c(10, 20))
    meta_data <- data.frame(
      var_names = c("a", "b"),
      data_type = c(DATA_TYPES$INTEGER, DATA_TYPES$INTEGER),
      missing_list = "999",
      jump_list = "998",
      stringsAsFactors = FALSE
    )
    colnames(meta_data) <- c(VAR_NAMES, DATA_TYPE, MISSING_LIST, JUMP_LIST)
    rules <- data.frame(
      var_names = "sum_ab",
      computation_rule = "[a] + [b]",
      data_preparation = "MISSING_LABEL | MISSING_NA",
      stringsAsFactors = FALSE
    )
    colnames(rules) <- c(VAR_NAMES, COMPUTATION_RULE, DATA_PREPARATION)

    expect_warning(
      result <- suppressMessages(prep_add_computed_variables(
        study_data = study_data,
        meta_data = meta_data,
        label_col = VAR_NAMES,
        rules = rules,
        use_value_labels = FALSE
      )),
      "Falling back"
    )

    expect_equal(result$ModifiedStudyData$sum_ab, c(11, 22))
  }
)

test_that("prep_add_computed_variables routes missing strategy variants", {
  skip_on_cran()

  study_data <- data.frame(a = 1, b = 2)
  meta_data <- data.frame(
    var_names = c("a", "b"),
    data_type = DATA_TYPES$INTEGER,
    missing_list = NA_character_,
    jump_list = NA_character_,
    stringsAsFactors = FALSE
  )
  colnames(meta_data) <- c(VAR_NAMES, DATA_TYPE, MISSING_LIST, JUMP_LIST)
  rules <- data.frame(
    var_names = c("x", "y"),
    computation_rule = c("[a]", "[b]"),
    data_preparation = c("MISSING_INTERPRET", "LABEL | MISSING_LABEL"),
    stringsAsFactors = FALSE
  )
  colnames(rules) <- c(VAR_NAMES, COMPUTATION_RULE, DATA_PREPARATION)
  calls <- new.env(parent = emptyenv())
  calls$args <- list()

  testthat::local_mocked_bindings(
    .util_add_computed_variables = function(ds1, meta_data, label_col, rules,
      use_value_labels, replace_missing_by, replace_limits = TRUE) {
      calls$args[[length(calls$args) + 1L]] <- list(
        rule = rules[[VAR_NAMES]],
        use_value_labels = use_value_labels,
        replace_missing_by = replace_missing_by,
        replace_limits = replace_limits
      )
      list(ModifiedStudyData = ds1)
    },
    .package = "dataquieR"
  )

  result <- prep_add_computed_variables(
    study_data = study_data,
    meta_data = meta_data,
    label_col = VAR_NAMES,
    rules = rules,
    use_value_labels = FALSE
  )

  expect_equal(
    as.data.frame(result$ModifiedStudyData),
    study_data,
    ignore_attr = TRUE
  )
  expect_length(calls$args, 2L)
  routed_by_missing <- setNames(calls$args, vapply(
    calls$args,
    `[[`,
    character(1),
    "replace_missing_by"
  ))
  expect_true(routed_by_missing$LABEL$use_value_labels)
  expect_false(routed_by_missing$LABEL$replace_limits)
  expect_false(routed_by_missing$INTERPRET$use_value_labels)
  expect_false(routed_by_missing$INTERPRET$replace_limits)
})

test_that("computed-variable failures return missing target values", {
  skip_on_cran()

  study_data <- data.frame(a = c(1, 2), b = c(10, 20))
  meta_data <- data.frame(
    var_names = c("a", "b"),
    data_type = c(DATA_TYPES$INTEGER, DATA_TYPES$INTEGER),
    missing_list = NA_character_,
    jump_list = NA_character_,
    stringsAsFactors = FALSE
  )
  colnames(meta_data) <- c(VAR_NAMES, DATA_TYPE, MISSING_LIST, JUMP_LIST)
  rules <- data.frame(
    var_names = "sum_ab",
    computation_rule = "[a] + [b]",
    stringsAsFactors = FALSE
  )
  colnames(rules) <- c(VAR_NAMES, COMPUTATION_RULE)

  testthat::local_mocked_bindings(
    util_eval_rule = function(...) {
      structure(
        "rule evaluation failed",
        class = "try-error",
        condition = simpleError("rule evaluation failed")
      )
    }
  )

  expect_warning(
    result <- .util_add_computed_variables(
      ds1 = study_data,
      meta_data = meta_data,
      label_col = VAR_NAMES,
      rules = rules,
      use_value_labels = FALSE
    ),
    "results are all NA"
  )

  expect_true(all(is.na(result$ModifiedStudyData$sum_ab)))
})

test_that("computed-variable evaluation derives value-label usage", {
  skip_on_cran()

  study_data <- data.frame(a = c(1, 2))
  meta_data <- data.frame(
    var_names = "a",
    value_labels = "1 = one | 2 = two",
    stringsAsFactors = FALSE
  )
  colnames(meta_data) <- c(VAR_NAMES, VALUE_LABELS)
  rules <- data.frame(
    var_names = "copy_a",
    computation_rule = "[a]",
    stringsAsFactors = FALSE
  )
  colnames(rules) <- c(VAR_NAMES, COMPUTATION_RULE)
  observed <- new.env(parent = emptyenv())

  testthat::local_mocked_bindings(
    util_eval_rule = function(rule, ds1, meta_data, use_value_labels, ...) {
      observed$use_value_labels <- use_value_labels
      ds1$a
    }
  )

  result <- .util_add_computed_variables(
    ds1 = study_data,
    meta_data = meta_data,
    label_col = VAR_NAMES,
    rules = rules
  )

  expect_true(observed$use_value_labels)
  expect_equal(result$ModifiedStudyData$copy_a, study_data$a)
})
