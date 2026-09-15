skip_on_cran()

test_that("prep_prepare_dataframes works", {
  skip_on_cran()

  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  withr::local_options(
    dataquieR.CONDITIONS_WITH_STACKTRACE = TRUE,
    dataquieR.ERRORS_WITH_CALLER = TRUE,
    dataquieR.WARNINGS_WITH_CALLER = TRUE,
    dataquieR.MESSAGES_WITH_CALLER = TRUE,
    dataquieR.guess_missing_codes = TRUE
  )
  acc_test1 <- function(
    resp_variable, aux_variable,
    time_variable, co_variables,
    group_vars, study_data, meta_data
  ) {
    prep_prepare_dataframes()
    invisible(ds1)
  }
  acc_test2 <- function(
    resp_variable, aux_variable,
    time_variable, co_variables,
    group_vars, study_data, meta_data, label_col
  ) {
    ds1 <- prep_prepare_dataframes(study_data, meta_data)
    invisible(ds1)
  }
  environment(acc_test1) <- asNamespace("dataquieR")
  environment(acc_test2) <- asNamespace("dataquieR")
  acc_test3 <- function(
    resp_variable, aux_variable, time_variable,
    co_variables, group_vars, study_data, meta_data,
    label_col
  ) {
    prep_prepare_dataframes()
    invisible(ds1)
  }
  acc_test4 <- function(
    resp_variable, aux_variable, time_variable,
    co_variables, group_vars, study_data, meta_data,
    label_col
  ) {
    ds1 <- prep_prepare_dataframes(study_data, meta_data)
    invisible(ds1)
  }
  environment(acc_test3) <- asNamespace("dataquieR")
  environment(acc_test4) <- asNamespace("dataquieR")
  meta_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data.RData") # nolint: line_length_linter.
  study_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData", # nolint: line_length_linter.
    keep_types = TRUE
  )
  meta_data2 <-
    prep_scalelevel_from_data_and_metadata(
      study_data = study_data,
      meta_data = meta_data
    )
  meta_data[[SCALE_LEVEL]] <-
    setNames(meta_data2[[SCALE_LEVEL]], nm = meta_data2[[VAR_NAMES]])[
      meta_data[[VAR_NAMES]]
    ]

  expect_error(
    acc_test1(),
    regexp = "Need study data as a data frame"
  )
  expect_error(
    acc_test2(),
    regexp = "Need study data as a data frame"
  )
  expect_warning(
    acc_test1(study_data = study_data),
    regexp =
      sprintf(
        "(?ms).*(%s|%s|%s|%s).*",
        paste(
          "Missing .+meta_data.+, try to guess a preliminary one from the",
          "data using .+prep_study2meta.+. Please consider amending",
          "this minimum guess manually."
        ),
        paste(
          "Metadata does not provide a filled column called",
          ".+MISSING_LIST.+ for replacing codes with NAs."
        ),
        paste(
          "Metadata does not provide a filled column called .+JUMP_LIST.+",
          "for replacing codes with NAs."
        ),
        paste(
          "Some code labels or -values",
          "are missing from .+meta_data.+."
        )
      ),
    perl = TRUE
  )
  expect_error(
    acc_test1(meta_data = meta_data),
    regexp = "Need study data as a data frame"
  )
  expect_error(
    acc_test2(study_data = 12, meta_data = meta_data),
    regexp = "Need study data as a data frame"
  )
  expect_equal(
    colnames(acc_test1(
      study_data = study_data,
      meta_data = meta_data
    )),
    c(
      "v00000",
      "v00001",
      "v00002",
      "v00003",
      "v00103",
      "v01003",
      "v01002",
      "v10000",
      "v00004",
      "v00005",
      "v00006",
      "v00007",
      "v00008",
      "v00009",
      "v00109",
      "v00010",
      "v00011",
      "v00012",
      "v00013",
      "v20000",
      "v00014",
      "v00015",
      "v00016",
      "v00017",
      "v30000",
      "v00018",
      "v01018",
      "v00019",
      "v00020",
      "v00021",
      "v00022",
      "v00023",
      "v00024",
      "v00025",
      "v00026",
      "v00027",
      "v00028",
      "v00029",
      "v00030",
      "v00031",
      "v00032",
      "v00033",
      "v40000",
      "v00034",
      "v00035",
      "v00036",
      "v00037",
      "v00038",
      "v00039",
      "v00040",
      "v00041",
      "v00042",
      "v50000"
    )
  )
  expect_equal(
    colnames(acc_test2(
      study_data = study_data,
      meta_data = meta_data
    )),
    c(
      "v00000",
      "v00001",
      "v00002",
      "v00003",
      "v00103",
      "v01003",
      "v01002",
      "v10000",
      "v00004",
      "v00005",
      "v00006",
      "v00007",
      "v00008",
      "v00009",
      "v00109",
      "v00010",
      "v00011",
      "v00012",
      "v00013",
      "v20000",
      "v00014",
      "v00015",
      "v00016",
      "v00017",
      "v30000",
      "v00018",
      "v01018",
      "v00019",
      "v00020",
      "v00021",
      "v00022",
      "v00023",
      "v00024",
      "v00025",
      "v00026",
      "v00027",
      "v00028",
      "v00029",
      "v00030",
      "v00031",
      "v00032",
      "v00033",
      "v40000",
      "v00034",
      "v00035",
      "v00036",
      "v00037",
      "v00038",
      "v00039",
      "v00040",
      "v00041",
      "v00042",
      "v50000"
    )
  )
  expect_equal(
    colnames(acc_test3(
      study_data = study_data,
      meta_data = meta_data
    )),
    c(
      "v00000",
      "v00001",
      "v00002",
      "v00003",
      "v00103",
      "v01003",
      "v01002",
      "v10000",
      "v00004",
      "v00005",
      "v00006",
      "v00007",
      "v00008",
      "v00009",
      "v00109",
      "v00010",
      "v00011",
      "v00012",
      "v00013",
      "v20000",
      "v00014",
      "v00015",
      "v00016",
      "v00017",
      "v30000",
      "v00018",
      "v01018",
      "v00019",
      "v00020",
      "v00021",
      "v00022",
      "v00023",
      "v00024",
      "v00025",
      "v00026",
      "v00027",
      "v00028",
      "v00029",
      "v00030",
      "v00031",
      "v00032",
      "v00033",
      "v40000",
      "v00034",
      "v00035",
      "v00036",
      "v00037",
      "v00038",
      "v00039",
      "v00040",
      "v00041",
      "v00042",
      "v50000"
    )
  )
  expect_equal(
    colnames(acc_test4(
      study_data = study_data,
      meta_data = meta_data
    )),
    c(
      "v00000",
      "v00001",
      "v00002",
      "v00003",
      "v00103",
      "v01003",
      "v01002",
      "v10000",
      "v00004",
      "v00005",
      "v00006",
      "v00007",
      "v00008",
      "v00009",
      "v00109",
      "v00010",
      "v00011",
      "v00012",
      "v00013",
      "v20000",
      "v00014",
      "v00015",
      "v00016",
      "v00017",
      "v30000",
      "v00018",
      "v01018",
      "v00019",
      "v00020",
      "v00021",
      "v00022",
      "v00023",
      "v00024",
      "v00025",
      "v00026",
      "v00027",
      "v00028",
      "v00029",
      "v00030",
      "v00031",
      "v00032",
      "v00033",
      "v40000",
      "v00034",
      "v00035",
      "v00036",
      "v00037",
      "v00038",
      "v00039",
      "v00040",
      "v00041",
      "v00042",
      "v50000"
    )
  )

  expect_equal(
    colnames(
      acc_test3(
        study_data = study_data,
        meta_data = meta_data,
        label_col = "LABEL"
      )
    ),
    c(
      "CENTER_0",
      "PSEUDO_ID",
      "SEX_0",
      "AGE_0",
      "AGE_GROUP_0",
      "AGE_1",
      "SEX_1",
      "PART_STUDY",
      "SBP_0",
      "DBP_0",
      "GLOBAL_HEALTH_VAS_0",
      "ASTHMA_0",
      "VO2_CAPCAT_0",
      "ARM_CIRC_0",
      "ARM_CIRC_DISC_0",
      "ARM_CUFF_0",
      "USR_VO2_0",
      "USR_BP_0",
      "EXAM_DT_0",
      "PART_PHYS_EXAM",
      "CRP_0",
      "BSG_0",
      "DEV_NO_0",
      "LAB_DT_0",
      "PART_LAB",
      "EDUCATION_0",
      "EDUCATION_1",
      "FAM_STAT_0",
      "MARRIED_0",
      "N_CHILD_0",
      "EATING_PREFS_0",
      "MEAT_CONS_0",
      "SMOKING_0",
      "SMOKE_SHOP_0",
      "N_INJURIES_0",
      "N_BIRTH_0",
      "INCOME_GROUP_0",
      "PREGNANT_0",
      "MEDICATION_0",
      "N_ATC_CODES_0",
      "USR_SOCDEM_0",
      "INT_DT_0",
      "PART_INTERVIEW",
      "ITEM_1_0",
      "ITEM_2_0",
      "ITEM_3_0",
      "ITEM_4_0",
      "ITEM_5_0",
      "ITEM_6_0",
      "ITEM_7_0",
      "ITEM_8_0",
      "QUEST_DT_0",
      "PART_QUESTIONNAIRE"
    )
  )

  expect_equal(
    colnames(
      acc_test3(
        study_data = study_data,
        meta_data = meta_data,
        label_col = LABEL
      )
    ),
    c(
      "CENTER_0",
      "PSEUDO_ID",
      "SEX_0",
      "AGE_0",
      "AGE_GROUP_0",
      "AGE_1",
      "SEX_1",
      "PART_STUDY",
      "SBP_0",
      "DBP_0",
      "GLOBAL_HEALTH_VAS_0",
      "ASTHMA_0",
      "VO2_CAPCAT_0",
      "ARM_CIRC_0",
      "ARM_CIRC_DISC_0",
      "ARM_CUFF_0",
      "USR_VO2_0",
      "USR_BP_0",
      "EXAM_DT_0",
      "PART_PHYS_EXAM",
      "CRP_0",
      "BSG_0",
      "DEV_NO_0",
      "LAB_DT_0",
      "PART_LAB",
      "EDUCATION_0",
      "EDUCATION_1",
      "FAM_STAT_0",
      "MARRIED_0",
      "N_CHILD_0",
      "EATING_PREFS_0",
      "MEAT_CONS_0",
      "SMOKING_0",
      "SMOKE_SHOP_0",
      "N_INJURIES_0",
      "N_BIRTH_0",
      "INCOME_GROUP_0",
      "PREGNANT_0",
      "MEDICATION_0",
      "N_ATC_CODES_0",
      "USR_SOCDEM_0",
      "INT_DT_0",
      "PART_INTERVIEW",
      "ITEM_1_0",
      "ITEM_2_0",
      "ITEM_3_0",
      "ITEM_4_0",
      "ITEM_5_0",
      "ITEM_6_0",
      "ITEM_7_0",
      "ITEM_8_0",
      "QUEST_DT_0",
      "PART_QUESTIONNAIRE"
    )
  )

  expect_equal(
    colnames(
      acc_test4(
        study_data = study_data,
        meta_data = meta_data,
        label_col = LABEL
      )
    ),
    c(
      "CENTER_0",
      "PSEUDO_ID",
      "SEX_0",
      "AGE_0",
      "AGE_GROUP_0",
      "AGE_1",
      "SEX_1",
      "PART_STUDY",
      "SBP_0",
      "DBP_0",
      "GLOBAL_HEALTH_VAS_0",
      "ASTHMA_0",
      "VO2_CAPCAT_0",
      "ARM_CIRC_0",
      "ARM_CIRC_DISC_0",
      "ARM_CUFF_0",
      "USR_VO2_0",
      "USR_BP_0",
      "EXAM_DT_0",
      "PART_PHYS_EXAM",
      "CRP_0",
      "BSG_0",
      "DEV_NO_0",
      "LAB_DT_0",
      "PART_LAB",
      "EDUCATION_0",
      "EDUCATION_1",
      "FAM_STAT_0",
      "MARRIED_0",
      "N_CHILD_0",
      "EATING_PREFS_0",
      "MEAT_CONS_0",
      "SMOKING_0",
      "SMOKE_SHOP_0",
      "N_INJURIES_0",
      "N_BIRTH_0",
      "INCOME_GROUP_0",
      "PREGNANT_0",
      "MEDICATION_0",
      "N_ATC_CODES_0",
      "USR_SOCDEM_0",
      "INT_DT_0",
      "PART_INTERVIEW",
      "ITEM_1_0",
      "ITEM_2_0",
      "ITEM_3_0",
      "ITEM_4_0",
      "ITEM_5_0",
      "ITEM_6_0",
      "ITEM_7_0",
      "ITEM_8_0",
      "QUEST_DT_0",
      "PART_QUESTIONNAIRE"
    )
  )
  expect_error(
    acc_test2(study_data = NULL, meta_data = meta_data),
    regexp = "Need study data as a data frame"
  )
  expect_error(
    prep_prepare_dataframes(.replace_missings = cars),
    regexp = paste(".replace_missings needs to be 1 logical value."),
    fixed = TRUE
  )

  meta_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data.RData") # nolint: line_length_linter.
  study_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData", # nolint: line_length_linter.
    keep_types = TRUE
  )
  meta_data2 <-
    prep_scalelevel_from_data_and_metadata(
      study_data = study_data,
      meta_data = meta_data
    )
  meta_data[[SCALE_LEVEL]] <-
    setNames(meta_data2[[SCALE_LEVEL]], nm = meta_data2[[VAR_NAMES]])[
      meta_data[[VAR_NAMES]]
    ]
  err_test1 <- function(
    resp_variable, aux_variable,
    time_variable, co_variables,
    group_vars, study_data, meta_data, label_col
  ) {
    prep_prepare_dataframes()
    invisible(ds1)
  }
  environment(err_test1) <- asNamespace("dataquieR")

  if (exists("testxx")) rm(testxx)
  expect_error(err_test1(study_data = cars, label_col = testxx),
    regexp = "Cannot resolve .+label_col = testxx.+",
    perl = TRUE
  )


  sd99 <- study_data
  err_test2 <- function(
    resp_variable, aux_variable,
    time_variable, co_variables,
    group_vars, study_data = sd99,
    meta_data, label_col
  ) {
    prep_prepare_dataframes()
    invisible(ds1)
  }
  environment(err_test2) <- asNamespace("dataquieR")

  sd99 <- 42

  expect_error(err_test2(meta_data = meta_data),
    regexp = "Need study data as a data frame"
  )

  err_test3 <- function(
    resp_variable, aux_variable,
    time_variable, co_variables,
    group_vars,
    meta_data, label_col
  ) {
    prep_prepare_dataframes()
    invisible(ds1)
  }
  environment(err_test3) <- asNamespace("dataquieR")
  if (exists("study_data")) rm(study_data)
  expect_error(err_test3(meta_data = meta_data),
    regexp = "Need study data as a data frame"
  )
  study_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData", # nolint: line_length_linter.
    keep_types = TRUE
  )
  md99 <- meta_data
  err_test4 <- function(
    resp_variable, aux_variable,
    time_variable, co_variables,
    group_vars, study_data,
    meta_data = md99, label_col
  ) {
    prep_prepare_dataframes()
    invisible(ds1)
  }
  environment(err_test4) <- asNamespace("dataquieR")

  expect_silent(err_test4(study_data = study_data))
  md99 <- 42
  expect_warning(err_test4(study_data = study_data),
    regexp = "guess a preliminary"
  )
  err_test5 <- function(
    resp_variable, aux_variable,
    time_variable, co_variables,
    group_vars, study_data,
    meta_data, label_col
  ) {
    prep_prepare_dataframes()
    invisible(ds1)
  }
  environment(err_test5) <- asNamespace("dataquieR")
  expect_warning(err_test5(study_data = study_data),
    regexp = sprintf(
      "(%s|%s|%s)",
      paste(
        "Missing .+meta_data.+, try to guess a",
        "preliminary one from the data using",
        ".+prep_study2meta.+. Please",
        "consider amending this minimum guess",
        "manually."
      ),
      paste(
        "Metadata does not provide a filled",
        "column called .+JUMP_LIST.+ for",
        "replacing codes with NAs."
      ),
      paste(
        "Some code labels or -values",
        "are missing from .+meta_data.+."
      )
    ),
    perl = TRUE
  )


  study_data <- tibble::as_tibble(study_data)
  meta_data <- tibble::as_tibble(meta_data)
  acc_test5 <- function(
    resp_variable, aux_variable, time_variable,
    co_variables, group_vars, study_data, meta_data,
    label_col
  ) {
    expect_true(tibble::is_tibble(study_data))
    expect_true(tibble::is_tibble(meta_data))
    prep_prepare_dataframes()
    expect_false(tibble::is_tibble(study_data))
    expect_false(tibble::is_tibble(meta_data))
    expect_false(tibble::is_tibble(ds1))
    invisible(ds1)
  }
  environment(acc_test5) <- asNamespace("dataquieR")

  x <- acc_test5(
    study_data = study_data,
    meta_data = meta_data, label_col = LABEL
  )
  y <- acc_test5(
    study_data = tibble::as_tibble(x),
    meta_data = meta_data, label_col = LABEL
  )
  attr(x, "dataquieR_preparation_signature") <- NULL
  attr(y, "dataquieR_preparation_signature") <- NULL
  expect_identical(x, y) # because this has already been mapped.
  md99 <- meta_data
  md99$VAR_NAMES <- NULL
  expect_error(
    y <- acc_test5(
      study_data = study_data,
      meta_data = md99, label_col = LABEL
    ),
    regexp = paste(
      "Missing columns.+VAR_NAMES.+from .+meta_data.*"
    ),
    perl = TRUE
  )
  sd99 <- study_data
  md99 <- meta_data
  colnames(sd99) <- sprintf("VAR_%06d", seq_len(ncol(sd99)))
  withr::with_options(
    list(dataquieR.ELEMENT_MISSMATCH_CHECKTYPE = "exact"),
    expect_conditions(
      expect_error(
        acc_test3(study_data = sd99, meta_data = md99),
        regexp = "No data left. Aborting"
      ),
      regexps = c(
        paste(
          "Lost 100. of the study data because of",
          "missing.not assignable metadata"
        ),
        paste(
          "Lost 100. of the metadata because of",
          "missing.not assignable study.data"
        ),
        paste(
          "Did not find any metadata for the following",
          "variables from the study data. .+VAR_"
        ),
        paste(
          "Found metadata for the following variables not",
          "found in the study data: .+v00000.+, .+v00001.+,",
          ".+v00002.+, .+v00003.+, .+v00004.+"
        )
      ),
      classes = c("warning", "warning", "message", "message")
    )
  )
})

test_that("prep_prepare_dataframes assigns a fallback study segment", {
  skip_on_cran()

  study_data <- data.frame(x = 1:3)
  meta_data <- prep_study2meta(study_data)
  meta_data[[STUDY_SEGMENT]] <- NA_character_

  expect_message(
    result <- suppressWarnings(prep_prepare_dataframes(
      .study_data = study_data,
      .meta_data = meta_data,
      .label_col = VAR_NAMES,
      .internal = FALSE
    )),
    "artificial segment"
  )

  expect_s3_class(result, "dataquieR_data_frame_prepared")
  expect_equal(result$x, study_data$x)
})

test_that("prep_prepare_dataframes warns about invalid participation values", {
  skip_on_cran()

  study_data <- data.frame(part = c(1, 2, 0), x = c(4, 5, 6))
  meta_data <- prep_create_meta(
    VAR_NAMES = c("part", "x"),
    DATA_TYPE = DATA_TYPES$INTEGER,
    MISSING_LIST = SPLIT_CHAR,
    JUMP_LIST = SPLIT_CHAR
  )
  meta_data[[PART_VAR]] <- c("", "part")

  seen <- new.env(parent = emptyenv())
  seen$warnings <- character()
  result <- withCallingHandlers(
    suppressMessages(prep_prepare_dataframes(
      .study_data = study_data,
      .meta_data = meta_data,
      .label_col = VAR_NAMES,
      .internal = FALSE
    )),
    warning = function(w) {
      seen$warnings <- c(seen$warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  expect_s3_class(result, "dataquieR_data_frame_prepared")
  expect_true(any(grepl(
    "Found entries different from TRUE/FALSE/1/0",
    seen$warnings,
    fixed = TRUE
  )))
})

test_that("raw study-data attributes require matching mapped columns", {
  meta_data <- prep_create_meta(
    VAR_NAMES = c("raw_x", "raw_y"),
    LABEL = c("X", "Y"),
    DATA_TYPE = DATA_TYPES$INTEGER,
    MISSING_LIST = SPLIT_CHAR,
    JUMP_LIST = SPLIT_CHAR
  )
  ds1 <- data.frame(X = 1:2, Y = 3:4)
  attr(ds1, "label_col") <- LABEL
  raw <- data.frame(raw_x = 1:2, raw_y = 3:4)

  expect_true(util_raw_study_data_attr_matches(ds1, raw, meta_data))
  expect_equal(
    util_raw_study_data_attr_subset(ds1, raw, meta_data),
    raw
  )

  expect_false(util_raw_study_data_attr_matches(
    ds1,
    raw[, "raw_x", drop = FALSE],
    meta_data
  ))
  expect_null(util_raw_study_data_attr_subset(
    ds1,
    raw[, "raw_x", drop = FALSE],
    meta_data
  ))

  raw_extra_row <- raw[c(1, 2, 2), , drop = FALSE]
  row.names(raw_extra_row) <- NULL
  expect_false(util_raw_study_data_attr_matches(ds1, raw_extra_row, meta_data))

  raw_wrong_rows <- raw
  row.names(raw_wrong_rows) <- c("a", "b")
  expect_null(util_raw_study_data_attr_subset(
    ds1,
    raw_wrong_rows,
    meta_data
  ))

  ds1_with_unknown_label_col <- ds1
  attr(ds1_with_unknown_label_col, "label_col") <- "UNKNOWN_LABEL"
  expect_null(util_raw_study_data_mapped_colnames(
    ds1_with_unknown_label_col,
    meta_data
  ))
  expect_false(util_raw_study_data_attr_matches(
    ds1_with_unknown_label_col,
    raw,
    meta_data
  ))
})

test_that("generated metadata table names do not fragment preparation cache", {
  util_purge_study_data_cache()
  withr::defer(util_purge_study_data_cache())

  x <- data.frame(a = 1)
  y <- structure(
    data.frame(a = 1),
    dataquieR_data_frame_loaded_at = Sys.time()
  )
  expect_identical(util_study_data_cache_hash(x), util_study_data_cache_hash(y))

  non_data_state <- list(
    env = new.env(parent = emptyenv()),
    fun = function(x) x,
    call = quote(a + b),
    condition = simpleError("cache")
  )
  equivalent_non_data_state <- list(
    env = new.env(parent = emptyenv()),
    fun = function(y) y,
    call = quote(a + b),
    condition = simpleError("cache")
  )
  expect_identical(
    util_study_data_cache_hash(non_data_state),
    util_study_data_cache_hash(equivalent_non_data_state)
  )

  deep_state <- "value"
  for (i in seq_len(22)) {
    deep_state <- list(deep_state)
  }
  expect_type(util_study_data_cache_hash(deep_state), "character")

  metrics <- new.env(parent = emptyenv())
  withr::local_options(list(
    dataquieR.study_data_cache_metrics = TRUE,
    dataquieR.study_data_cache_metrics_env = metrics
  ))

  study_data <- data.frame(a = c("1", "999"), b = c("2", "999"))
  meta_data <- data.frame(
    VAR_NAMES = c("a", "b"),
    LABEL = c("A", "B"),
    DATA_TYPE = DATA_TYPES$INTEGER,
    SCALE_LEVEL = SCALE_LEVELS$RATIO,
    MISSING_LIST = "999",
    JUMP_LIST = ""
  )

  prepare_minimal <- function(meta_data_arg = meta_data) {
    suppressWarnings(suppressMessages(prep_prepare_dataframes(
      .study_data = study_data,
      .meta_data = meta_data_arg,
      .label_col = LABEL,
      .replace_hard_limits = FALSE,
      .replace_missings = FALSE,
      .adjust_data_type = TRUE,
      .amend_scale_level = FALSE,
      .apply_factor_metadata = FALSE,
      .apply_factor_metadata_inadm = FALSE
    )))
  }

  prepared_once <- prepare_minimal()
  signature <- util_attr(
    prepared_once,
    "dataquieR_preparation_signature",
    exact = TRUE
  )
  prepared_twice <- prepare_minimal()

  expect_identical(
    util_attr(prepared_twice, "dataquieR_preparation_signature", exact = TRUE),
    signature
  )
  expect_equal(metrics$usage[[signature]], 1)
  expect_equal(length(ls(.study_data_cache)), 1)

  meta_data_other_missing <- meta_data
  meta_data_other_missing[[MISSING_LIST]] <- "888"
  prepared_with_other_missing <- prepare_minimal(meta_data_other_missing)

  expect_false(identical(
    util_attr(
      prepared_with_other_missing,
      "dataquieR_preparation_signature",
      exact = TRUE
    ),
    signature
  ))
})

test_that("prep_prepare_dataframes does not reuse stale raw study_data attributes", { # nolint: line_length_linter.
  study_data <- data.frame(
    x = c(1L, -99L, 3L),
    y = c(1L, 2L, 3L)
  )
  meta_data <- data.frame(
    VAR_NAMES = c("x", "y"),
    LABEL = c("x", "y"),
    DATA_TYPE = c("integer", "integer"),
    SCALE_LEVEL = c("metric", "metric"),
    VARIABLE_ROLE = c("primary", "primary"),
    MISSING_LIST = c("missing = -99", ""),
    JUMP_LIST = c("", ""),
    HARD_LIMITS = c("", ""),
    stringsAsFactors = FALSE
  )

  ds1 <- suppressWarnings(suppressMessages(prep_prepare_dataframes(
    .study_data = study_data,
    .meta_data = meta_data,
    .label_col = LABEL,
    .replace_missings = FALSE,
    .replace_hard_limits = FALSE
  )))

  ds1_subset <- ds1[1:2, , drop = FALSE]
  expect_true(isTRUE(util_attr(ds1_subset, "MAPPED", exact = TRUE)))
  expect_equal(nrow(ds1_subset), 2)
  expect_equal(nrow(util_attr(ds1_subset, "study_data", exact = TRUE)), 2)

  reused <- suppressWarnings(suppressMessages(prep_prepare_dataframes(
    .study_data = ds1_subset,
    .meta_data = meta_data,
    .label_col = LABEL,
    .replace_missings = FALSE,
    .replace_hard_limits = FALSE
  )))
  expect_equal(nrow(reused), 2)

  ds1_extra_raw_attr <- ds1
  attr(ds1_extra_raw_attr, "study_data")$z <- c(10L, 20L, 30L)

  rebuilt_from_extra_raw <- suppressWarnings(suppressMessages(
    prep_prepare_dataframes(
      .study_data = ds1_extra_raw_attr,
      .meta_data = meta_data,
      .label_col = LABEL,
      .replace_missings = TRUE,
      .replace_hard_limits = FALSE
    )
  ))
  expect_equal(colnames(rebuilt_from_extra_raw), colnames(ds1))
  expect_false("z" %in% colnames(rebuilt_from_extra_raw))

  ds1_reordered_attr <- ds1
  attr(ds1_reordered_attr, "study_data") <-
    util_attr(ds1, "study_data", exact = TRUE)[, c("y", "x"), drop = FALSE]

  rebuilt_from_reordered_raw <- suppressWarnings(suppressMessages(
    prep_prepare_dataframes(
      .study_data = ds1_reordered_attr,
      .meta_data = meta_data,
      .label_col = LABEL,
      .replace_missings = TRUE,
      .replace_hard_limits = FALSE
    )
  ))
  expect_equal(colnames(rebuilt_from_reordered_raw), colnames(ds1))
  expect_equal(
    colnames(util_attr(rebuilt_from_reordered_raw, "study_data", exact = TRUE)),
    colnames(rebuilt_from_reordered_raw)
  )

  ds1_subset_stale <- ds1_subset
  attr(ds1_subset_stale, "study_data") <- util_attr(ds1, "study_data",
    exact = TRUE
  )

  expect_error(
    suppressWarnings(suppressMessages(prep_prepare_dataframes(
      .study_data = ds1_subset_stale,
      .meta_data = meta_data,
      .label_col = LABEL,
      .replace_missings = TRUE,
      .replace_hard_limits = FALSE
    ))),
    regexp = "attribute is missing or stale"
  )
})

test_that("raw study_data attribute helpers map label columns", {
  skip_on_cran()

  ds1 <- data.frame(
    `x label` = 1:3,
    `y label` = 4:6,
    check.names = FALSE
  )
  attr(ds1, "label_col") <- LABEL
  meta_data <- prep_create_meta(
    VAR_NAMES = c("x", "y"),
    LABEL = c("x label", "y label"),
    DATA_TYPE = DATA_TYPES$INTEGER,
    MISSING_LIST = ""
  )
  raw_study_data <- data.frame(
    x = 1:3,
    y = 4:6,
    z = 7:9
  )

  expect_false(util_raw_study_data_attr_matches(
    ds1 = ds1,
    raw_study_data = raw_study_data,
    meta_data = meta_data
  ))

  subset <- util_raw_study_data_attr_subset(
    ds1 = ds1,
    raw_study_data = raw_study_data,
    meta_data = meta_data
  )

  expect_equal(subset, raw_study_data[c("x", "y")])

  row.names(raw_study_data) <- c("a", "b", "c")
  expect_null(util_raw_study_data_attr_subset(
    ds1 = ds1,
    raw_study_data = raw_study_data,
    meta_data = meta_data
  ))
})

test_that("prep_prepare_dataframes ignores irrelevant metadata duplicates", {
  study_data <- data.frame(a = 1:3, b = 4:6)
  meta_data <- prep_create_meta(
    VAR_NAMES = c("a", "b"),
    DATA_TYPE = DATA_TYPES$INTEGER,
    MISSING_LIST = ""
  )

  meta_data_with_irrelevant_duplicates <- rbind(
    meta_data,
    meta_data[1:2, , drop = FALSE]
  )
  meta_data_with_irrelevant_duplicates[[VAR_NAMES]][3:4] <- "extra"

  expect_silent(suppressMessages(suppressWarnings(
    prep_prepare_dataframes(
      study_data,
      meta_data_with_irrelevant_duplicates,
      .replace_missings = FALSE,
      .adjust_data_type = FALSE
    )
  )))

  meta_data_with_relevant_duplicates <- meta_data
  meta_data_with_relevant_duplicates[[VAR_NAMES]][2] <- "a"
  meta_data_with_relevant_duplicates[[LABEL]][2] <- "Conflicting label"

  expect_error(
    suppressWarnings(
      prep_prepare_dataframes(
        study_data,
        meta_data_with_relevant_duplicates,
        .replace_missings = FALSE,
        .adjust_data_type = FALSE
      )
    ),
    "Found duplicated"
  )
})

test_that("prep_prepare_dataframes applies local value-table metadata", {
  skip_on_cran()

  with_dataframe_environment({
    prep_add_data_frames(value_table = data.frame(
      CODE_VALUE = c("1", "2"),
      CODE_LABEL = c("One", "Two"),
      stringsAsFactors = FALSE
    ))
    study_data <- data.frame(
      x = c("1", "2", "999", "777", "3"),
      stringsAsFactors = FALSE
    )
    meta_data <- data.frame(
      VAR_NAMES = "x",
      LABEL = "X",
      DATA_TYPE = DATA_TYPES$STRING,
      SCALE_LEVEL = SCALE_LEVELS$NOMINAL,
      VALUE_LABEL_TABLE = "value_table",
      MISSING_LIST = "Missing = 999",
      JUMP_LIST = "Jump = 777",
      VARIABLE_ROLE = "primary",
      STUDY_SEGMENT = "SEG",
      stringsAsFactors = FALSE
    )

    kept <- suppressMessages(prep_prepare_dataframes(
      .study_data = study_data,
      .meta_data = meta_data,
      .label_col = LABEL,
      .replace_missings = FALSE,
      .adjust_data_type = FALSE,
      .apply_factor_metadata = TRUE,
      .apply_factor_metadata_inadm = TRUE,
      .internal = FALSE
    ))

    expect_s3_class(kept, "dataquieR_data_frame_prepared")
    expect_equal(
      as.character(kept$X),
      c("One", "Two", "999", "777", "3")
    )
    expect_equal(levels(kept$X), c("One", "Two", "999", "777", "3"))
    expect_identical(attr(kept$X, "hint", exact = TRUE), character(0))

    seen <- new.env(parent = emptyenv())
    seen$messages <- character()
    removed <- withCallingHandlers(
      prep_prepare_dataframes(
        .study_data = study_data,
        .meta_data = meta_data,
        .label_col = LABEL,
        .replace_missings = FALSE,
        .adjust_data_type = FALSE,
        .apply_factor_metadata = TRUE,
        .apply_factor_metadata_inadm = FALSE,
        .internal = FALSE
      ),
      message = function(msg) {
        seen$messages <- c(seen$messages, conditionMessage(msg))
        invokeRestart("muffleMessage")
      }
    )

    expect_equal(
      as.character(removed$X),
      c("One", "Two", NA_character_, NA_character_, NA_character_)
    )
    expect_true(any(grepl(
      "inadmissible categorical values",
      seen$messages,
      fixed = TRUE
    )))
    expect_match(
      paste(attr(removed$X, "hint", exact = TRUE), collapse = " "),
      "999.+777.+3",
      perl = TRUE
    )
  })
})

test_that("prep_prepare_dataframes requires code levels in value tables", {
  skip_on_cran()

  with_dataframe_environment({
    prep_add_data_frames(value_table = data.frame(
      CODE_LABEL = c("One", "Two"),
      stringsAsFactors = FALSE
    ))
    study_data <- data.frame(
      x = c("1", "2"),
      stringsAsFactors = FALSE
    )
    meta_data <- data.frame(
      VAR_NAMES = "x",
      LABEL = "X",
      DATA_TYPE = DATA_TYPES$STRING,
      SCALE_LEVEL = SCALE_LEVELS$NOMINAL,
      VALUE_LABEL_TABLE = "value_table",
      MISSING_LIST = "",
      JUMP_LIST = "",
      VARIABLE_ROLE = "primary",
      STUDY_SEGMENT = "SEG",
      stringsAsFactors = FALSE
    )

    expect_error(
      prep_prepare_dataframes(
        .study_data = study_data,
        .meta_data = meta_data,
        .label_col = LABEL,
        .replace_missings = FALSE,
        .adjust_data_type = FALSE,
        .apply_factor_metadata = TRUE,
        .internal = FALSE
      ),
      "Have only code labels, but not code levels"
    )
  })
})
