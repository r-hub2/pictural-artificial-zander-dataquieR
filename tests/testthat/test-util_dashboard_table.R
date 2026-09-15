skip_on_cran()

test_that("overview summary links resolve to report-by subreports", {
  skip_on_cran()

  tb <- data.frame(
    VAR_NAMES = "age",
    call_names = "com_item_missingness",
    title_ind = NA_character_,
    `..Origin` = "ship-SEX_0_1-SHIP subset",
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  this <- list(
    label_col = LABEL,
    meta_data = data.frame(
      VAR_NAMES = "age",
      LABEL = "AGE_0",
      stringsAsFactors = FALSE
    )
  )
  folder_of_report <- c(
    "ship-SEX_0_1-SHIP subset-AGE_0.com_item_missingness" =
      "report_ship_SEX_0_1_SHIPsubset"
  )

  linked <- util_add_links_to_summary_table(
    tb,
    this,
    folder_of_report = folder_of_report
  )

  expect_identical(
    linked$href,
    "report_ship_SEX_0_1_SHIPsubset/.report/VAR_AGE0.html#AGE0.com_item_missingness" # nolint: line_length_linter.
  )
  expect_identical(
    linked$popup_href,
    "report_ship_SEX_0_1_SHIPsubset/.report/VAR_AGE0.html#nm=com_item_missingness.AGE_0" # nolint: line_length_linter.
  )
})

test_that("overview summary links keep existing labels when remapping is absent", { # nolint: line_length_linter.
  skip_on_cran()

  tb <- data.frame(
    VAR_NAMES = "ship-SEX_0_1-SHIP subset - AGE_0",
    LABEL = "ship-SEX_0_1-SHIP subset - AGE_0",
    call_names = "com_item_missingness",
    title_ind = NA_character_,
    `..Origin` = "ship-SEX_0_1-SHIP subset",
    `Variable Label.1` = "AGE_0",
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  this <- list(
    label_col = LABEL,
    meta_data = data.frame(
      VAR_NAMES = "age",
      LABEL = "AGE_0",
      stringsAsFactors = FALSE
    )
  )
  folder_of_report <- c(
    "ship-SEX_0_1-SHIP subset-AGE_0.com_item_missingness" =
      "report_ship_SEX_0_1_SHIPsubset"
  )

  linked <- util_add_links_to_summary_table(
    tb,
    this,
    folder_of_report = folder_of_report
  )

  expect_true(startsWith(linked$href, "report_ship_SEX_0_1_SHIPsubset/.report/")) # nolint: line_length_linter.
  expect_false(grepl("NA/.report", linked$href, fixed = TRUE))
})

test_that("mixed item and cross-item rows do not poison dashboard labels", {
  skip_on_cran()

  tb <- data.frame(
    VAR_NAMES = c("age", "12"),
    LABEL = c(NA_character_, "Scale 12"),
    call_names = c("com_item_missingness", "con_contradictions_redcap"),
    title_ind = NA_character_,
    `..Origin` = c("study", NA_character_),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  this <- list(
    label_col = LABEL,
    meta_data = data.frame(
      VAR_NAMES = "age",
      LABEL = "AGE_0",
      stringsAsFactors = FALSE
    )
  )
  folder_of_report <- c(
    "study-AGE_0.com_item_missingness" = "report_study"
  )

  linked <- util_add_links_to_summary_table(
    tb,
    this,
    folder_of_report = folder_of_report
  )

  expect_identical(linked[[LABEL]], c("AGE_0", "Scale 12"))
  expect_match(
    linked$href[[1]],
    "^report_study/\\.report/VAR_AGE0\\.html#"
  )
  expect_false(any(grepl("VAR_NA", linked$href, fixed = TRUE)))
  expect_match(linked$href[[2]], "^VAR_Scale12\\.html#")
})

test_that("unrequested group combinations do not link to missing subreports", {
  tb <- data.frame(
    VAR_NAMES = "group_1",
    LABEL = "Group one",
    call_names = "des_scatterplot_matrix",
    indicator_metric = "EMPTY_OUTPUT",
    title_ind = NA_character_,
    stringsAsFactors = FALSE
  )
  this <- list(
    label_col = LABEL,
    meta_data = data.frame(
      VAR_NAMES = character(),
      LABEL = character(),
      stringsAsFactors = FALSE
    ),
    meta_data_cross_item = data.frame()
  )

  linked <- util_add_links_to_summary_table(
    tb,
    this,
    folder_of_report = c(
      "Group one.con_contradictions_redcap" = "report_study"
    ),
    vars_to_include = "variable_group"
  )

  expect_identical(linked$href, "")
  expect_identical(linked$popup_href, "")
})

test_that("util_add_links_to_summary_table handles empty summary tables", {
  meta_data <- data.frame(
    VAR_NAMES = character(0),
    LABEL = character(0),
    stringsAsFactors = FALSE
  )
  tb <- data.frame(
    VAR_NAMES = character(0),
    LABEL = character(0),
    call_names = character(0),
    title_ind = character(0),
    stringsAsFactors = FALSE
  )

  linked <- util_add_links_to_summary_table(
    tb,
    list(label_col = LABEL, meta_data = meta_data)
  )

  expect_named(linked, c(VAR_NAMES, LABEL, "call_names", "title_ind",
      "href", "popup_href", "title"))
  expect_equal(nrow(linked), 0L)
  expect_identical(linked$href, character(0))
  expect_identical(linked$popup_href, character(0))
  expect_identical(linked$title, character(0))
})

test_that("util_add_links_to_summary_table maps labels and report folders", {
  meta_data <- data.frame(
    VAR_NAMES = "age",
    LABEL = "Age 0",
    stringsAsFactors = FALSE
  )
  tb <- data.frame(
    VAR_NAMES = "age",
    LABEL = "old",
    call_names = "acc_distributions",
    title_ind = "Distribution",
    `..Origin` = "study",
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  folder_of_report <- c(
    "study-Age0.acc_distributions" = "report_study_age"
  )

  linked <- util_add_links_to_summary_table(
    tb,
    list(label_col = LABEL, meta_data = meta_data),
    folder_of_report = folder_of_report
  )

  expect_identical(linked[[LABEL]], "Age 0")
  expect_match(linked$href, "^report_study_age/\\.report/VAR_Age0\\.html#")
  expect_match(
    linked$popup_href,
    "^report_study_age/\\.report/VAR_Age0\\.html#nm=acc_distributions"
  )
  expect_match(linked$title, "^Age 0: ")
  expect_match(linked$title, "Distribution", fixed = TRUE)
  expect_identical(
    attr(linked, "orig_label_col", exact = TRUE),
    linked[[LABEL]]
  )
})

test_that("SSI summary links resolve computed metrics to scale pages", {
  meta_data <- data.frame(
    VAR_NAMES = "MaximumLongString",
    LABEL = "MaximumLongString",
    COMPUTED_VARIABLE_ROLE = "MAXIMUM_LONG_STRING",
    CHECK_ID = "all_questionnaire",
    stringsAsFactors = FALSE
  )
  meta_data_cross_item <- data.frame(
    CHECK_ID = "all_questionnaire",
    CHECK_LABEL = "all_questionnaire",
    SCALE_NAME = NA_character_,
    SCALE_ACRONYM = NA_character_,
    stringsAsFactors = FALSE
  )
  tb <- data.frame(
    VAR_NAMES = "MaximumLongString",
    LABEL = "MaximumLongString",
    call_names = "con_ssi_range_check",
    title_ind = NA_character_,
    stringsAsFactors = FALSE
  )

  linked <- util_add_links_to_summary_table(
    tb,
    list(
      label_col = LABEL,
      meta_data = meta_data,
      meta_data_cross_item = meta_data_cross_item
    ),
    vars_to_include = "ssi"
  )

  expect_identical(
    linked$href,
    "SSIGROUP_MAXIMUM_LONG_STRING.html#MAXIMUM_LONG_STRING"
  )
  expect_identical(
    linked$popup_href,
    "SSIGROUP_MAXIMUM_LONG_STRING.html#MAXIMUM_LONG_STRING"
  )
})

test_that("variable-group summary links resolve to cross-item pages", {
  meta_data <- data.frame(
    VAR_NAMES = "computed_group_metric",
    LABEL = "Computed group metric",
    CHECK_ID = "scale_d",
    stringsAsFactors = FALSE
  )
  meta_data_cross_item <- data.frame(
    CHECK_ID = "scale_d",
    CHECK_LABEL = "Scale D response group",
    SCALE_NAME = NA_character_,
    SCALE_ACRONYM = NA_character_,
    stringsAsFactors = FALSE
  )
  tb <- data.frame(
    VAR_NAMES = "scale_d",
    LABEL = "Computed group metric",
    CHECK_ID = "scale_d",
    call_names = "con_ssi_range_check",
    title_ind = NA_character_,
    stringsAsFactors = FALSE
  )

  linked <- util_add_links_to_summary_table(
    tb,
    list(
      label_col = LABEL,
      meta_data = meta_data,
      meta_data_cross_item = meta_data_cross_item
    ),
    vars_to_include = "variable_group"
  )

  expect_identical(
    linked$href,
    "ScaleDresponsegroup.html#Scale D response group"
  )
  expect_identical(
    linked$popup_href,
    "ScaleDresponsegroup.html#nm=variable_group.scale_d"
  )
})

test_that("contradiction-only groups use a result-only overview popup", {
  meta_data_cross_item <- data.frame(
    CHECK_ID = "rule_a",
    CHECK_LABEL = "Rule A",
    stringsAsFactors = FALSE
  )
  attr(meta_data_cross_item, "contradiction_only_check_ids") <- "rule_a"
  linked <- util_add_links_to_summary_table(
    data.frame(
      VAR_NAMES = "rule_a",
      LABEL = "Rule A",
      CHECK_ID = "rule_a",
      call_names = "con_contradictions_redcap",
      title_ind = NA_character_,
      stringsAsFactors = FALSE
    ),
    list(
      label_col = LABEL,
      meta_data = data.frame(
        VAR_NAMES = "rule_a",
        LABEL = "Rule A",
        stringsAsFactors = FALSE
      ),
      meta_data_cross_item = meta_data_cross_item
    ),
    vars_to_include = "variable_group"
  )

  expect_identical(linked$href, "dim_con.html#Contradictions")
  expect_identical(
    linked$popup_href,
    "dim_con.html#nm=con_contradictions_redcap.[ALL]"
  )
})

test_that("combined variable-group links use explicit group identifiers", {
  meta_data <- data.frame(
    VAR_NAMES = "study-A-scale_d",
    new_label = "study-A-Scale D response group",
    stringsAsFactors = FALSE
  )
  meta_data_cross_item <- data.frame(
    CHECK_ID = "scale_d",
    CHECK_LABEL = "Scale D response group",
    stringsAsFactors = FALSE
  )
  tb <- data.frame(
    VAR_NAMES = "study-A-scale_d",
    Original_var_name = "scale_d",
    CHECK_ID = "scale_d",
    new_label = "study-A-Scale D response group",
    call_names = "con_ssi_range_check",
    title_ind = NA_character_,
    stringsAsFactors = FALSE
  )
  report_folders <- c(
    `study-A-scale_d.con_ssi_range_check` = "report_study_A"
  )

  linked <- util_add_links_to_summary_table(
    tb,
    list(
      label_col = "new_label",
      meta_data = meta_data,
      meta_data_cross_item = meta_data_cross_item
    ),
    folder_of_report = report_folders,
    vars_to_include = "variable_group"
  )

  expect_identical(
    linked$href,
    paste0(
      "report_study_A/.report/",
      "ScaleDresponsegroup.html#Scale D response group"
    )
  )
  expect_identical(
    linked$popup_href,
    paste0(
      "report_study_A/.report/",
      "ScaleDresponsegroup.html#nm=variable_group.scale_d"
    )
  )
})

test_that("variable-group links are not inferred without CHECK_ID", {
  meta_data <- data.frame(
    VAR_NAMES = "study-A-scale_d",
    new_label = "study-A-Scale D response group",
    stringsAsFactors = FALSE
  )
  meta_data_cross_item <- data.frame(
    CHECK_ID = "scale_d",
    CHECK_LABEL = "Scale D response group",
    stringsAsFactors = FALSE
  )
  tb <- data.frame(
    VAR_NAMES = "study-A-scale_d",
    Original_var_name = "scale_d",
    new_label = "study-A-Scale D response group",
    call_names = "con_ssi_range_check",
    title_ind = NA_character_,
    stringsAsFactors = FALSE
  )

  linked <- util_add_links_to_summary_table(
    tb,
    list(
      label_col = "new_label",
      meta_data = meta_data,
      meta_data_cross_item = meta_data_cross_item
    ),
    vars_to_include = "variable_group"
  )

  expect_true(is.na(linked$href))
  expect_true(is.na(linked$popup_href))
})
