skip_on_cran()

test_that("SSI page titles cover available metadata combinations", {
  scale_and_label <- data.frame(
    CHECK_ID = "scale_a",
    CHECK_LABEL = "questionnaire",
    SCALE_NAME = "Scale A",
    SCALE_ACRONYM = NA_character_
  )
  scale_and_acronym <- data.frame(
    CHECK_ID = "scale_b",
    CHECK_LABEL = NA_character_,
    SCALE_NAME = "Scale B",
    SCALE_ACRONYM = "SB"
  )

  expect_identical(
    unname(util_generate_pages_ssi_cross_item_titles(scale_and_label)[
      "long_title"
    ]),
    "questionnaire (Scale A)"
  )
  expect_identical(
    unname(util_generate_pages_ssi_cross_item_titles(scale_and_acronym)[
      "long_title"
    ]),
    "Scale B (SB)"
  )
  expect_identical(
    util_ssi_cross_item_href(data.frame(CHECK_ID = NA_character_)),
    NA_character_
  )
  expect_identical(util_ssi_group_href(NA_character_), NA_character_)
})

test_that("variable-group totals link to group pages", {
  cells <- c(
    '<a href="VAR_ScaleA.html#ScaleA">Ok</a>',
    '<a href="VAR_ScaleB.html#ScaleB">Critical</a>'
  )
  cross_item <- data.frame(
    CHECK_ID = c("1", "2"),
    CHECK_LABEL = c("Scale A response group", "Scale B response group"),
    stringsAsFactors = FALSE
  )

  linked <- util_relink_variable_group_total_cells(
    cells,
    c("1", "2"),
    cross_item
  )

  expect_match(linked[[1]], "ScaleAresponsegroup.html#Scale A response group",
    fixed = TRUE
  )
  expect_match(linked[[2]], "ScaleBresponsegroup.html#Scale B response group",
    fixed = TRUE
  )
  expect_false(any(grepl('href="VAR_', linked, fixed = TRUE)))

  prefixed <- util_relink_variable_group_total_cells(
    '<a href="report_A/.report/VAR_ScaleA.html#ScaleA">Ok</a>',
    "1",
    cross_item
  )
  expect_match(
    prefixed,
    "report_A/.report/ScaleAresponsegroup.html#Scale A response group",
    fixed = TRUE
  )

  missing_total <- util_relink_variable_group_total_cells(
    list(cells[[1]], NULL),
    c("1", "2"),
    cross_item
  )
  expect_match(
    missing_total[[1]],
    "ScaleAresponsegroup.html#Scale A response group",
    fixed = TRUE
  )
  expect_true(is.na(missing_total[[2]]))

  expect_identical(
    util_relink_variable_group_hrefs(
      c("report_A/.report/VAR_ScaleA.html#ScaleA", NA_character_),
      c("1", "2"),
      cross_item
    ),
    c(
      "report_A/.report/ScaleAresponsegroup.html#Scale A response group",
      NA_character_
    )
  )

  expect_identical(
    util_result_variable_group_ids(
      c("study-A-scale_a", "missing", "ambiguous"),
      data.frame(
        VAR_NAMES = c("study-A-scale_a", "ambiguous", "ambiguous"),
        CHECK_ID = c("scale_a", "rule_a", "rule_b")
      )
    ),
    c("scale_a", NA_character_, NA_character_)
  )
})

test_that("contradiction-only groups link to the combined result", {
  summary_rows <- data.frame(
    CHECK_ID = c("rule_a", "rule_b", "rule_b", "group_c"),
    function_name = c(
      "con_contradictions_redcap",
      "con_contradictions_redcap",
      "con_ssi_range_check",
      "acc_repeated_measurements"
    ),
    stringsAsFactors = FALSE
  )
  cross_item <- data.frame(
    CHECK_ID = c("rule_a", "rule_b", "group_c"),
    CHECK_LABEL = c("Rule A", "Rule B", "Group C"),
    stringsAsFactors = FALSE
  )
  cross_item <- util_mark_contradiction_only_groups(
    cross_item,
    summary_rows
  )

  expect_identical(
    util_attr(cross_item, "contradiction_only_check_ids", exact = TRUE),
    "rule_a"
  )
  expect_identical(
    util_cross_item_hrefs(c("rule_a", "rule_b", "group_c"), cross_item),
    c(
      "dim_con.html#Contradictions",
      "RuleB.html#Rule B",
      "GroupC.html#Group C"
    )
  )
})

test_that("SSI links retain empty and non-SSI entries", {
  meta_data <- data.frame(
    VAR_NAMES = c("regular", "ssi_missing_target"),
    COMPUTED_VARIABLE_ROLE = c(NA_character_, "MISS_RESP"),
    CHECK_ID = c("regular", "missing"),
    stringsAsFactors = FALSE
  )
  cross_item <- data.frame(
    CHECK_ID = "another_scale",
    CHECK_LABEL = "another_scale",
    stringsAsFactors = FALSE
  )

  expect_identical(
    util_ssi_computed_variable_hrefs(
      "regular",
      meta_data = meta_data,
      meta_data_cross_item = cross_item
    ),
    NA_character_
  )

  no_metadata <- util_ssi_computed_variable_cross_item_links(
    "regular",
    meta_data = data.frame(VAR_NAMES = "regular"),
    meta_data_cross_item = cross_item
  )
  expect_true(all(is.na(no_metadata)))

  regular <- util_ssi_computed_variable_cross_item_links(
    "regular",
    meta_data = meta_data,
    meta_data_cross_item = cross_item
  )
  expect_true(all(is.na(regular)))

  missing_target <- util_ssi_computed_variable_cross_item_links(
    "ssi_missing_target",
    meta_data = meta_data,
    meta_data_cross_item = cross_item
  )
  expect_true(all(is.na(missing_target)))
})

test_that("SSI metric descriptions fall back for missing definitions", {
  testthat::local_mocked_bindings(
    util_map_labels = function(...) NA_character_,
    .package = "dataquieR"
  )

  description <- util_ssi_metric_description("UNKNOWN_SSI_METRIC")

  expect_match(description, "No description found", fixed = TRUE)
  expect_match(description, "UNKNOWN_SSI_METRIC", fixed = TRUE)
})
