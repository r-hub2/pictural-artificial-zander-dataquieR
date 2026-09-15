test_that("dq_report_by metadata helpers resolve bracket references", {
  skip_on_cran()

  references <- util_report_by_bracket_references(
    c("[A] + [Label B]", "[A]"),
    c("A", "Label B", "C")
  )
  expect_identical(references, list(c("A", "Label B"), "A"))

  computation <- data.frame(
    COMPUTATION_RULE = c("[A] + [Label B]", "[C]"),
    stringsAsFactors = FALSE
  )
  computation <- util_report_by_complete_computation_variables(
    computation,
    c("A", "Label B", "C")
  )
  expect_identical(
    unlist(computation[[VARIABLE_LIST]], use.names = FALSE),
    c("A | Label B", "C")
  )
})

test_that("dq_report_by metadata helpers map subgroup labels to variables", {
  skip_on_cran()

  meta_data <- data.frame(
    VAR_NAMES = c("A", "B"),
    LABEL = c("Label A", "Label B"),
    stringsAsFactors = FALSE
  )
  expect_identical(
    util_report_by_subgroup_variables(
      subgroup = "[Label B] == 1 & [A] == 2",
      variable_names = c("A", "B", "Label A", "Label B"),
      meta_data = meta_data,
      label_col = LABEL
    ),
    c("A", "B")
  )
})

test_that("dq_report_by metadata helpers normalize identifier variables", {
  skip_on_cran()

  meta_data <- data.frame(
    VAR_NAMES = c("id_one", "id_two"),
    LABEL = c("Identifier one", "Identifier two"),
    stringsAsFactors = FALSE
  )
  expect_identical(
    util_report_by_id_vars(
      "Identifier one | id_two",
      meta_data = meta_data,
      label_col = LABEL
    ),
    c("id_one", "id_two")
  )
  expect_error(
    util_report_by_id_vars(
      "missing_id",
      meta_data = meta_data,
      label_col = LABEL
    ),
    "The id_vars .missing_id. is not present in the item_level metadata"
  )
})

test_that("dq_report_by metadata helpers resolve segment selections", {
  skip_on_cran()

  meta_data <- data.frame(STUDY_SEGMENT = c("baseline", "followup"))
  expect_identical(
    util_report_by_segment_column(
      meta_data = meta_data,
      segment_column = NULL,
      segment_select = "baseline",
      segment_exclude = NULL
    ),
    STUDY_SEGMENT
  )
  expect_error(
    util_report_by_segment_column(
      meta_data = meta_data,
      segment_column = NULL,
      segment_select = NULL,
      segment_exclude = "unknown"
    ),
    "segment_exclude values are not present"
  )

  expect_error(
    util_report_by_segment_column(
      meta_data = data.frame(VAR_NAMES = "x"),
      segment_column = NULL,
      segment_select = "baseline",
      segment_exclude = NULL
    ),
    "No segment_column provided and no STUDY_SEGMENT available"
  )
  expect_error(
    util_report_by_segment_column(
      meta_data = data.frame(VAR_NAMES = "x"),
      segment_column = NULL,
      segment_select = NULL,
      segment_exclude = "baseline"
    ),
    "No segment_column provided and no STUDY_SEGMENT available"
  )
})

test_that("dq_report_by metadata helpers prepare final segment names", {
  skip_on_cran()

  meta_data <- data.frame(
    VAR_NAMES = c("id", "visit", "measure"),
    LABEL = c("ID", "Visit", "Measure"),
    segment = c("baseline", "", "followup"),
    stringsAsFactors = FALSE
  )
  prepared <- util_report_by_segment_names(
    meta_data = meta_data,
    segment_column = "segment",
    segment_select = "baseline | na",
    segment_exclude = NULL,
    label_col = LABEL,
    label_col_provided = LABEL
  )
  expect_equal(prepared$segment_names, c("baseline", "na"))
  expect_equal(prepared$meta_data$segment, c("baseline", "na", "followup"))

  selected <- util_report_by_segment_names(
    meta_data = meta_data,
    segment_column = "segment",
    segment_select = "^follow",
    segment_exclude = NULL,
    label_col = LABEL,
    label_col_provided = LABEL
  )
  expect_equal(selected$segment_names, "followup")

  excluded <- util_report_by_segment_names(
    meta_data = meta_data,
    segment_column = "segment",
    segment_select = NULL,
    segment_exclude = "^base",
    label_col = LABEL,
    label_col_provided = LABEL
  )
  expect_equal(excluded$segment_names, c("na", "followup"))

  label_mapped <- util_report_by_segment_names(
    meta_data = data.frame(
      VAR_NAMES = c("seg_a", "seg_b"),
      LABEL = c("Segment A", "Segment B"),
      segment = c("seg_a", "seg_b"),
      stringsAsFactors = FALSE
    ),
    segment_column = "segment",
    segment_select = NULL,
    segment_exclude = NULL,
    label_col = LABEL,
    label_col_provided = LABEL
  )
  expect_equal(
    unname(label_mapped$segment_names),
    c("Segment A", "Segment B")
  )

  missing_segment <- util_report_by_segment_names(
    meta_data = data.frame(
      VAR_NAMES = c("id", "visit", "measure"),
      LABEL = c("ID", "Visit", "Measure"),
      segment = c("na", "", "na0"),
      stringsAsFactors = FALSE
    ),
    segment_column = "segment",
    segment_select = NULL,
    segment_exclude = NULL,
    label_col = LABEL,
    label_col_provided = LABEL
  )
  expect_equal(missing_segment$meta_data$segment, c("na", "na1", "na0"))
  expect_equal(missing_segment$segment_names, c("na", "na1", "na0"))
})

test_that("dq_report_by segment names reject unmatched selections", {
  skip_on_cran()

  meta_data <- data.frame(
    VAR_NAMES = c("id", "visit", "measure"),
    LABEL = c("ID", "Visit", "Measure"),
    segment = c("baseline", "", "followup"),
    stringsAsFactors = FALSE
  )

  expect_error(
    util_report_by_segment_names(
      meta_data = meta_data,
      segment_column = "segment",
      segment_select = c("missing", "absent"),
      segment_exclude = NULL,
      label_col = LABEL,
      label_col_provided = LABEL
    ),
    "No segment_column level matches the provided names"
  )

  expect_error(
    util_report_by_segment_names(
      meta_data = meta_data,
      segment_column = "segment",
      segment_select = NULL,
      segment_exclude = c("missing", "absent"),
      label_col = LABEL,
      label_col_provided = LABEL
    ),
    "No segment_column level matches the provided names to exclude"
  )

  expect_error(
    util_report_by_segment_names(
      meta_data = meta_data,
      segment_column = "segment",
      segment_select = NULL,
      segment_exclude = ".*",
      label_col = LABEL,
      label_col_provided = LABEL
    ),
    "No segment_column level left after removing"
  )
})

test_that("dq_report_by metadata helpers prepare segment item metadata", {
  skip_on_cran()

  meta_data <- data.frame(
    VAR_NAMES = c("id", "x", "y"),
    LABEL = c("ID", "X", "Y"),
    DATA_TYPE = DATA_TYPES$INTEGER,
    SCALE_LEVEL = SCALE_LEVELS$RATIO,
    MISSING_LIST = "|",
    JUMP_LIST = "|",
    STUDY_SEGMENT = c("A", "A", "B"),
    segment = c("A", "A", "B")
  )
  seg_in_segment <- list(
    A = data.frame(STUDY_SEGMENT = "A"),
    B = data.frame(STUDY_SEGMENT = "B")
  )
  dfr_in_segment <- list(
    A = util_dataframe_metadata_for_names("study"),
    B = util_dataframe_metadata_for_names("study")
  )
  cil_in_segment <- list(
    A = util_ensure_cross_item_metadata(NULL),
    B = util_ensure_cross_item_metadata(NULL)
  )
  computed_in_segment <- list(A = data.frame(), B = data.frame())

  items <- util_report_by_segment_items(
    segment_names = c("A", "B"),
    meta_data = meta_data,
    segment_column = "segment",
    resp_vars = c("x", "y"),
    id_vars = "id",
    vars_in_subgroup = character(0),
    label_col = LABEL,
    seg_in_segment = seg_in_segment,
    dfr_in_segment = dfr_in_segment,
    cil_in_segment = cil_in_segment,
    computed_in_segment = computed_in_segment,
    strata_column = NULL
  )

  expect_equal(items$resp_vars_in_segment$A, "x")
  expect_equal(items$resp_vars_in_segment$B, "y")
  expect_true(all(c("id", "x") %in% items$vars_in_segment$A))
  expect_true(identical(attr(items$md_in_segment$A, "normalized"), TRUE))
  expect_true(identical(attr(items$md_in_segment$A, "version"), 2))
})

test_that("dq_report_by keeps a missing segment column distinguishable", {
  skip_on_cran()

  resolve_default_split <- function(segment_column = NULL) {
    segment_column_was_missing <- missing(segment_column)
    segment_column <- util_report_by_segment_column(
      meta_data = data.frame(VAR_NAMES = "x"),
      segment_column = segment_column,
      segment_select = NULL,
      segment_exclude = NULL
    )
    segment_column_was_missing && is.null(segment_column)
  }

  expect_true(resolve_default_split())
  expect_false(resolve_default_split(segment_column = NULL))
})

test_that("dq_report_by metadata helpers prepare metadata levels", {
  skip_on_cran()

  meta_data <- data.frame(
    VAR_NAMES = "id",
    LABEL = "Identifier",
    STUDY_SEGMENT = "baseline"
  )
  levels <- util_report_by_metadata_levels(
    meta_data = meta_data,
    meta_data_cross_item = NULL,
    meta_data_segment = NULL,
    meta_data_dataframe = data.frame(DF_NAME = "study.RDS"),
    label_col = LABEL,
    study_data_provided = FALSE,
    study_data_is_data_frame = FALSE,
    study_data_length = 0L,
    input_dir = NULL
  )
  expect_identical(levels$meta_data_segment[[STUDY_SEGMENT]], "baseline")
  expect_identical(levels$meta_data_dataframe[[DF_ID_VARS]], NA_character_)
  expect_identical(levels$meta_data_dataframe[[DF_CODE]], NA_character_)
})

test_that("dq_report_by metadata levels fall back for dataframe metadata", {
  skip_on_cran()

  meta_data <- data.frame(
    VAR_NAMES = "id",
    LABEL = "Identifier",
    STUDY_SEGMENT = "baseline"
  )

  expect_message(
    levels <- util_report_by_metadata_levels(
      meta_data = meta_data,
      meta_data_cross_item = NULL,
      meta_data_segment = NULL,
      meta_data_dataframe = "missing_df_metadata",
      label_col = LABEL,
      study_data_provided = FALSE,
      study_data_is_data_frame = FALSE,
      study_data_length = 0L,
      input_dir = NULL
    ),
    "No dataframe level metadata"
  )
  expect_identical(nrow(levels$meta_data_dataframe), 0L)
  expect_true(all(c(DF_NAME, DF_CODE, DF_ID_VARS) %in%
        names(levels$meta_data_dataframe)))

  expect_message(
    ignored <- util_report_by_metadata_levels(
      meta_data = meta_data,
      meta_data_cross_item = NULL,
      meta_data_segment = NULL,
      meta_data_dataframe = data.frame(DF_NAME = "study.RDS"),
      label_col = LABEL,
      study_data_provided = TRUE,
      study_data_is_data_frame = FALSE,
      study_data_length = 2L,
      input_dir = NULL
    ),
    "multiple data frames"
  )
  expect_identical(nrow(ignored$meta_data_dataframe), 0L)
})

test_that(
  "dq_report_by metadata helpers resolve strata labels and selections",
  {
    skip_on_cran()

    meta_data <- data.frame(
      VAR_NAMES = "grp",
      VALUE_LABELS = "1 = Yes | 2 = No",
      VALUE_LABEL_TABLE = NA_character_,
      stringsAsFactors = FALSE
    )
    expect_identical(
      util_report_by_expected_strata(meta_data, "grp"),
      c("1" = "Yes", "2" = "No")
    )

    meta_data[[VALUE_LABELS]] <- NA_character_
    meta_data[[VALUE_LABEL_TABLE]] <- "group_codes"

    testthat::local_mocked_bindings(
      util_expect_data_frame = function(x, dont_assign = FALSE) {
        if (identical(x, "group_codes")) {
          return(data.frame(
            CODE_VALUE = c("1", "2"),
            CODE_LABEL = c("Baseline", "Follow-up"),
            stringsAsFactors = FALSE
          ))
        }
        stop("unexpected table")
      }
    )
    expect_identical(
      util_report_by_expected_strata(meta_data, "grp"),
      c("1" = "Baseline", "2" = "Follow-up")
    )

    testthat::local_mocked_bindings(
      util_expect_data_frame = function(x, dont_assign = FALSE) {
        if (identical(x, CODE_LIST_TABLE)) {
          return(data.frame(
            VALUE_LABEL_TABLE = c("group_codes", "other_codes"),
            CODE_VALUE = c("1", "x"),
            CODE_LABEL = c("Baseline", "Other"),
            stringsAsFactors = FALSE
          ))
        }
        stop("table not found")
      }
    )
    expect_identical(
      util_report_by_expected_strata(meta_data, "grp"),
      c("1" = "Baseline")
    )

    testthat::local_mocked_bindings(
      util_expect_data_frame = function(...) {
        stop("table not found")
      }
    )
    expect_message(
      no_table <- util_report_by_expected_strata(meta_data, "grp"),
      "No value_label_table_name"
    )
    expect_identical(no_table, stats::setNames(character(0), character(0)))

    expect_identical(util_report_by_selection_values(NULL), NULL)
    expect_identical(util_report_by_selection_values("A | B"), c("A", "B"))
    expect_identical(util_report_by_selection_values(c("A", "B")), c("A", "B"))
  }
)

test_that("report metadata attribute helpers prefer current cross-item names", {
  skip_on_cran()

  report <- list()
  attr(report, "meta_data_cross") <- data.frame(old = 1L)
  expect_identical(
    util_report_meta_data_cross_item(report),
    data.frame(old = 1L)
  )
  expect_identical(
    util_report_meta_data_frames(report),
    "meta_data_cross"
  )

  attr(report, "meta_data_cross_item") <- data.frame(current = 2L)
  expect_identical(
    util_report_meta_data_cross_item(report),
    data.frame(current = 2L)
  )
  expect_identical(
    util_report_meta_data_frames(report),
    "meta_data_cross_item"
  )

  attr(report, "meta_data_cross") <- NULL
  attr(report, "meta_data_cross_item") <- NULL
  expect_identical(util_report_meta_data_cross_item(report), data.frame())
  expect_identical(util_report_meta_data_frames(report), character())
})
