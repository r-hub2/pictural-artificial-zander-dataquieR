skip_on_cran()

test_that("dq_report_by overview call text is compact", {
  env <- new.env(parent = emptyenv())
  env$study_data <- data.frame(a = 1)
  env$meta_data_v2 <- "https://example.org/path/ship_meta_v2.xlsx"
  env$dimensions <- c("Completeness", "Integrity")
  env$segment_column <- NULL
  env$strata_column <- NULL
  env$resp_vars <- c("age", "sex")

  call_text <- util_compact_dq_report_by_call_from_env(env)

  expect_match(call_text, "^dq_report_by\\(", perl = TRUE)
  expect_match(call_text, "study_data", fixed = TRUE)
  expect_match(call_text, 'meta_data_v2 = "ship_meta_v2.xlsx"',
    fixed = TRUE
  )
  expect_match(call_text, 'dimensions = c("Completeness", "Integrity")',
    fixed = TRUE
  )
  expect_match(call_text, "segment_column = NULL", fixed = TRUE)
  expect_false(grepl("^function", call_text))
})

test_that("dq_report_by overview call text handles scalar edge values", {
  env <- new.env(parent = emptyenv())
  env$study_data <- "/tmp/study.csv"
  env$dimensions <- character()
  env$segment_select <- TRUE
  env$strata_select <- NA
  env$selection_type <- 2
  env$subgroup <- quote(speed > 5)
  env$html_table_backend <- as.name("DT")

  call_text <- util_compact_dq_report_by_call_from_env(env)

  expect_match(call_text, 'dq_report_by("study.csv"', fixed = TRUE)
  expect_match(call_text, "dimensions = character(0)", fixed = TRUE)
  expect_match(call_text, "segment_column = NULL", fixed = TRUE)
  expect_match(call_text, "strata_column = NULL", fixed = TRUE)
  expect_match(call_text, "segment_select = TRUE", fixed = TRUE)
  expect_match(call_text, "strata_select = NA", fixed = TRUE)
  expect_match(call_text, "selection_type = 2", fixed = TRUE)
  expect_match(call_text, "subgroup = speed > 5", fixed = TRUE)
  expect_match(call_text, "html_table_backend = DT", fixed = TRUE)
})

test_that("dq_report_by overview call text handles compact fallbacks", {
  env <- new.env(parent = emptyenv())
  env$study_data <- ""
  env$meta_data_v2 <- NA_character_
  env$resp_vars <- c('a"b', "c\\d")
  env$selection_type <- list("custom", 1)
  env$subgroup <- data.frame(x = 1)

  call_text <- util_compact_dq_report_by_call_from_env(env)

  expect_match(call_text, 'dq_report_by(""', fixed = TRUE)
  expect_match(call_text, 'meta_data_v2 = "NA"', fixed = TRUE)
  expect_match(call_text, 'resp_vars = c("a\\"b", "c\\\\d")',
    fixed = TRUE
  )
  expect_match(call_text, "selection_type = list", fixed = TRUE)
  expect_match(call_text, "subgroup = study_data", fixed = TRUE)
})

test_that("dq_report_by overview call text skips missing local values", {
  env <- new.env(parent = emptyenv())
  env$study_data <- data.frame(x = 1)
  env$meta_data_v2 <- character(0)
  env$dimensions <- NULL
  env$segment_column <- rlang::missing_arg()
  env$strata_column <- "CENTER_0"

  call_text <- util_compact_dq_report_by_call_from_env(env)

  expect_match(call_text, "dq_report_by(study_data", fixed = TRUE)
  expect_match(call_text, "meta_data_v2 = character(0)", fixed = TRUE)
  expect_false(grepl("dimensions", call_text, fixed = TRUE))
  expect_match(call_text, "segment_column = NULL", fixed = TRUE)
  expect_match(call_text, 'strata_column = "CENTER_0"', fixed = TRUE)
})

test_that("dq_report_by overview prefers compact stored call text", {
  expect_identical(
    util_report_by_overview_call_text(
      by_call = dq_report_by,
      call_report_by = "do.call(dq_report_by, args)",
      call_report_by_overview = 'dq_report_by("study_data")'
    ),
    'dq_report_by("study_data")'
  )

  expect_identical(
    util_report_by_overview_call_text(
      by_call = quote(dq_report_by(study_data, dimensions = "int"))
    ),
    'dq_report_by(study_data, dimensions = "int")'
  )

  expect_identical(
    util_report_by_overview_call_text(
      call_report_by = "dq_report_by(study_data)"
    ),
    "dq_report_by(study_data)"
  )

  expect_identical(
    util_report_by_overview_call_text(),
    "dq_report_by(...)"
  )
})

test_that(
  "dq_report_by overview header summarizes split and dimension choices",
  {
    expect_identical(
      util_by_header_from_args(quote(dq_report_by(study_data, meta_data))),
      "Data Quality Report Bundle"
    )

    expect_match(
      util_by_header_from_args(quote(dq_report_by(
        study_data,
        meta_data,
        strata_column = center,
        segment_column = LABEL,
        dimensions = c("des", "acc")
      ))),
      paste(
        "Stratified by center and Split by Segments as Defined in LABEL",
        paste0(
          "in the Metadata and Including Accuracy Checks and ",
          "Without Completeness"
        ),
        "& Consistency Checks"
      ),
      fixed = TRUE
    )

    expect_match(
      util_by_header_from_args(quote(dq_report_by(
        study_data,
        meta_data,
        segment_column = dataquieR::STUDY_SEGMENT,
        dimensions = "all"
      ))),
      paste(
        "Split by Segments as Defined in STUDY_SEGMENT in the Metadata",
        "and Including Accuracy Checks"
      ),
      fixed = TRUE
    )

    expect_identical(
      util_by_header_from_args(quote(dq_report_by(
        study_data,
        meta_data,
        dimensions = c(des, int)
      ))),
      paste(
        "Data Quality Report Bundle Without Completeness, Consistency,",
        "& Accuracy Checks"
      )
    )

    expect_identical(
      util_by_header_from_args(quote(dq_report_by(
        study_data,
        meta_data,
        dimensions = c("des", "int", "com")
      ))),
      "Data Quality Report Bundle Without Consistency & Accuracy Checks"
    )

    expect_identical(
      util_by_header_from_args(quote(dq_report_by(
        study_data,
        meta_data,
        dimensions = "des"
      ))),
      paste(
        "Data Quality Report Bundle Without Completeness, Consistency,",
        "& Accuracy Checks"
      )
    )

    expect_match(
      util_by_header_from_args(quote(dq_report_by(
        study_data,
        meta_data,
        dimensions = c("des", custom_dim)
      ))),
      "Without Completeness, Consistency, & Accuracy Checks",
      fixed = TRUE
    )

    expect_match(
      util_by_header_from_args(quote(dq_report_by(
        study_data,
        meta_data,
        dimensions = c("des", "int", "acc")
      ))),
      paste(
        "Including Accuracy Checks and Without Completeness",
        "& Consistency Checks"
      ),
      fixed = TRUE
    )

    expect_match(
      util_by_header_from_args(quote(dq_report_by(
        study_data,
        meta_data,
        strata_column = NULL,
        segment_column = NULL,
        dimensions = character(0)
      ))),
      "Without Completeness, Consistency, & Accuracy Checks",
      fixed = TRUE
    )

    expect_match(
      util_by_header_from_args(quote(dq_report_by(
        study_data,
        meta_data,
        dimensions = c("des", utils::head, sqrt(4))
      ))),
      "Without Completeness, Consistency, & Accuracy Checks",
      fixed = TRUE
    )
  }
)

test_that("report-by metadata persists overview call information", {
  output_dir <- withr::local_tempdir()
  strata_column <- "CENTER_0"
  by_call <- quote(dq_report_by("study_data", strata_column = "CENTER_0"))
  call_report_by <- "dq_report_by(study_data, strata_column = CENTER_0)"
  call_report_by_overview <-
    'dq_report_by("study_data", strata_column = "CENTER_0")'

  util_report_by_meta(
    output_dir = output_dir,
    names = c(
      "strata_column",
      "by_call",
      "call_report_by",
      "call_report_by_overview"
    )
  )

  metadata <- readRDS(file.path(output_dir, "report_by_meta.RDS"))

  expect_identical(metadata$strata_column, strata_column)
  expect_identical(metadata$by_call, by_call)
  expect_identical(metadata$call_report_by, call_report_by)
  expect_identical(
    metadata$call_report_by_overview,
    call_report_by_overview
  )
})

test_that("report-by overview legend requires visible strata labels", {
  prep_purge_data_frame_cache()
  withr::defer(prep_purge_data_frame_cache())

  prep_add_data_frames(
    "..INFO_SD_NAME_FOR_REPORT" = data.frame(
      SD1 = "/very/long/study/data/name.RDS"
    )
  )

  expect_false(util_report_by_overview_has_legend(NULL))
  expect_true(util_report_by_overview_has_legend("CENTER_0"))
})

test_that("report-by overview labels retain cross-item group labels", {
  result <- data.frame(
    VAR_NAMES = c("v1", "12", "13"),
    LABEL = c("Stale item label", "Blood pressure checks", NA_character_),
    stringsAsFactors = FALSE
  )
  meta_data <- data.frame(
    VAR_NAMES = "v1",
    LABEL = "Variable 1",
    stringsAsFactors = FALSE
  )

  expect_identical(
    util_report_by_overview_labels(result, meta_data, LABEL),
    c("Variable 1", "Blood pressure checks", "13")
  )
  expect_identical(
    util_report_by_overview_labels(
      result[, VAR_NAMES, drop = FALSE],
      meta_data[, VAR_NAMES, drop = FALSE],
      LABEL
    ),
    c("v1", "12", "13")
  )
  expect_identical(
    util_report_by_overview_labels(result, meta_data, VAR_NAMES),
    c("v1", "12", "13")
  )
})

test_that("report-by dashboard links ignore empty shadow rows", {
  links <- data.frame(
    row_id = c("shadow", "result", "other"),
    VAR_NAMES = c("group-a", "group-a", "group-a"),
    call_names = "con_contradictions_redcap",
    indicator_metric = c(
      "NUM_con_con_contc", "NUM_con_con_contc", "PCT_con_con_contc"
    ),
    value = c(NA, "5", "2.50"),
    values_raw = c(NA, "5", "2.5"),
    href = c("shadow.html", "result.html", "other.html"),
    stringsAsFactors = FALSE
  )

  selected <- util_report_by_overview_dashboard_link_rows(links)

  expect_identical(selected$row_id, c("result", "other"))
  expect_identical(selected$href, c("result.html", "other.html"))
})

test_that("report-by item coverage distinguishes repeated strata", {
  reports <- list(list(name = "A"), list(name = "B"))
  summaries <- lapply(c("A", "B"), function(sdn) {
    structure(data.frame(), this = list(sdn = sdn))
  })
  coverage_row <- data.frame(
    dimension = "Completeness",
    function_name = "com_item_missingness",
    indicator_id = "DQ_2_1_1_1",
    indicator_label = "Missing values",
    variable = "v1",
    variable_label = "Variable 1",
    classifications = 1L,
    possible_classifications = 1L,
    applicable = TRUE,
    stringsAsFactors = FALSE
  )

  testthat::with_mocked_bindings(
    util_report_scope_item_coverage = function(report = NULL, repsum = NULL) {
      if (is.null(report)) coverage_row[0, , drop = FALSE] else coverage_row
    },
    .package = "dataquieR",
    {
      repeated <- util_report_by_overview_item_coverage(
        reports,
        summaries,
        repeated_items = TRUE
      )
      unique_items <- util_report_by_overview_item_coverage(
        reports,
        summaries,
        repeated_items = FALSE
      )
    }
  )

  expect_identical(repeated$variable, c("A-v1", "B-v1"))
  expect_identical(
    repeated$variable_label,
    c("A - Variable 1", "B - Variable 1")
  )
  expect_equal(nrow(unique_items), 1L)
})

test_that("report-by item scope keeps report-specific concept applicability", {
  reports <- list(list(id = "numeric"), list(id = "text"))
  summaries <- list(structure(data.frame(), this = list()),
    structure(data.frame(), this = list())
  )
  coverage <- data.frame(
    dimension = "Completeness",
    function_name = "com_item_missingness",
    indicator_id = "DQ_2_1_1_1",
    indicator_label = "Missing values",
    variable = "v1",
    variable_label = "Variable 1",
    classifications = 1L,
    computations = 1L,
    possible_classifications = 1L,
    applicable = TRUE,
    stringsAsFactors = FALSE
  )
  possible_seen <- NULL

  testthat::with_mocked_bindings(
    util_report_by_overview_item_coverage = function(...) coverage,
    util_report_scope_target_indicator_ids = function(target_entity,
      report = NULL) {
      paste0("DQ_", report$id)
    },
    util_render_report_scope_tree = function(...,
      item_possible_indicator_ids = NULL) {
      possible_seen <<- item_possible_indicator_ids
      htmltools::tags$div()
    },
    .package = "dataquieR",
    util_report_by_overview_item_scope(reports, summaries)
  )

  expect_identical(possible_seen, c("DQ_numeric", "DQ_text"))
})

test_that("report-by group scope distinguishes repeated strata", {
  group_node <- function() {
    matrix <- data.frame(
      unit = "group-1",
      unit_label = "Group 1",
      analysis = "Missing responses",
      classified = TRUE,
      applicable = TRUE,
      stringsAsFactors = FALSE
    )
    items <- list(
      possible = c("Missing responses\fgroup-1" =
          "Missing responses of Group 1"),
      classified = c("Missing responses\fgroup-1" =
          "Missing responses of Group 1"),
      unresolved = character(),
      row_label = "Variable group",
      matrix = matrix
    )
    util_report_scope_tree_node(
      "Variable-group assessment scope",
      coverage = util_report_scope_tree_coverage(1L, 1L),
      classification_items = items,
      concept_coverage = util_report_scope_tree_concept_coverage(1L, 2L),
      concept_items = list(
        possible = c("DQ_1" = "Missing responses", "DQ_2" = "Other"),
        assessed = c("DQ_1" = "Missing responses")
      ),
      label = "Variable-group assessment scope",
      measures = 1L,
      assessed = 1L
    )
  }
  reports <- list(list(id = "A"), list(id = "B"))
  summaries <- lapply(c("North", "South"), function(stratum) {
    structure(data.frame(), this = list(stratum = stratum))
  })

  testthat::with_mocked_bindings(
    util_generate_table_scale = function(...) data.frame(),
    util_report_scope_variable_group_tree_nodes = function(...) group_node(),
    .package = "dataquieR",
    {
      repeated <- util_report_by_overview_group_scope(
        reports,
        summaries,
        repeated_groups = TRUE
      )
      unique_groups <- util_report_by_overview_group_scope(
        reports,
        summaries,
        repeated_groups = FALSE
      )
    }
  )

  repeated_html <- as.character(repeated)
  unique_html <- as.character(unique_groups)
  expect_match(repeated_html, "2 of 2 expected results", fixed = TRUE)
  expect_match(repeated_html, "North - Group 1", fixed = TRUE)
  expect_match(repeated_html, "South - Group 1", fixed = TRUE)
  expect_match(unique_html, "1 of 1 expected result", fixed = TRUE)
})

test_that("report-by group merge preserves all coverage states", {
  group_node <- function(computed, classified, requested) {
    possible <- c("Metric\fgroup-1" = "Metric of Group 1")
    items <- list(
      possible = possible,
      computed = if (computed) possible else character(),
      classified = if (classified) possible else character(),
      unresolved = character(),
      row_label = "Variable group",
      matrix = data.frame(
        unit = "group-1",
        unit_label = "Group 1",
        analysis = "Metric",
        classified = classified,
        computed = computed,
        applicable = TRUE,
        stringsAsFactors = FALSE
      )
    )
    concepts <- list(
      possible = c(A = "Concept A", B = "Concept B"),
      requested = c(A = "Concept A", B = "Concept B")[requested],
      computed = if (computed) c(A = "Concept A") else character(),
      assessed = if (classified) c(A = "Concept A") else character()
    )
    util_report_scope_tree_node(
      "Variable-group assessment scope",
      coverage = util_report_scope_tree_coverage(
        classified,
        1L,
        computed
      ),
      classification_items = items,
      concept_coverage = util_report_scope_tree_concept_coverage(
        length(concepts$assessed),
        length(concepts$possible),
        length(concepts$computed),
        length(concepts$requested)
      ),
      concept_items = concepts,
      label = "Variable-group assessment scope",
      measures = 1L,
      assessed = 1L
    )
  }

  north <- util_report_by_overview_prefix_group_scope(
    group_node(TRUE, FALSE, c(TRUE, TRUE)),
    "North"
  )
  south <- util_report_by_overview_prefix_group_scope(
    group_node(TRUE, TRUE, c(TRUE, FALSE)),
    "South"
  )
  merged <- util_report_by_overview_merge_group_scopes(list(north, south))

  expect_identical(
    merged$coverage,
    list(classifications = 1L, possible = 2L, computed = 2L)
  )
  expect_identical(
    merged$concept_coverage,
    list(assessed = 1L, possible = 2L, computed = 1L, requested = 2L)
  )
  expect_length(merged$classification_items$computed, 2L)
  cell <- paste(as.character(util_report_scope_tree_table_concept_coverage_cell(
    merged$concept_coverage,
    merged$concept_items
  )), collapse = "")
  expect_match(cell, "100% requested", fixed = TRUE)
  expect_match(cell, "50% computed (50% classified)", fixed = TRUE)
})

test_that("report-by overview loads only usable stored reports", {
  output_dir <- withr::local_tempdir()
  file.create(file.path(output_dir, c("report_A.dq2", "report_B.dq2")))
  summary_names <- c(
    "report_summary_A.RDS",
    "report_summary_B.RDS",
    "report_summary_missing.RDS"
  )

  reports <- testthat::with_mocked_bindings(
    prep_load_report = function(path) {
      if (endsWith(path, "report_B.dq2")) {
        stop("broken report")
      }
      basename(path)
    },
    .package = "dataquieR",
    util_report_by_overview_reports(output_dir, summary_names)
  )

  expect_identical(reports[[1]], "report_A.dq2")
  expect_null(reports[[2]])
  expect_null(reports[[3]])
})

test_that("report-by overview resolves local split report metadata", {
  skip_on_cran()
  skip_if_not_installed("DT")
  skip_if_not_installed("htmltools")
  skip_if_not_installed("jsonlite")
  skip_if_not_installed("rmarkdown")

  output_dir <- withr::local_tempdir()
  start_time <- as.POSIXct("2026-07-30 09:00:00", tz = "UTC")
  mod_label <- list(
    label_modification_text = "labels changed",
    label_modification_table = data.frame(
      old = "v1",
      new = "Variable 1",
      stringsAsFactors = FALSE
    )
  )
  saveRDS(list(
    strata_column = "CENTER",
    segment_column = STUDY_SEGMENT,
    strata_column_label = "Center",
    subgroup = NULL,
    mod_label = mod_label,
    title = "Local overview",
    disable_plotly = FALSE,
    rep_id = "overview-id",
    start_time = start_time,
    subtitle = "Subtitle",
    author = "Tester",
    user_info = list(
      generated = start_time,
      by = quote(dq_report_by(study_data)),
      ignored = data.frame(x = 1)
    ),
    by_call = quote(dq_report_by(
      study_data,
      strata_column = CENTER,
      segment_column = STUDY_SEGMENT
    )),
    call_report_by = "dq_report_by(study_data)",
    call_report_by_overview = "dq_report_by(study_data)"
  ), file.path(output_dir, "report_by_meta.RDS"))

  make_summary <- function(sdn, stratum, segment, used_data_file) {
    include_group <- identical(sdn, "A")
    var_names <- c("v1", "v2", if (include_group) "12")
    labels <- c(
      "Variable 1",
      "Variable 2",
      if (include_group) "Blood pressure checks"
    )
    calls <- c(
      "des_summary",
      "acc_margins",
      if (include_group) "des_scatterplot_matrix"
    )
    this <- list(
      sdn = sdn,
      stratum = stratum,
      segment = segment,
      used_data_file = used_data_file,
      label_col = LABEL,
      meta_data = data.frame(
        VAR_NAMES = c("v1", "v2"),
        LABEL = c("Variable 1", "Variable 2"),
        STUDY_SEGMENT = segment,
        stringsAsFactors = FALSE
      ),
      result = data.frame(
        VAR_NAMES = var_names,
        LABEL = labels,
        call_names = calls,
        indicator_metric = seq_along(var_names),
        value = seq_along(var_names),
        stringsAsFactors = FALSE
      ),
      variable_group_call_names = if (include_group) {
        "des_scatterplot_matrix"
      } else {
        character()
      },
      meta_data_cross_item = if (include_group) {
        data.frame(
          CHECK_ID = "12",
          CHECK_LABEL = "Blood pressure checks",
          stringsAsFactors = FALSE
        )
      },
      summary_meta_data = data.frame(
        VAR_NAMES = c("v1", "v2"),
        LABEL = c("Variable 1", "Variable 2"),
        stringsAsFactors = FALSE
      )
    )
    structure(data.frame(x = 1), this = this)
  }

  saveRDS(
    make_summary("A", "Center: North", "Segment: Lab", "/tmp/study.csv"),
    file.path(output_dir, "report_summary_A.RDS")
  )
  saveRDS(
    make_summary("B", "Center: South", "Segment: Clinic", "study.csv"),
    file.path(output_dir, "report_summary_B.RDS")
  )
  item_dashboard <- data.frame(
    new_label = "Variable 1",
    call_names = "des_summary",
    indicator_metric = "Mean",
    value = 1,
    Graph = '<img src="item-distribution.png" alt="Distribution">',
    STUDY_SEGMENT = "Segment: Lab",
    `..VAR_NAMES` = "v1",
    href = "item.html",
    popup_href = "item.html",
    title = "Variable 1",
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  util_translated_colnames(item_dashboard) <- util_translate(
    colnames(item_dashboard),
    ns = "dashboard_table"
  )
  attr(item_dashboard, "name_of_study_data") <- "study.csv"
  attr(item_dashboard, "level_name") <- "Center: North"
  saveRDS(
    item_dashboard,
    file.path(output_dir, "report_dashboard_A.RDS")
  )
  dir.create(file.path(output_dir, "report_A", ".report"), recursive = TRUE)
  dir.create(file.path(output_dir, "report_B", ".report"), recursive = TRUE)
  writeLines(
    "<html></html>",
    file.path(output_dir, "report_A", ".report", "report.html")
  )
  writeLines(
    "<html></html>",
    file.path(output_dir, "report_B", ".report", "report.html")
  )
  saveRDS(
    "result.html",
    file.path(output_dir, "report_A", ".report", "anchor_list.RDS")
  )
  file.create(file.path(output_dir, c("report_A.dq2", "report_B.dq2")))

  dqi <- util_get_concept_info("dqi")
  item_concept <- dqi[
    dqi$Level == 3L & !is.na(dqi$abbreviation),
    ,
    drop = FALSE
  ]
  item_concept <- item_concept[1, , drop = FALSE]
  item_coverage <- data.frame(
    dimension = item_concept$Dimension,
    function_name = item_concept$function_R,
    indicator_id = item_concept$IndicatorID,
    indicator_label = item_concept$public_name,
    variable = "v1",
    variable_label = "Variable 1",
    classifications = 1L,
    possible_classifications = 1L,
    applicable = TRUE,
    stringsAsFactors = FALSE
  )

  summary_all <- structure(data.frame(x = 1),
    class = c("dataquieR_summary", "data.frame"),
    this = list(
      result = data.frame(
        VAR_NAMES = c(
          "study.csv-Center: North-Segment: Lab-v1",
          "study.csv-Center: South-Segment: Clinic-v2",
          "study.csv-Center: North-Segment: Lab-12",
          "study.csv-Center: North-Segment: Lab-13"
        ),
        call_names = c(
          "des_summary",
          "acc_margins",
          "des_scatterplot_matrix",
          "acc_multivariate_outlier"
        ),
        indicator_metric = c(
          "Mean", "Value", "PCT_con_con", "PCT_acc_ud_outlm"
        ),
        value = c(1, 2, 3, 4),
        class = c(1L, 1L, 3L, 2L),
        stringsAsFactors = FALSE
      ),
      meta_data = data.frame(
        VAR_NAMES = c(
          "study.csv-Center: North-Segment: Lab-v1",
          "study.csv-Center: South-Segment: Clinic-v2"
        ),
        new_label = c("Variable 1", "Variable 2"),
        stringsAsFactors = FALSE
      ),
      label_col = "new_label",
      rownames_of_report = c("Variable 1", "Variable 2"),
      variable_group_call_names = c(
        "des_scatterplot_matrix", "acc_multivariate_outlier"
      )
    )
  )

  overview_folders <- NULL
  rendered_scopes <- character()
  linked_scopes <- character()
  dashboard_image_dirs <- character()
  dashboard_tables <- list()
  call_record <- new.env(parent = emptyenv())
  call_record$variable_group_sunburst_calls <- 0L
  testthat::with_mocked_bindings(
    util_combine_list_report_summaries = function(...) summary_all,
    util_render_table_dataquieR_summary = function(...,
      folder_of_report = NULL, vars_to_include = "study") {
      overview_folders <<- folder_of_report
      rendered_scopes <<- c(rendered_scopes, vars_to_include)
      htmltools::HTML("<div>summary-table</div>")
    },
    plot.dataquieR_summary = function(..., vars_to_include = "study",
      hierarchy = NULL) {
      if (identical(vars_to_include, "variable_group") &&
          !is.null(hierarchy)) {
        call_record$variable_group_sunburst_calls <-
          call_record$variable_group_sunburst_calls + 1L
      }
      htmltools::HTML("<div>plot</div>")
    },
    util_add_links_to_summary_table = function(tb, ...,
      vars_to_include = "study") {
      linked_scopes <<- c(linked_scopes, vars_to_include)
      tb
    },
    util_fix_columns_in_dashboard_for_overview = function(x, image_dir, ...) {
      dashboard_image_dirs <<- c(dashboard_image_dirs, image_dir)
      x
    },
    util_dashboard_table2widget = function(table, ...) {
      dashboard_tables[[length(dashboard_tables) + 1L]] <<- table
      htmltools::HTML("<div>dashboard</div>")
    },
    util_setup_dashboard = function(...) {
      db <- data.frame(
        new_label = "Blood pressure checks",
        call_names = "des_scatterplot_matrix",
        indicator_metric = "PCT_con_con",
        value = 3,
        Graph = '<img src="distribution.png" alt="Distribution">',
        STUDY_SEGMENT = "Segment: Lab",
        `..VAR_NAMES` = "12",
        href = "group.html",
        popup_href = "group.html",
        title = "Blood pressure checks",
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
      util_translated_colnames(db) <- util_translate(
        colnames(db),
        ns = "dashboard_table"
      )
      db
    },
    prep_load_report = function(path) list(path = path),
    util_report_scope_item_coverage = function(...) item_coverage,
    util_report_by_overview_group_scope = function(...) {
      htmltools::HTML("<div>Variable-group assessment scope</div>")
    },
    util_write_renderinfo_js_json = function(...) invisible(NULL),
    .package = "dataquieR",
    expect_warning(
      util_create_report_by_overview(output_dir),
      "Cannot read"
    )
  )

  expect_true(file.exists(file.path(output_dir, "index.html")))
  expect_true(file.exists(file.path(output_dir, "sunburst.html")))
  expect_true(file.exists(file.path(output_dir, "dashboard.html")))
  expect_true(file.exists(file.path(output_dir, "tables.html")))
  expect_true(file.exists(file.path(
    output_dir,
    "variable-group-sunburst.html"
  )))
  expect_true(file.exists(file.path(
    output_dir,
    "variable-group-dashboard.html"
  )))
  expect_true(file.exists(file.path(
    output_dir,
    "variable-group-tables.html"
  )))
  expect_true(
    any(grepl(
      "Blood pressure checks.des_scatterplot_matrix",
      names(overview_folders),
      fixed = TRUE
    ))
  )
  anchors <- readLines(file.path(output_dir, "anchor_list.js"), warn = FALSE)
  expect_match(
    paste(anchors, collapse = "\n"),
    "report_A/.report/result.html",
    fixed = TRUE
  )
  expect_match(
    paste(readLines(file.path(output_dir, "index.html"), warn = FALSE),
      collapse = "\n"
    ),
    "Label modifications",
    fixed = TRUE
  )
  overview_html <- paste(
    readLines(file.path(output_dir, "index.html"), warn = FALSE),
    collapse = "\n"
  )
  expect_match(overview_html, "Assessment scope", fixed = TRUE)
  expect_match(overview_html, "Item-level assessment scope", fixed = TRUE)
  expect_match(overview_html, "lib/tippy-", fixed = TRUE)
  expect_match(
    overview_html,
    "Variable-group assessment scope",
    fixed = TRUE
  )
  expect_false(grepl("Item-level sunburst", overview_html, fixed = TRUE))
  expect_false(grepl(
    overview_html,
    "Variable-group sunburst",
    fixed = TRUE
  ))
  expect_match(overview_html, "Items: Sunburst", fixed = TRUE)
  expect_match(
    overview_html,
    "Variable groups: Summary table",
    fixed = TRUE
  )
  sunburst_html <- paste(
    readLines(file.path(output_dir, "sunburst.html"), warn = FALSE),
    collapse = "\n"
  )
  expect_match(
    sunburst_html,
    "dq-overview-section-heading\">Item-level sunburst",
    fixed = TRUE
  )
  expect_false(grepl(
    "Item-level assessment scope",
    sunburst_html,
    fixed = TRUE
  ))
  dashboard_html <- paste(
    readLines(file.path(output_dir, "dashboard.html"), warn = FALSE),
    collapse = "\n"
  )
  expect_match(
    dashboard_html,
    "dq-overview-section-heading\">Item-level dashboard",
    fixed = TRUE
  )
  expect_match(
    dashboard_html,
    "dq-overview-dashboard-container",
    fixed = TRUE
  )
  expect_false(grepl(
    dashboard_html,
    "Variable-group dashboard",
    fixed = TRUE
  ))
  tables_html <- paste(
    readLines(file.path(output_dir, "tables.html"), warn = FALSE),
    collapse = "\n"
  )
  expect_match(
    tables_html,
    "dq-overview-section-heading\">Item-level summary table",
    fixed = TRUE
  )
  expect_match(
    tables_html,
    "dq-overview-table-container",
    fixed = TRUE
  )
  expect_false(grepl(
    "Variable-group summary table",
    tables_html,
    fixed = TRUE
  ))
  variable_group_sunburst_html <- paste(
    readLines(
      file.path(output_dir, "variable-group-sunburst.html"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  expect_match(
    variable_group_sunburst_html,
    "dq-overview-section-heading\">Variable-group sunburst",
    fixed = TRUE
  )
  expect_match(
    variable_group_sunburst_html,
    "Other group checks",
    fixed = TRUE
  )
  expect_match(
    variable_group_sunburst_html,
    "Contradiction checks",
    fixed = TRUE
  )
  expect_match(
    variable_group_sunburst_html,
    "All group-level results",
    fixed = TRUE
  )
  expect_identical(call_record$variable_group_sunburst_calls, 3L)
  expect_match(
    variable_group_sunburst_html,
    "1 contradiction check across 1 variable group available",
    fixed = TRUE
  )
  expect_equal(
    lengths(regmatches(
      variable_group_sunburst_html,
      gregexpr(
        "data-dq-sunburst-mode-button",
        variable_group_sunburst_html,
        fixed = TRUE
      )
    )),
    3L
  )
  expect_match(
    variable_group_sunburst_html,
    paste0(
      "Sector area, label size, and color emphasize more critical checks.\\s+",
      "The count in the selector and notice is the number of evaluated"
    )
  )
  expect_false(grepl(
    "Item-level sunburst",
    variable_group_sunburst_html,
    fixed = TRUE
  ))
  variable_group_dashboard_html <- paste(
    readLines(
      file.path(output_dir, "variable-group-dashboard.html"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  expect_match(
    variable_group_dashboard_html,
    "dq-overview-section-heading\">Variable-group dashboard",
    fixed = TRUE
  )
  variable_group_tables_html <- paste(
    readLines(
      file.path(output_dir, "variable-group-tables.html"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  expect_match(
    variable_group_tables_html,
    "dq-overview-section-heading\">Variable-group summary table",
    fixed = TRUE
  )
  expect_true("variable_group" %in% rendered_scopes)
  expect_true(all(c("study", "variable_group") %in% linked_scopes))
  expect_identical(
    basename(dashboard_image_dirs),
    c("dashboard_images", "variable_group_dashboard_images")
  )
  expect_length(dashboard_tables, 2L)
  expect_true("Stratum" %in% util_untranslated_colnames(
    dashboard_tables[[1]]
  ))
  expect_identical(
    unique(util_with_orig_names(dashboard_tables[[1]])$Stratum),
    "Center: North"
  )
  expect_identical(
    util_with_orig_names(dashboard_tables[[1]])$Graph,
    '<img src="item-distribution.png" alt="Distribution">'
  )
  expect_identical(
    util_with_orig_names(dashboard_tables[[2]])$Graph,
    '<img src="distribution.png" alt="Distribution">'
  )
  expect_false(grepl(
    "Item-level assessment scope",
    paste(readLines(file.path(output_dir, "dashboard.html"), warn = FALSE),
      collapse = "\n"
    ),
    fixed = TRUE
  ))
})
