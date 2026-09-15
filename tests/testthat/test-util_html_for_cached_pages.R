test_that("HTML page helpers return cached variable pages before rendering", {
  skip_on_cran()

  out_dir <- tempfile("dq-html-var")
  dir.create(out_dir)
  cached <- list(file = "cached-variable-page.html", title = "Cached variable")
  saveRDS(cached, file.path(out_dir, "VAR_v1.RDS"))

  withr::local_options(dataquieR.resume_print = TRUE)

  expect_identical(
    util_html_for_var(
      results = "not-yet-validated",
      cur_var = "v1",
      use_plot_ly = FALSE,
      template = "default",
      rendered_repsum = NULL,
      dir = out_dir
    ),
    cached
  )
})

test_that("HTML variable page summary keeps category order and Total last", {
  skip_on_cran()
  skip_if_not_installed("htmltools")
  skip_if_not_installed("jsonlite")

  out_dir <- tempfile("dq-html-var-summary")
  dir.create(out_dir)

  this <- new.env(parent = emptyenv())
  this$meta_data <- data.frame(
    VAR_NAMES = "v1",
    LABEL = "Variable 1",
    DATA_TYPE = DATA_TYPES$FLOAT,
    stringsAsFactors = FALSE
  )
  this$label_col <- VAR_NAMES

  repsum_wide <- data.frame(
    Variables = c("v1", "group-1"),
    category_10 = c(
      '<a sort="4" href="VAR_v1.html#v1.category_10">Ok</a>',
      '<a sort="4" href="VAR_group-1.html#group-1.category_10">Ok</a>'
    ),
    Total = c(
      '<a href="VAR_v1.html#v1">Critical</a>',
      '<a href="VAR_group-1.html#group-1">Critical</a>'
    ),
    category_2 = c(
      '<a sort="1" href="VAR_v1.html#v1.category_2">Unclear</a>',
      '<a sort="1" href="VAR_group-1.html#group-1.category_2">Unclear</a>'
    ),
    check.names = FALSE
  )
  attr(repsum_wide, "label_col") <- VAR_NAMES

  rendered_repsum <- structure(
    list(),
    this = this,
    repsum_wide = repsum_wide
  )

  for (backend in c("DT", "DT2")) {
    skip_if(
      !requireNamespace(backend, quietly = TRUE),
      sprintf("%s is required to test this table backend", backend)
    )
    withr::local_options(dataquieR.html_table_backend = backend)

    page <- util_html_for_var(
      results = list(),
      cur_var = "v1",
      use_plot_ly = FALSE,
      template = "default",
      rendered_repsum = rendered_repsum,
      dir = out_dir,
      meta_data = this$meta_data,
      label_col = VAR_NAMES,
      dims_in_rep = character(0),
      clls_in_rep = list(),
      function_alias_map = data.frame()
    )

    page_html <- paste(as.character(page[[1]][[4]]), collapse = "")
    summary_match <- regexpr(
      '<script type="application/json" data-for="[^"]+">.*?</script>',
      page_html,
      perl = TRUE
    )
    summary_json <- regmatches(page_html, summary_match)
    summary_json <- sub('^<script type="application/json" data-for="[^"]+">',
      "", summary_json
    )
    summary_json <- sub("</script>$", "", summary_json)
    summary_widget <- jsonlite::fromJSON(summary_json)
    summary_data <- summary_widget$x$data
    if (is.matrix(summary_data)) {
      summary_data <- summary_data[1, ]
    } else if (!"__dataquieR_col_001" %in% names(summary_data)) {
      summary_data <- summary_data[[1]]
    } else {
      summary_data <- summary_data$`__dataquieR_col_001`
    }

    expect_equal(gsub("<[^>]+>", "", summary_data),
      c("category_2", "category_10", "Total"),
      info = backend
    )
    expect_match(summary_data[1], 'sort="000001"',
      fixed = TRUE,
      info = backend
    )
    expect_match(summary_data[2], 'sort="000002"',
      fixed = TRUE,
      info = backend
    )
    expect_match(summary_data[3], 'sort="~~~Total"',
      fixed = TRUE,
      info = backend
    )
    expect_match(page_html, '"total_last_cols":0',
      fixed = TRUE,
      info = backend
    )
    expect_match(page_html, '"grading_cols":[1]',
      fixed = TRUE,
      info = backend
    )
    expect_match(page_html, '"grading_order"',
      fixed = TRUE,
      info = backend
    )
  }
})

test_that("HTML page helpers omit empty dimension pages", {
  skip_on_cran()

  expect_null(
    util_html_for_dim(
      results = list(),
      cll = "acc_test",
      repsum = NULL,
      function_alias_map = data.frame(),
      meta_data = data.frame(),
      label_col = LABEL,
      use_plot_ly = FALSE,
      dir = tempdir(),
      template = "default",
      wd = tempdir()
    )
  )
})

test_that("HTML dimension pages omit results with empty output slots", {
  skip_on_cran()
  skip_if_not_installed("htmltools")

  empty_result <- structure(
    list(SegmentTable = data.frame(), SegmentData = data.frame()),
    class = c("dataquieR_result", "list")
  )
  attr(empty_result, "call") <- quote(
    com_qualified_segment_missingness(study_data, meta_data_segment)
  )
  attr(empty_result, "warning") <- list(
    simpleWarning("Required segment metadata are missing")
  )

  testthat::local_mocked_bindings(
    util_cll_nm2fkt_nm = function(...) {
      "com_qualified_segment_missingness"
    },
    plot.dataquieR_summary = function(...) NULL,
    util_combine_res = function(x) x,
    resnames.dataquieR_resultset2 = function(...) character(),
    util_all_ind_functions = function() {
      "com_qualified_segment_missingness"
    },
    util_get_concept_info = function(...) {
      data.frame(Reportoutputs = "")
    },
    .package = "dataquieR"
  )

  page <- util_html_for_dim(
    results = list(
      com_qualified_segment_missingness = empty_result
    ),
    cll = "com_qualified_segment_missingness",
    repsum = structure(list(), class = "dataquieR_summary"),
    function_alias_map = data.frame(),
    meta_data = data.frame(),
    label_col = LABEL,
    use_plot_ly = FALSE,
    dir = tempdir(),
    template = "default",
    wd = tempdir()
  )

  expect_null(page)
})

test_that("HTML page helpers return cached dimension pages before rendering", {
  skip_on_cran()

  out_dir <- tempfile("dq-html-dim")
  dir.create(out_dir)
  cached <- list(
    file = "cached-dimension-page.html", title = "Cached dimension"
  )

  withr::local_dir(out_dir)
  saveRDS(cached, "dim_acc_acc_test.RDS")
  withr::local_options(dataquieR.resume_print = TRUE)

  expect_identical(
    util_html_for_dim(
      results = list(result = "not-yet-rendered"),
      cll = "acc_test",
      repsum = NULL,
      function_alias_map = data.frame(),
      meta_data = data.frame(),
      label_col = LABEL,
      use_plot_ly = FALSE,
      dir = out_dir,
      template = "default",
      wd = out_dir
    ),
    cached
  )
})

test_that("HTML dimension pages render without per-result navigation links", {
  skip_on_cran()
  skip_if_not_installed("htmltools")

  out_dir <- tempfile("dq-html-dim")
  dir.create(out_dir)
  withr::local_dir(out_dir)
  withr::local_options(dataquieR.resume_print = TRUE)

  results <- list(result = list(SummaryTable = data.frame(x = 1)))
  testthat::local_mocked_bindings(
    util_cll_nm2fkt_nm = function(cll, function_alias_map = NULL) {
      "acc_distributions"
    },
    plot.dataquieR_summary = function(...) {
      htmltools::span("summary plot")
    },
    util_combine_res = function(x) {
      structure(
        list(acc_test = list(SummaryTable = data.frame(x = 1))),
        class = "dataquieR_resultset2"
      )
    },
    resnames.dataquieR_resultset2 = function(x) {
      "SummaryTable"
    },
    util_pretty_print = function(...) {
      htmltools::span("result body")
    },
    util_online_ref_candidates = function(...) {
      list(concept = "concept.html", implementation = "impl.html")
    },
    util_get_concept_links = function(...) {
      htmltools::span("links")
    },
    util_function_description = function(...) {
      "description"
    },
    util_get_concept_info = function(...) {
      data.frame(
        function_R = "acc_distributions",
        menu_location_report = "Custom menu"
      )
    },
    util_alias2caption = function(x, long = TRUE) {
      paste("caption", x)
    },
    .package = "dataquieR"
  )

  page <- util_html_for_dim(
    results = results,
    cll = "acc_test",
    repsum = structure(list(), class = "dataquieR_summary"),
    function_alias_map = data.frame(),
    meta_data = data.frame(),
    label_col = LABEL,
    use_plot_ly = FALSE,
    dir = out_dir,
    template = "default",
    wd = out_dir
  )

  expect_identical(page[[1]], "Custom menu")
  expect_equal(unname(page[[2]]), "caption acc_test", ignore_attr = TRUE)
  expect_identical(
    attr(page[[2]], "alternative_names", exact = TRUE),
    c("acc_test", "caption acc_test")
  )
  expect_identical(page[[3]], "dim_acc_acc_test.html")
  expect_s3_class(page[[4]], "shiny.tag.list")
  expect_match(as.character(page[[4]]), "result body", fixed = TRUE)
  expect_match(as.character(page[[4]]), "concept.html", fixed = TRUE)
  expect_true(file.exists("dim_acc_acc_test.RDS"))
  expect_identical(readRDS("dim_acc_acc_test.RDS")[[3]], page[[3]])
})

test_that("HTML dimension pages move result links into the page menu", {
  skip_on_cran()
  skip_if_not_installed("htmltools")

  out_dir <- tempfile("dq-html-dim")
  dir.create(out_dir)
  withr::local_dir(out_dir)
  withr::local_options(dataquieR.resume_print = FALSE)
  captured <- new.env(parent = emptyenv())

  results <- list(result = list(SummaryTable = data.frame(x = 1)))
  testthat::local_mocked_bindings(
    util_cll_nm2fkt_nm = function(cll, function_alias_map = NULL) {
      "acc_distributions"
    },
    plot.dataquieR_summary = function(..., vars_to_include = NULL) {
      captured$vars_to_include <- vars_to_include
      htmltools::span("summary plot")
    },
    util_combine_res = function(x) {
      structure(
        list(acc_test.result = list(SummaryTable = data.frame(x = 1))),
        class = "dataquieR_resultset2"
      )
    },
    resnames.dataquieR_resultset2 = function(x) {
      "SummaryTable"
    },
    util_pretty_print = function(...) {
      list(
        link = htmltools::a("jump"),
        body = htmltools::span("linked body")
      )
    },
    util_online_ref_candidates = function(...) {
      list(concept = "concept.html", implementation = "impl.html")
    },
    util_get_concept_links = function(...) {
      htmltools::span("links")
    },
    util_function_description = function(...) {
      "description"
    },
    util_get_concept_info = function(...) {
      data.frame(
        function_R = "acc_distributions",
        menu_location_report = "Custom menu"
      )
    },
    util_alias2caption = function(x, long = TRUE) {
      paste("caption", x)
    },
    .package = "dataquieR"
  )

  page <- util_html_for_dim(
    results = results,
    cll = "acc_test",
    repsum = structure(list(), class = "dataquieR_summary"),
    function_alias_map = data.frame(),
    meta_data = data.frame(),
    label_col = LABEL,
    use_plot_ly = FALSE,
    dir = out_dir,
    template = "default",
    wd = out_dir
  )
  html <- as.character(page[[4]])

  expect_identical(page[[1]], "Custom menu")
  expect_match(html, "floatmenu", fixed = TRUE)
  expect_match(html, "<a>jump</a>", fixed = TRUE)
  expect_match(html, "linked body", fixed = TRUE)
  expect_identical(captured$vars_to_include, "study")
  expect_false(file.exists("dim_acc_acc_test.RDS"))
})

test_that("HTML dimension pages use one core for invalid core counts", {
  skip_on_cran()

  out_dir <- tempfile("dq-html-dims")
  dir.create(out_dir)
  report <- list(
    acc_test.v1 = list(result = list(SummaryTable = data.frame(x = 1)))
  )
  class(report) <- "dataquieR_resultset2"
  attr(report, "names") <- names(report)
  attr(report, "cn") <- "acc_test"
  attr(report, "rn") <- "v1"
  attr(report, "meta_data") <- data.frame(VAR_NAMES = "v1", LABEL = "v1")
  attr(report, "label_col") <- VAR_NAMES
  attr(report, "matrix_list") <- structure(
    list(),
    row_indices = c(v1 = 1L),
    col_indices = c(acc_test = 1L),
    function_alias_map = data.frame()
  )

  testthat::local_mocked_bindings(
    util_setup_rstudio_job = function(...) {
      assign("progress", function(...) NULL, envir = parent.frame())
      assign("progress_msg", function(...) NULL, envir = parent.frame())
    },
    util_get_cores_safe = function() NA_real_,
    util_par_lapply_lb = function(X, fun, ...) {
      lapply(X, function(x) {
        fun(x, ...)
      })
    },
    util_html_for_dim = function(results, cll, ...) {
      list("Accuracy", cll, paste0(cll, ".html"), results)
    },
    .package = "dataquieR"
  )

  pages <- util_html_for_dims(
    report = report,
    use_plot_ly = FALSE,
    template = "default",
    block_load_factor = 1,
    repsum = structure(list(), class = "dataquieR_summary"),
    dir = out_dir
  )

  expect_length(pages, 1)
  expect_identical(pages[[1]][[2]], "acc_test")
  expect_named(pages[[1]][[4]], "acc_test.v1")
})
