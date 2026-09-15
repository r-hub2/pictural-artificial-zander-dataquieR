skip_on_cran()

skip_if_no_psock_cluster <- function() {
  cl <- try(parallel::makePSOCKcluster(1), silent = TRUE)
  if (inherits(cl, "try-error")) {
    skip("PSOCK clusters unavailable")
  }
  parallel::stopCluster(cl)
}

test_that("render block size uses workers before progress granularity", {
  expect_equal(
    dataquieR:::util_get_render_block_size(70, 2, NULL),
    7
  )
  expect_equal(
    dataquieR:::util_get_render_block_size(70, 4, NULL),
    7
  )
  expect_equal(
    dataquieR:::util_get_render_block_size(70, 8, NULL),
    8
  )
  expect_equal(
    dataquieR:::util_get_render_block_size(70, 12, NULL),
    12
  )
  expect_equal(
    dataquieR:::util_get_render_block_size(3, 0, NULL),
    1
  )
  expect_equal(
    dataquieR:::util_get_render_block_size(70, 2, 1),
    2
  )
  expect_equal(
    dataquieR:::util_get_render_block_size(70, 2, 5),
    10
  )
})

test_that("print.dataquieR_resultset2 works", {
  skip_on_cran() # slow
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  skip_if_not_installed("stringdist")
  prep_purge_data_frame_cache()
  study_data <- head(
    prep_get_data_frame(
      "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData", # nolint: line_length_linter.
      keep_types = TRUE
    ),
    50
  )

  r <- dq_report2(
    study_data = study_data, resp_vars = "SBP_0",
    meta_data_v2 =
      "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx", # nolint: line_length_linter.
    cores = NULL,
    dimensions = "Integrity"
  )

  render_conditions <- new.env(parent = emptyenv())
  render_conditions$messages <- character()
  render_conditions$warnings <- character()
  withCallingHandlers(
    print(r, cores = NULL, view = FALSE, dir = tempfile()),
    message = function(m) {
      render_conditions$messages <- c(
        render_conditions$messages,
        conditionMessage(m)
      )
      invokeRestart("muffleMessage")
    },
    warning = function(w) {
      render_conditions$warnings <- c(
        render_conditions$warnings,
        conditionMessage(w)
      )
      invokeRestart("muffleWarning")
    }
  )
  expect_identical(
    render_conditions$messages,
    "Compiling HTML report, please wait...\n"
  )
  expect_length(render_conditions$warnings, 0)

  skip_if_not_installed("parallel")
  skip_if_no_psock_cluster()
  cl <- parallel::makePSOCKcluster(1)
  withr::defer(parallel::stopCluster(cl))
  expect_warning(print(r, cores = cl, view = FALSE, dir = tempfile()),
    regexp = "Internal problem.+should be an integer below.+in the context.+"
  )

  r <- dq_report2(
    study_data = study_data, resp_vars = "SBP_0",
    meta_data_v2 =
      "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx", # nolint: line_length_linter.
    filter_indicator_functions = "acc_margins",
    cores = NULL,
    dimensions = "Integrity"
  )
  expect_error(
    {
      print(r)
    },
    regexp = ".*results at all\\."
  )

  r <- dq_report2(
    study_data = study_data, resp_vars = "SBP_0",
    meta_data_v2 =
      "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx", # nolint: line_length_linter.
    filter_indicator_functions = "con_inadmissible_categorical",
    cores = NULL,
    dimensions = "Consistency"
  )
  expect_error(
    {
      print(r)
    },
    regexp = "Report is empty, no results at all.*Applicability"
  )
})

test_that("print.dataquieR_resultset2 validates scalar render arguments", {
  skip_on_cran()
  skip_if_not_installed("DT")
  skip_if_not_installed("htmltools")
  skip_if_not_installed("markdown")
  skip_if_not_installed("rmarkdown")

  result <- structure(
    list(SummaryTable = data.frame(x = 1)),
    class = "dataquieR_result"
  )
  report <- structure(
    list(call_a.x = result),
    class = "dataquieR_resultset2",
    all_calls = list(call_a.x = quote(call_a(x))),
    rn = "x",
    cn = "call_a",
    names = "call_a.x",
    matrix_list = structure(
      list(),
      row_indices = c(x = 1),
      col_indices = c(call_a = 1)
    )
  )
  testthat::local_mocked_bindings(
    util_parallel_start = function(...) invisible(NULL),
    util_parallel_stop = function() invisible(NULL)
  )

  expect_error(
    print.dataquieR_resultset2(report,
      dir = c(tempfile(), tempfile()),
      view = FALSE,
      cores = NULL
    ),
    regexp = "dir must be a character\\(1\\)"
  )
  expect_error(
    print.dataquieR_resultset2(report,
      dir = tempfile(),
      template = 1,
      view = FALSE,
      cores = NULL
    ),
    regexp = "template must be a character\\(1\\)"
  )
  expect_error(
    print.dataquieR_resultset2(report,
      dir = tempfile(),
      view = NA,
      cores = NULL
    ),
    regexp = "view must be a logical\\(1\\)"
  )
  expect_error(
    print.dataquieR_resultset2(report,
      dir = tempfile(),
      by_report = "yes",
      view = FALSE,
      cores = NULL
    ),
    regexp = "by_report must be a logical\\(1\\)"
  )
  expect_message(
    expect_error(
      print.dataquieR_resultset2(report,
        view = FALSE,
        force_overwrite = "yes",
        cores = NULL
      ),
      regexp = "force_overwrite.*must be a logical value"
    ),
    regexp = "No output directory given"
  )
})

test_that("print.dataquieR_resultset2 validates setup options early", {
  skip_on_cran()

  result <- structure(
    list(SummaryTable = data.frame(x = 1)),
    class = "dataquieR_result"
  )
  report <- structure(
    list(call_a.x = result),
    class = "dataquieR_resultset2",
    all_calls = list(call_a.x = quote(call_a(x))),
    rn = "x",
    cn = "call_a",
    names = "call_a.x",
    matrix_list = structure(
      list(),
      row_indices = c(x = 1),
      col_indices = c(call_a = 1)
    )
  )
  testthat::local_mocked_bindings(
    util_parallel_start = function(...) invisible(NULL),
    util_parallel_stop = function() invisible(NULL)
  )

  expect_error(
    print.dataquieR_resultset2(report,
      view = FALSE,
      cores = NULL,
      advanced_options = "not-a-list"
    ),
    regexp = "advanced_options"
  )
  expect_error(
    print.dataquieR_resultset2(report,
      view = FALSE,
      cores = NULL,
      html_table_backend = 1
    ),
    regexp = "html_table_backend"
  )
  expect_error(
    print.dataquieR_resultset2(report,
      view = FALSE,
      cores = NULL,
      html_table_backend = "bad"
    ),
    regexp = "should be one of"
  )

  report_cores <- report
  attr(report_cores, "min_render_version") <- NULL
  attr(report_cores, "translation_version") <- NULL

  skip_if_not_installed("DT")
  skip_if_not_installed("markdown")

  expect_warning(
    expect_error(
      print.dataquieR_resultset2(report_cores,
        dir = tempfile(),
        template = "missing-template",
        view = FALSE,
        cores = NULL
      ),
      regexp = "Template .+ not found"
    ),
    regexp = "may be not rendered correctly"
  )

  attr(report, "min_render_version") <- as.numeric_version("1.0.0")
  attr(report, "translation_version") <- "0.0.0"
  expect_warning(
    expect_error(
      print.dataquieR_resultset2(report,
        dir = tempfile(),
        template = "missing-template",
        view = FALSE,
        cores = NULL
      ),
      regexp = "Template .+ not found"
    ),
    regexp = "translations may have changed"
  )

  seen <- new.env(parent = emptyenv())
  testthat::local_mocked_bindings(
    util_parallel_start = function(mode, cpus, ...) {
      seen$mode <- mode
      seen$cpus <- cpus
      invisible(NULL)
    },
    util_parallel_stop = function() invisible(NULL)
  )
  report_cores <- report
  attr(report_cores, "min_render_version") <- as.numeric_version("1.0.0")
  attr(report_cores, "translation_version") <- NULL

  expect_warning(
    expect_error(
      print.dataquieR_resultset2(report_cores,
        dir = tempfile(),
        template = "missing-template",
        view = FALSE,
        cores = 4L
      ),
      regexp = "Template .+ not found"
    ),
    regexp = "Internal problem: .cores. should be an integer below"
  )
  expect_equal(seen$mode, "socket")
  expect_equal(seen$cpus, 2L)

  seen_list <- new.env(parent = emptyenv())
  testthat::local_mocked_bindings(
    is_testing = function() FALSE,
    .package = "testthat"
  )
  testthat::local_mocked_bindings(
    Sys.sleep = function(...) invisible(NULL),
    .package = "base"
  )
  testthat::local_mocked_bindings(
    util_parallel_start = function(...) {
      seen_list$args <- list(...)
      invisible(NULL)
    },
    util_parallel_stop = function() {
      seen_list$stopped <- TRUE
      invisible(NULL)
    },
    .package = "dataquieR"
  )

  expect_error(
    print.dataquieR_resultset2(report_cores,
      dir = tempfile(),
      template = "missing-template",
      view = FALSE,
      cores = list(
        mode = "socket",
        cpus = 3L,
        logging = TRUE,
        load.balancing = FALSE
      )
    ),
    regexp = "Template .+ not found"
  )
  expect_equal(seen_list$args$mode, "socket")
  expect_equal(seen_list$args$cpus, 3L)
  expect_true(seen_list$args$logging)
  expect_false(seen_list$args$load.balancing)
})

test_that(
  "print.dataquieR_resultset2 explains empty reports before rendering",
  {
    skip_on_cran()
    skip_if_not_installed("DT")
    skip_if_not_installed("htmltools")
    skip_if_not_installed("markdown")
    skip_if_not_installed("rmarkdown")

    empty_report <- structure(
      list(),
      class = "dataquieR_resultset2",
      matrix_list = structure(
        list(),
        row_indices = c(),
        col_indices = c()
      )
    )
    testthat::local_mocked_bindings(
      util_parallel_start = function(...) invisible(NULL),
      util_parallel_stop = function() invisible(NULL)
    )

    expect_error(
      print.dataquieR_resultset2(
        empty_report,
        view = FALSE,
        cores = NULL,
        block_load_factor = 0
      ),
      regexp = "block_load_factor.*above 0"
    )

    expect_error(
      print.dataquieR_resultset2(empty_report, view = FALSE, cores = NULL),
      regexp = "Report is empty, no results at all\\."
    )

    failed_result <- structure(
      list(SummaryTable = data.frame()),
      class = "dataquieR_result"
    )
    attr(failed_result, "error") <- list(simpleError("synthetic failure"))
    failed_report <- structure(
      list(call_a.x = failed_result),
      class = "dataquieR_resultset2",
      all_calls = list(call_a.x = quote(call_a(x))),
      names = "call_a.x",
      matrix_list = structure(
        list(),
        row_indices = c(),
        col_indices = c(call_a = 1)
      )
    )

    expect_error(
      print.dataquieR_resultset2(failed_report, view = FALSE, cores = NULL),
      regexp = "possible reasons are:[[:space:]]+- synthetic failure"
    )

    applicability_error <- simpleError("not applicable")
    attr(applicability_error, "applicability_problem") <- TRUE
    attr(failed_result, "error") <- list(applicability_error)
    failed_report[[1]] <- failed_result

    expect_error(
      print.dataquieR_resultset2(failed_report, view = FALSE, cores = NULL),
      regexp = "possible reasons are:[[:space:]]+- Applicability problem"
    )
  }
)

test_that("iframe writing ignores stale default cluster for explicit cores", {
  skip_on_cran()
  skip_if_not_installed("parallel")
  skip_if_not_installed("htmltools")
  skip_if_no_psock_cluster()

  stale_cl <- parallel::makePSOCKcluster(1)
  parallel::setDefaultCluster(stale_cl)
  parallel::stopCluster(stale_cl)
  withr::defer(parallel::setDefaultCluster(NULL))

  out_dir <- tempfile()
  dir.create(out_dir)

  iframe <- htmltools::tags$div("iframe content")
  attr(iframe, "html_file") <- file.path(out_dir, "iframe.html")
  attr(iframe, "html_inner") <- htmltools::tags$div("inner content")
  attr(iframe, "ggthumb") <- dataquieR:::util_compress(NULL)
  attr(iframe, "thumbnail_args") <- list(figure_type_id = "dot_mat")

  dataquieR:::util_write_iframe_results(
    pages = list(iframe),
    progress_msg = function(...) invisible(NULL),
    progress = function(...) invisible(NULL),
    block_load_factor = 1,
    template_file = system.file("templates", "default", "iframe.html",
      package = "dataquieR"
    ),
    dir = out_dir,
    cores = list(
      mode = "socket",
      logging = FALSE,
      cpus = 1,
      load.balancing = TRUE
    )
  )

  expect_true(file.exists(file.path(out_dir, "iframe.html")))
})

test_that("iframe writer starts a private cluster for explicit cores", {
  skip_on_cran()
  skip_if_not_installed("parallel")
  skip_if_no_psock_cluster()

  expect_null(parallel::getDefaultCluster())

  cl <- dataquieR:::util_start_iframe_writer_cluster(
    list(
      mode = "socket",
      logging = FALSE,
      cpus = 2,
      load.balancing = TRUE
    ),
    max_tasks = 2
  )
  withr::defer(dataquieR:::util_stop_iframe_writer_cluster(cl))

  expect_s3_class(cl, "cluster")
  expect_length(cl, 2)
  expect_length(unique(unlist(parallel::clusterCall(cl, Sys.getpid))), 2)
  expect_null(parallel::getDefaultCluster())
})

test_that("iframe writer handles local no-op and invalid modes", {
  skip_on_cran()
  skip_if_not_installed("htmltools")

  out_dir <- tempfile()
  dir.create(out_dir)
  progress_seen <- new.env(parent = emptyenv())
  progress_seen$called <- FALSE
  result <- dataquieR:::util_write_iframe_results(
    pages = list(htmltools::tags$div("plain content")),
    progress_msg = function(...) {
      progress_seen$called <- TRUE
    },
    progress = function(...) {
      progress_seen$called <- TRUE
    },
    block_load_factor = 1,
    template_file = "unused.html",
    dir = out_dir,
    cores = NULL
  )
  expect_null(result)
  expect_false(progress_seen$called)

  expect_null(dataquieR:::util_start_iframe_writer_cluster(
    list(mode = "local", cpus = 2),
    max_tasks = 1
  ))
  expect_warning(
    expect_null(dataquieR:::util_start_iframe_writer_cluster(
      list(mode = "unknown", cpus = 2),
      max_tasks = 1
    )),
    regexp = "Unknown parallel mode"
  )
})

test_that("iframe writer fills missing cpus before local no-op", {
  skip_on_cran()

  detected <- new.env(parent = emptyenv())
  detected$called <- FALSE
  testthat::local_mocked_bindings(
    util_detect_cores = function() {
      detected$called <- TRUE
      3L
    },
    .package = "dataquieR"
  )

  expect_null(dataquieR:::util_start_iframe_writer_cluster(
    list(mode = "local"),
    max_tasks = 1
  ))
  expect_true(detected$called)
})

test_that("iframe writing keeps caller-provided cluster alive", {
  skip_on_cran()
  skip_if_not_installed("parallel")
  skip_if_not_installed("htmltools")
  skip_if_no_psock_cluster()

  cl <- parallel::makePSOCKcluster(1)
  withr::defer(parallel::stopCluster(cl))

  out_dir <- tempfile()
  dir.create(out_dir)

  iframe <- htmltools::tags$div("iframe content")
  attr(iframe, "html_file") <- file.path(out_dir, "iframe.html")
  attr(iframe, "html_inner") <- htmltools::tags$div("inner content")
  attr(iframe, "ggthumb") <- dataquieR:::util_compress(NULL)
  attr(iframe, "thumbnail_args") <- list(figure_type_id = "dot_mat")

  dataquieR:::util_write_iframe_results(
    pages = list(iframe),
    progress_msg = function(...) invisible(NULL),
    progress = function(...) invisible(NULL),
    block_load_factor = 1,
    template_file = system.file("templates", "default", "iframe.html",
      package = "dataquieR"
    ),
    dir = out_dir,
    cores = cl
  )

  expect_true(file.exists(file.path(out_dir, "iframe.html")))
  expect_true(isTRUE(parallel::clusterCall(cl, function() TRUE)[[1]]))
  expect_null(parallel::getDefaultCluster())
})

test_that("iframe writing does not touch an existing default cluster", {
  skip_on_cran()
  skip_if_not_installed("parallel")
  skip_if_not_installed("htmltools")
  skip_if_no_psock_cluster()

  default_cl <- parallel::makePSOCKcluster(1)
  withr::defer(parallel::stopCluster(default_cl))
  withr::defer(parallel::setDefaultCluster(NULL))
  parallel::setDefaultCluster(default_cl)

  out_dir <- tempfile()
  dir.create(out_dir)

  iframe <- htmltools::tags$div("iframe content")
  attr(iframe, "html_file") <- file.path(out_dir, "iframe.html")
  attr(iframe, "html_inner") <- htmltools::tags$div("inner content")
  attr(iframe, "ggthumb") <- dataquieR:::util_compress(NULL)
  attr(iframe, "thumbnail_args") <- list(figure_type_id = "dot_mat")

  dataquieR:::util_write_iframe_results(
    pages = list(iframe),
    progress_msg = function(...) invisible(NULL),
    progress = function(...) invisible(NULL),
    block_load_factor = 1,
    template_file = system.file("templates", "default", "iframe.html",
      package = "dataquieR"
    ),
    dir = out_dir,
    cores = list(
      mode = "socket",
      logging = FALSE,
      cpus = 1,
      load.balancing = TRUE
    )
  )

  expect_true(file.exists(file.path(out_dir, "iframe.html")))
  expect_true(isTRUE(parallel::clusterCall(default_cl, function() TRUE)[[1]]))
  expect_identical(parallel::getDefaultCluster(), default_cl)
})

test_that("RStudio caller-owned cluster guard gives concrete alternatives", {
  fake_cl <- structure(list(), class = "cluster")

  testthat::local_mocked_bindings(
    util_really_rstudio = function() TRUE
  )

  expect_error(
    dataquieR:::util_guard_rstudio_user_cluster(fake_cl),
    regexp = paste(
      "caller-created parallel cluster",
      "Let dataquieR create",
      "Windows start R",
      "force_rstudio_user_cluster",
      sep = ".*"
    )
  )

  expect_warning(
    dataquieR:::util_guard_rstudio_user_cluster(
      fake_cl,
      list(dataquieR.force_rstudio_user_cluster = TRUE)
    ),
    regexp = "unsupported RStudio setup"
  )
})

test_that("print.ReportSummaryTable explicit relative argument overrides attribute", { # nolint: line_length_linter.
  rst <- util_new_report_summary_table(
    data.frame(Variables = "a", N = 10, A = 5)
  )
  rst <- util_set_report_summary_table_relative(rst, TRUE)

  relative_plot <- print.ReportSummaryTable(rst, view = FALSE)
  expect_warning(
    print.ReportSummaryTable(rst, relative = FALSE, view = FALSE),
    regexp = "deprecated"
  )
  absolute_plot <- suppressWarnings(
    print.ReportSummaryTable(rst, relative = FALSE, view = FALSE)
  )

  expect_equal(relative_plot$data$value, 0.5)
  expect_equal(absolute_plot$data$value, 5)
})

test_that("class ReportSummaryTable", {
  skip_on_cran()
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  meta_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data.RData") # nolint: line_length_linter.
  study_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData", keep_types = TRUE) # nolint: line_length_linter.
  sd1 <- study_data
  md1 <- meta_data
  code_labels <- prep_get_data_frame(
    "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx|missing_table" # nolint: line_length_linter.
  )

  md1 <- prep_add_cause_label_df(md1, code_labels)

  item_miss <- com_item_missingness(
    study_data = sd1,
    meta_data = md1,
    label_col = "LABEL",
    show_causes = TRUE,
    include_sysmiss = TRUE,
    threshold_value = 80,
    suppressWarnings = TRUE,
    drop_levels = TRUE,
    assume_consistent_codes = TRUE,
    expand_codes = TRUE
  )
  apm <- pro_applicability_matrix(study_data, meta_data)

  item_missc <- com_item_missingness(
    study_data = sd1,
    resp_vars = c("CENTER_0"),
    meta_data = md1,
    label_col = "LABEL",
    show_causes = TRUE,
    include_sysmiss = TRUE,
    threshold_value = 80,
    suppressWarnings = TRUE,
    drop_levels = !TRUE,
    assume_consistent_codes = TRUE,
    expand_codes = TRUE
  )

  item_misss <- com_item_missingness(
    study_data = sd1,
    resp_vars = c("SEX_0"),
    meta_data = md1,
    label_col = "LABEL",
    show_causes = TRUE,
    include_sysmiss = TRUE,
    threshold_value = 80,
    suppressWarnings = TRUE,
    drop_levels = !TRUE,
    assume_consistent_codes = TRUE,
    expand_codes = TRUE
  )

  item_missa <- com_item_missingness(
    study_data = sd1,
    resp_vars = c("AGE_0"),
    meta_data = md1,
    label_col = "LABEL",
    show_causes = TRUE,
    include_sysmiss = TRUE,
    threshold_value = 80,
    suppressWarnings = TRUE,
    drop_levels = !TRUE,
    assume_consistent_codes = TRUE,
    expand_codes = TRUE
  )

  item_missb <- com_item_missingness(
    study_data = sd1,
    resp_vars = c("SBP_0"),
    meta_data = md1,
    label_col = "LABEL",
    show_causes = TRUE,
    include_sysmiss = TRUE,
    threshold_value = 80,
    suppressWarnings = TRUE,
    drop_levels = !TRUE,
    assume_consistent_codes = TRUE,
    expand_codes = TRUE
  )

  item_miss_combined <- rbind(
    item_missa$ReportSummaryTable,
    item_missb$ReportSummaryTable,
    item_missc$ReportSummaryTable,
    item_misss$ReportSummaryTable
  )

  expect_equal(nrow(item_miss_combined), 4)
  expect_equal(nrow(item_miss$ReportSummaryTable), 53)
  expect_s3_class(item_miss_combined, "ReportSummaryTable")
  expect_s3_class(apm$ReportSummaryTable, "ReportSummaryTable")

  item_miss_emtpy <- com_item_missingness(
    study_data = sd1,
    resp_vars = c("CENTER_0"),
    meta_data = md1,
    label_col = "LABEL",
    show_causes = TRUE,
    include_sysmiss = FALSE,
    threshold_value = 80,
    suppressWarnings = TRUE,
    drop_levels = TRUE,
    assume_consistent_codes = TRUE,
    expand_codes = TRUE
  )


  skip_on_cran()

  fkt2 <- function(x) {
    withr::local_options(list(viewer = NULL))
    td <- withr::local_tempdir()
    v <- function(x, ...) {}
    options(viewer = v)
    w <- print.ReportSummaryTable(x = x, dt = TRUE)
    if (inherits(w, "htmlwidget")) {
      expect_true(all(c("x", "dependencies", "elementId") %in% names(w)))
      expect_true(length(w$dependencies) > 0)
    } else {
      expect_s3_class(w, "dataquieR_result")
      expect_s3_class(w, "html")
    }
  }
  if (!requireNamespace("DT2", quietly = TRUE) &&
      !requireNamespace("DT", quietly = TRUE)) {
    skip("Neither DT2 nor DT is installed.")
  }
  expect_warning(fkt2(item_miss_emtpy$ReportSummaryTable),
    regexp = "Empty result"
  )
  fkt2(item_miss_combined)
  fkt2(item_missa$ReportSummaryTable)
  fkt2(item_missb$ReportSummaryTable)
  fkt2(item_missc$ReportSummaryTable)
  fkt2(item_misss$ReportSummaryTable)
  fkt2(item_miss$ReportSummaryTable)
  fkt2(apm$ReportSummaryTable)

  g1 <- util_suppress_graphics(print(apm$ReportSummaryTable,
      dt = FALSE
    ))
  g2 <- util_suppress_graphics(print(item_miss$ReportSummaryTable,
      dt = FALSE
    ))
  g3 <- util_suppress_graphics(print(item_missa$ReportSummaryTable,
      dt = FALSE
    ))
  expect_warning(util_suppress_graphics(g4 <- print(item_miss_emtpy$ReportSummaryTable, # nolint: line_length_linter.
        dt = FALSE
      )), regexp = "Empty result")

  skip_on_cran()
  skip_if_not_installed("vdiffr")
  expect_doppelganger2("app-ex-repsumtab", g1)
  expect_doppelganger2("im-ex1-repsumtab", g2)
  expect_doppelganger2("im-ex2-repsumtab", g3)
  expect_warning(expect_doppelganger2("im-empty-repsumtab", g4),
    regexp = "Empty result"
  )
})

test_that("print.interval works", {
  skip_on_cran()
  require_english_locale_and_berlin_tz()
  expect_output(
    print(util_parse_interval("(1;)")),
    "(1;Inf)",
    fixed = TRUE
  )
  expect_output(
    print(util_parse_interval("[1;)")),
    "[1;Inf)",
    fixed = TRUE
  )
  expect_output(
    print(util_parse_interval("[1;]")),
    "[1;Inf]",
    fixed = TRUE
  )
  expect_output(
    print(util_parse_interval("(2001-01-01;)")),
    "(2001-01-01;Inf)",
    fixed = TRUE
  )
  expect_output(
    print(util_parse_interval("[2001-01-01;)")),
    "[2001-01-01;Inf)",
    fixed = TRUE
  )
  expect_output(
    print(util_parse_interval("[2001-01-01;]")),
    "[2001-01-01;Inf]",
    fixed = TRUE
  )
})

test_that("print.ReportSummaryTable works when called within the pipeline with duplicated labels for the plot, which are not the primary labels for the report", { # nolint: line_length_linter.
  skip_on_cran() # slow test
  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  skip_if_not_installed("stringdist")
  skip_if_not_installed("DT")
  meta_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data.RData") # nolint: line_length_linter.
  study_data <- prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData", keep_types = TRUE) # nolint: line_length_linter.
  md0 <- meta_data
  md0$STUDY_SEGMENT <- NULL
  md0$LABEL[26:27] <- "EDUCATION"
  suppressWarningsMatching(
    dq <- dq_report2(
      study_data = study_data, item_level = md0, cores = NULL,
      label_col = "LONG_LABEL",
      filter_indicator_functions = "com_item_miss"
    ),
    "Unique labels are required"
  )
  # check that the VAR_NAMES attribute exists and is correct
  exp_vn <- prep_map_labels(
    x = c("EDUCATION_0", "EDUCATION_1"),
    item_level = md0,
    to = VAR_NAMES, from = LONG_LABEL
  )
  expect_equal(
    util_report_summary_table_var_names(
      dq$com_item_missingness.EDUCATION_0$ReportSummaryTable
    ),
    exp_vn[1]
  )
  # Use class() locally to inspect ReportSummaryTable/dataquieR_result layers.
  expect_equal(
    util_report_summary_table_var_names(
      dq["EDUCATION_0", "com_item_missingness", "ReportSummaryTable"][[1]]
    ),
    exp_vn[1]
  )
  expect_equal(
    util_report_summary_table_var_names(
      dq[
        c("EDUCATION_0", "EDUCATION_1"), "com_item_missingness",
        "ReportSummaryTable"
      ][[1]]
    ),
    exp_vn
  )
  # check that the ReportSummaryTable can be printed
  expect_false(
    inherits(try(print(dq["EDUCATION_0", "com_item_missingness", "ReportSummaryTable"])), "try-error") # nolint: line_length_linter.
  )
  expect_false(
    inherits(try(dq[, "com_item_missingness", "ReportSummaryTable"]), "try-error") # nolint: line_length_linter.
  )
  suppressWarningsMatching(
    {
      expect_false(
        inherits(
          try(print(dq[, "com_item_missingness", "ReportSummaryTable"])),
          "try-error"
        )
      )
    },
    regexps = ".*not being part of one of the segments.*"
  )

  dq_rst <- dq[c("EDUCATION_0", "EDUCATION_1"), "com_item_missingness", "ReportSummaryTable"] # nolint: line_length_linter.
  expect_false(
    inherits(try(print(dq_rst)), "try-error")
  )
  expect_false(
    inherits(try(print(dq_rst$ReportSummaryTable)), "try-error")
  )
  expect_false(
    inherits(try(print(dq_rst[[1]])), "try-error")
  )
  # check that the expected axis labels appear in the figure
  exp_lab <- sprintf("%s: EDUCATION", exp_vn)
  dq_rst_plot <- ggplot_build(print(dq_rst$ReportSummaryTable))
  expect_true(
    identical(
      dq_rst_plot$layout$panel_params[[1]]$x$get_labels(),
      exp_lab
    ) ||
      identical(
        dq_rst_plot$layout$panel_params[[1]]$y$get_labels(),
        exp_lab
      )
  )
})
