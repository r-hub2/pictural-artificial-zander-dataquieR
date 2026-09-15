test_that("util_parallel_futures works", {
  # testthat::skip_if(identical(Sys.getenv("R_COVR"), "true"),
  #                   message = "Crashes, if instrumented")
  skip_on_cran() # slow, parallel, ...
  skip_if_not_installed("future")
  skip_if_not_installed("stringdist")

  study_data <- data.frame(
    x = c(1L, 2L, NA_integer_, 4L),
    y = c(1L, 1L, 2L, 2L)
  )
  meta_data <- data.frame(
    VAR_NAMES = c("x", "y"),
    LABEL = c("x", "y"),
    DATA_TYPE = c(DATA_TYPES$INTEGER, DATA_TYPES$INTEGER),
    SCALE_LEVEL = c(SCALE_LEVELS$RATIO, SCALE_LEVELS$NOMINAL),
    MISSING_LIST = c(SPLIT_CHAR, SPLIT_CHAR),
    JUMP_LIST = c(SPLIT_CHAR, SPLIT_CHAR),
    stringsAsFactors = FALSE
  )

  prep_purge_data_frame_cache()
  withr::defer(prep_purge_data_frame_cache())

  invisible(testthat::capture_output_lines(gc(full = TRUE, verbose = FALSE)))

  # don't include huge reports as RData in the package
  expect_warning(
    expect_warning(
      report <- dq_report2(study_data, meta_data,
        resp_vars = c("x", "y"),
        filter_indicator_functions =
          c("^com_item_missingness$"),
        filter_result_slots =
          c("^SummaryTable$"),
        cores = list(mode = "socket", cores = 1),
        mode = "futures",
        dimensions = "Completeness"
      ),
      regexp = ".*mode.*testthat",
      perl = TRUE
    ),
    regexp = ".*cores.*testthat",
    perl = TRUE
  )

  if (nres(report) == 0) {
    if (identical(Sys.getenv("CI_PROJECT_ID"), "10015470")) { # only in our gitlab CI pipeline # nolint: line_length_linter.
      p <- file.path(path.expand("~"), "addtional_output")
      if (!dir.exists(p)) {
        dir.create(p, recursive = TRUE)
      }
      if (dir.exists(p)) {
        save(report, file = file.path(p, "util_parallel_futures_report.RData"))
      }
    }
  }

  expect_equal(dim(report), c(2, 1, 1))
  expect_snapshot(summary(report))
})
