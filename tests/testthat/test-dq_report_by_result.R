test_that("dq_report_by keeps reports in memory or flushes them to disk", {
  skip_on_cran()

  study_data <- data.frame(
    x = c(1L, NA_integer_, 3L, 4L),
    y = c(1L, 1L, 2L, 2L),
    z = c(2L, NA_integer_, 2L, 3L)
  )
  item_level <- data.frame(
    VAR_NAMES = c("x", "y", "z"),
    LABEL = c("x", "y", "z"),
    DATA_TYPE = rep(DATA_TYPES$INTEGER, 3),
    SCALE_LEVEL = c(
      SCALE_LEVELS$RATIO,
      SCALE_LEVELS$NOMINAL,
      SCALE_LEVELS$NOMINAL
    ),
    MISSING_LIST = rep(SPLIT_CHAR, 3),
    JUMP_LIST = rep(SPLIT_CHAR, 3),
    STUDY_SEGMENT = c("A", "A", "B"),
    stringsAsFactors = FALSE
  )
  report_args <- list(
    study_data = study_data,
    item_level = item_level,
    segment_column = STUDY_SEGMENT,
    dimensions = "Completeness",
    filter_indicator_functions = "^com_item_missingness$",
    filter_result_slots = "^SummaryTable$",
    cores = NULL,
    also_print = FALSE,
    view = FALSE
  )

  in_memory <- suppressWarnings(suppressMessages(
    do.call(dq_report_by, report_args)
  ))
  in_memory_leaves <- unlist(unclass(in_memory), recursive = FALSE)

  expect_s3_class(in_memory, "dataquieR_report_by")
  expect_true(is.list(in_memory))
  expect_identical(names(in_memory), c("A", "B"))
  expect_true(all(vapply(
    in_memory_leaves,
    inherits,
    FUN.VALUE = logical(1),
    what = "dataquieR_resultset2"
  )))

  output_parent <- withr::local_tempdir("dq-report-by-result-")
  output_dir <- file.path(output_parent, "new-output")
  expect_false(dir.exists(output_dir))

  disk_backed <- suppressWarnings(suppressMessages(do.call(
    dq_report_by,
    c(report_args, list(dir = output_dir))
  )))
  disk_leaves <- unlist(unclass(disk_backed), recursive = FALSE)

  contains_report_data <- function(x) {
    if (inherits(x, "dataquieR_resultset2") ||
        inherits(x, "dataquieR_result") ||
        is.data.frame(x)) {
      return(TRUE)
    }
    children <- if (is.list(x)) unclass(x) else list()
    attrs <- attributes(x)
    attrs[["class"]] <- NULL
    any(vapply(
      c(children, attrs),
      contains_report_data,
      FUN.VALUE = logical(1)
    ))
  }

  expect_s3_class(disk_backed, "dataquieR_report_by")
  expect_true(is.list(disk_backed))
  expect_identical(names(disk_backed), names(in_memory))
  expect_true(all(vapply(disk_leaves, is.null, FUN.VALUE = logical(1))))
  expect_false(contains_report_data(disk_backed))
  expect_true(dir.exists(output_dir))
  expect_length(
    list.files(output_dir, pattern = "[.]dq2$", recursive = TRUE),
    2
  )
  expect_length(
    list.files(output_dir, pattern = "[.]html$", recursive = TRUE),
    0
  )
  expect_error(
    suppressWarnings(suppressMessages(do.call(
      dq_report_by,
      c(report_args, list(dir = output_dir))
    ))),
    "already exists"
  )
})
