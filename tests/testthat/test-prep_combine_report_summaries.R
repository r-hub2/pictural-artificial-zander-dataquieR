skip_on_cran()

test_that("prep_combine_report_summaries works", {
  skip_if_not_installed("stringdist")

  summary <- .dq_test_mini_report_summary(c("x", "y"))
  summary2 <- .dq_test_mini_report_summary(c("y", "z"))

  # ignore deprecation warning and known bug of a superfluous warning
  suppressWarnings(comb_sum <- prep_combine_report_summaries(summary, summary2))

  expect_snapshot(comb_sum)
})

mini_summary <- function(vars = c("x", "y"), segment = "Study") {
  structure(
    list(
      Data = data.frame(
        VAR_NAMES = vars,
        STUDY_SEGMENT = segment,
        VALUE = seq_along(vars),
        stringsAsFactors = FALSE
      ),
      Table = data.frame(
        VAR_NAMES = vars,
        STUDY_SEGMENT = segment,
        VALUE = seq_along(vars),
        stringsAsFactors = FALSE
      ),
      meta_data = data.frame(
        VAR_NAMES = vars,
        LABEL = vars,
        stringsAsFactors = FALSE
      )
    ),
    class = "dq_report2_summary"
  )
}

test_that("prep_combine_report_summaries validates inputs", {
  expect_error(
    suppressWarnings(prep_combine_report_summaries(
      summaries_list = "not a list"
    )),
    "summaries_list"
  )

  expect_error(
    suppressWarnings(prep_combine_report_summaries(
      summaries_list = setNames(list(mini_summary()), ""),
      amend_segment_names = TRUE
    )),
    "must be named"
  )

  broken <- mini_summary()
  broken$Data[[STUDY_SEGMENT]] <- NULL
  expect_error(
    suppressWarnings(prep_combine_report_summaries(broken)),
    "Data"
  )

  broken <- mini_summary()
  broken$Table[[STUDY_SEGMENT]] <- NULL
  expect_error(
    suppressWarnings(prep_combine_report_summaries(broken)),
    "Table"
  )

  expect_error(
    suppressWarnings(prep_combine_report_summaries(
      structure(list(Data = data.frame()), class = "dq_report2_summary")
    )),
    "feature"
  )

  expect_error(
    suppressWarnings(prep_combine_report_summaries(
      mini_summary(),
      amend_segment_names = NA
    )),
    "amend_segment_names"
  )
})

test_that("prep_combine_report_summaries can amend segment names", {
  one <- mini_summary("x", segment = "all")
  two <- mini_summary("y", segment = "all")

  combined <- suppressWarnings(prep_combine_report_summaries(
    summaries_list = list(first = one, second = two),
    amend_segment_names = TRUE
  ))

  expect_s3_class(combined, "dq_report2_summary")
  expect_equal(as.character(combined$Data[[STUDY_SEGMENT]]),
    c("first: all", "second: all"))
  expect_equal(as.character(combined$Table[[STUDY_SEGMENT]]),
    c("first: all", "second: all"))
  expect_equal(as.character(combined$meta_data[[STUDY_SEGMENT]]),
    c("first: Study", "second: Study"))
})

test_that("prep_combine_report_summaries deduplicates overlapping variables", {
  one <- mini_summary(c("x", "y"))
  two <- mini_summary(c("y", "z"))

  expect_warning(
    combined <- suppressMessages(prep_combine_report_summaries(one, two)),
    "overlapping variables"
  )

  expect_equal(as.character(combined$Data[[VAR_NAMES]]), c("x", "y", "z"))
  expect_equal(as.character(combined$Table[[VAR_NAMES]]), c("x", "y", "z"))
  expect_equal(as.character(combined$meta_data[[VAR_NAMES]]), c("x", "y", "z"))
})
