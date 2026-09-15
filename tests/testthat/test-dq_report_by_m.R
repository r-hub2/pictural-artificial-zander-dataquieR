test_that("dq_report_by works with content m", {
  skip_if_not_installed("DT")
  skip_if_not_installed("markdown")
  skip_if_not_installed("stringdist")

  skip_if_offline(host = "dataquality.qihs.uni-greifswald.de")
  withr::local_options(
    dataquieR.CONDITIONS_WITH_STACKTRACE = TRUE,
    dataquieR.ERRORS_WITH_CALLER = TRUE,
    dataquieR.WARNINGS_WITH_CALLER = TRUE,
    dataquieR.MESSAGES_WITH_CALLER = TRUE
  )
  skip_on_cran() # slow test
  target <- withr::local_tempdir("testdqareportby")

  study_data <- head(prep_get_data_frame("https://dataquality.qihs.uni-greifswald.de/extdata/fortests/study_data.RData", keep_types = TRUE), 100) # nolint: line_length_linter.


  # Added a new grading ruleset
  default_grading_ruleset <-
    prep_get_data_frame(system.file("grading_rulesets.xlsx",
        package = "dataquieR"
      ))

  default_grading_ruleset[default_grading_ruleset$indicator_metric ==
      "PCT_con_rvv_icat", ] <- c(
    "0",
    "PCT_con_rvv_icat",
    NA,
    NA,
    NA,
    NA,
    "[0; 0]",
    "(0; 100]"
  )

  prep_add_data_frames(grading_rulesets = default_grading_ruleset)

  render_conditions <- new.env(parent = emptyenv())
  render_conditions$compiling_messages <- character()
  render_conditions$warnings <- character()
  expect_no_error(
    withCallingHandlers(
      dq_report_by(
        study_data = study_data,
        dimensions = "int",
        cores = NULL,
        segment_column = STUDY_SEGMENT,
        segment_select = c("STUDY", "LAB"),
        meta_data_v2 = "https://dataquality.qihs.uni-greifswald.de/extdata/fortests/meta_data_v2.xlsx", # nolint: line_length_linter.
        output_dir = file.path(target, "m"),
        also_print = TRUE
      ),
      message = function(m) {
        if (identical(
          conditionMessage(m),
          "Compiling HTML report, please wait...\n"
        )) {
          render_conditions$compiling_messages <- c(
            render_conditions$compiling_messages,
            conditionMessage(m)
          )
          invokeRestart("muffleMessage")
        }
      },
      warning = function(w) {
        render_conditions$warnings <- c(
          render_conditions$warnings,
          conditionMessage(w)
        )
        invokeRestart("muffleWarning")
      }
    )
  )
  expect_identical(
    render_conditions$compiling_messages,
    rep("Compiling HTML report, please wait...\n", 2)
  )
  expect_length(render_conditions$warnings, 0)
  required_files <- c("anchor_list.js",
    "dashboard.html",
    "index.html",
    "logo.png",
    "report_by_meta.RDS",
    "report_dashboard_study_data_all_observations_LAB.RDS",
    "report_dashboard_study_data_all_observations_STUDY.RDS",
    "report_study_data_all_observations_LAB",
    "report_study_data_all_observations_LAB.dq2",
    "report_study_data_all_observations_STUDY",
    "report_study_data_all_observations_STUDY.dq2",
    "report_summary_study_data_all_observations_LAB.RDS",
    "report_summary_study_data_all_observations_STUDY.RDS",
    "tables.html")
  optional_artifacts <- c("dashboard_images", "lib")
  actual_files <- sort(list.files(file.path(target, "m")))
  expect_setequal(intersect(required_files, actual_files), required_files)
  expect_true(all(optional_artifacts %in% actual_files))
  expect_true(file.info(
    file.path(target, "m", "report_by_meta.RDS")
  )$size > 0)
  report_dirs <- file.path(
    target,
    "m",
    c(
      "report_study_data_all_observations_LAB",
      "report_study_data_all_observations_STUDY"
    )
  )
  expect_true(all(file.exists(file.path(
    report_dirs,
    ".report",
    "report.html"
  ))))
})
