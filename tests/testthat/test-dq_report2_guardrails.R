test_that("dq_report2 reconciles checkpoint resume with amend settings early", {
  skip_on_cran()

  prep_purge_data_frame_cache()
  withr::defer(prep_purge_data_frame_cache())

  expect_message(
    expect_error(
      dq_report2(
        checkpoint_resumed = TRUE,
        amend = FALSE,
        cores = NULL
      ),
      "Missing .study_data."
    ),
    "I will set .checkpoint_resumed. to .FALSE."
  )
})
