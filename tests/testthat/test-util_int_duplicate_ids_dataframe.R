test_that(
  "util_int_duplicate_ids_dataframe derives ID variables from metadata",
  {
    skip_on_cran()
    prep_purge_data_frame_cache()
    withr::defer(prep_purge_data_frame_cache())

    prep_add_data_frames(
      df_a = data.frame(id = c("A", "A", "B")),
      append = FALSE
    )
    meta_data_dataframe <- data.frame(
      DF_NAME = "df_a",
      DF_UNIQUE_ID = 1,
      DF_ID_VARS = "id",
      stringsAsFactors = FALSE
    )

    result <- util_int_duplicate_ids_dataframe(
      meta_data_dataframe = meta_data_dataframe
    )

    expect_equal(result$DataframeTable$NUM_int_sts_dupl_ids, 1L)
    expect_equal(result$DataframeTable$PCT_int_sts_dupl_ids, 33.333)
    expect_identical(result$DataframeTable$GRADING, 1L)
  }
)
