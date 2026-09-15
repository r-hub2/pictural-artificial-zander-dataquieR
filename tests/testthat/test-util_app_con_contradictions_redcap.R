test_that(
  paste(
    "util_app_con_contradictions_redcap combines datatype",
    "applicability"
  ),
  {
    skip_on_cran()

    meta_data <- data.frame(VAR_NAMES = c("a", "b"), stringsAsFactors = FALSE)
    score <- util_app_con_contradictions_redcap(meta_data, c(FALSE, TRUE))

    expect_s3_class(score, "factor")
    expect_equal(as.character(score), c("1", "3"))
  }
)
