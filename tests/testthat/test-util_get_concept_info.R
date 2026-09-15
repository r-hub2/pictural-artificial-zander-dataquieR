test_that("util_get_concept_info reads shipped concept tables", {
  skip_on_cran()

  dqi <- util_get_concept_info("dqi")

  expect_s3_class(dqi, "data.frame")
  expect_true(all(c("abbreviation", "Name") %in% colnames(dqi)))
  expect_gt(nrow(dqi), 0)
})

test_that(
  "util_get_concept_info uses cached concept tables and subset arguments",
  {
    skip_on_cran()

    assign(
      "zz_concept_cache",
      data.frame(id = c("keep", "drop"), value = c(1, 2)),
      envir = .concept_chache
    )
    withr::defer(rm("zz_concept_cache", envir = .concept_chache))

    expect_equal(
      util_get_concept_info("zz_concept_cache", id == "keep", "value",
        drop = TRUE),
      1
    )

    expect_warning(
      result <- util_get_concept_info("zz_concept_cache", missing_column == 1),
      "object 'missing_column' not found"
    )
    expect_null(result)
  }
)

test_that("util_get_concept_info reports missing concept files", {
  skip_on_cran()

  expect_error(
    util_get_concept_info("missing_concept_table"),
    "Cannot read file"
  )
})
