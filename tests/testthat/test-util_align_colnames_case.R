skip_on_cran()

test_that("util_align_colnames_case aligns study-data names to metadata case", {
  expect_equal(
    util_align_colnames_case(
      .colnames = c("id", "sbp_0", "unknown"),
      .var_names = c("ID", "SBP_0", "DBP_0")
    ),
    c("ID", "SBP_0", "unknown")
  )
})

test_that(
  "util_align_colnames_case coerces inputs and keeps exact duplicates",
  {
    expect_equal(
      util_align_colnames_case(
        .colnames = c("1", "x", NA),
        .var_names = c(1, "X", "X")
      ),
      c("1", "X", NA)
    )
  }
)

test_that(
  "util_align_colnames_case rejects case-insensitive metadata conflicts",
  {
    expect_error(
      util_align_colnames_case(
        .colnames = "age",
        .var_names = c("AGE", "age")
      ),
      "case-insensitively distinct",
      class = dataquieR.applicability_problem
    )
  }
)
