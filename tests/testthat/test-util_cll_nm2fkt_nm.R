test_that("util_cll_nm2fkt_nm resolves indicator call names", {
  skip_on_cran()

  expect_equal(
    unname(util_cll_nm2fkt_nm(c(
      "con_hard_limits",
      "acc_robust_univariate_outlier",
      "des_summary_foo",
      "unknown_call"
    ))),
    c("con_limit_deviations", "acc_univariate_outlier",
      "des_summary", "unknown_call")
  )
})

test_that("util_cll_nm2fkt_nm resolves explicit report alias maps", {
  skip_on_cran()

  function_alias_map <- data.frame(
    alias = c("a1", "a2", "a3"),
    name = c("con_soft_limits", "acc_robust_univariate_outlier",
      "des_summary")
  )

  expect_equal(
    unname(util_cll_nm2fkt_nm(
      c("a1", "a2", "a3", "missing"),
      function_alias_map = function_alias_map
    )),
    c("con_limit_deviations", "acc_univariate_outlier",
      "des_summary", "missing")
  )
})

test_that("util_cll_nm2fkt_nm reads alias maps from report attributes", {
  skip_on_cran()

  matrix_list <- structure(
    list(),
    function_alias_map = data.frame(
      alias = "alias_call",
      name = "con_detection_limits"
    )
  )
  report <- structure(list(), matrix_list = matrix_list)

  expect_equal(
    unname(util_cll_nm2fkt_nm(
      c("alias_call", "unknown_call"),
      report = report
    )),
    c("con_limit_deviations", "unknown_call")
  )
})
