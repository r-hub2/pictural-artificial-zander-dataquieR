skip_on_cran()

test_that(
  "util_all_ind_functions returns exported dimension-prefixed functions",
  {
    functions <- util_all_ind_functions()
    exports <- getNamespaceExports(utils::packageName())
    prefixes <- paste0(names(dims), "_")

    expect_type(functions, "character")
    expect_true("acc_distributions" %in% functions)
    expect_true("des_summary" %in% functions)
    expect_true(all(functions %in% exports))
    expect_true(all(vapply(
      functions,
      function(name) any(startsWith(name, prefixes)),
      logical(1)
    )))
  }
)
