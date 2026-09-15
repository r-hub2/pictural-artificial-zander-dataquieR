test_that("util_map_by_largest_prefix works", {
  skip_on_cran()

  manual_like_haystack <- c(
    "aaaa", "acc_distributions_loc", "acc_distributions", "xxx"
  )

  expect_equal(util_map_by_largest_prefix(
    "acc_distributions_loc_ecdf_observer_time",
    manual_like_haystack
  ), "acc_distributions_loc")

  expect_equal(
    util_map_by_largest_prefix(
      "acc_distributions_loc_observer_time",
      manual_like_haystack
    ),
    "acc_distributions_loc"
  )

  expect_equal(
    util_map_by_largest_prefix(
      "acc_distributions_loc_ecdf",
      manual_like_haystack
    ),
    "acc_distributions_loc"
  )

  expect_equal(
    util_map_by_largest_prefix(
      "acc_distribution",
      manual_like_haystack
    ),
    NA_character_
  )

  expect_equal(
    util_map_by_largest_prefix(
      "acc_distributions_loc",
      manual_like_haystack
    ),
    "acc_distributions_loc"
  )

  expect_equal(
    util_map_by_largest_prefix(
      "acc_distributions_loessf",
      c(
        "aaaa", "acc_distributions_loc_ecdf", "acc_distributions_loc",
        "acc_distributions", "xxx"
      )
    ),
    "acc_distributions"
  )

  expect_equal(
    util_map_by_largest_prefix(
      "acc_distributions_loessf",
      c(
        "aaaa", "acc_distributions_loc_ecdf", "acc_distributions_loc",
        "acc_distributsons", "xxx"
      )
    ),
    NA_character_
  )

  expect_equal(
    util_map_by_largest_prefix(
      "acc_distributions_loess",
      c(
        "aaaa", "acc_distributions_loc_ecdf", "acc_distributions_loc",
        "acc_distributions", "xxx", "acc_distributions_loess"
      )
    ),
    "acc_distributions_loess"
  )

  expect_equal(
    util_map_by_largest_prefix(
      "acc_distributions_loessf",
      c(
        "aaaa", "acc_distributions_loc_ecdf", "acc_distributions_loc",
        "acc_distributions", "xxx", "acc_distributions_loess"
      )
    ),
    "acc_distributions"
  )

  expect_equal(
    util_map_by_largest_prefix(
      "acc_distributions_loess",
      c(
        "aaaa", "acc_distributions_loc_ecdf", "acc_distributions_loc",
        "acc_distributions", "xxx", "acc_distributions_loessf"
      )
    ),
    "acc_distributions"
  )

  expect_equal(
    util_map_by_largest_prefix(
      split_char = "",
      "acc_distributions_loessf",
      c(
        "aaaa", "acc_distributions_loc_ecdf", "acc_distributions_loc",
        "acc_distributions", "xxx"
      )
    ),
    "acc_distributions"
  )


  expect_equal(
    util_map_by_largest_prefix(
      split_char = "",
      "acc_distributions_loess",
      c(
        "aaaa", "acc_distributions_loc_ecdf", "acc_distributions_loc",
        "acc_distributions", "xxx", "acc_distributions_loess"
      )
    ),
    "acc_distributions_loess"
  )

  expect_equal(
    util_map_by_largest_prefix(
      split_char = "",
      "acc_distributions_loessf",
      c(
        "aaaa", "acc_distributions_loc_ecdf", "acc_distributions_loc",
        "acc_distributions", "xxx", "acc_distributions_loess"
      )
    ),
    "acc_distributions_loess"
  )

  expect_equal(
    util_map_by_largest_prefix(
      split_char = "",
      "acc_distributions_loess",
      c(
        "aaaa", "acc_distributions_loc_ecdf", "acc_distributions_loc",
        "acc_distributions", "xxx", "acc_distributions_loessf"
      )
    ),
    "acc_distributions"
  )

  expect_equal(
    util_map_by_largest_prefix(
      split_char = "",
      "acc_distributions_loess",
      c(
        "aaaa", "acc_distributions_loc_ecdf", "acc_distributions_loc",
        "acc_distributionsR", "xxx", "acc_distributions_loessf"
      )
    ),
    NA_character_
  )
})

test_that("util_map_by_largest_prefix handles names and variable suffixes", {
  skip_on_cran()
  haystack <- c(
    "title a" = "acc_distributions",
    "title b" = "acc_distributions_loc",
    "title c" = "acc_distributions_loc_ecdf"
  )

  expect_equal(
    util_map_by_largest_prefix(
      "acc_distributions_loc_ecdf.GROUP_VAR_OBSERVER",
      haystack
    ),
    "acc_distributions_loc_ecdf"
  )
  expect_equal(
    util_map_by_largest_prefix(
      "acc_distributions_loc_ecdf.GROUP_VAR_OBSERVER",
      haystack,
      remove_var_suffix = FALSE
    ),
    "acc_distributions_loc"
  )
})
