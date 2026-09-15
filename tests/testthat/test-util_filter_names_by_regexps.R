test_that("util_filter_names_regexps works", {
  skip_on_cran()

  # selecting functions I want to keep with regexp----
  expect_equal(
    util_filter_names_by_regexps(
      setNames(nm = c("acc_margins", "com_item_miss", "con_limit_dev")),
      c("^acc_", "^con_")
    ),
    setNames(nm = c("acc_margins", "con_limit_dev"))
  )

  expect_equal(
    util_filter_names_by_regexps(
      setNames(nm = c("acc_margins", "com_item_miss", "con_limit_dev")),
      c("^acc_")
    ),
    setNames(nm = c("acc_margins"))
  )

  expect_equal(
    util_filter_names_by_regexps(
      setNames(nm = c("acc_margins", "com_item_miss", "con_limit_dev")),
      c()
    ),
    setNames(nm = c("acc_margins", "com_item_miss", "con_limit_dev"))
  )

  expect_error(
    util_filter_names_by_regexps(
      setNames(nm = c()), c("^acc_", "^con_")
    ),
    regexp = paste(".*Need names.*"),
    perl = TRUE
  )

  expect_identical(
    util_filter_names_by_regexps(
      setNames(nm = character(0)),
      c("^acc_", "^con_")
    ),
    setNames(character(0), character(0))
  )

  expect_equal(
    util_filter_names_by_regexps(
      setNames(nm = c("acc_margins")),
      c("^acc_", "^con_")
    ),
    setNames(nm = c("acc_margins"))
  )

  expect_identical(
    util_filter_names_by_regexps(
      setNames(nm = c("acc_margins")),
      c("^xacc_", "^con_")
    ),
    setNames(character(0), character(0))
  )

  # excluding functions I do not want to keep with regexp----
  expect_equal(
    util_filter_names_by_regexps(
      setNames(nm = c("acc_margins", "com_item_miss", "con_limit_dev")),
      c("^acc_", "^con_"),
      negate = TRUE
    ),
    setNames(nm = c("com_item_miss"))
  )

  expect_equal(
    util_filter_names_by_regexps(
      setNames(nm = c("acc_margins", "com_item_miss", "con_limit_dev")),
      c("^acc_"),
      negate = TRUE
    ),
    setNames(nm = c("com_item_miss", "con_limit_dev"))
  )

  expect_equal(
    util_filter_names_by_regexps(
      setNames(nm = c("acc_margins", "com_item_miss", "con_limit_dev")),
      c(),
      negate = TRUE
    ),
    setNames(nm = c("acc_margins", "com_item_miss", "con_limit_dev"))
  )

  expect_error(
    util_filter_names_by_regexps(
      setNames(nm = c()),
      c("^acc_", "^con_"),
      negate = TRUE
    ),
    regexp = paste(".*Need names.*"),
    perl = TRUE
  )

  expect_identical(
    util_filter_names_by_regexps(
      setNames(nm = character(0)),
      c("^acc_", "^con_"),
      negate = TRUE
    ),
    setNames(character(0), character(0))
  )

  expect_equal(
    util_filter_names_by_regexps(
      setNames(nm = c("acc_margins")),
      c("^acc_", "^con_"),
      negate = TRUE
    ),
    setNames(character(0), character(0))
  )
})

test_that("util_filter_names_by_regexps preserves attributes", {
  skip_on_cran()

  collection <- structure(
    setNames(c(1, 2, 3), c("acc_margins", "com_item_miss", "con_limit_dev")),
    custom_attribute = "keep-me"
  )

  filtered <- util_filter_names_by_regexps(collection, c("^acc_", "^con_"))

  expect_equal(as.vector(filtered), c(1, 3))
  expect_equal(names(filtered), c("acc_margins", "con_limit_dev"))
  expect_equal(attr(filtered, "custom_attribute", exact = TRUE), "keep-me")
})

test_that("util_filter_names_by_regexps validates arguments", {
  skip_on_cran()

  collection <- setNames(c(1, 2), c("acc_margins", "con_limit_dev"))

  expect_error(
    util_filter_names_by_regexps(collection, 1),
    "regexps must be characters"
  )
  expect_error(
    util_filter_names_by_regexps(collection, "^acc_", negate = c(TRUE, FALSE)),
    "negate"
  )
  expect_error(
    util_filter_names_by_regexps(unname(collection), "^acc_"),
    "Need names"
  )
})
