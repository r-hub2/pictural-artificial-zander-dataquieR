skip_on_cran()

test_that(
  "util_function_description treats literal NA interpretations as empty",
  {
    skip_on_cran()

    testthat::local_mocked_bindings(
      util_get_concept_info = function(...) data.frame(),
      util_map_labels = function(...) "N/A"
    )

    expect_match(
      util_function_description("unknown_function"),
      "No description found",
      fixed = TRUE
    )
  }
)

test_that("util_function_description returns non-empty interpretations", {
  skip_on_cran()
  skip_if_not_installed("markdown")

  testthat::local_mocked_bindings(
    util_get_concept_info = function(...) data.frame(),
    util_map_labels = function(...) "**Interpretation** text"
  )

  expect_match(
    util_function_description("known_function"),
    "<strong>Interpretation</strong> text",
    fixed = TRUE
  )
})

test_that("util_function_description returns rendered markdown", {
  skip_if_not_installed("markdown")

  testthat::local_mocked_bindings(
    util_map_labels = function(...) "**A useful description**",
    util_get_concept_info = function(...) data.frame(),
    util_ensure_suggested = function(...) TRUE
  )

  expect_match(
    util_function_description("acc_example"),
    "<strong>A useful description</strong>",
    fixed = TRUE
  )
})

test_that("util_function_description renders markdown code fragments", {
  skip_on_cran()
  skip_if_not_installed("markdown")

  testthat::local_mocked_bindings(
    util_map_labels = function(...) {
      paste(
        "Inline `code`",
        "",
        "```",
        "block_code",
        "```",
        sep = "\n"
      )
    },
    util_get_concept_info = function(...) data.frame(),
    util_ensure_suggested = function(...) TRUE
  )

  desc <- util_function_description("known_function")

  expect_match(desc, "<code>code</code>", fixed = TRUE)
  expect_match(desc, "<pre><code>block_code", fixed = TRUE)
})

test_that("util_function_description leaves manual fallback as manual HTML", {
  skip_on_cran()
  skip_if_not_installed("markdown")

  testthat::local_mocked_bindings(
    util_get_concept_info = function(...) data.frame(),
    util_map_labels = function(...) ""
  )

  desc <- util_function_description("unknown_`code`_**bold**")

  expect_match(desc, "No description found", fixed = TRUE)
  expect_match(desc, "<tt>", fixed = TRUE)
  expect_match(desc, "`code`", fixed = TRUE)
  expect_match(desc, "**bold**", fixed = TRUE)
  expect_false(grepl("<code>code</code>", desc, fixed = TRUE))
})
