test_that("util_finalize_sizing_hints works", {
  skip_on_cran()
  skip_if_not_installed("jsonlite")

  sh <- jsonlite::parse_json(
    '{"figure_type_id": "scatt_plot",
      "range": 63,
      "number_of_vars": 1,
      "no_char_y":3
    }'
  )

  sh <- util_finalize_sizing_hints(sh)

  expect_equal(sh$w_in_cm, 21.5, tolerance = 0.1)
  expect_equal(sh$h_in_cm, 9.3, tolerance = 0.1)

  expect_true(any(
    grepl(
      "atomic vectors",
      capture.output(sh <- util_finalize_sizing_hints("x"),
        type = "message"
      )
    )
  ))

  sh <- jsonlite::parse_json(
    '{"figure_type_id": "pass_through",
      "w": "10px"
    }'
  )

  expect_message2(sh <- util_finalize_sizing_hints(sh),
    regexp = "sizes in pixels"
  )

  expect_equal(sh$w_in_cm, 0.26, tolerance = 0.1)

  sh <- jsonlite::parse_json(
    '{"figure_type_id": "pass_through",
      "w": "10pt"
    }'
  )

  sh <- util_finalize_sizing_hints(sh)

  expect_equal(sh$w_in_cm, 0.35, tolerance = 0.1)

  sh <- jsonlite::parse_json(
    '{"figure_type_id": "pass_through",
      "w": "10pc"
    }'
  )

  sh <- util_finalize_sizing_hints(sh)

  expect_equal(sh$w_in_cm, 4.2, tolerance = 0.1)

  sh <- jsonlite::parse_json(
    '{"figure_type_id": "pass_through",
      "w": "1em"
    }'
  )

  sh <- util_finalize_sizing_hints(sh)

  expect_equal(sh$w_in_cm, 0.4, tolerance = 0.1)

  sh <- jsonlite::parse_json(
    '{"figure_type_id": "pass_through",
      "w": "1ex"
    }'
  )

  sh <- util_finalize_sizing_hints(sh)

  expect_equal(sh$w_in_cm, 0.4, tolerance = 0.1)

  sh <- jsonlite::parse_json(
    '{"figure_type_id": "pass_through",
      "w": "1ch"
    }'
  )

  sh <- util_finalize_sizing_hints(sh)

  expect_equal(sh$w_in_cm, 0.4, tolerance = 0.1)

  sh <- jsonlite::parse_json(
    '{"figure_type_id": "pass_through",
      "w": "1rem"
    }'
  )

  sh <- util_finalize_sizing_hints(sh)

  expect_equal(sh$w_in_cm, 0.4, tolerance = 0.1)

  sh <- jsonlite::parse_json(
    '{"figure_type_id": "pass_through",
      "w": "1vw"
    }'
  )

  sh <- util_finalize_sizing_hints(sh)

  expect_equal(sh$w_in_cm, 48.8, tolerance = 0.1)

  sh <- jsonlite::parse_json(
    '{"figure_type_id": "pass_through",
      "w": "1vh"
    }'
  )

  sh <- util_finalize_sizing_hints(sh)

  expect_equal(sh$w_in_cm, 27.4, tolerance = 0.1)

  sh <- jsonlite::parse_json(
    '{"figure_type_id": "pass_through",
      "w": "1vmin"
    }'
  )

  sh <- util_finalize_sizing_hints(sh)

  expect_equal(sh$w_in_cm, 27.4, tolerance = 0.1)

  sh <- jsonlite::parse_json(
    '{"figure_type_id": "pass_through",
      "w": "1vmax"
    }'
  )

  sh <- util_finalize_sizing_hints(sh)

  expect_equal(sh$w_in_cm, 48.8, tolerance = 0.1)

  sh <- jsonlite::parse_json(
    '{"figure_type_id": "pass_through",
      "w": "1%"
    }'
  )

  sh <- util_finalize_sizing_hints(sh)

  expect_equal(sh$w_in_cm, 48.8, tolerance = 0.1)

  sh <- jsonlite::parse_json(
    '{"figure_type_id":"dot_loess",
      "range":28.2342,
      "no_char_y":6,
      "n_groups":14
    }'
  )

  sh <- util_finalize_sizing_hints(sh)

  expect_equal(sh$w_in_cm, 25.4, tolerance = 0.1)
})

test_that("util_finalize_sizing_hints finalizes common figure type branches", {
  skip_on_cran()

  expect_finalized <- function(hints) {
    out <- suppressMessages(util_finalize_sizing_hints(hints))
    expect_true(all(c("figure_type_id", "w", "h", "w_in_cm", "h_in_cm") %in% names(out)))
    expect_true(is.character(out$w))
    expect_true(is.character(out$h))
    expect_true(is.numeric(out$w_in_cm))
    expect_true(is.numeric(out$h_in_cm))
    expect_true(is.finite(out$w_in_cm))
    expect_true(is.finite(out$h_in_cm))
    expect_gt(out$w_in_cm, 0)
    expect_gt(out$h_in_cm, 0)
    out
  }

  cases <- list(
    list(
      figure_type_id = "bar_chart",
      number_of_bars = 18,
      range = 100,
      rotated = TRUE,
      no_char_numbers = NA,
      no_char_vars = 3
    ),
    list(
      figure_type_id = "bar_chart",
      number_of_bars = 40,
      range = 400,
      no_char_x = 2,
      no_char_y = 3
    ),
    list(
      figure_type_id = "dot_mat",
      number_of_vars = 25,
      number_of_cat = 8,
      rotated = TRUE,
      no_char_vars = 3,
      no_char_cat = 2
    ),
    list(
      figure_type_id = "dot_mat",
      number_of_vars = 8,
      number_of_cat = 5,
      no_char_vars = 3,
      no_char_cat = 2
    ),
    list(
      figure_type_id = "dot_mat_fix",
      number_of_vars = 10,
      number_of_cat = 20,
      no_char_vars = 3,
      no_char_cat = 2
    ),
    list(
      figure_type_id = "mahalanobis_plot",
      range_x = 300,
      range_y = 150
    ),
    list(
      figure_type_id = "bar_limit",
      no_bars_in_all_w = 35,
      range = 450,
      no_char_y = 2
    ),
    list(
      figure_type_id = "marg_plot",
      n_groups = 12,
      type_plot = "count_plot",
      no_char_x = 3,
      no_char_y = 2
    ),
    list(
      figure_type_id = "marg_plot",
      n_groups = 22,
      type_plot = "violin_plot",
      no_char_x = 3,
      no_char_y = 2
    ),
    list(
      figure_type_id = "marg_plot",
      n_groups = 22,
      type_plot = "rotated_plot",
      no_char_x = 3,
      no_char_y = 2
    ),
    list(
      figure_type_id = "pairs_plot",
      number_of_vars = 7
    ),
    list(
      figure_type_id = "multivar_plot",
      number_of_vars = 8,
      range = 500,
      no_char_y = 2
    ),
    list(
      figure_type_id = "pass_through",
      w = "calc(var(--screen-width) * 0.25 * 1px);",
      h = "2in",
      custom = "keep-me"
    )
  )

  out <- lapply(cases, expect_finalized)
  expect_identical(out[[length(out)]]$custom, "keep-me")
})

test_that("util_finalize_sizing_hints converts units without units package", {
  skip_on_cran()

  testthat::local_mocked_bindings(
    util_ensure_suggested = function(...) FALSE
  )

  out <- suppressMessages(util_finalize_sizing_hints(list(
    figure_type_id = "pass_through",
    w = "1in + 10mm",
    h = "2cm"
  )))

  expect_equal(out$w_in_cm, 3.54, tolerance = 0.001)
  expect_equal(out$h_in_cm, 2, tolerance = 0.001)
})

test_that("util_finalize_sizing_hints covers size threshold branches", {
  skip_on_cran()

  expect_finalized <- function(hints) {
    out <- suppressMessages(util_finalize_sizing_hints(hints))
    expect_true(is.character(out$w))
    expect_true(is.character(out$h))
    expect_true(is.numeric(out$w_in_cm))
    expect_true(is.numeric(out$h_in_cm))
    expect_true(is.finite(out$w_in_cm))
    expect_true(is.finite(out$h_in_cm))
    expect_gt(out$w_in_cm, 0)
    expect_gt(out$h_in_cm, 0)
    out
  }

  cases <- list(
    list(
      figure_type_id = "bar_chart",
      number_of_bars = 120,
      range = 100,
      rotated = TRUE,
      no_char_numbers = 2,
      no_char_vars = 80
    ),
    list(
      figure_type_id = "bar_chart",
      number_of_bars = 20,
      range = 200,
      no_char_x = 2,
      no_char_y = 2
    ),
    list(
      figure_type_id = "bar_chart",
      number_of_bars = 35,
      range = 700,
      no_char_x = 5,
      no_char_y = 5
    ),
    list(
      figure_type_id = "dot_mat",
      number_of_vars = 25,
      number_of_cat = 4,
      rotated = TRUE,
      no_char_vars = 3,
      no_char_cat = 3
    ),
    list(
      figure_type_id = "dot_mat",
      number_of_vars = 25,
      number_of_cat = 20,
      rotated = TRUE,
      no_char_vars = 9,
      no_char_cat = 5
    ),
    list(
      figure_type_id = "dot_mat",
      number_of_vars = 5,
      number_of_cat = 3,
      no_char_vars = 3,
      no_char_cat = 3
    ),
    list(
      figure_type_id = "dot_mat",
      number_of_vars = 16,
      number_of_cat = 16,
      no_char_vars = 5,
      no_char_cat = 5
    ),
    list(
      figure_type_id = "dot_mat_fix",
      number_of_vars = 5,
      number_of_cat = 4,
      no_char_vars = 3,
      no_char_cat = 3
    ),
    list(
      figure_type_id = "dot_mat_fix",
      number_of_vars = 16,
      number_of_cat = 4,
      no_char_vars = 5,
      no_char_cat = 5
    ),
    list(
      figure_type_id = "scatt_plot",
      range = 10,
      number_of_vars = 1,
      no_char_y = 2
    ),
    list(
      figure_type_id = "scatt_plot",
      range = 150,
      number_of_vars = 2,
      no_char_y = 5
    ),
    list(
      figure_type_id = "scatt_plot",
      range = 300,
      number_of_vars = 1,
      no_char_y = 5
    ),
    list(
      figure_type_id = "mahalanobis_plot",
      range_x = 100,
      range_y = 10
    ),
    list(
      figure_type_id = "mahalanobis_plot",
      range_x = 300,
      range_y = 50
    ),
    list(
      figure_type_id = "mahalanobis_plot",
      range_x = 300,
      range_y = 300
    ),
    list(
      figure_type_id = "mahalanobis_plot",
      range_x = 500,
      range_y = 500
    ),
    list(
      figure_type_id = "bar_limit",
      no_bars_in_all_w = 20,
      range = 200,
      no_char_y = 2
    ),
    list(
      figure_type_id = "bar_limit",
      no_bars_in_all_w = 35,
      range = 700,
      no_char_y = 5
    ),
    list(
      figure_type_id = "bar_limit",
      no_bars_in_all_w = 45,
      range = 700,
      no_char_y = 5
    ),
    list(
      figure_type_id = "dot_loess",
      n_groups = 10,
      no_char_y = 2
    ),
    list(
      figure_type_id = "dot_loess",
      n_groups = 25,
      no_char_y = 5
    ),
    list(
      figure_type_id = "marg_plot",
      n_groups = 5,
      type_plot = "count_plot",
      no_char_x = 2,
      no_char_y = 2
    ),
    list(
      figure_type_id = "marg_plot",
      n_groups = 15,
      type_plot = "count_plot",
      no_char_x = 5,
      no_char_y = 5
    ),
    list(
      figure_type_id = "marg_plot",
      n_groups = 25,
      type_plot = "count_plot",
      no_char_x = 5,
      no_char_y = 5
    ),
    list(
      figure_type_id = "marg_plot",
      n_groups = 5,
      type_plot = "violin_plot",
      no_char_x = 2,
      no_char_y = numeric(0)
    ),
    list(
      figure_type_id = "marg_plot",
      n_groups = 10,
      type_plot = "violin_plot",
      no_char_x = 5,
      no_char_y = 5
    ),
    list(
      figure_type_id = "marg_plot",
      n_groups = 25,
      type_plot = "violin_plot",
      no_char_x = 5,
      no_char_y = 5
    ),
    list(
      figure_type_id = "marg_plot",
      n_groups = 5,
      type_plot = "rotated_plot",
      no_char_x = 5,
      no_char_y = 2
    ),
    list(
      figure_type_id = "marg_plot",
      n_groups = 15,
      type_plot = "rotated_plot",
      no_char_x = 5,
      no_char_y = 5
    ),
    list(
      figure_type_id = "marg_plot",
      n_groups = 25,
      type_plot = "rotated_plot",
      no_char_x = 5,
      no_char_y = 5
    ),
    list(
      figure_type_id = "pairs_plot",
      number_of_vars = 4
    ),
    list(
      figure_type_id = "multivar_plot",
      number_of_vars = 5,
      range = 50,
      no_char_y = 2
    ),
    list(
      figure_type_id = "multivar_plot",
      number_of_vars = 5,
      range = 200,
      no_char_y = 5
    ),
    list(
      figure_type_id = "multivar_plot",
      number_of_vars = 8,
      range = 500,
      no_char_y = 5
    )
  )

  invisible(lapply(cases, expect_finalized))
})

test_that("util_finalize_sizing_hints falls back but preserves original hints", {
  skip_on_cran()

  capture.output(
    out <- util_finalize_sizing_hints(list(
      figure_type_id = "bar_chart",
      note = "missing-required-sizing-fields"
    )),
    type = "message"
  )

  expect_identical(out$w, "14cm")
  expect_identical(out$h, "10cm")
  expect_identical(out$figure_type_id, "bar_chart")
  expect_identical(out$note, "missing-required-sizing-fields")
  expect_equal(out$w_in_cm, 14)
  expect_equal(out$h_in_cm, 10)
})

test_that("util_finalize_sizing_hints falls back for incomplete plot hints", {
  skip_on_cran()

  cases <- list(
    list(figure_type_id = "dot_mat", note = "missing dot matrix dimensions"),
    list(figure_type_id = "dot_mat_fix", note = "missing fixed matrix size"),
    list(figure_type_id = "scatt_plot", note = "missing scatter range"),
    list(figure_type_id = "mahalanobis_plot", note = "missing md ranges"),
    list(figure_type_id = "bar_limit", note = "missing bar limits"),
    list(figure_type_id = "dot_loess", note = "missing loess groups"),
    list(figure_type_id = "marg_plot", note = "missing margin plot type"),
    list(figure_type_id = "pairs_plot", note = "missing pair count"),
    list(figure_type_id = "multivar_plot", note = "missing multivar range")
  )

  for (hints in cases) {
    capture.output(
      out <- util_finalize_sizing_hints(hints),
      type = "message"
    )

    expect_identical(out$w, "14cm")
    expect_identical(out$h, "10cm")
    expect_identical(out$figure_type_id, hints$figure_type_id)
    expect_identical(out$note, hints$note)
    expect_equal(out$w_in_cm, 14)
    expect_equal(out$h_in_cm, 10)
  }
})

test_that("util_finalize_sizing_hints rejects malformed CSS sizing", {
  skip_on_cran()

  expect_error(
    util_finalize_sizing_hints(list(
      figure_type_id = "pass_through",
      w = "calc(1cm + bad)",
      h = "1cm"
    )),
    "Could not parse unit"
  )

  expect_error(
    util_finalize_sizing_hints(list(
      figure_type_id = "pass_through",
      w = "calc(1cm / )",
      h = "1cm"
    )),
    "Could not compute unit"
  )
})

test_that("util_fix_sizing_hints keeps sizing hints on the result object only", {
  skip_on_cran()

  dqr <- structure(list(result = TRUE), class = "dataquieR_result")
  x <- structure(list(plot = TRUE), sizing_hints = list(w = "1cm", h = "2cm"))

  fixed <- util_fix_sizing_hints(dqr, x)
  expect_identical(attr(fixed$dqr, "sizing_hints"), list(w = "1cm", h = "2cm"))
  expect_null(attr(fixed$x, "sizing_hints", exact = TRUE))

  attr(dqr, "sizing_hints") <- list(w = "9cm", h = "9cm")
  expect_error(
    util_fix_sizing_hints(dqr, x),
    "two different"
  )
})
