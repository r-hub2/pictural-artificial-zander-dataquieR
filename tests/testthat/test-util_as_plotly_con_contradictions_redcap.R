test_that(
  "util_as_plotly_con_contradictions_redcap returns its Plotly fallback",
  {
    skip_on_cran()

    testthat::local_mocked_bindings(
      util_ensure_suggested = function(...) FALSE
    )

    fallback <- util_as_plotly_con_contradictions_redcap(list())

    expect_match(as.character(fallback), "No Plotly", fixed = TRUE)
  }
)

test_that(
  "util_as_plotly_con_contradictions_redcap converts a local summary plot",
  {
    skip_on_cran()
    skip_if_not_installed("plotly")

    summary_plot <- ggplot2::ggplot(
      data.frame(
        x = c(1, 2),
        y = c(2, 3),
        grading_class = c("low", "high")
      ),
      ggplot2::aes(x = x, y = y, colour = grading_class)
    ) +
      ggplot2::geom_point()
    result <- list(SummaryPlot = summary_plot)
    attr(result, "sizing_hints") <- NULL

    converted <- util_as_plotly_con_contradictions_redcap(result)

    expect_s3_class(converted, "plotly")
    expect_gt(length(converted$x$data), 0)
  }
)

test_that("con_contradictions_redcap Plotly helper labels are stable", {
  skip_on_cran()

  expect_equal(
    util_con_contradiction_type_label("EMPIRICAL"),
    "Empirical contradictions"
  )
  expect_equal(
    util_con_contradiction_type_label("LOGICAL"),
    "Logical contradictions"
  )
  expect_equal(
    util_con_contradiction_type_label(NA_character_),
    "Contradictions"
  )
  expect_equal(
    util_con_contradiction_type_label("CUSTOM"),
    "Custom contradictions"
  )

  expect_equal(
    util_con_contradiction_type_display_label("EMPIRICAL", n_rows = 2),
    "Emp."
  )
  expect_equal(
    util_con_contradiction_type_display_label("LOGICAL", n_rows = 6),
    "Logical"
  )
  expect_equal(
    util_con_contradiction_type_display_label("CUSTOM", n_rows = 8),
    "Custom contradictions"
  )

  expect_equal(util_con_contradiction_type_band_fill("LOGICAL"), "#FAD9D9")
  expect_equal(util_con_contradiction_type_band_fill("EMPIRICAL"), "#D9D9D9")
  expect_equal(util_con_contradiction_type_band_fill("CUSTOM"), "#E8E8E8")
  expect_match(
    util_con_contradiction_type_band_fill_plotly("LOGICAL"),
    "^rgba\\("
  )
})

test_that("con_contradictions_redcap Plotly bands update layout", {
  skip_on_cran()

  py <- list(x = list(
    layout = list(yaxis = list(range = c(0, 10), domain = c(0.2, 0.8))),
    data = list()
  ))
  bands <- data.frame(
    xmin = c(1, 5),
    xmax = c(3, 8),
    xmid = c(2, 6.5),
    display_label = c("Emp.", "Log."),
    fill_plotly = c("rgba(1,1,1,0.1)", "rgba(2,2,2,0.2)"),
    stringsAsFactors = FALSE
  )

  updated <- util_con_con_add_plotly_type_bands(py, bands)

  expect_length(updated$x$layout$shapes, 2)
  expect_length(updated$x$layout$annotations, 2)
  expect_equal(updated$x$layout$margin$r, 160)
  expect_equal(updated$x$layout$paper_bgcolor, "rgba(0,0,0,0)")
  expect_equal(updated$x$layout$plot_bgcolor, "rgba(0,0,0,0)")
  expect_true(all(vapply(
    updated$x$layout$shapes,
    `[[`,
    character(1),
    "xref"
  ) == "paper"))

  expect_identical(util_con_con_add_plotly_type_bands(py, NULL), py)
  expect_identical(util_con_con_add_plotly_type_bands(py, bands[0, ]), py)

  flat_py <- list(x = list(layout = list(yaxis = list(range = c(1, 1)))))
  expect_identical(util_con_con_add_plotly_type_bands(flat_py, bands), flat_py)
})

test_that("con_contradictions_redcap Plotly hover helpers shorten text", {
  skip_on_cran()

  hover <- paste(
    "CONTRADICTION_TYPE: empirical",
    "PCT_con_con: 12,345%",
    sep = "<br>"
  )

  expect_equal(
    util_con_con_extract_hover_field(hover, "PCT_con_con"),
    "12,345%"
  )
  expect_equal(
    util_con_con_short_hover(hover),
    "Empirical - 12.35%"
  )
  expect_equal(
    util_con_con_short_hover("CONTRADICTION_TYPE: logical"),
    "Logical"
  )
  expect_equal(
    util_con_con_short_hover("PCT_con_con: not-a-number"),
    "Contradictions"
  )

  py <- list(x = list(data = list(
    list(type = "bar", hovertext = hover),
    list(type = "scatter", mode = "markers", text = "unchanged")
  )))
  cleaned <- util_con_con_clean_plotly_hover(py)

  expect_equal(unname(cleaned$x$data[[1]]$hovertext), "Empirical - 12.35%")
  expect_equal(cleaned$x$data[[1]]$hoverinfo, "text")
  expect_equal(cleaned$x$data[[2]]$text, "unchanged")
})

test_that("con_contradictions_redcap type-label traces can be removed", {
  skip_on_cran()

  bands <- data.frame(display_label = c("Emp.", "Log."))
  py <- list(x = list(data = list(
    list(type = "scatter", mode = "text", text = c("Emp.", "Log.")),
    list(type = "scatter", mode = "markers", text = c("Emp.", "Log.")),
    list(type = "bar", text = "Emp.")
  )))

  cleaned <- util_con_con_remove_plotly_type_label_traces(py, bands)

  expect_length(cleaned$x$data, 2)
  expect_identical(cleaned$x$data[[1]]$mode, "markers")
  expect_identical(cleaned$x$data[[2]]$type, "bar")
  expect_identical(util_con_con_remove_plotly_type_label_traces(py, NULL), py)
})
