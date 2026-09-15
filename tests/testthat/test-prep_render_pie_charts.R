skip_on_cran()

test_that("prep_render_pie_chart_from_summaryclasses_ggplot2 works", {
  skip_if_not_installed("stringdist")

  summary <- .dq_test_mini_report_summary()
  classes <- suppressWarnings(prep_summary_to_classes(summary))

  plot_tab <- classes %>%
    dplyr::filter(!is.na(value)) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c(VAR_NAMES, "call_names")))) %>% # nolint: line_length_linter.
    dplyr::summarise(
      class =
        suppressWarnings(
          util_as_cat(max(util_as_cat(class), na.rm = TRUE))
        )
    )

  suppressWarnings(sum_plot_tab <- plot_tab %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c("class", "call_names")))) %>%
    dplyr::summarise(
      value = length(VAR_NAMES),
      note = util_pretty_vector_string(
        n_max = 3,
        suppressWarnings(prep_get_labels(VAR_NAMES,
            max_len = 15,
            label_class = "SHORT",
            meta_data =
              summary$meta_data
          ))
      )
    ))


  suppressWarnings(
    r <- prep_render_pie_chart_from_summaryclasses_ggplot2(sum_plot_tab,
      meta_data =
        summary$meta_data
    )
  )
  r <- as.character(r)
  # expect_snapshot_value(r, style = "deparse") snapshots do not work, here. the plots differ (like the label is on the test platform on top locally at the bottom of the big pie-piece) # nolint: line_length_linter.
  # do a very unspecific verification:

  expect_gt(nchar(r), 20000)
})

test_that(
  paste0(
    "prep_render_pie_chart_from_summaryclasses_ggplot2 ",
    "handles empty and unclassified data"
  ),
  {
    skip_on_cran()

    meta_data <- data.frame(
      VAR_NAMES = c("a", "b"),
      LABEL = c("A", "B"),
      LONG_LABEL = c("A long", "B long"),
      DATA_TYPE = c(DATA_TYPES$INTEGER, DATA_TYPES$INTEGER),
      SCALE_LEVEL = c(SCALE_LEVELS$RATIO, SCALE_LEVELS$RATIO),
      MISSING_LIST = "",
      JUMP_LIST = "",
      stringsAsFactors = FALSE
    )

    empty_data <- data.frame(
      class = integer(0),
      VAR_NAMES = character(0),
      value = integer(0)
    )
    empty_plot <- suppressWarnings(
      prep_render_pie_chart_from_summaryclasses_ggplot2(empty_data, meta_data)
    )

    expect_s3_class(empty_plot, "html")
    expect_identical(as.character(empty_plot), "")

    unclassified_data <- data.frame(
      class = c(NA_integer_, NA_integer_),
      VAR_NAMES = c("a", "b"),
      value = c(1, 1)
    )
    unclassified_plot <- suppressWarnings(
      prep_render_pie_chart_from_summaryclasses_ggplot2(
        unclassified_data,
        meta_data
      )
    )

    expect_match(as.character(unclassified_plot), "<table>", fixed = TRUE)
    expect_match(
      as.character(unclassified_plot),
      "A summary of data quality assessments",
      fixed = TRUE
    )
  }
)

test_that(
  "prep_render_pie_chart_from_summaryclasses_ggplot2 lays out multiple groups",
  {
    skip_on_cran()

    meta_data <- data.frame(
      VAR_NAMES = c("a", "b", "c"),
      LABEL = c("A", "B", "C"),
      LONG_LABEL = c("A long", "B long", "C long"),
      stringsAsFactors = FALSE
    )
    grouped_data <- data.frame(
      class = c(1, 2, 1),
      STUDY_SEGMENT = c("segment 1", "segment 1", "segment 2"),
      value = c(2, 1, 3)
    )

    grouped_plot <- suppressWarnings(
      prep_render_pie_chart_from_summaryclasses_ggplot2(grouped_data, meta_data)
    )
    grouped_html <- as.character(grouped_plot)

    expect_match(grouped_html, "<table>", fixed = TRUE)
    expect_match(grouped_html, "<td>", fixed = TRUE)
    expect_gt(nchar(grouped_html), 1000)
  }
)

test_that("pie grids handle an odd number of panels", {
  skip_on_cran()
  skip_if_not_installed("plotly")
  skip_if_not_installed("htmltools")

  data <- data.frame(
    class = rep(1L, 3),
    custom_group = c("Group A", "Group B", "Group C"),
    value = rep(1L, 3)
  )

  ggplot_grid <- suppressWarnings(
    prep_render_pie_chart_from_summaryclasses_ggplot2(data, data.frame())
  )
  plotly_grid <- suppressWarnings(
    prep_render_pie_chart_from_summaryclasses_plotly(data, data.frame())
  )

  ggplot_html <- as.character(ggplot_grid)
  plotly_html <- as.character(plotly_grid)
  expect_length(
    gregexpr("<td", ggplot_html, fixed = TRUE)[[1]],
    4L
  )
  expect_length(
    gregexpr("<td", plotly_html, fixed = TRUE)[[1]],
    4L
  )
  expect_match(plotly_html, "Group C", fixed = TRUE)
})

test_that("exported pie functions retain call suffixes", {
  skip_on_cran()
  skip_if_not_installed("plotly")
  skip_if_not_installed("htmltools")

  data <- data.frame(
    class = c(1L, 2L),
    call_names = "acc_distributions_custom",
    value = c(2L, 1L)
  )
  withr::local_options(lifecycle_verbosity = "quiet")

  ggplot_result <- do.call(
    dataquieR::prep_render_pie_chart_from_summaryclasses_ggplot2,
    list(data = data, meta_data = data.frame())
  )
  plotly_result <- do.call(
    dataquieR::prep_render_pie_chart_from_summaryclasses_plotly,
    list(data = data, meta_data = data.frame())
  )

  expect_s3_class(ggplot_result, "shiny.tag")
  expect_match(as.character(plotly_result), ": Custom", fixed = TRUE)
})

test_that(
  "prep_render_pie_chart_from_summaryclasses_ggplot2 handles title variants",
  {
    skip_on_cran()

    meta_data <- data.frame(
      VAR_NAMES = c("a", "b"),
      LABEL = c("A", "B"),
      LONG_LABEL = c("A long", "B long"),
      DATA_TYPE = c(DATA_TYPES$INTEGER, DATA_TYPES$INTEGER),
      SCALE_LEVEL = c(SCALE_LEVELS$RATIO, SCALE_LEVELS$RATIO),
      MISSING_LIST = "",
      JUMP_LIST = "",
      stringsAsFactors = FALSE
    )
    cases <- list(
      indicator_metric = data.frame(
        class = c(1L, 2L),
        indicator_metric = "GRADING",
        value = c(2L, 1L)
      ),
      variable = data.frame(
        class = c(1L, 2L),
        VAR_NAMES = "a",
        value = c(2L, 1L)
      ),
      function_name = data.frame(
        class = factor(c("cat1", "cat2")),
        function_name = "acc_distributions",
        value = c(2L, 1L)
      ),
      fallback = data.frame(
        class = c(1L, 2L),
        custom_group = "custom",
        value = c(2L, 1L)
      )
    )

    plots <- lapply(cases, function(data) {
      suppressWarnings(
        prep_render_pie_chart_from_summaryclasses_ggplot2(data, meta_data)
      )
    })
    html <- vapply(plots, as.character, FUN.VALUE = character(1))

    expect_true(all(vapply(
      plots, inherits, "shiny.tag", FUN.VALUE = logical(1)
    )))
    expect_true(all(nchar(html) > 1000))
  }
)

test_that(
  "prep_render_pie_chart_from_summaryclasses_plotly handles local edge cases",
  {
    skip_on_cran()
    skip_if_not_installed("plotly")
    skip_if_not_installed("htmltools")

    meta_data <- data.frame(
      VAR_NAMES = c("a", "b", "c"),
      LABEL = c("A", "B", "C"),
      LONG_LABEL = c("A long", "B long", "C long"),
      DATA_TYPE = DATA_TYPES$INTEGER,
      SCALE_LEVEL = SCALE_LEVELS$RATIO,
      MISSING_LIST = SPLIT_CHAR,
      JUMP_LIST = SPLIT_CHAR,
      stringsAsFactors = FALSE
    )

    empty_data <- data.frame(
      class = integer(0),
      indicator_metric = character(0),
      value = integer(0)
    )
    empty_plot <- suppressWarnings(
      prep_render_pie_chart_from_summaryclasses_plotly(empty_data, meta_data)
    )
    expect_s3_class(empty_plot, "html")
    expect_identical(as.character(empty_plot), "")

    single_group <- data.frame(
      class = c(1, 2, 3),
      indicator_metric = rep("GRADING", 3),
      value = c(2, 1, 1),
      note = c("first", "second", "third")
    )
    single_plot <- suppressWarnings(
      prep_render_pie_chart_from_summaryclasses_plotly(single_group, meta_data)
    )

    expect_s3_class(single_plot, "shiny.tag")
    expect_identical(single_plot$name, "span")
    expect_identical(single_plot$attribs$title, "GRADING")
    expect_s3_class(single_plot$children[[1]], "plotly")
    expect_match(
      as.character(single_plot),
      "label+percent+value+text",
      fixed = TRUE
    )
    expect_match(as.character(single_plot), "data-tippy-always-on",
      fixed = TRUE)

    all_missing <- data.frame(
      class = c(NA_integer_, NA_integer_),
      indicator_metric = "GRADING",
      value = c(1, 1)
    )
    missing_plot <- suppressWarnings(
      prep_render_pie_chart_from_summaryclasses_plotly(all_missing, meta_data)
    )
    expect_s3_class(missing_plot, "shiny.tag")
    expect_match(
      as.character(missing_plot),
      "Not classi<br>fied",
      fixed = TRUE
    )

    grouped_data <- data.frame(
      class = c(1, 2, 1),
      VAR_NAMES = c("a", "a", "b"),
      value = c(2, 1, 3)
    )
    grouped_plot <- suppressWarnings(
      prep_render_pie_chart_from_summaryclasses_plotly(grouped_data, meta_data)
    )
    grouped_html <- as.character(grouped_plot)

    expect_s3_class(grouped_plot, "shiny.tag")
    expect_match(grouped_html, "<table>", fixed = TRUE)
    expect_match(grouped_html, "<td>", fixed = TRUE)
    expect_gt(nchar(grouped_html), 1000)

    ssi_data <- single_group
    attr(ssi_data, "vars_to_include") <- "ssi"
    ssi_plot <- suppressWarnings(
      prep_render_pie_chart_from_summaryclasses_plotly(ssi_data, meta_data)
    )
    expect_false(grepl(
      "variables classified",
      as.character(ssi_plot),
      fixed = TRUE
    ))
  }
)

test_that("prep_render_pie_chart_from_summaryclasses_plotly skips gracefully", {
  skip_on_cran()

  testthat::local_mocked_bindings(
    util_ensure_suggested = function(...) FALSE
  )

  data <- data.frame(
    class = c(1L, 2L),
    indicator_metric = "GRADING",
    value = c(2L, 1L)
  )

  expect_null(suppressWarnings(
    prep_render_pie_chart_from_summaryclasses_plotly(data)
  ))
})

test_that(
  "prep_render_pie_chart_from_summaryclasses_plotly handles title variants",
  {
    skip_on_cran()
    skip_if_not_installed("plotly")
    skip_if_not_installed("htmltools")

    meta_data <- data.frame(
      VAR_NAMES = c("a", "b"),
      LABEL = c("A", "B"),
      LONG_LABEL = c("A long", "B long"),
      stringsAsFactors = FALSE
    )
    cases <- list(
      call_names = data.frame(
        class = c(1L, 2L),
        call_names = "acc_distributions",
        value = c(2L, 1L)
      ),
      suffixed_call_names = data.frame(
        class = c(1L, 2L),
        call_names = "acc_distributions_ABC",
        value = c(2L, 1L)
      ),
      segment = data.frame(
        class = c(1L, 2L),
        STUDY_SEGMENT = "baseline",
        value = c(2L, 1L)
      ),
      function_name = data.frame(
        class = factor(c("cat1", "cat2")),
        function_name = "acc_distributions",
        value = c(2L, 1L)
      ),
      fallback = data.frame(
        class = c(1L, 2L),
        custom_group = "custom",
        value = c(2L, 1L)
      )
    )

    plots <- lapply(cases, function(data) {
      suppressWarnings(
        prep_render_pie_chart_from_summaryclasses_plotly(data, meta_data)
      )
    })

    expect_identical(plots$call_names$attribs$title, "acc_distributions")
    expect_match(
      as.character(plots$suffixed_call_names),
      ": ABC",
      fixed = TRUE
    )
    expect_identical(plots$segment$attribs$title, "baseline")
    expect_identical(plots$function_name$attribs$title, "acc_distributions")
    expect_identical(plots$fallback$attribs$title, "custom")
    expect_true(all(vapply(
      plots,
      function(plot) {
        inherits(plot$children[[1]], "plotly")
      },
      logical(1)
    )))
  }
)

test_that(
  "prep_render_pie_chart_from_summaryclasses_plotly adjusts margins",
  {
    skip_on_cran()
    skip_if_not_installed("plotly")
    skip_if_not_installed("htmltools")

    meta_data <- data.frame(
      VAR_NAMES = paste0("v", seq_len(5)),
      LABEL = paste0("V", seq_len(5)),
      LONG_LABEL = paste0("Variable ", seq_len(5)),
      stringsAsFactors = FALSE
    )
    even_data <- data.frame(
      class = c(1L, 2L, 3L),
      custom_group = "Short title",
      value = c(1L, 1L, 1L)
    )
    crowded_data <- data.frame(
      class = c(1L, 2L, 3L, 4L, 5L),
      custom_group = "Short title",
      value = c(20L, 1L, 50L, 1L, 28L)
    )
    attr(even_data, "vars_to_include") <- "variable_group"
    attr(crowded_data, "vars_to_include") <- "variable_group"

    even_plot <- suppressWarnings(
      prep_render_pie_chart_from_summaryclasses_plotly(even_data, meta_data)
    )
    crowded_plot <- suppressWarnings(
      prep_render_pie_chart_from_summaryclasses_plotly(crowded_data, meta_data)
    )
    uneven_data <- data.frame(
      class = c(1L, 2L),
      custom_group = "Uneven",
      value = c(9L, 1L)
    )
    attr(uneven_data, "vars_to_include") <- "variable_group"
    uneven_plot <- suppressWarnings(
      prep_render_pie_chart_from_summaryclasses_plotly(uneven_data, meta_data)
    )

    plots <- list(even_plot, crowded_plot, uneven_plot)
    layouts <- lapply(plots, function(plot) {
      plot$children[[1]]$x$layoutAttrs[[1]]
    })
    expect_true(all(vapply(
      layouts,
      function(layout) identical(layout$margin$t, 60),
      logical(1)
    )))
    expect_true(all(vapply(
      layouts,
      function(layout) identical(layout$margin$b, 85),
      logical(1)
    )))
    expect_true(all(vapply(
      layouts,
      function(layout) identical(layout$legend$orientation, "h"),
      logical(1)
    )))
  }
)

test_that("pie labels omit sectors that cannot hold readable text", {
  skip_on_cran()
  skip_if_not_installed("plotly")
  skip_if_not_installed("htmltools")

  values <- c(26L, 14L, 7L, 4L, 2L)
  expect_identical(
    util_pie_display_percentages(values),
    c("49.1%", "26.4%", "13.2%", "", "")
  )
  expect_identical(
    util_pie_display_percentages(c(0, NA_real_)),
    c("", "")
  )
  expect_error(util_pie_display_percentages(values, min_fraction = 1.1))

  data <- data.frame(
    class = seq_along(values),
    custom_group = "Crowded summary",
    value = values,
    note = paste("Class", seq_along(values))
  )
  plot <- suppressWarnings(
    prep_render_pie_chart_from_summaryclasses_plotly(data, data.frame())
  )
  attrs <- plot$children[[1]]$x$attrs[[2]]

  expect_identical(attrs$text, util_pie_display_percentages(values))
  expect_identical(attrs$textposition, "inside")
  expect_identical(attrs$textinfo, "text")
  expect_true(attrs$showlegend)
  expect_identical(attrs$customdata, data$note)
  expect_match(attrs$hovertemplate, "%{customdata}", fixed = TRUE)

  attr(data, "vars_to_include") <- "variable_group"
  attr(data, "summary_subtitle") <- "checks, not affected observations"
  attr(data, "summary_unit_label") <- "contradiction checks"
  contradiction_plot <- suppressWarnings(
    prep_render_pie_chart_from_summaryclasses_plotly(data, data.frame())
  )
  contradiction_attrs <- contradiction_plot$children[[1]]$x$attrs[[2]]
  contradiction_layout <-
    contradiction_plot$children[[1]]$x$layoutAttrs[[1]]
  expect_match(
    contradiction_attrs$hovertemplate,
    "%{value} contradiction checks",
    fixed = TRUE
  )
  expect_match(
    contradiction_layout$title$text,
    "not affected observations",
    fixed = TRUE
  )
})

test_that("Plotly pie titles wrap and reserve vertical space", {
  skip_on_cran()
  skip_if_not_installed("plotly")
  skip_if_not_installed("htmltools")

  long_title <- paste(
    "8 of 9 available variable groups classified",
    "(17 requested)"
  )
  data <- data.frame(
    class = c(1L, 2L, 3L),
    custom_group = long_title,
    value = c(1L, 1L, 1L)
  )
  plot <- suppressWarnings(
    prep_render_pie_chart_from_summaryclasses_plotly(data, data.frame())
  )
  layout <- plot$children[[1]]$x$layoutAttrs[[1]]

  expect_match(layout$title$text, "<br>", fixed = TRUE)
  expect_gte(layout$margin$t, 78)
  expect_identical(plot$children[[1]]$height, 436)

  unbroken <- util_plotly_wrap_text(paste(rep("x", 80), collapse = ""))
  expect_identical(attr(unbroken, "line_count", exact = TRUE), 3L)
  expect_equal(length(strsplit(as.character(unbroken), "<br>", fixed = TRUE)[[1]]), 3L) # nolint: line_length_linter.
})
