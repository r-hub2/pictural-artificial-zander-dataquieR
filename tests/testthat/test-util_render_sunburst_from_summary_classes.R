skip_on_cran()

local_sunburst_widget <- function(x) {
  if (inherits(x, "plotly")) {
    return(x)
  }
  if (is.list(x)) {
    for (item in x) {
      widget <- local_sunburst_widget(item)
      if (inherits(widget, "plotly")) {
        return(widget)
      }
    }
  }
  NULL
}

local_sunburst_fixture <- function(label, indicator_metric = "metric",
  metric_level = 3L, remove_lines = FALSE, include_label = TRUE,
  folder_of_report = NULL) {
  repsum <- structure(
    data.frame(dummy = 1),
    class = c("dataquieR_summary", "data.frame")
  )
  attr(repsum, "this") <- rlang::env(
    meta_data = data.frame(VAR_NAMES = "leaf", LABEL = label),
    label_col = LABEL,
    colnames_of_report = "leaf",
    rownames_of_report = "leaf"
  )

  dashboard_table <- data.frame(
    VAR_NAMES = "leaf",
    indicator_metric = indicator_metric,
    class = 3L,
    href = "leaf.html",
    popup_href = "leaf-popup.html",
    title = "Leaf result",
    stringsAsFactors = FALSE
  )
  if (include_label) {
    dashboard_table[[LABEL]] <- label
  }

  abbreviations <- rep(NA_character_, 3L)
  abbreviations[[metric_level]] <- indicator_metric
  dqi <- data.frame(
    Dimension = c(
      "Extremely long dimension label with several useful words",
      "Extremely long dimension label with several useful words",
      "Extremely long dimension label with several useful words"
    ),
    Domain = c(
      NA,
      "Extremely long domain label with several useful words",
      "Extremely long domain label with several useful words"
    ),
    Name = c(
      "Extremely long dimension label with several useful words",
      "Extremely long domain label with several useful words",
      "Extremely long indicator label with several useful words"
    ),
    Parent_Element_ID = c(NA, NA, NA),
    IndicatorID = c("dim", "dom", "metric"),
    abbreviation = abbreviations,
    Level = c(1L, 2L, 3L),
    stringsAsFactors = FALSE
  )

  testthat::local_mocked_bindings(
    util_dashboard_table = function(...) dashboard_table,
    util_get_concept_info = function(name) {
      expect_identical(name, "dqi")
      dqi
    },
    prep_get_labels = function(...) label,
    util_get_colors = function() {
      c("#55a868", "#88c999", "#f0c36d", "#e78a61", "#c44e52")
    }
  )

  suppressWarnings(util_render_sunburst_from_summary_classes(repsum,
      remove_lines = remove_lines,
      vars_to_include = if (include_label) NULL else "variable_group",
      folder_of_report = folder_of_report
    ))
}

test_that("sunburst keeps results without a DQ_OBS mapping visible", {
  skip_on_cran()
  skip_if_not_installed("plotly")
  skip_if_not_installed("htmlwidgets")
  skip_if_not_installed("rmarkdown")

  rendered <- local_sunburst_fixture(
    label = "Association group A",
    indicator_metric = "max_cor"
  )
  widget <- local_sunburst_widget(rendered)
  trace <- plotly::plotly_build(widget)$x$data[[1]]
  hover_labels <- vapply(trace$customdata, `[[`, "label",
    FUN.VALUE = character(1)
  )

  expect_true("No DQ_OBS mapping" %in% hover_labels)
  expect_true("Association group A" %in% hover_labels)
})

test_that("sunburst supports compact variable-group fallbacks", {
  skip_on_cran()
  skip_if_not_installed("plotly")
  skip_if_not_installed("htmlwidgets")
  skip_if_not_installed("rmarkdown")

  rendered <- local_sunburst_fixture(
    label = "Fallback group",
    remove_lines = TRUE,
    include_label = FALSE
  )

  expect_s3_class(local_sunburst_widget(rendered), "plotly")
})

test_that("variable-group sunburst leaves use the group label", {
  labels <- dataquieR:::util_sunburst_variable_group_labels( # nolint
    var_names = c(
      "scale-c",
      "scale-d",
      "orphan_result"
    ),
    meta_data_cross_item = data.frame(
      CHECK_ID = c("scale-c", "scale-d"),
      CHECK_LABEL = c("Scale C response group", "Scale D response group"),
      stringsAsFactors = FALSE
    ),
    fallback = c("Maximum Long String", "Missing responses", "Fallback")
  )

  expect_identical(
    labels,
    c("Scale C response group", "Scale D response group", "Fallback")
  )
})

test_that("sunburst retains concept nodes that also have abbreviations", {
  skip_on_cran()
  skip_if_not_installed("plotly")
  skip_if_not_installed("htmlwidgets")
  skip_if_not_installed("rmarkdown")

  rendered <- local_sunburst_fixture(
    label = "Scale A response group",
    indicator_metric = "domain_metric",
    metric_level = 2L
  )
  widget <- local_sunburst_widget(rendered)
  trace <- plotly::plotly_build(widget)$x$data[[1]]

  expect_true(all(trace$parents[trace$parents != ""] %in% trace$ids))
})

test_that("sunburst plotly widget inherits responsive wrapper dimensions", {
  skip_on_cran()
  skip_if_not_installed("plotly")
  skip_if_not_installed("htmlwidgets")
  skip_if_not_installed("rmarkdown")

  rendered <- local_sunburst_fixture("Short leaf")
  widget <- local_sunburst_widget(rendered)

  expect_s3_class(widget, "plotly")
  expect_identical(widget$width, "100%")
  expect_identical(widget$height, "100%")
  expect_true(widget$x$layoutAttrs[[1]]$autosize)
  expect_true(widget$x$config$responsive)
  expect_identical(widget$sizingPolicy$defaultHeight, "100%")
})

test_that("sunburst area and styling retain severity emphasis", {
  skip_on_cran()
  skip_if_not_installed("plotly")
  skip_if_not_installed("htmlwidgets")
  skip_if_not_installed("rmarkdown")

  rendered <- local_sunburst_fixture("Short leaf")
  trace <- plotly::plotly_build(local_sunburst_widget(rendered))$x$data[[1]]

  expect_true(all(trace$values == 3))
  expect_true(all(trace$marker$colors > 0))
  expect_true(all(trace$textfont$size > 12))
})

test_that("report-by sunburst omits the standalone page spacer", {
  skip_on_cran()
  skip_if_not_installed("plotly")
  skip_if_not_installed("htmlwidgets")
  skip_if_not_installed("rmarkdown")

  rendered <- local_sunburst_fixture(
    "Short leaf",
    folder_of_report = c("leaf.metric" = "report/.report/leaf.html")
  )
  html <- as.character(rendered)

  expect_false(grepl('class="navbar"', html, fixed = TRUE))
  expect_false(grepl('class="content"', html, fixed = TRUE))
  expect_match(html, "dq-sunburst-container", fixed = TRUE)
})

test_that("sunburst uses readable display labels and full hover labels", {
  skip_on_cran()
  skip_if_not_installed("plotly")
  skip_if_not_installed("htmlwidgets")
  skip_if_not_installed("rmarkdown")

  long_leaf_label <- paste(
    "This is a deliberately long outer label",
    "that should use available sunburst space better"
  )
  rendered <- local_sunburst_fixture(long_leaf_label)
  widget <- local_sunburst_widget(rendered)
  built_trace <- plotly::plotly_build(widget)$x$data[[1]]
  display_labels <- built_trace$labels
  customdata <- built_trace$customdata
  hover_labels <- vapply(customdata, `[[`, "label",
    FUN.VALUE = character(1)
  )
  hover_titles <- vapply(customdata, function(x) {
    title <- x[["title"]]
    if (is.null(title)) {
      NA_character_
    } else {
      title
    }
  }, FUN.VALUE = character(1))

  expect_match(display_labels[grepl("outer label", hover_labels)][[1]],
    "<br>",
    fixed = TRUE
  )
  expect_true(any(grepl("\\.\\.\\.$", display_labels)))
  expect_true(long_leaf_label %in% hover_labels)
  expect_true(any(grepl(long_leaf_label, hover_titles, fixed = TRUE)))
  expect_true(all(built_trace$hovertemplate ==
        "%{customdata.label}<br>%{value}<extra></extra>"
    ))
  expect_false(any(display_labels == long_leaf_label))
  expect_true(any(nchar(hover_labels) > nchar(display_labels)))
})

test_that("util_sunburst_display_labels handles edge cases", {
  expect_identical(
    util_sunburst_display_labels(c(short = "Good"))[["short"]],
    "Good"
  )
  expect_match(
    util_sunburst_display_labels(c(long = paste(
      "A very long class label that should stay readable",
      "inside the sunburst chart"
    )))[["long"]],
    "<br>",
    fixed = TRUE
  )
  expect_identical(
    util_sunburst_display_labels(c(blank = "   "))[["blank"]],
    ""
  )
  expect_true(is.na(util_sunburst_display_labels(c(missing = NA))[["missing"]]))

  no_whitespace <- paste0(rep("SHIPStudyQuestionnaireItemLabel", 3),
    collapse = ""
  )
  displayed <- util_sunburst_display_labels(no_whitespace)
  display_lines <- strsplit(displayed, "<br>", fixed = TRUE)[[1]]

  expect_length(display_lines, 3L)
  expect_true(all(nchar(display_lines) <= 12L))
  expect_true(endsWith(displayed, "..."))
  expect_false(grepl(no_whitespace, displayed, fixed = TRUE))

  aggressively_wrapped <- util_sunburst_display_labels(
    "Several short words need more than two lines",
    line_width = 8L,
    max_lines = 2L
  )
  aggressive_lines <- strsplit(
    aggressively_wrapped,
    "<br>",
    fixed = TRUE
  )[[1]]

  expect_length(aggressive_lines, 2L)
  expect_true(endsWith(aggressively_wrapped, "..."))
})

test_that("sunburst variable-group labels fall back without usable metadata", {
  var_names <- c("prefix Short", "orphan")
  fallback <- c("Fallback short", "Fallback orphan")

  expect_identical(
    util_sunburst_variable_group_labels(
      var_names,
      meta_data_cross_item = data.frame(),
      fallback = fallback
    ),
    fallback
  )
  expect_identical(
    util_sunburst_variable_group_labels(
      var_names,
      meta_data_cross_item = data.frame(
        CHECK_ID = c("short", "empty"),
        CHECK_LABEL = c(NA_character_, ""),
        stringsAsFactors = FALSE
      ),
      fallback = fallback
    ),
    fallback
  )

  labels <- util_sunburst_variable_group_labels(
    c("prefix-short", "orphan"),
    meta_data_cross_item = data.frame(
      CHECK_ID = c("short", "prefix-short"),
      CHECK_LABEL = c("Short", "prefix Short"),
      stringsAsFactors = FALSE
    ),
    fallback = fallback
  )
  expect_identical(labels, c("prefix Short", "Fallback orphan"))
})

test_that("sunburst unmapped rows handle empty and study-level results", {
  dqi <- data.frame(
    Dimension = "Completeness",
    Domain = "Missing data",
    Name = "Item missingness",
    Parent_Element_ID = NA_character_,
    IndicatorID = "DQ1",
    abbreviation = "mapped_metric",
    Level = 3L,
    stringsAsFactors = FALSE
  )
  mapped <- data.frame(
    indicator_metric = "mapped_metric",
    call_names = "com_item_missingness",
    stringsAsFactors = FALSE
  )
  expect_identical(
    util_sunburst_unmapped_dqi_rows(mapped, dqi),
    dqi[0, , drop = FALSE]
  )

  unmapped <- data.frame(
    indicator_metric = c("max_cor", "in_range", "other_metric"),
    call_names = c(NA_character_, "", "des_summary"),
    stringsAsFactors = FALSE
  )
  nodes <- util_sunburst_unmapped_dqi_rows(unmapped, dqi)

  expect_true("No DQ_OBS mapping" %in% nodes$Name)
  expect_true("Unmapped implementation" %in% nodes$Domain)
  expect_true("Maximum correlation" %in% nodes$Name)
  expect_true("Within requested range" %in% nodes$Name)
  expect_true("other metric" %in% nodes$Name)
})

test_that("sunburst provisional group rows cover empty and distinct metrics", {
  dqi <- data.frame(
    Dimension = character(),
    Domain = character(),
    Name = character(),
    Parent_Element_ID = character(),
    IndicatorID = character(),
    abbreviation = character(),
    Level = integer(),
    stringsAsFactors = FALSE
  )
  empty <- data.frame(indicator_metric = c(NA_character_, ""))
  expect_identical(
    util_sunburst_provisional_variable_group_dqi_rows(empty, dqi),
    dqi
  )

  repsum <- data.frame(
    indicator_metric = c("metric_a", "metric_a", "metric_b"),
    stringsAsFactors = FALSE
  )
  nodes <- util_sunburst_unmapped_dqi_rows(
    repsum,
    dqi,
    vars_to_include = "variable_group"
  )
  expect_identical(nodes$abbreviation, c("metric_a", "metric_b"))
  expect_true(all(nodes$Dimension == "Consistency"))
})

test_that("plotly modebar helper removes only the requested button", {
  unchanged <- list(x = list(config = list()))
  expect_identical(
    util_plotly_remove_modebar_button_added(unchanged, "remove"),
    unchanged
  )

  widget <- list(x = list(config = list(
    modeBarButtonsToAdd = list(
      list(name = "remove"),
      list(name = "keep"),
      "remove",
      "keep",
      c("unknown", "shape"),
      list(icon = "unknown")
    )
  )))
  filtered <- util_plotly_remove_modebar_button_added(widget, "remove")
  buttons <- filtered$x$config$modeBarButtonsToAdd

  expect_length(buttons, 4L)
  expect_identical(buttons[[1]]$name, "keep")
  expect_identical(buttons[[2]], "keep")
  expect_identical(buttons[[3]], c("unknown", "shape"))
  expect_identical(buttons[[4]]$icon, "unknown")
})
