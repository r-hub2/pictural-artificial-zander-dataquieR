# nolint start: line_length_linter.
#' Display a sunburst chart
#'
#' @param repsum [data.frame] a report summary
#' @param remove_lines display lines?
#' @param ex exponent to steepen color gradient
#' @param folder_of_report a named vector with the location of variable and
#'                         `call_names`
#' @param var_uniquenames a data frame with the original variable names and the
#'                        unique names in case of reports created with dq_report_by
#'                        containing the same variable in several reports
#'                        (e.g., creation of reports by sex)
#'
#' @returns a `plotly` object
#' @noRd
# nolint end
util_render_sunburst_from_summary_classes <- function(repsum,
  remove_lines,
  ex = 5,
  vars_to_include = "study",
  folder_of_report = NULL,
  var_uniquenames = NULL) {
  util_ensure_suggested(pkg = "plotly", goal = "generate sunburst plot")
  util_ensure_suggested(pkg = "htmlwidgets", goal = "generate sunburst plot")
  util_ensure_suggested(pkg = "rmarkdown", goal = "generate sunburst plot")

  # NSE variables (for R CMD check): avoid "no visible binding" notes
  Dimension <- Domain <- Name <- Parent_Element_ID <- IndicatorID <-
    abbreviation <- Level <- Indicator <- class <- max.class <-
    severity_num <- mean_class_within_name <- mean_class_within_dom <-
    mean_class_within_dim <- color <- id <- parent <- path <- L1 <-
    L2 <- L3 <- L4 <- severity_mix <- n <- font_size <- id_match <-
    children <- child_vals <- n_leaves_dim <- n_leaves_dom <-
    href <- popup_href <- title <- n_leaves_name <- label_display <-
    label_full <- title_full <- NULL


  this <- util_attr(repsum, "this", exact = TRUE)

  if (missing(remove_lines)) {
    remove_lines <- length(unique(this$colnames_of_report)) *
      length(unique(this$rownames_of_report)) > 1000
  }

  repsum2 <- util_dashboard_table(
    repsum,
    folder_of_report = folder_of_report,
    vars_to_include = vars_to_include
  )
  is_by <- !is.null(folder_of_report)

  sunburst_meta_data <- this$summary_meta_data
  if (is.null(sunburst_meta_data)) sunburst_meta_data <- this$meta_data
  if (identical(vars_to_include, "variable_group")) {
    fallback_labels <- if (this$label_col %in% names(repsum2)) {
      repsum2[[this$label_col]]
    } else {
      repsum2[[VAR_NAMES]]
    }
    repsum2$label_full <- util_sunburst_variable_group_labels(
      var_names = repsum2[[VAR_NAMES]],
      meta_data_cross_item = this$meta_data_cross_item,
      fallback = fallback_labels
    )
  } else {
    repsum2$label_full <-
      prep_get_labels(repsum2[[VAR_NAMES]],
        item_level = sunburst_meta_data,
        label_col = this$label_col, max_len = .Machine$integer.max,
        label_class = "LONG",
        resp_vars_are_var_names_only = TRUE, resp_vars_match_label_col_only = FALSE # nolint: line_length_linter.
      )
  }
  if (!identical(vars_to_include, "variable_group") &&
      this$label_col %in% names(repsum2)) {
    resolved_labels <- as.character(repsum2[[this$label_col]])
    repsum2$label_full[!util_empty(resolved_labels)] <-
      resolved_labels[!util_empty(resolved_labels)]
  }

  dqi <- util_get_concept_info("dqi")

  dqi$Indicator <- dqi$Name
  dqi$Indicator[dqi$Level != 3] <- NA

  cols_dqi <- c("Dimension", "Domain", "Name", "Parent_Element_ID", "IndicatorID", "abbreviation", "Level", "Indicator") # nolint: line_length_linter.

  dqi <-
    dqi %>%
    dplyr::select(dplyr::all_of(cols_dqi)) %>%
    dplyr::distinct(dplyr::across(dplyr::all_of(cols_dqi)), .keep_all = FALSE)


  repsum2$indicator_metric <- sub("^[^_]+_", "", repsum2$indicator_metric)

  # Keep report results that are not represented in the current DQ_OBS export
  # visible. They belong in the chart, but must not be presented as mapped.
  dqi <- dplyr::bind_rows(
    dqi,
    util_sunburst_unmapped_dqi_rows(repsum2, dqi,
      vars_to_include = vars_to_include
    )
  )

  df <- dplyr::left_join(dqi, repsum2, by = c("abbreviation" = "indicator_metric")) # nolint: line_length_linter.

  sunburst_df <-
    df %>%
    dplyr::select(
      "Parent_Element_ID",
      "IndicatorID",
      "label_full",
      "Dimension",
      "Domain",
      "Name",
      "class",
      "Level",
      "href",
      "popup_href",
      "title"
    )

  dqi_nodes <-
    dqi %>%
    dplyr::filter(get("Level") %in% 1:3) %>%
    dplyr::select(
      "Dimension",
      "Domain",
      "Name",
      "Level"
    ) %>%
    dplyr::distinct() %>%
    dplyr::mutate(
      "label_full" = NA,
      "max.class" = NA
    )

  cols_sunburst_df <- c("Dimension", "Domain", "Name", "label_full", "Level", "href", "popup_href", "title") # nolint: line_length_linter.

  sunburst_df <-
    sunburst_df %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(cols_sunburst_df))) %>%
    dplyr::mutate("Level" = dplyr::case_when(
      !is.na(get("label_full")) ~ 4,
      TRUE ~ get("Level")
    )) %>%
    dplyr::summarize("max.class" = suppressWarnings(max(get("class"), na.rm = TRUE))) %>%
    dplyr::select("Dimension", "Domain", "Name", "label_full", "Level", "max.class", "href", "popup_href", "title") %>% # nolint: line_length_linter.
    dplyr::filter(get("Level") == 4)

  # Keep concept nodes independent from result rows. Some DQ_OBS parent nodes
  # have their own abbreviation; joining results to those rows must not replace
  # the parent node with an outer result leaf.
  sunburst_df <- dplyr::bind_rows(dqi_nodes, sunburst_df)

  # Define a severity → numeric scale
  sunburst_df$severity_num <- as.numeric(sunburst_df$max.class) # now, we have numbers according to the level order in the ordered class factor # nolint: line_length_linter.

  # Level 3 = Mean der Leaves
  sunburst_df <- sunburst_df %>%
    dplyr::group_by(Dimension, Domain, Name) %>%
    dplyr::mutate(
      mean_class_within_name = mean(severity_num[Level == 4], na.rm = TRUE),
      severity_num = ifelse(Level == 3, mean_class_within_name, severity_num)
    ) %>%
    dplyr::ungroup()

  # Level 2
  sunburst_df <- sunburst_df %>%
    dplyr::group_by(Dimension, Domain) %>%
    dplyr::mutate(
      mean_class_within_dom = mean(severity_num[Level == 4], na.rm = TRUE),
      severity_num = ifelse(Level == 2, mean_class_within_dom, severity_num)
    ) %>%
    dplyr::ungroup()

  # Level 1
  sunburst_df <- sunburst_df %>%
    dplyr::group_by(Dimension) %>%
    dplyr::mutate(
      mean_class_within_dim = mean(severity_num[Level == 4], na.rm = TRUE),
      severity_num = ifelse(Level == 1, mean_class_within_dim, severity_num)
    ) %>%
    dplyr::ungroup()

  sunburst_df$label_full <-
    vapply(sunburst_df$label_full, function(x) {
      if (is.null(x)) NA_character_ else as.character(x)
    }, FUN.VALUE = character(1))

  sunburst_df <-
    sunburst_df %>%
    dplyr::mutate(
      "label_full" = dplyr::case_when(
        get("Level") == 1 ~ get("Name"),
        get("Level") == 2 ~ get("Name"),
        get("Level") == 3 ~ get("Name"),
        TRUE ~ get("label_full")
      ),
      "title_full" = dplyr::case_when(
        get("Level") == 4 ~ paste(get("label_full"), get("Name"), sep = ": "),
        TRUE ~ get("title")
      )
    )

  sunburst_df$label_display <- util_sunburst_display_labels(
    sunburst_df$label_full
  )

  # Create an ID and parent relationship for sunburst
  sunburst_full <- sunburst_df %>%
    dplyr::filter(!is.na(get("severity_num"))) %>%
    dplyr::mutate(
      "id" = dplyr::case_when( # ids must be allowed anchor names in a URL
        get("Level") == 1 ~ get("Dimension"),
        get("Level") == 2 ~ paste(get("Dimension"), get("Domain"), sep = " - "),
        get("Level") == 3 ~ paste(get("Dimension"), get("Domain"), get("Name"), sep = " - "), # nolint: line_length_linter.
        get("Level") == 4 ~ paste(get("Dimension"), get("Domain"), get("Name"), get("label_full"), sep = " - ") # nolint: line_length_linter.
      ),
      "parent" = dplyr::case_when(
        get("Level") == 1 ~ "",
        get("Level") == 2 ~ get("Dimension"),
        get("Level") == 3 ~ paste(get("Dimension"), get("Domain"), sep = " - "),
        get("Level") == 4 ~ paste(get("Dimension"), get("Domain"), get("Name"), sep = " - ") # nolint: line_length_linter.
      )
    ) %>%
    # ---- leaf counts (descendant leaves); leaves are Level==4 ----
    dplyr::left_join(
      dplyr::filter(., get("Level") == 4) %>%
        dplyr::count(Dimension, name = "n_leaves_dim"),
      by = "Dimension"
    ) %>%
    dplyr::left_join(
      dplyr::filter(., get("Level") == 4) %>%
        dplyr::count(Dimension, Domain, name = "n_leaves_dom"),
      by = c("Dimension", "Domain")
    ) %>%
    dplyr::left_join(
      dplyr::filter(., get("Level") == 4) %>%
        dplyr::count(Dimension, Domain, Name, name = "n_leaves_name"),
      by = c("Dimension", "Domain", "Name")
    ) %>%
    dplyr::mutate(
      n_leaves = dplyr::case_when(
        get("Level") == 1 ~ as.integer(n_leaves_dim),
        get("Level") == 2 ~ as.integer(n_leaves_dom),
        get("Level") == 3 ~ as.integer(n_leaves_name),
        get("Level") == 4 ~ 1L,
        TRUE ~ NA_integer_
      )
    ) %>%
    dplyr::select(-n_leaves_dim, -n_leaves_dom, -n_leaves_name)

  line_cfg <- if (remove_lines) {
    list(width = 0, color = "rgba(0,0,0,0)")
  } else {
    list(width = 1, color = "white") # Standard: white seperator lines
  }

  min_font <- 12
  max_font <- 30

  sunburst_full <- sunburst_full %>%
    dplyr::mutate(
      # scale based on severity like in a word-cloud
      font_size = min_font + (severity_num - 1) / 4 * (max_font - min_font)
    )

  sunburst_full$font_size[is.na(sunburst_full$font_size)] <- min_font

  sunburst_full <- sunburst_full %>%
    dplyr::mutate(
      customdata = Map(
        function(href, popup_href, title, severity_num, label) {
          list(
            href = href,
            popup_href = popup_href,
            title = title,
            severity_num = severity_num,
            label = label
          )
        },
        href,
        popup_href,
        title_full,
        severity_num,
        label_full
      )
    )

  # ---------------------------
  # Plot
  # ---------------------------
  py <- plotly::plot_ly(
    data = sunburst_full,
    ids = ~id,
    labels = ~label_display,
    parents = ~parent,
    values = ~ severity_num * n_leaves,
    customdata = ~customdata,
    type = "sunburst",
    branchvalues = "total",
    textinfo = "label",
    textfont = list(size = ~font_size),
    hovertemplate = "%{customdata.label}<br>%{value}<extra></extra>",
    marker = list(
      colors = ~ 1 - ((1 - ((severity_num - 1) / 4))^ex),
      line = line_cfg,
      # Historical fixed color scales removed here. Inspect commits 851d1c1336
      # and 4b85879fbf before restoring custom Plotly scale experiments.
      colorscale = lapply(mapply(SIMPLIFY = FALSE, nm = 1.0 * (1:5 - 1) / 4, cl = util_get_colors(), list), unname), # nolint: line_length_linter.
      cmin = 0,
      cmax = 1,
      showscale = FALSE
    )
  )

  py <- htmlwidgets::onRender(
    py,
    "sunburst_on_render",
    data = list(ex_init = ex)
  )

  py$height <- "100%"
  py$width <- "100%"
  # Responsiveness is controlled by the surrounding report layout.

  py <- plotly::layout(py,
    autosize = TRUE
  )

  py <- plotly::config(py, displaylogo = FALSE, responsive = TRUE)

  py <- util_decorate_plotly(py)

  py <- util_plotly_add_modebar_buttons(
    py,
    list(
      list(
        name = "Back to root",
        icon = htmlwidgets::JS("Plotly.Icons.home"),
        click = htmlwidgets::JS("Plotly_dq_go_root")
      )
    )
  )

  py <- util_plotly_remove_modebar_button_added(py, "Restore initial Size")

  py <- util_plotly_modebar_right(py, 50)

  py <- plotly::layout(py,
    margin = list(r = 40)
  )

  py$sizingPolicy$defaultHeight <- "100%"

  if (.called_in_pipeline || is_by) {
    dummy <- htmltools::tagList()
  } else {
    dummy <-
      htmltools::tagList(
        htmltools::div(class = "navbar"),
        htmltools::div(class = "content"),
        htmltools::tags$script(
          type = "text/javascript",
          "window.dq_report2 = true"
        )
      )
  }

  if (is_by) {
    py <- htmltools::tagList(
      dummy,
      htmltools::div(
        class = "dq-sunburst-container",
        `data-tippy-always-on` = "true",
        py
      )
    )
  } else {
    py <- htmltools::tagList(
      dummy,
      htmltools::div(
        class = "dq-sunburst-container",
        `data-tippy-always-on` = "true",
        py
      )
    )
  }


  if (!.called_in_pipeline) {
    py <- htmltools::tagList(
      rmarkdown::html_dependency_jquery(),
      html_dependency_clipboard(),
      html_dependency_tippy(),
      html_dependency_dataquieR(iframe = FALSE),
      py
    )
  }

  py <- htmltools::browsable(py)

  py
}

#' Sunburst helper for variable-group labels
#'
#' @noRd
util_sunburst_variable_group_labels <- function(var_names,
  meta_data_cross_item, fallback = var_names) {
  fallback <- as.character(fallback)
  if (!is.data.frame(meta_data_cross_item) ||
      !all(c(CHECK_ID, CHECK_LABEL) %in% colnames(meta_data_cross_item))) {
    return(fallback)
  }
  labels <- as.character(meta_data_cross_item[[CHECK_LABEL]])[
    match(
      as.character(var_names),
      as.character(meta_data_cross_item[[CHECK_ID]])
    )
  ]
  missing_labels <- is.na(labels) | util_empty(labels)
  labels[missing_labels] <- fallback[missing_labels]
  labels
}

#' Sunburst helper for unmapped DQI rows
#'
#' @noRd
util_sunburst_unmapped_dqi_rows <- function(repsum, dqi,
  vars_to_include = "study") {
  mapped_metrics <- unique(stats::na.omit(as.character(dqi$abbreviation)))
  repsum <- repsum[
    !is.na(repsum$indicator_metric) &
      !(repsum$indicator_metric %in% mapped_metrics),
    ,
    drop = FALSE
  ]
  if (nrow(repsum) == 0) {
    return(dqi[0, , drop = FALSE])
  }

  if (identical(vars_to_include, "variable_group")) {
    return(util_sunburst_provisional_variable_group_dqi_rows(repsum, dqi))
  }

  call_names <- if ("call_names" %in% names(repsum)) {
    as.character(repsum$call_names)
  } else {
    rep(NA_character_, nrow(repsum))
  }
  domains <- vapply(call_names, function(call_name) {
    if (util_empty(call_name)) {
      return("Unmapped implementation")
    }
    util_alias2caption(call_name, long = TRUE)
  }, FUN.VALUE = character(1))
  metric_names <- vapply(repsum$indicator_metric, function(metric) {
    metric <- switch(metric,
      "max_cor" = "Maximum correlation",
      "in_range" = "Within requested range",
      metric
    )
    gsub("_", " ", metric, fixed = TRUE)
  }, FUN.VALUE = character(1))

  root_name <- "No DQ_OBS mapping"
  node_rows <- function(dimension, domain, name, indicator_id, abbreviation,
    level) {
    nodes <- dqi[rep(NA_integer_, length(name)), , drop = FALSE]
    nodes$Dimension <- dimension
    nodes$Domain <- domain
    nodes$Name <- name
    nodes$Parent_Element_ID <- NA_character_
    nodes$IndicatorID <- indicator_id
    nodes$abbreviation <- abbreviation
    nodes$Level <- as.integer(level)
    nodes
  }

  domains <- unname(unique(domains))
  names_by_domain <- unique(data.frame(
    domain = unname(vapply(call_names, function(call_name) {
      if (util_empty(call_name)) {
        return("Unmapped implementation")
      }
      util_alias2caption(call_name, long = TRUE)
    }, FUN.VALUE = character(1))),
    name = unname(metric_names),
    abbreviation = unname(as.character(repsum$indicator_metric)),
    stringsAsFactors = FALSE,
    row.names = NULL
  ))
  dplyr::bind_rows(
    node_rows(root_name, NA_character_, root_name,
      "DQ_UNMAPPED", NA_character_, 1L),
    node_rows(root_name, domains, domains,
      paste0("DQ_UNMAPPED_DOMAIN_", seq_along(domains)), NA_character_, 2L),
    node_rows(root_name, names_by_domain$domain, names_by_domain$name,
      paste0("DQ_UNMAPPED_INDICATOR_", seq_len(nrow(names_by_domain))),
      names_by_domain$abbreviation, 3L)
  )
}

#' Sunburst helper for provisional variable-group DQI rows
#'
#' @noRd
util_sunburst_provisional_variable_group_dqi_rows <- function(repsum, dqi) {
  metrics <- unique(as.character(repsum$indicator_metric))
  metrics <- metrics[!is.na(metrics) & !util_empty(metrics)]
  if (!length(metrics)) {
    return(dqi[0, , drop = FALSE])
  }

  nodes <- dqi[rep(NA_integer_, length(metrics)), , drop = FALSE]
  nodes$Dimension <- "Consistency"
  nodes$Domain <- "Range and value violations"
  nodes$Name <- "Provisional variable-group mappings"
  nodes$Parent_Element_ID <- NA_character_
  nodes$IndicatorID <- paste0("DQ_PROVISIONAL_GROUP_", seq_along(metrics))
  nodes$abbreviation <- metrics
  nodes$Level <- 3L
  nodes
}

#' Internal helper: sunburst display labels
#'
#' @noRd
util_sunburst_display_labels <- function(labels, line_width = 12,
  max_chars = 33, max_lines = 3) {
  label_names <- names(labels)
  labels <- as.character(labels)

  display_labels <- vapply(seq_along(labels), function(i) {
    label <- labels[[i]]
    if (is.na(label)) {
      return(label)
    }
    label <- gsub("[[:space:]]+", " ", trimws(label))
    if (!nzchar(label)) {
      return(label)
    }

    if (nchar(label, type = "chars") > max_chars) {
      label <- paste0(substr(label, 1, max_chars - 3), "...")
    }

    lines <- strwrap(label, width = line_width, simplify = FALSE)[[1]]
    lines <- unlist(lapply(lines, function(line) {
      starts <- seq.int(1L, nchar(line), by = line_width)
      substring(line, starts, starts + line_width - 1L)
    }), use.names = FALSE)
    if (length(lines) > max_lines) {
      lines <- lines[seq_len(max_lines)]
      lines[[max_lines]] <- paste0(
        substr(lines[[max_lines]], 1L, line_width - 3L),
        "..."
      )
    }
    paste(lines, collapse = "<br>")
  }, FUN.VALUE = character(1))
  names(display_labels) <- label_names
  display_labels
}

# nolint start: line_length_linter.
#' Force `Plotly` `modebar` to the top-right (`htmlwidgets`)
#'
#' Some CSS setups or late resizes can make `Plotly` place the `modebar` (buttons)
#' on the left. This helper pins it to the right via a widget-local JS hook.
#'
#' @param p A `plotly` `htmlwidget`.
#' @param right_px Inset from the right edge (CSS px).
# nolint end

#' @return The modified widget.
#' @noRd
util_plotly_modebar_right <- function(p, right_px = 16) {
  util_stop_if_not(
    "`right_px` must be one finite number" =
      is.numeric(right_px) && length(right_px) == 1L && is.finite(right_px)
  )

  htmlwidgets::onRender(
    p,
    sprintf("
function(el, x) {
  function pin() {
    var mbs = el.querySelectorAll('.modebar, .modebar-container');
    for (var i = 0; i < mbs.length; i++) {
      mbs[i].style.left  = 'auto';
      mbs[i].style.right = '%spx';
    }
  }

  pin();

  if (typeof Plotly !== 'undefined' && el && typeof el.on === 'function') {
    el.on('plotly_afterplot', pin);
    el.on('plotly_relayout',  pin);
  }

  requestAnimationFrame(pin);
  setTimeout(pin, 0);
  setTimeout(pin, 50);
}
", as.integer(round(right_px)))
  )
}

#' Internal helper: plotly remove modebar button added
#'
#' @noRd
util_plotly_remove_modebar_button_added <- function(p, name) {
  cfg <- p$x$config
  mba <- cfg$modeBarButtonsToAdd
  if (is.null(mba) || !length(mba)) {
    return(p)
  }

  cfg$modeBarButtonsToAdd <- Filter(
    function(btn) {
      # keep unknown shapes by default
      if (is.list(btn)) {
        # custom buttons are lists with $name
        return(is.null(btn$name) || !identical(btn$name, name))
      }
      # atomic entries (sometimes characters) -> drop only if equal
      if (is.atomic(btn) && length(btn) == 1L) {
        return(!identical(as.character(btn), name))
      }
      TRUE
    },
    mba
  )

  p$x$config <- cfg
  p
}
# Nur AGE_0 wird organge?
