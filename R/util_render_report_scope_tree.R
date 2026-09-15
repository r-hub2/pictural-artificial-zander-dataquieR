#' Render a compact tree for report-scope summaries
#'
#' @param info_dim_dq item-level scope summary table
#' @param info_scale_dq variable-group scope summary table
#' @param report a report
#' @param repsum [data.frame] optional precomputed [summary()] of `report`
#' @param item_coverage [data.frame] optional precomputed item-level coverage
#'   from `util_report_scope_item_coverage()`.
#' @param item_possible_indicator_ids [character] optional DQ_OBS indicator IDs
#'   that are possible across reports represented by `item_coverage`.
#'
#' @return an [htmltools::tag()]
#'
#' @noRd
util_render_report_scope_tree <- function(
  info_dim_dq,
  info_scale_dq,
  report = NULL,
  repsum = NULL,
  item_coverage = NULL,
  item_possible_indicator_ids = NULL
) {
  trees <- util_report_scope_tree_nodes(info_dim_dq, info_scale_dq,
    report = report, repsum = repsum, item_coverage = item_coverage,
    item_possible_indicator_ids = item_possible_indicator_ids
  )
  trees <- Filter(Negate(is.null), lapply(
    trees,
    util_report_scope_tree_prune_unavailable
  ))
  if (!length(trees)) {
    return(NULL)
  }

  htmltools::div(
    class = "dq-report-scope-tree-grid",
    lapply(trees, util_render_report_scope_tree_panel)
  )
}

#' Render one report-scope tree panel
#'
#' @noRd
util_render_report_scope_tree_panel <- function(node) {
  tree <- util_render_report_scope_tree_table(node)
  htmltools::div(
    class = "dq-report-scope-tree-panel",
    htmltools::tags$h3(node[["label"]]),
    tree
  )
}

#' Remove unavailable nodes from a report-scope tree
#'
#' @noRd
util_report_scope_tree_prune_unavailable <- function(node) {
  node[["children"]] <- Filter(Negate(is.null), lapply(
    node[["children"]],
    util_report_scope_tree_prune_unavailable
  ))
  coverage <- node[["coverage"]]
  if (is.null(coverage) || is.na(coverage[["possible"]]) ||
      coverage[["possible"]] < 1L) {
    return(NULL)
  }
  node
}

#' Render one report-scope tree table
#'
#' @noRd
util_render_report_scope_tree_table <- function(node) {
  item_level <- startsWith(node[["label"]], "Item-level")
  panel_id <- paste0(
    "dq_report_scope_tree_table_",
    if (item_level) "items" else "variable_groups"
  )
  measure_label <- "Data-quality indicators"
  assessed_label <- if (item_level) {
    "Assessed items"
  } else {
    "Assessed groups"
  }
  computed_label <- "Computed"
  possible_label <- "Expected results"
  coverage_label <- "Result coverage"
  assessed_unit <- if (item_level) {
    "item"
  } else {
    "variable-group"
  }
  coverage_help <- paste(
    sprintf(
      paste(
        "Counts results expected for each requested and applicable %s and",
        "data-quality indicator combination."
      ),
      assessed_unit
    ),
    paste(
      "The denominator is the number of expected results. Checks that are not",
      "applicable by definition are excluded; requested checks that could not",
      "be computed because required information is missing remain included."
    ),
    paste(
      "The main percentage is computed results divided by expected results;",
      "the smaller percentage is classified results divided by expected",
      "results. A computed result may remain unclassified when information",
      "required for grading is unavailable. Select a value for details."
    )
  )
  concept_label <- "DQ_OBS concept coverage"
  concept_help <- paste(
    paste(
      "The requested percentage is the number of distinct reference concepts",
      "requested divided by the total number of applicable reference concepts."
    ),
    paste(
      "Concepts that are not applicable by definition are excluded from this",
      "denominator."
    ),
    paste(
      "The computed and classified percentages use the number of requested",
      "concepts as their denominator. Their numerators count concepts with at",
      "least one computed result and concepts that were also classified,",
      "respectively. Each concept is counted once; unrequested concepts are",
      "excluded from these two percentages. Select a value for details."
    )
  )
  open_depth <- 1L
  table <- htmltools::tags$table(
    id = panel_id,
    class = "dq-report-scope-tree-table",
    role = "treegrid",
    htmltools::tags$thead(htmltools::tags$tr(
      htmltools::tags$th("Assessment scope"),
      htmltools::tags$th(measure_label),
      htmltools::tags$th(assessed_label),
      htmltools::tags$th(computed_label),
      htmltools::tags$th(possible_label),
      htmltools::tags$th(
        coverage_label,
        htmltools::tags$button(
          type = "button",
          class = "dq-report-scope-header-help",
          title = coverage_help,
          `aria-label` = paste("About", coverage_label),
          "\u24d8"
        )
      ),
      htmltools::tags$th(
        concept_label,
        htmltools::tags$button(
          type = "button",
          class = "dq-report-scope-header-help",
          title = concept_help,
          `aria-label` = paste("About", concept_label),
          "\u24d8"
        )
      )
    )),
    htmltools::tags$tbody(util_report_scope_tree_table_rows(
      node,
      panel_id = panel_id,
      open_depth = open_depth
    ))
  )
  table
}

#' Render rows of a report-scope tree table
#'
#' @noRd
util_report_scope_tree_table_rows <- function(
  node,
  panel_id,
  parent_id = NULL,
  depth = 0L,
  visible = TRUE,
  open_depth,
  node_path = "1"
) {
  node_id <- paste0(panel_id, "_", node_path)
  children <- node[["children"]]
  has_children <- length(children) > 0L
  expanded <- has_children && depth < open_depth
  row <- htmltools::tags$tr(
    `data-node-id` = node_id,
    `data-parent-id` = parent_id,
    `aria-level` = depth + 1L,
    style = if (!visible) "display:none;" else NULL,
    util_report_scope_tree_table_name_cell(
      node,
      node_id = node_id,
      depth = depth,
      expanded = expanded
    ),
    util_report_scope_tree_table_number_cell(node[["measures"]]),
    util_report_scope_tree_table_number_cell(node[["assessed"]]),
    util_report_scope_tree_table_number_cell(
      node[["coverage"]][["computed"]] %||%
        node[["coverage"]][["classifications"]]
    ),
    util_report_scope_tree_table_number_cell(node[["coverage"]][["possible"]]),
    util_report_scope_tree_table_coverage_cell(
      node[["coverage"]],
      node[["classification_items"]]
    ),
    util_report_scope_tree_table_concept_coverage_cell(
      node[["concept_coverage"]],
      node[["concept_items"]]
    )
  )
  child_rows <- unlist(Map(
    f = function(child, child_index) {
      util_report_scope_tree_table_rows(
        child,
        panel_id = panel_id,
        parent_id = node_id,
        depth = depth + 1L,
        visible = visible && expanded,
        open_depth = open_depth,
        node_path = paste(node_path, child_index, sep = "_")
      )
    },
    child = children,
    child_index = seq_along(children)
  ), recursive = FALSE)
  c(list(row), child_rows)
}

#' Render a name cell in a report-scope tree table
#'
#' @noRd
util_report_scope_tree_table_name_cell <- function(
  node,
  node_id,
  depth,
  expanded
) {
  style <- util_report_scope_tree_coverage_style(node[["coverage"]])
  title <- if (identical(
    node[["classification_items"]][["row_label"]],
    "Variable group"
  )) {
    util_report_scope_tree_group_coverage_label(node[["coverage"]])
  } else {
    style[["title"]]
  }
  toggle <- if (length(node[["children"]])) {
    htmltools::tags$button(
      type = "button",
      class = "dq-report-scope-tree-toggle",
      `data-node-id` = node_id,
      `aria-expanded` = tolower(as.character(expanded)),
      htmltools::HTML(if (expanded) "&#9662;" else "&#9656;")
    )
  } else {
    htmltools::tags$span(class = "dq-report-scope-tree-toggle-spacer")
  }
  htmltools::tags$td(
    style = htmltools::css(
      padding_left = paste0(0.9 * depth, "em")
    ),
    toggle,
    htmltools::tags$span(
      class = "dq-report-scope-tree-node",
      title = title,
      node[["label"]]
    )
  )
}

#' Render a numeric cell in a report-scope tree table
#'
#' @noRd
util_report_scope_tree_table_number_cell <- function(value) {
  htmltools::tags$td(ifelse(is.na(value), "", as.character(value)))
}

#' Render a classification-coverage cell
#'
#' @noRd
util_report_scope_tree_table_coverage_cell <- function(
  coverage,
  classification_items = NULL
) {
  style <- util_report_scope_tree_coverage_style(coverage)
  possible <- coverage[["possible"]]
  value <- if (is.na(possible) || possible < 1L) {
    "N/A"
  } else {
    computed <- coverage[["computed"]] %||% coverage[["classifications"]]
    classified <- coverage[["classifications"]]
    htmltools::tagList(
      paste0(round(100 * computed / possible), "%"),
      htmltools::tags$small(
        class = "dq-report-scope-tree-coverage-secondary",
        paste0(round(100 * classified / possible), "% classified")
      )
    )
  }
  htmltools::tags$td(
    class = "dq-report-scope-tree-coverage",
    style = htmltools::css(
      background = style[["background"]],
      color = style[["color"]]
    ),
    title = util_report_scope_tree_coverage_tooltip(
      coverage,
      classification_items
    ),
    value
  )
}

#' Render a concept-coverage cell
#'
#' @noRd
util_report_scope_tree_table_concept_coverage_cell <- function(
  coverage,
  concept_items = NULL
) {
  if (is.null(coverage[["requested"]]) &&
      !is.null(concept_items[["requested"]])) {
    coverage[["requested"]] <- length(concept_items[["requested"]])
  }
  style <- util_report_scope_tree_concept_coverage_style(coverage)
  possible <- coverage[["possible"]]
  value <- if (is.na(possible) || possible < 1L) {
    "N/A"
  } else {
    computed <- coverage[["computed"]] %||% coverage[["assessed"]]
    classified <- coverage[["assessed"]]
    requested <- coverage[["requested"]] %||% computed
    completed <- if (requested > 0L) {
      round(100 * computed / requested)
    } else {
      100
    }
    classified_completed <- if (requested > 0L) {
      round(100 * classified / requested)
    } else {
      100
    }
    htmltools::tagList(
      paste0(round(100 * requested / possible), "% requested"),
      htmltools::tags$small(
        class = "dq-report-scope-tree-coverage-secondary",
        sprintf(
          "%s%% computed (%s%% classified)",
          completed,
          classified_completed
        )
      )
    )
  }
  htmltools::tags$td(
    class = "dq-report-scope-tree-coverage",
    style = htmltools::css(
      background = style[["background"]],
      color = style[["color"]]
    ),
    title = util_report_scope_tree_concept_coverage_tooltip(
      coverage,
      concept_items
    ),
    value
  )
}

#' Build item- and variable-group report-scope trees
#'
#' @noRd
util_report_scope_tree_nodes <- function(
  info_dim_dq,
  info_scale_dq,
  report = NULL,
  repsum = NULL,
  item_coverage = NULL,
  item_possible_indicator_ids = NULL
) {
  list(
    util_report_scope_item_tree_nodes(
      info_dim_dq,
      report,
      repsum,
      item_coverage = item_coverage,
      item_possible_indicator_ids = item_possible_indicator_ids
    ),
    util_report_scope_variable_group_tree_nodes(info_scale_dq, report, repsum)
  )
}

#' Build the item-level report-scope tree
#'
#' @noRd
util_report_scope_item_tree_nodes <- function(
  info_dim_dq,
  report = NULL,
  repsum = NULL,
  item_coverage = NULL,
  item_possible_indicator_ids = NULL
) {
  if (!is.data.frame(info_dim_dq) ||
      !all(c("Dimension", "No. DQ indicators") %in% colnames(info_dim_dq))) {
    return(list(text = "Item-level data quality assessment", children = list()))
  }
  if (sum(info_dim_dq[["No. DQ indicators"]], na.rm = TRUE) < 1L) {
    return(list(text = "Item-level assessment scope", children = list()))
  }

  if (is.null(item_coverage)) {
    item_coverage <- util_report_scope_item_coverage(report, repsum)
  }
  if (!"computations" %in% colnames(item_coverage)) {
    item_coverage[["computations"]] <- item_coverage[["classifications"]]
  }
  coverage_concepts <- util_report_scope_coverage_concept_ids(item_coverage)
  concept_ids_for <- function(keep) {
    unique(unlist(coverage_concepts[keep], use.names = FALSE))
  }
  represented_indicator_ids <- concept_ids_for(rep(
    TRUE,
    length(coverage_concepts)
  ))
  possible_indicator_ids <- item_possible_indicator_ids
  if (is.null(possible_indicator_ids)) {
    possible_indicator_ids <- util_report_scope_target_indicator_ids(
      "item",
      report = report
    )
  }
  possible_indicator_ids <- union(
    possible_indicator_ids,
    represented_indicator_ids
  )
  hierarchy <- util_report_scope_dqi_hierarchy(represented_indicator_ids)
  children <- lapply(hierarchy, util_report_scope_item_hierarchy_node,
    coverage_rows = item_coverage,
    report = report,
    possible_indicator_ids = possible_indicator_ids
  )

  coverage <- util_report_scope_tree_coverage(
    sum(item_coverage$classifications),
    sum(item_coverage$possible_classifications),
    sum(item_coverage$computations)
  )
  concept_items <- util_report_scope_dqi_concepts(
    possible_indicator_ids,
    target_entity = "item",
    assessed_ids = concept_ids_for(item_coverage$classifications > 0L),
    computed_ids = concept_ids_for(item_coverage$computations > 0L),
    requested_ids = concept_ids_for(item_coverage$applicable),
    report = report,
    possible_ids = possible_indicator_ids
  )
  if ("indicator_metric" %in% colnames(item_coverage)) {
    concept_items[["joint_assessments"]] <-
      util_report_scope_joint_assessment_labels(
        item_coverage$indicator_metric
      )
  }
  concept_coverage <- util_report_scope_tree_concept_coverage(
    length(concept_items[["assessed"]]),
    length(concept_items[["possible"]]),
    length(concept_items[["computed"]]),
    length(concept_items[["requested"]])
  )
  classification_items <- util_report_scope_tree_merge_classification_items(
    lapply(children, `[[`, "classification_items")
  )
  util_report_scope_tree_node(
    sprintf(
      "Item-level assessment scope: %s; %s; %s",
      util_count_label(length(unique(item_coverage$indicator_id)),
        "data-quality indicator"
      ),
      util_count_label(length(unique(item_coverage$variable)), "assessed item"),
      util_report_scope_tree_coverage_label(coverage)
    ),
    coverage = coverage,
    classification_items = classification_items,
    concept_coverage = concept_coverage,
    concept_items = concept_items,
    label = "Item-level assessment scope",
    measures = length(unique(item_coverage$indicator_id)),
    assessed = length(unique(item_coverage$variable)),
    children = children
  )
}

#' Build the represented part of the DQ_OBS indicator hierarchy
#'
#' @param indicator_ids Level-three DQ_OBS indicator identifiers to display.
#'
#' @return A nested list following `Parent_Element_ID`.
#'
#' @noRd
util_report_scope_dqi_hierarchy <- function(indicator_ids) {
  indicator_ids <- unique(as.character(indicator_ids))
  indicator_ids <- indicator_ids[!is.na(indicator_ids) & nzchar(indicator_ids)]
  if (!length(indicator_ids)) {
    return(list())
  }
  dqi <- util_get_concept_info("dqi")
  columns <- c(
    "Level", "IndicatorID", "Parent_Element_ID", "Name", "public_name",
    "order_nr"
  )
  dqi <- unique(dqi[, columns, drop = FALSE])
  dqi <- dqi[!duplicated(dqi[["IndicatorID"]]), , drop = FALSE]
  rownames(dqi) <- dqi[["IndicatorID"]]
  indicator_ids <- intersect(indicator_ids, dqi[["IndicatorID"]])
  selected_ids <- indicator_ids
  parent_ids <- dqi[indicator_ids, "Parent_Element_ID", drop = TRUE]
  while (length(parent_ids)) {
    parent_ids <- unique(parent_ids[
      !is.na(parent_ids) & parent_ids != "root" &
        parent_ids %in% dqi[["IndicatorID"]]
    ])
    new_ids <- setdiff(parent_ids, selected_ids)
    if (!length(new_ids)) {
      break
    }
    selected_ids <- c(selected_ids, new_ids)
    parent_ids <- dqi[new_ids, "Parent_Element_ID", drop = TRUE]
  }

  label_for <- function(id) {
    label <- dqi[id, "public_name", drop = TRUE]
    if (is.na(label) ||
        util_empty(label) ||
        trimws(label) %in% c(".", "-", "_")) {
      label <- dqi[id, "Name", drop = TRUE]
    }
    label
  }
  descendants_for <- function(id) {
    children <- dqi[["IndicatorID"]][
      !is.na(dqi[["Parent_Element_ID"]]) &
        dqi[["Parent_Element_ID"]] == id
    ]
    descendants <- c(
      id,
      unlist(lapply(children, descendants_for), use.names = FALSE)
    )
    descendants[dqi[descendants, "Level", drop = TRUE] == 3L]
  }
  build <- function(id) {
    children <- dqi[["IndicatorID"]][
      !is.na(dqi[["Parent_Element_ID"]]) &
        dqi[["Parent_Element_ID"]] == id &
        dqi[["IndicatorID"]] %in% selected_ids
    ]
    children <- children[
      order(dqi[children, "order_nr", drop = TRUE], na.last = TRUE)
    ]
    list(
      indicator_id = id,
      indicator_ids = unique(descendants_for(id)),
      label = label_for(id),
      children = lapply(children, build)
    )
  }
  roots <- selected_ids[
    !dqi[selected_ids, "Parent_Element_ID", drop = TRUE] %in% selected_ids
  ]
  roots <- roots[order(dqi[roots, "order_nr", drop = TRUE], na.last = TRUE)]
  lapply(roots, build)
}

#' Build one item-assessment node from the DQ_OBS hierarchy
#'
#' @param spec One hierarchy specification.
#' @param coverage_rows Item-level classification rows.
#' @param report A result set, or `NULL`.
#'
#' @return A report-scope tree node.
#'
#' @noRd
util_report_scope_item_hierarchy_node <- function(
  spec,
  coverage_rows,
  report = NULL,
  possible_indicator_ids = NULL
) {
  coverage_concepts <- util_report_scope_coverage_concept_ids(coverage_rows)
  in_scope <- vapply(coverage_concepts, function(ids) {
    any(ids %in% spec[["indicator_ids"]])
  }, logical(1))
  rows <- coverage_rows[in_scope, , drop = FALSE]
  row_concepts <- coverage_concepts[in_scope]
  concept_ids_for <- function(keep) {
    unique(unlist(row_concepts[keep], use.names = FALSE))
  }
  coverage <- util_report_scope_tree_coverage(
    sum(rows$classifications),
    sum(rows$possible_classifications),
    sum(rows$computations)
  )
  concept_items <- util_report_scope_dqi_concepts(
    spec[["indicator_ids"]],
    target_entity = "item",
    assessed_ids = concept_ids_for(rows$classifications > 0L),
    computed_ids = concept_ids_for(rows$computations > 0L),
    requested_ids = concept_ids_for(rows$applicable),
    report = report,
    possible_ids = possible_indicator_ids
  )
  if ("indicator_metric" %in% colnames(rows)) {
    concept_items[["joint_assessments"]] <-
      util_report_scope_joint_assessment_labels(rows$indicator_metric)
  }
  util_report_scope_tree_node(
    sprintf(
      "%s: %s; %s; %s",
      spec[["label"]],
      util_count_label(length(unique(rows$indicator_id)),
        "data-quality indicator"
      ),
      util_count_label(length(unique(rows$variable)), "assessed item"),
      util_report_scope_tree_coverage_label(coverage)
    ),
    coverage = coverage,
    classification_items = util_report_scope_item_classification_items(
      rows,
      report = report
    ),
    concept_coverage = util_report_scope_tree_concept_coverage(
      length(concept_items[["assessed"]]),
      length(concept_items[["possible"]]),
      length(concept_items[["computed"]]),
      length(concept_items[["requested"]])
    ),
    concept_items = concept_items,
    label = spec[["label"]],
    measures = length(unique(rows$indicator_id)),
    assessed = length(unique(rows$variable)),
    children = lapply(spec[["children"]],
      util_report_scope_item_hierarchy_node,
      coverage_rows = coverage_rows,
      report = report,
      possible_indicator_ids = possible_indicator_ids
    )
  )
}

#' Summarize DQ_OBS concepts in one hierarchy branch
#'
#' @param indicator_ids Indicator identifiers below the branch.
#' @param target_entity Result entity used for the possible-concept denominator.
#' @param assessed_ids Indicator identifiers represented by results.
#' @param report A result set, or `NULL`.
#'
#' @return A list with possible and assessed concept labels.
#'
#' @noRd
util_report_scope_dqi_concepts <- function(
  indicator_ids,
  target_entity,
  assessed_ids = character(),
  computed_ids = assessed_ids,
  requested_ids = indicator_ids,
  report = NULL,
  possible_ids = NULL
) {
  dqi <- util_get_concept_info("dqi")
  if (is.null(possible_ids)) {
    possible_ids <- util_report_scope_target_indicator_ids(
      target_entity,
      report = report
    )
  }
  keep <- dqi[["Level"]] == 3L &
    dqi[["IndicatorID"]] %in% indicator_ids &
    dqi[["IndicatorID"]] %in% possible_ids
  dqi <- dqi[keep, , drop = FALSE]
  dqi <- dqi[!duplicated(dqi[["IndicatorID"]]), , drop = FALSE]
  labels <- dqi[["public_name"]]
  placeholder <- is.na(labels) | util_empty(labels) |
    trimws(labels) %in% c(".", "-", "_")
  labels[placeholder] <- dqi[["Name"]][placeholder]
  possible <- stats::setNames(labels, dqi[["IndicatorID"]])
  list(
    possible = possible,
    requested = possible[names(possible) %in% unique(requested_ids)],
    assessed = possible[names(possible) %in% unique(assessed_ids)],
    computed = possible[names(possible) %in% unique(computed_ids)]
  )
}

#' Build the variable-group report-scope tree
#'
#' @noRd
util_report_scope_variable_group_tree_nodes <- function(
  info_scale_dq,
  report = NULL,
  repsum = NULL
) {
  scale_columns <- c(
    "Requested variable-group metric",
    "Computed variable-group results"
  )
  if (!is.data.frame(info_scale_dq) ||
      !all(scale_columns %in% colnames(info_scale_dq))) {
    return(list(text = "Variable-group assessment scope", children = list()))
  }

  direct_results <- util_report_scope_variable_group_result_metrics(
    report,
    repsum
  )
  all_results <- direct_results
  categorical_groups <-
    util_report_scope_variable_group_categorical_groups(report)
  static_metrics <- util_report_scope_variable_group_dqi_metrics(
    info_scale_dq[["Requested variable-group metric"]]
  )
  direct_results <- direct_results[
    !(direct_results$abbreviation %in% static_metrics$abbreviation),
    , drop = FALSE
  ]
  categorical_metrics <- names(categorical_groups)
  categorical_metric_map <- util_report_scope_variable_group_dqi_metrics(
    categorical_metrics
  )
  categorical_parent_ids <- unique(
    categorical_metric_map$Parent_Element_ID[
      !is.na(categorical_metric_map$Parent_Element_ID)
    ]
  )
  direct_metric_map <- util_report_scope_variable_group_dqi_metrics(
    unique(direct_results$metric)
  )
  categorical_family_metrics <- direct_metric_map$metric[
    direct_metric_map$Parent_Element_ID %in% categorical_parent_ids
  ]
  active_direct_groups <- unique(direct_results$group[
    direct_results$computed | direct_results$classified
  ])
  requested_direct_metrics <- unique(c(
    direct_results$metric[direct_results$computed |
        direct_results$classified],
    direct_results$metric[
      direct_results$group %in% active_direct_groups &
        !(direct_results$metric %in% categorical_family_metrics)
    ],
    intersect(names(categorical_groups), direct_results$metric)
  ))
  direct_results <- direct_results[
    direct_results$metric %in% requested_direct_metrics,
    , drop = FALSE
  ]
  direct_info <- util_report_scope_variable_group_result_summary(
    direct_results
  )
  if (nrow(direct_info)) {
    info_scale_dq <- rbind(info_scale_dq, direct_info)
  }
  metric_map <- util_report_scope_variable_group_dqi_metrics(
    info_scale_dq[["Requested variable-group metric"]]
  )
  info_scale_dq <- info_scale_dq[!is.na(metric_map$IndicatorID), , drop = FALSE]
  metric_map <- metric_map[!is.na(metric_map$IndicatorID), , drop = FALSE]

  requested_groups <- util_report_scope_merge_group_lists(
    util_report_scope_variable_group_counts(report),
    categorical_groups
  )
  if (nrow(direct_results)) {
    direct_requested <- direct_results[
      !(direct_results$metric %in% categorical_metrics) |
        direct_results$computed | direct_results$classified,
      , drop = FALSE
    ]
    direct_requested <- split(
      direct_requested$group,
      direct_requested$metric
    )
    requested_groups <- util_report_scope_merge_group_lists(
      requested_groups,
      direct_requested
    )
  }
  computed_groups <- util_report_scope_variable_group_computed_groups(
    info_scale_dq,
    requested_groups,
    report = report
  )
  classified_groups <- list()
  if (nrow(all_results)) {
    result_metric <- metric_map$metric[match(
      all_results$abbreviation,
      metric_map$abbreviation
    )]
    keep <- !is.na(result_metric) & !util_empty(result_metric)
    all_results <- all_results[keep, , drop = FALSE]
    result_metric <- result_metric[keep]
    direct_computed <- split(
      all_results$group[all_results$computed],
      result_metric[all_results$computed]
    )
    direct_classified <- split(
      all_results$group[all_results$classified],
      result_metric[all_results$classified]
    )
    computed_groups <- util_report_scope_merge_group_lists(
      computed_groups,
      direct_computed
    )
    classified_groups <- util_report_scope_merge_group_lists(
      classified_groups,
      direct_classified
    )
  }
  info_scale_dq[["Computed variable-group results"]] <-
    util_report_scope_numeric_count(
      info_scale_dq[["Computed variable-group results"]]
    )
  hierarchy <- util_report_scope_variable_group_hierarchy_spec(metric_map)
  children <- util_report_scope_variable_group_hierarchy_nodes(
    info_scale_dq,
    requested_groups,
    computed_groups,
    classified_groups,
    hierarchy = hierarchy,
    report = report
  )

  summary <- util_report_scope_variable_group_summary(
    info_scale_dq,
    requested_groups,
    computed_groups,
    classified_groups,
    concept_scope = list(),
    report = report
  )
  util_report_scope_tree_node(
    sprintf(
      "Variable-group assessment scope: %s; %s; %s",
      util_count_label(nrow(info_scale_dq), "variable-group metric"),
      util_count_label(length(summary$groups), "group"),
      util_report_scope_tree_group_coverage_label(summary$coverage)
    ),
    coverage = summary$coverage,
    classification_items = summary$classification_items,
    concept_coverage = summary$concept_coverage,
    concept_items = summary$concept_items,
    label = "Variable-group assessment scope",
    measures = nrow(info_scale_dq),
    assessed = length(summary$groups),
    children = children
  )
}

#' Build variable-group hierarchy nodes from result metrics
#'
#' @noRd
util_report_scope_variable_group_hierarchy_nodes <- function(
  info_scale_dq,
  requested_groups,
  computed_groups,
  classified_groups,
  hierarchy = util_report_scope_variable_group_hierarchy_spec(),
  report = NULL
) {
  lapply(hierarchy, util_report_scope_variable_group_hierarchy_node,
    info_scale_dq = info_scale_dq,
    requested_groups = requested_groups,
    computed_groups = computed_groups,
    classified_groups = classified_groups,
    report = report
  )
}

#' Extract variable-group metrics from result objects
#'
#' @noRd
util_report_scope_variable_group_result_metrics <- function(
  report = NULL,
  repsum = NULL
) {
  empty <- data.frame(
    metric = character(),
    abbreviation = character(),
    indicator_id = character(),
    group = character(),
    computed = logical(),
    classified = logical(),
    stringsAsFactors = FALSE
  )
  if (is.null(report)) {
    return(empty)
  }
  if (is.null(repsum)) {
    return(empty)
  }
  this <- util_attr(repsum, "this", exact = TRUE)
  if (!is.environment(this) || !is.data.frame(this$result)) {
    return(empty)
  }
  meta_data <- this$summary_meta_data
  if (is.null(meta_data)) {
    meta_data <- this$meta_data
  }
  if (!is.data.frame(meta_data) || !VAR_NAMES %in% colnames(meta_data)) {
    return(empty)
  }
  results <- util_filter_repsum(
    this$result,
    vars_to_include = "variable_group",
    meta_data = meta_data,
    rownames_of_report = this$rownames_of_report,
    label_col = this$label_col,
    variable_group_call_names = this$variable_group_call_names,
    study_var_names = this$meta_data[[VAR_NAMES]]
  )
  if (!nrow(results) || !"indicator_metric" %in% colnames(results)) {
    return(empty)
  }
  keep <- !startsWith(results$indicator_metric, "CAT_") &
    !startsWith(results$indicator_metric, "MSG_") &
    !util_report_scope_variable_group_is_aggregate_metric(
      results$indicator_metric
    ) &
    !is.na(results$indicator_metric)
  results <- results[keep, , drop = FALSE]
  if (!nrow(results)) {
    return(empty)
  }

  abbreviations <- util_report_scope_normalize_indicator_metrics(
    results$indicator_metric
  )
  dqi <- util_get_concept_info("dqi")
  dqi <- dqi[dqi[["Level"]] == 3L &
      !is.na(dqi[["abbreviation"]]) &
      !util_empty(dqi[["abbreviation"]]),
    c("abbreviation", "Name", "IndicatorID"), drop = FALSE]
  dqi <- dqi[!duplicated(dqi[["abbreviation"]]), , drop = FALSE]
  match_index <- match(abbreviations, dqi[["abbreviation"]])
  keep <- !is.na(match_index)
  results <- results[keep, , drop = FALSE]
  abbreviations <- abbreviations[keep]
  match_index <- match_index[keep]
  if (!nrow(results)) {
    return(empty)
  }
  metric <- dqi[["Name"]][match_index]
  groups <- util_report_scope_variable_group_result_labels(
    report,
    results[[VAR_NAMES]]
  )
  data.frame(
    metric = metric,
    abbreviation = abbreviations,
    indicator_id = dqi[["IndicatorID"]][match_index],
    group = groups,
    computed = !util_summary_display_value_missing(results$values_raw),
    classified = if ("class" %in% colnames(results)) {
      !is.na(results$class)
    } else {
      rep(FALSE, nrow(results))
    },
    stringsAsFactors = FALSE
  )
}

#' Resolve variable-group labels from report metadata
#'
#' @noRd
util_report_scope_variable_group_result_labels <- function(report, var_names) {
  cross_item <- util_attr(report, "meta_data_cross_item", exact = TRUE)
  if (!is.data.frame(cross_item) ||
      !all(c(CHECK_ID, CHECK_LABEL) %in% colnames(cross_item))) {
    return(as.character(var_names))
  }
  check_ids <- as.character(cross_item[[CHECK_ID]])
  check_labels <- as.character(cross_item[[CHECK_LABEL]])
  result <- as.character(var_names)
  matched <- match(result, check_ids)
  use_label <- !is.na(matched) & !util_empty(check_labels[matched])
  result[use_label] <- check_labels[matched[use_label]]
  result
}

#' Summarize the grades of variable-group results
#'
#' @noRd
util_report_scope_variable_group_result_summary <- function(results) {
  if (!nrow(results)) {
    return(data.frame(
      "Requested variable-group metric" = character(),
      "Computed variable-group results" = integer(),
      check.names = FALSE
    ))
  }
  split_results <- split(results, results$metric)
  data.frame(
    "Requested variable-group metric" = names(split_results),
    "Computed variable-group results" = vapply(split_results, function(x) {
      length(unique(x$group[x$computed]))
    }, integer(1)),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

#' Build a DQI-backed hierarchy for variable-group metrics
#'
#' @noRd
util_report_scope_variable_group_hierarchy_spec <- function(
  metrics = character()) {
  metric_map <- if (is.data.frame(metrics)) {
    metrics
  } else {
    metrics <- unique(as.character(metrics))
    metrics <- metrics[!is.na(metrics) & !util_empty(metrics)]
    util_report_scope_variable_group_dqi_metrics(metrics)
  }
  metric_map <- metric_map[!is.na(metric_map$IndicatorID), , drop = FALSE]
  if (!nrow(metric_map)) {
    return(list())
  }
  hierarchy <- util_report_scope_dqi_hierarchy(metric_map$IndicatorID)
  add_metrics <- function(spec) {
    spec[["metrics"]] <- metric_map$metric[
      metric_map$IndicatorID == spec[["indicator_id"]]
    ]
    spec[["concept_scope"]] <- list(IndicatorID = spec[["indicator_ids"]])
    spec[["children"]] <- lapply(spec[["children"]], add_metrics)
    spec
  }
  lapply(hierarchy, add_metrics)
}

#' Map variable-group metrics to DQI identifiers
#'
#' @noRd
util_report_scope_variable_group_dqi_metrics <- function(metrics) {
  dqi <- util_get_concept_info("dqi")
  dqi <- dqi[dqi[["Level"]] == 3L &
      !is.na(dqi[["abbreviation"]]) &
      !util_empty(dqi[["abbreviation"]]),
    c(
      "Dimension", "Domain", "Name", "abbreviation", "IndicatorID",
      "Parent_Element_ID", "order_nr"
    ), drop = FALSE]
  dqi <- dqi[!duplicated(dqi[["abbreviation"]]), , drop = FALSE]

  ssi_info <- util_get_concept_info("ssi")
  roles <- ssi_info[["SSI_METRICS"]][match(metrics, ssi_info[["menu_label"]])]
  mapping <- util_get_concept_info("computed_vars_ind_mapping")
  abbreviations <- mapping[["replacement"]][match(
    roles,
    mapping[["computed_role"]]
  )]
  direct_metrics <- util_report_scope_normalize_indicator_metrics(metrics)
  abbreviations[is.na(abbreviations)] <- direct_metrics[is.na(abbreviations)]
  dqi_names <- dqi[["abbreviation"]][match(metrics, dqi[["Name"]])]
  abbreviations[!(abbreviations %in% dqi[["abbreviation"]])] <-
    dqi_names[!(abbreviations %in% dqi[["abbreviation"]])]

  match_index <- match(abbreviations, dqi[["abbreviation"]])
  data.frame(
    metric = metrics,
    abbreviation = abbreviations,
    Dimension = dqi[["Dimension"]][match_index],
    Domain = dqi[["Domain"]][match_index],
    Name = dqi[["Name"]][match_index],
    IndicatorID = dqi[["IndicatorID"]][match_index],
    Parent_Element_ID = dqi[["Parent_Element_ID"]][match_index],
    order_nr = dqi[["order_nr"]][match_index],
    stringsAsFactors = FALSE
  )
}

#' Normalize indicator-metric names for hierarchy matching
#'
#' @noRd
util_report_scope_normalize_indicator_metrics <- function(metrics) {
  sub(
    "^(NUM|PCT|ICC|TF|FLG|LOGICAL)_",
    "",
    as.character(metrics)
  )
}

#' Read declarative mappings for joint indicator assessments
#'
#' @return A data frame mapping one result metric to all DQ_OBS concepts jointly
#'   assessed by that result.
#'
#' @noRd
util_report_scope_joint_assessment_mapping <- function() {
  empty <- data.frame(
    indicator_metric = character(),
    indicator_id = character(),
    assessment_label = character(),
    stringsAsFactors = FALSE
  )
  path <- system.file(
    "report_scope_joint_assessments.csv",
    package = "dataquieR"
  )
  if (!nzchar(path) || !file.exists(path)) {
    return(empty)
  }
  mapping <- utils::read.csv(path, stringsAsFactors = FALSE)
  required <- colnames(empty)
  util_stop_if_not(
    "Joint-assessment mappings must have the expected columns" =
      identical(colnames(mapping), required),
    "Joint-assessment metrics and indicator IDs must not be empty" =
      !any(util_empty(mapping$indicator_metric)) &&
      !any(util_empty(mapping$indicator_id))
  )
  mapping$indicator_metric <-
    util_report_scope_normalize_indicator_metrics(mapping$indicator_metric)
  mapping
}

#' Resolve all DQ_OBS concepts represented by result metrics
#'
#' @param metrics Indicator-metric column names or normalized abbreviations.
#'
#' @return A list of character vectors aligned with `metrics`.
#'
#' @noRd
util_report_scope_metric_indicator_ids <- function(metrics) {
  abbreviations <- util_report_scope_normalize_indicator_metrics(metrics)
  dqi <- util_get_concept_info("dqi")
  dqi <- dqi[
    dqi[["Level"]] == 3L &
      !is.na(dqi[["abbreviation"]]) &
      !util_empty(dqi[["abbreviation"]]),
    c("abbreviation", "IndicatorID"),
    drop = FALSE
  ]
  joint <- util_report_scope_joint_assessment_mapping()
  lapply(abbreviations, function(abbreviation) {
    joint_ids <- joint$indicator_id[
      joint$indicator_metric == abbreviation
    ]
    ids <- if (length(joint_ids)) {
      joint_ids
    } else {
      dqi[["IndicatorID"]][dqi[["abbreviation"]] == abbreviation]
    }
    unique(ids[!is.na(ids) & !util_empty(ids)])
  })
}

#' Resolve labels for joint DQ_OBS assessments
#'
#' @param metrics Indicator-metric column names or normalized abbreviations.
#'
#' @return Unique non-empty labels from the declarative mapping.
#'
#' @noRd
util_report_scope_joint_assessment_labels <- function(metrics) {
  mapping <- util_report_scope_joint_assessment_mapping()
  abbreviations <- util_report_scope_normalize_indicator_metrics(metrics)
  labels <- mapping$assessment_label[
    mapping$indicator_metric %in% abbreviations
  ]
  unique(labels[!is.na(labels) & !util_empty(labels)])
}

#' Resolve DQ_OBS concepts represented by item-coverage rows
#'
#' @param coverage Item-coverage data frame.
#'
#' @return A list of character vectors aligned with the coverage rows.
#'
#' @noRd
util_report_scope_coverage_concept_ids <- function(coverage) {
  if (!nrow(coverage)) {
    return(list())
  }
  if ("indicator_metric" %in% colnames(coverage)) {
    return(util_report_scope_metric_indicator_ids(
      coverage$indicator_metric
    ))
  }
  lapply(coverage$indicator_id, function(id) id)
}

#' Identify aggregate variable-group metrics
#'
#' @noRd
util_report_scope_variable_group_is_aggregate_metric <- function(metrics) {
  as.character(metrics) %in% c("NUM_con_con", "PCT_con_con")
}

#' Collect all metrics represented by a hierarchy
#'
#' @noRd
util_report_scope_hierarchy_metrics <- function(spec) {
  direct_metrics <- spec[["metrics"]] %||% character()
  child_metrics <- unlist(lapply(
    spec[["children"]] %||% list(),
    util_report_scope_hierarchy_metrics
  ), use.names = FALSE)
  unique(c(direct_metrics, child_metrics))
}

#' Build one variable-group hierarchy node
#'
#' @noRd
util_report_scope_variable_group_hierarchy_node <- function(
  spec,
  info_scale_dq,
  requested_groups,
  computed_groups,
  classified_groups,
  report = NULL
) {
  metrics <- spec[["metrics"]] %||% character()
  rows <- info_scale_dq[
    info_scale_dq[["Requested variable-group metric"]] %in% metrics,
    , drop = FALSE
  ]
  children <- lapply(spec[["children"]] %||% list(),
    util_report_scope_variable_group_hierarchy_node,
    info_scale_dq = info_scale_dq,
    requested_groups = requested_groups,
    computed_groups = computed_groups,
    classified_groups = classified_groups,
    report = report
  )
  all_metrics <- util_report_scope_hierarchy_metrics(spec)
  all_rows <- info_scale_dq[
    info_scale_dq[["Requested variable-group metric"]] %in% all_metrics,
    , drop = FALSE
  ]
  summary <- util_report_scope_variable_group_summary(
    all_rows,
    requested_groups,
    computed_groups,
    classified_groups,
    concept_scope = spec[["concept_scope"]],
    report = report
  )
  label <- spec[["label"]]
  if (is.null(label)) {
    if (!length(children)) {
      return(util_report_scope_tree_node(
        text = "",
        coverage = summary$coverage,
        measures = 0L,
        assessed = 0L
      ))
    }
    return(children[[1]])
  }
  util_report_scope_tree_node(
    sprintf(
      "%s: %s; %s; %s",
      label,
      util_count_label(nrow(all_rows), "variable-group metric"),
      util_count_label(length(summary$groups), "group"),
      util_report_scope_tree_group_coverage_label(summary$coverage)
    ),
    coverage = summary$coverage,
    classification_items = summary$classification_items,
    concept_coverage = summary$concept_coverage,
    concept_items = summary$concept_items,
    label = label,
    measures = nrow(all_rows),
    assessed = length(summary$groups),
    children = children
  )
}

#' Summarize requested and computed variable-group assessments
#'
#' @noRd
util_report_scope_variable_group_summary <- function(
  rows,
  requested_groups,
  computed_groups,
  classified_groups,
  concept_scope = NULL,
  report = NULL
) {
  metrics <- rows[["Requested variable-group metric"]]
  groups <- unique(unlist(requested_groups[metrics], use.names = FALSE))
  groups <- groups[!is.na(groups) & !util_empty(groups)]
  possible <- vapply(metrics, function(metric) {
    metric_groups <- requested_groups[[metric]]
    metric_groups <- metric_groups[
      !is.na(metric_groups) & !util_empty(metric_groups)
    ]
    length(unique(metric_groups))
  }, integer(1))
  classified_metrics <- metrics[vapply(metrics, function(metric) {
    classified <- unique(classified_groups[[metric]] %||% character())
    requested <- unique(requested_groups[[metric]] %||% character())
    length(intersect(classified, requested)) > 0L
  }, logical(1))]
  computed_metrics <- metrics[vapply(metrics, function(metric) {
    computed <- unique(c(
      computed_groups[[metric]] %||% character(),
      classified_groups[[metric]] %||% character()
    ))
    requested <- unique(requested_groups[[metric]] %||% character())
    length(intersect(computed, requested)) > 0L
  }, logical(1))]
  concept_items <- util_report_scope_variable_group_concepts(
    classified_metrics,
    concept_scope,
    report = report,
    requested_metrics = metrics,
    computed_metrics = computed_metrics
  )
  classified_count <- sum(vapply(metrics, function(metric) {
    groups <- unique(classified_groups[[metric]] %||% character())
    requested <- unique(requested_groups[[metric]] %||% character())
    length(intersect(groups, requested))
  }, integer(1)))
  computed_count <- sum(vapply(metrics, function(metric) {
    groups <- unique(c(
      computed_groups[[metric]] %||% character(),
      classified_groups[[metric]] %||% character()
    ))
    requested <- unique(requested_groups[[metric]] %||% character())
    length(intersect(groups, requested))
  }, integer(1)))
  list(
    groups = groups,
    coverage = util_report_scope_tree_coverage(
      classified_count,
      sum(possible),
      computed_count
    ),
    classification_items =
      util_report_scope_variable_group_classification_items(
        rows,
        requested_groups,
        classified_groups,
        computed_groups
      ),
    concept_coverage = util_report_scope_tree_concept_coverage(
      length(concept_items[["assessed"]]),
      length(concept_items[["possible"]]),
      length(concept_items[["computed"]]),
      length(concept_items[["requested"]])
    ),
    concept_items = concept_items
  )
}

#' Collect computed groups by variable-group metric
#'
#' @noRd
util_report_scope_variable_group_computed_groups <- function(
  info_scale_dq,
  requested_groups,
  report = NULL
) {
  rows <- util_attr(
    info_scale_dq,
    "computed_variable_group_rows",
    exact = TRUE
  )
  if (!is.data.frame(rows) || !all(c("SSI", VAR_NAMES) %in% colnames(rows))) {
    return(list())
  }
  ssi_info <- util_get_concept_info("ssi")
  metric_labels <- setNames(
    ssi_info[["menu_label"]],
    ssi_info[["SSI_METRICS"]]
  )
  normalise_label <- function(x) {
    tolower(gsub("[^[:alnum:]]", "", x))
  }
  groups <- list()
  for (i in seq_len(nrow(rows))) {
    metric <- unname(metric_labels[rows[["SSI"]][[i]]])
    if (!length(metric) || is.na(metric) ||
        !metric %in% names(requested_groups)) {
      next
    }
    candidates <- unique(requested_groups[[metric]])
    candidates <- candidates[!is.na(candidates) & !util_empty(candidates)]
    if (!length(candidates)) {
      next
    }
    var_name <- rows[[VAR_NAMES]][[i]]
    prefix <- paste0(rows[["SSI"]][[i]], "_")
    if (is.na(var_name)) {
      next
    }
    group <- if (startsWith(var_name, prefix)) {
      substr(var_name, nchar(prefix) + 1L, nchar(var_name))
    } else {
      var_name
    }
    if (!is.null(report)) {
      check_id <- rows[[CHECK_ID]][[i]]
      if (is.na(check_id) || util_empty(check_id)) {
        next
      }
      group <- util_report_scope_variable_group_result_labels(report, check_id)
    }
    match <- candidates[normalise_label(candidates) == normalise_label(group)]
    if (length(match) == 1L) {
      groups[[metric]] <- unique(c(groups[[metric]], match))
    }
  }
  groups
}

#' Calculate item-level classification and concept coverage
#'
#' @noRd
util_report_scope_item_coverage <- function(report = NULL, repsum = NULL) {
  empty <- data.frame(
    dimension = character(0),
    function_name = character(0),
    indicator_metric = character(0),
    indicator_id = character(0),
    indicator_label = character(0),
    variable = character(0),
    variable_label = character(0),
    classifications = integer(0),
    computations = integer(0),
    possible_classifications = integer(0),
    applicable = logical(0),
    stringsAsFactors = FALSE
  )
  if (is.null(report)) {
    return(empty)
  }
  if (is.null(repsum)) {
    repsum <- summary(report)
  }
  this <- util_attr(repsum, "this", exact = TRUE)
  meta_data <- util_attr(report, "meta_data", exact = TRUE)
  if (!is.environment(this) || !is.data.frame(this$result) ||
      !is.data.frame(meta_data) || !VAR_NAMES %in% colnames(meta_data)) {
    return(empty)
  }

  study_vars <- meta_data[[VAR_NAMES]]
  if (COMPUTED_VARIABLE_ROLE %in% colnames(meta_data)) {
    study_vars <- study_vars[util_empty(meta_data[[COMPUTED_VARIABLE_ROLE]])]
  }
  variable_group_functions <- util_report_scope_variable_group_function_names()
  result <- this$result
  required_columns <- c(
    VAR_NAMES, "function_name", "indicator_metric", "class"
  )
  if (!all(required_columns %in% colnames(result))) {
    return(empty)
  }
  keep <- vapply(
    result$function_name,
    util_report_scope_function_is_indicator,
    logical(1)
  ) &
    result[[VAR_NAMES]] %in% study_vars &
    !(result$function_name %in% variable_group_functions)
  result <- result[keep, , drop = FALSE]
  result[[".scope_call_name"]] <- result$function_name
  if ("call_names" %in% colnames(result)) {
    call_names <- as.character(result$call_names)
    has_call_name <- !is.na(call_names) & !util_empty(call_names)
    result[[".scope_call_name"]][has_call_name] <- call_names[has_call_name]
  }
  applicable_calls <- unique(result[[".scope_call_name"]])
  applicable_calls <- applicable_calls[vapply(
    applicable_calls,
    util_report_scope_function_is_applicable,
    logical(1),
    report = report
  )]
  result <- result[result[[".scope_call_name"]] %in% applicable_calls, ,
    drop = FALSE
  ]
  applicable_results <- mapply(
    util_report_scope_result_is_applicable,
    function_name = result[[".scope_call_name"]],
    variable = result[[VAR_NAMES]],
    MoreArgs = list(report = report),
    USE.NAMES = FALSE
  )
  result[[".scope_applicable"]] <- applicable_results
  result[[".scope_computed"]] <- if ("values_raw" %in% colnames(result)) {
    !util_summary_display_value_missing(result[["values_raw"]]) |
      !is.na(result[["class"]])
  } else {
    !is.na(result[["class"]])
  }
  stopped_functions <- this$stopped_functions
  if (length(stopped_functions)) {
    available_calls <- vapply(unique(result[[".scope_call_name"]]),
      function(call_name) {
        stopped <- unname(stopped_functions[startsWith(
          names(stopped_functions),
          paste0(call_name, ".")
        )])
        !length(stopped) || any(!stopped)
      },
      FUN.VALUE = logical(1)
    )
    result <- result[
      result[[".scope_call_name"]] %in% names(available_calls)[available_calls],
      , drop = FALSE
    ]
  }
  if (!nrow(result)) {
    return(empty)
  }

  dqi <- util_get_concept_info("dqi")
  dqi <- dqi[dqi[["Level"]] == 3L, , drop = FALSE]
  metrics <- as.character(result$indicator_metric)
  valid_metric <- !is.na(metrics) & !util_empty(metrics) &
    !startsWith(metrics, "CAT_") & !startsWith(metrics, "MSG_")
  abbreviations <- util_report_scope_normalize_indicator_metrics(metrics)
  result[[".scope_applicable"]] <- result[[".scope_applicable"]] &
    util_report_scope_metric_data_type_is_applicable(
      abbreviations,
      result[[VAR_NAMES]],
      report
    )
  indicator_index <- match(abbreviations, dqi[["abbreviation"]])
  keep <- valid_metric & !is.na(indicator_index)
  result <- result[keep, , drop = FALSE]
  indicator_index <- indicator_index[keep]
  abbreviations <- abbreviations[keep]
  if (!nrow(result)) {
    return(empty)
  }
  result$indicator_id <- dqi[["IndicatorID"]][indicator_index]
  result$indicator_label <- dqi[["public_name"]][indicator_index]
  placeholder <- is.na(result$indicator_label) |
    util_empty(result$indicator_label) |
    trimws(result$indicator_label) %in% c(".", "-", "_")
  result$indicator_label[placeholder] <- dqi[["Name"]][indicator_index][
    placeholder
  ]
  joint <- util_report_scope_joint_assessment_mapping()
  joint_labels <- joint$assessment_label[match(
    abbreviations,
    joint$indicator_metric
  )]
  has_joint_label <- !is.na(joint_labels) & !util_empty(joint_labels)
  result$indicator_label[has_joint_label] <- joint_labels[has_joint_label]
  result$dimension <- dqi[["Dimension"]][indicator_index]

  key <- paste(result$indicator_id, result[[VAR_NAMES]], sep = "\f")
  split_rows <- split(result, key)
  rows <- lapply(split_rows, function(x) {
    applicable <- x[[".scope_applicable"]]
    data.frame(
      dimension = x$dimension[[1]],
      function_name = x$function_name[[1]],
      indicator_metric = x$indicator_metric[[1]],
      indicator_id = x$indicator_id[[1]],
      indicator_label = x$indicator_label[[1]],
      variable = x[[VAR_NAMES]][[1]],
      classifications = as.integer(any(!is.na(x$class) & applicable)),
      computations = as.integer(any(x[[".scope_computed"]] & applicable)),
      possible_classifications = as.integer(any(applicable)),
      applicable = any(applicable),
      stringsAsFactors = FALSE
    )
  })
  rows <- do.call(rbind, rows)
  variables <- unique(rows$variable)
  variable_labels <- util_report_scope_item_display_labels(variables, report)
  rows$variable_label <- variable_labels[match(rows$variable, variables)]
  rows
}

#' Build item-level classification details
#'
#' @noRd
util_report_scope_item_classification_items <- function(rows, report = NULL) {
  if (!nrow(rows)) {
    return(util_report_scope_tree_empty_classification_items("Item"))
  }
  indicator_ids <- if ("indicator_id" %in% colnames(rows)) {
    rows$indicator_id
  } else {
    rows$function_name
  }
  identifiers <- paste(indicator_ids, rows$variable, sep = "\f")
  indicator_labels <- if ("indicator_label" %in% colnames(rows)) {
    rows$indicator_label
  } else {
    util_report_scope_item_indicator_labels(rows$function_name)
  }
  unit_labels <- if ("variable_label" %in% colnames(rows)) {
    rows$variable_label
  } else {
    util_report_scope_item_display_labels(rows$variable, report)
  }
  labels <- sprintf("%s of %s", indicator_labels, unit_labels)
  possible <- stats::setNames(labels, identifiers)
  applicable <- if ("applicable" %in% colnames(rows)) {
    rows$applicable
  } else {
    rep(TRUE, nrow(rows))
  }
  computed <- if ("computations" %in% colnames(rows)) {
    rows$computations > 0L
  } else {
    rows$classifications > 0L
  }
  list(
    possible = possible[applicable],
    computed = possible[applicable & computed],
    classified = possible[rows$classifications > 0L],
    unresolved = character(),
    row_label = "Item",
    matrix = data.frame(
      unit = rows$variable,
      unit_label = unit_labels,
      analysis = indicator_labels,
      classified = rows$classifications > 0L,
      computed = computed,
      applicable = applicable,
      stringsAsFactors = FALSE
    )
  )
}

#' Resolve display labels for item-level scope rows
#'
#' @noRd
util_report_scope_item_display_labels <- function(variables, report = NULL) {
  variables <- as.character(variables)
  meta_data <- util_attr(report, "meta_data", exact = TRUE)
  label_col <- as.character(util_attr(report, "label_col", exact = TRUE))
  if (!is.data.frame(meta_data) || !VAR_NAMES %in% colnames(meta_data)) {
    return(variables)
  }
  if (length(label_col) != 1L || !label_col %in% colnames(meta_data)) {
    label_col <- VAR_NAMES
  }
  if (DATA_TYPE %in% colnames(meta_data)) {
    labels <- prep_get_labels(
      resp_vars = variables,
      item_level = meta_data,
      label_col = label_col,
      label_class = "LONG",
      resp_vars_are_var_names_only = TRUE
    )
  } else {
    display_columns <- unique(c(LONG_LABEL, label_col, LABEL))
    display_columns <- display_columns[display_columns %in% colnames(meta_data)]
    labels <- variables
    if (length(display_columns)) {
      labels <- util_map_labels(
        variables,
        meta_data = meta_data,
        from = VAR_NAMES,
        to = display_columns[[1]],
        ifnotfound = variables
      )
    }
  }
  labels <- as.character(labels)
  labels[is.na(labels) | util_empty(labels)] <- variables[
    is.na(labels) | util_empty(labels)
  ]
  ifelse(labels == variables, variables, sprintf("%s: %s", variables, labels))
}

#' Resolve indicator labels for item-level scope rows
#'
#' @noRd
util_report_scope_item_indicator_labels <- function(function_names) {
  dqi <- util_get_concept_info("dqi")
  public_names <- dqi[["public_name"]]
  placeholder <- util_empty(public_names) |
    trimws(public_names) %in% c(".", "-", "_")
  public_names[placeholder] <- dqi[["Name"]][placeholder]
  indicator_labels <- vapply(function_names, function(function_name) {
    labels <- unique(public_names[dqi[["function_R"]] == function_name])
    labels <- labels[!is.na(labels) & nzchar(labels)]
    if (length(labels) == 1L) {
      return(labels)
    }
    sub("^[^:]+: ", "", util_report_scope_function_label(function_name))
  }, character(1))
  indicator_labels
}

#' Build an empty classification-detail table
#'
#' @noRd
util_report_scope_tree_empty_classification_items <- function(row_label) {
  list(
    possible = character(),
    computed = character(),
    classified = character(),
    unresolved = character(),
    row_label = row_label,
    matrix = data.frame(
      unit = character(),
      unit_label = character(),
      analysis = character(),
      classified = logical(),
      computed = logical(),
      applicable = logical(),
      stringsAsFactors = FALSE
    )
  )
}

#' Build variable-group classification details
#'
#' @noRd
util_report_scope_variable_group_classification_items <- function(
  rows,
  requested_groups,
  classified_groups,
  computed_groups = classified_groups
) {
  empty <- util_report_scope_tree_empty_classification_items("Variable group")
  if (!nrow(rows)) {
    return(empty)
  }
  result <- lapply(seq_len(nrow(rows)), function(i) {
    metric <- rows[["Requested variable-group metric"]][[i]]
    groups <- unique(requested_groups[[metric]])
    groups <- groups[!is.na(groups) & !util_empty(groups)]
    if (!length(groups)) {
      return(empty)
    }
    ids <- paste(metric, groups, sep = "\f")
    possible <- stats::setNames(paste(metric, "of", groups), ids)
    graded_groups <- unique(classified_groups[[metric]] %||% character())
    graded_groups <- graded_groups[graded_groups %in% groups]
    calculated_groups <- unique(c(
      computed_groups[[metric]] %||% character(),
      graded_groups
    ))
    calculated_groups <- calculated_groups[calculated_groups %in% groups]
    classified_ids <- ids[groups %in% graded_groups]
    list(
      possible = possible,
      computed = possible[ids[groups %in% calculated_groups]],
      classified = possible[classified_ids],
      unresolved = character(),
      row_label = "Variable group",
      matrix = data.frame(
        unit = groups,
        unit_label = groups,
        analysis = metric,
        classified = groups %in% graded_groups,
        computed = groups %in% calculated_groups,
        applicable = TRUE,
        stringsAsFactors = FALSE
      )
    )
  })
  util_report_scope_tree_merge_classification_items(result)
}

#' Calculate concept coverage for variable-group metrics
#'
#' @noRd
util_report_scope_variable_group_concepts <- function(
  assessed_metrics,
  concept_scope = NULL,
  report = NULL,
  requested_metrics = assessed_metrics,
  computed_metrics = assessed_metrics
) {
  empty <- list(
    possible = character(),
    requested = character(),
    assessed = character(),
    computed = character()
  )
  if (identical(concept_scope, FALSE)) {
    return(empty)
  }

  dqi <- util_get_concept_info("dqi")
  dqi <- dqi[dqi[["Level"]] == 3L, , drop = FALSE]
  reachable_ids <- util_report_scope_target_indicator_ids(
    "variable_group",
    report = report
  )
  assessed_mapping <- util_report_scope_variable_group_dqi_metrics(
    assessed_metrics
  )
  computed_mapping <- util_report_scope_variable_group_dqi_metrics(
    computed_metrics
  )
  requested_mapping <- util_report_scope_variable_group_dqi_metrics(
    requested_metrics
  )
  if (!is.null(report)) {
    represented_ids <- unique(requested_mapping[["IndicatorID"]])
    represented_ids <- represented_ids[
      !is.na(represented_ids) & !util_empty(represented_ids)
    ]
    reachable_ids <- union(reachable_ids, represented_ids)
  }
  dqi <- dqi[dqi[["IndicatorID"]] %in% reachable_ids, , drop = FALSE]
  dqi <- dqi[!duplicated(dqi[["IndicatorID"]]), , drop = FALSE]
  if (!nrow(dqi)) {
    return(empty)
  }

  if (is.null(concept_scope)) {
    concept_scope <- util_report_scope_variable_group_dqi_metrics(
      assessed_metrics
    )[, c("Dimension", "Domain", "Name"), drop = FALSE]
  }
  if (is.list(concept_scope) && !is.data.frame(concept_scope)) {
    concept_scope <- as.data.frame(concept_scope, stringsAsFactors = FALSE)
  }
  if (is.data.frame(concept_scope) && nrow(concept_scope)) {
    if ("IndicatorID" %in% colnames(concept_scope)) {
      dqi <- dqi[dqi[["IndicatorID"]] %in% concept_scope[["IndicatorID"]],
        , drop = FALSE
      ]
    }
    fields <- intersect(colnames(concept_scope),
      c("Dimension", "Domain", "Name")
    )
    if (length(fields)) {
      in_scope <- rep(FALSE, nrow(dqi))
      for (i in seq_len(nrow(concept_scope))) {
        matches <- rep(TRUE, nrow(dqi))
        for (field in fields) {
          value <- concept_scope[[field]][[i]]
          if (!is.na(value) && !util_empty(value)) {
            matches <- matches & dqi[[field]] == value
          }
        }
        in_scope <- in_scope | matches
      }
      dqi <- dqi[in_scope, , drop = FALSE]
    }
  }
  if (!nrow(dqi)) {
    return(empty)
  }

  concept_label <- function(x) {
    label <- x[["public_name"]][[1]]
    if (util_empty(label) || trimws(label) %in% c(".", "-", "_")) {
      label <- x[["Name"]][[1]]
    }
    label
  }
  concept_rows <- split(dqi, dqi[["IndicatorID"]])
  possible <- vapply(concept_rows, concept_label, character(1))
  requested_ids <- unique(dqi[["IndicatorID"]][
    dqi[["abbreviation"]] %in% requested_mapping[["abbreviation"]]
  ])
  requested <- possible[names(possible) %in% requested_ids]
  assessed_ids <- unique(dqi[["IndicatorID"]][
    dqi[["abbreviation"]] %in% assessed_mapping[["abbreviation"]]
  ])
  assessed <- possible[names(possible) %in% assessed_ids]
  computed_ids <- unique(dqi[["IndicatorID"]][
    dqi[["abbreviation"]] %in% computed_mapping[["abbreviation"]]
  ])
  computed <- possible[names(possible) %in% computed_ids]
  list(
    possible = possible,
    requested = requested,
    assessed = assessed,
    computed = computed
  )
}

#' Find reachable DQ_OBS indicators for a result entity
#'
#' @param target_entity Target entity registered for implementations.
#' @param report A result set, or `NULL`.
#'
#' @return Character vector of DQ_OBS indicator identifiers.
#'
#' @noRd
util_report_scope_target_indicator_ids <- function(
  target_entity,
  report = NULL
) {
  target_functions <- util_report_scope_target_functions(target_entity)
  target_functions <- target_functions[vapply(
    target_functions,
    util_report_scope_function_is_indicator,
    logical(1)
  )]
  dqi <- util_get_concept_info("dqi")
  indicator_functions <- unique(dqi[["function_R"]][dqi[["Level"]] == 3L])
  target_functions <- intersect(target_functions, indicator_functions)
  target_functions <- target_functions[vapply(
    target_functions,
    util_report_scope_function_is_applicable,
    logical(1),
    report = report
  )]
  if (!length(target_functions)) {
    return(character())
  }
  indicator_ids <- dqi[["IndicatorID"]][
    dqi[["Level"]] == 3L & dqi[["function_R"]] %in% target_functions
  ]
  possible_by_data_type <- util_report_scope_indicator_data_type_is_possible(
    dqi[["abbreviation"]],
    report
  )
  indicator_ids <- intersect(
    indicator_ids,
    dqi[["IndicatorID"]][dqi[["Level"]] == 3L & possible_by_data_type]
  )
  if (target_entity == "variable_group") {
    computed_mapping <- util_get_concept_info("computed_vars_ind_mapping")
    indicator_ids <- c(indicator_ids, dqi[["IndicatorID"]][
      dqi[["Level"]] == 3L &
        dqi[["abbreviation"]] %in% computed_mapping[["replacement"]]
    ])
  }
  unique(indicator_ids[!is.na(indicator_ids) & nzchar(indicator_ids)])
}

#' Check metric applicability for the data type of an assessed item
#'
#' @noRd
util_report_scope_metric_data_type_is_applicable <- function(
  abbreviations,
  variables,
  report = NULL
) {
  meta_data <- util_attr(report, "meta_data", exact = TRUE)
  if (!is.data.frame(meta_data) ||
      !all(c(VAR_NAMES, DATA_TYPE) %in% colnames(meta_data))) {
    return(rep(TRUE, length(abbreviations)))
  }
  data_types <- meta_data[[DATA_TYPE]][match(variables, meta_data[[VAR_NAMES]])]
  numeric_metrics <- abbreviations %in% c("con_rvv_inum", "con_rvv_unum")
  temporal_metrics <- abbreviations %in% c("con_rvv_itdat", "con_rvv_utdat")
  applicable <- rep(TRUE, length(abbreviations))
  applicable[numeric_metrics] <- data_types[numeric_metrics] %in% c(
    DATA_TYPES$INTEGER,
    DATA_TYPES$FLOAT
  )
  applicable[temporal_metrics] <- data_types[temporal_metrics] %in% c(
    DATA_TYPES$DATETIME,
    DATA_TYPES$TIME
  )
  applicable[is.na(applicable)] <- TRUE
  applicable
}

#' Check concept availability for the data types in a report
#'
#' @noRd
util_report_scope_indicator_data_type_is_possible <- function(
  abbreviations,
  report = NULL
) {
  meta_data <- util_attr(report, "meta_data", exact = TRUE)
  if (!is.data.frame(meta_data) || !DATA_TYPE %in% colnames(meta_data)) {
    return(rep(TRUE, length(abbreviations)))
  }
  if (COMPUTED_VARIABLE_ROLE %in% colnames(meta_data)) {
    meta_data <- meta_data[
      util_empty(meta_data[[COMPUTED_VARIABLE_ROLE]]),
      ,
      drop = FALSE
    ]
  }
  data_types <- unique(meta_data[[DATA_TYPE]])
  numeric_metrics <- abbreviations %in% c("con_rvv_inum", "con_rvv_unum")
  temporal_metrics <- abbreviations %in% c("con_rvv_itdat", "con_rvv_utdat")
  possible <- rep(TRUE, length(abbreviations))
  possible[numeric_metrics] <- any(data_types %in% c(
    DATA_TYPES$INTEGER,
    DATA_TYPES$FLOAT
  ))
  possible[temporal_metrics] <- any(data_types %in% c(
    DATA_TYPES$DATETIME,
    DATA_TYPES$TIME
  ))
  possible
}

#' Check whether Roxygen marks a function as an indicator
#'
#' @param function_name Name of a package function.
#'
#' @return `TRUE` for documented indicators, otherwise `FALSE`.
#'
#' @noRd
util_report_scope_function_is_indicator <- function(function_name) {
  if (!exists(
    function_name,
    envir = .indicator_or_descriptor,
    inherits = FALSE
  )) {
    return(FALSE)
  }
  isTRUE(get(
    function_name,
    envir = .indicator_or_descriptor,
    inherits = FALSE
  ))
}

#' Check whether report results make an implementation applicable
#'
#' @param function_name Name of a package function.
#' @param report A result set, or `NULL`.
#'
#' @return `TRUE` unless every available result only contains intrinsic
#'   applicability problems.
#'
#' @noRd
util_report_scope_function_is_applicable <- function(
  function_name,
  report = NULL
) {
  if (is.null(report) || !function_name %in% colnames(report)) {
    return(TRUE)
  }
  results <- if (length(dim(report)) >= 3L &&
      !inherits(report, "dataquieR_resultset2")) {
    report[, function_name, , drop = TRUE]
  } else {
    report[, function_name, drop = TRUE]
  }
  if (!length(results)) {
    return(TRUE)
  }
  if (inherits(results, "dataquieR_result")) {
    results <- list(results)
  }
  util_report_scope_results_are_applicable(results)
}

#' Check applicability for one implementation and item
#'
#' @param function_name Name of a package function.
#' @param variable Technical item name represented by summary rows.
#' @param report A result set, or `NULL`.
#'
#' @return `FALSE` if the matching item results contain only intrinsic
#'   applicability problems, otherwise `TRUE`.
#'
#' @noRd
util_report_scope_result_is_applicable <- function(
  function_name,
  variable,
  report = NULL
) {
  report_rows <- util_attr(report, "rn", exact = TRUE)
  report_columns <- util_attr(report, "cn", exact = TRUE)
  report_dimnames <- dimnames(report)
  if (!length(report_rows) && length(report_dimnames)) {
    report_rows <- report_dimnames[[1]]
  }
  if (!length(report_columns) && length(report_dimnames) >= 2L) {
    report_columns <- report_dimnames[[2]]
  }
  if (is.null(report) || !length(report_rows) ||
      !function_name %in% report_columns) {
    return(TRUE)
  }

  candidates <- as.character(variable)
  meta_data <- util_attr(report, "meta_data", exact = TRUE)
  label_col <- as.character(util_attr(report, "label_col", exact = TRUE))
  if (is.data.frame(meta_data) && VAR_NAMES %in% colnames(meta_data) &&
      length(label_col) == 1L && label_col %in% colnames(meta_data)) {
    label_index <- match(variable, meta_data[[VAR_NAMES]])
    if (!is.na(label_index)) {
      candidates <- c(candidates, as.character(
        meta_data[[label_col]][[label_index]]
      ))
    }
  }
  item_rows <- report_rows %in%
    candidates[!is.na(candidates) & !util_empty(candidates)]
  if (!any(item_rows)) {
    return(TRUE)
  }

  results <- if (length(dim(report)) >= 3L &&
      !inherits(report, "dataquieR_resultset2")) {
    report[unique(report_rows[item_rows]), function_name, , drop = FALSE]
  } else {
    report[unique(report_rows[item_rows]), function_name, drop = FALSE]
  }
  util_report_scope_results_are_applicable(as.list(results))
}

#' Check whether result objects contain an applicable result
#'
#' @param results List of report result objects.
#'
#' @return `FALSE` if every result only contains intrinsic applicability
#'   problems, otherwise `TRUE`.
#'
#' @noRd
util_report_scope_results_are_applicable <- function(results) {
  intrinsic_only <- vapply(results, function(result) {
    conditions <- util_attr(result, "error", exact = TRUE)
    length(conditions) > 0L && all(vapply(
      conditions,
      inherits,
      logical(1),
      dataquieR.intrinsic_applicability_problem
    ))
  }, logical(1))
  !all(intrinsic_only)
}

#' List DQ functions matching a target entity
#'
#' @noRd
util_report_scope_target_functions <- function(target_entity) {
  allowed_entities <- c(
    "item", "variable_group", "observational_unit", "segment", "dataframe"
  )
  util_stop_if_not(
    "`target_entity` must be a known scalar entity" =
      length(target_entity) == 1L && target_entity %in% allowed_entities
  )
  mapping <- util_get_concept_info("implementations")
  util_stop_if_not(
    "DQ_OBS implementations must provide `target_entity`" =
      "target_entity" %in% names(mapping)
  )
  targets <- lapply(mapping[["target_entity"]], jsonlite::fromJSON)
  valid_targets <- vapply(targets, function(x) {
    is.character(x) && all(x %in% allowed_entities)
  }, logical(1))
  util_stop_if_not(
    "All DQ_OBS target entities must be known" = all(valid_targets)
  )
  mapping[["function_R"]][vapply(
    targets,
    function(x) target_entity %in% x,
    logical(1)
  )]
}

#' Merge duplicate concept-detail entries
#'
#' @noRd
util_report_scope_tree_merge_concept_items <- function(items) {
  items <- Filter(Negate(is.null), items)
  if (!length(items)) {
    return(list(
      possible = character(),
      requested = character(),
      assessed = character(),
      computed = character(),
      joint_assessments = character()
    ))
  }
  merge_one <- function(name, fallback = NULL) {
    values <- unlist(lapply(items, function(item) {
      value <- item[[name]]
      if (is.null(value) && !is.null(fallback)) {
        value <- item[[fallback]]
      }
      value %||% character()
    }), use.names = TRUE)
    values[!duplicated(names(values))]
  }
  list(
    possible = merge_one("possible"),
    requested = merge_one("requested", "possible"),
    assessed = merge_one("assessed"),
    computed = merge_one("computed", "assessed"),
    joint_assessments = unique(unlist(lapply(items, function(item) {
      item[["joint_assessments"]] %||% character()
    }), use.names = FALSE))
  )
}

#' Merge duplicate classification-detail entries
#'
#' @noRd
util_report_scope_tree_merge_classification_items <- function(items) {
  items <- Filter(Negate(is.null), items)
  if (!length(items)) {
    return(util_report_scope_tree_empty_classification_items("Unit"))
  }
  merge_one <- function(name, fallback = NULL) {
    values <- unlist(lapply(items, function(x) x[[name]] %||% character()),
      use.names = TRUE
    )
    if (!is.null(fallback)) {
      values <- unlist(lapply(items, function(x) {
        x[[name]] %||% x[[fallback]] %||% character()
      }), use.names = TRUE)
    }
    values[!duplicated(names(values))]
  }
  matrices <- lapply(items, function(x) {
    matrix <- x[["matrix"]] %||% data.frame()
    if (nrow(matrix) && !"computed" %in% colnames(matrix)) {
      matrix[["computed"]] <- matrix[["classified"]]
    }
    matrix
  })
  matrices <- Filter(nrow, matrices)
  list(
    possible = merge_one("possible"),
    computed = merge_one("computed", "classified"),
    classified = merge_one("classified"),
    unresolved = merge_one("unresolved"),
    row_label = items[[1]][["row_label"]] %||% "Unit",
    matrix = if (length(matrices)) {
      unique(do.call(rbind, matrices))
    } else {
      util_report_scope_tree_empty_classification_items("Unit")[["matrix"]]
    }
  )
}

#' Construct one report-scope tree node
#'
#' @noRd
util_report_scope_tree_node <- function(
  text,
  coverage = NULL,
  classification_items = NULL,
  concept_coverage = NULL,
  concept_items = NULL,
  children = list(),
  label = text,
  measures = NA_integer_,
  assessed = NA_integer_
) {
  list(
    text = text,
    label = label,
    coverage = coverage,
    classification_items = classification_items,
    concept_coverage = concept_coverage,
    concept_items = concept_items,
    measures = measures,
    assessed = assessed,
    children = children
  )
}

#' Calculate classification coverage
#'
#' @noRd
util_report_scope_tree_coverage <- function(
  classifications,
  possible,
  computed = classifications
) {
  list(
    classifications = as.integer(classifications),
    possible = as.integer(possible),
    computed = as.integer(computed)
  )
}

#' Calculate concept coverage
#'
#' @noRd
util_report_scope_tree_concept_coverage <- function(
  assessed,
  possible,
  computed = assessed,
  requested = computed
) {
  list(
    assessed = as.integer(assessed),
    possible = as.integer(possible),
    computed = as.integer(computed),
    requested = as.integer(requested)
  )
}

#' Normalize a report-scope count
#'
#' @noRd
util_report_scope_numeric_count <- function(x) {
  count <- suppressWarnings(as.integer(as.character(x)))
  count[is.na(count)] <- 0L
  count
}

#' Format an item-level classification-coverage label
#'
#' @noRd
util_report_scope_tree_coverage_label <- function(coverage) {
  if (is.null(coverage) || coverage[["possible"]] < 1L) {
    return("no expected results")
  }
  possible <- coverage[["possible"]]
  classified <- coverage[["classifications"]]
  computed <- coverage[["computed"]] %||% classified
  sprintf(
    "%s of %s expected results computed (%s%%); %s classified (%s%%)",
    computed,
    possible,
    formatC(100 * computed / possible, format = "f", digits = 0),
    classified,
    formatC(100 * classified / possible, format = "f", digits = 0)
  )
}

#' Format a variable-group classification-coverage label
#'
#' @noRd
util_report_scope_tree_group_coverage_label <- function(coverage) {
  util_report_scope_tree_coverage_label(coverage)
}

#' Explain classification coverage in a tooltip
#'
#' @noRd
util_report_scope_tree_coverage_tooltip <- function(
  coverage,
  classification_items = NULL
) {
  possible <- coverage[["possible"]]
  classified <- coverage[["classifications"]]
  computed <- coverage[["computed"]] %||% classified
  computed <- max(classified, min(possible, computed))
  unclassified <- max(0L, computed - classified)
  missing <- max(0L, possible - computed)
  if (is.null(classification_items)) {
    classification_items <- util_report_scope_tree_empty_classification_items(
      "Unit"
    )
  }
  matrix <- util_report_scope_tree_classification_matrix(classification_items)
  matrix_data <- classification_items[["matrix"]]
  has_not_applicable <- is.data.frame(matrix_data) &&
    "applicable" %in% colnames(matrix_data) && any(!matrix_data$applicable)
  legend <- if (has_not_applicable) {
    paste(
      "x = classified; \u25cb = computed, not classified;",
      "N/A = not applicable by definition;",
      paste(
        "empty = computation unavailable",
        "(for example, required metadata are missing)"
      )
    )
  } else {
    paste(
      "x = classified; \u25cb = computed, not classified;",
      "empty = computation unavailable",
      "(for example, required metadata are missing)"
    )
  }
  details <- if (is.null(matrix)) list() else list(
    htmltools::tags$hr(),
    htmltools::tags$strong("Classification matrix"),
    htmltools::tags$br(),
    htmltools::tags$span(legend),
    matrix
  )
  heading <- "Result coverage"
  paste(as.character(htmltools::tags$div(
    class = "dq-report-scope-tooltip",
    style = paste(
      "max-height:min(60vh,22em); overflow-y:auto;",
      "padding-right:1.6em; text-align:left;"
    ),
    htmltools::tags$strong(heading),
    htmltools::tags$br(),
    sprintf(
      "Computed results: %s of %s (%s%%)",
      computed,
      possible,
      round(100 * computed / possible)
    ),
    htmltools::tags$br(),
    sprintf(
      "Classified results: %s of %s (%s%%)",
      classified,
      possible,
      round(100 * classified / possible)
    ),
    htmltools::tags$br(),
    sprintf("Expected results: %s", possible), htmltools::tags$br(),
    sprintf("Computed but not classified: %s", unclassified),
    htmltools::tags$br(),
    sprintf("Computation unavailable: %s", missing),
    details
  )), collapse = "")
}

#' Build the classification-detail hover matrix
#'
#' @noRd
util_report_scope_tree_classification_matrix <- function(items) {
  data <- items[["matrix"]]
  if (!is.data.frame(data) || !nrow(data)) {
    return(NULL)
  }
  analyses <- unique(data$analysis)
  units <- unique(data$unit)
  cell <- function(unit, analysis) {
    matches <- data$unit == unit & data$analysis == analysis
    if (!any(matches)) {
      return(htmltools::tags$td())
    }
    if (any(data$classified[matches])) {
      return(htmltools::tags$td(
        title = "Classified",
        style = "text-align:center; font-weight:700;",
        "x"
      ))
    }
    computed <- if ("computed" %in% colnames(data)) {
      data$computed[matches]
    } else {
      data$classified[matches]
    }
    if (any(computed)) {
      return(htmltools::tags$td(
        title = "Computed, not classified",
        style = "text-align:center; font-weight:700;",
        "\u25cb"
      ))
    }
    applicable <- if ("applicable" %in% colnames(data)) {
      data$applicable[matches]
    } else {
      rep(TRUE, sum(matches))
    }
    if (!any(applicable)) {
      return(htmltools::tags$td(
        title = "Not applicable by definition",
        style = "text-align:center; color:#5f6b73;",
        "N/A"
      ))
    }
    htmltools::tags$td(title = "Computation unavailable")
  }
  htmltools::tags$table(
    style = paste(
      "border-collapse:collapse; margin-top:0.3em; font-size:0.9em;",
      "width:100%;"
    ),
    htmltools::tags$thead(htmltools::tags$tr(
      htmltools::tags$th(
        style = "text-align:left; padding:0.15em 0.35em;",
        items[["row_label"]] %||% "Unit"
      ),
      lapply(analyses, function(analysis) {
        htmltools::tags$th(
          style = paste(
            "text-align:center; padding:0.15em 0.35em;",
            "white-space:normal;"
          ),
          analysis
        )
      })
    )),
    htmltools::tags$tbody(lapply(units, function(unit) {
      unit_label <- data$unit_label[data$unit == unit]
      if (!length(unit_label)) {
        unit_label <- unit
      }
      htmltools::tags$tr(
        htmltools::tags$th(
          style = "text-align:left; padding:0.15em 0.35em;",
          unit_label[[1]]
        ),
        lapply(analyses, function(analysis) cell(unit, analysis))
      )
    }))
  )
}

#' Count requested variable groups by metric
#'
#' @noRd
util_report_scope_variable_group_counts <- function(report = NULL) {
  if (is.null(report)) {
    return(list())
  }
  meta_data_cross_item <- util_attr(report, "meta_data_cross_item",
    exact = TRUE
  )
  if (!is.data.frame(meta_data_cross_item) ||
      !CHECK_LABEL %in% colnames(meta_data_cross_item)) {
    return(list())
  }

  ssi_info <- util_get_concept_info("ssi")
  mapping <- setNames(ssi_info[["menu_label"]], ssi_info[["SSI_METRICS"]])
  requested <- intersect(names(mapping), colnames(meta_data_cross_item))
  setNames(lapply(requested, function(metric) {
    meta_data_cross_item[[CHECK_LABEL]][!util_empty(
      meta_data_cross_item[[metric]]
    )]
  }), mapping[requested])
}

#' Collect variable groups represented by categorical results
#'
#' @noRd
util_report_scope_variable_group_categorical_groups <- function(
  report = NULL
) {
  if (is.null(report)) {
    return(list())
  }
  cross_item <- util_attr(report, "meta_data_cross_item", exact = TRUE)
  required <- c(CHECK_LABEL, CONTRADICTION_TYPE)
  if (!is.data.frame(cross_item) ||
      !all(required %in% colnames(cross_item))) {
    return(list())
  }
  labels <- as.character(cross_item[[CHECK_LABEL]])
  types <- trimws(toupper(as.character(cross_item[[CONTRADICTION_TYPE]])))
  keep <- !is.na(labels) & !util_empty(labels) &
    !is.na(types) & !util_empty(types)
  if (!any(keep)) {
    return(list())
  }
  metrics <- vapply(
    types[keep],
    util_con_contradiction_type_label,
    character(1)
  )
  lapply(split(labels[keep], metrics), unique)
}

#' Merge named lists of variable groups
#'
#' @noRd
util_report_scope_merge_group_lists <- function(...) {
  lists <- list(...)
  result <- list()
  for (groups in lists) {
    if (!length(groups)) {
      next
    }
    for (metric in names(groups)) {
      values <- as.character(groups[[metric]])
      values <- values[!is.na(values) & !util_empty(values)]
      result[[metric]] <- unique(c(result[[metric]], values))
    }
  }
  result
}

#' List functions producing variable-group results
#'
#' @noRd
util_report_scope_variable_group_function_names <- function() {
  ssi_info <- util_get_concept_info("ssi")
  functions <- unlist(strsplit(ssi_info[["functions"]], "|", fixed = TRUE))
  unique(sub("[.].*$", "", functions))
}

#' Resolve a report label for a function
#'
#' @noRd
util_report_scope_function_label <- function(function_name) {
  label <- gsub("_", " ", function_name, fixed = TRUE)
  label <- gsub("^com ", "Completeness: ", label)
  label <- gsub("^int ", "Integrity: ", label)
  label <- gsub("^acc ", "Accuracy: ", label)
  label <- gsub("^con ", "Consistency: ", label)
  label
}

#' Select the classification-coverage cell style
#'
#' @noRd
util_report_scope_tree_coverage_style <- function(coverage) {
  if (is.null(coverage)) {
    return(NULL)
  }
  possible <- coverage[["possible"]]
  classifications <- coverage[["classifications"]]
  computed <- coverage[["computed"]] %||% classifications
  if (is.na(possible) || possible < 1L) {
    return(list(
      background = "#f0f0f0",
      color = "#555555",
      title = "No expected results"
    ))
  }
  proportion <- max(0, min(1, computed / possible))
  low <- c(255, 241, 224)
  high <- c(196, 89, 0)
  color <- grDevices::rgb(
    red = round(low[[1]] + proportion * (high[[1]] - low[[1]])),
    green = round(low[[2]] + proportion * (high[[2]] - low[[2]])),
    blue = round(low[[3]] + proportion * (high[[3]] - low[[3]])),
    maxColorValue = 255
  )
  list(
    background = color,
    color = if (proportion >= 0.6) "#ffffff" else "#1f2933",
    title = util_report_scope_tree_coverage_label(coverage)
  )
}

#' Format a concept-coverage label
#'
#' @noRd
util_report_scope_tree_concept_coverage_label <- function(coverage) {
  if (is.null(coverage) || coverage[["possible"]] < 1L) {
    return("No reference concepts available")
  }
  possible <- coverage[["possible"]]
  classified <- coverage[["assessed"]]
  computed <- coverage[["computed"]] %||% classified
  requested <- coverage[["requested"]] %||% computed
  computed_percent <- if (requested > 0L) {
    formatC(100 * computed / requested, format = "f", digits = 0)
  } else {
    "100"
  }
  classified_percent <- if (requested > 0L) {
    formatC(100 * classified / requested, format = "f", digits = 0)
  } else {
    "100"
  }
  sprintf(
    paste(
      "%s of %s possible reference concepts requested (%s%%);",
      "%s of %s requested concepts computed (%s%%);",
      "%s classified (%s%%)"
    ),
    requested,
    possible,
    formatC(100 * requested / possible, format = "f", digits = 0),
    computed,
    requested,
    computed_percent,
    classified,
    classified_percent
  )
}

#' Explain concept coverage in a tooltip
#'
#' @noRd
util_report_scope_tree_concept_coverage_tooltip <- function(
  coverage,
  concept_items = NULL
) {
  if (is.null(concept_items)) {
    return(util_report_scope_tree_concept_coverage_label(coverage))
  }
  possible <- concept_items[["possible"]]
  assessed <- concept_items[["assessed"]]
  computed <- concept_items[["computed"]] %||% assessed
  requested <- concept_items[["requested"]] %||% possible
  coverage[["requested"]] <- length(requested)
  joint_assessments <- concept_items[["joint_assessments"]] %||% character()
  not_classified <- requested[setdiff(names(requested), names(assessed))]
  computed_not_classified <- computed[setdiff(names(computed), names(assessed))]
  not_computed <- possible[setdiff(names(possible), names(computed))]
  requested_not_computed <- not_computed[
    names(not_computed) %in% names(requested)
  ]
  not_requested <- possible[setdiff(names(possible), names(requested))]
  pretty <- function(values) {
    if (!length(values)) {
      return("None")
    }
    htmltools::htmlEscape(util_pretty_vector_string(unname(values)))
  }
  paste(as.character(htmltools::tags$div(
    class = "dq-report-scope-tooltip",
    style = paste(
      "max-height:min(60vh,22em); overflow-y:auto;",
      "padding-right:1.6em; text-align:left;"
    ),
    htmltools::tags$strong("DQ_OBS concept coverage"),
    htmltools::tags$br(),
    util_report_scope_tree_concept_coverage_label(coverage),
    if (length(joint_assessments)) {
      htmltools::tagList(
        htmltools::tags$hr(),
        htmltools::tags$strong("Joint assessments"),
        htmltools::tags$br(),
        htmltools::HTML(pretty(joint_assessments)),
        htmltools::tags$br(),
        htmltools::tags$small(
          paste(
            "One result jointly covers multiple reference concepts;",
            "separate metric values were not computed."
          )
        )
      )
    },
    htmltools::tags$hr(),
    htmltools::tags$strong(sprintf(
      "Computed concepts (%s)",
      length(computed)
    )),
    htmltools::tags$br(),
    htmltools::HTML(pretty(computed)),
    htmltools::tags$hr(),
    htmltools::tags$strong(sprintf(
      "Classified concepts (%s)",
      length(assessed)
    )),
    htmltools::tags$br(),
    htmltools::HTML(pretty(assessed)),
    htmltools::tags$hr(),
    htmltools::tags$strong(sprintf(
      "Requested concepts not classified (%s)",
      length(not_classified)
    )),
    htmltools::tags$br(),
    htmltools::HTML(pretty(not_classified)),
    htmltools::tags$hr(),
    htmltools::tags$strong(sprintf(
      "Computed concepts without a classification (%s)",
      length(computed_not_classified)
    )),
    htmltools::tags$br(),
    htmltools::HTML(pretty(computed_not_classified)),
    htmltools::tags$hr(),
    htmltools::tags$strong(sprintf(
      "Requested concepts not computed (%s)",
      length(requested_not_computed)
    )),
    htmltools::tags$br(),
    htmltools::HTML(pretty(requested_not_computed)),
    htmltools::tags$hr(),
    htmltools::tags$strong(sprintf(
      "Reference concepts not requested (%s)",
      length(not_requested)
    )),
    htmltools::tags$br(),
    htmltools::HTML(pretty(not_requested))
  )), collapse = "")
}

#' Select the concept-coverage cell style
#'
#' @noRd
util_report_scope_tree_concept_coverage_style <- function(coverage) {
  if (is.null(coverage) || is.na(coverage[["possible"]]) ||
      coverage[["possible"]] < 1L) {
    return(list(
      background = "#f0f0f0",
      color = "#555555",
      title = "No reference concepts available"
    ))
  }
  requested <- coverage[["requested"]] %||%
    coverage[["computed"]] %||%
    coverage[["assessed"]]
  proportion <- max(0, min(1, requested / coverage[["possible"]]))
  low <- c(234, 243, 250)
  high <- c(33, 102, 172)
  color <- grDevices::rgb(
    red = round(low[[1]] + proportion * (high[[1]] - low[[1]])),
    green = round(low[[2]] + proportion * (high[[2]] - low[[2]])),
    blue = round(low[[3]] + proportion * (high[[3]] - low[[3]])),
    maxColorValue = 255
  )
  list(
    background = color,
    color = if (proportion >= 0.6) "#ffffff" else "#1f2933",
    title = util_report_scope_tree_concept_coverage_label(coverage)
  )
}

#' Format a count with its singular or plural label
#'
#' @noRd
util_count_label <- function(n, singular) {
  sprintf("%s %s", n, util_plural(n, singular))
}

#' Choose a singular or plural noun
#'
#' @noRd
util_plural <- function(n, singular) {
  if (identical(as.numeric(n), 1)) {
    singular
  } else {
    paste0(singular, "s")
  }
}
