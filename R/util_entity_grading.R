#' Attach render-time grading context to entity-level result tables
#'
#' @param result [list] result returned by an indicator.
#' @param env [environment] evaluation environment containing entity metadata.
#' @param function_name [character] indicator function name.
#'
#' @return `result` with grading context attributes on segment- and
#'   dataframe-level table/data slots. No class or color is computed here.
#' @noRd
util_attach_entity_grading_context <- function(result,
  env,
  function_name) {
  specs <- list(
    list(
      table_slot = "SegmentTable",
      data_slot = "SegmentData",
      table_id = "Segment",
      metadata_name = "meta_data_segment",
      metadata_id = STUDY_SEGMENT,
      entity = "SEGMENT"
    ),
    list(
      table_slot = "DataframeTable",
      data_slot = "DataframeData",
      table_id = DF_NAME,
      metadata_name = "meta_data_dataframe",
      metadata_id = DF_NAME,
      entity = "DATAFRAME"
    )
  )

  for (spec in specs) {
    if (!(spec$table_slot %in% names(result))) {
      next
    }
    table <- result[[spec$table_slot]]
    if (!is.data.frame(table) || !nrow(table) ||
        !(spec$table_id %in% colnames(table))) {
      next
    }
    metadata <- try(
      get(spec$metadata_name, envir = env, inherits = TRUE),
      silent = TRUE
    )
    if (util_is_try_error(metadata) || !is.data.frame(metadata) ||
        !(spec$metadata_id %in% colnames(metadata))) {
      next
    }

    metadata <- util_ensure_grading_ruleset_metadata(metadata)
    entity_names <- as.character(table[[spec$table_id]])
    grading_rule_sets <- util_entity_grading_rulesets(
      entity_names = entity_names,
      metadata = metadata,
      metadata_id = spec$metadata_id
    )
    metrics <- util_extract_indicator_metrics(table)
    metric_names <- setdiff(
      colnames(metrics),
      grep("^GRADING_", colnames(metrics), value = TRUE)
    )
    if (!length(metric_names)) {
      next
    }

    context <- list(
      entity = spec$entity,
      table_id = spec$table_id,
      metadata_id = spec$metadata_id,
      entity_names = entity_names,
      grading_rule_sets = grading_rule_sets,
      indicator_metrics = metric_names,
      values_raw = table[, metric_names, drop = FALSE],
      function_name = function_name
    )
    attr(result[[spec$table_slot]], "entity_grading_context") <- context
    if (spec$data_slot %in% names(result) &&
        is.data.frame(result[[spec$data_slot]])) {
      attr(result[[spec$data_slot]], "entity_grading_context") <- context
    }
  }
  result
}

#' Resolve entity-level grading rulesets deterministically
#'
#' @param entity_names [character] entity IDs in result-row order.
#' @param metadata [data.frame] checked entity-level metadata.
#' @param metadata_id [character] entity-ID metadata column.
#'
#' @return [character] one effective ruleset for every entity.
#' @noRd
util_entity_grading_rulesets <- function(entity_names,
  metadata,
  metadata_id) {
  vapply(entity_names, function(entity_name) {
    matches <- as.character(metadata[[GRADING_RULESET]][
      as.character(metadata[[metadata_id]]) == entity_name
    ])
    matches <- unique(matches[!util_empty(matches)])
    if (length(matches) > 1L) {
      util_error(
        "More than one %s is assigned to %s %s.",
        sQuote(GRADING_RULESET),
        tolower(metadata_id),
        dQuote(entity_name),
        applicability_problem = TRUE
      )
    }
    if (!length(matches)) "0" else matches[[1L]]
  }, FUN.VALUE = character(1), USE.NAMES = FALSE)
}

#' Add current entity grading to a table immediately before rendering
#'
#' @param x [data.frame] entity-level table or display data.
#'
#' @return `x`, with graded cells marked as HTML and DataTables grading
#'   configuration attached. The stored result object is not modified.
#' @noRd
util_apply_entity_grading_for_render <- function(x) {
  context <- util_attr(x, "entity_grading_context", exact = TRUE)
  required <- c(
    "entity", "table_id", "entity_names", "grading_rule_sets",
    "indicator_metrics", "values_raw", "function_name"
  )
  if (is.null(context) || !all(required %in% names(context)) ||
      !is.data.frame(context$values_raw)) {
    return(x)
  }

  display_id <- if (identical(context$entity, "DATAFRAME")) {
    intersect(c("Dataframe", DF_NAME), colnames(x))
  } else {
    intersect(c("Segment", STUDY_SEGMENT), colnames(x))
  }
  if (!length(display_id)) {
    return(x)
  }
  display_id <- display_id[[1L]]
  source_rows <- match(
    as.character(x[[display_id]]),
    context$entity_names
  )
  if (all(is.na(source_rows))) {
    return(x)
  }

  metric_targets <- util_entity_grading_metric_targets(
    colnames(x),
    context$indicator_metrics
  )
  metric_targets <- metric_targets[!is.na(metric_targets)]
  if (!length(metric_targets)) {
    return(x)
  }

  # Prefer percentage grading when a compact N (%) display represents both
  # count and percentage metrics.
  metric_order <- order(!startsWith(names(metric_targets), "PCT_"))
  metric_targets <- metric_targets[metric_order]
  metric_targets <- metric_targets[!duplicated(metric_targets)]

  rendered <- util_df_escape(x)
  grading_columns <- character()
  grading_labels <- util_get_labels_grading_class()
  for (metric in names(metric_targets)) {
    target <- unname(metric_targets[[metric]])
    values_raw <- context$values_raw[[metric]][source_rows]
    summary_values <- data.frame(
      function_name = rep(context$function_name, nrow(x)),
      indicator_metric = rep(metric, nrow(x)),
      values_raw = values_raw,
      call_names = rep("", nrow(x)),
      stringsAsFactors = FALSE
    )
    summary_values[[VAR_NAMES]] <- as.character(x[[display_id]])
    summary_values$.entity_grading_order <- seq_len(nrow(summary_values))
    grading_meta_data <- data.frame(
      context$entity_names,
      context$grading_rule_sets,
      stringsAsFactors = FALSE
    )
    colnames(grading_meta_data) <- c(VAR_NAMES, GRADING_RULESET)
    grading_meta_data <- unique(grading_meta_data)
    classified <- suppressWarnings(util_metrics_to_classes(
      summary_values,
      grading_meta_data,
      entity = context$entity
    ))
    classified <- classified[order(classified$.entity_grading_order), ,
      drop = FALSE
    ]
    labels <- unname(grading_labels[as.character(classified$class)])
    graded <- !is.na(labels)
    if (!any(graded)) {
      next
    }
    rendered[[target]][graded] <- sprintf(
      '<span data-grading="%s">%s</span>',
      htmltools::htmlEscape(labels[graded], attribute = TRUE),
      rendered[[target]][graded]
    )
    grading_columns <- c(grading_columns, target)
  }

  if (!length(grading_columns)) {
    return(x)
  }
  attr(rendered, "is_html_escaped") <- TRUE
  attr(rendered, "entity_grading_args") <- list(
    grading_cols = unique(grading_columns),
    grading_order = grading_labels,
    grading_colors = util_get_colors(),
    fg_colors = util_get_fg_color(util_get_colors())
  )
  rendered
}

#' Map raw indicator metrics to rendered entity-table columns
#'
#' @param display_names [character] rendered column names.
#' @param metrics [character] raw indicator metric names.
#'
#' @return named [character] vector of display columns; unmatched metrics are
#'   represented by `NA`.
#' @noRd
util_entity_grading_metric_targets <- function(display_names, metrics) {
  translated <- util_translate_indicator_metrics(
    metrics,
    short = FALSE,
    long = TRUE,
    ignore_unknown = TRUE
  )
  targets <- vapply(seq_along(metrics), function(index) {
    exact <- intersect(c(metrics[[index]], translated[[index]]), display_names)
    if (length(exact)) {
      return(exact[[1L]])
    }
    if (!startsWith(metrics[[index]], "PCT_")) {
      return(NA_character_)
    }
    concept <- sub(" \\(.*$", "", translated[[index]], perl = TRUE)
    compact <- display_names[
      startsWith(display_names, concept) & endsWith(display_names, "N (%)")
    ]
    if (length(compact)) compact[[1L]] else NA_character_
  }, FUN.VALUE = character(1), USE.NAMES = FALSE)
  setNames(targets, metrics)
}
