#' Create a table summarizing the number of social science metrics created in
#' the report
#'
#' @param report a report
#' @param repsum [data.frame] optional precomputed [summary()] of `report`
#'
#' @return a table containing the number of variable groups for which a
#' social science metric wascreated
#'
#' @noRd
util_generate_table_scale <- function(report, repsum = NULL) {
  util_stop_if_not(inherits(report, "dataquieR_resultset2"))

  # Obtain the names of the group (e.g., observer, device...)
  tab_attrib <- attributes(attributes(report)$matrix_list)$function_alias_map

  alias2functionname <- setNames(tab_attrib$name, nm = tab_attrib$alias)

  ssi_info <- util_get_concept_info("ssi")
  vector_ssi <- ssi_info[["menu_label"]]
  ssi_metrics <- ssi_info[["SSI_METRICS"]]
  requested_ssi_metrics <- character(0)
  meta_data_cross_item <- util_attr(report, "meta_data_cross_item",
    exact = TRUE
  )
  if (is.data.frame(meta_data_cross_item) && nrow(meta_data_cross_item) > 0) {
    requested_ssi_metrics <- intersect(ssi_metrics, colnames(
      meta_data_cross_item
    ))
    requested_ssi_metrics <- requested_ssi_metrics[
      vapply(
        meta_data_cross_item[requested_ssi_metrics],
        function(x) any(!util_empty(x)),
        FUN.VALUE = logical(1)
      )
    ]
  }

  if (is.null(repsum)) {
    repsum <- summary(report)
  }
  # this represents the current object
  this <- rlang::env_clone(util_attr(repsum, "this", exact = TRUE))
  this$requested_ssi_metrics <- requested_ssi_metrics
  this$vector_ssi <- vector_ssi
  this$ssi_metrics <- ssi_metrics
  this$ssi_info <- ssi_info
  withr::with_environment(this, {
    metric_rows <- data.frame(
      SSI = character(0),
      CHECK_ID = character(0),
      VAR_NAMES = character(0),
      stringsAsFactors = FALSE
    )
    empty_metric_rows <- metric_rows

    # For robustness: in case none of the assessments work, create an empty
    # result
    # if result is empty or contains only errors
    if (!prod(dim(this$result))) {
      # create a table containing only zeros
      info_scale_dq <- data.frame(
        "Requested variable-group metric" =
          vector_ssi[match(requested_ssi_metrics, ssi_metrics)],
        "Computed variable-group results" =
          rep(0, length(requested_ssi_metrics)),
        check.names = FALSE
      )
    } else {
      #import variable-group metrics list----
      ssi_functions <- unique(sub(
        "[.].*$",
        "",
        unlist(strsplit(ssi_info[["functions"]], "|", fixed = TRUE))
      ))

      successful_names <- names(this$stopped_functions)[
        !unname(this$stopped_functions)
      ]
      successful_names <- successful_names[
        sub("[.].*$", "", successful_names) %in% ssi_functions
      ]

      meta_data <- util_attr(report, "meta_data", exact = TRUE)
      if (!COMPUTED_VARIABLE_ROLE %in% colnames(meta_data)) {
        meta_data[[COMPUTED_VARIABLE_ROLE]] <- NA_character_
      }
      if (!CHECK_ID %in% colnames(meta_data)) {
        meta_data[[CHECK_ID]] <- NA_character_
      }
      metric_rows <- lapply(successful_names, function(result_name) {
        call <- util_attr(report[[result_name]], "call", exact = TRUE)
        var_names <- util_attr(call, VAR_NAMES, exact = TRUE)
        if (!length(var_names)) {
          return(NULL)
        }
        var_names <- unname(var_names)
        roles <- util_map_labels(
          var_names,
          meta_data = meta_data,
          to = COMPUTED_VARIABLE_ROLE,
          from = VAR_NAMES,
          ifnotfound = NA_character_
        )
        check_ids <- util_map_labels(
          var_names,
          meta_data = meta_data,
          to = CHECK_ID,
          from = VAR_NAMES,
          ifnotfound = NA_character_
        )
        keep <- roles %in% ssi_metrics
        if (!any(keep)) {
          return(NULL)
        }
        data.frame(
          SSI = roles[keep],
          CHECK_ID = check_ids[keep],
          VAR_NAMES = var_names[keep],
          stringsAsFactors = FALSE
        )
      })
      metric_rows <- Filter(is.data.frame, metric_rows)
      if (length(metric_rows)) {
        metric_rows <- do.call(rbind, metric_rows)
        metric_rows <- unique(metric_rows)
      } else {
        metric_rows <- empty_metric_rows
      }
      info_scale_dq <- as.data.frame(
        table(factor(metric_rows$SSI, levels = requested_ssi_metrics)),
        responseName = "count"
      )
      mapping_ssi <- setNames(vector_ssi, ssi_metrics)
      info_scale_dq$Var1 <- mapping_ssi[as.character(info_scale_dq$Var1)]
      colnames(info_scale_dq) <- c("Requested variable-group metric",
        "Computed variable-group results")
    }
    attr(info_scale_dq, "computed_variable_group_rows") <- metric_rows
    info_scale_dq
  })
}
