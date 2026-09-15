#' Generate function calls for a given indicator function
#'
#' new reporting pipeline v2.0
#'
#' @inheritParams .template_function_report_plan
#'
#' @param fkt the indicator function's name
#' @param ssi_functions [vector] functions for `SSI-computation`
#' @param non_ssi_functions  functions computation excluding `SSI-computation`
#'
#' @return function calls for the given function
#'
#' @family reporting_functions
#' @concept process
#' @noRd
util_generate_calls_for_function <-
  function(fkt,
    meta_data,
    label_col,
    meta_data_segment,
    meta_data_dataframe,
    meta_data_cross_item,
    specific_args,
    arg_overrides,
    resp_vars,
    ssi_functions,
    non_ssi_functions) {
    fkt_alias <- names(fkt)
    if (length(fkt_alias) != 1 || util_empty(fkt_alias)) {
      fkt_alias <- fkt
    }
    fkt <- unname(fkt)
    .meta_data_env$fkt <- fkt_alias
    .meta_data_env$meta_data <- meta_data
    .meta_data_env$label_col <- label_col
    .meta_data_env$meta_data_segment <- meta_data_segment
    .meta_data_env$meta_data_dataframe <- meta_data_dataframe
    .meta_data_env$meta_data_cross_item <- meta_data_cross_item
    .meta_data_env$apply_to_ssi <- (fkt_alias %in% names(ssi_functions) ||
        fkt %in% ssi_functions)
    .meta_data_env$apply_to_nssi <- (fkt_alias %in% names(non_ssi_functions) ||
        fkt %in% non_ssi_functions)
    withr::defer({
      .meta_data_env$fkt <- NULL
      .meta_data_env$meta_data <- NULL
      .meta_data_env$label_col <- NULL
      .meta_data_env$meta_data_segment <- NULL
      .meta_data_env$meta_data_dataframe <- NULL
      .meta_data_env$meta_data_cross_item <- NULL
      .meta_data_env$target_meta_data <- NULL
      .meta_data_env$apply_to_ssi <- NULL
      .meta_data_env$apply_to_nssi <- NULL
    })
    .to_fill <- formals(fkt)
    to_fill <- list()
    to_fill[intersect(names(.to_fill), names(arg_overrides))] <-
      arg_overrides[intersect(names(.to_fill), names(arg_overrides))]
    to_fill[intersect(names(.to_fill), names(specific_args[[fkt]]))] <-
      specific_args[[fkt]][
        intersect(names(.to_fill), names(specific_args[[fkt]]))
      ]
    if ("study_data" %in% names(.to_fill)) {
      to_fill[["study_data"]] <- quote(study_data)
    }
    if ("meta_data_cross_item" %in% names(.to_fill)) {
      to_fill[["meta_data_cross_item"]] <- quote(meta_data_cross_item)
    }
    if ("meta_data_dataframe" %in% names(.to_fill)) {
      to_fill[["meta_data_dataframe"]] <- quote(meta_data_dataframe)
    }
    if ("meta_data_segment" %in% names(.to_fill)) {
      to_fill[["meta_data_segment"]] <- quote(meta_data_segment)
    }
    if ("meta_data" %in% names(.to_fill)) {
      to_fill[["meta_data"]] <- quote(meta_data)
    }
    if ("label_col" %in% names(.to_fill)) {
      to_fill[["label_col"]] <- label_col
    }
    if ("resp_vars" %in% names(.to_fill)) {
      if ("variable_group" %in% names(.to_fill)) {
        util_error("")
      }
      .meta_data_env$target_meta_data <- "item_level"
      lapply(setNames(nm = resp_vars), function(rv) {
        fillers <- names(.meta_data_env)
        fillers <- fillers[vapply(
          FUN.VALUE = logical(1),
          fillers,
          function(f) {
            is.function(.meta_data_env[[f]])
          }
        )]
        can_fill <- intersect(names(.to_fill), fillers)
        to_fill[can_fill] <-
          lapply(setNames(nm = can_fill), function(filler) {
            .meta_data_env[[filler]](rv)
          })
        if (CHECK_ID %in% colnames(meta_data) &&
            CHECK_ID %in% colnames(meta_data_cross_item)) {
          check_id <- util_map_labels(
            rv,
            meta_data = meta_data,
            from = label_col,
            to = CHECK_ID,
            ifnotfound = NA_character_,
            warn_ambiguous = FALSE
          )
          if (length(check_id) == 1L && !util_empty(check_id)) {
            attr(to_fill, CHECK_ID) <- as.character(check_id)
            check_row <- match(
              as.character(check_id),
              as.character(meta_data_cross_item[[CHECK_ID]])
            )
            if (!is.na(check_row) &&
                CHECK_LABEL %in% colnames(meta_data_cross_item)) {
              attr(to_fill, CHECK_LABEL) <- as.character(
                meta_data_cross_item[[CHECK_LABEL]][[check_row]]
              )
            }
          }
        }
        # resp_vars can be filled directly from rv when needed.
        to_fill
      })
    } else if ("variable_group" %in% names(.to_fill)) {
      if ("resp_vars" %in% names(.to_fill)) {
        util_error("")
      }
      .meta_data_env$target_meta_data <- "cross-item_level"
      ck_id <- meta_data_cross_item[[CHECK_ID]]
      ck_id[util_empty(ck_id)] <- NA_character_
      nm <- meta_data_cross_item[[CHECK_LABEL]]
      nm[util_empty(nm)] <- NA_character_
      if (any(is.na(nm))) {
        util_message("Removing rows from %s because %s is missing.",
          sQuote("cross-item_level"),
          sQuote(CHECK_LABEL),
          applicability_problem = TRUE
        )
      }
      ck_id <- setNames(ck_id[!is.na(nm) & !is.na(ck_id)],
        nm = nm[!is.na(nm) & !is.na(ck_id)]
      )
      lapply(ck_id, function(ci) {
        if (!util_empty(ci)) {
          withr::defer(
            .meta_data_env$meta_data_cross_item <- meta_data_cross_item
          )
          .meta_data_env$meta_data_cross_item <- meta_data_cross_item[
            (is.na(meta_data_cross_item[[CHECK_ID]]) & is.na(ci)) |
              (meta_data_cross_item[[CHECK_ID]] == ci), ,
            drop = FALSE
          ]
          fillers <- names(.meta_data_env)
          fillers <- fillers[vapply(
            FUN.VALUE = logical(1),
            fillers,
            function(f) {
              is.function(.meta_data_env[[f]])
            }
          )]
          can_fill <- intersect(names(.to_fill), fillers)
          to_fill[can_fill] <-
            lapply(setNames(nm = can_fill), function(filler) {
              .meta_data_env[[filler]](ci)
            })

          vg <- .meta_data_env$meta_data_cross_item[[VARIABLE_LIST]]

          variable_group <- names(util_parse_assignments(vg))
          variable_group <- util_find_var_by_meta(variable_group,
            meta_data,
            label_col = label_col,
            target = label_col,
            ifnotfound = variable_group
          )
          to_fill$variable_group <- variable_group
          attr(to_fill, CHECK_ID) <- as.character(ci)
          attr(to_fill, CHECK_LABEL) <- as.character(
            .meta_data_env$meta_data_cross_item[[CHECK_LABEL]][[1]]
          )
        }
        to_fill
      })
    } else {
      # util_error("") not an error, may be a function w/o entity form the int
      # dim
      list(`[ALL]` = to_fill)
    }
  }
