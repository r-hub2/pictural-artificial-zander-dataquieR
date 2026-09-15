# nolint start: line_length_linter.
#' Checks user-defined contradictions in study data
#'
#' @description
#' This approach considers a contradiction if impossible combinations of data
#' are observed in one participant. For example, if age of a participant is
#' recorded repeatedly the value of age is (unfortunately) not able to decline.
#' Most cases of contradictions rest on comparison of two variables.
#'
#' Important to note, each value that is used for comparison may represent a
#' possible characteristic but the combination of these two values is considered
#' to be impossible. The approach does not consider implausible or inadmissible
#' values.
#'
#' [Indicator]
#'
#' @details
#' ### Algorithm of this implementation:
#'
#'  - Remove missing codes from the study data (if defined in the metadata)
#'  - Remove measurements deviating from limits defined in the metadata
#'  - Assign label to levels of categorical variables (if applicable)
#'  - Apply contradiction checks (given as `REDCap`-like rules in a separate
#'    metadata table)
#'  - Identification of measurements fulfilling contradiction rules. Therefore
#'    two output data frames are generated:
#'    - on the level of observation to flag each contradictory value
#'      combination, and
#'    - a summary table for each contradiction check.
#'  - A summary plot illustrating the number of contradictions is generated.
#'
#' List function.
#'
#' @inheritParams .template_function_indicator
#'
#' @param threshold_value [numeric] from=0 to=100. a numerical value
#'                                                 ranging from 0-100
#' @param summarize_categories [logical] Needs a column `CONTRADICTION_TYPE` in
#'                             the `meta_data_cross_item`.
#'                             If set, a summary output is generated for the
#'                             defined categories plus one plot per
#'                             category. Categories cannot currently be
#'                             configured through metadata.
#' @param use_value_labels [logical] Deprecated in favor of [DATA_PREPARATION].
#'                             If set to `TRUE`, labels can be used in the
#'                             `REDCap` syntax to specify contraction checks for
#'                             categorical variables. If set to `FALSE`,
#'                             contractions have to be specified using the coded
#'                             values. In case that this argument is not set in
#'                             the function call, it will be set to `TRUE` if
#'                             the metadata contains a column `VALUE_LABELS`
#'                             which is not empty.
#'
#' `con_contradictions_redcap()` expects contradiction rules in the cross-item
#' level metadata. See the
#' [online documentation](https://dataquality.qihs.uni-greifswald.de/VIN_Cross_Item_Level_Metadata.html)
#' for the cross-item metadata structure.
#'
#' @return
#' If `summarize_categories` is `FALSE`:
#' A [list] with:
#'   - `FlaggedStudyData`: The first output of the contradiction function is a
#'                         data frame of similar dimension regarding the number
#'                         of observations in the study data. In addition, for
#'                         each applied check on the variables an additional
#'                         column is added which flags observations with a
#'                         contradiction given the applied check.
#'   - `VariableGroupData`: The second output summarizes this information
#'                     into one
#'                     data frame. This output can be used to provide an
#'                     executive overview on the amount of contradictions.
#'   - `VariableGroupTable`: A subset of `VariableGroupData` used within the
#'                           pipeline.
#'   - `SummaryPlot`: The third output visualizes summarized information
#'                    of `SummaryData`.
#'
#' If `summarize_categories` is `TRUE`, other objects are returned:
#' A list with one element `Other`, a list with the following entries:
#' One per category named by that category (e.g. "Empirical") containing a
#' result for contradiction checks within that category only. Additionally, in the
#' slot `all_checks`, a result as it would have been returned with
#' `summarize_categories` set to `FALSE`. Finally, in
#' the top-level list, a slot `SummaryData` is
#' returned containing sums per Category and an according [ggplot2::ggplot] in
#' `SummaryPlot`.
#'
#' @export
#'
#' @importFrom ggplot2 ggplot geom_bar scale_fill_manual theme_minimal scale_y_continuous geom_hline coord_flip theme aes geom_text xlab scale_x_continuous sec_axis
#' @importFrom stats ave setNames
#' @seealso
#' [Online Documentation for the function](
#' https://dataquality.qihs.uni-greifswald.de/VIN_con_impl_contradictions_redcap.html
#' )
#' [meta_data_cross_item]
#' [Online Documentation for the required cross-item-level metadata](
#' https://dataquality.qihs.uni-greifswald.de/Cross_Item_Level_Metadata.html
#' )
#' @author Thomas J. Musholt contributed the contradiction-type readability
#'   improvements for the summary plot.
# nolint end
con_contradictions_redcap <- function(study_data,
  item_level = "item_level",
  label_col, threshold_value,
  meta_data_cross_item = "cross-item_level",
  use_value_labels,
  summarize_categories = FALSE,
  # Historical `flip_mode` argument prototype removed here.
  meta_data = item_level,
  cross_item_level,
  `cross-item_level`,
  meta_data_v2) {
  # preps ----------------------------------------------------------------------
  util_maybe_load_meta_data_v2()

  display_label <- xmax <- xmid <- xmin <- NULL

  util_ck_arg_aliases()

  if (!missing(use_value_labels)) {
    lifecycle::deprecate_stop(
      when = "2.1.0",
      what = "con_contradictions_redcap(use_value_labels)",
      details =
        "Please use DATA_PREPARATION in meta_data_cross_item now."
    )
  }

  # table of specified contradictions
  util_expect_data_frame(meta_data_cross_item, list(
    CONTRADICTION_TERM = is.character
  ),
  min_rows = 1,
  empty_error_message = sprintf(
    paste(
      "No cross-item-level metadata given.",
      "%s requires cross-item-level metadata with contradiction rules.",
      "Load a metadata workbook containing the %s sheet or pass %s explicitly."
    ),
    sQuote("con_contradictions_redcap()"),
    sQuote("cross-item_level"),
    sQuote("meta_data_cross_item")
  )
  )
  if (!CHECK_LABEL %in% colnames(meta_data_cross_item)) {
    if (nrow(meta_data_cross_item) > 0) {
      meta_data_cross_item[[CHECK_LABEL]] <-
        paste0("Check #", seq_len(nrow(meta_data_cross_item)))
    } else {
      meta_data_cross_item[[CHECK_LABEL]] <- character(0)
    }
  }
  util_expect_data_frame(meta_data_cross_item, list(
    CONTRADICTION_TERM = is.character,
    CHECK_LABEL = is.character
  ))

  # map metadata to study data
  prep_prepare_dataframes(
    .replace_hard_limits = FALSE,
    .replace_missings = FALSE
  ) # replacements are performed later

  meta_data_cross_item <-
    meta_data_cross_item[!util_empty(
      meta_data_cross_item[[CONTRADICTION_TERM]]
    ), , FALSE]

  meta_data_cross_item <- util_normalize_cross_item(
    meta_data = meta_data,
    meta_data_cross_item = meta_data_cross_item,
    label_col = label_col
  )
  if (!(GRADING_RULESET %in% colnames(meta_data_cross_item))) {
    meta_data_cross_item[[GRADING_RULESET]] <- rep("0", nrow(meta_data_cross_item)) # nolint: line_length_linter.
  }
  meta_data_cross_item[[GRADING_RULESET]] <-
    trimws(as.character(meta_data_cross_item[[GRADING_RULESET]]))
  meta_data_cross_item[
    util_empty(meta_data_cross_item[[GRADING_RULESET]]),
    GRADING_RULESET
  ] <- "0"

  # There might be rows without contradiction rules (NAs), which should be
  # removed first.
  if (any(is.na(meta_data_cross_item[[CONTRADICTION_TERM]]))) {
    meta_data_cross_item <- meta_data_cross_item[-which(is.na(meta_data_cross_item[[CONTRADICTION_TERM]])), , drop = FALSE] # nolint: line_length_linter.
  }

  if (missing(threshold_value)) {
    threshold_value <- NA_real_
    if (!.called_in_pipeline) {
      util_message("No %s has been set; using default %s.",
        dQuote("threshold_value"),
        dQuote(as.character(threshold_value)),
        applicability_problem = FALSE
      )
    }
  } else {
    util_expect_scalar(threshold_value,
      allow_na = TRUE,
      check_type = function(x) {
        if (is.na(x)) {
          return(TRUE)
        }
        is.numeric(x) && !is.na(x) && x >= 0 && x <= 100
      },
      error_message = sprintf(
        "%s must be a number between %d and %d",
        sQuote("threshold_value"),
        0,
        100
      )
    )
    threshold_value <- as.numeric(threshold_value)
  }

  util_expect_scalar(summarize_categories, check_type = is.logical)

  # parse redcap rules to obtain interpretable contradiction checks
  compiled_rules <- lapply(
    setNames(nm = meta_data_cross_item[[CONTRADICTION_TERM]]),
    util_parse_redcap_rule
  )

  # colors
  cols <- c("0" = "#2166AC", "1" = "#B2182B")

  # summarize contradictions per category given in CONTRADICTION_TYPE
  # -------------------------------------
  if (summarize_categories) {
    if (!(CONTRADICTION_TYPE %in% colnames(meta_data_cross_item))) {
      util_error(
        c(
          "Cannot summerize categories of contradictions,",
          "because these are not defined in the meta_data_cross_item",
          "as column %s."
        ),
        sQuote(CONTRADICTION_TYPE),
        applicability_problem = TRUE
      )
    }

    split_tags <- lapply(strsplit(meta_data_cross_item[[CONTRADICTION_TYPE]], SPLIT_CHAR, fixed = TRUE), trimws) # nolint: line_length_linter.
    tags <- sort(unique(unlist(split_tags)))
    tags <- setNames(nm = tags)
    tags_ext <- tags
    tags_ext[["all_checks"]] <- NA

    result <- lapply(tags_ext, function(atag) {
      # generate one output per category (stratified)
      if (is.na(atag)) {
        new_ct <- meta_data_cross_item[, ,
          drop = FALSE
        ]
      } else {
        contains_tag <- function(x, tg) {
          any(x == tg, na.rm = TRUE)
        }
        rows_matching_tag <- vapply(split_tags, contains_tag,
          tg = atag,
          logical(1)
        )
        new_ct <- meta_data_cross_item[rows_matching_tag, ,
          drop = FALSE
        ]
      }
      # recursive call of the function only for the contradiction checks of the
      # currently selected category in "atag"
      r <- try(con_contradictions_redcap(
        study_data = study_data,
        meta_data = meta_data, label_col = label_col,
        threshold_value = threshold_value, meta_data_cross_item = new_ct,
        summarize_categories = FALSE
      ), silent = TRUE)
      if (inherits(r, "try-error")) {
        list(FlaggedStudyData = data.frame())
      } else {
        r
      }
    })

    # summarize the outputs of the recursive calls
    rx <- lapply(tags_ext, function(atag) {
      if (is.na(atag)) {
        round(sum(rowSums(result[["all_checks"]]$FlaggedStudyData[, -1, drop = FALSE], # nolint: line_length_linter.
              na.rm = TRUE
            ) > 0) /
            nrow(result[["all_checks"]]$FlaggedStudyData) * 100, digits = 2)
      } else {
        round(sum(rowSums(result[[atag]]$FlaggedStudyData[, -1, drop = FALSE],
              na.rm = TRUE
            ) > 0) /
            nrow(result[[atag]]$FlaggedStudyData) * 100, digits = 2)
      }
    })
    rx_num <- lapply(tags_ext, function(atag) {
      if (is.na(atag)) {
        sum(rowSums(result[["all_checks"]]$FlaggedStudyData[, -1, drop = FALSE],
            na.rm = TRUE
          ) > 0)
      } else {
        sum(rowSums(result[[atag]]$FlaggedStudyData[, -1, drop = FALSE],
            na.rm = TRUE
          ) > 0)
      }
    })
    rx <- data.frame(
      CONTRADICTION_TYPE = names(rx),
      PCT_con_con = unlist(rx),
      NUM_con_con = unlist(rx_num),
      GRADING = ordered(ifelse(unlist(rx) > threshold_value, 1, 0))
    )
    rx[[GRADING_RULESET]] <- vapply(tags_ext, function(atag) {
      if (is.na(atag)) {
        rulesets <- result[["all_checks"]]$VariableGroupTable[[GRADING_RULESET]]
      } else {
        rulesets <- result[[atag]]$VariableGroupTable[[GRADING_RULESET]]
      }
      rulesets <- unique(rulesets[!util_empty(rulesets)])
      if (length(rulesets) == 0) {
        "0"
      } else {
        paste(rulesets, collapse = SPLIT_CHAR)
      }
    }, FUN.VALUE = character(1))
    if ("LOGICAL" %in% rx[[CONTRADICTION_TYPE]]) {
      rx$PCT_con_con_contc <- rep(NA_real_, nrow(rx))
      rx$PCT_con_con_contc[
        rx[[CONTRADICTION_TYPE]] %in% c("LOGICAL")
      ] <- rx$PCT_con_con[rx[[CONTRADICTION_TYPE]]
        %in% c("LOGICAL")]
      rx$NUM_con_con_contc <- rep(NA_integer_, nrow(rx))
      rx$NUM_con_con_contc[
        rx[[CONTRADICTION_TYPE]] %in% c("LOGICAL")
      ] <- rx$NUM_con_con[rx[[CONTRADICTION_TYPE]]
        %in% c("LOGICAL")]
    }
    if ("EMPIRICAL" %in% rx[[CONTRADICTION_TYPE]]) {
      rx$PCT_con_con_contu <- rep(NA_real_, nrow(rx))
      rx$PCT_con_con_contu[
        rx[[CONTRADICTION_TYPE]] %in% c("EMPIRICAL")
      ] <- rx$PCT_con_con[rx[[CONTRADICTION_TYPE]]
        %in% c("EMPIRICAL")]
      rx$NUM_con_con_contu <- rep(NA_integer_, nrow(rx))
      rx$NUM_con_con_contu[
        rx[[CONTRADICTION_TYPE]] %in% c("EMPIRICAL")
      ] <- rx$NUM_con_con[rx[[CONTRADICTION_TYPE]]
        %in% c("EMPIRICAL")]
    }

    result$OtherTable <- rx
    # Create Data Slot
    result$OtherData <- rx
    result$OtherData$PCT_con_con_contc <- NULL
    result$OtherData$PCT_con_con_contu <- NULL
    result$OtherData$GRADING <- NULL
    result$OtherData <- util_make_data_slot_from_table_slot(result$OtherData)

    e <- new.env(parent = environment(con_contradictions_redcap))
    e$rx <- rx
    e$meta_data <- meta_data

    cls_rx <- rlang::new_quosure(quote(
      util_make_cls_binding(rx, meta_data = meta_data)
    ), e)

    # Plot for summarized contradiction checks
    # -----------------------------------------------------
    con_con_type_bands <- util_con_contradiction_type_bands(rx)
    con_con_y_limit <- max(c(
      1,
      1.2 * rx$PCT_con_con,
      threshold_value
    ), na.rm = TRUE)

    p <- util_create_lean_ggplot(
      ggplot(rx, aes(
        x = seq_along(CONTRADICTION_TYPE), y = PCT_con_con,
        fill = (if (!is.na(threshold_value)) {
          as.ordered(GRADING)
        } else {
          !!cls_rx
        })
      )) +
        ggplot2::geom_rect(
          data = con_con_type_bands,
          aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
          inherit.aes = FALSE,
          fill = con_con_type_bands$fill,
          alpha = 0.35,
          color = NA
        ) +
        geom_bar(stat = "identity") +
        theme_minimal() +
        scale_y_continuous(
          name = "(%)",
          limits = c(0, con_con_y_limit)
          # Historical expansion padding was removed in commit 01e0267432.
        ) +
        scale_x_continuous(
          breaks = seq_len(nrow(rx)),
          labels = rx[[CONTRADICTION_TYPE]]
          # Historical top/reverse axis options removed in commit 01e0267432.
        ) +
        # Historical descriptive x-axis label removed in commit 01e0267432.
        xlab("") +
        (if (!is.na(threshold_value)) {
          scale_fill_manual(values = cols, name = " ", guide = "none")
        } else {
          util_scale_fill_dataquieR()
        }) +
        (if (!is.na(threshold_value)) {
          geom_hline(
            yintercept = threshold_value,
            color = "red", linetype = 2
          )
        }) +
        geom_text(
          label = util_paste0_with_na(" ", rx$PCT_con_con, "%"),
          hjust = 0, vjust = 0.5, size = 3.5,
          show.legend = FALSE
        ) +
        geom_text(
          data = con_con_type_bands,
          aes(
            x = xmid,
            y = 0.97 * con_con_y_limit,
            label = display_label
          ),
          inherit.aes = FALSE,
          angle = 90,
          hjust = 0.5,
          vjust = 1,
          size = 3.3,
          show.legend = FALSE
        ) +
        coord_flip() +
        theme(
          axis.text.x = element_text(size = 10),
          axis.text.y.right = element_text(size = 10),
          axis.text.y.left = element_text(size = 10),
          legend.title = element_blank()
        ),
      rx = rx,
      cls_rx = cls_rx,
      con_con_type_bands = con_con_type_bands,
      con_con_y_limit = con_con_y_limit,
      threshold_value = threshold_value,
      cols = cols
    )

    # Historical lazy coordinate-flip experiment removed in commit c48791d1e3.

    # Estimate sizing without building the plot. Building evaluates the lazy
    # grading quosure and can bind rule-set state before rendering.
    w <- 2 * nrow(rx)
    if (w == 0) {
      w <- 10
    }
    label_width <- max(nchar(as.character(rx[[CONTRADICTION_TYPE]])),
      na.rm = TRUE
    )
    if (!is.finite(label_width)) {
      label_width <- 0
    }
    w <- w + 2 +
      label_width
    h <- 2 * nrow(rx)
    if (h == 0) {
      h <- 10
    }
    h <- h + 15

    p <- util_set_size(p, width_em = w, height_em = h)

    result$SummaryPlot <- p

    to_other <- setdiff(names(result), c(
      "SummaryData",
      "OtherTable",
      "OtherData",
      "SummaryPlot"
    ))

    Other <- result[to_other]

    result[to_other] <- NULL

    result$Other <- Other


    return(util_attach_attr(
      result,
      as_plotly = "util_as_plotly_con_contradictions_redcap",
      contradiction_type_bands = con_con_type_bands,
      sizing_hints = list(
        figure_type_id = "bar_chart",
        rotated = TRUE,
        number_of_bars = nrow(p$data),
        range = max(p$data$PCT_con_con) - min(p$data$PCT_con_con)
      )
    ))
  } else {
    # run contradiction checks without summarizing
    # -------------------------------------------------------
    # apply contradiction checks
    # -------------------------------------------------------------------------
    rule_preparation <- lapply(
      util_parse_assignments(
        meta_data_cross_item[[DATA_PREPARATION]],
        multi_variate_text = TRUE
      ),
      function(prep) {
        prep <- as.character(names(prep))
        use_value_labels <- ("LABEL" # meta_data_cross_item has been normalized, already # nolint: line_length_linter.
          %in% prep)
        if ("MISSING_NA" %in% prep) {
          replace_missing_by <- "NA"
        } else if ("MISSING_LABEL" %in% prep) {
          replace_missing_by <- "LABEL"
        } else if ("MISSING_INTERPRET" %in% prep) {
          replace_missing_by <- "INTERPRET"
        } else {
          replace_missing_by <- ""
        }
        replace_limits <- ("LIMITS" %in% prep)
        if (replace_limits && replace_missing_by != "NA") {
          util_message(
            c(
              "Cannot replace hard limits, if missing codes are not deleted. I will", # nolint: line_length_linter.
              "therefore replace the missing codes by NA, too."
            ),
            applicability_problem = TRUE
          )
          replace_missing_by <- "NA"
        }
        list(
          use_value_labels = use_value_labels,
          replace_missing_by = replace_missing_by,
          replace_limits = replace_limits
        )
      }
    )

    rule_preparation_key <- vapply(rule_preparation, function(prep) {
      paste(prep$use_value_labels, prep$replace_missing_by,
        prep$replace_limits,
        sep = "\r"
      )
    }, FUN.VALUE = character(1))

    prepared_ds1 <- lapply(
      setNames(unique(rule_preparation_key), unique(rule_preparation_key)),
      function(prep_key) {
        prep <- rule_preparation[[match(prep_key, rule_preparation_key)]]
        prep_prepare_dataframes(
          .replace_hard_limits = prep$replace_limits,
          .replace_missings = (prep$replace_missing_by == "NA"),
          .study_data = ds1,
          .meta_data = meta_data,
          .label_col = label_col
        )
      }
    )

    rule_match <- mapply(
      SIMPLIFY = FALSE,
      rule = compiled_rules,
      prep = rule_preparation,
      prep_key = rule_preparation_key,
      FUN = function(rule, prep, prep_key) {
        if (is.list(rule) && !length(rule) &&
            is.null(util_attr(rule, "class", exact = TRUE))) {
          r <- try(util_error("Parser error"), silent = TRUE)
        } else {
          r <- try(
            util_eval_rule(
              rule = rule,
              ds1 = prepared_ds1[[prep_key]],
              meta_data = meta_data,
              use_value_labels = prep$use_value_labels,
              replace_missing_by = prep$replace_missing_by,
              replace_limits = prep$replace_limits
            ),
            silent = TRUE
          )
        }
        if (inherits(r, "try-error")) {
          rule_src <- util_attr(rule, "src", exact = TRUE)
          if (length(rule_src) == 0) {
            rule_src <- util_deparse1(rule)
          }
          util_warning(
            "Could not evaluate rule %s: %s",
            dQuote(rule_src),
            conditionMessage(util_attr(r, "condition",
                exact = TRUE
              ))
          )
          r <- "error"
        }
        r
      }
    )

    rule_errors <- vapply(rule_match, identical, "error",
      FUN.VALUE = logical(1)
    )
    rule_match <- lapply(rule_match, as.logical)

    list_element_length <- vapply(rule_match, length, FUN.VALUE = integer(1))
    if (any(list_element_length == 1)) {
      # not all columns of same length, fix this for as.data.frame
      rule_match[list_element_length == 1] <- lapply(
        rule_match[list_element_length == 1],
        function(to_recycle) {
          rep(to_recycle, nrow(ds1))
        }
      )
    }

    if (length(unique(vapply(rule_match, length, FUN.VALUE = integer(1)))) > 1) { # nolint: line_length_linter.
      util_error(c(
        "Internal error: unexpected inhomogeneous length of rules result.",
        "This is an internal error, please excuse and contact the dataquieR developers." # nolint: line_length_linter.
      ))
    }

    if (length(unique(vapply(rule_match, length, FUN.VALUE = integer(1)))) == 0) { # nolint: line_length_linter.
      summary_df1 <- data.frame(Obs = seq_len(nrow(ds1)))
    } else {
      summary_df1 <- cbind(
        data.frame(Obs = seq_len(nrow(ds1))),
        as.data.frame(rule_match)
      )
    }

    colnames(summary_df1)[-1] <- paste0(
      "flag_con",
      formatC(seq_len(nrow(meta_data_cross_item)),
        width = nchar(nrow(meta_data_cross_item)),
        format = "d",
        flag = "0"
      )
    )

    summary_df2 <- meta_data_cross_item

    summary_df2$NUM_con_con <- as.numeric(lapply(rule_match, sum, na.rm = TRUE))
    summary_df2$NUM_con_con[rule_errors] <- rep(NA_integer_, sum(rule_errors))

    summary_df2$PCT_con_con <- round(summary_df2$NUM_con_con / nrow(ds1) * 100,
      digits = 2
    )


    if (CONTRADICTION_TYPE %in% colnames(summary_df2)) {
      summary_df2[["CONTRADICTION_TYPE"]] <-
        trimws(toupper(summary_df2[["CONTRADICTION_TYPE"]]))
      # logical
      summary_df2$NUM_con_con_contc <- rep(NA_integer_, nrow(summary_df2))
      summary_df2$NUM_con_con_contc[
        summary_df2[["CONTRADICTION_TYPE"]] %in% c("LOGICAL")
      ] <- summary_df2$NUM_con_con[summary_df2[["CONTRADICTION_TYPE"]]
        %in% c("LOGICAL")]
      summary_df2$PCT_con_con_contc <- rep(NA_real_, nrow(summary_df2))
      summary_df2$PCT_con_con_contc[
        summary_df2[["CONTRADICTION_TYPE"]] %in% c("LOGICAL")
      ] <- summary_df2$PCT_con_con[summary_df2[["CONTRADICTION_TYPE"]]
        %in% c("LOGICAL")]


      summary_df2$NUM_con_con_contu <- rep(NA_integer_, nrow(summary_df2))
      summary_df2$NUM_con_con_contu[
        summary_df2[["CONTRADICTION_TYPE"]] %in% c("EMPIRICAL")
      ] <- summary_df2$NUM_con_con[summary_df2[["CONTRADICTION_TYPE"]]
        %in% c("EMPIRICAL")]
      summary_df2$PCT_con_con_contu <- rep(NA_real_, nrow(summary_df2))
      summary_df2$PCT_con_con_contu[
        summary_df2[[CONTRADICTION_TYPE]] %in% c("EMPIRICAL")
      ] <- summary_df2$PCT_con_con[summary_df2[[CONTRADICTION_TYPE]]
        %in% c("EMPIRICAL")]
    } else {
      summary_df2[["CONTRADICTION_TYPE"]] <- rep(NA_character_, nrow(summary_df2)) # nolint: line_length_linter.
    }

    summary_df2$GRADING <- ifelse(summary_df2$PCT_con_con > threshold_value,
      1, 0
    )

    summary_df2 <- summary_df2[order(summary_df2[["PCT_con_con"]],
        decreasing = TRUE
      ), , drop = FALSE]
    summary_df2 <- summary_df2[order(summary_df2[[CONTRADICTION_TYPE]],
        decreasing = TRUE
      ), , drop = FALSE]

    summary_df2_for_return <- summary_df2

    summary_df2 <- summary_df2[rev(seq_len(nrow(summary_df2))), , drop = FALSE]


    e <- new.env(parent = environment(con_contradictions_redcap))
    e$summary_df2 <- summary_df2
    e$meta_data <- meta_data

    cls_summary_df2 <- rlang::new_quosure(quote(
      util_make_cls_binding(summary_df2, meta_data = meta_data)
    ), e)

    ctype_pal <- setNames( # does not really depend on grading formats
      scales::pal_hue(h = c(0, 360))(n =
          length(unique(summary_df2[[CONTRADICTION_TYPE]]))
      ),
      nm = unique(summary_df2[[CONTRADICTION_TYPE]])
    )
    ctype_pal[["LOGICAL"]] <- getOption(
      "dataquieR.col_con_con_logical",
      dataquieR.col_con_con_logical_default
    )
    ctype_pal[["EMPIRICAL"]] <- getOption(
      "dataquieR.col_con_con_empirical",
      dataquieR.col_con_con_empirical_default
    )
    # Plot for all contradiction checks
    # --------------------------------------------------------
    con_con_type_bands <- util_con_contradiction_type_bands(summary_df2)
    con_con_y_limit <- max(c(
      1,
      1.2 * summary_df2$PCT_con_con,
      threshold_value
    ), na.rm = TRUE)

    p <- util_create_lean_ggplot(
      ggplot(summary_df2, aes(
        x = seq_along(CHECK_ID),
        y = PCT_con_con,
        color = CONTRADICTION_TYPE,
        fill = (if (!is.na(threshold_value)) {
          as.ordered(GRADING)
        } else {
          !!cls_summary_df2
        })
      )) +
        ggplot2::geom_rect(
          data = con_con_type_bands,
          aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
          inherit.aes = FALSE,
          fill = con_con_type_bands$fill,
          alpha = 0.35,
          color = NA
        ) +
        geom_bar(stat = "identity") +
        theme_minimal() +
        # xlab("Applied contradiction checks") +
        xlab("") +
        scale_y_continuous(
          name = "(%)",
          limits = c(0, con_con_y_limit),
          expand = expansion(
            mult = c(0, 0.1)
          )
        ) +
        scale_x_continuous(
          breaks = seq_len(nrow(summary_df2)),
          labels = summary_df2$CHECK_LABEL
        ) +
        ggplot2::scale_color_discrete(type = ctype_pal) +
        (if (!is.na(threshold_value)) {
          scale_fill_manual(values = cols, name = " ", guide = "none")
        } else {
          util_scale_fill_dataquieR()
        }) +
        (if (!is.na(threshold_value)) {
          geom_hline(
            yintercept = threshold_value,
            color = "red", linetype = 2
          )
        }) +
        geom_text(
          label = util_paste0_with_na(" ", summary_df2$PCT_con_con, "%"),
          hjust = 0, vjust = 0.5, size = 3.5,
          show.legend = FALSE
        ) +
        geom_text(
          data = con_con_type_bands,
          aes(
            x = xmid,
            y = 0.97 * con_con_y_limit,
            label = display_label
          ),
          inherit.aes = FALSE,
          angle = 90,
          hjust = 0.5,
          vjust = 1,
          size = 3.3,
          show.legend = FALSE
        ) +
        coord_flip() +
        theme(
          axis.text.x = element_text(size = 10),
          axis.text.y.right = element_text(size = 10),
          axis.text.y.left = element_text(size = 10),
          legend.title = element_blank()
        ),
      summary_df2 = summary_df2,
      cls_summary_df2 = cls_summary_df2,
      con_con_type_bands = con_con_type_bands,
      con_con_y_limit = con_con_y_limit,
      ctype_pal = ctype_pal,
      threshold_value = threshold_value,
      cols = cols
    )

    if (!prod(dim(summary_df2))) {
      util_error("No contradiction check defined",
        applicability_problem = TRUE,
        intrinsic_applicability_problem = TRUE
      )
    }

    # create Data Slot
    summary_df3 <- summary_df2_for_return[, intersect(c(
      VARIABLE_LIST, CHECK_LABEL,
      "NUM_con_con",
      "PCT_con_con",
      CONTRADICTION_TYPE,
      "GRADING"
    ), colnames(summary_df2_for_return)), drop = FALSE]
    summary_df3 <- util_make_data_slot_from_table_slot(summary_df3)

    # Add sizing information without building the plot. Building evaluates the
    # lazy grading quosure and can bind rule-set state before rendering.
    number_of_bars <- nrow(summary_df2)
    range_values <- suppressWarnings(as.numeric(summary_df2$PCT_con_con))
    if (!is.na(threshold_value)) {
      range_values <- c(range_values, threshold_value)
    }
    range <- max(range_values, na.rm = TRUE) -
      min(range_values, na.rm = TRUE)
    if (!is.finite(range)) {
      range <- 0
    }

    no_char_vars <- max(c(0, nchar(summary_df2$CHECK_LABEL)), na.rm = TRUE)
    no_char_numbers <- max(c(0, nchar(round(range_values, digits = 0))),
      na.rm = TRUE
    )

    # VARIABLE_LIST_ORDER is used only internally for computed variables.
    summary_df2$VARIABLE_LIST_ORDER <- NULL
    summary_df2_for_return$VARIABLE_LIST_ORDER <- NULL

    # Add new attribute to the columns of VariableGroupData to define the
    # datatype of each column
    summary_df3 <- util_fix_datatype_for_table(summary_df3)

    # Output
    return(util_attach_attr(
      list(
        FlaggedStudyData = summary_df1,
        VariableGroupTable = summary_df2_for_return,
        VariableGroupData = summary_df3,
        SummaryPlot = p
      ),
      as_plotly = "util_as_plotly_con_contradictions_redcap",
      contradiction_type_bands = con_con_type_bands,
      sizing_hints = list(
        figure_type_id = "bar_chart",
        rotated = TRUE,
        number_of_bars = number_of_bars,
        range = range,
        no_char_vars = no_char_vars,
        no_char_numbers = no_char_numbers
      )
    ))
  }


  # Add new attribute to the columns of VariableGroupData to define the
  # datatype of each column
  summary_df3 <- util_fix_datatype_for_table(summary_df3)

  # Never called, just for documentation.
  return(list( # nocov start
    FlaggedStudyData = summary_df1,
    VariableGroupTable = summary_df2,
    VariableGroupData = summary_df3,
    SummaryPlot = p
  )) # nocov end
}

util_reshape_for_cls_binding <- local({
  cache_env <- new.env(parent = emptyenv())
  function(rx, idvars) {
    metric_names <- c(
      "PCT_con_con",
      "PCT_con_con_contc",
      "PCT_con_con_contu",
      "NUM_con_con",
      "NUM_con_con_contc",
      "NUM_con_con_contu"
    )
    missing_metric_names <- setdiff(metric_names, colnames(rx))
    for (metric_name in missing_metric_names) {
      rx[[metric_name]] <- rep(NA_real_, nrow(rx))
    }

    key <- rlang::hash(list(rx, idvars))
    if (exists(key, envir = cache_env, inherits = FALSE)) {
      return(get(key, envir = cache_env, inherits = FALSE))
    }
    reshaped <- stats::reshape(
      data = rx,
      timevar = "indicator_metric",
      idvar = idvars,
      times = metric_names,
      varying = list(metric_names),
      v.names = "values_raw",
      direction = "long"
    )
    assign(key, reshaped, envir = cache_env)
    reshaped
  }
})

util_make_cls_binding <- local({
  gg_cache_env <- new.env(parent = emptyenv())
  function(rx, meta_data) {
    rs <- util_get_rule_sets()
    rf <- util_get_ruleset_formats()

    key <- paste0("KEY_", rlang::hash(list(rs, rf, rx, meta_data)))
    if (exists(key, envir = gg_cache_env)) {
      return((get(key, envir = gg_cache_env)))
    }

    grading_labs <- util_get_labels_grading_class()
    grading_colors <- util_get_colors()
    grading_colors["NA"] <- "#888888"
    grading_rules <- rs[["0"]]

    if (is.data.frame(grading_rules) &&
        is.character(grading_labs) &&
        length(grading_labs) > 0) {
      rx$.orig_order <- seq_len(nrow(rx))
      if (!(GRADING_RULESET %in% colnames(rx))) {
        rx[[GRADING_RULESET]] <- "0"
      }
      rx[[GRADING_RULESET]] <- trimws(as.character(rx[[GRADING_RULESET]]))
      rx[[GRADING_RULESET]][util_empty(rx[[GRADING_RULESET]])] <- "0"
      rx_plot <- rx
      row_rulesets <- strsplit(rx[[GRADING_RULESET]], SPLIT_CHAR, fixed = TRUE)
      row_rulesets <- lapply(row_rulesets, function(rulesets) {
        rulesets <- trimws(rulesets)
        rulesets <- rulesets[!util_empty(rulesets)]
        if (length(rulesets) == 0) {
          "0"
        } else {
          rulesets
        }
      })
      rx <- rx[rep(seq_len(nrow(rx)), lengths(row_rulesets)), , drop = FALSE]
      rx[[GRADING_RULESET]] <- unlist(row_rulesets, use.names = FALSE)
      rx$.grading_row_id <- paste0(
        "con_contradictions_redcap.",
        rx$.orig_order,
        ".",
        ave(rx$.orig_order, rx$.orig_order, FUN = seq_along)
      )

      idvars <- intersect(
        c(
          VARIABLE_LIST, CHECK_LABEL,
          CHECK_ID, ".orig_order"
        ),
        colnames(rx)
      )
      summ <- util_reshape_for_cls_binding(rx, c(idvars, ".grading_row_id"))
      summ$call_names <- paste0("con_contradictions_redcap")
      summ$function_name <- paste0("con_contradictions_redcap")
      summ[[VAR_NAMES]] <- summ$.grading_row_id
      grading_meta_data <- unique(rx[, c(".grading_row_id", GRADING_RULESET),
          drop = FALSE
        ])
      colnames(grading_meta_data)[colnames(grading_meta_data) ==
          ".grading_row_id"] <- VAR_NAMES
      summ <- util_metrics_to_classes(
        summ,
        grading_meta_data,
        entity = "CROSS_ITEM"
      )
      cls <- summ[, c(idvars, "class"), drop = FALSE]
      cls_key <- do.call(paste, c(cls[, idvars, drop = FALSE], sep = "\r"))
      cls <- unsplit(lapply(
        split(cls, cls_key),
        FUN =
          function(x) {
            if (any(!is.na(as.numeric(x$class)))) {
              x$class <- max(as.numeric(x$class), na.rm = TRUE)
            } else {
              x$class <- rep(NA_integer_, nrow(x))
            }
            x
          }
      ), cls_key)
      cls <- unique(cls)
      rx <- merge(rx_plot, cls, by = idvars, sort = FALSE)
      # Do not apply `unique()` here; it changes the order.
      rx$class <- grading_colors[paste(rx$class)]
      # Historical factor-based color-class prototype removed here.
      rx <- rx[order(rx[[".orig_order"]]), , drop = FALSE]
      res <- rx$class
    } else {
      res <- NA
    }
    assign(key, res, envir = gg_cache_env)
    return(res)
  }
})

#' Internal helper: scale fill dataquieR
#'
#' @noRd
util_scale_fill_dataquieR <- function(...) {
  r <- ggplot2::scale_fill_identity(
    name = " ",
    labels = setNames(
      util_get_labels_grading_class(),
      util_get_colors()
    ),
    guide = ggplot2::guide_legend()
  )
  r$get_labels <- function(x, ..., self) {
    p <- setNames(
      nm = util_get_colors(),
      util_get_labels_grading_class()
    )
    p[x]
  }
  r
}

#' Internal helper: con contradiction type bands
#'
#' @noRd
util_con_contradiction_type_bands <- function(x) {
  n <- nrow(x)
  if (!CONTRADICTION_TYPE %in% names(x) || n == 0) {
    return(data.frame(
      xmin = numeric(0),
      xmax = numeric(0),
      xmid = numeric(0),
      contradiction_type = character(0),
      display_label = character(0),
      display_label_full = character(0),
      fill = character(0),
      fill_plotly = character(0),
      stringsAsFactors = FALSE
    ))
  }

  types <- as.character(x[[CONTRADICTION_TYPE]])
  types[is.na(types) | !nzchar(types)] <- "NA"
  runs <- rle(types)
  ends <- cumsum(runs$lengths)
  starts <- ends - runs$lengths + 1
  labels <- vapply(runs$values, util_con_contradiction_type_label,
    FUN.VALUE = character(1)
  )
  display_labels <- mapply(util_con_contradiction_type_display_label,
    runs$values, runs$lengths,
    USE.NAMES = FALSE
  )
  fill <- vapply(runs$values, util_con_contradiction_type_band_fill,
    FUN.VALUE = character(1)
  )
  fill_plotly <- vapply(runs$values,
    util_con_contradiction_type_band_fill_plotly,
    FUN.VALUE = character(1)
  )

  data.frame(
    xmin = starts - 0.5,
    xmax = ends + 0.5,
    xmid = (starts + ends) / 2,
    contradiction_type = runs$values,
    display_label = display_labels,
    display_label_full = labels,
    fill = fill,
    fill_plotly = fill_plotly,
    stringsAsFactors = FALSE
  )
}

#' Internal helper: con contradiction type label
#'
#' @noRd
util_con_contradiction_type_label <- function(x) {
  if (identical(x, "EMPIRICAL")) {
    return("Empirical contradictions")
  }
  if (identical(x, "LOGICAL")) {
    return("Logical contradictions")
  }
  if (is.na(x) || !nzchar(x)) {
    return("Contradictions")
  }
  paste0(
    toupper(substr(x, 1, 1)),
    tolower(substr(x, 2, nchar(x))),
    " contradictions"
  )
}

#' Internal helper: con contradiction type display label
#'
#' @noRd
util_con_contradiction_type_display_label <- function(x, n_rows) {
  if (n_rows <= 2) {
    if (identical(x, "EMPIRICAL")) {
      return("Emp.")
    }
    if (identical(x, "LOGICAL")) {
      return("Log.")
    }
  }
  if (n_rows < 8) {
    if (identical(x, "EMPIRICAL")) {
      return("Empirical")
    }
    if (identical(x, "LOGICAL")) {
      return("Logical")
    }
  }
  util_con_contradiction_type_label(x)
}

#' Internal helper: con contradiction type band fill
#'
#' @noRd
util_con_contradiction_type_band_fill <- function(x) {
  if (identical(x, "LOGICAL")) {
    return("#FAD9D9")
  }
  if (identical(x, "EMPIRICAL")) {
    return("#D9D9D9")
  }
  "#E8E8E8"
}

#' Internal helper: con contradiction type band fill plotly
#'
#' @noRd
util_con_contradiction_type_band_fill_plotly <- function(x) {
  if (identical(x, "LOGICAL")) {
    return("rgba(250,217,217,0.35)")
  }
  if (identical(x, "EMPIRICAL")) {
    return("rgba(217,217,217,0.35)")
  }
  "rgba(232,232,232,0.35)"
}

#' Internal helper: con con add plotly type bands
#'
#' @noRd
util_con_con_add_plotly_type_bands <- function(py, bands) {
  if (is.null(bands) || !nrow(bands)) {
    return(py)
  }

  yaxis_range <- NULL
  if (!is.null(py$x$layout$yaxis) && !is.null(py$x$layout$yaxis$range)) {
    yaxis_range <- as.numeric(py$x$layout$yaxis$range)
  }
  if (is.null(yaxis_range) || any(is.na(yaxis_range))) {
    yaxis_range <- c(
      min(bands$xmin, na.rm = TRUE),
      max(bands$xmax, na.rm = TRUE)
    )
  }
  y_span <- yaxis_range[2] - yaxis_range[1]
  if (!is.finite(y_span) || y_span <= 0) {
    return(py)
  }

  y_domain <- c(0, 1)
  if (!is.null(py$x$layout$yaxis) && !is.null(py$x$layout$yaxis$domain)) {
    y_domain <- as.numeric(py$x$layout$yaxis$domain)
  }
  map_to_paper <- function(y) {
    rel <- (y - yaxis_range[1]) / y_span
    y_domain[1] + rel * (y_domain[2] - y_domain[1])
  }

  shapes <- lapply(seq_len(nrow(bands)), function(i) {
    list(
      type = "rect",
      xref = "paper",
      x0 = 0,
      x1 = 1,
      yref = "paper",
      y0 = map_to_paper(bands$xmin[i]),
      y1 = map_to_paper(bands$xmax[i]),
      fillcolor = bands$fill_plotly[i],
      line = list(width = 0),
      layer = "below"
    )
  })

  annotations <- lapply(seq_len(nrow(bands)), function(i) {
    list(
      x = 0.98,
      xref = "paper",
      y = map_to_paper(bands$xmid[i]),
      yref = "paper",
      text = bands$display_label[i],
      showarrow = FALSE,
      xanchor = "right",
      yanchor = "middle",
      textangle = 90,
      font = list(size = 12, color = "rgba(0,0,0,0.85)")
    )
  })

  if (is.null(py$x$layout$margin)) {
    py$x$layout$margin <- list()
  }
  old_margin <- suppressWarnings(as.numeric(py$x$layout$margin$r))
  if (!length(old_margin) || !is.finite(old_margin)) {
    old_margin <- 0
  }
  py$x$layout$margin$r <- max(160, old_margin)
  py$x$layout$shapes <- c(py$x$layout$shapes, shapes)
  py$x$layout$annotations <- c(py$x$layout$annotations, annotations)
  py$x$layout$paper_bgcolor <- "rgba(0,0,0,0)"
  py$x$layout$plot_bgcolor <- "rgba(0,0,0,0)"
  py
}

#' Internal helper: con con remove plotly type label traces
#'
#' @noRd
util_con_con_remove_plotly_type_label_traces <- function(py, bands) {
  if (is.null(bands) || !nrow(bands) || !length(py$x$data)) {
    return(py)
  }

  type_labels <- bands$display_label
  keep <- vapply(py$x$data, function(trace) {
    if (!identical(trace$type, "scatter") ||
        is.null(trace$mode) ||
        !grepl("text", trace$mode) ||
        is.null(trace$text)) {
      return(TRUE)
    }
    !all(as.character(trace$text) %in% type_labels)
  }, FUN.VALUE = logical(1))
  py$x$data <- py$x$data[keep]
  py
}

#' Internal helper: con con clean plotly hover
#'
#' @noRd
util_con_con_clean_plotly_hover <- function(py) {
  for (i in seq_along(py$x$data)) {
    tr <- py$x$data[[i]]
    if (!identical(tr$type, "bar")) {
      next
    }
    src <- if (!is.null(tr$hovertext)) tr$hovertext else tr$text
    if (is.null(src)) {
      next
    }
    py$x$data[[i]]$hovertext <- vapply(as.character(src),
      util_con_con_short_hover,
      FUN.VALUE = character(1)
    )
    py$x$data[[i]]$hoverinfo <- "text"
  }
  py
}

#' Internal helper: con con short hover
#'
#' @noRd
util_con_con_short_hover <- function(x) {
  pct <- util_con_con_extract_hover_field(x, "PCT_con_con")
  ctype <- util_con_con_extract_hover_field(x, CONTRADICTION_TYPE)
  ctype <- gsub(" contradictions$", "",
    util_con_contradiction_type_label(toupper(ctype))
  )
  if (is.na(pct) || !nzchar(pct)) {
    return(ctype)
  }
  pct <- gsub("%", "", pct, fixed = TRUE)
  pct <- gsub(",", ".", pct, fixed = TRUE)
  pct_num <- suppressWarnings(as.numeric(pct))
  if (is.na(pct_num)) {
    return(ctype)
  }
  paste(ctype, "-", sprintf("%.2f%%", pct_num))
}

#' Internal helper: con con extract hover field
#'
#' @noRd
util_con_con_extract_hover_field <- function(x, field_name) {
  pattern <- paste0("(?i)", field_name, "\\s*:\\s*([^<\\n\\r]+)")
  match <- regexec(pattern, x, perl = TRUE)
  value <- regmatches(x, match)[[1]]
  if (length(value) < 2) {
    return(NA_character_)
  }
  trimws(value[2])
}

#' @family plotly_shims
#' @concept plotly_shims
#' @author Thomas J. Musholt contributed the Plotly readability approach for
#'   contradiction-type bands, annotations, and hover text.
#' @noRd
util_as_plotly_con_contradictions_redcap <- function(res, ...) {
  if (!util_ensure_suggested("plotly", err = FALSE)) {
    return(htmltools::HTML("No Plotly"))
  }
  # Maybe, we have Other, but certainly, we have SummaryPlot
  # fix the Legend
  col_map <- setNames(nm = util_get_colors(), util_get_labels_grading_class())
  col_map["#888888"] <- "NA"
  # hline, no legnedn from .. or guide hidden --> no legend needed, legacy mode
  # with threshold
  all_geoms <- lapply(util_gg_get(res$SummaryPlot, "layers"), `[[`, "geom")
  vlines <- vapply(all_geoms, inherits, "GeomVline",
    FUN.VALUE = logical(1)
  )
  hlines <- vapply(all_geoms, inherits, "GeomHline",
    FUN.VALUE = logical(1)
  )
  legacy_mode <- !!sum(vlines, hlines)
  py <- util_plot_figure_plotly(
    res[["SummaryPlot"]],
    util_attr(res, "sizing_hints", exact = TRUE)
  )
  type_bands <- util_attr(res, "contradiction_type_bands", exact = TRUE)
  py <- util_con_con_clean_plotly_hover(py)
  py <- util_con_con_remove_plotly_type_label_traces(py, type_bands)
  py <- util_con_con_add_plotly_type_bands(
    py,
    type_bands
  )
  if (legacy_mode) { # no legend in gg, legend static, here
    if (any(vlines)) {
      all_geoms[vlines][[1]]$parameters
      thr <- util_gg_get(res$SummaryPlot, "layers")[vlines][[1]]$data$xintercept
    } else {
      all_geoms[hlines][[1]]$parameters
      thr <- util_gg_get(res$SummaryPlot, "layers")[hlines][[1]]$data$yintercept
    }
    col_map_yn <- c(
      "1" = sprintf("> %.4g%%", thr),
      "0" = sprintf("\u2264 %.4g%%", thr)
    )
    for (i in seq_along(py$x$data)) {
      py$x$data[[i]]$name <- lapply(
        strsplit(gsub("^\\((.*)\\)$", "\\1", py$x$data[[i]]$name), split = ","),
        function(scales) {
          scales[scales %in% names(col_map_yn)] <-
            col_map_yn[scales[scales %in% names(col_map_yn)]]
          paste0("(", paste0(scales, collapse = ","), ")")
        }
      )
    }
  } else {
    for (i in seq_along(py$x$data)) {
      py$x$data[[i]]$name <- lapply(
        strsplit(gsub("^\\((.*)\\)$", "\\1", py$x$data[[i]]$name), split = ","),
        function(scales) {
          scales[scales %in% names(col_map)] <-
            col_map[scales[scales %in% names(col_map)]]
          paste0("", paste0(scales, collapse = ", "), "")
        }
      )
    }
  }
  py
}
