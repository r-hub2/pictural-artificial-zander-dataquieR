# nolint start: line_length_linter.
#' Summarizes missingness for individuals in specific segments
#'
#' @description
#' ### This implementation can be applied in two use cases:
#'
#'  1. participation in study segments is not recorded by respective variables,
#'     e.g. a participant's refusal to attend a specific examination is not
#'     recorded.
#'  2. participation in study segments is recorded by respective
#'     variables.
#'
#' Use case *(1)* will be common in smaller studies. For the calculation of
#' segment missingness it is assumed that study variables are nested in
#' respective segments. This structure must be specified in the static metadata.
#' The R-function identifies all variables within each segment and returns TRUE
#' if all variables within a segment are missing, otherwise FALSE.
#'
#' Use case *(2)* assumes a more complex structure of study data and metadata.
#' The study data comprise so-called intro-variables (either TRUE/FALSE or codes
#' for non-participation). The column `PART_VAR` in the metadata is
#' filled by variable-IDs indicating for each variable the respective
#' intro-variable. This structure has the benefit that subsequent calculation of
#' item missingness obtains correct denominators for the calculation of
#' missingness rates.
#'
#' [Descriptor]
#'
#' @details
#' ### Implementation and use of thresholds
#' Without an explicit `threshold_value`, segment-missingness percentages are
#' colored using the grading rules. An explicitly supplied threshold activates
#' the legacy binary threshold display. If the direction is `above`, values
#' below the threshold are normal and values above the threshold are critical.
#' For `below`, the interpretation is reversed.
#'
#' ### Hint
#' This function does not support a `resp_vars` argument but `exclude_roles` to
#' specify variables not relevant for detecting a missing segment.
#'
#' List function.
#'
#' @inheritParams .template_function_indicator
#'
#' @param strata_vars [variable] the name of a variable used for stratification,
#'                               defaults to NULL for not grouping output
#' @param meta_data_segment [data.frame] Segment level metadata. Optional.
#'   [GRADING_RULESET] selects the ruleset for each [STUDY_SEGMENT]; missing
#'   assignments use ruleset `0`.
#' @param threshold_value [numeric] from=0 to=100. An optional legacy threshold.
#'   If omitted, grading rules determine the colors.
#' @param direction [enum] low | high. "high" or "low", i.e. are deviations
#'                                     above/below the threshold critical. This argument is deprecated and replaced by *color_gradient_direction*.
#' @param color_gradient_direction [enum] above | below. "above" or "below", i.e. are deviations
#'                                     above or below the threshold critical? (default: above)
#' @param exclude_roles [variable roles] a character (vector) of variable roles
#'                                       not included
#'
#' @param expected_observations [enum] HIERARCHY | ALL | SEGMENT. If ALL, all
#'                                     observations are expected to comprise
#'                                     all study segments. If SEGMENT, the
#'                                     `PART_VAR` is expected to point
#'                                     to a variable with values of 0 and 1,
#'                                     indicating whether the variable was
#'                                     expected to be observed for each data
#'                                     row. If HIERARCHY, this is also
#'                                     checked recursively, so, if a variable
#'                                     points to such a participation variable,
#'                                     and that other variable does has also
#'                                     a `PART_VAR` entry pointing
#'                                     to a variable, the observation of the
#'                                     initial variable is only
#'                                     expected, if both segment variables are
#'                                     1.
#'
#' @return a list with:
#'   - `ResultData`: data frame about segment missingness. Unless an explicit
#'                   legacy threshold is active, it includes the effective
#'                   [GRADING_RULESET] for every segment.
#'   - `SummaryPlot`: visual summary of segment missingness. An explicitly
#'                    supplied legacy threshold is applied to this plot.
#'   - `ReportSummaryTable`: data frame underlying `SummaryPlot` for calls
#'                           without grouping or stratification. Its colors
#'                           follow grading rules or an explicitly supplied
#'                           legacy threshold.
#'
#' @export
#' @seealso
#' [Online Documentation](
#' https://dataquality.qihs.uni-greifswald.de/VIN_com_impl_segment_missingness.html
#' )
# nolint end
com_segment_missingness <- function(study_data,
  item_level = "item_level",
  strata_vars = NULL,
  group_vars = NULL,
  label_col,
  threshold_value,
  direction,
  color_gradient_direction,
  expected_observations = c(
    "HIERARCHY",
    "ALL",
    "SEGMENT"
  ),
  exclude_roles =
    c(VARIABLE_ROLES$PROCESS),
  meta_data = item_level,
  meta_data_v2,
  segment_level,
  meta_data_segment) {
  #########
  # STOPS #
  #########

  util_maybe_load_meta_data_v2()

  util_ck_arg_aliases()

  if (!missing(meta_data_segment)) {
    meta_data_segment <- util_expect_data_frame(meta_data_segment)
  } else {
    meta_data_segment <- data.frame()
  }
  meta_data_segment <- util_ensure_grading_ruleset_metadata(meta_data_segment)

  expected_observations_missing <- missing(expected_observations)
  util_expect_scalar(expected_observations, allow_more_than_one = TRUE)
  expected_observations <- match.arg(expected_observations)
  util_expect_scalar(expected_observations)

  if (expected_observations != "ALL") {
    int_part_vars_structure(
      study_data = study_data,
      meta_data = meta_data,
      label_col = label_col,
      expected_observations = expected_observations,
      disclose_problem_paprt_var_data = FALSE
    )
  }

  use_legacy_threshold <- !missing(threshold_value)
  if (use_legacy_threshold &&
      (length(threshold_value) != 1 ||
          !is.numeric(threshold_value) ||
          is.na(threshold_value) ||
          threshold_value < 0 || threshold_value > 100)) {
    if (!.called_in_pipeline) {
      util_message(
        c(
          "threshold_value should be a single number between 0 and 100.",
          "The invalid threshold is ignored and grading rules are used."
        ),
        applicability_problem = TRUE
      )
    }
    use_legacy_threshold <- FALSE
  }
  if (!use_legacy_threshold) {
    threshold_value <- NA_real_
  }

  if (missing(color_gradient_direction)) {
    color_gradient_direction <- "above"
  }

  if (length(color_gradient_direction) != 1) {
    util_error(
      "Parameter %s, if not missing, should be of length 1, but not %d.",
      dQuote("color_gradient_direction"), length(color_gradient_direction),
      applicability_problem = TRUE
    )
  }

  if (!all(color_gradient_direction %in% c("above", "below"))) {
    util_error(
      "Parameter %s should be either %s or %s, but not %s.",
      dQuote("color_gradient_direction"),
      dQuote("above"), dQuote("below"), dQuote(color_gradient_direction),
      applicability_problem = TRUE
    )
  }

  if (!(missing(direction))) {
    if (direction %in% c("high", "low")) {
      if ((direction == "low" && color_gradient_direction == "above") ||
          (direction == "high" && color_gradient_direction == "below")) {
        util_error(
          "Conflicting options for %s and %s (%s is deprecated).",
          dQuote("color_gradient_direction"),
          dQuote("direction"), dQuote("direction"),
          applicability_problem = TRUE
        )
      } else {
        util_warning("%s is deprecated.", dQuote("direction"),
          applicability_problem = FALSE
        )
      }
    } else {
      util_warning("%s is deprecated.", dQuote("direction"),
        applicability_problem = FALSE
      )
    }
  }


  ####################
  # PREPS AND CHECKS #
  ####################

  # map meta to study
  prep_prepare_dataframes()
  ds1_labelled <- prep_prepare_dataframes(
    .apply_factor_metadata_inadm = TRUE,
    .internal = FALSE
  )

  if (!expected_observations_missing && expected_observations != "ALL" &&
      !(PART_VAR %in% colnames(meta_data))) {
    util_message(
      c(
        "For %s = %s, a column %s is needed in %s. Falling",
        "back to %s = %s."
      ),
      sQuote("expected_observations"),
      dQuote(expected_observations),
      dQuote(PART_VAR),
      sQuote("meta_data"),
      sQuote("expected_observations"),
      dQuote("ALL"),
      applicability_problem = TRUE
    )
    expected_observations <- "ALL"
  }

  # correct variable usage
  util_correct_variable_use("strata_vars",
    allow_null = TRUE,
    need_type = "!float"
  )

  util_correct_variable_use("group_vars",
    allow_null = TRUE,
    need_type = "!float"
  )

  # Historical `exclude_roles` default sketch removed here.

  # should some variables not be considered?
  if (VARIABLE_ROLE %in% names(meta_data)) {
    # a: not all roles specified found in metadata
    if (!(all(exclude_roles %in% meta_data[[VARIABLE_ROLE]]))) {
      if (any(exclude_roles %in% meta_data[[VARIABLE_ROLE]])) {
        util_warning(paste0(
          "Specified VARIABLE_ROLE(s): '",
          exclude_roles[!(exclude_roles %in% meta_data[[VARIABLE_ROLE]])],
          "' was not found in metadata, only: '",
          exclude_roles[exclude_roles %in% meta_data[[VARIABLE_ROLE]]],
          "' is used."
        ), applicability_problem = TRUE)

        exclude_roles <- exclude_roles[exclude_roles %in%
            meta_data[[VARIABLE_ROLE]]]

        which_vars_not <-
          meta_data[[label_col]][meta_data[[VARIABLE_ROLE]] %in%
            c(exclude_roles, VARIABLE_ROLES$SUPPRESS)]
        if (missing(label_col)) {
          which_vars_not <-
            meta_data[[VAR_NAMES]][meta_data[[VARIABLE_ROLE]] %in%
              c(exclude_roles, VARIABLE_ROLES$SUPPRESS)]
        }
        which_vars_not <- setdiff(which_vars_not, strata_vars)
        which_vars_not <- setdiff(which_vars_not, group_vars)
        if (length(intersect(names(ds1), which_vars_not)) > 0) {
          util_message(
            paste0(
              "Study variables: ",
              paste(dQuote(intersect(names(ds1), which_vars_not)),
                collapse = ", "
              ),
              " are not considered due to their VARIABLE_ROLE."
            ),
            applicability_problem = TRUE,
            intrinsic_applicability_problem = TRUE
          )
        }
        ds1 <- ds1[, !(names(ds1) %in% which_vars_not), drop = TRUE]
        ds1_labelled <- ds1_labelled[, !(names(ds1_labelled) %in%
              which_vars_not), drop = TRUE]
      } else {
        exclude_roles <- FALSE
        util_warning(
          c(
            "Specified VARIABLE_ROLE(s) were not found in metadata.",
            "All variables are included here."
          ),
          applicability_problem = TRUE
        )
      }


      # b: all roles are found in metadata
    } else {
      if (missing(exclude_roles)) {
        if (!.called_in_pipeline) {
          util_message(
            c(
              "Formal exclude_roles is used with default: all process variables", # nolint: line_length_linter.
              "are not included here."
            ),
            applicability_problem = TRUE
          )
        }
      }

      which_vars_not <- meta_data[[label_col]][meta_data[[VARIABLE_ROLE]] %in%
        c(
          exclude_roles,
          VARIABLE_ROLES$SUPPRESS
        )]
      if (missing(label_col)) {
        which_vars_not <- meta_data[[VAR_NAMES]][meta_data[[VARIABLE_ROLE]] %in%
          c(
            exclude_roles,
            VARIABLE_ROLES$SUPPRESS
          )]
      }
      which_vars_not <- setdiff(which_vars_not, strata_vars)
      which_vars_not <- setdiff(which_vars_not, group_vars)
      if (length(intersect(names(ds1), which_vars_not)) > 0) {
        util_message(
          paste0(
            "Study variables: ", paste(
              dQuote(intersect(
                names(ds1),
                which_vars_not
              )),
              collapse = ", "
            ),
            " are not considered due to their VARIABLE_ROLE."
          ),
          applicability_problem = TRUE,
          intrinsic_applicability_problem = TRUE
        )
      }
      ds1 <- ds1[, !(names(ds1) %in% which_vars_not), drop = TRUE]
      ds1_labelled <- ds1_labelled[, !(names(ds1_labelled) %in%
            which_vars_not), drop = TRUE]
    }
  } else {
    # since there are no roles defined exclusion is set to false
    exclude_roles <- FALSE
    util_message(
      c(
        "VARIABLE_ROLE has not been defined in the metadata,",
        "therefore all variables within segments are used."
      ),
      applicability_problem = TRUE,
      intrinsic_applicability_problem = TRUE
    )
  }

  # Which segments?
  if (!(STUDY_SEGMENT %in% names(meta_data))) {
    util_error("Metadata do not contain the column STUDY_SEGMENT",
      applicability_problem = TRUE
    )
  }

  meta_data[[STUDY_SEGMENT]][is.na(meta_data[[STUDY_SEGMENT]])] <- ""

  if (VARIABLE_ROLE %in% colnames(meta_data)) {
    seg_names <- meta_data[
      util_empty(meta_data[[VARIABLE_ROLE]]) |
        meta_data[[VARIABLE_ROLE]] !=
          VARIABLE_ROLES$SUPPRESS, # omit segments added on-the-fly as dependencies # nolint: line_length_linter.
      STUDY_SEGMENT,
      drop = TRUE
    ] # Historical LONG_LABEL segment-name fallback removed here.
  } else {
    seg_names <- meta_data[
      , STUDY_SEGMENT,
      drop = TRUE
    ] # Historical LONG_LABEL segment-name fallback removed here.
  }

  if (!(PART_VAR %in% names(meta_data))) {
    if (expected_observations != "ALL") {
      util_warning("Metadata do not contain the column PART_VAR",
        applicability_problem = TRUE
      )
    }
    pv <- seg_names
    pv[!startsWith(pv, "PART_")] <-
      paste0("PART_", pv[!startsWith(pv, "PART_")])
    while (any(pv %in% c(
      meta_data[[VAR_NAMES]],
      meta_data[[LABEL]],
      meta_data[[label_col]]
    ))) {
      pv[pv %in% c(
        meta_data[[VAR_NAMES]],
        meta_data[[LABEL]],
        meta_data[[label_col]]
      )] <-
        paste0("_", pv[pv %in% c(
          meta_data[[VAR_NAMES]],
          meta_data[[LABEL]],
          meta_data[[label_col]]
        )], "_")
    }
    # remove part_vars referring to SSI
    pv[pv == "PART_.COMPUTED__ssi"] <- NA_character_
    meta_data[[PART_VAR]] <- pv
  }

  part_vars <- meta_data[[PART_VAR]]
  names(part_vars) <- seg_names

  keep <- !is.na(part_vars) & !is.na(seg_names)

  part_vars <- part_vars[keep]

  part_vars <- part_vars[!duplicated(part_vars)]

  sn <- seg_names[!is.na(seg_names)]
  sn <- setdiff(sn, ".COMPUTED__ssi") #remove the SSI segment
  sn <- setNames(nm = unique(sn))
  .sn <- sn

  # do not compute segment plots, if not explicitly requested, if only one
  # segment in data
  keep_in_segmiss <- character(0)
  if (all(c(STUDY_SEGMENT, SEGMENT_MISS) %in% colnames(meta_data_segment))) {
    nas <- util_empty(meta_data_segment[[SEGMENT_MISS]])
    not_in_output <-
      util_is_na_0_empty_or_false(meta_data_segment[[SEGMENT_MISS]]) &
      !nas
    must_in_output <-
      !util_is_na_0_empty_or_false(meta_data_segment[[SEGMENT_MISS]])
    auto <- meta_data_segment[nas, STUDY_SEGMENT, drop = TRUE]
    remove <- meta_data_segment[not_in_output, STUDY_SEGMENT, drop = TRUE]
    keep_in_segmiss <- meta_data_segment[must_in_output,
      STUDY_SEGMENT,
      drop = TRUE
    ]
    sn <- union(keep_in_segmiss, setdiff(sn, remove))
  }

  sn <- intersect(.sn, sn)

  sn <- sn[!is.na(sn)]
  sn <- setNames(nm = unique(sn))

  if (length(sn) < 2 && length(keep_in_segmiss) == 0) {
    util_error("No segment missingness plot for fewer than two segments",
      applicability_problem = TRUE,
      intrinsic_applicability_problem = TRUE
    )
  }

  # determine which vars per segment
  var_sets <- lapply(sn, util_get_vars_in_segment,
    meta_data = meta_data,
    label_col = label_col
  )

  # remove all part_vars
  if (TRUE) {
    remove_part_vars <- function(vars, meta_data, label_col) {
      v <- util_map_labels(vars, meta_data, to = VAR_NAMES, from = label_col)
      vars[!(v %in% part_vars)]
    }
    var_sets <- lapply(var_sets, remove_part_vars,
      meta_data = meta_data,
      label_col = label_col
    )
  }

  # remove variables excluded by roles
  if (!isFALSE(exclude_roles)) {
    remove_roles <- function(vars, meta_data, label_col) {
      roles <- util_map_labels(vars,
        meta_data = meta_data,
        to = VARIABLE_ROLE,
        from = label_col
      )
      vars[!(roles %in% exclude_roles)]
    }
    var_sets <- lapply(var_sets, remove_roles,
      meta_data = meta_data,
      label_col = label_col
    )
  }

  var_sets <- var_sets[vapply(var_sets, length, FUN.VALUE = integer(1)) > 0]

  # Which groups?
  if (length(group_vars) > 0) {
    is_grouped <- TRUE

    ds1[, setdiff(group_vars, part_vars)] <-
      ds1_labelled[, setdiff(group_vars, part_vars), FALSE]
    gr <- unique(ds1[[group_vars]][!is.na(ds1[[group_vars]])])
    gr <- gr[order(gr)]
    # covariables for plot
    cvs <- c(group_vars, "Examinations")

    # missings in grouping variable?
    ds1_labelled <- ds1_labelled[!is.na(ds1[[group_vars]]), , drop = FALSE]
    ds1 <- ds1[!is.na(ds1[[group_vars]]), , drop = FALSE]
  } else {
    is_grouped <- FALSE
    gr <- 1
    group_vars <- "Group"
    ds1$Group <- 1
    cvs <- "Examinations"
  }

  if (length(strata_vars) > 0) {
    # No. of strata levels and labels
    if (dim(ds1)[1] != dim(ds1[!is.na(ds1[[strata_vars]]), , drop = FALSE])[1]) { # nolint: line_length_linter.
      ds1_labelled <- ds1_labelled[!is.na(ds1[[strata_vars]]), , drop = FALSE]
      ds1 <- ds1[!is.na(ds1[[strata_vars]]), , drop = FALSE]
      util_message(
        paste0(
          "Some observations in ", strata_vars,
          " are NA and were removed."
        ),
        applicability_problem = FALSE
      )
    }

    ds1[, setdiff(strata_vars, part_vars)] <-
      ds1_labelled[, setdiff(strata_vars, part_vars), FALSE]

    strata <- unique(ds1[[strata_vars]])[!is.na(unique(ds1[[strata_vars]]))]
    strata <- strata[order(strata)]
    # covariables for plot
    cvs <- c(strata_vars, group_vars, "Examinations")
  }

  # create result dataframe by factor combinations
  if (length(strata_vars) == 0) {
    res_df <- expand.grid(Group = gr, Examinations = names(var_sets))
    colnames(res_df) <- c(group_vars, "Examinations")
  } else {
    res_df <- expand.grid(
      Strata = strata, Group = gr, Examinations = names(var_sets),
      stringsAsFactors = TRUE
    )
    colnames(res_df) <- c(strata_vars, group_vars, "Examinations")
    res_df <- res_df[order(res_df[[strata_vars]], res_df[[group_vars]]), , drop = FALSE] # nolint: line_length_linter.
  }

  ################
  # CALCULATIONS #
  ################

  myfun <- function(x) {
    all(is.na(x))
  }
  n_participants <- c()
  n_missing_segments <- c()

  if (length(strata_vars) == 0) {
    for (j in seq_along(var_sets)) {
      for (i in seq_along(gr)) {
        one_var_from_seg_w_r_o_g <- var_sets[[j]][[1]] # empty var set is impossible, since segments are read from item-level-metadata here. so, the first one could represent for expected observations the whole segment. # nolint: line_length_linter.
        check_df <- ds1[
          util_observation_expected(
            rv = one_var_from_seg_w_r_o_g,
            study_data = ds1,
            meta_data = meta_data,
            label_col = label_col,
            expected_observations =
              expected_observations
          ) &
            ds1[[group_vars]] == gr[i],
          c(as.character(unlist(var_sets[j]))),
          drop = FALSE
        ]
        n_participants <- c(n_participants, nrow(check_df))
        n_missing_segments <- c(
          n_missing_segments,
          sum(apply(check_df, 1, myfun))
        )
      }
    }
  } else {
    for (i in seq_along(strata)) {
      for (j in seq_along(gr)) {
        for (k in seq_along(var_sets)) {
          one_var_from_seg_w_r_o_g <- var_sets[[k]][[1]] # empty var set is impossible, since segments are read from item-level-metadata here. so, the first one could represent for expected observations the whole segment. # nolint: line_length_linter.
          check_df <-
            ds1[
              util_observation_expected(
                rv = one_var_from_seg_w_r_o_g,
                study_data = ds1,
                meta_data = meta_data,
                label_col = label_col,
                expected_observations =
                expected_observations
              ) &
              ds1[[strata_vars]] == strata[i] & ds1[[group_vars]] == gr[j],
              c(as.character(unlist(var_sets[k])))
            ]
          n_participants <- c(n_participants, nrow(check_df))
          n_missing_segments <- c(
            n_missing_segments,
            sum(apply(check_df, 1, myfun))
          )
        }
      }
    }
  }

  res_df$"No. Participants" <- n_participants
  res_df$"No. missing segments" <- n_missing_segments
  res_df$"(%) of missing segments" <- round(res_df$`No. missing segments` /
      res_df$`No. Participants` *
      100, digits = 2)
  res_df$"(%) of missing segments" <-
    as.numeric(res_df$"(%) of missing segments")
  if (use_legacy_threshold) {
    res_df$threshold <- threshold_value
    res_df$direction <- color_gradient_direction
  }

  res_df$"(%) of missing segments"[
    !is.finite(res_df$"(%) of missing segments")
  ] <- NA_real_ # to avoid Inf %

  if (!use_legacy_threshold) {
    res_df[[GRADING_RULESET]] <- util_segment_grading_rulesets(
      segments = as.character(res_df$Examinations),
      meta_data_segment = meta_data_segment
    )
  }

  # order result data frame by grouping variable
  repsumtab <- NULL
  if (length(strata_vars) == 0) {
    # order result data frame by grouping variable
    res_df <- res_df[order(res_df[[group_vars]]), , drop = FALSE]
    if (!is_grouped) {
      repsumtab <- util_segment_missingness_report_summary_table(
        df = res_df,
        meta_data = meta_data,
        label_col = label_col,
        use_legacy_threshold = use_legacy_threshold,
        threshold_value = threshold_value,
        color_gradient_direction = color_gradient_direction
      )
      if (use_legacy_threshold) {
        p <- print(repsumtab, view = FALSE)
        attr(p, "segment_missingness_colors") <- "legacy_threshold"
      } else {
        p <- util_create_lean_ggplot(
          print(repsumtab, view = FALSE),
          repsumtab = repsumtab,
          .lazy = TRUE
        )
      }
    } else if (use_legacy_threshold) {
      p <- util_heatmap_1th(
        df = res_df, cat_vars = cvs, values = "(%) of missing segments",
        right_intv = TRUE, threshold = threshold_value,
        invert = as.integer(color_gradient_direction == "below")
      )$SummaryPlot
    } else {
      p <- util_create_lean_ggplot(
        util_segment_missingness_grading_plot(
          df = res_df,
          cat_vars = cvs
        ),
        res_df = res_df,
        cvs = cvs,
        .lazy = TRUE
      )
    }
  } else {
    # order result data frame by grouping variable
    res_df <- res_df[order(res_df[[strata_vars]], res_df[[group_vars]]), , drop = FALSE] # nolint: line_length_linter.
    if (use_legacy_threshold) {
      p <- util_heatmap_1th(
        df = res_df, cat_vars = cvs[-1],
        values = "(%) of missing segments",
        right_intv = TRUE, threshold = threshold_value,
        invert = as.integer(color_gradient_direction == "below"),
        strata = strata_vars
      )$SummaryPlot
    } else {
      p <- util_create_lean_ggplot(
        util_segment_missingness_grading_plot(
          df = res_df,
          cat_vars = cvs[-1],
          strata = strata_vars
        ),
        res_df = res_df,
        cvs = cvs,
        strata_vars = strata_vars,
        .lazy = TRUE
      )
    }
  }
  # Historical segment-missingness plot-size prototype removed here. Inspect
  # with `git show 214dd76a7d -- R/com_segment_missingness.R`.

  text_to_display <- util_get_hovertext("[com_segment_missingness_hover]")
  attr(res_df, "description") <- text_to_display

  res_df <- res_df[, seq_along(res_df), drop = FALSE]

  # Add new attribute to the columns of ResultData to define the datatype of
  # each column
  attr(res_df[[paste(colnames(res_df)[1])]], DATA_TYPE) <- DATA_TYPES$STRING # The column name depends on the presence of the argument group_vars, if not present is Group otherwise is the name of the variable in group_vars # nolint: line_length_linter.
  attr(res_df$Examinations, DATA_TYPE) <- DATA_TYPES$STRING
  attr(res_df$`No. Participants`, DATA_TYPE) <- DATA_TYPES$INTEGER
  attr(res_df$`No. missing segments`, DATA_TYPE) <- DATA_TYPES$INTEGER
  attr(res_df$`(%) of missing segments`, DATA_TYPE) <- DATA_TYPES$FLOAT
  if (GRADING_RULESET %in% colnames(res_df)) {
    attr(res_df[[GRADING_RULESET]], DATA_TYPE) <- DATA_TYPES$INTEGER
  }
  if (use_legacy_threshold) {
    attr(res_df$threshold, DATA_TYPE) <- DATA_TYPES$INTEGER
    attr(res_df$direction, DATA_TYPE) <- DATA_TYPES$STRING
  }


  result <- list(ResultData = res_df)
  if (!is.null(repsumtab)) {
    result$ReportSummaryTable <- repsumtab
  }
  result$SummaryPlot <- p

  return(util_attach_attr(
    result
    # Historical sizing-hints prototype removed here. Inspect with
    # `git show 214dd76a7d -- R/com_segment_missingness.R`.
  ))
}

#' Build the segment-missingness ReportSummaryTable
#'
#' @param df [data.frame] segment-missingness results.
#' @inheritParams .template_function_developer
#' @param use_legacy_threshold [logical] whether to use binary legacy colors.
#' @param threshold_value [numeric] effective legacy threshold.
#' @param color_gradient_direction [character] critical threshold direction.
#'
#' @return a `ReportSummaryTable` object.
#' @noRd
util_segment_missingness_report_summary_table <- function(df, meta_data,
  label_col, use_legacy_threshold, threshold_value,
  color_gradient_direction) {
  repsumtab <- df[, c(
    "Examinations",
    "No. missing segments",
    "No. Participants"
  ), drop = TRUE]
  colnames(repsumtab)[c(1, 3)] <- c("Variables", "N")
  repsumtab <- util_new_report_summary_table(repsumtab,
    meta_data = meta_data,
    label_col = label_col
  )
  attr(repsumtab, "render_as_plot") <- TRUE
  attr(repsumtab, "segment_missingness_bar") <- list(
    value_column = "No. missing segments",
    color_mode = if (use_legacy_threshold) {
      "legacy_threshold"
    } else {
      "grading_rules"
    }
  )

  if (use_legacy_threshold) {
    relative_values <- df$`No. missing segments` / df$`No. Participants`
    percentages <- 100 * relative_values
    critical <- if (color_gradient_direction == "above") {
      percentages > threshold_value
    } else {
      percentages < threshold_value
    }
    colors <- ifelse(critical, "#7f0000", "#2166AC")
    classes <- ifelse(critical, "Critical", "Normal")
    bar_context <- util_attr(
      repsumtab,
      "segment_missingness_bar",
      exact = TRUE
    )
    bar_context$colors <- colors
    bar_context$class_labels <- classes
    bar_context$threshold_value <- threshold_value
    bar_context$direction <- color_gradient_direction
    attr(repsumtab, "segment_missingness_bar") <- bar_context

    valid <- is.finite(relative_values)
    observed_values <- sort(unique(relative_values[valid]))
    value_keys <- as.character(observed_values)
    first_rows <- match(observed_values, relative_values)
    repsumtab <- util_set_report_summary_table_continuous(repsumtab, FALSE)
    repsumtab <- util_set_report_summary_table_colcode(
      repsumtab,
      setNames(colors[first_rows], value_keys)
    )
    repsumtab <- util_set_report_summary_table_level_names(
      repsumtab,
      setNames(classes[first_rows], value_keys)
    )
  } else {
    attr(repsumtab, "grading_context") <- list(
      indicator_metric = "PCT_com_crm_mv",
      entity = "SEGMENT",
      values_raw = df$`(%) of missing segments`,
      var_names = as.character(df$Examinations),
      grading_rule_sets = as.character(df[[GRADING_RULESET]])
    )
  }
  repsumtab <- util_set_report_summary_table_relative(repsumtab, TRUE)
  repsumtab
}

#' Classify segment-missingness values using grading rules
#'
#' @param df [data.frame] segment-missingness results.
#'
#' @return a data frame with grading classes and colors.
#' @noRd
util_segment_missingness_grading <- function(df) {
  segments <- unique(as.character(df$Examinations))
  grading_meta_data <- data.frame(segments, stringsAsFactors = FALSE)
  colnames(grading_meta_data) <- VAR_NAMES
  grading_meta_data[[GRADING_RULESET]] <- as.character(
    df[[GRADING_RULESET]][match(segments, as.character(df$Examinations))]
  )

  summary_values <- data.frame(
    function_name = rep("com_segment_missingness", nrow(df)),
    indicator_metric = rep("PCT_com_crm_mv", nrow(df)),
    values_raw = df$`(%) of missing segments`,
    call_names = rep("", nrow(df)),
    stringsAsFactors = FALSE
  )
  summary_values[[VAR_NAMES]] <- as.character(df$Examinations)
  summary_values$.row_id <- seq_len(nrow(summary_values))
  classes <- suppressWarnings(util_metrics_to_classes(
    summary_values,
    grading_meta_data,
    entity = "SEGMENT"
  ))
  classes <- classes[order(classes$.row_id), , drop = FALSE]

  colors <- unname(util_get_colors()[as.character(classes$class)])
  colors[is.na(colors)] <- "#888888"
  data.frame(class = classes$class, color = colors)
}

#' Resolve grading rulesets for segment-level results
#'
#' @param segments [character] Segment names in result-row order.
#' @param meta_data_segment [data.frame] Segment-level metadata.
#'
#' @return A character vector with one grading ruleset per result row.
#' @noRd
util_segment_grading_rulesets <- function(segments, meta_data_segment) {
  rulesets <- rep("0", length(segments))
  if (!(STUDY_SEGMENT %in% colnames(meta_data_segment))) {
    return(rulesets)
  }

  for (segment in unique(segments)) {
    segment_rulesets <- unique(meta_data_segment[
      as.character(meta_data_segment[[STUDY_SEGMENT]]) == segment,
      GRADING_RULESET,
      drop = TRUE
    ])
    segment_rulesets <- segment_rulesets[!util_empty(segment_rulesets)]
    if (length(segment_rulesets) > 1L) {
      util_error(
        "More than one %s is defined for segment %s: %s.",
        sQuote(GRADING_RULESET),
        dQuote(segment),
        util_pretty_vector_string(dQuote(segment_rulesets)),
        applicability_problem = TRUE
      )
    }
    if (length(segment_rulesets) == 1L) {
      rulesets[segments == segment] <- segment_rulesets
    }
  }

  rulesets
}

#' Create a segment-missingness plot using grading rules
#'
#' @param df [data.frame] segment-missingness results.
#' @param cat_vars [character] one or two categorical plot dimensions.
#' @param strata [character] optional stratum variable.
#'
#' @return a [ggplot2::ggplot] object.
#' @noRd
util_segment_missingness_grading_plot <- function(df, cat_vars, strata) {
  util_stop_if_not(length(cat_vars) %in% 1:2)

  grading <- util_segment_missingness_grading(df)
  df$.dq_grading_class <- grading$class
  df$.dq_grading_color <- grading$color

  if (length(cat_vars) == 1) {
    y_limit <- max(c(1, 1.2 * df$`(%) of missing segments`), na.rm = TRUE)
    p <- ggplot(df, aes(
      x = .data[[cat_vars]],
      y = .data[["(%) of missing segments"]],
      fill = .data$.dq_grading_color
    )) +
      geom_bar(stat = "identity", na.rm = TRUE) +
      geom_text(
        label = util_paste0_with_na(
          " ", df$`(%) of missing segments`, "%"
        ),
        hjust = 0, vjust = 0.5, show.legend = FALSE
      ) +
      util_scale_fill_dataquieR() +
      theme_minimal() +
      scale_x_discrete(name = cat_vars) +
      scale_y_continuous(name = "(%)", limits = c(0, y_limit)) +
      coord_flip()
  } else {
    df[[cat_vars[[1]]]] <- factor(df[[cat_vars[[1]]]])
    df[[cat_vars[[2]]]] <- factor(df[[cat_vars[[2]]]])
    if (nlevels(df[[cat_vars[[1]]]]) < nlevels(df[[cat_vars[[2]]]])) {
      df$.dq_x <- df[[cat_vars[[1]]]]
      df$.dq_y <- df[[cat_vars[[2]]]]
      name_x <- cat_vars[[1]]
      name_y <- cat_vars[[2]]
    } else {
      df$.dq_x <- df[[cat_vars[[2]]]]
      df$.dq_y <- df[[cat_vars[[1]]]]
      name_x <- cat_vars[[2]]
      name_y <- cat_vars[[1]]
    }

    p <- ggplot(df, aes(
      x = .data$.dq_x,
      y = .data$.dq_y,
      fill = .data$.dq_grading_color
    )) +
      geom_tile(colour = "white", linewidth = 0.8) +
      geom_text(
        label = util_paste0_with_na(
          df$`(%) of missing segments`, "%"
        ),
        show.legend = FALSE
      ) +
      util_scale_fill_dataquieR() +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
      scale_x_discrete(name = name_x) +
      scale_y_discrete(
        expand = c(0, 0), name = name_y,
        limits = rev(levels(df$.dq_y))
      ) +
      xlab("Study segments")
    if (!missing(strata)) {
      p <- p + facet_grid(.data[[strata]] ~ .)
    }
  }

  attr(p, "segment_missingness_colors") <- "grading_rules"
  p
}
