#' Compute Indicators for Qualified Segment Missingness
#'
#' [Indicator]
#'
#' @inheritParams .template_function_indicator
#'
#' @param meta_data_segment [data.frame] Segment level metadata
#' @param expected_observations [enum] HIERARCHY | ALL | SEGMENT. Report the
#'                                     number of observations expected using
#'                                     the old `PART_VAR` concept. See
#'                                     [com_item_missingness] for an
#'                                     explanation.
#'
#' @return A [list] with:
#'   - `SegmentTable`: [data.frame] containing data quality checks for
#'                     "Non-response rate" (`PCT_com_qum_nonresp`) and
#'                     "Refusal rate" (`PCT_com_qum_refusal`) for each segment.
#'   - `SegmentData`: a [data.frame] containing data quality checks for
#'                    "Unexpected location" and "Unexpected proportion" per
#'                     segment for a report
#'
#' @export
com_qualified_segment_missingness <- function(label_col = NULL,
  study_data,
  item_level = "item_level",
  expected_observations = c(
    "HIERARCHY",
    "ALL",
    "SEGMENT"
  ),
  meta_data = item_level,
  meta_data_v2,
  meta_data_segment,
  segment_level) {
  # Preparation of the input ----
  util_maybe_load_meta_data_v2()

  util_ck_arg_aliases()

  prep_prepare_dataframes(.replace_missings = FALSE)

  expected_observations <- util_match_arg(expected_observations)

  meta_data_segment <- prep_check_meta_data_segment(meta_data_segment)

  # allowed AAPOR-States
  AAPOR_STATES <- c("I", "P", "PL", "R", "BO", "NC", "O", "UH", "UO", "NE")

  # Loop over all segments defined on segment-level metadata ----

  aapors <-
    lapply(
      setNames(nm = meta_data_segment[[STUDY_SEGMENT]]),
      function(segment) { # segment is the currently examined segment's name
        cur_seg <- meta_data_segment[ # only the line(s) from metadata segment, that refer to the currently examined segment # nolint: line_length_linter.
          meta_data_segment[[STUDY_SEGMENT]] == segment, , drop = FALSE
        ]
        util_stop_if_not(nrow(cur_seg) == 1)
        .cur_state_var <- cur_seg[[SEGMENT_PART_VARS]] # participation status variable of the current segment # nolint: line_length_linter.
        if (length(.cur_state_var) != 1 || is.na(.cur_state_var)) {
          util_warning("Missing or doubled %s in %s for %s",
            sQuote(SEGMENT_PART_VARS),
            sQuote("meta_data_segment"),
            dQuote(segment),
            applicability_problem = TRUE
          )
          return(NULL)
        }
        cur_state_var_vn <- meta_data[
          meta_data[[label_col]] == .cur_state_var |
            meta_data[[VAR_NAMES]] == .cur_state_var,
          "VAR_NAMES"
          , drop = TRUE]
        cur_state_var <- meta_data[
          meta_data[[label_col]] == .cur_state_var |
            meta_data[[VAR_NAMES]] == .cur_state_var,
          label_col
          , drop = TRUE]
        if (length(cur_state_var) != 1) {
          util_warning("Missing or doubled %s in %s",
            dQuote(.cur_state_var),
            sQuote("meta_data"),
            applicability_problem = TRUE
          )
          return(NULL)
        }
        if (cur_state_var %in% colnames(ds1)) {
          .cur_miss <- meta_data[
            meta_data[[label_col]] == cur_state_var,
            MISSING_LIST_TABLE
            , drop = TRUE]
          if (length(.cur_miss) == 0) {
            .cur_miss <- NA_character_
          }
          cur_miss <- try(util_expect_data_frame(
            x = .cur_miss,
            dont_assign = TRUE,
            col_names = list(
              CODE_VALUE = util_is_valid_missing_codes,
              CODE_INTERPRET = function(x) is.character(x) & x %in% AAPOR_STATES
            ),
            custom_errors = list(
              CODE_VALUE = "CODE_VALUE must be numeric or DATETIME",
              CODE_INTERPRET = paste(
                "CODE_INTERPRET must be one of",
                util_pretty_vector_string(AAPOR_STATES)
              )
            ),
            convert_if_possible = list(
              CODE_VALUE = util_as_valid_missing_codes,
              CODE_INTERPRET = function(x) trimws(toupper(x))
            )
          ), silent = TRUE)
          if (inherits(cur_miss, "try-error")) {
            if (!is.na(.cur_miss)) {
              util_warning(
                "Could not load missing-match-table %s for %s variable %s for segment %s: %s", # nolint: line_length_linter.
                dQuote(.cur_miss),
                sQuote(SEGMENT_PART_VARS),
                dQuote(cur_state_var),
                dQuote(segment),
                conditionMessage(util_attr(cur_miss, "condition",
                    exact = TRUE
                  )),
                applicability_problem = TRUE
              )
            } else {
              util_warning(
                "No missing-match-table for response variable %s for segment %s", # nolint: line_length_linter.
                dQuote(cur_state_var),
                dQuote(segment),
                applicability_problem = TRUE
              )
            }
            return(NULL)
          } else {
            cur_miss <- util_filter_missing_list_table_for_rv(
              cur_miss, cur_state_var, cur_state_var_vn
            )

            # really compute stuff ----
            r <- util_table_of_vct(factor(
              ds1[[cur_state_var]],
              levels = cur_miss[[CODE_VALUE]],
              labels = cur_miss[[CODE_INTERPRET]]
            ))

            r <- as.list(setNames(r$Freq, nm = r$Var1))

            # replacement list
            rclean <- r
            for (name in AAPOR_STATES) {
              if (is.null(rclean[[name]])) rclean[[name]] <- 0
            }

            # Response-rate 1 and non-response-rate 1.
            if (!is.null(r$I) || !is.null(r$P)) {
              r$RR1 <- (rclean$I + rclean$P) /
                ((rclean$I + rclean$P + rclean$PL) +
                    (rclean$R + rclean$BO + rclean$NC + rclean$O) +
                    (rclean$UH + rclean$UO))
              r$NRR1 <- 1 - r$RR1
              r$PCT_com_qum_nonresp <- 100 * r$NRR1
            } else {
              util_warning(
                "Preconditions to generate Nonresponse Rate 1 not given for segment %s", # nolint: line_length_linter.
                dQuote(segment),
                applicability_problem = TRUE
              )
              r$NRR1 <- NA_real_
              r$RR1 <- NA_real_
              r$PCT_com_qum_nonresp <- NA_real_
            }

            # Response-rate 2 and non-response-rate 2.
            if (!is.null(r$I) || !is.null(r$P) || !is.null(r$PL)) {
              r$RR2 <- (rclean$I + rclean$P + rclean$PL) /
                ((rclean$I + rclean$P + rclean$PL) +
                    (rclean$R + rclean$BO + rclean$NC + rclean$O) +
                    (rclean$UH + rclean$UO))
              r$NRR2 <- 1 - r$RR2
            } else {
              util_warning(
                "Preconditions to generate Nonresponse Rate 1 not given for segment %s", # nolint: line_length_linter.
                dQuote(segment),
                applicability_problem = TRUE
              )
              r$NRR2 <- NA_real_
              r$RR2 <- NA_real_
            }

            # Refusal-rate 1 and refusal percentage.
            if (!is.null(r$R) || !is.null(r$BO)) {
              r$REF1 <- (rclean$R + rclean$BO) /
                ((rclean$I + rclean$P + rclean$PL) +
                    (rclean$R + rclean$BO + rclean$NC + rclean$O) +
                    (rclean$UH + rclean$UO))
              r$PCT_com_qum_refusal <- 100 * r$REF1
            } else {
              util_warning(
                "Preconditions to generate Nonresponse Rate 1 not given for segment %s", # nolint: line_length_linter.
                dQuote(segment),
                applicability_problem = TRUE
              )
              r$REF1 <- NA_real_
              r$PCT_com_qum_refusal <- NA_real_
            }

            r$N <- sum(!util_empty(ds1[[cur_state_var]]))
            r$N2 <- util_count_expected_observations(
              cur_state_var,
              study_data = study_data,
              meta_data = meta_data,
              label_col = label_col,
              expected_observations = expected_observations
            )

            r <- cbind(
              data.frame(Segment = segment, stringsAsFactors = FALSE, check.names = FALSE, row.names = NULL), # nolint: line_length_linter.
              data.frame(r, stringsAsFactors = FALSE, check.names = FALSE, row.names = NULL) # nolint: line_length_linter.
            )
          }
        } else {
          util_warning("Missing %s variable %s from %s for %s",
            sQuote(SEGMENT_PART_VARS),
            dQuote(cur_state_var),
            sQuote("study_data"),
            dQuote(segment),
            applicability_problem = TRUE,
            intrinsic_applicability_problem = TRUE
          )
          return(NULL)
        }
      }
    )

  SegmentTable <- util_rbind(data_frames_list = aapors)
  rownames(SegmentTable) <- NULL
  if ("Segment" %in% colnames(SegmentTable)) {
    attr(SegmentTable$Segment, DATA_TYPE) <- DATA_TYPES$STRING
  }
  for (cl in grep("^NUM_", colnames(SegmentTable), value = TRUE)) {
    attr(SegmentTable[[cl]], DATA_TYPE) <- DATA_TYPES$INTEGER
  }
  for (cl in grep("^PCT_", colnames(SegmentTable), value = TRUE)) {
    attr(SegmentTable[[cl]], DATA_TYPE) <- DATA_TYPES$FLOAT
  }

  # --- Start refactored part: use helpers for selection + translation ---
  # Select only indicator metric columns
  indicator_cols <- util_extract_indicator_metrics(SegmentTable)

  # Assemble SegmentData = Segment + indicator columns
  SegmentData <- cbind(
    data.frame(Segment = SegmentTable[["Segment"]], stringsAsFactors = FALSE, check.names = FALSE, row.names = NULL), # nolint: line_length_linter.
    indicator_cols
  )

  # Format percentage columns BEFORE renaming (based on original PCT_ prefixes)
  pct_cols <- colnames(SegmentData)[startsWith(colnames(SegmentData), "PCT_")]
  if (length(pct_cols)) {
    SegmentData[, pct_cols] <- lapply(
      SegmentData[, pct_cols, drop = FALSE],
      function(cl) paste0(round(cl, 2), "%")
    )
  }
  if ("Segment" %in% colnames(SegmentData)) {
    attr(SegmentData$Segment, DATA_TYPE) <- DATA_TYPES$STRING
  }
  for (cl in grep("^NUM_", colnames(SegmentData), value = TRUE)) {
    attr(SegmentData[[cl]], DATA_TYPE) <- DATA_TYPES$INTEGER
  }
  for (cl in pct_cols) {
    attr(SegmentData[[cl]], DATA_TYPE) <- DATA_TYPES$STRING
  }

  # Translate column headers via helper (keep unknowns such as "Segment" intact)
  colnames(SegmentData) <- util_translate_indicator_metrics(
    colnames(SegmentData),
    short = FALSE,
    long = TRUE,
    ignore_unknown = TRUE
  )
  # --- End refactored part ---


  text_to_display <- util_get_hovertext("[com_qualified_segment_missingness_hover]") # nolint: line_length_linter.
  attr(SegmentData, "description") <- text_to_display

  return(list(
    SegmentTable = SegmentTable,
    SegmentData  = SegmentData
  ))
}
