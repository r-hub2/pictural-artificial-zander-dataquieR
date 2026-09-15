# nolint start: line_length_linter.
#' Summarize missingness columnwise (in variable)
#'
#' @description
#' Item-Missingness (also referred to as item nonresponse (De Leeuw et al.
#' 2003)) describes the missingness of single values, e.g. blanks or empty data
#' cells in a data set. Item-Missingness occurs for example in case a respondent
#' does not provide information for a certain question, a question is overlooked
#' by accident, a programming failure occurs or a provided answer were missed
#' while entering the data.
#'
#' [Indicator]
#'
#' @details
#' # ALGORITHM OF THIS IMPLEMENTATION:
#'
#'  - Lists of missing codes and, if applicable, jump codes are selected from
#'    the metadata
#'  - The no. of system missings (NA) in each variable is calculated
#'  - The no. of used missing codes is calculated for each variable
#'  - The no. of used jump codes is calculated for each variable
#'  - Two result dataframes (1: on the level of observations, 2: a summary for
#'    each variable) are generated
#'  - *OPTIONAL:* if `show_causes` is selected, one summary plot for all
#'                `resp_vars` is provided
#'
#' @inheritParams .template_function_indicator
#'
#' @param show_causes [logical] if TRUE, then the distribution of missing codes
#'                              is shown
#' @param cause_label_df [data.frame] missing code table. If missing codes have
#'                                    labels the respective data frame can be
#'                                    specified here or in the metadata as
#'                                    assignments, see [cause_label_df]
#' @param include_sysmiss [logical] Optional, if TRUE system missingness (NAs)
#'                                  is evaluated in the summary plot
#' @param threshold_value [numeric] from=0 to=100. a numerical value ranging
#'                                                 from 0-100
#' @param suppressWarnings [logical] warn about consistency issues with missing
#'                                   and jump lists
#' @param assume_consistent_codes [logical] if TRUE and no labels are given and
#'                                          the same missing/jump code is used
#'                                          for more than one variable, the
#'                                          labels assigned for this code are
#'                                          treated as being be the same for
#'                                          all variables.
#' @param expand_codes [logical] if TRUE, code labels are copied from other
#'                               variables, if the code is the same and the
#'                               label is set somewhere
#' @param drop_levels [logical] if TRUE, do not display unused missing codes in
#'                             the figure legend.
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
#' @param pretty_print [logical] deprecated. If you want to have a human
#'                               readable output, use `SummaryData` instead
#'                               of `SummaryTable`
#'
#' @return a list with:
#'   - `SummaryTable`: data frame about item missingness per response variable,
#'                     including the count (`NUM_int_vfe_missunc`) and
#'                     percentage (`PCT_int_vfe_missunc`) of system-missing
#'                     values as uncertain missingness status, and the count
#'                     (`NUM_com_qum_spec`) and percentage (`PCT_com_qum_spec`)
#'                     of missing and jump codes among expected observations
#'                     that are not system missing
#'   - `SummaryData`: data frame about item missingness per response variable
#'                    formatted for user
#'   - `SummaryPlot`: ggplot2 heatmap plot, if show_causes was TRUE
#'   - `ReportSummaryTable`: data frame underlying `SummaryPlot`
#'
#' @export
#' @importFrom ggplot2 ggplot facet_wrap geom_bar theme_minimal theme annotate scale_fill_gradientn theme element_blank
#' @seealso
#' [Online Documentation](
#' https://dataquality.qihs.uni-greifswald.de/VIN_com_impl_item_missingness.html
#' )
# nolint end
com_item_missingness <- function(resp_vars = NULL,
  study_data,
  label_col,
  item_level = "item_level",
  show_causes = TRUE,
  cause_label_df,
  include_sysmiss = TRUE,
  threshold_value,
  suppressWarnings = FALSE,
  assume_consistent_codes = TRUE,
  expand_codes = assume_consistent_codes,
  drop_levels = FALSE,
  expected_observations = c(
    "HIERARCHY",
    "ALL",
    "SEGMENT"
  ),
  pretty_print = lifecycle::deprecated(),
  meta_data = item_level,
  meta_data_v2) {
  util_maybe_load_meta_data_v2()

  if (lifecycle::is_present(pretty_print)) {
    # Signal the deprecation to the user
    lifecycle::deprecate_stop("2.1.0",
      "dataquieR::com_item_missingness(pretty_print = 'FALSE')",
      "dataquieR::com_item_missingness()",
      details = c(
        "Replace as follows: dataquieR::com_item_missingness(pretty_print = FALSE)$SummaryTable --> dataquieR::com_item_missingness()$SummaryTable", # nolint: line_length_linter.
        "dataquieR::com_item_missingness(pretty_print = TRUE)$SummaryTable --> dataquieR::com_item_missingness()$SummaryData", # nolint: line_length_linter.
        "dataquieR::com_item_missingness()$SummaryTable --> dataquieR::com_item_missingness()$SummaryData" # nolint: line_length_linter.
      )
    )
  }
  expected_observations_missing <- missing(expected_observations)
  util_expect_scalar(expected_observations, allow_more_than_one = TRUE)
  expected_observations <- match.arg(expected_observations)
  util_expect_scalar(expected_observations)

  util_expect_scalar(assume_consistent_codes, check_type = is.logical)
  util_expect_scalar(include_sysmiss, check_type = is.logical)
  util_expect_scalar(suppressWarnings, check_type = is.logical)
  util_expect_scalar(drop_levels, check_type = is.logical)

  if (is.character(study_data)) {
    study_data <- prep_get_data_frame(study_data)
  }

  study_data <- util_cast_off(study_data, "study_data", TRUE)
  .sd <- study_data
  prep_prepare_dataframes(.replace_missings = FALSE)
  study_data <- .sd

  if (missing(threshold_value)) {
    if (!.called_in_pipeline) {
      util_message(
        c(
          "The mandatory argument threshold_value was not",
          "defined and is set to the default of 90%%."
        ),
        applicability_problem = TRUE
      )
    }
    threshold_value <- 90
  }
  .threshold_value <- suppressWarnings(as.numeric(threshold_value))
  if (is.na(.threshold_value)) {
    util_message(
      c(
        "Could not convert threshold_value %s to a number.",
        "Set to default value 90%%."
      ),
      dQuote(as.character(threshold_value)),
      applicability_problem = TRUE
    )
    threshold_value <- 90
  } else {
    threshold_value <- .threshold_value
  }


  if (!missing(show_causes) &&
      length(show_causes) == 1 &&
      is.logical(show_causes) &&
      !show_causes) {
    util_warning(
      c(
        "The argument %s has been deprecated. It will be ignored",
        "and in a future version be removed."
      ),
      dQuote("show_causes")
    )
  }

  if (!missing(cause_label_df)) {
    util_expect_data_frame(cause_label_df, c(CODE_VALUE, CODE_LABEL))
    util_warning(
      c(
        "The argument %s has been deprecated. It will be",
        "in a future version be removed."
      ),
      dQuote("cause_label_df")
    )
    meta_data <-
      prep_add_cause_label_df(
        meta_data = meta_data,
        cause_label_df = cause_label_df,
        label_col = label_col,
        assume_consistent_codes = assume_consistent_codes
      )
  }

  if (!suppressWarnings) {
    util_validate_missing_lists(
      meta_data = meta_data,
      cause_label_df = cause_label_df,
      assume_consistent_codes =
        assume_consistent_codes,
      expand_codes = FALSE,
      suppressWarnings = suppressWarnings,
      label_col = label_col
    )
  }

  if (missing(resp_vars) || is.null(resp_vars)) {
    resp_vars <- meta_data[[label_col]]
    if (VARIABLE_ORDER %in% colnames(meta_data)) {
      try(
        resp_vars <- resp_vars[order(util_as_numeric(meta_data[[VARIABLE_ORDER]], # nolint: line_length_linter.
          warn =
            sprintf(
              "%s is not fully numeric: %%s",
              sQuote(VARIABLE_ORDER)
            )
        ))],
        silent = TRUE
      )
    }
  }

  util_correct_variable_use("resp_vars",
    allow_more_than_one = TRUE,
    allow_all_obs_na = TRUE,
    allow_null = TRUE
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

  if (expand_codes) {
    meta_data <- prep_expand_codes(meta_data,
      suppressWarnings,
      mix_jumps_and_missings = FALSE
    )
  }

  {
    r <- util_study_var2factor(
      study_data = study_data, meta_data = meta_data,
      resp_vars = resp_vars, label_col = label_col,
      assume_consistent_codes = assume_consistent_codes,
      have_cause_label_df = !missing(cause_label_df),
      code_name = MISSING_LIST,
      include_sysmiss = FALSE
    )
    colnames(r) <-
      util_map_labels(colnames(r),
        meta_data,
        to = VAR_NAMES,
        from = label_col
      )
    m <-
      vapply(lapply(
        util_seg_table(r, study_data,
          meta_data,
          expected_observations = expected_observations
        ),
        `[[`, "Freq"
      ), sum, FUN.VALUE = integer(1))
  }
  {
    r <- util_study_var2factor(
      study_data = study_data, meta_data = meta_data,
      resp_vars = resp_vars, label_col = label_col,
      assume_consistent_codes = assume_consistent_codes,
      have_cause_label_df = !missing(cause_label_df),
      code_name = JUMP_LIST,
      include_sysmiss = FALSE
    )
    colnames(r) <-
      util_map_labels(colnames(r),
        meta_data,
        to = VAR_NAMES,
        from = label_col
      )
    j <-
      vapply(lapply(
        util_seg_table(r, study_data,
          meta_data,
          expected_observations = expected_observations
        ),
        `[[`, "Freq"
      ), sum, FUN.VALUE = integer(1))
  }
  sys_miss <-
    vapply(
      prep_map_labels(resp_vars,
        meta_data = meta_data,
        to = VAR_NAMES,
        from = label_col
      ),
      function(rv) {
        sum(is.na(study_data[[rv]][
          util_observation_expected(
            rv = rv,
            study_data = study_data,
            meta_data = meta_data,
            label_col = VAR_NAMES,
            expected_observations =
              expected_observations
          )
        ]))
      },
      FUN.VALUE = integer(1)
    )

  n_obs <-
    util_count_expected_observations(resp_vars,
      study_data = ds1,
      meta_data = meta_data,
      label_col = label_col,
      expected_observations = expected_observations
    )

  data_values_n <- vapply(
    resp_vars,
    function(rv) {
      in_seg <-
        util_observation_expected(
          rv = rv,
          study_data = ds1,
          meta_data = meta_data,
          label_col = label_col,
          expected_observations = expected_observations
        )
      {
        vn <- util_map_labels(
          rv, meta_data,
          VAR_NAMES, label_col
        )
        vals <- study_data[!in_seg, vn, drop = FALSE]
        vals <- util_replace_codes_by_NA(
          study_data = vals, meta_data = meta_data
        )[[vn]]
        have_data_not_expected <- !util_is_na_0_empty_or_false(vals)
        # also allow 0/FALSE, if no data is expected
        if (any(have_data_not_expected) && !suppressWarnings) {
          segvars <- util_all_intro_vars_for_rv(rv,
            ds1, meta_data,
            label_col,
            expected_observations =
              expected_observations
          )
          util_warning(
            c(
              "There are %d meassurements of %s for participants",
              "not being part of one of the segments %s"
            ),
            sum(have_data_not_expected),
            dQuote(rv),
            paste(
              dQuote(util_map_labels(
                x = util_map_labels(segvars, meta_data,
                  from = label_col,
                  to = VAR_NAMES
                ),
                meta_data = meta_data,
                from = PART_VAR, to = STUDY_SEGMENT
              )),
              paste0("(if ", dQuote(segvars), " \u2260 1)"),
              collapse = ", "
            ),
            applicability_problem = TRUE
          )
        }
      }
      sum(!is.na(study_data[in_seg, util_map_labels(
        rv, meta_data,
        VAR_NAMES, label_col
      ), drop = TRUE]))
    },
    FUN.VALUE = integer(1)
  )

  n_meas <- data_values_n - m - j

  summary_table <- data.frame(
    check.names = FALSE,
    Variables = resp_vars,
    `Expected observations N` = n_obs,
    `Sysmiss N` = sys_miss,
    `Datavalues N` = data_values_n,
    `Missing codes N` = m,
    `Jumps N` = j,
    `Measurements N` = n_meas
  )

  rownames(summary_table) <- NULL

  sys_miss_pct <- round(sys_miss / n_obs * 100, digits = 2)
  data_values_n_pct <- round(data_values_n / n_obs * 100, digits = 2)
  missing_codes_pct <- round(m / n_obs * 100, digits = 2)
  jumps_pct <- round(j / n_obs * 100, digits = 2)
  n_meas_pct <- round(n_meas / (n_obs - j) * 100, digits = 2)
  specified_reason_n <- m + j
  specified_reason_denominator <- n_obs - sys_miss
  specified_reason_pct <- ifelse(
    specified_reason_denominator > 0,
    round(specified_reason_n / specified_reason_denominator * 100, digits = 2),
    NA_real_
  )
  missing_expected_obs <- sys_miss + j + m

  summary_table$NUM_int_vfe_missunc <- sys_miss
  summary_table$PCT_int_vfe_missunc <- sys_miss_pct
  summary_table$NUM_com_qum_spec <- specified_reason_n
  summary_table$PCT_com_qum_spec <- specified_reason_pct
  summary_table$Missing_expected_obs <- missing_expected_obs
  missing_expected_obs_pct <- round((sys_miss + j + m) / n_obs * 100,
    digits = 2
  )
  PCT_com_crm_mv <- round((sys_miss + j + m) / nrow(ds1) * 100, digits = 2)
  summary_table$PCT_com_crm_mv <- PCT_com_crm_mv
  summary_table$GRADING <- n_meas_pct < threshold_value
  attr(summary_table$Variables, DATA_TYPE) <- DATA_TYPES$STRING
  attr(summary_table$`Expected observations N`, DATA_TYPE) <-
    DATA_TYPES$INTEGER
  attr(summary_table$`Sysmiss N`, DATA_TYPE) <- DATA_TYPES$INTEGER
  attr(summary_table$`Datavalues N`, DATA_TYPE) <- DATA_TYPES$INTEGER
  attr(summary_table$`Missing codes N`, DATA_TYPE) <- DATA_TYPES$INTEGER
  attr(summary_table$`Jumps N`, DATA_TYPE) <- DATA_TYPES$INTEGER
  attr(summary_table$`Measurements N`, DATA_TYPE) <- DATA_TYPES$INTEGER
  attr(summary_table$NUM_int_vfe_missunc, DATA_TYPE) <- DATA_TYPES$INTEGER
  attr(summary_table$PCT_int_vfe_missunc, DATA_TYPE) <- DATA_TYPES$FLOAT
  attr(summary_table$NUM_com_qum_spec, DATA_TYPE) <- DATA_TYPES$INTEGER
  attr(summary_table$PCT_com_qum_spec, DATA_TYPE) <- DATA_TYPES$FLOAT
  attr(summary_table$Missing_expected_obs, DATA_TYPE) <- DATA_TYPES$INTEGER
  attr(summary_table$PCT_com_crm_mv, DATA_TYPE) <- DATA_TYPES$FLOAT
  attr(summary_table$GRADING, DATA_TYPE) <- DATA_TYPE_LOGICAL

  # Create SummaryData
  uncertain_status_metrics <- c(
    "NUM_int_vfe_missunc", "PCT_int_vfe_missunc"
  )
  specified_reason_metrics <- c("NUM_com_qum_spec", "PCT_com_qum_spec")
  indicator_metrics <- c(uncertain_status_metrics, specified_reason_metrics)
  specified_reason_data <- util_make_data_slot_from_table_slot(
    summary_table[, c("Variables", specified_reason_metrics), drop = FALSE]
  )
  specified_reason_data$Variables <- NULL
  SummaryData <- summary_table[
    , !names(summary_table) %in% c("GRADING", indicator_metrics),
    drop = FALSE
  ]

  names(SummaryData)[names(SummaryData) == "Observations N"] <-
    "Expected observations N"

  names(SummaryData)[names(SummaryData) == "Sysmiss N"] <-
    "Sysmiss N (%)"
  SummaryData$`Sysmiss N (%)` <- paste0(sys_miss, " (", sys_miss_pct, ")")

  names(SummaryData)[names(SummaryData) == "Datavalues N"] <-
    "Datavalues N (%)"
  SummaryData$`Datavalues N (%)` <- paste0(
    data_values_n,
    " (",
    data_values_n_pct, ")"
  )

  names(SummaryData)[names(SummaryData) == "Missing codes N"] <-
    "Missing codes N (%)"
  SummaryData$`Missing codes N (%)` <- paste0(m, " (", missing_codes_pct, ")")

  names(SummaryData)[names(SummaryData) == "Jumps N"] <- "Jumps N (%)"
  SummaryData$`Jumps N (%)` <- paste0(j, " (", jumps_pct, ")")

  names(SummaryData)[names(SummaryData) == "Measurements N"] <-
    "Measurements N (%)"
  SummaryData$`Measurements N (%)` <- paste0(n_meas, " (", n_meas_pct, ")")

  names(SummaryData)[names(SummaryData) == "Missing_expected_obs"] <-
    "Missing expected obs. N (%)"
  SummaryData$`Missing expected obs. N (%)` <- paste0(
    missing_expected_obs,
    " (",
    missing_expected_obs_pct, ")"
  )

  names(SummaryData)[names(SummaryData) == "PCT_com_crm_mv"] <-
    "Crude missingness N (%)"
  SummaryData$`Crude missingness N (%)` <- paste0(
    sys_miss + j + m, " (",
    PCT_com_crm_mv, ")"
  )
  attr(SummaryData$Variables, DATA_TYPE) <- DATA_TYPES$STRING
  attr(SummaryData$`Expected observations N`, DATA_TYPE) <- DATA_TYPES$INTEGER
  attr(SummaryData$`Sysmiss N (%)`, DATA_TYPE) <- DATA_TYPE_NUMBER_PAREN
  attr(SummaryData$`Datavalues N (%)`, DATA_TYPE) <- DATA_TYPE_NUMBER_PAREN
  attr(SummaryData$`Missing codes N (%)`, DATA_TYPE) <- DATA_TYPE_NUMBER_PAREN
  attr(SummaryData$`Jumps N (%)`, DATA_TYPE) <- DATA_TYPE_NUMBER_PAREN
  attr(SummaryData$`Measurements N (%)`, DATA_TYPE) <- DATA_TYPE_NUMBER_PAREN
  attr(SummaryData$`Missing expected obs. N (%)`, DATA_TYPE) <-
    DATA_TYPE_NUMBER_PAREN
  attr(SummaryData$`Crude missingness N (%)`, DATA_TYPE) <-
    DATA_TYPE_NUMBER_PAREN
  SummaryData <- cbind(SummaryData, specified_reason_data)


  # to add formulas in hover text:
  # create a named vector with names = column names of the SummaryData and
  # values = what you want to be displayed in the hover text
  text_to_display <- util_get_hovertext("[com_item_missingness_hover]")


  # Historical MC/JC availability display columns removed here.

  r <- util_study_var2factor(
    study_data = study_data, meta_data = meta_data,
    resp_vars = resp_vars, label_col = label_col,
    assume_consistent_codes = assume_consistent_codes,
    have_cause_label_df = !missing(cause_label_df),
    include_sysmiss = include_sysmiss
  )
  colnames(r) <-
    util_map_labels(colnames(r),
      meta_data,
      to = VAR_NAMES,
      from = label_col
    )
  r <- util_seg_table(r, study_data, meta_data,
    expected_observations = expected_observations
  )
  r <- r[vapply(r, ncol, FUN.VALUE = integer(1)) == 2]
  r <- lapply(names(r), function(nm) setNames(r[[nm]], c("CODES", nm)))
  mctab <- Reduce(function(dtf1, dtf2) {
    merge(dtf1, dtf2,
      by = "CODES",
      all = TRUE
    )
  }, r)
  rownames(mctab) <- mctab$CODES
  mctab$CODES <- NULL
  mctab <- t(mctab)
  mctab[is.na(mctab)] <- 0
  mctab <- data.frame(mctab, check.names = FALSE)
  if (drop_levels) {
    mctab <- mctab[, colSums(mctab) != 0, drop = FALSE]
  }
  if (!ncol(mctab)) {
    mctab <- data.frame(
      check.names = FALSE,
      Variables = resp_vars,
      stringsAsFactors = FALSE
    )
  } else {
    mctab$Variables <- prep_map_labels(rownames(mctab),
      meta_data = meta_data,
      to = label_col,
      from = VAR_NAMES
    )
  }
  rownames(mctab) <- NULL
  mctab$N <-
    util_count_expected_observations(mctab$Variables,
      study_data = ds1,
      meta_data = meta_data,
      label_col = label_col,
      expected_observations = expected_observations
    )

  mctab <- util_new_report_summary_table(mctab,
    meta_data = meta_data,
    label_col = label_col
  )
  mctab <- util_set_report_summary_table_continuous(mctab, TRUE)
  mctab <- util_set_report_summary_table_relative(mctab, TRUE)

  attr(SummaryData, "description") <- text_to_display

  list(
    SummaryTable = summary_table,
    SummaryData = SummaryData,
    SummaryPlot = print(mctab, view = FALSE),
    ReportSummaryTable = mctab
  )
}
