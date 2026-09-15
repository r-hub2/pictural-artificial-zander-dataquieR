#' Internal function only existing for technical reasons.
#'
#' Planned to be removed in future releases.
#'
#' `r lifecycle::badge("experimental")`
#'
#' @description Report the sum of attention check item mismatches.
#'
#' [Indicator]
#'
#' @details
#' # ALGORITHM OF THIS IMPLEMENTATION:
#' - Implementation is restricted to attention check variables (e.g., bogus, or
#' instructed response items)
#' - Remove missing codes from the study data (if defined in the metadata)
#' - Consider missing values as non-matching expectations
#' - The matrix is estimated for all variables of `resp_vars`
#' (variables of `ITEM_TYPE` BOGUS or INSTRUCTED, provided in the cross-item
#' level or obtained from item_level column `ITEM_TYPE`)
#' - Sum of non-matching values (1 if non-matching or missing variables, 0
#' otherwise)
#' - By default is problematic if the percentage of attention check variables
#' non-matching the expected values is equal to or
#' greater than 50% (`Curran` 2016)
#'
#' @inheritParams .template_function_indicator
#'
#' @param meta_data_cross_item [data.frame] -- Cross-item level metadata
#'
#'
#' @return a list with:
#'   - `SummaryData`: [data.frame] table with user friendly caption
#'   - `SummaryTable`: [data.frame] table with indicators
#'   - `FlaggedStudyData` [data.frame] contains the original data frame of the
#'                                      variables used to calculate
#'                                      the sum of attention check items
#'                                      with an additional column indicating if
#'                                      the
#'                                      observational unit has a percentage of
#'                                      incorrect bogus or instructed items
#'                                      greater than the threshold.
#'
#' @export
#'
con_attention_check_items <- function(
  resp_vars = NULL,
  study_data,
  label_col = VAR_NAMES,
  item_level = "item_level",
  meta_data = item_level,
  meta_data_v2,
  meta_data_cross_item = "cross-item_level",
  cross_item_level,
  `cross-item_level`
) {
  # preps -----------------------------------------------
  util_maybe_load_meta_data_v2()

  # Load cross-item_level metadata and normalize it ----
  # check if there is a cross item metadata and if it is a data frame
  # in case is not present, create an empty data frame for cross item metadata
  try(util_expect_data_frame(meta_data_cross_item), silent = TRUE)
  if (!is.data.frame(meta_data_cross_item)) {
    util_message(sprintf(
      "No cross-item level metadata %s found",
      sQuote(meta_data_cross_item)
    ))
    meta_data_cross_item <- data.frame(
      VARIABLE_LIST = character(0),
      CHECK_LABEL = character(0),
      CHECK_ID = character(0)
    )
  }

  # First normalize input for meta_data_cross_item from the user
  meta_data_cross_item <- util_normalize_cross_item(
    meta_data = meta_data,
    meta_data_cross_item = meta_data_cross_item,
    label_col = label_col
  )

  # Check label_col
  if (missing(label_col)) {
    orig_label_col <- rlang::missing_arg()
  } else {
    orig_label_col <- force(label_col)
  }

  label_col <- util_attr(
    prep_get_labels("",
      item_level = meta_data,
      label_class = "SHORT",
      label_col = label_col
    ),
    "label_col",
    exact = TRUE
  )

  # map metadata to study data
  prep_prepare_dataframes(.replace_hard_limits = FALSE)

  # check for variable role in the metadata for the resp_var
  util_correct_variable_use(resp_vars,
    need_type = DATA_TYPES$FLOAT,
    need_scale = SCALE_LEVELS$RATIO,
    need_computed_role = COMPUTED_VARIABLE_ROLES$SUM_ATTENTION_CHECK_ITEMS
  )

  # Check resp_vars
  util_expect_scalar(
    arg_name = resp_vars,
    allow_more_than_one = FALSE,
    check_type = is.character
  )


  # select current variable from data ------------------------------------
  # Select CHECK_ID of the current variables group
  current_check_id <- util_map_labels(
    resp_vars,
    meta_data = meta_data,
    to = CHECK_ID,
    from = label_col
  )
  all_checkID_with_vars <- setNames(meta_data_cross_item[[VARIABLE_LIST]],
    nm = meta_data_cross_item[[CHECK_ID]]
  )
  intermediate2 <- lapply(
    util_parse_assignments(all_checkID_with_vars,
      multi_variate_text = TRUE
    ),
    lapply,
    prep_get_labels,
    label_col = VAR_NAMES,
    force_label_col = "TRUE",
    item_level = meta_data
  )
  intermediate3 <- lapply(intermediate2, unique)
  no_vars_per_check_ID <- vapply(
    lapply(
      intermediate3,
      function(vl) intersect(unlist(vl), meta_data[[VAR_NAMES]])
    ), length,
    FUN.VALUE = integer(1)
  )
  tot_no_vars <- as.numeric(no_vars_per_check_ID[current_check_id])
  rm(all_checkID_with_vars, intermediate2, intermediate3, no_vars_per_check_ID)


  FlaggedStudyData <- ds1
  current_sum_of_vars <- ds1[[resp_vars]]
  percentage_col <- "Percentage of non-matching attention check items"
  FlaggedStudyData[[percentage_col]] <- round(
    current_sum_of_vars / tot_no_vars * 100,
    digits = 2
  )


  # Define the threshold ----
  range_hard_limits <- meta_data_cross_item[
    meta_data_cross_item[[CHECK_ID]] == current_check_id,
    SUM_ATTENTION_CHECK_ITEMS,
    drop = TRUE
  ]
  util_expect_scalar(range_hard_limits, check_type = is.character)
  range_hard_limits <- util_parse_interval(range_hard_limits)
  if (!inherits(range_hard_limits, "interval")) {
    util_error("Invalid interval in %s for %s.",
      dQuote(SUM_ATTENTION_CHECK_ITEMS),
      dQuote(current_check_id),
      applicability_problem = TRUE
    )
  }


  # new complete data with the column indicating the outliers
  FlaggedStudyData$incorrect_responses_over_threshold <- NA
  below <- if (range_hard_limits$inc_l) {
    current_sum_of_vars < range_hard_limits$low
  } else {
    current_sum_of_vars <= range_hard_limits$low
  }
  above <- if (range_hard_limits$inc_u) {
    current_sum_of_vars > range_hard_limits$upp
  } else {
    current_sum_of_vars >= range_hard_limits$upp
  }
  FlaggedStudyData$incorrect_responses_over_threshold <- ifelse(
    is.na(current_sum_of_vars) | below | above,
    1,
    0
  )
  n_non_careless <- sum(
    FlaggedStudyData$incorrect_responses_over_threshold == 0,
    na.rm = TRUE
  )
  n_careless <- sum(
    FlaggedStudyData$incorrect_responses_over_threshold == 1,
    na.rm = TRUE
  )

  # create summary table
  st1 <- data.frame(Variables = resp_vars)
  st1$"Check items (N)" <- tot_no_vars
  st1$"Cases with incorrect responses (N)" <- n_careless
  st1$"Cases with incorrect responses (%)" <- round(
    n_careless / nrow(FlaggedStudyData) * 100,
    digits = 2
  )
  st1$"Cases with correct responses (N)" <- n_non_careless
  st1$"N" <- nrow(FlaggedStudyData)

  SummaryData <- st1
  SummaryTable <- st1
  names(SummaryTable)[
    names(SummaryTable) == "Cases with incorrect responses (N)"
  ] <- "NUM_ssc_sumattit"
  names(SummaryTable)[
    names(SummaryTable) == "Cases with incorrect responses (%)"
  ] <- "PCT_ssc_sumattit"
  SummaryTable <- SummaryTable[, c(
    "Variables",
    "NUM_ssc_sumattit",
    "PCT_ssc_sumattit"
  ),
  drop = FALSE
  ]


  SummaryData$"Cases with incorrect responses N (%)" <-
    paste0(
      SummaryData$"Cases with incorrect responses (N)",
      " (",
      SummaryData$"Cases with incorrect responses (%)",
      "%)"
    )

  SummaryData <- SummaryData[, c(
    "Variables",
    "Check items (N)",
    "Cases with incorrect responses N (%)",
    "N"
  ),
  drop = FALSE
  ]


  # Add data types for rendering SummaryData columns.
  attr(SummaryData$Variables, DATA_TYPE) <- DATA_TYPES$STRING
  attr(SummaryData$`Check items (N)`, DATA_TYPE) <- DATA_TYPES$INTEGER
  attr(
    SummaryData$`Cases with incorrect responses N (%)`,
    DATA_TYPE
  ) <- DATA_TYPES$STRING
  attr(SummaryData$N, DATA_TYPE) <- DATA_TYPES$INTEGER

  return(list(
    FlaggedStudyData = FlaggedStudyData,
    SummaryTable = SummaryTable,
    SummaryData = SummaryData
  ))
}
