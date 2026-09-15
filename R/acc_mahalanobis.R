# nolint start: line_length_linter.
#' Calculate and plot `Mahalanobis` distances
#'
#' @description
#' A standard tool to calculate `Mahalanobis` distance.
#' In this approach the squared `Mahalanobis` distance is calculated for ordinal
#' variables (treated as continuous) to identify inattentive responses.
#' It calculates the distance for each observational unit from the sample mean.
#' The greater the distance, the atypical the responses.
#'
#' [Indicator]
#'
#' @details
#' # ALGORITHM OF THIS IMPLEMENTATION:
#' - Implementation is restricted to variables of type integer
#' - Remove missing codes from the study data (if defined in the metadata)
#' - The covariance matrix is estimated for all variables from `variable_group`
#' - The `Mahalanobis` distance of each observation is calculated
#'   \eqn{MD^2_i  = (x_i - \mu)^T \Sigma^{-1} (x_i -  \mu)}
#' - The default to consider a value an `outlier` is to use the 0.975 quantile
#'    of a theoretical chi-square distribution with degrees of freedom
#'    equals to the number of variables used to calculate the
#'    `Mahalanobis` distance (`Mayrhofer and Filzmoser`, 2023)
#'
#' @inheritParams .template_function_indicator
#'
#' @param variable_group [variable list] the names of the variables used to
#'                                        calculate the `Mahalanobis` distance
#' @param mahalanobis_threshold [numeric] the confidence level to use to define
#'                                        `outliers`, if not stated it is by default
#'                                        0.975.
#'
#' @return a list with:
#'   - `SummaryTable`: [data.frame] underlying the plot
#'   - `SummaryData`: [data.frame] underlying the plot with speaking column labels
#'   - `SummaryPlot`: [ggplot2::ggplot2] Q-Q plot of squared `Mahalanobis`
#'                                        distances vs. a theoretical
#'                                        chi-squared distribution showing `outliers`.
#'   - `FlaggedStudyData`: [data.frame] contains the original data frame of the
#'                                      variables used to calculate
#'                                      the squared `Mahalanobis` distances
#'                                      with the additional column,
#'                                      containing the squared
#'                                       `Mahalanobis` distance, and a column
#'                                       called `MD_outliers`, that contains
#'                                       1 if the observational unit is considered
#'                                       a multivariate `outlier`.
#'
#' @export
#' @importFrom ggplot2 ggplot aes geom_path  scale_color_manual geom_point discrete_scale theme_minimal scale_alpha_manual
#'
#' @importFrom stats mahalanobis
#' @importFrom rlang .data
#' @seealso
#' [Online Documentation](
#' https://dataquality.qihs.uni-greifswald.de/VIN_acc_impl_multivariate_outlier.html
#' )
# nolint end
acc_mahalanobis <- function(variable_group = NULL,
  study_data,
  item_level = "item_level",
  meta_data = item_level,
  meta_data_cross_item = "cross-item_level",
  label_col = VAR_NAMES,
  meta_data_v2,
  cross_item_level,
  `cross-item_level`,
  mahalanobis_threshold =
    suppressWarnings(
      as.numeric(
        getOption(
          "dataquieR.MAHALANOBIS_THRESHOLD",
          dataquieR.MAHALANOBIS_THRESHOLD_default
        )
      )
    )) {
  # Preps------
  if (.called_in_pipeline) {
    util_error(
      m = "This function is not meant to run in the pipeline",
      intrinsic_applicability_problem = TRUE
    )
  }

  util_maybe_load_meta_data_v2()

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
  # Load cross-item_level metadata and normalize it ----
  # check if there is a cross item metadata and if it is a data frame
  # in case is not present, create an empty data frame for cross item metadata
  try(util_expect_data_frame(meta_data_cross_item), silent = TRUE)
  if (!is.data.frame(meta_data_cross_item)) {
    util_message(sprintf(
      "No cross-item level metadata %s found",
      dQuote(meta_data_cross_item)
    ))
    meta_data_cross_item <- data.frame(
      VARIABLE_LIST = character(0),
      CHECK_LABEL = character(0)
    )
  }

  # First normalize input for meta_data_cross_item from the user
  meta_data_cross_item <- util_normalize_cross_item(
    meta_data = meta_data,
    meta_data_cross_item = meta_data_cross_item,
    label_col = label_col
  )
  # map metadata to study data
  prep_prepare_dataframes(.replace_hard_limits = TRUE)


  # Define variable groups
  vars <- NULL
  if (!is.null(variable_group)) {
    util_correct_variable_use("variable_group",
      allow_more_than_one = TRUE,
      allow_any_obs_na = TRUE,
      need_type = "integer | float",
      need_scale = "interval | ratio | ordinal" # ES: should I leave only ordinal? # nolint: line_length_linter.
    )

    if (length(variable_group) == 1) {
      util_error("Need at least two variables for Mahalanobis distance.",
        applicability_problem = TRUE
      )
    }

    vars <- setNames(list(variable_group[!is.na(variable_group)]), "variable_group") # nolint: line_length_linter.


    mahalanobis_threshold <- util_normalize_mahalanobis_threshold(
      mahalanobis_threshold,
      context = dQuote("mahalanobis_threshold")
    )

    mahalanobis_threshold <- setNames(list(mahalanobis_threshold), "variable_group") # nolint: line_length_linter.
  } else {
    given_mahal_cols <- intersect(
      MAHALANOBIS_THRESHOLD,
      colnames(meta_data_cross_item)
    )
    if (!all(util_empty(as.vector(meta_data_cross_item[
      , given_mahal_cols,
      drop = FALSE
    ])))) {
      # reduce the cross-item_level content to only the rows that contains
      # mahal. info
      cur_cross <-
        meta_data_cross_item[
          !util_empty(meta_data_cross_item[[MAHALANOBIS_THRESHOLD]]), ,
          FALSE
        ]
      if (nrow(cur_cross) == 0) {
        vars <- NULL
      } else {
        # Select only the column of interest
        cur_cross <- cur_cross[
          , intersect(
            c(
              VARIABLE_LIST,
              CHECK_ID,
              CHECK_LABEL,
              DATA_PREPARATION,
              MAHALANOBIS_THRESHOLD,
              MAHALANOBIS_RATIO
            ),
            colnames(cur_cross)
          ),
          drop = FALSE
        ]

        cur_cross[[MAHALANOBIS_THRESHOLD]] <- mapply(
          x = cur_cross[[MAHALANOBIS_THRESHOLD]],
          label = cur_cross[[CHECK_LABEL]],
          SIMPLIFY = TRUE,
          FUN = function(x, label) {
            util_normalize_mahalanobis_threshold(
              x,
              context = sprintf(
                "%s for check %s",
                dQuote(MAHALANOBIS_THRESHOLD),
                dQuote(label)
              )
            )
          }
        )


        vars <- setNames(lapply(cur_cross$VARIABLE_LIST, function(a) {
          unname(unlist(util_parse_assignments(a)))
        }), cur_cross$CHECK_LABEL)

        mahalanobis_threshold <- as.list(setNames(
          as.numeric(cur_cross$MAHALANOBIS_THRESHOLD),
          cur_cross$CHECK_LABEL
        ))
      }
    }
  }

  # vars is a list
  if (is.null(vars)) {
    util_error("No variables provided to calculate Mahalanobis distance",
      applicability_problem = TRUE
    )
  }

  mahalanobis_results <- Map(
    f = function(rv, check_label) {
      util_mahalanobis_group_result(
        study_data = ds1,
        rv = rv,
        check_label = check_label,
        mahalanobis_threshold = mahalanobis_threshold[[check_label]]
      )
    },
    rv = vars,
    check_label = names(vars)
  )
  names(mahalanobis_results) <- names(vars)

  plot_list <- lapply(mahalanobis_results, `[[`, "SummaryPlot")
  sumdat <- do.call(
    rbind.data.frame,
    lapply(mahalanobis_results, `[[`, "SummaryData")
  )

  SummaryTable <- sumdat
  names(SummaryTable)[names(SummaryTable) == "MD_outliers (N)"] <- "NUM_ssc_mah"
  names(SummaryTable)[names(SummaryTable) == "MD_outliers (%)"] <- "PCT_ssc_mah"
  attr(SummaryTable$Variables, DATA_TYPE) <- DATA_TYPES$STRING
  attr(SummaryTable$NUM_ssc_mah, DATA_TYPE) <- DATA_TYPES$INTEGER
  attr(SummaryTable$PCT_ssc_mah, DATA_TYPE) <- DATA_TYPES$FLOAT
  attr(SummaryTable$N, DATA_TYPE) <- DATA_TYPES$INTEGER
  attr(SummaryTable$observational_units_removed, DATA_TYPE) <-
    DATA_TYPES$INTEGER
  attr(SummaryTable$mahalanobis_threshold, DATA_TYPE) <- DATA_TYPES$FLOAT

  attr(sumdat$Variables, DATA_TYPE) <- DATA_TYPES$STRING
  attr(sumdat$`MD_outliers (N)`, DATA_TYPE) <- DATA_TYPES$INTEGER
  attr(sumdat$`MD_outliers (%)`, DATA_TYPE) <- DATA_TYPES$FLOAT
  attr(sumdat$N, DATA_TYPE) <- DATA_TYPES$INTEGER
  attr(sumdat$observational_units_removed, DATA_TYPE) <- DATA_TYPES$INTEGER
  attr(sumdat$mahalanobis_threshold, DATA_TYPE) <- DATA_TYPES$FLOAT


  FlaggedStudyData <- lapply(mahalanobis_results, `[[`, "FlaggedStudyData")

  if (length(FlaggedStudyData) == 1) {
    FlaggedStudyData_all <- FlaggedStudyData[[1]]
    FlaggedStudyData_all <-
      FlaggedStudyData_all[, names(FlaggedStudyData_all) != "row_n"]
  } else {
    row_counts <- sapply(FlaggedStudyData, nrow)
    if (length(unique(row_counts)) != 1) {
      util_error("Internal error, sorry: The original data frame should have the same number of rows. Please report") # nolint: line_length_linter.
    }
    FlaggedStudyData_all <- Reduce(function(x, y) {
      extra_cols <- c("row_n", setdiff(colnames(y), colnames(x)))
      merge(x, y[, extra_cols, drop = TRUE], by = "row_n", all = TRUE)
    }, FlaggedStudyData)


    FlaggedStudyData_all <-
      FlaggedStudyData_all[, names(FlaggedStudyData_all) != "row_n"]
  }

  return(list(
    SummaryTable = SummaryTable,
    SummaryData = sumdat,
    SummaryPlotList = plot_list,
    FlaggedStudyData = FlaggedStudyData_all
  ))
}
