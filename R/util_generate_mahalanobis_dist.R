# nolint start: line_length_linter.
#' Utility function to calculate Mahalanobis distances for a group of variables
#'
#' @param x [data.frame] containing the variables to use to
#'                        calculate the Mahalanobis distance
#' @param rv a vector containing the names of the variables
#' @param check_label_rv [character] the label of the check list used to define
#'                                    the group of variables in the cross-item metadata
#'                                    or "variable_group" if the list of variables
#'                                    is provided as an argument
#'
#' @return a list with:
#'   - `x_with_MD`: [data.frame] containing the original variables with the
#'                                addition of a new column containing the
#'                                Mahalanobis distance (only for complete cases)
#'   - `df`: [numeric] the degree of freedom (no.variables)
#'
#' @family util_functions
#' @concept outlier
#' @keywords internal
#'
#' @noRd
# nolint end
util_generate_mahalanobis_dist <- function(x,
  rv,
  check_label_rv) {
  x$row_numbers <- seq_len(nrow(x))
  n_prior <- dim(x)[1]
  ds1completecases <- x[rowSums(is.na(x[, rv, drop = FALSE])) == 0, ,
    drop = FALSE
  ]


  n_post <- dim(ds1completecases)[1]

  if (n_post == 0) {
    util_error("No observational unit with complete cases for all variables %s. Aborting.", # nolint: line_length_linter.
      paste0(sQuote(rv), collapse = ", "),
      applicability_problem = FALSE
    )
  }

  if (n_post < n_prior) {
    util_message(paste0(
      "Due to missing values",
      " N=", n_prior - n_post,
      " observational units were excluded."
    ), applicability_problem = FALSE)
  }

  # Mahalanobis ----------------------------------------------------------------
  # no. variables used to calculate the MD
  degree_freedom <- ncol(ds1completecases[, rv, drop = TRUE]) # NOTE: this is the no. columns or no. variables # nolint: line_length_linter.

  # Estimate covariance of response variables
  sx <- cov(ds1completecases[, rv, drop = FALSE])

  # Calculate squared Mahalanobis distance
  ds1completecases[[paste0("MD_", check_label_rv)]] <-
    mahalanobis(
      ds1completecases[, rv, drop = TRUE],
      colMeans(ds1completecases[, rv, drop = FALSE]),
      sx
    )

  ds1completecases <- merge(x,
    ds1completecases,
    by = intersect(colnames(x), colnames(ds1completecases)),
    all.x = TRUE
  )

  ds1completecases <- ds1completecases[, names(ds1completecases) != "row_numbers", drop = FALSE] # nolint: line_length_linter.


  return(list(
    x_with_MD = ds1completecases,
    df = degree_freedom
  ))
}

#' Normalize Mahalanobis threshold metadata or arguments
#'
#' @param x threshold value
#' @param context [character] context for diagnostics
#'
#' @return [numeric] one finite probability in `(0, 1)`
#'
#' @family util_functions
#' @concept outlier
#' @keywords internal
#'
#' @noRd
util_normalize_mahalanobis_threshold <- function(x,
  context = dQuote(MAHALANOBIS_THRESHOLD)) {
  use_default <- function() {
    util_message(
      "Invalid %s. Using default (%g).",
      context,
      dataquieR.MAHALANOBIS_THRESHOLD_default,
      applicability_problem = TRUE
    )
    dataquieR.MAHALANOBIS_THRESHOLD_default
  }

  if (length(x) != 1 || util_empty(x)) {
    return(use_default())
  }

  x_chr <- trimws(as.character(x))
  if (tolower(x_chr) %in% c("true", "1", "t", "+")) {
    return(dataquieR.MAHALANOBIS_THRESHOLD_default)
  }

  x_num <- suppressWarnings(as.numeric(x_chr))
  if (!is.finite(x_num) || x_num <= 0 || x_num >= 1) {
    return(use_default())
  }

  x_num
}

#' Build all Mahalanobis outputs for one variable group
#'
#' @param study_data [data.frame] prepared study data
#' @param rv [character] variables in the Mahalanobis group
#' @param check_label [character] label for the group
#' @param mahalanobis_threshold [numeric] threshold probability
#'
#' @return [list] with `SummaryData`, `SummaryPlot`, and `FlaggedStudyData`
#'
#' @family util_functions
#' @concept outlier
#' @keywords internal
#'
#' @noRd
util_mahalanobis_group_result <- function(study_data,
  rv,
  check_label,
  mahalanobis_threshold) {
  ds1_group <- study_data[, rv, drop = FALSE]
  md_res <- util_generate_mahalanobis_dist(
    ds1_group,
    rv,
    check_label
  )
  ds1_group <- md_res$x_with_MD
  degree_freedom <- md_res$df
  rm(md_res)

  md_outliers_threshold <- unname(stats::qchisq(
    mahalanobis_threshold,
    df = degree_freedom
  ))
  md_col <- paste0("MD_", check_label)
  outlier_col <- paste0("MD_outliers_", check_label)

  ds1_group[[outlier_col]] <- ifelse(
    ds1_group[[md_col]] > md_outliers_threshold,
    1,
    0
  )

  ds1plot <- ds1_group[rowSums(is.na(ds1_group[, rv, drop = FALSE])) == 0, ,
    drop = FALSE
  ]
  ds1plot <- ds1plot[order(ds1plot[[md_col]]), , drop = FALSE]
  ds1plot$MD_ratio <- ds1plot[[md_col]] / md_outliers_threshold

  res_md <- util_create_mahalanobis_ggplot(
    md_ratio = ds1plot$MD_ratio,
    mahalanobis_threshold = mahalanobis_threshold,
    df = degree_freedom
  )

  n_devs <- sum(ds1_group[[outlier_col]] == 1, na.rm = TRUE)
  nas_obs_units <- sum(rowSums(is.na(ds1_group[, rv, drop = FALSE])) > 0)
  nrows_completecases <- nrow(ds1_group) - nas_obs_units

  SummaryData <- data.frame(Variables = check_label)
  SummaryData$"MD_outliers (N)" <- n_devs
  SummaryData$"MD_outliers (%)" <- round(
    n_devs / nrows_completecases * 100,
    digits = 2
  )
  SummaryData$"N" <- nrows_completecases
  SummaryData$"observational_units_removed" <- nas_obs_units
  SummaryData$"mahalanobis_threshold" <- mahalanobis_threshold

  ds1_group$row_n <- seq_len(nrow(ds1_group))

  list(
    SummaryData = SummaryData,
    SummaryPlot = res_md$plot_MD,
    FlaggedStudyData = ds1_group
  )
}
