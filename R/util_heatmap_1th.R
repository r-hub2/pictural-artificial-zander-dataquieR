#' Utility Function Heatmap with 1 Threshold
#'
#' Function to create heatmap-like plot given one threshold -- works for
#' percentages for now.
#'
#' @param df [data.frame] with data to display as a heatmap.
#' @param cat_vars [variable list] len=1-2. Variables to group by. Up to 2
#'                                          group levels supported.
#' @param values [variable] the name of the percentage variable
#' @param threshold [numeric] lowest acceptable value
#' @param right_intv [logical] len=1. If `FALSE` (default), intervals used
#'                                    to define color ranges in the heatmap
#'                                    are closed on the left side, if `TRUE` on
#'                                    the right side, respectively.
#' @param invert [logical] len=1. If `TRUE`, high values are better, warning
#'                                colors are used for low values. `FALSE` works
#'                                vice versa.
#' @param cols deprecated, ignored.
#' @param strata [variable] optional, the name of a variable
#'                          used for stratification
#' `inheritParams` `acc_distributions`
#' @return a [list] with:
#'   - `SummaryPlot`: [ggplot2::ggplot] object with the heatmap
#'
#' @importFrom ggplot2 ggplot aes geom_bar geom_text scale_fill_manual
#'                     scale_y_discrete coord_flip facet_grid theme_minimal
#'                     scale_x_discrete
#'
#'
#' @family figure_functions
#' @concept summary
#' @keywords internal
util_heatmap_1th <- function(df, cat_vars, values, threshold, right_intv,
                             invert, cols, strata
                             # , flip_mode # TODO: pass through from all usages of this function
                             ) {

  # STOPs
  if (!(length(cat_vars) %in% c(1, 2))) {
    util_error(paste0(
      "Argument cat_vars can have 1 or 2 elements. You specified ",
      length(cat_vars), "elements."), applicability_problem = TRUE)
  }

  if (!(is.numeric(df[[values]]))) {
    util_error(paste0("The variable you specified under ",
                      values, " must be numeric."),
               applicability_problem = TRUE,
               intrinsic_applicability_problem = TRUE)
  }

  if (missing(threshold)) {
    util_error(paste0("No threshold has been specified"),
               applicability_problem = TRUE)
  }

  # Preps
  df$z2 <- df[[values]]

  if (missing(right_intv)) {
    right_intv <- FALSE
  }

  # maximum of data values
  if (invert == 0) {
    maxz <- max(df$z2, na.rm = TRUE)
  } else {
    maxz <- min(df$z2, na.rm = TRUE)
  }

  # interpret threshold as lowest acceptable value
  minz <- threshold

  warn_cols <- c(
    "#7f0000", "#b30000", "#d7301f", "#ef6548", "#fc8d59",
    "#fdbb84", "#fdd49e", "#fee8c8", "#2166AC"
  )

  range <- maxz - minz

  # values below thresholds == BLUE, above threshold a range to darkred
  steps <- range / (length(warn_cols) - 1)

  # create step-wise color-coding
  midrange <- c(threshold)
  nsteps <- (range / steps - 1)
  if (is.finite(nsteps)) {
    for (i in 1:nsteps) {
      midrange <- c(midrange, i * steps + threshold)
    }
  }

  # extract unique levels (if range is too small)
  midrange <- unique(round(midrange, 2))

  # Categorize values
  df[[values]] <- cut(df[[values]], breaks = c(-Inf, midrange, Inf),
                      right = right_intv)

  if (invert == 0) {
    disc_cols <- rev(warn_cols)
    names(disc_cols) <- levels(df[[values]])
  } else {
    disc_cols <- warn_cols
    names(disc_cols) <- levels(df[[values]])
  }

  # allocate factor variables to x and y axis according to number of levels
  if (length(cat_vars) == 2) {
    df[[cat_vars[1]]] <- factor(df[[cat_vars[1]]])
    df[[cat_vars[2]]] <- factor(df[[cat_vars[2]]])

    A <- length(levels(df[[cat_vars[1]]]))
    B <- length(levels(df[[cat_vars[2]]]))

    if (A < B) {
      df$x <- df[[cat_vars[1]]]
      df$y <- df[[cat_vars[2]]]
      namex <- cat_vars[1]
      namey <- cat_vars[2]
    } else {
      df$x <- df[[cat_vars[2]]]
      df$y <- df[[cat_vars[1]]]
      namex <- cat_vars[2]
      namey <- cat_vars[1]
    }

    if (!missing(strata)) {
      p <- ggplot(df, aes(x, y, fill = .data[[values]])) +
        facet_grid(.data[[strata]] ~ .) + # TODO: test ~
        geom_tile(colour = "white", linewidth = 0.8) + # https://github.com/tidyverse/ggplot2/issues/5051
        geom_text(label = paste0(round(df$z2, 2), " %")) +
        scale_fill_manual(values = disc_cols, name = " ") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
        scale_x_discrete(name = namex) +
        scale_y_discrete(expand = c(0, 0), name = namey,
                         limits = rev(levels(df$y))) +
        xlab("Study segments")
    } else {
      p <- ggplot(df, aes(x, y, fill = .data[[values]])) +
        geom_tile(colour = "white", linewidth = 0.8) + # https://github.com/tidyverse/ggplot2/issues/5051
        geom_text(label = paste0(round(df$z2, 2), " %")) +
        scale_fill_manual(values = disc_cols, name = " ") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
        scale_x_discrete(name = namex) +
        scale_y_discrete(expand = c(0, 0), name = namey,
                         limits = rev(levels(df$y))) +
        xlab("Study segments")
    }
  } else {
    namex <- cat_vars

    p <- ggplot(df, aes(.data[[cat_vars]], y = z2, fill = .data[[values]])) +
      geom_bar(stat = "identity", na.rm = TRUE) +
      geom_text(label = paste0(" ", round(df$z2, digits = 2), "%"),
                hjust = 0, vjust = 0.5) +
      scale_fill_manual(values = disc_cols, name = " ") +
      theme_minimal() +
      scale_x_discrete(name = namex) +
      scale_y_continuous(name = "(%)",
                         limits = c(0, 1.2 * max(df$z2))) +
      coord_flip() # TODO: use util_coord_flip
  }

  return(list(SummaryPlot = p))
}
