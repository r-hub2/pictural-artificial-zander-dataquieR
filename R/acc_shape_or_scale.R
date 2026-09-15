# nolint start: line_length_linter.
#' Compare observed versus expected distributions
#'
#' @description
#' This implementation contrasts the empirical distribution of a measurement
#' variables against assumed distributions. The approach is adapted from the
#' idea of rootograms (Tukey 1977) which is also applicable for count data
#' (Kleiber and Zeileis 2016).
#'
#' [Indicator]
#'
#' @details
#' # ALGORITHM OF THIS IMPLEMENTATION:
#'   - `resp_vars` identifies one interval or ratio scaled measurement variable
#'     of type float or integer.
#'   - Missing codes are removed from resp_vars (if defined in the metadata)
#'   - The user must specify the column of the metadata containing probability
#'     distribution (currently only: normal, uniform, gamma)
#'  - Parameters of each distribution can be estimated from the data or are
#'    specified by the user
#'  - A histogram-like plot contrasts the empirical vs. the technical
#'    distribution
#'
#' @inheritParams .template_function_indicator
#'
#' @param dist_col [variable attribute] the name of the variable attribute in
#'                                      meta_data that provides the expected
#'                                      distribution of a study variable
#' @param guess [logical] estimate parameters
#' @param par1 [numeric] first parameter of the distribution if applicable
#' @param par2 [numeric] second parameter of the distribution if applicable
#' @param end_digits [logical] internal use. check for end digits preferences
#' @inheritParams acc_distributions
#'
#' @return a list with:
#'   - `ResultData`: [data.frame] underlying the plot
#'   - `SummaryPlot`: [ggplot2::ggplot2] probability distribution plot
#'   - `SummaryTable`: [data.frame] with the columns `Variables` and `FLG_acc_ud_shape`
#'
#' @export
#' @importFrom ggplot2 ggplot theme_minimal geom_bar aes scale_fill_manual geom_line geom_hline xlab scale_color_manual geom_errorbar
#' @importFrom stats dunif dgamma dnorm optim punif pgamma pnorm median
#' @seealso
#' [Online Documentation](
#' https://dataquality.qihs.uni-greifswald.de/VIN_acc_impl_shape_or_scale.html
#' )
# nolint end
acc_shape_or_scale <- function(resp_vars,
  study_data,
  label_col,
  item_level = "item_level",
  dist_col,
  guess,
  par1, par2,
  end_digits,
  flip_mode = "noflip",
  meta_data = item_level,
  meta_data_v2) {
  util_maybe_load_meta_data_v2()
  # map metadata to study data
  prep_prepare_dataframes(
    .replace_hard_limits = TRUE,
    .replace_missings = TRUE
  )

  # correct variable use?
  util_correct_variable_use("resp_vars",
    need_type = "integer | float",
    need_scale = "interval | ratio"
  )

  if (missing(dist_col)) {
    dist_col <- DISTRIBUTION
    # util_message(
    #   c("A column of the metaddata specifying the distributions has",
    #   "not been specified. Trying the default %s."),
    #   dQuote(DISTRIBUTION), applicability_problem = TRUE)
  }

  if (!(dist_col %in% colnames(meta_data))) {
    util_error("Did not find variable attribute %s in the meta_data",
      dist_col,
      applicability_problem = TRUE,
      intrinsic_applicability_problem = TRUE
    )
  }

  if (missing(guess) || is.na(guess[[1]])) {
    if (!missing(guess) && length(guess) > 1) {
      util_message("Have more than one value for guess, use the first one only",
        applicability_problem = TRUE
      )
    }
    if (!missing(par1) && !missing(par2)) {
      guess <- FALSE
      util_message("Since parameters were specified: 'guess' is set to false",
        applicability_problem = TRUE
      )
    } else {
      guess <- TRUE
    }
  }

  if (!(missing(end_digits)) && length(end_digits) > 1) {
    util_message(
      c(
        "end_digits should be a scalar logical value. Have more than one",
        "value, use the first one only"
      ),
      applicability_problem = TRUE
    )
    end_digits <- end_digits[[1]]
  }

  if (!missing(guess) && length(guess) > 1) {
    util_message(
      c(
        "guess should be a scalar logical value.",
        "Have more than one value, use the first one only"
      ),
      applicability_problem = TRUE
    )
    guess <- guess[[1]]
  }

  if (!missing(par1) && length(par1) > 1) {
    util_message(
      c(
        "par1 should be a scalar numeric value. Have more than one value,",
        "use the first one only"
      ),
      applicability_problem = TRUE
    )
    par1 <- par1[[1]]
  }

  if (!missing(par2) && length(par2) > 1) {
    util_message(
      c(
        "par2 should be a scalar numeric value.",
        "Have more than one value, use the first one only"
      ),
      applicability_problem = TRUE
    )
    par2 <- par2[[1]]
  }

  if (!missing(guess) && !all(is.logical(guess))) {
    guess <- suppressWarnings(as.logical(guess))
    if ((!all(is.logical(guess))) || any(is.na(guess))) {
      util_error("guess should be a logical value",
        applicability_problem = TRUE
      )
    }
  }

  if (!missing(end_digits) && !all(is.logical(end_digits))) {
    end_digits <- suppressWarnings(as.logical(end_digits))
    if ((!all(is.logical(end_digits))) || any(is.na(end_digits))) {
      util_error("end_digits should be a logical value",
        applicability_problem = TRUE
      )
    }
  }

  if (!missing(par1) && !all(is.numeric(par1))) {
    par1 <- suppressWarnings(as.numeric(par1))
    if ((!all(is.numeric(par1))) || any(is.na(par1))) {
      util_error("par1 should be a numeric value",
        applicability_problem = TRUE
      )
    }
  }

  if (!missing(par2) && !all(is.numeric(par2))) {
    par2 <- suppressWarnings(as.numeric(par2))
    if ((!all(is.numeric(par2))) || any(is.na(par2))) {
      util_error("par2 should be a numeric value",
        applicability_problem = TRUE
      )
    }
  }

  if (length(ds1[[resp_vars]]) == 0L || mode(ds1[[resp_vars]]) != "numeric") {
    util_error("resp_vars == '%s' must be a non-empty numeric variable",
      resp_vars,
      applicability_problem = TRUE,
      intrinsic_applicability_problem = TRUE
    )
  }

  # missings in resp_vars?
  n_prior <- dim(ds1)[1]
  ds1 <- ds1[!(is.na(ds1[[resp_vars]])), , drop = FALSE]
  n_post <- dim(ds1)[1]

  if (n_post < n_prior) {
    util_message(paste0("Due to missing values in ", resp_vars, " ",
        n_prior - n_post,
        " observations were deleted.",
        collapse = " "
      ), applicability_problem = FALSE)
  }

  if (guess == FALSE && (missing(par1) || missing(par2) ||
        !is.finite(par1) || !is.finite(par2))) {
    util_error(
      c(
        "Since 'guess' is not true finite numerical",
        "parameters must be prespecified"
      ),
      applicability_problem = TRUE
    )
  }

  if (missing(end_digits)) {
    end_digits <- FALSE
  }

  if (
    !is.character(meta_data[meta_data[[label_col]] == resp_vars, dist_col, drop = TRUE]) || # nolint: line_length_linter.
      is.na(meta_data[meta_data[[label_col]] == resp_vars, dist_col, drop = TRUE]) || # nolint: line_length_linter.
      trimws(meta_data[meta_data[[label_col]] == resp_vars, dist_col, drop = TRUE]) == "") { # nolint: line_length_linter.
    util_error(
      paste0("No distribution specified for ", resp_vars, " in ",
        dist_col,
        collapse = ""
      ),
      applicability_problem = TRUE,
      intrinsic_applicability_problem = TRUE
    )
  }

  dist <- paste0(meta_data[meta_data[[label_col]] == resp_vars, dist_col, drop = TRUE]) # nolint: line_length_linter.

  if (end_digits == TRUE) {
    dist <- "uniform"
  }

  if (guess && dist == "uniform") {
    par1 <- min(ds1[[resp_vars]], na.rm = TRUE)
    par2 <- max(ds1[[resp_vars]], na.rm = TRUE)
  }

  if (guess && dist %in% c("normal", "gamma")) {
    estimated_pars <- suppressWarnings(util_fit_distribution_params(
      ds1[[resp_vars]],
      dist
    ))
    par1 <- estimated_pars[[1]]
    par2 <- estimated_pars[[2]]
  }

  # assign densities/probability arguments
  density_fun <- switch(dist,
    uniform = dunif,
    gamma = dgamma,
    normal = dnorm,
    NULL
  )
  probability_fun <- switch(dist,
    uniform = punif,
    gamma = pgamma,
    normal = pnorm,
    NULL
  )

  if (is.null(density_fun)) {
    util_error(sprintf("This distribution '%s' is not supported yet...", dist),
      applicability_problem = TRUE
    )
  }

  # create histogram
  h1 <- graphics::hist(ds1[[resp_vars]], plot = FALSE)
  nobs <- sum(h1$counts)

  if (dist == "uniform" && (par1 < min(h1$breaks) || par2 > max(h1$breaks))) {
    breaks <- seq(min(par1, min(h1$breaks)), max(par2, max(h1$breaks)), by = 1)
    h1 <- graphics::hist(ds1[[resp_vars]], breaks = breaks, plot = FALSE)
    nobs <- sum(h1$counts)
  }

  if (identical(
    getOption(
      "dataquieR.acc_shape_or_scale_ci",
      dataquieR.acc_shape_or_scale_ci_default
    ),
    "binomial"
  )) {
    ci <- util_binomial_ci
  } else if (identical(
    getOption(
      "dataquieR.acc_shape_or_scale_ci",
      dataquieR.acc_shape_or_scale_ci_default
    ),
    "multinomial"
  )) {
    ci <- util_multinomial_ci
  } else {
    util_error(
      "option %s should be either %s or %s",
      sQuote("dataquieR.acc_shape_or_scale_ci"),
      dQuote("multinomial"),
      dQuote("binomial")
    )
  }

  if (end_digits == FALSE) {
    if (all(util_is_integer(ds1[[resp_vars]])) &&
        length(unique(ds1[[resp_vars]])) < 20 && dist == "uniform") {
      df1 <- data.frame(
        INTERVALS = factor(unique(ds1[[resp_vars]])[
          order(unique(ds1[[resp_vars]]))
        ]),
        COUNT = as.vector(table(ds1[[resp_vars]])),
        PROB = as.vector(table(ds1[[resp_vars]])) / nobs,
        EXP_PROB = round(1 / length(unique(ds1[[resp_vars]])), digits = 2),
        EXP_COUNT = nobs * 1 / length(unique(ds1[[resp_vars]])),
        LOWER_CL = ci(as.vector(table(ds1[[resp_vars]])),
          alpha = 0.05 / (length(h1$mids) - 1)
        )[, 1, drop = TRUE],
        UPPER_CL = ci(as.vector(table(ds1[[resp_vars]])),
          alpha = 0.05 / (length(h1$mids) - 1)
        )[, 2, drop = TRUE]
      )
    } else {
      df1 <- data.frame(
        INTERVALS = h1$mids,
        COUNT = h1$counts,
        PROB = h1$counts / nobs,
        EXP_PROB = min(diff(h1$mids)) * density_fun(h1$mids, par1, par2),
        EXP_COUNT = nobs * diff(probability_fun(h1$breaks, par1, par2)),
        LOWER_CL = ci(h1$counts,
          alpha = 0.05 / (length(h1$mids) - 1)
        )[, 1, drop = TRUE],
        UPPER_CL = ci(h1$counts,
          alpha = 0.05 / (length(h1$mids) - 1)
        )[, 2, drop = TRUE]
      )
    }
  } else {
    ds1[[resp_vars]] <- factor(ds1[[resp_vars]], levels = 0:9)
    df1 <- data.frame(
      INTERVALS = factor(0:9),
      COUNT = as.vector(table(ds1[[resp_vars]])),
      PROB = as.vector(table(ds1[[resp_vars]])) / nobs,
      EXP_PROB = 0.1,
      EXP_COUNT = nobs * 0.1,
      LOWER_CL = ci(as.vector(table(ds1[[resp_vars]])),
        alpha = 0.05 / (length(h1$mids) - 1)
      )[, 1, drop = TRUE],
      UPPER_CL = ci(as.vector(table(ds1[[resp_vars]])),
        alpha = 0.05 / (length(h1$mids) - 1)
      )[, 2, drop = TRUE]
    )
  }

  df1$GRADING <- factor(ifelse(df1$EXP_PROB < df1$UPPER_CL &
        df1$EXP_PROB > df1$LOWER_CL, 0, 1))

  df1$PROB <- round(df1$PROB, digits = 2)
  df1$EXP_PROB <- round(df1$EXP_PROB, digits = 2)
  df1$EXP_COUNT <- round(df1$EXP_COUNT, digits = 2)
  df1$LOWER_CL <- round(df1$LOWER_CL, digits = 2)
  df1$UPPER_CL <- round(df1$UPPER_CL, digits = 2)


  x2 <- seq(floor(min(h1$breaks)), ceiling(max(h1$breaks)),
    length.out = length(h1$breaks) * 100
  )
  y_line <- min(diff(h1$breaks)) * density_fun(x2, par1, par2)

  if (end_digits) {
    y_line <- rep(0.1, length = length(x2))
  }

  if (all(util_is_integer(ds1[[resp_vars]])) &&
      length(unique(ds1[[resp_vars]])) < 20 && dist == "uniform") {
    y_line <- rep(round(1 / length(unique(ds1[[resp_vars]])), digits = 2),
      length = length(x2)
    )
  }

  df2 <- data.frame(
    x2 = x2,
    y_line = y_line
  )

  grading_cols <- c("#2166AC", "#B2182B")
  names(grading_cols) <- c("0", "1")

  p1 <- util_create_lean_ggplot(
    ggplot(df1, aes(x = INTERVALS, y = PROB)) +
      theme_minimal() +
      geom_bar(aes(fill = GRADING), stat = "identity") +
      scale_fill_manual(values = grading_cols, guide = "none") +
      geom_errorbar(aes(ymin = LOWER_CL, ymax = UPPER_CL), width = 0.1) +
      {
        if (!(end_digits) & dist != "uniform") {
          geom_line(
            data = df2, aes(x = x2, y = y_line, color = "#E69F00"),
            linewidth = 1
          )
        }
      } +
      {
        if (!(end_digits) & dist == "uniform") {
          geom_hline(
            yintercept = unique(y_line), color = "#E69F00",
            linewidth = 1
          )
        }
      } +
      {
        if (!(end_digits) & dist == "uniform") {
          xlab("Values")
        }
      } +
      {
        if (end_digits) {
          geom_hline(
            yintercept = 0.1, color = "#E69F00",
            linewidth = 1
          )
        }
      } +
      {
        if (end_digits) xlab("End digits")
      } +
      scale_color_manual(values = c("#E69F00"), guide = "none"),
    df1 = df1,
    grading_cols = grading_cols,
    end_digits = end_digits,
    x2 = x2,
    y_line = y_line,
    dist = dist,
    df2 = df2
  )

  p1 <- p1 + theme(
    legend.position = "none",
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 10)
  ) # also suppresses the legend in the ggplotly figure
  fli <- util_coord_flip(p = p1)
  p1 <- util_lazy_add_coord(p1, fli)
  is_flipped <- inherits(fli, "CoordFlip")
  p1 <- util_set_size(p1)
  # Put the old SummaryData table (that was not organized one row per variable)
  # inside the new result OtherTable that can be further processed
  OtherTable <- df1

  # Historical GRADING-column removal from OtherTable removed here.

  # Create the SummaryTable
  SummaryTable <- data.frame(
    Variables = resp_vars,
    FLG_acc_ud_shape = 1 - prod(1 - util_as_numeric(df1$GRADING))
  )


  no_intervals_flagged <- sum(as.numeric(as.character(df1$GRADING)))
  no_intervals_over_tot <- sum(as.numeric(as.character(df1$GRADING))) / nrow(df1) # nolint: line_length_linter.

  # Historical interval-count SummaryData branch removed here.

  return(util_attach_attr(list(
    SummaryPlot = p1,
    ResultData = OtherTable,
    SummaryTable = SummaryTable
  ), sizing_hints = list(
    figure_type_id = "bar_chart",
    rotated = is_flipped,
    number_of_bars = nrow(p1$data),
    range = max(p1$data$UPPER_CL) - min(p1$data$LOWER_CL),
    no_char_x = 4,
    no_char_y = 4
  )))
}

#' Estimate distribution parameters used by acc_shape_or_scale()
#'
#' @param x A non-empty numeric vector.
#' @param dist The distribution name. Currently supports `"normal"` and
#'   `"gamma"`.
#'
#' @return A named numeric vector with the estimated distribution parameters.
#' @noRd
util_fit_distribution_params <- function(x, dist) {
  if (length(x) == 0L || mode(x) != "numeric") {
    util_error(
      "'x' must be a non-empty numeric vector",
      applicability_problem = TRUE
    )
  }
  if (any(!is.finite(x))) {
    util_error(
      "'x' contains missing or infinite values",
      applicability_problem = TRUE
    )
  }

  if (identical(dist, "normal")) {
    mx <- mean(x)
    return(c(
      mean = mx,
      sd = sqrt(mean((x - mx)^2))
    ))
  }

  if (identical(dist, "gamma")) {
    if (any(x < 0)) {
      util_error(
        "gamma values must be >= 0",
        applicability_problem = TRUE
      )
    }
    mean_x <- mean(x)
    var_x <- var(x)
    if (!is.finite(var_x) || var_x <= 0) {
      util_error(
        "gamma values must have positive variance",
        applicability_problem = TRUE
      )
    }
    initial <- c(
      shape = mean_x * mean_x / var_x,
      rate = mean_x / var_x
    )
    gamma_neg_log_lik <- function(params, values) {
      -sum(dgamma(
        values,
        shape = params[["shape"]],
        rate = params[["rate"]],
        log = TRUE
      ))
    }
    fit <- optim(
      par = initial,
      fn = gamma_neg_log_lik,
      values = x,
      method = "BFGS",
      control = list(parscale = c(shape = 1, rate = initial[["rate"]]))
    )
    if (fit$convergence > 0L) {
      util_error(
        "optimization failed",
        applicability_problem = TRUE
      )
    }
    return(fit$par)
  }

  util_error(
    "This distribution '%s' is not supported yet...",
    dist,
    applicability_problem = TRUE
  )
}
