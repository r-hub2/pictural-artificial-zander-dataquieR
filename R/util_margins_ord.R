# nolint start: line_length_linter.
#' Utility function to create a plot similar to the margins plots for ordinal variables
#'
#' This function is still under development. It uses the `ordinal` package to
#' compute ordered regression models.
#'
#' @inheritParams .template_function_indicator
#' @param min_obs_in_subgroup [integer] from=0. This optional argument specifies
#'                       the minimum number of observations that is required to
#'                       include a subgroup (level) of the `group_var` in the
#'                       analysis.
#' @param min_subgroups [integer] from=0. This optional argument specifies
#'                       the minimum number of subgroups (levels) of the
#'                       `group_var` that is required to run the analysis. The
#'                       `ordinal::clmm()` implementation requires random-effect
#'                       grouping factors to have at least 3 levels
#'                       (`ordinal` 2025.12-29, `R/clmm.R:getREterms()`, checks
#'                       `nlevels > 2`). Smaller values are raised to 3.
#' @param ds1 [data.frame] the data frame that contains the measurements, after
#'                       replacing missing value codes by `NA`, excluding
#'                       inadmissible values and transforming categorical
#'                       variables to factors.
#' @param adjusted_hint [character] hint, if adjusted for `co_vars`
#' @param title [character] title for the plot
#' @param sort_group_var_levels [logical] Should the levels of the grouping
#'                        variable be sorted descending by the number of
#'                        observations (in the figure)?
#'
#' @return A table and a matching plot.
#'
#' @importFrom ggplot2 ggplot geom_point geom_pointrange theme_minimal theme labs xlab ylab ggtitle scale_colour_manual aes geom_vline
#' @importFrom stats qnorm p.adjust
#'
#' @noRd
# nolint end
util_margins_ord <- function(resp_vars = NULL, group_vars = NULL, co_vars = NULL, # nolint: line_length_linter.
  min_obs_in_subgroup = 5,
  min_subgroups =
    getOption(
      "dataquieR.min_group_var_levels",
      dataquieR.min_group_var_levels_default
    ),
  ds1, label_col,
  adjusted_hint = "",
  title = "",
  sort_group_var_levels =
    getOption(
      "dataquieR.acc_margins_sort",
      dataquieR.acc_margins_sort_default
    )) {
  # preps and checks -----------------------------------------------------------
  util_ensure_suggested("ordinal")
  # Historical data-preparation prototype removed here. Inspect with
  # `git show dada113988 -- R/util_margins_ord.R` before restoring it.
  # It also contained a later `min_obs_in_subgroup` validation sketch.
  util_expect_scalar(min_subgroups,
    check_type = util_is_numeric_in(
      min = 0,
      whole_num = TRUE,
      finite = TRUE
    )
  )
  if (min_subgroups < 3) {
    util_message(
      c(
        "Argument min_subgroups is %d, but ordinal::clmm() requires at least",
        "3 levels for random-effect grouping factors",
        "(ordinal 2025.12-29, R/clmm.R:getREterms(), nlevels > 2).",
        "Using min_subgroups = 3 for ordinal margins."
      ),
      min_subgroups,
      applicability_problem = TRUE
    )
    min_subgroups <- 3L
  }

  # Historical subgroup-filtering prototype removed here. Inspect with
  # `git show dada113988 -- R/util_margins_ord.R` before restoring it.

  if (length(co_vars) > 0 && all(!util_empty(co_vars))) {
    util_error(paste(
      "Covariate argument for ordinal regression",
      "not yet supported."
    ))
  }

  # ensure that the grouping variable has the specified minimum number of levels
  check_df <- util_table_of_vct(ds1[[group_vars]])
  if (length(check_df[, 1, drop = TRUE]) < min_subgroups) {
    util_error("%d < %d levels in %s.",
      length(check_df[, 1, drop = TRUE]),
      min_subgroups,
      dQuote(group_vars),
      applicability_problem = TRUE,
      intrinsic_applicability_problem = TRUE
    )
  }

  # ensure that the response variable is not constant
  var_prop <- util_dist_selection(ds1[, resp_vars, drop = FALSE])
  if (var_prop$NDistinct < 2) {
    util_error("The response variable is constant after data preparation.",
      applicability_problem = TRUE,
      intrinsic_applicability_problem = TRUE
    )
  }

  # modelling ------------------------------------------------------------------
  # prepare model formula
  if (length(co_vars) == 0) {
    co_vars <- "1"
  } else {
    co_vars <- util_bQuote(co_vars)
  }
  fmla <- as.formula(paste0(
    util_bQuote(resp_vars), " ~ ",
    paste0(c(co_vars, paste0("(1|", util_bQuote(group_vars), ")")),
      collapse = "+"
    )
  ))

  fit1 <- try(ordinal::clmm(
    formula = fmla, data = ds1,
    Hess = TRUE, nAGQ = 10
  ))
  if (inherits(fit1, "try-error")) {
    fit1 <- try(ordinal::clmm(
      formula = fmla, data = ds1,
      Hess = TRUE
    ))
  }
  if (inherits(fit1, "try-error")) {
    util_error("Model could not be fit automatically.")
  }

  if (sort_group_var_levels) {
    # order levels of the grouping variable by number of observations
    tbl_gr <- check_df %>%
      dplyr::arrange(dplyr::desc(.data$Var1)) %>%
      dplyr::arrange(.data$Freq)
    ds1[[group_vars]] <- factor(ds1[[group_vars]],
      levels = as.character(tbl_gr$Var1)
    )
  } else { # original order in plot, but from top of the y-axis to the bottom
    ds1[[group_vars]] <- factor(ds1[[group_vars]],
      levels = rev(levels(ds1[[group_vars]]))
    )
  }

  # calculation of random effects confidence intervals (using conditional modes)
  # taken from the vignette:
  # 'A Tutorial on fitting Cumulative Link Mixed Models with clmm2 from
  # the ordinal Package'
  # https://cran.r-project.org/web/packages/ordinal/vignettes/clmm2_tutorial.pdf
  plot_data <- data.frame(
    group = factor(levels(ds1[[group_vars]]), levels = levels(ds1[[group_vars]])), # nolint: line_length_linter.
    ran_ef = fit1$ranef,
    lower = fit1$ranef - qnorm(0.975) * sqrt(fit1$condVar),
    upper = fit1$ranef + qnorm(0.975) * sqrt(fit1$condVar)
  )
  colnames(check_df) <- c("group", "sample_size")
  plot_data <- merge(plot_data, check_df)
  if (sort_group_var_levels) {
    plot_data <- plot_data %>%
      dplyr::arrange(dplyr::desc(.data[["sample_size"]]))
  }
  # Wald tests (here z-tests, two-sided, H0: mu = 0) to detect outlying levels
  # of the grouping variable
  plot_data$z <- fit1$ranef / sqrt(fit1$condVar)
  plot_data$p <- (1 - pnorm(abs(plot_data$z), 0, 1)) * 2
  # adjust p values for multiple testing
  plot_data$GRADING <- as.character(as.numeric(
    p.adjust(plot_data$p, method = "fdr") < 0.05
  ))

  # figure ---------------------------------------------------------------------
  warn_code <- c("1" = "#B2182B", "0" = "#2166AC")

  p1 <- util_create_lean_ggplot(
    ggplot(
      plot_data,
      aes(
        x = .data[["ran_ef"]],
        y = .data[["group"]],
        col = .data[["GRADING"]]
      )
    ) +
      # +/- 1 SD of random effects (group_var) around 0
      geom_vline(
        xintercept = -sqrt(as.numeric(ordinal::VarCorr(fit1)[[group_vars]])),
        linetype = 2,
        col = "gray60"
      ) +
      geom_vline(
        xintercept = sqrt(as.numeric(ordinal::VarCorr(fit1)[[group_vars]])),
        linetype = 2,
        col = "gray60"
      ) +
      util_geom_pointrange_robust(aes(
        xmin = .data[["lower"]],
        xmax = .data[["upper"]]
      )) +
      geom_point() +
      scale_color_manual(
        values = warn_code,
        guide = "none"
      ) +
      xlab("conditional mode") +
      ylab("") +
      theme_minimal() +
      ggtitle(
        label = title,
        subtitle = adjusted_hint
      ) +
      labs(caption = "Dashed lines mark the interval 0 \u00B1 SD."),
    plot_data = plot_data,
    fit1 = fit1,
    group_vars = group_vars,
    warn_code = warn_code,
    title = title,
    adjusted_hint = adjusted_hint
  )

  # Historical emmeans alternatives for ordinal margins removed here. Inspect
  # with `git show dada113988 -- R/util_margins_ord.R` before restoring them.

  plot_data <- dplyr::rename(plot_data, c(
    "margins" = "ran_ef", "LCL" = "lower",
    "UCL" = "upper"
  ))
  plot_data$z <- NULL
  plot_data$p <- NULL

  return(list("plot" = p1, "plot_data" = plot_data))
}
