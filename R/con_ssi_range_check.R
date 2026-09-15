# nolint start: line_length_linter.
#' Internal function only existing for technical reasons.
#' Planned to be removed in future releases.
#'
#' `r lifecycle::badge("experimental")`
#'
#' @description Report the social science indicators
#' inadmissible values according to
#' limits defined by users in the cross-item level metadata
#'
#' [Indicator]
#'
#' @details
#' # ALGORITHM OF THIS IMPLEMENTATION:
#'  - Implementation is restricted to social science metrics (e.g., maximum long string)
#'  - Interpretation of variable specific intervals as supplied in the metadata.
#'  - Identification of measurements outside defined limits. Therefore two
#'    output data frames are generated:
#'    - on the level of observation to flag each deviation, and
#'    - a summary table for each variable.
#'  - A list of plots is generated for each computed variable examined for limit
#'    deviations. The histogram-like plots indicate respective limits as well
#'    as deviations.
#'
#' @inheritParams .template_function_indicator
#'
#' @param meta_data_cross_item [data.frame] -- Cross-item level metadata
#' @param flip_mode [character] optional direction for coordinate flipping in
#'   generated plots
#' @param return_flagged_study_data [logical] return `FlaggedStudyData` in the
#' @param show_obs [logical] Should (selected) individual observations be marked
#'                           in the figure for continuous variables?
#'                                                             result
#' @importFrom ggplot2 ggplot geom_histogram scale_fill_manual coord_flip labs theme_minimal theme geom_bar geom_vline annotate scale_linetype_manual
#' @importFrom stats setNames IQR complete.cases
#' @importFrom grDevices colorRampPalette gray.colors
#'
#' @return a list with:
#'   - `SummaryData`: [data.frame] table with user friendly caption summarizing
#'                                 limit deviations for each
#'                                 computed social science metric in the report
#'   - `SummaryTable`: [data.frame] table with indicators
#'   - `SummaryPlotList` [list] of [ggplot2::ggplot]s The plots for each variable are
#'                            either a histogram (continuous) or a
#'                            barplot (discrete).
#'   - `FlaggedStudyData` [data.frame] related to the study data by a 1:1
#'                                   relationship, i.e. for each observation is
#'                                   checked whether the value is below or above
#'                                   the limits. Optional, see
#'                                   `return_flagged_study_data`.
#'
#'
#' @export
#'
#' @seealso
#' [Online Documentation](
#' https://dataquality.qihs.uni-greifswald.de/
#' )
# nolint end
con_ssi_range_check <- function(resp_vars = NULL,
  study_data,
  label_col = VAR_NAMES,
  item_level = "item_level",
  meta_data = item_level,
  meta_data_v2,
  meta_data_cross_item = "cross-item_level",
  cross_item_level,
  `cross-item_level`,
  flip_mode = "noflip",
  return_flagged_study_data = FALSE,
  show_obs = TRUE) {

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
      CHECK_LABEL = character(0)
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
  prep_prepare_dataframes(.replace_hard_limits = FALSE) # nolint: line_length_linter.

  # check for variable role in the metadata for the resp_var
  util_correct_variable_use(resp_vars,
    need_type = paste(
      c(DATA_TYPES$INTEGER, DATA_TYPES$FLOAT),
      collapse = sprintf(" %s ", SPLIT_CHAR)
    ),
    need_scale = SCALE_LEVELS$RATIO,
    need_computed_role = paste(
      c(
        COMPUTED_VARIABLE_ROLES$MAXIMUM_LONG_STRING,
        COMPUTED_VARIABLE_ROLES$MISS_RESP,
        COMPUTED_VARIABLE_ROLES$IRV,
        COMPUTED_VARIABLE_ROLES$TOTRESPT,
        COMPUTED_VARIABLE_ROLES$RESPT_PER_ITEM,
        COMPUTED_VARIABLE_ROLES$RELCOMPL_SPEED,
        COMPUTED_VARIABLE_ROLES$PSYCHOMETRIC_SYN,
        COMPUTED_VARIABLE_ROLES$PSYCHOMETRIC_ANT
      ),
      collapse = sprintf(" %s ", SPLIT_CHAR)
    )
  )
  # Check resp_vars
  util_expect_scalar(
    arg_name = resp_vars,
    allow_more_than_one = FALSE,
    check_type = is.character
  )





  # select current variable from data ------------------------------------
  # Select CHECK_ID of the current variables group
  current_check_id <- util_map_labels(resp_vars, meta_data = meta_data, to = CHECK_ID, from = label_col) # nolint: line_length_linter.
  all_checkID_with_vars <- setNames(meta_data_cross_item$VARIABLE_LIST,
    nm = meta_data_cross_item$CHECK_ID
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
  # Historical vars_per_check_ID extraction removed here.

  current_no_vars <- as.numeric(no_vars_per_check_ID[current_check_id])
  current_vars <- all_checkID_with_vars[[current_check_id]]
  rm(all_checkID_with_vars, intermediate2, intermediate3, no_vars_per_check_ID)




  current_computed_vars <- ds1[[resp_vars]]
  #Found matching ssi metric
  current_metric <- util_map_labels(
    resp_vars,
    meta_data = meta_data,
    to = COMPUTED_VARIABLE_ROLE,
    from = label_col,
    ifnotfound = NA_character_
  )


  # Define the threshold ----
  range_users_limits <- meta_data_cross_item[
    meta_data_cross_item[[CHECK_ID]] == current_check_id,
    current_metric,
    drop = TRUE
  ]

  util_expect_scalar(range_users_limits, check_type = is.character)
  range_users_limits <- util_parse_interval(range_users_limits)
  if (!inherits(range_users_limits, "interval")) {
    util_error("Invalid interval in %s for %s.",
      dQuote(current_metric),
      dQuote(names(current_check_id)),
      applicability_problem = TRUE
    )
  }



  FlaggedStudyData <- ds1
  if (return_flagged_study_data) {
    # new complete data with the column indicating the outliers
    FlaggedStudyData[[paste0(current_metric, ":outside_range")]] <- NA

    below <- if (range_users_limits$inc_l) {
      current_computed_vars < range_users_limits$low
    } else {
      current_computed_vars <= range_users_limits$low
    }

    above <- if (range_users_limits$inc_u) {
      current_computed_vars > range_users_limits$upp
    } else {
      current_computed_vars >= range_users_limits$upp
    }

    FlaggedStudyData[[paste0(current_metric, ":outside_range")]] <- ifelse(
      below | above,
      1,
      0
    )
  }


  # Plot-Vorbereitung ---------------------------------------------------
  ref_env <- environment()
  base_col <- colorRampPalette(
    colors = c(
      "#2166AC", "#fdd49e", "#fc8d59",
      "#d7301f", "#B2182B", "#7f0000"
    )
  )(length(range_users_limits) + 1)

  base_lty <- c(2, 4:6)
  spec_txt <- element_text(
    colour = "black",
    hjust = .5,
    vjust = .5,
    face = "plain"
  )

  spec_lines <- data.frame(limits = "LIMITS", color = "#B2182B", lty = 1)



  sd_lim <- rep(NA, length(current_computed_vars))

  if (range_users_limits$inc_l) {
    below <- current_computed_vars < range_users_limits$low
  } else {
    below <- current_computed_vars <= range_users_limits$low
  }

  within <- redcap_env$`in`(current_computed_vars, range_users_limits)

  if (range_users_limits$inc_u) {
    above <- current_computed_vars > range_users_limits$upp
  } else {
    above <- current_computed_vars >= range_users_limits$upp
  }

  sd_lim[which(below)] <- "below"
  sd_lim[which(within)] <- "within"
  sd_lim[which(above)] <- "above"
  sd_lim <- factor(sd_lim, levels = c("below", "within", "above"))

  hl_viol_ind <- which(sd_lim %in% c("below", "above"))

  # Limits einsammeln (Werte unname(), Namen separat)
  lower_limits <- range_users_limits$low
  if (any(is.infinite(lower_limits))) lower_limits[is.infinite(lower_limits)] <- NA # nolint: line_length_linter.

  upper_limits <- range_users_limits$upp
  if (any(is.infinite(upper_limits))) upper_limits[is.infinite(upper_limits)] <- NA # nolint: line_length_linter.

  lower_limit_names <- "LIMITS"
  upper_limit_names <- "LIMITS"


  # Daten
  rv_data <- data.frame("values" = current_computed_vars,
    sd_lim)
  rv_data <- rv_data[complete.cases(rv_data[, "values", drop = TRUE]), ,
    drop = FALSE]


  # Bereiche
  max_data <- max(rv_data$values, na.rm = TRUE)
  min_data <- min(rv_data$values, na.rm = TRUE)
  max_plot <- max(c(max_data, lower_limits, upper_limits), na.rm = TRUE)
  min_plot <- min(c(min_data, lower_limits, upper_limits), na.rm = TRUE)

  # Füllungen
  colnames(rv_data) <- c("values", "LIMITS")  #TODO: modify names and add LIMITS
  rv_data$viol_n_limits <- ifelse(rv_data$LIMITS %in% c("above", "below"), 1, 0)

  rv_data$limit_violations <- paste0(
    "detected (",
    rv_data$viol_n_limits,
    ")"
  )

  rv_data$limit_violations[rv_data$viol_n_limits == 0] <- "none"

  rv_data$fill <- vapply(rv_data$viol_n_limits,
    FUN.VALUE = "character(1)",
    FUN = function(vv) base_col[vv + 1]
  )

  if ("LIMITS" %in% colnames(rv_data)) {
    rv_data$limit_violations[rv_data$LIMITS != "within"] <- "severe"
    rv_data$fill[rv_data$LIMITS != "within"] <- rev(base_col)[1]
  }

  spec_bars <- unique(rv_data[
    ,
    c("LIMITS", "limit_violations", "fill"),
    drop = FALSE
  ])

  plot_histogram_integer <-
    meta_data[[SCALE_LEVEL]][which(meta_data[[label_col]] == resp_vars)] %in%
    c(SCALE_LEVELS$INTERVAL, SCALE_LEVELS$RATIO) &&
    meta_data[[DATA_TYPE]][which(meta_data[[label_col]] == resp_vars)] %in%
    c("integer")

  xlb <- prep_get_labels(
    resp_vars = resp_vars,
    resp_vars_match_label_col_only = TRUE,
    label_class = "SHORT",
    label_col = label_col,
    item_level = meta_data
  )

  low <- range_users_limits$low
  high <- range_users_limits$upp


  md_lim_all_segments <- setNames(
    list(
      LIMITS = list(
        below  = list(str = sprintf("(-Inf;%s)", low),
          low = -Inf,
          high = low,
          inc_l = FALSE,
          inc_u = !(range_users_limits$inc_l)),
        within = list(str = sprintf("[%s;%s]", low, high),
          low = low,
          high = high,
          inc_l = range_users_limits$inc_l,
          inc_u = range_users_limits$inc_u),
        above  = list(str = sprintf("(%s;Inf)", high),
          low = high,
          high = Inf,
          inc_l = !(range_users_limits$inc_u),
          inc_u = TRUE)
      )
    ),
    resp_vars
  )
  # Non-ratio plotting was unreachable because util_correct_variable_use()
  # requires ratio scales. See the removed code with:
  # git show 520d2f4b5:QualityIndicatorFunctions/R/con_ssi_range_check.R
  {
    # Segmente bestimmen
    plot_segments <- rv_data %>%
      dplyr::arrange(.data[["values"]]) %>%
      dplyr::select("LIMITS") %>%
      dplyr::group_by_all(.) %>%
      dplyr::count() %>%
      as.data.frame(., drop = FALSE)

    plot_segments_intervals <-
      apply(plot_segments[, "LIMITS", drop = FALSE], 1, function(rr) {
        rr_int <-  md_lim_all_segments[[resp_vars]]
        out_int <- rr_int[[rr]]$str
        out_int <- util_parse_interval(out_int)
        return(out_int)
      })

    largest_segment <- plot_segments_intervals[[which.max(plot_segments$n)]]

    histogram_cuts <- sort(as.numeric(unique(c(
      lower_limits,
      upper_limits
    ))))
    bin_breaks <- util_optimize_histogram_bins(
      x = rv_data$values,
      interval_freedman_diaconis = largest_segment,
      cuts = histogram_cuts,
      nbins_max = 100
    )

    segment_names <- as.character(plot_segments[["LIMITS"]])
    if (length(histogram_cuts) == 2) {
      break_names <- "within"
      if (min(rv_data$values) < histogram_cuts[[1]]) {
        break_names <- c("below", break_names)
      }
      if (max(rv_data$values) > histogram_cuts[[2]]) {
        break_names <- c(break_names, "above")
      }
      names(bin_breaks) <- break_names
      plot_breaks <- bin_breaks[segment_names]
    } else {
      plot_breaks <- rep(bin_breaks, length(segment_names))
      names(plot_breaks) <- segment_names
    }

    # Histogramdaten
    plot_data <- lapply(seq_along(plot_segments_intervals), function(ii) {
      subd1 <- rv_data[
        redcap_env$`in`(
          rv_data$values,
          plot_segments_intervals[[ii]]
        ),
        c("values", "limit_violations"),
        drop = FALSE
      ]
      if (nrow(subd1) == 0) return(NULL)
      h1 <- hist(subd1$values, plot = FALSE, breaks = plot_breaks[[ii]])
      return(data.frame(
        histogram_x = h1$mids,
        histogram_y = h1$counts,
        limit_violations = subd1$limit_violations[1],
        segment_number = ii,
        row.names = NULL
      ))
    })
    plot_data <- do.call(rbind, plot_data)

    # x-Limits
    myxlim <- c(floor(min_plot), ceiling(max_plot))

    # Limit-Linien (Werte & Namen getrennt)
    limit_values <- c(lower_limits, upper_limits)
    finite_limits <- is.finite(limit_values)
    all_limits_df <- data.frame(
      limits = rep("LIMITS", sum(finite_limits)),
      values = limit_values[finite_limits]
    )

    # Histogram-Plot
    p <- util_create_lean_ggplot(
      ggplot(
        data = plot_data,
        aes(
          x = .data[["histogram_x"]],
          y = .data[["histogram_y"]],
          fill = .data[["limit_violations"]]
        )
      ),
      plot_data = plot_data
    )

    for (ii in unique(plot_data$segment_number)) {
      dt <- plot_data[plot_data$segment_number == ii, , drop = FALSE]
      bb_ii <- as.numeric(plot_breaks[[ii]])
      width_col <- bb_ii[2] - bb_ii[1]
      p <- p %lean+% util_create_lean_ggplot(
        geom_col(data = dt, width = width_col),
        dt = dt,
        width_col = width_col
      )
    }

    #points
    bb <- do.call(c, unname(plot_breaks))
    bb <- bb[!duplicated(bb)]
    if (plot_histogram_integer) {
      if (mean(bb[-1] - bb[-length(bb)]) < 7 && length(bb) <= 6) show_obs <- FALSE # nolint: line_length_linter.
    }

    if (show_obs) {
      max_bar_height <- max(table(cut(rv_data$value, breaks = bb)))
      ypos <- -0.05 * max_bar_height
      subdata <- NULL
      for (ii in seq_len(length(bb) - 1)) {
        if (ii != length(bb) - 1) {
          subd <- rv_data[
            rv_data$values >= bb[ii] & rv_data$values < bb[ii + 1],
            ,
            drop = FALSE
          ]
        } else {
          subd <- rv_data[
            rv_data$values >= bb[ii] & rv_data$values <= bb[ii + 1],
            ,
            drop = FALSE
          ]
        }
        if (nrow(subd) > 7) {
          sel_val <- quantile(as.numeric(subd$values),
            probs = seq(0, 1, length.out = 7), type = 3
          )
          sel_ind <- vapply(sel_val,
            FUN.VALUE = integer(1),
            FUN = function(x) {
              which(as.numeric(subd$value) == x)[1]
            }
          )
          subd <- subd[sel_ind, , drop = FALSE]
        }
        if (nrow(subd) > 0) subdata <- rbind(subdata, subd)
      }
      colnames(subdata)[which(colnames(subdata) == "values")] <-
        "histogram_x"

      p <- p %lean+% util_create_lean_ggplot(
        geom_point(aes(y = ypos),
          data = subdata,
          col = "darkgray",
          position = position_jitter(
            height = 0.01 * max_bar_height,
            width = 0,
            seed = 1L
          ),
          size = 1,
          alpha = 0.6
        ),
        ypos = ypos,
        subdata = subdata,
        max_bar_height = max_bar_height
      )
    }

    fli <- util_coord_flip(ref_env = ref_env, xlim = myxlim)
    p <- p %lean+% util_create_lean_ggplot(
      geom_vline(
        data = all_limits_df,
        aes(
          xintercept = .data[["values"]],
          linetype = .data[["limits"]],
          color = .data[["limits"]]
        )
      ),
      all_limits_df = all_limits_df
    )
    p <- util_lazy_add_coord(p, fli)
    p <- p %lean+%
      scale_fill_manual(
        values = spec_bars$fill,
        breaks = spec_bars$limit_violations,
        guide = "none"
      ) %lean+%
      ggplot2::scale_linetype_manual(
        name = "limits",
        values = spec_lines$lty,
        breaks = spec_lines$limits
      ) %lean+%
      scale_color_manual(
        name = "limits",
        values = spec_lines$color,
        breaks = spec_lines$limits
      ) %lean+%
      labs(x = paste0(xlb), y = "") %lean+%
      theme_minimal() %lean+%
      theme(
        title = spec_txt,
        axis.text.x = spec_txt,
        axis.text.y = spec_txt,
        axis.title.x = spec_txt,
        axis.title.y = spec_txt
      )

    if (plot_histogram_integer) {
      pretty_x_axt <- util_int_breaks_rounded(c(
        rv_data$values,
        all_limits_df$values,
        myxlim
      ))
      pretty_x_axt <- pretty_x_axt[
        which(pretty_x_axt >= myxlim[1] - 0.1 * abs(myxlim[2] - myxlim[1]) &
            pretty_x_axt <= myxlim[2] + 0.1 * abs(myxlim[2] - myxlim[1]))
      ]
      p <- p %lean+% util_create_lean_ggplot(
        scale_x_continuous(
          breaks = pretty_x_axt,
          expand = expansion(mult = 0.1)
        ),
        pretty_x_axt = pretty_x_axt
      )
    } else {
      p <- p %lean+% util_create_lean_ggplot(
        scale_x_continuous(expand = expansion(mult = 0.1))
      )
    }

    # Sizing
    min_bin_height <- min(plot_data$histogram_y)
    max_bin_height <- max(plot_data$histogram_y)
    no_bars <- nrow(plot_data)
    obj1 <- util_create_lean_ggplot(ggplot2::ggplot_build(p), p = p)
    total_w <- c(
      util_rbind(data_frames_list = obj1$data)$xmin,
      util_rbind(data_frames_list = obj1$data)$xmax,
      util_rbind(data_frames_list = obj1$data)$xintercept
    )
    total_w <- total_w[!is.na(total_w)]
    min_total_w <- min(total_w)
    max_total_w <- max(total_w)
    total_w <- max_total_w - min_total_w
    min_x_plot <- min(
      util_rbind(data_frames_list = obj1$data)$xmin,
      na.rm = TRUE
    )
    max_x_plot <- max(
      util_rbind(data_frames_list = obj1$data)$xmax,
      na.rm = TRUE
    )
    no_char_y <- nchar(round(max_bin_height, digits = 0))
    no_char_x <- nchar(round(max_total_w, digits = 0))
    rm(obj1)

    min_limits <- min(all_limits_df$values)
    max_limits <- max(all_limits_df$values)
  }

  plot_list_final <- setNames(list(p), resp_vars)

  sumtab_final <- NA
  sumdat_wide_final <- NA

  #Summary Outputs -----
  t1 <- table(sd_lim)
  freq_lim_viol <- setNames(nm = names(t1), as.vector(t1))
  samplesize <- sum(freq_lim_viol)

  sumdat <- data.frame(
    "Variables" = resp_vars,
    "Admissible range" = as.character(range_users_limits),
    "Section" = names(freq_lim_viol),
    "Number" = as.numeric(freq_lim_viol),
    "Percentage" = round(as.numeric(freq_lim_viol) / samplesize * 100, 2),
    check.names = FALSE
  )

  sumdat_wide <- stats::reshape(sumdat,
    idvar = c("Variables", "Admissible range"),
    timevar = "Section",
    direction = "wide"
  )

  sumdat_wide$"All.outside.rangeN" <-
    sumdat_wide$Number.below + sumdat_wide$Number.above
  sumdat_wide$"All.outside.range%" <- round(
    sumdat_wide$"All.outside.rangeN" / samplesize * 100, 2
  )

  sumdat_wide_final <- data.frame(
    "Variables" = resp_vars,
    "Admissible range" = as.character(range_users_limits),
    "Below range N (%)" = paste0(sumdat_wide$Number.below,
      " (",
      sumdat_wide$Percentage.below,
      ")"),
    "Within range N (%)" = paste0(sumdat_wide$Number.within,
      " (",
      sumdat_wide$Percentage.within,
      ")"),
    "Above range N (%)" = paste0(sumdat_wide$Number.above,
      " (", sumdat_wide$Percentage.above,
      ")"),
    "All outside range N (%)" = paste0(sumdat_wide$All.outside.rangeN,
      " (",
      sumdat_wide$`All.outside.range%`,
      ")"),
    "N" = samplesize,
    "Observational units removed" =
      length(current_computed_vars) - samplesize,
    check.names = FALSE
  )

  attr(sumdat_wide_final, "description") <-
    util_get_hovertext("[con_limit_dev_ssi_hover]")
  attr(sumdat_wide_final$N, DATA_TYPE) <- DATA_TYPES$INTEGER
  attr(sumdat_wide_final$"Observational units removed", DATA_TYPE) <-
    DATA_TYPES$INTEGER

  n_viol <- length(hl_viol_ind)
  source_metric <- util_get_concept_info(
    "computed_vars_ind_mapping",
    get("computed_role") == current_metric,
    "needle",
    drop = TRUE
  )
  util_expect_scalar(source_metric, check_type = is.character)
  sumtab_final <- data.frame(
    "Variables" = resp_vars,
    check.names = FALSE
  )
  sumtab_final[[paste0("NUM_", source_metric)]] <- n_viol
  sumtab_final[[paste0("PCT_", source_metric)]] <- round(
    n_viol / samplesize * 100,
    2
  )
  sumtab_final[[paste0("FLG_", source_metric)]] <- n_viol > 0

  return(dqr = util_attach_attr(
    list(
      FlaggedStudyData = FlaggedStudyData,
      SummaryTable = sumtab_final,
      SummaryData = sumdat_wide_final,
      SummaryPlotList = plot_list_final
    ),
    as_plotly = "util_as_plotly_con_limit_deviations"
  ))
}
