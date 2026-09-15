#' Parse stratified item-level limit metadata
#'
#' @param limit character scalar from a `*_LIMITS` metadata column
#'
#' @return an interval or an internal stratified-limit object
#'
#' @noRd
util_parse_stratified_limit <- function(limit) {
  if (util_empty(limit)) {
    return(NA)
  }

  simple_interval <- util_try_parse_interval(limit)
  if (inherits(simple_interval, "interval")) {
    return(simple_interval)
  }

  parts <- trimws(strsplit(limit, SPLIT_CHAR, fixed = TRUE)[[1]])
  parts <- parts[!util_empty(parts)]
  rules <- lapply(seq_along(parts), function(ii) {
    util_parse_stratified_limit_part(parts[[ii]], ii)
  })
  keep <- vapply(rules, Negate(is.null), FUN.VALUE = logical(1))
  if (any(!keep)) {
    util_message(
      "Ignoring malformed stratified limit part(s): %s",
      paste(sQuote(parts[!keep]), collapse = ", "),
      applicability_problem = TRUE
    )
  }
  rules <- rules[keep]

  if (length(rules) == 0) {
    return(NA)
  }
  if (length(rules) == 1 && isTRUE(rules[[1]][["is_default"]])) {
    return(rules[[1]][["interval"]])
  }

  structure(list(rules = rules), class = "stratified_limit")
}


#' Parse one conditional limit rule
#'
#' @param part one `condition: interval` or default `interval` segment
#' @param order rule order
#'
#' @return list with condition and interval details
#'
#' @noRd
util_parse_stratified_limit_part <- function(part, order) {
  default_interval <- util_try_parse_interval(part)
  if (inherits(default_interval, "interval")) {
    return(list(
      condition = NA_character_,
      parsed_condition = NULL,
      interval = default_interval,
      label = "default",
      is_default = TRUE,
      order = order
    ))
  }

  starts <- gregexpr(":[[:space:]]*[\\[\\(]", part, perl = TRUE)[[1]]
  if (identical(starts, -1L)) {
    return(NULL)
  }
  start <- starts[[length(starts)]]
  condition <- trimws(substr(part, 1, start - 1))
  interval_text <- trimws(substr(part, start + 1, nchar(part)))
  interval <- util_try_parse_interval(interval_text)

  if (util_empty(condition) || !inherits(interval, "interval")) {
    return(NULL)
  }

  condition <- util_normalize_stratified_limit_condition(condition)
  parsed_condition <- withCallingHandlers(
    util_parse_redcap_rule(condition),
    warning = function(cnd) {
      invokeRestart("muffleWarning")
    }
  )
  if (length(parsed_condition) == 0) {
    return(NULL)
  }

  list(
    condition = condition,
    parsed_condition = parsed_condition,
    interval = interval,
    label = condition,
    is_default = FALSE,
    order = order
  )
}


#' Normalize issue-style interval conditions for the REDCap rule parser
#'
#' @param condition condition text from `condition: interval`
#'
#' @return normalized condition text
#'
#' @noRd
util_normalize_stratified_limit_condition <- function(condition) {
  pattern <- paste0(
    "\\b(in|not[[:space:]]+in)[[:space:]]*",
    "([\\[\\(])([^\\]\\)]*,[^\\]\\)]*)([\\]\\)])"
  )
  matches <- gregexpr(pattern, condition,
    ignore.case = TRUE,
    perl = TRUE
  )[[1]]
  if (identical(matches, -1L)) {
    return(condition)
  }
  lengths <- util_attr(matches, "match.length", exact = TRUE)
  for (ii in rev(seq_along(matches))) {
    start <- matches[[ii]]
    stop <- start + lengths[[ii]] - 1L
    replacement <- gsub(",", ";", substr(condition, start, stop),
      fixed = TRUE
    )
    condition <- paste0(
      substr(condition, 1L, start - 1L),
      replacement,
      substr(condition, stop + 1L, nchar(condition))
    )
  }
  condition
}


#' Parse intervals without surfacing exploratory parser warnings
#'
#' @param limit interval text
#'
#' @return interval or `NA`
#'
#' @noRd
util_try_parse_interval <- function(limit) {
  interval <- withCallingHandlers(
    try(util_parse_interval(limit), silent = TRUE),
    warning = function(cnd) {
      invokeRestart("muffleWarning")
    }
  )
  if (util_is_try_error(interval)) {
    return(NA)
  }
  interval
}


#' Hide stratified limit syntax from legacy metadata validation
#'
#' @param meta_data item-level metadata
#'
#' @return metadata with stratified `*_LIMITS` cells set to `NA`
#'
#' @noRd
util_mask_stratified_limits_for_validation <- function(meta_data) {
  limit_cols <- intersect(
    c(HARD_LIMITS, SOFT_LIMITS, DETECTION_LIMITS),
    colnames(meta_data)
  )
  for (limit_col in limit_cols) {
    is_stratified <- vapply(
      meta_data[[limit_col]],
      FUN.VALUE = logical(1),
      function(limit) {
        inherits(util_parse_stratified_limit(limit), "stratified_limit")
      }
    )
    meta_data[[limit_col]][is_stratified] <- NA_character_
  }
  meta_data
}


#' Select effective limits for observations
#'
#' @param limit stratified-limit object
#' @param ds1 prepared study data
#' @param meta_data item-level metadata
#' @param data_preparation `DATA_PREPARATION` for the limit rule
#'
#' @return list of interval objects or `NA`
#'
#' @noRd
util_resolve_stratified_limit <- function(limit,
  ds1,
  meta_data,
  data_preparation = NULL) {
  util_stop_if_not(
    "Internal error: expected a stratified limit." =
      inherits(limit, "stratified_limit")
  )
  prep <- util_stratified_limit_eval_preparation(data_preparation)

  out <- rep(list(NA), nrow(ds1))
  unmatched <- rep(TRUE, nrow(ds1))

  for (rule in limit[["rules"]]) {
    if (isTRUE(rule[["is_default"]])) {
      selected <- unmatched
    } else {
      selected <- util_eval_rule(
        rule[["parsed_condition"]],
        ds1 = ds1,
        meta_data = meta_data,
        use_value_labels = prep[["use_value_labels"]],
        replace_missing_by = prep[["replace_missing_by"]],
        replace_limits = prep[["replace_limits"]]
      )
      selected <- !is.na(selected) & as.logical(selected) & unmatched
    }
    if (any(selected)) {
      out[selected] <- list(rule[["interval"]])
      unmatched[selected] <- FALSE
    }
  }

  out
}


#' Select matching stratified-limit rule indices for observations
#'
#' @param limit stratified-limit object
#' @param ds1 prepared study data
#' @param meta_data item-level metadata
#' @param data_preparation `DATA_PREPARATION` for the limit rule
#'
#' @return integer vector with matching rule indices
#'
#' @noRd
util_resolve_stratified_limit_rules <- function(limit,
  ds1,
  meta_data,
  data_preparation = NULL) {
  util_stop_if_not(
    "Internal error: expected a stratified limit." =
      inherits(limit, "stratified_limit")
  )
  prep <- util_stratified_limit_eval_preparation(data_preparation)

  out <- rep(NA_integer_, nrow(ds1))
  unmatched <- rep(TRUE, nrow(ds1))

  for (ii in seq_along(limit[["rules"]])) {
    rule <- limit[["rules"]][[ii]]
    if (isTRUE(rule[["is_default"]])) {
      selected <- unmatched
    } else {
      selected <- util_eval_rule(
        rule[["parsed_condition"]],
        ds1 = ds1,
        meta_data = meta_data,
        use_value_labels = prep[["use_value_labels"]],
        replace_missing_by = prep[["replace_missing_by"]],
        replace_limits = prep[["replace_limits"]]
      )
      selected <- !is.na(selected) & as.logical(selected) & unmatched
    }
    if (any(selected)) {
      out[selected] <- ii
      unmatched[selected] <- FALSE
    }
  }

  out
}


#' DATA_PREPARATION value for an item-level stratified limit rule
#'
#' @param meta_data item-level metadata
#' @param label_col metadata variable-name column
#' @param rv response variable name
#'
#' @return character scalar
#'
#' @noRd
util_stratified_limit_data_preparation <- function(meta_data, label_col, rv) {
  default <- sprintf("LABEL %s MISSING_NA", SPLIT_CHAR)
  if (!DATA_PREPARATION %in% colnames(meta_data)) {
    return(default)
  }
  data_preparation <- meta_data[[DATA_PREPARATION]][
    which(meta_data[[label_col]] == rv)[[1]]
  ]
  if (util_empty(data_preparation)) {
    return(default)
  }
  data_preparation
}


#' Translate DATA_PREPARATION to util_eval_rule() options for limits
#'
#' @param data_preparation `DATA_PREPARATION` value
#'
#' @return list with `use_value_labels`, `replace_missing_by`, `replace_limits`
#'
#' @noRd
util_stratified_limit_eval_preparation <- function(data_preparation = NULL) {
  if (length(data_preparation) == 0 || util_empty(data_preparation)) {
    data_preparation <- sprintf("LABEL %s MISSING_NA", SPLIT_CHAR)
  }
  prep <- util_parse_missing_code_data_preparation(data_preparation)
  if ("LIMITS" %in% prep) {
    util_error(
      "%s = %s cannot be used for stratified item-level limits.",
      sQuote(DATA_PREPARATION),
      dQuote("LIMITS"),
      applicability_problem = TRUE
    )
  }
  list(
    use_value_labels = "LABEL" %in% prep,
    replace_missing_by = util_missing_code_replacement_mode(prep),
    replace_limits = FALSE
  )
}


#' Classify values using observation-specific intervals
#'
#' @param values vector with study data values
#' @param intervals list of interval objects or `NA`
#'
#' @return factor with `below`, `within`, and `above`
#'
#' @noRd
util_classify_stratified_limits <- function(values, intervals) {
  out <- rep(NA_character_, length(values))

  for (ii in seq_along(values)) {
    int <- intervals[[ii]]
    value <- values[[ii]]
    if (is.na(value) || !inherits(int, "interval")) {
      next
    }
    if (int$inc_l) {
      below <- value < int$low
    } else {
      below <- value <= int$low
    }
    within <- redcap_env$`in`(value, int)
    if (int$inc_u) {
      above <- value > int$upp
    } else {
      above <- value >= int$upp
    }
    if (isTRUE(below)) {
      out[[ii]] <- "below"
    } else if (isTRUE(within)) {
      out[[ii]] <- "within"
    } else if (isTRUE(above)) {
      out[[ii]] <- "above"
    }
  }

  factor(out, levels = c("below", "within", "above"))
}


#' Maximum number of strata to show in stratified limit plots
#'
#' @return positive integer scalar
#'
#' @noRd
util_max_strata_in_limit_plots <- function() {
  max_strata <- getOption("dataquieR.max_strata_in_limit_plots", 5)
  if (
    length(max_strata) != 1 ||
      is.na(max_strata) ||
      !is.numeric(max_strata) ||
      max_strata < 1
  ) {
    max_strata <- 5
  }
  as.integer(floor(max_strata))
}


#' Placeholder plot for stratified limits that cannot be drawn clearly
#'
#' @param xlb x axis label
#' @param message plot message
#' @param spec_txt ggplot text theme
#'
#' @return ggplot
#'
#' @noRd
util_create_stratified_limit_message_plot <- function(xlb, message, spec_txt) {
  p <- util_create_lean_ggplot(
    ggplot2::ggplot() %lean+%
      ggplot2::geom_col(
        data = data.frame(x = 0.5, y = 1),
        ggplot2::aes(x = .data[["x"]], y = .data[["y"]]),
        width = 0.1,
        alpha = 0
      ) %lean+%
      ggplot2::geom_text(
        ggplot2::aes(x = 0.5, y = 0.5, label = message),
        size = 5,
        color = "gray50",
        check_overlap = TRUE
      ) %lean+%
      ggplot2::xlim(0, 1) %lean+%
      ggplot2::ylim(0, 1) %lean+%
      ggplot2::labs(x = paste0(xlb), y = "") %lean+%
      ggplot2::theme_minimal() %lean+%
      ggplot2::theme(
        title = spec_txt,
        axis.text.x = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_blank(),
        axis.title.x = spec_txt,
        axis.title.y = ggplot2::element_blank(),
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank()
      ),
    xlb = xlb,
    message = message,
    spec_txt = spec_txt
  )
  attr(p, "sizing_hints") <- list(
    figure_type_id = "bar_limit",
    range = 1,
    no_bars_in_all_w = 1,
    no_char_x = 4,
    no_char_y = nchar(message)
  )
  p
}


#' Plot item-level stratified limits
#'
#' @param rv response variable name
#' @param ds1 prepared study data
#' @param meta_data item-level metadata
#' @param label_col metadata variable-name column
#' @param limits named list of parsed limits for `rv`
#' @param limit_results named list of classified limit results for `rv`
#' @param data_preparation `DATA_PREPARATION` for the limit rule
#' @param is_datetime_var if `rv` is a datetime variable
#' @param is_time_var if `rv` is a time variable
#' @param spec_txt ggplot text theme
#' @param ref_env reference environment for coordinate handling
#' @param show_obs show sampled observations
#'
#' @return ggplot
#'
#' @noRd
util_create_stratified_limit_plot <- function(rv,
  ds1,
  meta_data,
  label_col,
  limits,
  limit_results,
  data_preparation,
  is_datetime_var,
  is_time_var,
  spec_txt,
  ref_env,
  show_obs) {
  xlb <- prep_get_labels(
    resp_vars = rv,
    resp_vars_match_label_col_only = TRUE,
    label_class = "SHORT",
    label_col = label_col,
    item_level = meta_data
  )

  stratified_limit_names <- names(limits)[
    vapply(limits, inherits,
      FUN.VALUE = logical(1),
      what = "stratified_limit"
    )
  ]
  if (length(stratified_limit_names) != 1) {
    return(util_create_stratified_limit_message_plot(
      xlb,
      "Plot available for one stratified item-level limit only",
      spec_txt
    ))
  }

  max_strata <- util_max_strata_in_limit_plots()
  limit_name <- stratified_limit_names[[1]]
  limit <- limits[[limit_name]]
  rules <- limit[["rules"]]
  if (length(rules) > max_strata) {
    return(util_create_stratified_limit_message_plot(
      xlb,
      sprintf(
        "Too many strata for stratified limit plot (%d > %d)",
        length(rules),
        max_strata
      ),
      spec_txt
    ))
  }

  rule_index <- util_resolve_stratified_limit_rules(
    limit,
    ds1 = ds1,
    meta_data = meta_data,
    data_preparation = data_preparation
  )
  values <- ds1[[rv]]
  if (is_datetime_var) {
    values <- as.numeric(values)
  } else if (is_time_var) {
    if (inherits(values, "hms")) {
      values <- suppressWarnings(as.numeric(values))
    } else if (inherits(values, "difftime")) {
      values <- suppressWarnings(as.numeric(values, units = "secs"))
    } else if (is.character(values)) {
      values <- suppressWarnings(as.numeric(util_parse_time(values)))
    } else if (!is.numeric(values)) {
      values <- suppressWarnings(as.numeric(hms::as_hms(values)))
    }
  }

  keep <- !is.na(values) & !is.na(rule_index)
  if (!any(keep)) {
    return(util_create_stratified_limit_message_plot(
      xlb,
      "No plottable observations for stratified item-level limits",
      spec_txt
    ))
  }

  rule_labels <- vapply(rules, function(rule) {
    if (isTRUE(rule[["is_default"]])) {
      "default"
    } else {
      rule[["condition"]]
    }
  }, FUN.VALUE = character(1))
  rule_labels <- make.unique(rule_labels)

  classification <- limit_results[[limit_name]]
  limit_violations <- rep("none", length(classification))
  limit_violations[classification %in% c("below", "above")] <- "detected"
  if (identical(limit_name, HARD_LIMITS)) {
    limit_violations[classification %in% c("below", "above")] <- "severe"
  }
  limit_violations[is.na(classification)] <- "not checked"

  plot_data <- data.frame(
    values = values[keep],
    stratum = factor(rule_labels[rule_index[keep]], levels = rule_labels),
    limit_violations = factor(
      limit_violations[keep],
      levels = c("none", "detected", "severe", "not checked")
    ),
    stringsAsFactors = FALSE
  )

  limit_lines <- lapply(seq_along(rules), function(ii) {
    int <- rules[[ii]][["interval"]]
    vals <- c(int$low, int$upp)
    vals <- vals[is.finite(vals)]
    if (length(vals) == 0) {
      return(NULL)
    }
    data.frame(
      stratum = factor(rule_labels[[ii]], levels = rule_labels),
      limits = paste(limit_name, rule_labels[[ii]]),
      values = vals,
      stringsAsFactors = FALSE
    )
  })
  limit_lines <- limit_lines[
    vapply(limit_lines, Negate(is.null), FUN.VALUE = logical(1))
  ]
  if (length(limit_lines) == 0) {
    limit_lines <- NULL
    limit_line_values <- numeric(0)
  } else {
    limit_lines <- do.call(rbind.data.frame, limit_lines)
    limit_line_values <- limit_lines$values
  }
  min_plot <- min(c(plot_data$values, limit_line_values), na.rm = TRUE)
  max_plot <- max(c(plot_data$values, limit_line_values), na.rm = TRUE)
  if (!is_datetime_var && !is_time_var) {
    min_plot <- floor(min_plot)
    max_plot <- ceiling(max_plot)
  }
  if (identical(min_plot, max_plot)) {
    min_plot <- min_plot - 0.5
    max_plot <- max_plot + 0.5
  }

  if (is_datetime_var) {
    plot_data$values <- util_parse_date(plot_data$values)
    if (!is.null(limit_lines)) {
      limit_lines$values <- as.POSIXct(limit_lines$values,
        origin = min(Sys.time(), 0)
      )
    }
    xlim <- util_parse_date(c(min_plot, max_plot))
  } else {
    xlim <- c(min_plot, max_plot)
  }

  plot_histogram <- meta_data[[SCALE_LEVEL]][
    which(meta_data[[label_col]] == rv)
  ] %in% c(SCALE_LEVELS$INTERVAL, SCALE_LEVELS$RATIO)
  plot_histogram_integer <- plot_histogram &&
    meta_data[[DATA_TYPE]][which(meta_data[[label_col]] == rv)] %in%
      DATA_TYPES$INTEGER

  fill_values <- c(
    "none" = "#2166AC",
    "detected" = "#fc8d59",
    "severe" = "#7f0000",
    "not checked" = "gray70"
  )
  fill_breaks <- intersect(
    names(fill_values),
    as.character(unique(plot_data$limit_violations))
  )
  fli <- util_coord_flip(ref_env = ref_env, xlim = xlim)

  if (plot_histogram && !plot_histogram_integer &&
      !is_datetime_var && !is_time_var) {
    plot_data_hist <- lapply(seq_along(rules), function(ii) {
      stratum <- factor(rule_labels[[ii]], levels = rule_labels)
      subd <- plot_data[plot_data$stratum == stratum, , drop = FALSE]
      if (nrow(subd) == 0) {
        return(NULL)
      }
      int <- rules[[ii]][["interval"]]
      cuts <- c(int$low, int$upp)
      cuts <- cuts[is.finite(cuts)]
      bin_breaks <- suppressMessages(util_optimize_histogram_bins(
        x = subd$values,
        interval_freedman_diaconis = int,
        cuts = cuts,
        nbins_max = 100
      ))
      bin_data <- lapply(seq_along(bin_breaks), function(jj) {
        breaks <- as.numeric(bin_breaks[[jj]])
        bin_left <- breaks[[1]]
        bin_right <- breaks[[length(breaks)]]
        boundary_equal <- function(value, boundary) {
          is.finite(boundary) && abs(value - boundary) < .Machine$double.eps
        }
        include_left <- TRUE
        include_right <- TRUE
        if (boundary_equal(bin_left, int$low)) {
          include_left <- isTRUE(int$inc_l)
        }
        if (boundary_equal(bin_left, int$upp)) {
          include_left <- !isTRUE(int$inc_u)
        }
        if (boundary_equal(bin_right, int$low)) {
          include_right <- !isTRUE(int$inc_l)
        }
        if (boundary_equal(bin_right, int$upp)) {
          include_right <- isTRUE(int$inc_u)
        }
        in_segment <- if (include_left) {
          subd$values >= bin_left
        } else {
          subd$values > bin_left
        }
        in_segment <- in_segment & if (include_right) {
          subd$values <= bin_right
        } else {
          subd$values < bin_right
        }
        segment_data <- subd[
          in_segment,
          ,
          drop = FALSE
        ]
        if (nrow(segment_data) == 0) {
          return(NULL)
        }
        split_data <- split(segment_data, segment_data$limit_violations)
        split_data <- split_data[vapply(split_data, nrow, integer(1)) > 0]
        do.call(rbind, lapply(split_data, function(split_part) {
          hist_data <- hist(
            split_part$values,
            plot = FALSE,
            breaks = breaks
          )
          data.frame(
            histogram_x = hist_data$mids,
            histogram_y = hist_data$counts,
            stratum = stratum,
            limit_violations = split_part$limit_violations[[1]],
            segment_number = paste(ii, jj, sep = "_"),
            bin_width = breaks[[2]] - breaks[[1]],
            stringsAsFactors = FALSE
          )
        }))
      })
      do.call(rbind, bin_data[vapply(bin_data, Negate(is.null), logical(1))])
    })
    plot_data_hist <- do.call(
      rbind,
      plot_data_hist[vapply(plot_data_hist, Negate(is.null), logical(1))]
    )

    p <- util_create_lean_ggplot(
      ggplot2::ggplot(
        plot_data_hist,
        ggplot2::aes(
          x = .data[["histogram_x"]],
          y = .data[["histogram_y"]],
          fill = .data[["limit_violations"]]
        )
      ) %lean+%
        ggplot2::facet_wrap(ggplot2::vars(.data[["stratum"]]),
          ncol = 1,
          scales = "free_y"
        ),
      plot_data_hist = plot_data_hist
    )
    for (segment in unique(plot_data_hist$segment_number)) {
      segment_data <- plot_data_hist[
        plot_data_hist$segment_number == segment,
        ,
        drop = FALSE
      ]
      p <- p %lean+% util_create_lean_ggplot(
        ggplot2::geom_col(data = segment_data, width = segment_data$bin_width[[1]]), # nolint: line_length_linter.
        segment_data = segment_data
      )
    }
    if (show_obs) {
      max_bar_height <- max(plot_data_hist$histogram_y, na.rm = TRUE)
      p <- p %lean+% util_create_lean_ggplot(
        ggplot2::geom_point(
          ggplot2::aes(y = 0),
          color = "darkgray",
          position = ggplot2::position_jitter(
            height = 0.01 * max_bar_height,
            width = 0
          ),
          size = 1,
          alpha = 0.6
        ),
        max_bar_height = max_bar_height
      )
    }
  } else if (plot_histogram && !plot_histogram_integer) {
    p <- util_create_lean_ggplot(
      ggplot2::ggplot(
        plot_data,
        ggplot2::aes(
          x = .data[["values"]],
          fill = .data[["limit_violations"]]
        )
      ) %lean+%
        ggplot2::geom_histogram(bins = 30) %lean+%
        ggplot2::facet_wrap(ggplot2::vars(.data[["stratum"]]),
          ncol = 1,
          scales = "free_y"
        ),
      plot_data = plot_data
    )
    if (show_obs) {
      p <- p %lean+% util_create_lean_ggplot(
        ggplot2::geom_point(
          ggplot2::aes(y = 0),
          color = "darkgray",
          position = ggplot2::position_jitter(height = 0.1, width = 0),
          size = 1,
          alpha = 0.6
        )
      )
    }
  } else {
    plot_data_count <- stats::aggregate(
      list(n = rep(1, nrow(plot_data))),
      plot_data[, c("values", "stratum", "limit_violations"), drop = FALSE],
      sum
    )
    p <- util_create_lean_ggplot(
      ggplot2::ggplot(
        plot_data_count,
        ggplot2::aes(
          x = .data[["values"]],
          y = .data[["n"]],
          fill = .data[["limit_violations"]]
        )
      ) %lean+%
        ggplot2::geom_col(width = 0.8) %lean+%
        ggplot2::facet_wrap(ggplot2::vars(.data[["stratum"]]),
          ncol = 1,
          scales = "free_y"
        ),
      plot_data_count = plot_data_count
    )
  }

  if (!is.null(limit_lines) && nrow(limit_lines) > 0) {
    p <- p %lean+% util_create_lean_ggplot(
      ggplot2::geom_vline(
        data = limit_lines,
        ggplot2::aes(
          xintercept = .data[["values"]],
          linetype = .data[["limits"]],
          color = .data[["limits"]]
        )
      ),
      limit_lines = limit_lines
    )
  }
  p <- util_lazy_add_coord(p, fli)
  p <- p %lean+%
    ggplot2::scale_fill_manual(
      values = fill_values,
      breaks = fill_breaks,
      drop = FALSE,
      guide = "none"
    ) %lean+%
    ggplot2::labs(x = paste0(xlb), y = "") %lean+%
    ggplot2::theme_minimal() %lean+%
    ggplot2::theme(
      title = spec_txt,
      axis.text.x = spec_txt,
      axis.text.y = spec_txt,
      axis.title.x = spec_txt,
      axis.title.y = spec_txt
    )

  if (!is.null(limit_lines) && nrow(limit_lines) > 0) {
    p <- p %lean+%
      ggplot2::scale_linetype_discrete(name = "limits") %lean+%
      ggplot2::scale_color_discrete(name = "limits")
  }
  if (is_datetime_var) {
    p <- p %lean+% util_create_lean_ggplot(
      ggplot2::scale_x_datetime(expand = ggplot2::expansion(mult = 0.1))
    )
  } else if (is_time_var) {
    p <- p %lean+% util_create_lean_ggplot(
      ggplot2::scale_x_continuous(
        expand = ggplot2::expansion(mult = 0.1),
        labels = function(x) {
          util_as_character(hms::as_hms(x))
        }
      )
    )
  } else {
    p <- p %lean+% util_create_lean_ggplot(
      ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = 0.1))
    )
  }

  attr(p, "sizing_hints") <- list(
    figure_type_id = "bar_limit",
    range = nrow(plot_data),
    no_bars_in_all_w = max(1, nrow(plot_data)),
    no_char_x = nchar(round(max_plot - min_plot, digits = 0)),
    no_char_y = max(nchar(rule_labels))
  )
  p
}
