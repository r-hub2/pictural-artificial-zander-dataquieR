#' an environment with functions available for `REDcap` rules
#'
#' @return environment
#'
#' @family redcap
#' @concept metadata_management
#' @noRd
util_get_redcap_rule_env <- function() {
  redcap_env
}

#' Utility function to parse dates/date-times
#'
#' @param dt a date vector, usually as a [character()], anything should work
#' @param tryFormats see [as.POSIXct()]
#' @param optional see [as.POSIXct()]
#' @param tz see [as.POSIXct()]
#'
#' @return the parsed `dt`
#'
#' @family parser_functions
#' @concept metadata_management
#' @noRd
util_parse_date <- function(dt, tryFormats, optional = TRUE, tz) {
  # this function is needed, here, so not in an own file
  if (is.numeric(dt)) {
    dt <- as.POSIXct(dt, origin = lubridate::origin, optional = optional)
  }
  if (missing(tryFormats)) {
    if (is.character(dt)) {
      r <- suppressWarnings(lubridate::as_datetime(dt))
      not_conv <- !is.na(dt) & is.na(r)
      if (any(not_conv)) {
        r[not_conv] <- suppressWarnings(as.POSIXct(dt[not_conv],
          optional = optional,
          tryFormats =
            c(
              "%Y-%m-%dT%H:%M:%OS",
              "%Y/%m/%dT%H:%M:%OS",
              "%Y-%m-%dT%H:%M",
              "%Y/%m/%dT%H:%M",
              "%Y-%m-%d %H:%M:%OS",
              "%Y/%m/%d %H:%M:%OS",
              "%Y-%m-%d %H:%M",
              "%Y/%m/%d %H:%M",
              "%Y-%m-%d",
              "%Y/%m/%d"
            )
        ))
      }
      # r <- readr::parse_datetime(dt, %Z) too strict regarding timezones
    } else {
      r <- suppressWarnings(lubridate::as_datetime(dt))
    }
    if (!optional &&
        !all(is.na(r) == is.na(dt))) {
      util_error(
        "Invalid date/time: %s",
        util_pretty_vector_string(
          n_max = 5,
          dt[is.na(r) != is.na(dt)]
        )
      )
    }
  } else {
    r <- suppressWarnings(as.POSIXct(dt,
        tryFormats = tryFormats,
        optional = optional
      ))
  }
  if (inherits(r, "POSIXct")) {
    r <- as.POSIXct(round(r, units = "secs"))
  }
  if (missing(tz)) {
    r <- lubridate::with_tz(lubridate::force_tz(r))
  } else {
    r <- lubridate::with_tz(lubridate::force_tz(r, tzone = tz))
  }
  r
}

# nolint start: line_length_linter.
#' Utility function to parse times
#'
#' @param to a time-only vector, usually as a [character()], anything should
#'           work
#' @param tryFormats Character vector of time formats tried in order,
#'                   similar to [as.POSIXct()]. Defaults to "%H:%M:%S",
#'                   "%H:%M", "%H%M%S", "%H%M".
#' @param optional If FALSE, stop on un-parseable entries; if TRUE, set to NA.
#' @param tz Optional timezone; if missing, behaves like [lubridate::force_tz()].
#'
#' @return An [hms::hms] vector.
#'
#' @noRd
#' @family parser_functions
#' @concept metadata_management
# nolint end
util_parse_time <- function(
  to,
  tryFormats = c("%H:%M:%S", "%H:%M", "%H%M%S", "%H%M"),
  optional = TRUE,
  tz
) {
  # --------------------------------------------------------------------------
  # Early exit for already correct classes
  # --------------------------------------------------------------------------
  if (inherits(to, "hms")) {
    return(to)
  }
  if (inherits(to, "difftime")) {
    return(hms::as_hms(to))
  }
  if (inherits(to, "POSIXt")) {
    return(hms::as_hms(to))
  }

  # --------------------------------------------------------------------------
  # Handle numeric input
  # --------------------------------------------------------------------------
  if (is.numeric(to)) {
    if (all(is.finite(to)) && all(to >= 0 & to < 1, na.rm = TRUE)) {
      # Excel-style fraction of a day
      return(hms::as_hms(to * 24 * 3600))
    }

    # Try HHMMSS / HHMM forms if integers
    to_chr <- sprintf("%06d", as.integer(to))
    suppressWarnings({
      trial <- tryCatch(
        as.difftime(to_chr, format = "%H%M%S", units = "secs"),
        error = function(e) NULL
      )
    })
    if (!is.null(trial) && any(!is.na(trial))) {
      return(hms::as_hms(trial))
    }

    # Fallback: interpret as seconds since epoch
    if (missing(tz)) {
      to <- as.POSIXct(to, origin = lubridate::origin)
      to <- lubridate::force_tz(to, tzone = lubridate::tz(to))
    } else {
      to <- as.POSIXct(to, origin = lubridate::origin, tz = tz)
      to <- lubridate::force_tz(to, tzone = tz)
    }
    return(hms::as_hms(to))
  }

  if (is.list(to)) {
    return(lapply(to, util_parse_time,
        tryFormats = tryFormats,
        optional = optional,
        tz = tz
      ))
  }

  # --------------------------------------------------------------------------
  # Character input normalization
  # --------------------------------------------------------------------------
  if (is.factor(to)) to <- as.character(to)
  to_chr <- as.character(to)
  to_chr[trimws(to_chr) == ""] <- NA_character_

  # Detect and reject date-time patterns
  has_date_pattern <- grepl("\\d{4}[-/]\\d{2}[-/]\\d{2}", to_chr, ignore.case = TRUE) # nolint: line_length_linter.
  if (any(has_date_pattern, na.rm = TRUE)) {
    bad <- unique(to_chr[has_date_pattern])
    util_warning(
      "Detected full date-time values; these are not pure times. Examples: %s",
      paste(utils::head(bad, 3L), collapse = ", ")
    )
    to_chr[has_date_pattern] <- NA_character_
  }

  n <- length(to_chr)
  parsed <- as.difftime(rep(NA_real_, n), units = "secs")

  # --------------------------------------------------------------------------
  # Try parsing with multiple formats
  # --------------------------------------------------------------------------
  for (fmt in tryFormats) {
    idx <- which(is.na(parsed))
    if (length(idx) == 0L) break
    suppressWarnings({
      trial <- tryCatch(
        as.difftime(to_chr[idx], format = fmt, units = "secs"),
        error = function(e) rep(as.difftime(NA_real_, units = "secs"), length(idx)) # nolint: line_length_linter.
      )
    })
    ok <- !is.na(trial)
    if (any(ok)) parsed[idx[ok]] <- trial[ok]
  }

  # --------------------------------------------------------------------------
  # Warn or error for failed parsing
  # --------------------------------------------------------------------------
  if (anyNA(parsed)) {
    bad <- unique(to_chr[is.na(parsed) & !is.na(to_chr)])
    if (length(bad)) {
      msg <- sprintf(
        "Failed to parse %d/%d values. Examples: %s",
        sum(is.na(parsed)), n,
        paste(utils::head(bad, 3L), collapse = ", ")
      )
      if (!optional) {
        util_error("%s", msg)
      } else {
        util_warning("%s", msg)
      }
    }
  }

  # --------------------------------------------------------------------------
  # Return final hms vector
  # --------------------------------------------------------------------------
  hms::as_hms(parsed)
}


redcap_env <- (function() {
  penv <- new.env(parent = emptyenv())
  as_numeric_matrix <- function(all_data) {
    if (!length(all_data)) {
      return(matrix(numeric(0), nrow = 0, ncol = 0))
    }
    matrix(
      as.numeric(all_data),
      nrow = nrow(all_data),
      ncol = ncol(all_data),
      dimnames = dimnames(all_data)
    )
  }

  ### generic handling of redcap NAs ####
  # see gen_op below

  R_na2redcap_na <- function(x) {
    if (!is.vector(x)) {
      util_error("no complex arguemnts supported, esp. no %s",
        dQuote(class(x)),
        applicability_problem = TRUE
      )
    }
    if (is.factor(x) && !("" %in% levels(x))) {
      levels(x) <- c(levels(x), "")
    }
    if (!prep_dq_data_type_of(x) %in% c(
      DATA_TYPES$DATETIME,
      DATA_TYPES$TIME
    )) {
      if (any(is.na(x))) x[is.na(x)] <- ""
    }
    x
  }

  redcap_na2R_na <- function(x) {
    orig_dim <- dim(x)
    orig_dimnames <- dimnames(x)
    if (!is.atomic(x)) {
      util_error("no complex arguemnts supported, esp. no %s",
        dQuote(class(x)),
        applicability_problem = TRUE
      )
    }
    if (is.factor(x) && !("" %in% levels(x))) {
      levels(x) <- c(levels(x), "")
    }
    # Historical REDCap NA conversion branch removed here. Inspect with
    # `git show 341cea140c -- R/util_get_redcap_rule_env.R` before restoring.
    if (!prep_dq_data_type_of(x) %in% c(
      DATA_TYPES$DATETIME,
      DATA_TYPES$TIME
    )) {
      x_na <- is.na(x)
      if (any(x_na)) {
        if (is.factor(x)) {
          x[x_na] <- ""
        } else {
          x <- as.character(x)
          x[x_na] <- ""
        }
      }
    }
    if (!is.null(orig_dim)) {
      dim(x) <- orig_dim
      dimnames(x) <- orig_dimnames
    }
    x
  }

  decorate_fkt_redcap_na <- function(fkt) {
    function(...) {
      args <- list(...)
      args <- lapply(args, R_na2redcap_na)
      r <- do.call(fkt, args)
      redcap_na2R_na(r)
    }
  }

  gen_op <- function(op) {
    # support REDcap NAs in operators; see decorate_fkt_redcap_na
    function(x, y) {
      # Historical debug hooks were removed here; inspect Git history before
      # restoring interactive operator tracing.
      if (identical(op, `&`) ||
          identical(op, `|`)
      ) {
        x <- suppressWarnings(as.logical(x))
        y <- suppressWarnings(as.logical(y))
      }
      if (is.factor(x) && !("" %in% levels(x))) {
        levels(x) <- c(levels(x), "")
      }
      if (is.factor(y) && !("" %in% levels(y))) {
        levels(y) <- c(levels(y), "")
      }
      # Historical datatype debug messages were removed here.
      if (prep_dq_data_type_of(x) %in% c(
        DATA_TYPES$DATETIME,
        DATA_TYPES$TIME
      ) &&
        prep_dq_data_type_of(y) == DATA_TYPES$STRING) {
        yrcna <- y == ""
        if (any(yrcna)) y[[yrcna]] <- NA
      }
      if (prep_dq_data_type_of(x) == DATA_TYPES$STRING &&
        prep_dq_data_type_of(y) %in% c(
          DATA_TYPES$DATETIME,
          DATA_TYPES$TIME
        )) {
        xrcna <- x == ""
        if (any(xrcna)) x[[x == ""]] <- NA
      }
      if (prep_dq_data_type_of(x) == DATA_TYPES$DATETIME ||
          prep_dq_data_type_of(y) == DATA_TYPES$DATETIME) {
        xx <- try(util_parse_date(x, optional = FALSE), silent = TRUE)
        yy <- try(util_parse_date(y, optional = FALSE), silent = TRUE)
        if (util_is_try_error(xx)) {
          util_error(
            "Rule evaluation: Could not interpret %s as %s",
            util_pretty_vector_string(x), sQuote(DATA_TYPES$DATETIME)
          )
        }
        if (util_is_try_error(yy)) {
          util_error(
            "Rule evaluation: Could not interpret %s as %s",
            util_pretty_vector_string(y), sQuote(DATA_TYPES$DATETIME)
          )
        }
        res <- op(xx, yy)
        resrna <- is.na(res)
        if (identical(op, .Primitive("!="))) {
          if (any(resrna)) {
            res[resrna] <- TRUE
            res[resrna & is.na(x) & is.na(y)] <- FALSE
          }
        } else if (identical(op, .Primitive("=="))) {
          if (any(resrna)) {
            res[resrna] <- FALSE
            res[resrna & is.na(x) & is.na(y)] <- TRUE
          }
        } else {
          if (!inherits(res, "difftime") && any(resrna)) res[resrna] <- ""
        }
      } else if (prep_dq_data_type_of(x) == DATA_TYPES$TIME ||
          prep_dq_data_type_of(y) == DATA_TYPES$TIME) {
        xx <- try(util_parse_time(x, optional = FALSE), silent = TRUE)
        yy <- try(util_parse_time(y, optional = FALSE), silent = TRUE)
        if (util_is_try_error(xx)) {
          util_error(
            "Rule evaluation: Could not interpret %s as %s",
            util_pretty_vector_string(x), sQuote(DATA_TYPES$TIME)
          )
        }
        if (util_is_try_error(yy)) {
          util_error(
            "Rule evaluation: Could not interpret %s as %s",
            util_pretty_vector_string(y), sQuote(DATA_TYPES$TIME)
          )
        }
        res <- op(xx, yy)
        resrna <- is.na(res)
        if (identical(op, .Primitive("!="))) {
          if (any(resrna)) {
            res[resrna] <- TRUE
            res[resrna & is.na(x) & is.na(y)] <- FALSE
          }
        } else if (identical(op, .Primitive("=="))) {
          if (any(resrna)) {
            res[resrna] <- FALSE
            res[resrna & is.na(x) & is.na(y)] <- TRUE
          }
        } else {
          if (any(resrna)) res[resrna] <- ""
        }
      } else {
        xrcna <- util_empty(x)
        yrcna <- util_empty(y)
        if (is.logical(x) && is.logical(y)) {
          res <- op(x, y)
          if (any(is.na(res))) {
            res[is.na(res)] <- ""
          }
        } else if (is.numeric(x) && is.numeric(y)) {
          res <- op(x, y)
          if (any(is.na(res))) {
            res[is.na(res)] <- ""
          }
        } else if (prep_dq_data_type_of(x) == DATA_TYPES$DATETIME &&
            prep_dq_data_type_of(y) == DATA_TYPES$DATETIME) {
          xx <- try(util_parse_date(x, optional = FALSE), silent = TRUE)
          yy <- try(util_parse_date(y, optional = FALSE), silent = TRUE)
          if (util_is_try_error(xx)) {
            util_error(
              "Rule evaluation: Could not interpret %s as %s",
              util_pretty_vector_string(x), sQuote(DATA_TYPES$DATETIME)
            )
          }
          if (util_is_try_error(yy)) {
            util_error(
              "Rule evaluation: Could not interpret %s as %s",
              util_pretty_vector_string(y), sQuote(DATA_TYPES$DATETIME)
            )
          }
          res <- op(xx, yy)
          if (any(is.na(res))) {
            res[is.na(res)] <- ""
          }
        } else if (prep_dq_data_type_of(x) == DATA_TYPES$TIME &&
            prep_dq_data_type_of(y) == DATA_TYPES$TIME) {
          xx <- try(util_parse_time(x, optional = FALSE), silent = TRUE)
          yy <- try(util_parse_time(y, optional = FALSE), silent = TRUE)
          if (util_is_try_error(xx)) {
            util_error(
              "Rule evaluation: Could not interpret %s as %s",
              util_pretty_vector_string(x), sQuote(DATA_TYPES$TIME)
            )
          }
          if (util_is_try_error(yy)) {
            util_error(
              "Rule evaluation: Could not interpret %s as %s",
              util_pretty_vector_string(y), sQuote(DATA_TYPES$TIME)
            )
          }
          res <- op(xx, yy)
          if (any(is.na(res))) {
            res[is.na(res)] <- ""
          }
        } else {
          if (any(xrcna)) x[is.na(x)] <- ""
          if (any(yrcna)) y[is.na(y)] <- ""
          res <- try(op(x, y), silent = TRUE)
          if (inherits(res, "try-error") ||
              (is.factor(x) && !is.factor(y)) ||
              (!is.factor(x) && is.factor(y))) {
            .x <- trimws(as.character(x))
            .x[is.na(x)] <- ""
            .y <- trimws(as.character(y))
            .y[is.na(y)] <- ""
            res <- try(op(as.character(.x), as.character(.y)), silent = TRUE)
            if (inherits(res, "try-error")) {
              res <- try(op(readr::parse_guess(.x), readr::parse_guess(.y)),
                silent = TRUE
              )
            }
          }
          res
        }
      }
      if (inherits(res, "difftime")) {
        res <- as.numeric(res, units = "secs")
        res[is.na(res)] <- ""
      }
      res
    }
  }

  ### Operators and functions ####

  # avoid operator precedence by R
  penv[["or"]] <- gen_op(base::`|`)
  attr(penv[["or"]], "prio") <- -2
  penv[["and"]] <- gen_op(base::`&`)
  attr(penv[["and"]], "prio") <- -1

  penv[["("]] <- base::`(`


  penv[["**"]] <- gen_op(base::`^`) #  must not be in prio=3 if prio 3 has no order # nolint: line_length_linter.
  attr(penv[["**"]], "order") <- -1
  attr(penv[["**"]], "prio") <- 3

  penv[["^"]] <- gen_op(base::`^`)
  attr(penv[["^"]], "prio") <- 3

  penv[["+"]] <- gen_op(base::`+`)
  attr(penv[["+"]], "prio") <- 1
  penv[["-"]] <- gen_op(base::`-`)
  attr(penv[["-"]], "prio") <- 1
  penv[["*"]] <- gen_op(base::`*`)
  attr(penv[["*"]], "prio") <- 2
  penv[["/"]] <- gen_op(base::`/`)
  attr(penv[["/"]], "prio") <- 2

  penv[["prod"]] <- decorate_fkt_redcap_na(base::`prod`)
  penv[["sum"]] <- decorate_fkt_redcap_na(base::`sum`)

  make_num <- function(number) {
    orig_dim <- dim(number)
    orig_dimnames <- dimnames(number)
    was_posixct <- inherits(number, "POSIXct")
    tzone <- util_attr(number, "tzone", exact = TRUE)
    number[trimws(number) == ""] <- NA
    numnumber <- as.numeric(number)
    if (suppressWarnings((!all(is.na(numnumber) == is.na(number))))) {
      util_error(
        "%s not numeric",
        dQuote(util_deparse1(substitute(number)))
      )
    }
    if (was_posixct) {
      numnumber <- as.POSIXct(numnumber, origin = "1970-01-01", tz = tzone)
    }
    if (!is.null(orig_dim)) {
      dim(numnumber) <- orig_dim
      dimnames(numnumber) <- orig_dimnames
    }
    numnumber
  }

  get_input_as_num_matrix <- function(...) {
    args <- list(...)

    # Check if all inputs are POSIXct
    all_posixct <- all(vapply(args, inherits, logical(1), "POSIXct"))

    # Harmonize time zones if all are POSIXct
    if (all_posixct) {
      tzones <- vapply(args, function(x) {
        util_attr(x, "tzone", exact = TRUE) %||% ""
      }, character(1))
      tzones <- tzones[tzones != ""]
      if (length(unique(tzones)) > 1) {
        util_warning(
          "Different time zones found; coercing all to first non-empty one."
        )
      }
      common_tzone <- if (length(tzones) > 0) tzones[[1]] else "UTC"

      # Convert all elements to common time zone
      args <- lapply(args, function(x) as.POSIXct(x, tz = common_tzone))
    }

    # Manually cbind, set class later
    x <- do.call(cbind, args)

    # Restore POSIXct class if applicable
    if (all_posixct) {
      class(x) <- c("POSIXct", "POSIXt", "matrix")
      attr(x, "tzone") <- common_tzone
    }

    # Convert to numeric (or retain POSIXct structure)
    x <- make_num(x)

    x
  }

  get_rule_binding <- function(name, eval_env) {
    while (!identical(eval_env, emptyenv())) {
      if (exists(name, envir = eval_env, inherits = FALSE)) {
        return(get(name, envir = eval_env, inherits = FALSE))
      }
      eval_env <- parent.env(eval_env)
    }
    NULL
  }

  get_rule_meta <- function(all_data, eval_env) {
    var_names <- colnames(all_data)
    if (is.null(var_names)) {
      var_names <- character(0)
    }
    meta_data <- get_rule_binding("[meta_data]", eval_env)
    if (is.null(meta_data) || !is.data.frame(meta_data) ||
        !VAR_NAMES %in% colnames(meta_data)) {
      return(data.frame(row.names = var_names))
    }
    label_col <- get_rule_binding("[label_col]", eval_env)
    idx <- match(var_names, meta_data[[VAR_NAMES]])
    if (!util_empty(label_col) && label_col %in% colnames(meta_data)) {
      missing_idx <- is.na(idx)
      idx[missing_idx] <- match(var_names[missing_idx], meta_data[[label_col]])
    }
    meta_data <- meta_data[idx, , drop = FALSE]
    rownames(meta_data) <- var_names
    meta_data
  }

  get_value_label_bounds <- function(value_labels) {
    if (util_empty(value_labels) || !is.character(value_labels)) {
      return(NULL)
    }
    value_labels <- unlist(strsplit(value_labels, paste0(
      "\\s*\\",
      SPLIT_CHAR,
      "\\s*"
    )))
    values <- suppressWarnings(as.numeric(sub("\\s*=.*$", "", value_labels)))
    values <- values[is.finite(values)]
    if (!length(values)) {
      return(NULL)
    }
    range(values)
  }

  get_value_label_table_bounds <- function(value_label_table) {
    if (util_empty(value_label_table)) {
      return(NULL)
    }
    value_label_table <- try(
      prep_get_data_frame(value_label_table),
      silent = TRUE
    )
    if (util_is_try_error(value_label_table) ||
        !CODE_VALUE %in% colnames(value_label_table)) {
      return(NULL)
    }
    values <- suppressWarnings(as.numeric(value_label_table[[CODE_VALUE]]))
    values <- values[is.finite(values)]
    if (!length(values)) {
      return(NULL)
    }
    range(values)
  }

  reverse_code_ssi_items <- function(all_data, eval_env) {
    meta <- get_rule_meta(all_data, eval_env)
    if (!ITEM_TYPE %in% colnames(meta)) {
      return(all_data)
    }
    reverse_idx <- grepl("REVERSE", meta[[ITEM_TYPE]],
      ignore.case = TRUE
    )
    reverse_idx[is.na(reverse_idx)] <- FALSE
    if (!any(reverse_idx)) {
      return(all_data)
    }
    for (var_name in rownames(meta)[reverse_idx]) {
      bounds <- NULL
      if (VALUE_LABELS %in% colnames(meta)) {
        bounds <- get_value_label_bounds(meta[
          var_name,
          VALUE_LABELS,
          drop = TRUE
        ])
      }
      if (is.null(bounds) && VALUE_LABEL_TABLE %in% colnames(meta)) {
        bounds <- get_value_label_table_bounds(
          meta[var_name, VALUE_LABEL_TABLE, drop = TRUE]
        )
      }
      if (is.null(bounds)) {
        values <- all_data[, var_name, drop = TRUE]
        values <- values[is.finite(values)]
        bounds <- if (length(values)) {
          range(values)
        } else {
          c(NA_real_, NA_real_)
        }
      }
      if (all(is.finite(bounds))) {
        all_data[, var_name] <- sum(bounds) - all_data[, var_name, drop = TRUE]
      }
    }
    all_data
  }

  assert_no_attention_items <- function(all_data, metric, eval_env) {
    meta <- get_rule_meta(all_data, eval_env)
    if (!ITEM_TYPE %in% colnames(meta)) {
      return(invisible())
    }
    attention_idx <- grepl("^(BOGUS|INSTRUCTED)", meta[[ITEM_TYPE]],
      ignore.case = TRUE
    )
    attention_idx[is.na(attention_idx)] <- FALSE
    if (any(attention_idx)) {
      util_error(
        "%s cannot be calculated from attention-check variables: %s.",
        metric,
        util_pretty_vector_string(rownames(meta)[attention_idx]),
        applicability_problem = TRUE
      )
    }
    invisible()
  }

  # All functions from ####
  # https://docs.google.com/document/d/1l3nGBgqqPKi5PtMe75g7q0dny8QzGMd_/edit?tab=t.0 # nolint: line_length_linter.

  penv[["round"]] <- function(number, decimal_places = 0) {
    numnumber <- make_num(number)
    decimal_places <- make_num(decimal_places)
    redcap_na2R_na(round(numnumber, decimal_places))
  }
  attr(penv[["round"]], "order") <- 99

  penv[["roundup"]] <- function(number, decimal_places = 0) {
    number <- make_num(number)
    decimal_places <- make_num(decimal_places)
    redcap_na2R_na(ceiling(number * (10^(decimal_places))) / (10^(decimal_places))) # nolint: line_length_linter.
  }

  penv[["rounddown"]] <- function(number, decimal_places = 0) {
    number <- make_num(number)
    decimal_places <- make_num(decimal_places)
    redcap_na2R_na(floor(number * (10^(decimal_places))) / (10^(decimal_places))) # nolint: line_length_linter.
  }

  penv[["sqrt"]] <- function(number) {
    number <- make_num(number)
    redcap_na2R_na(sqrt(number))
  }

  penv[["abs"]] <- function(number) {
    number <- make_num(number)
    redcap_na2R_na(abs(number))
  }

  penv[["min"]] <- function(...) {
    x <- make_num(c(...))
    redcap_na2R_na(min(x, na.rm = TRUE))
  }

  penv[["max"]] <- function(...) {
    x <- make_num(c(...))
    redcap_na2R_na(max(x, na.rm = TRUE))
  }

  penv[["mean"]] <- function(...) {
    x <- make_num(c(...))
    redcap_na2R_na(mean(x, na.rm = TRUE))
  }

  penv[["rowMin"]] <- function(...) {
    x <- get_input_as_num_matrix(...)
    x <- suppressWarnings(apply(x, 1, min, na.rm = TRUE))
    if (inherits(x, "numeric") && all(vapply(list(...), inherits, logical(1), "POSIXct"))) { # nolint: line_length_linter.
      x <- as.POSIXct(x,
        origin = "1970-01-01",
        tz = util_attr(list(...)[[1]], "tzone", exact = TRUE)
      )
    }
    redcap_na2R_na(x)
  }

  penv[["rowMax"]] <- function(...) {
    x <- get_input_as_num_matrix(...)
    x <- suppressWarnings(apply(x, 1, max, na.rm = TRUE))
    if (inherits(x, "numeric") && all(vapply(list(...), inherits, logical(1), "POSIXct"))) { # nolint: line_length_linter.
      x <- as.POSIXct(x,
        origin = "1970-01-01",
        tz = util_attr(list(...)[[1]], "tzone", exact = TRUE)
      )
    }
    redcap_na2R_na(x)
  }

  penv[["rowMean"]] <- function(...) {
    x <- get_input_as_num_matrix(...)
    x <- apply(x, 1, mean, na.rm = TRUE)
    if (inherits(x, "numeric") && all(vapply(list(...), inherits, logical(1), "POSIXct"))) { # nolint: line_length_linter.
      x <- as.POSIXct(x,
        origin = "1970-01-01",
        tz = util_attr(list(...)[[1]], "tzone", exact = TRUE)
      )
    }
    redcap_na2R_na(x)
  }

  penv[["median"]] <- function(...) {
    x <- make_num(c(...))
    redcap_na2R_na(median(x, na.rm = TRUE))
  }

  penv[["stdev"]] <- function(...) {
    x <- make_num(c(...))
    redcap_na2R_na(sd(x, na.rm = TRUE))
  }

  penv[["util_parse_date"]] <- util_parse_date
  attr(penv[["util_parse_date"]], "internal") <- TRUE

  penv[["util_parse_time"]] <- util_parse_time
  attr(penv[["util_parse_time"]], "internal") <- TRUE

  successive_dates <- function(..., strictly = TRUE) {
    util_expect_scalar(strictly, check_type = is.logical)
    if (length(unique(vapply(list(...), length, FUN.VALUE = integer(1))))
      != 1) {
      util_warning(
        c(
          "%s was called with vectors of differing lengths. This should",
          "never happen."
        ),
        sQuote("successive_dates")
      )
      return(FALSE)
    }
    all_dates <- list(...)
    FUN <- function(...) {
      dates <- list(...)
      if (length(dates) < 2) {
        return(TRUE)
      }
      is_timepoint <- vapply(dates, lubridate::is.timepoint,
        FUN.VALUE =
          logical(1)
      )
      if (!all(is_timepoint)) {
        dates[!is_timepoint] <- lapply(
          dates[!is_timepoint],
          function(x) {
            if (util_empty(x)) {
              NULL
            } else {
              try(
                util_parse_date(x,
                  tryFormats =
                    c(
                      "%Y-%m-%d %H:%M:%OS",
                      "%Y-%m-%d"
                    ),
                  optional = FALSE
                ),
                silent = TRUE
              )
            }
          }
        )
        dates <- dates[!vapply(dates, is.null, FUN.VALUE = logical(1))]
        is_timepoint <- vapply(dates, lubridate::is.timepoint,
          FUN.VALUE =
            logical(1)
        )
        if (!all(is_timepoint)) {
          util_warning(
            c(
              "Found non-dates in %s: %s, so,",
              "dates are not successive."
            ),
            sQuote("successive_dates"),
            util_pretty_vector_string(dates[!is_timepoint])
          )
          return(NA)
        }
      }
      LHS_indices_to_check <- (head(seq_along(dates), -1))
      if (strictly) {
        .cmp_fun <- function(lhs_index) {
          dates[[lhs_index]] < dates[[lhs_index + 1]]
        }
      } else {
        .cmp_fun <- function(lhs_index) {
          dates[[lhs_index]] <= dates[[lhs_index + 1]]
        }
      }
      vapply(LHS_indices_to_check,
        FUN.VALUE = logical(1),
        FUN = .cmp_fun
      )
    }
    vapply(do.call(
      what = mapply,
      args =
        c(
          all_dates,
          list(
            FUN = FUN,
            SIMPLIFY = FALSE,
            USE.NAMES = FALSE,
            MoreArgs = list()
          )
        )
    ), all, na.rm = TRUE, FUN.VALUE = logical(1))
  }

  penv[["successive_dates"]] <- function(...) {
    successive_dates(...,
      strictly = FALSE
    )
  }
  penv[["strictly_successive_dates"]] <- function(...) {
    successive_dates(..., strictly = TRUE)
  }

  penv[["maxLongStr"]] <- function(...) {
    # maximum longstring
    all_data <- cbind(...)
    apply(all_data, 1, function(rw) {
      max(rle(rw)$length)
    })
  }

  penv[["NCOL"]] <- function(...) {
    length(list(...))
  }

  penv[["NROW"]] <- function(...) {
    if (length(list(...)) < 1) {
      return(0)
    } else {
      if (1 != length(unique(vapply(list(...), length, FUN.VALUE = integer(1))))) { # nolint: line_length_linter.
        util_warning(
          c(
            "NROW was used for a set of variables with different",
            "observation counts (including NAs)"
          ),
          applicability_problem = TRUE
        )
      }
      return(length(list(...)[[1]]))
    }
  }

  penv[["perc_miss_in_row"]] <- function(...) {
    all_data <- cbind(...)
    apply(all_data, 1, function(rw) {
      100 * sum(is.na(rw)) / length(rw)
    })
  }


  penv[["CASES_NA_BELOW"]] <- function(p, ...) {
    if (inherits(p, "MISSING")) {
      p <- 100
    }
    util_expect_scalar(p,
      check_type =
        util_is_numeric_in(
          min = 0,
          max = 100
        ),
      error_message =
        "Percentage of variables need not to be NA must be a number between 0 and 100" # nolint: line_length_linter.
    )
    # filter rows with PCT_NA>x
    all_data <- as_numeric_matrix(cbind(...))
    i <- 100 * rowSums(is.na(all_data)) / ncol(all_data) <= p
    if (sum(i) < nrow(all_data)) {
      all_data[rep(!i, ncol(all_data))] <- NA
      util_message(
        "Deleting %d records because of < %d%% variables filled",
        nrow(all_data) - sum(i),
        p
      )
    }
    all_data # [i, , drop = FALSE] cannot delete records totally, later, they won't match the dataset any more. # nolint: line_length_linter.
  }

  penv[["IRV"]] <- function(...) {
    # Intra-individual Response variability
    all_data <- as_numeric_matrix(cbind(...))
    eval_env <- parent.frame()
    assert_no_attention_items(all_data, "IRV", eval_env)
    all_data <- reverse_code_ssi_items(all_data, eval_env)
    apply(all_data, 1, sd, na.rm = TRUE)
  }

  penv[["QCHISQ"]] <- function(p, df) {
    stats::qchisq(p = p, df = df)
  }

  penv[["mahalanobis_threshold"]] <- function(x) {
    if (missing(x) || length(x) == 0) {
      return(NA_real_)
    }
    util_expect_scalar(x)
    by_opt <- getOption(
      "dataquieR.MAHALANOBIS_THRESHOLD",
      dataquieR.MAHALANOBIS_THRESHOLD_default
    )
    if (tolower(trimws(by_opt)) %in% c("true", "1", "t", "+")) {
      by_opt <- dataquieR.MAHALANOBIS_THRESHOLD_default
    }
    if (tolower(trimws(x)) %in% c("true", "1", "t", "+")) {
      .r <- by_opt
    } else {
      .r <- suppressWarnings(as.numeric(x))
    }
    if (length(.r) != 1) {
      .r <- NA_real_
    }
    .r
  }
  # Psychometric syn. and ant. ----
  get_psychometric_pairs <- function(...) {
    all_data <- get_input_as_num_matrix(...)
    cor_matrix <- cor(
      all_data,
      method = "pearson",
      use = "pairwise.complete.obs"
    )
    cor_table <- as.data.frame(as.table(cor_matrix))
    colnames(cor_table) <- c("Var1", "Var2", "cor_Pearson")
    cor_table <- subset(cor_table, Var1 != Var2)
    sorted_pairs <- t(apply(
      cor_table[, c("Var1", "Var2"), drop = FALSE],
      1,
      sort
    ))
    cor_table <- cor_table[!duplicated(sorted_pairs), , drop = FALSE]
    rm(sorted_pairs)
    cor_table
  }

  penv[["PSYCHOMETRIC_SYN"]] <- function(...) {
    all_data <- get_input_as_num_matrix(...)
    cor_table <- get_psychometric_pairs(...)
    pairs_cor_threshold <- getOption(
      "dataquieR.psychometric_cor_pairs",
      dataquieR.psychometric_cor_pairs_default
    )
    util_expect_scalar(pairs_cor_threshold)
    cor_table <- cor_table[cor_table$cor_Pearson > pairs_cor_threshold, ,
      drop = FALSE
    ]
    if (nrow(cor_table) < 3) {
      util_warning(
        "Less than 3 pairs available to calculate the psychometric syn. index"
      )
      apply(all_data, 1, function(row) {
        return(NA)
      })
    } else {
      if (nrow(cor_table) < 10) {
        util_warning(
          "Only %s pairs available to calculate the psychometric syn. index",
          sQuote(nrow(cor_table))
        )
      }
      apply(all_data, 1, function(row) {
        vector1 <- as.numeric(row[cor_table$Var1])
        vector2 <- as.numeric(row[cor_table$Var2])
        if (anyNA(vector1) || anyNA(vector2)) {
          return(NA_real_)
        }
        res <- try(cor(
          vector1,
          vector2,
          method = "pearson"
        ), silent = TRUE)
        if (util_is_try_error(res)) {
          return(NA)
        } else {
          return(res)
        }
      })
    }
  }

  penv[["PSYCHOMETRIC_ANT"]] <- function(...) {
    all_data <- get_input_as_num_matrix(...)
    cor_table <- get_psychometric_pairs(...)
    pairs_cor_threshold <- getOption(
      "dataquieR.psychometric_cor_pairs",
      dataquieR.psychometric_cor_pairs_default
    )
    util_expect_scalar(pairs_cor_threshold)
    if (pairs_cor_threshold > 0) {
      pairs_cor_threshold <- pairs_cor_threshold * -1
    }
    cor_table <- cor_table[cor_table$cor_Pearson < pairs_cor_threshold, ,
      drop = FALSE
    ]
    if (nrow(cor_table) < 3) {
      util_warning(
        "Less than 3 pairs available to calculate the psychometric ant. index"
      )
      apply(all_data, 1, function(row) {
        return(NA)
      })
    } else {
      if (nrow(cor_table) < 10) {
        util_warning(
          "Only %s pairs available to calculate the psychometric ant. index",
          sQuote(nrow(cor_table))
        )
      }
      apply(all_data, 1, function(row) {
        vector1 <- as.numeric(row[cor_table$Var1])
        vector2 <- as.numeric(row[cor_table$Var2])
        if (anyNA(vector1) || anyNA(vector2)) {
          return(NA_real_)
        }
        res <- try(cor(
          vector1,
          vector2,
          method = "pearson"
        ), silent = TRUE)
        if (util_is_try_error(res)) {
          return(NA)
        } else {
          return(res)
        }
      })
    }
  }


  penv[["SUM_ATTENTION_CHECK_ITEMS"]] <- function(...) {
    all_data <- get_input_as_num_matrix(...)
    which_vars <-
      as.character(rlang::call_args(sys.call(length(sys.calls()))))
    all_data <- as.data.frame(all_data)
    colnames(all_data) <- which_vars
    meta_data <-
      get("[meta_data]", parent.frame())
    label_col <-
      get("[label_col]", parent.frame())

    # Identify attention check variables (BOGUS or INSTRUCTED) in metadata.
    attention_idx <- grepl(
      "^(BOGUS|INSTRUCTED)",
      meta_data[[ITEM_TYPE]],
      ignore.case = TRUE
    )
    attention_idx[is.na(attention_idx)] <- FALSE
    attention_vars <- meta_data[[VAR_NAMES]][attention_idx]
    if (!all(which_vars %in% attention_vars)) {
      util_error(
        m = paste0(
          "Internal error.",
          "Not all the attention check variables in the cross-item level",
          " are present in the item_level metadata"
        ),
        intrinsic_applicability_problem = FALSE,
        applicability_problem = TRUE
      )
    }
    rm(attention_vars)
    # Count of attention check variables
    if (length(which_vars) == 0) {
      util_error("No attention check variables selected.",
        applicability_problem = TRUE
      )
    }

    # Extract admissible expected values.
    item_types <- setNames(meta_data[[ITEM_TYPE]], nm = meta_data[[VAR_NAMES]])
    def_cats_attention <- setNames(
      lapply(item_types[which_vars], function(item_type) {
        raw_vals <- sub(".*:\\s*", "", item_type)
        if (is.na(raw_vals) || raw_vals == item_type) {
          return(NA_character_)
        }
        trimws(unlist(strsplit(
          raw_vals,
          paste0("\\s*\\", SPLIT_CHAR, "\\s*")
        )))
      }),
      nm = which_vars
    )
    rm(item_types)

    # Returns 0 = valid response, 1 = non-matching (wrong value or NA)
    score_matrix <- vapply(which_vars, function(var_name) {
      allowed <- def_cats_attention[[var_name]]
      observed_vals <- as.character(all_data[[var_name]])

      # 1 if NA or value not in allowed values
      ifelse(is.na(observed_vals) | !(observed_vals %in% allowed), 1, 0)
    }, FUN.VALUE = numeric(nrow(all_data)))
    ATTENTION_ERRORS_N <- rowSums(score_matrix, na.rm = TRUE)
    ATTENTION_ERRORS_N
  }

  penv[["MAHALANOBIS"]] <- function(...) {
    x <- get_input_as_num_matrix(...)
    x <- reverse_code_ssi_items(x, parent.frame())

    keep_rows <- rowSums(is.na(x)) == 0

    x_clean <- x[keep_rows, , drop = FALSE]

    # Calculation of the Mahalanobis distance
    mahalanobis(x, colMeans(x_clean), cov(x_clean))
  }

  penv[["measurement_time"]] <- function(...) {
    which_vars <-
      as.character(rlang::call_args(sys.call(length(sys.calls()))))
    meta_data <-
      get("[meta_data]", parent.frame())
    label_col <-
      get("[label_col]", parent.frame())
    if (TIME_VAR %in% colnames(meta_data)) {
      time_vars <-
        util_find_var_by_meta(which_vars,
          meta_data = meta_data,
          label_col = label_col,
          target = TIME_VAR
        )
    } else {
      util_error("Don't have any time varialbes in the metadata",
        applicability_problem = TRUE
      )
    }
    time_vars <-
      util_find_var_by_meta(time_vars,
        meta_data = meta_data,
        label_col = label_col,
        target = label_col
      )
    res <- lapply(time_vars, get, envir = parent.frame())
    res <- suppressWarnings(lapply(res, util_parse_date))
    m <- do.call(cbind, res)
    class(m) <- c("POSIXct", "POSIXt")
    m
  }
  penv[["measurement_end_time"]] <- function(...) {
    which_vars <-
      as.character(rlang::call_args(sys.call(length(sys.calls()))))
    meta_data <-
      get("[meta_data]", parent.frame())
    label_col <-
      get("[label_col]", parent.frame())
    if (TIME_VAR_END %in% colnames(meta_data)) {
      time_vars <-
        util_find_var_by_meta(which_vars,
          meta_data = meta_data,
          label_col = label_col,
          target = TIME_VAR_END
        )
    } else if (TIME_VAR %in% colnames(meta_data)) {
      time_vars <-
        util_find_var_by_meta(which_vars,
          meta_data = meta_data,
          label_col = label_col,
          target = TIME_VAR
        )
    } else {
      util_error("Don't have any time varialbes in the metadata",
        applicability_problem = TRUE
      )
    }
    time_vars <-
      util_find_var_by_meta(time_vars,
        meta_data = meta_data,
        label_col = label_col,
        target = label_col
      )
    res <- lapply(time_vars, get, envir = parent.frame())
    res <- suppressWarnings(lapply(res, util_parse_date))
    m <- do.call(cbind, res)
    class(m) <- c("POSIXct", "POSIXt")
    m
  }

  penv[["datediff"]] <- function(date1,
    date2,
    units = "",
    date_format = "Y-M-D",
    Return_Signed_Value = FALSE) {
    util_expect_scalar(
      arg_name = units,
      allow_na = FALSE, allow_null = FALSE,
      check_type = function(x) {
        x %in% c(
          "y", "M", "d",
          "h", "m", "s", ""
        )
      }
    )

    util_expect_scalar(
      arg_name = date_format,
      allow_na = FALSE, allow_null = FALSE,
      check_type = function(x) {
        x %in% c(
          "Y-M-D",
          "M-D-Y",
          "D-M-Y"
        )
      }
    )

    try_formats <- list(
      `Y-M-D` = list("%Y-%m-%d %H:%M:%OS", "%Y-%m-%d"),
      `M-D-Y` = list("%m-%d-%Y %H:%M:%OS", "%m-%d-%Y"),
      `D-M-Y` = list("%d-%m-%Y %H:%M:%OS", "%d-%m-%Y")
    )[[date_format]]

    util_expect_scalar(
      arg_name = Return_Signed_Value,
      allow_na = FALSE, allow_null = FALSE,
      check_type = is.logical
    )

    util_expect_scalar(
      arg_name = date1,
      allow_na = TRUE,
      allow_null = FALSE,
      allow_more_than_one = TRUE,
      check_type = function(x) {
        prep_dq_data_type_of(x) %in%
          c(
            DATA_TYPES$DATETIME,
            DATA_TYPES$STRING
          )
      }
    )

    util_expect_scalar(
      arg_name = date2,
      allow_na = TRUE,
      allow_null = FALSE,
      allow_more_than_one = TRUE,
      check_type = function(x) {
        prep_dq_data_type_of(x) %in%
          c(
            DATA_TYPES$DATETIME,
            DATA_TYPES$STRING
          )
      }
    )

    if (prep_dq_data_type_of(date1) == DATA_TYPES$STRING) {
      try(
        {
          date1 <- util_parse_date(date1,
            tryFormats = unlist(try_formats),
            optional = FALSE
          )
        },
        silent = TRUE
      )
      if (prep_dq_data_type_of(date1) != DATA_TYPES$DATETIME) {
        date1 <- NA
      }
    }
    if (prep_dq_data_type_of(date2) == DATA_TYPES$STRING) {
      try(
        {
          date2 <- util_parse_date(date2,
            tryFormats = unlist(try_formats),
            optional = FALSE
          )
        },
        silent = TRUE
      )
      if (prep_dq_data_type_of(date2) != DATA_TYPES$DATETIME) {
        date2 <- NA
      }
    }

    if (units == "") units <- "a"

    u <-
      c(
        "a" = "auto",
        "s" = "secs",
        "m" = "mins",
        "h" = "hours",
        "d" = "days",
        "M" = "months",
        "y" = "years"
      )[[units]]

    if (units %in% c("M", "y")) {
      u <- "days"
    }

    res <- as.numeric(difftime(date1, date2,
        units = u
      ))

    if (units == "M") {
      res <- res / 30.44
    } else if (units == "y") {
      res <- res / 365.2425
    }

    if (!Return_Signed_Value) {
      res <- abs(res)
    }

    redcap_na2R_na(res)
  }

  # Historical direct REDCap operator binding prototype removed here. It
  # collided with REDcapexpression grammar; inspect Git history before
  # restoring support for REDCap-style conditional rules.

  # Note: For each name, ensure, that no op function with the same prefix is in
  # the same prio class
  # So, attr(., "order") is needed. Default order will be 0
  penv[["="]] <- gen_op(`==`)
  attr(penv[["="]], "prio") <- 0
  penv[["=="]] <- gen_op(`==`) #  must not be in prio=0 if prio 0 has no order
  attr(penv[["=="]], "order") <- -1
  attr(penv[["=="]], "prio") <- 0
  penv[["!="]] <- gen_op(`!=`)
  attr(penv[["!="]], "prio") <- 0
  penv[["<>"]] <- gen_op(`!=`) #  must not be in prio=0 if prio 0 has no order
  attr(penv[["<>"]], "order") <- -1
  attr(penv[["<>"]], "prio") <- 0
  penv[["<"]] <- gen_op(`<`)
  attr(penv[["<"]], "prio") <- 0
  penv[[">"]] <- gen_op(`>`)
  attr(penv[[">"]], "prio") <- 0
  penv[["<="]] <- gen_op(`<=`)
  attr(penv[["<="]], "prio") <- 0
  attr(penv[["<="]], "order") <- -1
  penv[[">="]] <- gen_op(`>=`)
  attr(penv[[">="]], "prio") <- 0
  attr(penv[[">="]], "order") <- -1

  penv[["set"]] <- function(...) {
    base::unique(base::c(...))
  }

  penv[["interval"]] <- function(inc_l, low, upp, inc_u) {
    util_expect_scalar(inc_l, check_type = base::is.logical)
    util_expect_scalar(inc_u, check_type = base::is.logical)
    if (inherits(low, "POSIXct")) {
      util_expect_scalar(low, check_type = function(x) {
        inherits(low, "POSIXct")
      })
    } else if (inherits(low, "hms")) {
      util_expect_scalar(low, check_type = function(x) {
        inherits(low, "hms")
      })
    } else {
      util_expect_scalar(low, check_type = base::is.numeric)
    }
    if (inherits(upp, "POSIXct")) {
      util_expect_scalar(upp, check_type = function(x) {
        inherits(upp, "POSIXct")
      })
    } else if (inherits(low, "hms")) {
      util_expect_scalar(low, check_type = function(x) {
        inherits(low, "hms")
      })
    } else {
      util_expect_scalar(upp, check_type = base::is.numeric)
    }
    i <- list(inc_l = inc_l, low = low, upp = upp, inc_u = inc_u)
    class(i) <- "interval"
    i
  }
  attr(penv[["interval"]], "order") <- -1 # before in

  my_in <- gen_op(`%in%`)

  penv[["in"]] <- function(x, table) {
    if (inherits(table, "interval")) {
      r <- rep(TRUE, length(x))
      inc_l <- table$inc_l
      if (is.infinite(table$low) && table$low < 0) { # -Inf
        inc_l <- TRUE # despite the math rules, we have to "include" Inf, so
        # open-ended lower intervals keep negative infinity included
      }
      inc_u <- table$inc_u
      if (is.infinite(table$upp) && table$upp > 0) { # positive infinity
        inc_u <- TRUE # despite the math rules, we have to "include" Inf, so
        # open-ended upper intervals keep positive infinity included
      }
      if (inc_l) {
        r <- r & (x >= table$low)
      } else {
        r <- r & (x > table$low)
      }
      if (inc_u) {
        r <- r & (x <= table$upp)
      } else {
        r <- r & (x < table$upp)
      }
      r
    } else {
      my_in(x, table)
    }
  }
  attr(penv[["in"]], "prio") <- -1

  penv[["not in"]] <- function(x, table) {
    !penv[["in"]](x, table)
  }
  attr(penv[["not in"]], "prio") <- -1
  attr(penv[["not in"]], "order") <- -1

  penv[["if"]] <- decorate_fkt_redcap_na(function(test, yes, no) {
    ifelse(test, yes, no)
  })

  penv[["not"]] <- decorate_fkt_redcap_na(function(x0) {
    x <- suppressWarnings(as.logical(x0))
    which_not <- util_empty(x) != util_empty(x0)
    if (any(which_not)) {
      util_warning("Not a logical value: %s. %s returns %s",
        util_pretty_vector_string(paste(x0[which_not]), n_max = 4),
        sQuote("not"),
        util_pretty_vector_string(paste(x[which_not])),
        n_max = 4
      )
      NA
    } else {
      !x
    }
  })

  penv[["pi"]] <- pi
  penv[["e"]] <- exp(1)
  penv[["pearson"]] <- function(...) {
    x <- get_input_as_num_matrix(...)
    x <- cor(x, use = "complete.obs", method = "pearson")
    x
  }
  penv[["spearman"]] <- function(...) {
    x <- get_input_as_num_matrix(...)
    x <- cor(x, use = "complete.obs", method = "spearman")
    x
  }
  penv[["kendall"]] <- function(...) {
    x <- get_input_as_num_matrix(...)
    x <- cor(x, use = "complete.obs", method = "kendall")
    x
  }

  penv[["."]] <- structure(NA, class = "MISSING")

  `in` <- function(...) {
    `%in%`(...)
  }

  # Historical identity helper binding removed here.

  makeActiveBinding("today", function() {
    util_parse_date(Sys.Date())
  }, penv)

  penv
})()
