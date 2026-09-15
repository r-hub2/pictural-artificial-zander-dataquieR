#' Internal helper: readr cols from metadata
#'
#' @noRd
util_readr_cols_from_metadata <- function(meta_data) {
  util_expect_data_frame(meta_data, c(VAR_NAMES, DATA_TYPE))
  r <- setNames(readr_coltypes[trimws(tolower(meta_data[[DATA_TYPE]]))],
    nm = meta_data[[VAR_NAMES]]
  )
  unkown_dt <- vapply(r, is.null, FUN.VALUE = logical(1))
  if (any(unkown_dt)) {
    util_warning(
      c(
        "For the following variables, the %s is not one of %s: %s\n",
        "--> falling back to %s"
      ),
      sQuote(DATA_TYPE),
      util_pretty_vector_string(DATA_TYPES),
      prep_deparse_assignments(names(which(unkown_dt)),
        meta_data[[DATA_TYPE]][unkown_dt],
        mode = "string_codes"
      ),
      dQuote(DATA_TYPES$STRING),
      applicability_problem = TRUE
    )
    r[unkown_dt] <- lapply(r[unkown_dt], function(x) readr::col_character())
  }
  r
}

readr_coltypes <-
  list(
    DATA_TYPES$INTEGER, readr::col_double(),
    DATA_TYPES$STRING, readr::col_character(),
    DATA_TYPES$FLOAT, readr::col_double(),
    DATA_TYPES$DATETIME, readr::col_datetime(),
    DATA_TYPES$TIME, readr::col_time()
  )

readr_coltypes <-
  setNames(readr_coltypes[2 * seq_len(length(readr_coltypes) / 2)],
    nm = readr_coltypes[2 * seq_len(length(readr_coltypes) / 2) - 1]
  )

#' Internal helper: skip readr type convert cols
#'
#' @noRd
util_skip_readr_type_convert_cols <- function(study_data, meta_data) {
  util_expect_data_frame(meta_data, c(VAR_NAMES, DATA_TYPE))

  study_data_names <- names(study_data)
  simple_types <- c(DATA_TYPES$INTEGER, DATA_TYPES$FLOAT)
  unknown_or_unnamed <- is.na(study_data_names) |
    !nzchar(study_data_names) |
    !(study_data_names %in% meta_data[[VAR_NAMES]])

  expected <- rep(NA_character_, length(study_data))
  names(expected) <- study_data_names
  known_idx <- match(
    study_data_names[!unknown_or_unnamed],
    meta_data[[VAR_NAMES]]
  )
  expected[!unknown_or_unnamed] <-
    tolower(trimws(meta_data[[DATA_TYPE]][known_idx]))

  candidate <- !unknown_or_unnamed & expected %in% simple_types
  actual_matches <- rep(FALSE, length(study_data))
  if (any(candidate)) {
    needs_full_conversion <- vapply(
      study_data[candidate],
      function(x) {
        is.factor(x) ||
          is.logical(x) ||
          inherits(x, c(
            "POSIXt", "Date", "hms", "times", "ITime",
            "time_of_day"
          )) ||
          is.list(x)
      },
      FUN.VALUE = logical(1)
    )
    candidate_idx <- which(candidate)[!needs_full_conversion]
    if (length(candidate_idx) > 0) {
      actual_matches[candidate_idx] <- mapply(
        FUN = function(x, data_type) {
          switch(data_type,
            integer = is.integer(x) ||
              (is.numeric(x) && all(util_is_integer(x))),
            float = is.numeric(x),
            FALSE
          )
        },
        x = study_data[candidate_idx],
        data_type = expected[candidate_idx],
        SIMPLIFY = TRUE,
        USE.NAMES = FALSE
      )
    }
  }

  setNames(candidate & actual_matches, nm = study_data_names)
}

#' Internal helper: can skip readr type convert
#'
#' @noRd
util_can_skip_readr_type_convert <- function(study_data, meta_data) {
  all(util_skip_readr_type_convert_cols(study_data, meta_data))
}

#' Internal helper: readr cols for study data
#'
#' @noRd
util_readr_cols_for_study_data <- function(study_data_names, col_types) {
  known <- !is.na(study_data_names) &
    nzchar(study_data_names) &
    study_data_names %in% names(col_types)
  r <- vector("list", length(study_data_names))
  names(r) <- study_data_names
  r[known] <- col_types[study_data_names[known]]
  if (any(!known)) {
    r[!known] <- lapply(seq_len(sum(!known)), function(x) readr::col_guess())
  }
  r
}

#' Internal helper: adjust integer column
#'
#' @noRd
util_adjust_integer_column <- function(cl) {
  if (is.integer(cl)) {
    as.numeric(cl)
  } else {
    floor(cl)
  }
}

#' Internal helper: adjust float column
#'
#' @noRd
util_adjust_float_column <- function(cl) {
  if (is.integer(cl)) {
    as.numeric(cl)
  } else {
    cl
  }
}

#' Internal helper: type adjust warning counts
#'
#' @noRd
util_type_adjust_warning_counts <- function(counts = integer(0),
  vars = character(0),
  increments = rep.int(
    1L,
    length(vars)
  )) {
  if (!length(vars)) {
    return(counts)
  }
  if (length(vars) == 1L) {
    if (is.na(vars)) {
      return(counts)
    }
    increment <- as.integer(increments[[1L]])
    if (vars %in% names(counts)) {
      old_count <- counts[[vars]]
    } else {
      old_count <- 0L
    }
    counts[[vars]] <- old_count + increment
    return(counts)
  }

  new_counts <- tapply(as.integer(increments), vars, sum)
  old_counts <- counts[names(new_counts)]
  old_counts[is.na(old_counts)] <- 0L
  counts[names(new_counts)] <- old_counts + new_counts
  counts
}

#' Internal helper: adjust data type2
#'
#' @noRd
util_adjust_data_type2 <- function(study_data,
  meta_data,
  relevant_vars_for_warnings = character(0),
  language_code =
    getOption(
      "dataquieR.locale",
      dataquieR.locale_default
    )) {
  util_expect_data_frame(study_data, keep_types = TRUE)
  if (length(relevant_vars_for_warnings) == 0) {
    relevant_vars_for_warnings <- colnames(study_data)
  }
  if (isTRUE(util_attr(study_data, "Data_type_matches", exact = TRUE))) {
    return(study_data)
  }
  skip_readr_type_convert <-
    util_skip_readr_type_convert_cols(study_data, meta_data)
  meta_data_type <- tolower(trimws(meta_data[[DATA_TYPE]]))
  intg_vars <- meta_data[[VAR_NAMES]][meta_data_type == DATA_TYPES$INTEGER]
  flt_vars <- meta_data[[VAR_NAMES]][meta_data_type == DATA_TYPES$FLOAT]
  datim_vars <- meta_data[[VAR_NAMES]][meta_data_type == DATA_TYPES$DATETIME]
  if (all(skip_readr_type_convert)) {
    intg <- intersect(colnames(study_data), intg_vars)
    flt <- intersect(colnames(study_data), flt_vars)
    study_data[, intg] <-
      lapply(study_data[, intg, drop = FALSE], util_adjust_integer_column)
    study_data[, flt] <-
      lapply(study_data[, flt, drop = FALSE], util_adjust_float_column)
    attr(study_data, "Data_type_matches") <- TRUE
    return(study_data)
  }
  n_readr_type_convert <- sum(!skip_readr_type_convert)
  if (n_readr_type_convert > 1L &&
      .called_in_pipeline && rlang::is_integerish(dynGet("cores")) &&
      as.integer(dynGet("cores")) > 1) {
    mycl <- parallel::makePSOCKcluster(as.integer(dynGet("cores")))
    parallel::clusterCall(mycl, library, "dataquieR", character.only = TRUE)
    parallel::clusterCall(mycl, loadNamespace, "hms")
    withr::defer(parallel::stopCluster(mycl))
  } else {
    mycl <- NULL
  }
  to_warn <- integer(0)
  my_env <- environment()
  old_names <- names(study_data)
  no_name <- is.na(names(study_data)) | !nzchar(names(study_data))
  if (any(no_name)) {
    # find unused prefix
    prefix <- "#"
    while (any(startsWith(names(study_data), prefix), na.rm = TRUE)) {
      prefix <- paste0("#", prefix)
    }
    names(study_data)[no_name] <-
      paste0(prefix, seq_len(sum(no_name)))
  }
  e <- l10n_info()$codeset
  if (is.null(e))
    e <- "UTF-8" # no reliable way of detecting the current encoding
  locale <- readr::locale(
    date_names = language_code,
    #    date_format = , # keep the default
    #    time_format = , # keep the default
    decimal_mark = Sys.localeconv()[["decimal_point"]],
    grouping_mark = Sys.localeconv()[["thousands_sep"]],
    tz = Sys.timezone(),
    encoding = e,
    asciify = FALSE
  )
  locale$tz <- Sys.timezone()
  col_types <- util_readr_cols_from_metadata(meta_data) # also checks for VAR_NAMES and DATA_TYPE # nolint: line_length_linter.
  num <- intersect(colnames(study_data), c(flt_vars, intg_vars))
  intg <- intersect(colnames(study_data), intg_vars)
  flt <- intersect(colnames(study_data), flt_vars)

  datim <- intersect(colnames(study_data), datim_vars)
  study_data_to_convert <-
    study_data[, !skip_readr_type_convert, drop = FALSE]
  col_types_to_convert <-
    util_readr_cols_for_study_data(names(study_data_to_convert), col_types)
  for (cn in colnames(study_data_to_convert)) {
    if (!is.null(study_data_to_convert[[cn]])) {
      attr(study_data_to_convert[[cn]], "..cn") <-
        cn
    }
  }
  factor_cols <- vapply(study_data_to_convert, is.factor,
    FUN.VALUE = logical(1)
  )
  if (isTRUE(as.logical(getOption(
    "dataquieR.old_factor_handling",
    dataquieR.old_factor_handling_default
  )))) {
    # This is for backwards compatibility, but it may be more user friendly to
    # omit this in both adjust_data_type functions
    study_data_to_convert[, factor_cols] <-
      lapply(study_data_to_convert[, factor_cols, drop = FALSE], as.integer)
  } else {
    study_data_to_convert[, factor_cols] <-
      util_par_lapply_lb(
        cl = mycl,
        study_data_to_convert[, factor_cols, drop = FALSE],
        function(x) util_as_character(x)
      )
  }
  non_character_cols <- !vapply(study_data_to_convert, is.character,
    FUN.VALUE = logical(1)
  )
  if (any(non_character_cols)) {
    study_data_to_convert[, non_character_cols] <-
      util_par_lapply_lb(
        cl = mycl,
        study_data_to_convert[, non_character_cols, drop = FALSE],
        util_as_character
      )
  }
  if (length(intg) > 0) {
    intg_to_convert <- intersect(intg, names(study_data_to_convert))
    study_data_to_convert[, intg_to_convert] <- util_par_lapply_lb(
      cl = mycl,
      study_data_to_convert[, intg_to_convert, drop = FALSE],
      function(cl) {
        r <- cl
        r <- trimws(tolower(r))
        r[r %in% c("t", "true")] <- "1"
        r[r %in% c("f", "false")] <- "1"
        r
      }
    )
  }
  if (length(datim) > 0) {
    datim_to_convert <- intersect(datim, names(study_data_to_convert))
    study_data_to_convert[, datim_to_convert] <- util_par_lapply_lb(
      cl = mycl,
      study_data_to_convert[, datim_to_convert, drop = FALSE],
      function(cl) {
        r <- cl
        r <- # prepend 000 to one-digit years
          gsub(
            "^(\\d)\\-(\\d\\d)\\-(\\d\\d)(\\W.*|)$", "000\\1-\\2-\\3\\4",
            r
          )
        r <- # prepend 00 to two-digit years
          gsub(
            "^(\\d\\d)\\-(\\d\\d)\\-(\\d\\d)(\\W.*|)$", "00\\1-\\2-\\3\\4",
            r
          )
        r <- # prepend 0 to three-digit years
          gsub(
            "^(\\d\\d\\d)\\-(\\d\\d)\\-(\\d\\d)(\\W.*|)$", "0\\1-\\2-\\3\\4",
            r
          )
        r
      }
    )
  }

  if (!is.null(mycl) &&
    isTRUE(as.logical(getOption(
      "dataquieR.type_adjust_parallel",
      dataquieR.type_adjust_parallel_default
    )))) {
    processed_list <- util_par_lapply_lb(
      cl = mycl,
      X = seq_along(study_data_to_convert),
      fun = function(i, study_data, col_types, locale, relevant_vars_for_warnings) { # nolint: line_length_linter.
        e <- environment()

        col_name <- names(study_data)[i]
        col_vec <- study_data[[i]]

        df_one <- data.frame(col_vec, stringsAsFactors = FALSE)
        names(df_one) <- col_name

        if (!col_name %in% names(col_types)) {
          col_types[[col_name]] <- readr::col_guess()
        }
        converted_df <- withCallingHandlers(
          readr::type_convert(
            df_one,
            col_types     = col_types[col_name],
            trim_ws       = TRUE,
            guess_integer = FALSE,
            locale        = locale,
            na            = c("", "NA", "Inf", "+Inf", "-Inf", "NaN")
          ),
          warning = function(w) {
            # Historical warning dump to a temporary file removed here.
            # Inspect commit 3cc5b36426 before restoring local diagnostics.
            cm <- util_extract_named_groups(
              "^\\[(?<row>\\d+), +(?<col>\\d+)\\]:.*",
              conditionMessage(w)
            )
            if (nrow(cm) > 0) {
              invokeRestart("muffleWarning")
            }
          }
        )
        introduced_na <- is.na(converted_df[[col_name]]) &
          !is.na(col_vec) &
          !(trimws(col_vec) %in% c("", "NA", "Inf", "+Inf", "-Inf", "NaN"))
        local_warns <- integer(0)
        if (col_name %in% relevant_vars_for_warnings) {
          introduced_na_count <- sum(introduced_na)
          if (introduced_na_count > 0L) {
            local_warns <- stats::setNames(introduced_na_count, col_name)
          }
        }

        list(
          data  = converted_df[[col_name]],
          warns = local_warns
        )
      },
      study_data = study_data_to_convert,
      col_types = col_types_to_convert,
      locale = locale,
      relevant_vars_for_warnings = relevant_vars_for_warnings
    )

    data_list <- lapply(processed_list, `[[`, "data")
    converted_data <- as.data.frame(data_list, stringsAsFactors = FALSE)
    names(converted_data) <- names(study_data_to_convert)
    study_data0 <- study_data
    study_data0[, names(converted_data)] <- converted_data

    warn_lists <- lapply(processed_list, `[[`, "warns")
    to_warn <- unlist(warn_lists)
  } else {
    converted_data <- withCallingHandlers(readr::type_convert(
      # Historical TIME conversion warning reproducer removed here. Inspect
      # commit 3cc5b36426 before restoring the local dq_report2 recipe.
      study_data_to_convert,
      col_types = col_types_to_convert,
      trim_ws = TRUE,
      guess_integer = FALSE,
      locale = locale,
      na = c("", "NA", "Inf", "+Inf", "-Inf", "NaN")
    ), warning = function(c) {
      cm <- util_extract_named_groups(
        "^\\[(?<row>\\d+), +(?<col>\\d+)\\]:.*",
        conditionMessage(c)
      )
      if (nrow(cm) > 0) {
        vars <- colnames(study_data_to_convert)[as.integer(cm[, "col", drop = TRUE])] # nolint: line_length_linter.
        vars <- vars[vars %in% relevant_vars_for_warnings]
        my_env$to_warn <- util_type_adjust_warning_counts(
          counts = my_env$to_warn,
          vars = vars
        )
        invokeRestart("muffleWarning")
      }
    })
    study_data0 <- study_data
    study_data0[, names(converted_data)] <- converted_data
  }

  converted_num <- intersect(num, names(study_data_to_convert))
  if (length(converted_num) > 0) {
    study_data0[, converted_num] <- mapply(
      SIMPLIFY = FALSE,
      cl = study_data0[, converted_num, drop = FALSE],
      orig = study_data_to_convert[, converted_num, drop = FALSE],
      FUN = function(cl, orig) {
        if (any(is.na(cl))) {
          cl[is.na(cl) & orig %in% c("Inf", "+Inf", "-Inf", "NaN")] <-
            as.vector(as.numeric(
              orig[is.na(cl) & orig %in% c("Inf", "+Inf", "-Inf", "NaN")]
            ))
        }
        cl
      }
    )
  }
  study_data <- study_data0
  rm(study_data0)
  names(study_data) <- old_names
  dt_cols <- vapply(study_data, inherits, "POSIXt", FUN.VALUE = logical(1))
  study_data[, dt_cols] <- util_par_lapply_lb(
    cl = mycl, study_data[, dt_cols, drop = FALSE],
    function(cl) {
      cl <- as.POSIXct(round(as.POSIXct(cl), units = "secs"))
      cl <- lubridate::with_tz(lubridate::force_tz(cl))
    }
  )
  study_data[, intg] <- util_par_lapply_lb(
    cl = mycl,
    study_data[, intg, drop = FALSE],
    util_adjust_integer_column
  )
  study_data[, flt] <- util_par_lapply_lb(
    cl = mycl,
    study_data[, flt, drop = FALSE],
    util_adjust_float_column
  )
  if (length(to_warn) > 0) {
    invisible(mapply(function(rv, freq) {
      util_message(
        paste(
          "Data type transformation of", dQuote(rv), "introduced",
          freq,
          "additional missing values."
        ),
        applicability_problem = TRUE
      )
    }, names(to_warn), to_warn))
  }
  attr(study_data, "Data_type_matches") <- TRUE
  study_data
}

# Historical data-type conversion benchmark removed here. Inspect commit
# 5b7f976388 before restoring the local microbenchmark setup.
#
# Unit: seconds
# expr      min       lq     mean   median       uq       max neval
# util_adjust_data_type(s, meta_data = m) 3.910896 4.021739 4.163222 4.046629
# 4.070163 15.708896 100
# util_adjust_data_type2(s, meta_data = m) 2.797905 2.935524 3.025309 2.994379
# 3.035081 4.840821 100
# >
