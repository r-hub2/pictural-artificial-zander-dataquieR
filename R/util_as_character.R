# nolint start: line_length_linter.
#' like as.character, but S3-aware for time classes (`hms`, `difftime`, `chron::times`)
#'
#' @param x  object to convert
#' @param digits.secs integer, passed via options(digits.secs) to control fractional seconds
#' @param tz optional `tz` for `POSIXt` formatting (if `NULL`, use default printing)
#' @param ... ignored
#' @return character vector (length(x)), never recurses
#' @noRd
# nolint end
util_as_character <- function(
  x,
  digits.secs = getOption("digits.secs", 3L),
  tz = NULL,
  ...
) {
  withr::local_options(list(digits.secs = digits.secs))

  # Fast paths (vectorised) ----------------------------------------------------
  if (inherits(x, "hms")) {
    r <- .util_format_hms(x)
    r[trimws(r) == "NA"] <- NA_character_
    return(r)
  }
  if (inherits(x, "difftime")) {
    r <- .util_format_hms(hms::as_hms(x))
    r[trimws(r) == "NA"] <- NA_character_
    return(r)
  }
  if (inherits(x, "times")) {
    r <- .util_format_hms(hms::as_hms(as.numeric(x) * 86400))
    r[trimws(r) == "NA"] <- NA_character_
    return(r)
  }
  if (inherits(x, "POSIXt")) {
    if (is.null(tz)) {
      r <- format(x)
    } else {
      r <- format(x, tz = tz, usetz = TRUE)
    }
    r[trimws(r) == "NA"] <- NA_character_
    return(r)
  }
  if (is.factor(x)) {
    return(as.character(x))
  }
  if (is.character(x)) {
    return(x)
  }
  if (is.numeric(x)) {
    return(as.character(x))
  }
  if (is.logical(x)) {
    return(as.character(x))
  }
  if (length(x) == 0L) {
    return(character(0))
  }

  # List-handling (ohne Rekursion) --------------------------------------------
  if (is.list(x)) {
    # Check for inhomogeneous value formats (hms "vectors")
    ..is_hms <- vapply(x, inherits, "hms", FUN.VALUE = logical(1))
    ..is_na1 <- lapply(x, function(x) {
      r <- FALSE
      try(r <- length(x) == 1 && is.na(x), silent = TRUE)
      r
    })
    if (any(..is_hms) && !all(..is_hms)) {
      varname <- util_attr(x, "..cn", exact = TRUE)
      vn <- dQuote(varname)
      if (length(vn) == 0) {
        vn <- "data"
      }
      x[!..is_hms] <-
        lapply(x[!..is_hms], function(x) {
          if (length(x) != 1) {
            r <- hms::as_hms(NA_character_)
          } else {
            r <- try(hms::as_hms(x), silent = TRUE)
            if (util_is_try_error(r)) {
              r <- hms::as_hms(NA_character_)
            }
          }
          r
        })
      ..is_na2 <- lapply(x, function(x) {
        r <- FALSE
        try(r <- length(x) == 1 && is.na(x), silent = TRUE)
        r
      })
      lost <- sum(!vapply(
        mapply(..is_na1, ..is_na2,
          FUN = identical,
          SIMPLIFY = FALSE
        ),
        FUN = identity,
        FUN.VALUE = logical(1)
      ))
      if (!.called_in_pipeline2()) {
        if (lost > 0) {
          .cnd <- util_warning
        } else {
          .cnd <- util_message
        }
        .cnd(
          c(
            "Found %d non-hms %s values in %s -- %d of which",
            "could not be casted to hms"
          ),
          sum(!..is_hms),
          sQuote(DATA_TYPES$TIME),
          vn,
          lost,
          integrity_indicator = "int_vfe_inhom",
          varname = varname
        )
      } else {
        .cnd <- util_message
        .cnd(
          c("Found %d non-hms %s values in %s"),
          sum(!..is_hms),
          sQuote(DATA_TYPES$TIME),
          vn,
          integrity_indicator = "int_vfe_inhom",
          varname = varname
        )
      }
    }

    # Format scalar time-like list entries without recursing into this helper.
    # Historical list-casting prototype removed here. Inspect with
    # `git show 471e4b0e1a -- R/util_as_character.R` before restoring.
    out <- lapply(
      x,
      function(e) {
        if (is.null(e) || length(e) == 0L) {
          return(NA_character_)
        }
        if (inherits(e, "hms")) {
          r <- .util_format_hms(e)
          r[trimws(r) == "NA"] <- NA_character_
          return(r)
        }
        if (inherits(e, "difftime")) {
          r <- .util_format_hms(hms::as_hms(e))
          r[trimws(r) == "NA"] <- NA_character_
          return(r)
        }
        if (inherits(e, "times")) {
          r <- .util_format_hms(hms::as_hms(as.numeric(e) * 86400))
          r[trimws(r) == "NA"] <- NA_character_
          return(r)
        }
        if (inherits(e, "POSIXt")) {
          if (is.null(tz)) r <- format(e)
          r <- format(e, tz = tz, usetz = TRUE)
          r[trimws(r) == "NA"] <- NA_character_
          return(r)
        }
        if (is.factor(e)) {
          r <- as.character(e)
          r[trimws(r) == "NA"] <- NA_character_
          return(r)
        }
        if (is.character(e)) {
          return(e)
        }
        if (is.numeric(e)) {
          r <- as.character(e)
          r[trimws(r) == "NA"] <- NA_character_
          return(r)
        }
        if (is.list(e)) {
          return(NA_character_) # Schutz vor verschachtelten Listen → keine Rekursion! # nolint: line_length_linter.
        }
        r <- as.character(e)
        r[trimws(r) == "NA"] <- NA_character_
        return(r)
      }
    )
    out[vapply(out, length, FUN.VALUE = integer(1)) != 1] <-
      NA_character_
    out <- vapply(
      out,
      FUN = identity,
      FUN.VALUE = character(1),
      USE.NAMES = FALSE
    )
    return(out)
  }

  # Fallback -------------------------------------------------------------------
  as.character(x)
}

#' Internal helper: util format hms
#'
#' @noRd
.util_format_hms <- function(...) {
  r <- ..util_format_hms(...)
  if (identical(r, "hms()")) {
    r <- character(0)
  }
  r
}

..util_format_hms <- utils::getS3method("format", "hms",
  envir = asNamespace("hms")
)
