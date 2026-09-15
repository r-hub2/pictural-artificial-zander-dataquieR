#' Guess the data type of a vector
#'
#' @param x a vector with characters
#' @param k [numeric] sample size, if less than `floor(length(x) / (it/20)))`,
#'                    minimum sample size is 1.
#' @param it [integer] number of iterations when taking samples
#'
#' @details
#' # Algorithm
#' This function takes `x` and tries to guess the data type of random subsets of
#' this vector using [readr::guess_parser()]. The RNG is initialized with a
#' constant, so the function stays deterministic. It does such sub-sample based
#' checks `it` times, the majority of the detected datatype determines the
#' guessed data type.
#'
#' @return a guess of the data type of `x`. An attribute `orig_type` is also
#' attached to give the more detailed guess returned by [readr::guess_parser()].
#' @export
#' @seealso [prep_datatype_from_data()]
prep_robust_guess_data_type <- function(x, k = 50, it = 200) {
  withr::with_seed(4242, {
    util_expect_scalar(x,
      allow_more_than_one = TRUE,
      allow_na = TRUE,
      check_type = is.character,
      error_message = "Need a character vector in argument x"
    )
    if (all(is.na(x))) {
      util_error("argument x cannot be NA only")
    }
    x <- trimws(x)
    x <- x[!is.na(x) & x != ""]
    if (length(x) == 0) {
      return(DATA_TYPES$INTEGER)
    }
    smpl_size <- max(min(k, floor(length(x) / (it / 20))), 1)
    gp <- vapply(seq_len(it), function(i) {
      sub_x <- sample(x,
        size =
          smpl_size, replace = FALSE
      )
      readr::guess_parser(sub_x, guess_integer = TRUE)
    }, FUN.VALUE = character(1))
    parsers <- as.matrix(table(gp))[, 1, drop = TRUE]
    util_attach_attr(
      unname(c(
        date = DATA_TYPES$DATETIME,
        time = DATA_TYPES$TIME,
        datetime = DATA_TYPES$DATETIME,
        integer = DATA_TYPES$INTEGER,
        double = DATA_TYPES$FLOAT,
        character = DATA_TYPES$STRING,
        logical = DATA_TYPES$INTEGER,
        number = DATA_TYPES$FLOAT
      )[names(which.max(parsers))]),
      orig_type = names(which.max(parsers))
    )
  })
}

#' Guess encoding of text or text files
#'
#' @param x [character] string to guess encoding for
#' @param file [character] file to guess encoding for
#'
#' @return encoding
#' @export
prep_guess_encoding <- function(x, file) {
  if ((missing(file) && missing(x)) ||
      (!missing(file) && !missing(x))) {
    util_error("Can neither have both nor none of the arguments x and file")
  }
  if (!missing(file)) {
    if (!util_stringi_available()) {
      return(util_guess_encoding_without_stringi(file = file))
    }
    readr::guess_encoding(file, n_max = -1, threshold = 0)
  } else {
    util_expect_scalar(x,
      allow_more_than_one = TRUE, check_type =
        is.character, convert_if_possible = as.character,
      allow_na = TRUE
    )
    x <- x[validEnc(x)]
    x <- x[!is.na(x)]
    if (!util_stringi_available()) {
      return(util_guess_encoding_without_stringi(x = x))
    }
    guesses <- table(unlist(lapply(x, Encoding)))
    guess1 <- names(which.max(guesses))
    if (length(x) > 0 && identical(guess1, "unknown") &&
        util_all_ascii_strings(x)) {
      return(data.frame(encoding = "ASCII", confidence = 1))
    }
    if (length(x) > 0 && is.null(guess1) || identical(guess1, "unknown")) {
      xx <- lapply(x, charToRaw)
      guess1 <- readr::guess_encoding(xx,
        n_max = -1,
        threshold = 0
      )
    } else {
      if (length(guesses) == 0) {
        guesses <- data.frame(
          encoding = "unknown",
          confidence = 1
        )
      }
      guess1 <- setNames(data.frame(guesses), nm = c("encoding", "confidence"))
      guess1$confidence <- guess1$confidence / sum(guess1$confidence,
        na.rm = TRUE
      )
    }
    guess1$encoding <- as.character(guess1$encoding)
    guess1$confidence <- as.numeric(guess1$confidence)
    return(guess1)
    # Historical collapsed-string readr encoding fallback removed here.
  }
}

#' Internal helper: stringi available
#'
#' @noRd
util_stringi_available <- function() {
  requireNamespace("stringi", quietly = TRUE)
}

#' Internal helper: unknown encoding guess
#'
#' @noRd
util_unknown_encoding_guess <- function() {
  data.frame(encoding = "unknown", confidence = 1)
}

#' Internal helper: all ascii strings
#'
#' @noRd
util_all_ascii_strings <- function(x) {
  if (length(x) < 1) {
    return(FALSE)
  }
  all(vapply(x, function(xx) {
    all(as.integer(charToRaw(xx)) < 128)
  }, FUN.VALUE = logical(1)))
}

#' Internal helper: guess encoding without stringi
#'
#' @noRd
util_guess_encoding_without_stringi <- function(x, file) {
  if (!missing(file)) {
    if (!is.character(file) || length(file) != 1 || !file.exists(file)) {
      return(util_unknown_encoding_guess())
    }
    bytes <- readBin(file, what = "raw", n = file.info(file)$size)
    if (length(bytes) > 0 && all(as.integer(bytes) < 128)) {
      return(data.frame(encoding = "ASCII", confidence = 1))
    }
    return(util_unknown_encoding_guess())
  }
  if (length(x) > 0 && util_all_ascii_strings(x)) {
    return(data.frame(encoding = "ASCII", confidence = 1))
  }
  util_unknown_encoding_guess()
}

#' Verify encoding
#' @param dt0 [data.frame] data to verify
#' @param ref_encs [character] names are column names of `dt0`, values their
#'                             expected encoding, can be missing.
#'
#' @noRd
#' @examples
#' \dontrun{
#' dt0 <-
#'   prep_get_data_frame(
#'     file.path(
#'       "~",
#'       "rsync", "nako_mrt_qs$", "exporte", "NAKO_Datensatz_bereinigte_Daten",
#'       "NatCoEdc_Export", "export_mannheim_30.csv"
#'     )
#'   )
#' util_verify_encoding(dt0)
#' dt0$mrt_note[[1]] <- iconv("Härbärt", "UTF-8", "cp1252")
#' util_verify_encoding(dt0)
#' dt0$mrt_note[[15]] <- iconv("Härbärt", "UTF-8", "cp1252")
#' util_verify_encoding(dt0)
#' dt0$mrt_note[[1]] <- "Härbärt"
#' util_verify_encoding(dt0)
#' dt0$mrt_note[[17]] <- iconv("Härbärt", "UTF-8", "latin3")
#' util_verify_encoding(dt0)
#' }
util_verify_encoding <- function(dt0, ref_encs) {
  find_cols_with_clear_encoding <- function(x) {
    gs <- prep_guess_encoding(x)
    util_attach_attr((all(validEnc(x))) &&
        any(c("UTF-8", "ASCII") %in% gs$encoding) &&
        !any(gs$confidence < 1), enc = gs)
  }
  x_matches_ref_enc <- function(x, ref_enc) {
    if (ref_enc == "unknown") {
      return(TRUE)
    }
    y <- iconv(x, ref_enc, "UTF-8", sub = "")
    r <- (is.na(x) & is.na(y)) |
      (!is.na(x) & !is.na(y) & (x == y))
    r
  }
  cols_enc <- lapply(dt0, find_cols_with_clear_encoding)
  cols_with_unclear_encoding <- setNames(nm = names(which(
    !vapply(
      cols_enc,
      identity,
      FUN.VALUE = logical(1)
    ),
    useNames = TRUE
  )))
  not_missing_refencs <- !missing(ref_encs)
  res <- lapply(
    setNames(nm = cols_with_unclear_encoding),
    function(col_unclear) {
      if (not_missing_refencs) {
        ref_enc <- ref_encs[[col_unclear]]
      } else {
        ref_enc <-
          prep_guess_encoding(dt0[[col_unclear]])$encoding[[1]]
      }
      res <- which(!vapply(dt0[[col_unclear]],
          x_matches_ref_enc,
          ref_enc = ref_enc,
          FUN.VALUE = logical(1)
        ))
      act_enc <- lapply(setNames(dt0[res, col_unclear, drop = TRUE],
          nm = res
        ), prep_guess_encoding)
      util_attach_attr(res,
        ref_enc = ref_enc,
        act_enc = act_enc
      )
    }
  )
  res[vapply(res, length, FUN.VALUE = integer(1)) == 0] <- NULL
  util_attach_attr(res, cols_enc = lapply(cols_enc, util_attr, "enc"))
}
