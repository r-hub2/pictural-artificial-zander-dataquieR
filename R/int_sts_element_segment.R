# nolint start: line_length_linter.
#' Checks for element set
#'
#' Depends on `dataquieR.ELEMENT_MISSMATCH_CHECKTYPE` option,
#'
#' [Indicator]
#'
#' @inheritParams .template_function_indicator
#'
#' @return a [list] with
#'   - `SegmentData`: data frame with the unexpected elements check results.
#'                       - `Segment`: name of the corresponding segment,
#'                                    if applicable, `ALL` otherwise
#'   - `SegmentTable`: data frame with the unexpected elements check results, used for the data quality report.
#'                       - `Segment`: name of the corresponding segment,
#'                                    if applicable, `ALL` otherwise
#'
#' @export
#'
#' @examples
#' \dontrun{
#' study_data <- cars
#' meta_data <- dataquieR::prep_create_meta(
#'   VAR_NAMES = c("speedx", "distx"),
#'   DATA_TYPE = c("integer", "integer"), MISSING_LIST = "|", JUMP_LIST = "|",
#'   STUDY_SEGMENT = c("Intro", "Ex")
#' )
#' options(dataquieR.ELEMENT_MISSMATCH_CHECKTYPE = "none")
#' int_sts_element_segment(study_data, meta_data)
#' options(dataquieR.ELEMENT_MISSMATCH_CHECKTYPE = "exact")
#' int_sts_element_segment(study_data, meta_data)
#' study_data <- cars
#' meta_data <- dataquieR::prep_create_meta(
#'   VAR_NAMES = c("speedx", "distx"),
#'   DATA_TYPE = c("integer", "integer"), MISSING_LIST = "|", JUMP_LIST = "|",
#'   STUDY_SEGMENT = c("Intro", "Intro")
#' )
#' options(dataquieR.ELEMENT_MISSMATCH_CHECKTYPE = "none")
#' int_sts_element_segment(study_data, meta_data)
#' options(dataquieR.ELEMENT_MISSMATCH_CHECKTYPE = "exact")
#' int_sts_element_segment(study_data, meta_data)
#' study_data <- cars
#' meta_data <- dataquieR::prep_create_meta(
#'   VAR_NAMES = c("speed", "distx"),
#'   DATA_TYPE = c("integer", "integer"), MISSING_LIST = "|", JUMP_LIST = "|",
#'   STUDY_SEGMENT = c("Intro", "Intro")
#' )
#' options(dataquieR.ELEMENT_MISSMATCH_CHECKTYPE = "none")
#' int_sts_element_segment(study_data, meta_data)
#' options(dataquieR.ELEMENT_MISSMATCH_CHECKTYPE = "exact")
#' int_sts_element_segment(study_data, meta_data)
#' }
# nolint end
int_sts_element_segment <- function(study_data,
  item_level = "item_level",
  label_col,
  meta_data = item_level,
  meta_data_v2) {
  util_maybe_load_meta_data_v2()
  # prevent error that we have item_level and meta_data
  # if prep_prepare_dataframes was not called but both objects
  # are not missing any more, when prep_prepare_dataframes is called below.
  invisible(prep_prepare_dataframes(
    .label_col = VAR_NAMES,
    .allow_empty = TRUE
  ))
  meta_data <- util_prepare_item_level_metadata(
    meta_data = meta_data,
    label_col = VAR_NAMES
  )
  util_expect_data_frame(meta_data, c(VAR_NAMES))
  if (!STUDY_SEGMENT %in% colnames(meta_data)) {
    meta_data[[STUDY_SEGMENT]] <- "<NO SEGMENT>"
  }
  if (any(meta_data[[STUDY_SEGMENT]] == "ALL")) {
    util_message("No segment should be named %s, renaming it.",
      dQuote("ALL"),
      applicability_problem = TRUE
    )
    meta_data[[STUDY_SEGMENT]][meta_data[[STUDY_SEGMENT]] == "ALL"] <-
      "RENAMED SEGMENT: ALL"
  }
  Q <- substr(dQuote(""), 1, 1)
  E <- substr(dQuote(""), 2, 2)
  e <- new.env(parent = emptyenv())
  e$conds <- list()
  register_cond <- function(cond) {
    if (identical(
      util_attr(cond, "integrity_indicator", exact = TRUE), "int_sts_element"
    )) {
      e$conds <- c(e$conds, list(cond))
    }
  }

  util_purge_study_data_cache()

  suppressMessages(
    suppressWarnings(
      withCallingHandlers(
        without_pipeline(
          prep_prepare_dataframes(
            .study_data = study_data,
            .meta_data = meta_data,
            .label_col = VAR_NAMES, .allow_empty = TRUE,
            .internal = TRUE
          )
        ),
        message = register_cond,
        warning = register_cond
      )
    )
  )

  msgs <- vapply(e$conds, inherits, "message", FUN.VALUE = logical(1))
  wrns <- vapply(e$conds, inherits, "warning", FUN.VALUE = logical(1))

  pct <- data.frame(
    stringsAsFactors = FALSE,
    Segment = rep("ALL", sum(wrns)),
    DESCRIPTION = vapply(e$conds[wrns],
      conditionMessage,
      FUN.VALUE = character(1)
    ),
    MISSING = ifelse(grepl("of the study data", vapply(e$conds[wrns],
          conditionMessage,
          FUN.VALUE = character(1)
        ), fixed = TRUE),
      "metadata",
      "study data"
    ),
    PCT_int_sts_element = round(as.numeric(gsub(
      "^.*?([0-9,.]+)%.*$", "\\1",
      vapply(e$conds[wrns],
        conditionMessage,
        FUN.VALUE = character(1)
      )
    )), 2)
  )

  abs <- data.frame(
    stringsAsFactors = FALSE,
    Segment = rep("ALL", sum(msgs)),
    DESCRIPTION = vapply(e$conds[msgs],
      conditionMessage,
      FUN.VALUE = character(1)
    ),
    MISSING = ifelse(grepl("Did not find any metadata", vapply(e$conds[msgs],
          conditionMessage,
          FUN.VALUE = character(1)
        ), fixed = TRUE),
      "metadata",
      "study data"
    ),
    NUM_int_sts_element = vapply(gsub(sprintf("[^%s]", Q), "", vapply(e$conds[msgs], # nolint: line_length_linter.
          conditionMessage,
          FUN.VALUE = character(1)
        )), nchar, FUN.VALUE = integer(1))
  )

  r <- merge(pct, abs, by = c("Segment", "MISSING"), suffixes = c("p", "a"))

  r$DESCRIPTION <- paste(r$DESCRIPTIONp, r$DESCRIPTIONa)
  r$DESCRIPTIONa <- NULL
  r$DESCRIPTIONp <- NULL

  vars <- util_extract_matches(
    r$DESCRIPTION,
    paste0(Q, ".+?", E)
  )

  vars <- lapply(
    vars,
    gsub,
    pattern = paste0("^", Q, collapse = ""),
    replacement = ""
  )

  vars <- lapply(
    vars,
    gsub,
    pattern = paste0(E, "$", collapse = ""),
    replacement = ""
  )

  rr <- lapply(seq_len(nrow(r)), function(x) r[x, , drop = FALSE])

  segsizes <- util_table_of_vct(
    meta_data[[STUDY_SEGMENT]]
  )
  segsizes <- setNames(segsizes$Freq, nm = segsizes$Var1)
  rrr <- mapply(rr, vars, SIMPLIFY = FALSE, FUN = function(a, v) {
    x <- merge(a, v, by = NULL)
    x$Segment <- prep_map_labels(
      x = v,
      meta_data = meta_data,
      to = STUDY_SEGMENT,
      ifnotfound = NA_character_
    )

    x$Segment <- ifelse(is.na(x$Segment), "ALL", x$Segment)
    x$resp_vars <- x$y
    x$y <- NULL

    format_resp_vars <- function(resp_vars) {
      if (a$MISSING == "study data") {
        resp_vars <- prep_get_labels(
          resp_vars = resp_vars,
          item_level = meta_data,
          label_col = LABEL,
          label_class = "SHORT",
          resp_vars_are_var_names_only = TRUE
        )
      }
      resp_vars <- sort(resp_vars)
      if (length(names(resp_vars)) == 0) {
        return(prep_deparse_assignments(
          mode = "string_codes",
          resp_vars
        ))
      }
      prep_deparse_assignments(
        mode = "string_codes",
        names(resp_vars),
        resp_vars
      )
    }

    if (all(x$Segment != "ALL")) {
      xl <- split(x, x$Segment)
      xl <- lapply(xl, function(x) {
        x$NUM_int_sts_element <- nrow(x)
        x$PCT_int_sts_element <- nrow(x) / segsizes[unique(x$Segment)]
        x$resp_vars <- format_resp_vars(x$resp_vars)
        x[!duplicated(x), , drop = FALSE]
      })
      x <- do.call(rbind.data.frame, c(xl, list(
        make.row.names = FALSE,
        stringsAsFactors = FALSE
      )))
    } else {
      if (any(x$Segment != "ALL")) {
        util_error(c(
          "Unexpected error for Segments: found a mixture of",
          "segments. Internal error in dataquieR, please report."
        ))
      }
      x <- data.frame(
        stringsAsFactors = FALSE,
        Segment = "ALL",
        MISSING = paste(unique(x$MISSING), collapse = ", "),
        DESCRIPTION = paste(unique(x$DESCRIPTION),
          collapse = ", "
        ),
        NUM_int_sts_element = nrow(x),
        PCT_int_sts_element = max(x$PCT_int_sts_element),
        resp_vars = format_resp_vars(x$resp_vars)
      )
    }
    x
  })

  SegmentTable <- do.call(
    rbind.data.frame,
    c(rrr, list(
      make.row.names = FALSE,
      stringsAsFactors = FALSE
    ))
  )

  SegmentTable$DESCRIPTION <- NULL

  # Historical segment extraction from segsizes removed here.

  segments <- setdiff(meta_data[[STUDY_SEGMENT]], SegmentTable$Segment)
  segments <- setdiff(segments, ".COMPUTED__ssi")

  # add all segments that did not have a problem ----
  complement <- data.frame(
    Segment = segments,
    MISSING = rep(NA_character_, length(segments)),
    PCT_int_sts_element = rep(0, length(segments)),
    NUM_int_sts_element = rep(0, length(segments)),
    resp_vars = rep(NA_character_, length(segments))
  )

  SegmentTable <- SegmentTable[SegmentTable$Segment != ".COMPUTED__ssi", ,
    drop = FALSE]
  SegmentTable <- rbind(complement, SegmentTable)
  attr(SegmentTable$Segment, DATA_TYPE) <- DATA_TYPES$STRING
  attr(SegmentTable$MISSING, DATA_TYPE) <- DATA_TYPES$STRING
  attr(SegmentTable$PCT_int_sts_element, DATA_TYPE) <- DATA_TYPES$FLOAT
  attr(SegmentTable$NUM_int_sts_element, DATA_TYPE) <- DATA_TYPES$INTEGER
  attr(SegmentTable$resp_vars, DATA_TYPE) <- DATA_TYPES$STRING

  SegmentData <- SegmentTable
  names(SegmentData) <- c(
    "Segment",
    "Missing",
    "Percentage of unexpected elements",
    "Number of unexpected elements",
    "Response variables"
  )
  attr(SegmentData$Segment, DATA_TYPE) <- DATA_TYPES$STRING
  attr(SegmentData$Missing, DATA_TYPE) <- DATA_TYPES$STRING
  attr(SegmentData$`Percentage of unexpected elements`, DATA_TYPE) <-
    DATA_TYPES$FLOAT
  attr(SegmentData$`Number of unexpected elements`, DATA_TYPE) <-
    DATA_TYPES$INTEGER
  attr(SegmentData$`Response variables`, DATA_TYPE) <- DATA_TYPES$STRING

  list(
    SegmentData = SegmentData,
    SegmentTable = SegmentTable
  )
}
