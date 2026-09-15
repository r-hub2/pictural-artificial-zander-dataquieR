.util_condition_once_seen <- new.env(parent = emptyenv())

#' Internal helper: condition once seen
#'
#' @noRd
util_condition_once_seen <- function(id) {
  exists(id, envir = .util_condition_once_seen, inherits = FALSE)
}

#' Internal helper: condition once mark seen
#'
#' @noRd
util_condition_once_mark_seen <- function(id) {
  assign(id, TRUE, envir = .util_condition_once_seen)
  invisible(TRUE)
}

#' Internal helper: clean condition once cache
#'
#' @noRd
util_clean_condition_once_cache <- function() {
  rm(
    list = ls(.util_condition_once_seen),
    envir = .util_condition_once_seen
  )
  invisible(NULL)
}

#' Produce a condition function
#'
#' @param .condition_type [character] the type of the conditions being created
#'   and signaled by the function, "error", "warning", or "message"
#'
#' @family condition_functions
#' @concept process
#' @noRd
util_condition_constructor_factory <- function(.condition_type = c("error", "warning", "message")) { # nolint: line_length_linter.
  .condition_type <- match.arg(.condition_type)

  .signal_fkt <- switch(.condition_type,
    error = stop,
    warning = warning,
    message = message
  )

  .cond_constructor <-
    switch(.condition_type,
      error = rlang::error_cnd,
      warning = rlang::warning_cnd,
      message = rlang::message_cnd
    )

  .caller_control_att <- paste0(
    "dataquieR.",
    toupper(.condition_type),
    "S_WITH_CALLER"
  )
  .caller_control_att_default <- get(paste0(.caller_control_att, "_default"))

  function(m, ..., applicability_problem = NA,
    intrinsic_applicability_problem = NA,
    integrity_indicator = "none", level = 0, immediate, title = "",
    additional_classes = c(), varname = NULL, once_id) {
    invis <- FALSE
    if (identical(Sys.getenv("TESTTHAT"), "true")) {
      if (!isTRUE(getOption("dataquieR.testthat_expect_message_active", NULL))) { # nolint: line_length_linter.
        invis <- TRUE
      }
    }
    if (identical(
      getOption("dataquieR.debug", dataquieR.debug_default),
      TRUE
    )) {
      browser() # intended use of browser() -- dont modify this line
    }
    if (missing(immediate)) {
      immediate <- FALSE
    }
    m_args <- eval(quote(force(list(...))))
    if (!missing(once_id)) {
      util_expect_scalar(once_id, check_type = is.character)
    }
    # shows some false positive note on possible misplaced
    # ...
    # m_args <- lapply(
    #   rlang::call_args(rlang::call_match(dots_expand = FALSE))[["..."]], eval,
    #   envir = parent.frame())
    util_expect_scalar(integrity_indicator,
      allow_na = TRUE,
      check_type = is.character
    )
    if (!(integrity_indicator %in% c(na.omit(subset(util_get_concept_info("dqi"), # nolint: line_length_linter.
              get("Dimension") == "Integrity",
              select = "abbreviation",
              drop = TRUE
            )), "none"))) {
      util_error(
        "Internal error: %s is not a supported %s. Did you update %s?",
        dQuote(integrity_indicator),
        sQuote("integrity_indicator"),
        sQuote("dqi.rds")
      )
    }
    if (integrity_indicator == "none") {
      integrity_indicator <- NA_character_
    }
    util_stop_if_not(length(applicability_problem) == 1 &&
        is.logical(applicability_problem))
    util_stop_if_not(length(intrinsic_applicability_problem) == 1 &&
        is.logical(intrinsic_applicability_problem))

    caller. <- if (identical(as.logical(getOption(
      .caller_control_att,
      .caller_control_att_default
    )), FALSE)) {
      NULL
    } else {
      sys.call(1)
    }
    calling <- character(0)
    stacktrace <- if (identical(as.logical(getOption(
      "dataquieR.CONDITIONS_WITH_STACKTRACE",
      dataquieR.CONDITIONS_WITH_STACKTRACE_default
    )), FALSE)) {
      ""
    } else {
      character(0)
    }
    if (identical(stacktrace, "")) {
      if ((exists(".called_in_pipeline") && .called_in_pipeline) ||
          .condition_type != "error") {
        calling <- character(0)
      }
    }
    if (inherits(m, "try-error")) {
      m <- util_attr(m, "condition", exact = TRUE)
    }
    if (inherits(m, "condition")) {
      .m <- m
      m <- paste0(title, conditionMessage(m))
      if (m == "") {
        if (inherits(.m, "error")) {
          m <- "Error"
        } else if (inherits(.m, "warning")) {
          m <- "Warning"
        } else if (inherits(.m, "message")) {
          m <- "Message"
        } else {
          m <- "Condition -- should not be displayed, sorry. Please report."
        }
      }

      if (isTRUE(getOption(
        "dataquieR.traceback",
        dataquieR.traceback_default
      ))) {
        tc <- rlang::trace_back(bottom = 2)
      } else {
        tc <- NULL
      }
      ec <-
        .cond_constructor(
          message = paste(c(m, calling, stacktrace),
            collapse = "\n"
          ),
          trace = tc,
          use_cli_format = (!exists(".called_in_pipeline") ||
              !.called_in_pipeline),
          call = caller.
        )
    } else {
      mm <- paste0(m,
        collapse =
          " "
      )
      if (nchar(mm) > 8192) {
        mm <- substr(mm, 1, 8192)
        mm <- sub("(%[^%]$)", "\\1", mm, perl = TRUE)
      }
      if (isTRUE(getOption(
        "dataquieR.traceback",
        dataquieR.traceback_default
      ))) {
        tc <- rlang::trace_back(bottom = 2)
      } else {
        tc <- NULL
      }
      formatted_message <- if (length(m_args)) {
        do.call("sprintf", c(
          list(fmt = paste0(title, mm)),
          m_args
        ))
      } else {
        paste0(title, mm)
      }
      ec <-
        .cond_constructor(
          trace = tc,
          use_cli_format = (!exists(".called_in_pipeline") ||
              !.called_in_pipeline),
          message = paste0(c(
            formatted_message,
            calling,
            stacktrace
          ), collapse = "\n"),
          call = caller.
        )
    }
    attr(ec, "applicability_problem") <- applicability_problem
    attr(ec, "intrinsic_applicability_problem") <- intrinsic_applicability_problem # nolint: line_length_linter.
    attr(ec, "integrity_indicator") <- integrity_indicator
    attr(ec, "varname") <- varname
    dq_err_classes <- character(0)
    if (isTRUE(intrinsic_applicability_problem)) {
      dq_err_classes <- unique(c(
        dq_err_classes,
        dataquieR.intrinsic_applicability_problem
      ))
    }
    if (isTRUE(applicability_problem)) {
      dq_err_classes <- unique(c(
        dq_err_classes,
        dataquieR.applicability_problem
      ))
    }
    class(ec) <- unique(c(additional_classes, dq_err_classes, class(ec)))
    if (!isTRUE(.dq2_globs$.called_in_pipeline) && !missing(once_id)) {
      if (util_condition_once_seen(once_id)) {
        return(invisible(ec))
      }
      util_condition_once_mark_seen(once_id)
    }
    if (level >= getOption(
      "dataquieR.CONDITIONS_LEVEL_TRHESHOLD",
      dataquieR.CONDITIONS_LEVEL_TRHESHOLD_default
    ) ||
      inherits(ec, "error")) {
      # Historical direct signal-function call removed here.
      if (immediate && inherits(ec, "warning")) {
        cat("In",
          as.character(conditionCall(ec)),
          ":\n",
          conditionMessage(ec),
          "\n",
          file = stderr()
        ) # rlang currently only calls warning
      }
      if (inherits(ec, "error")) {
        rlang::cnd_signal(ec)
      } else {
        if (inherits(ec, "warning")) {
          rlang::cnd_signal(ec)
        } else {
          x <- capture.output(rlang::cnd_signal(ec), file = NULL, type = "message") # nolint: line_length_linter.
          if (!invis && length(x)) {
            cat(sep = "\n", x, file = stderr())
            if (!endsWith(x[length(x)], "\n")) {
              cat("\n", file = stderr())
            }
          }
        }
      }
    }
    invisible(ec)
  }
}
