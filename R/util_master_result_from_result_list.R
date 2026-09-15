#' Internal helper: master result from result list
#'
#' @noRd
util_master_result_from_result_list <- function(x, warn_errors = FALSE,
  title_mode = c("function", "ssi")) {
  title_mode <- util_match_arg(title_mode)
  if (!is.list(x) || length(x) == 0) {
    return(NULL)
  }
  if (!identical(
    try(all(
      vapply(x, function(y) {
        inherits(y, "dataquieR_result") &&
          inherits(y, "master_result")
      }, FUN.VALUE = logical(1)),
      na.rm = TRUE
    ), silent = TRUE),
    TRUE
  )) {
    return(NULL)
  }

  original_x <- x
  error_flags <- vapply(x, FUN.VALUE = logical(1), FUN = function(y) {
    length(util_attr(y, "error", exact = TRUE)) > 0
  })
  if (warn_errors && any(error_flags)) {
    for (e in x[error_flags]) {
      for (cnd in util_attr(e, "error", exact = TRUE)) {
        util_warning(cnd)
      }
    }
  }
  x <- x[!error_flags]
  if (length(x) == 0) {
    return(util_sectioned_master_result_from_result_list(original_x,
        title_mode = title_mode
      ))
  }

  function_names <- unique(unlist(lapply(
    x, util_attr,
    "function_name",
    exact = TRUE
  )))
  if (length(function_names) != 1) {
    return(util_sectioned_master_result_from_result_list(x,
        title_mode = title_mode
      ))
  }

  all_slots <- unique(unlist(lapply(x, names)))
  all_slots <- all_slots[vapply(all_slots,
    function(slot) {
      all(
        vapply(x, function(dqr) {
          slot %in%
            names(dqr)
        },
        FUN.VALUE = logical(1)
        ),
        na.rm = TRUE
      )
    },
    FUN.VALUE = logical(1)
  )]
  if (length(all_slots) == 0) {
    return(util_sectioned_master_result_from_result_list(x,
        title_mode = title_mode
      ))
  }

  names(x) <- paste0(rep("r.", length(x)), seq_along(x))
  nm1 <- unlist(lapply(x, util_attr, "function_name", exact = TRUE))
  calls <- lapply(x, util_attr, "call", exact = TRUE)
  nm2 <- unlist(lapply(calls, function(call_attr) {
    entity_name <- util_attr(call_attr, "entity_name", exact = TRUE)
    if (length(entity_name) == 1 && !is.na(entity_name)) {
      return(entity_name)
    }
    resp_vars <- try(eval(call_attr$resp_vars, parent.frame()), silent = TRUE)
    if (!util_is_try_error(resp_vars) &&
        length(resp_vars) == 1 &&
        !is.na(resp_vars)) {
      return(resp_vars)
    }
    NULL
  }))
  if (!!length(nm1) &&
      length(nm1) == length(nm2)) {
    names(x) <- paste0(nm1, ".", nm2)
  }

  mini_function_name <- unique(nm1)
  if (length(mini_function_name) != 1) {
    mini_function_name <- NULL
  }
  mini_cn <- unique(util_sub_string_left_from_.(names(x)))
  if (length(mini_cn) != 1) {
    mini_cn <- mini_function_name
  }
  mini_call <- Filter(Negate(is.null), calls)
  if (length(mini_call) > 0) {
    mini_call <- mini_call[[1]]
  } else {
    mini_call <- NULL
  }

  x <- lapply(setNames(nm = all_slots), function(slot) {
    cr <- util_combine_res(lapply(x, `[`, slot))
    if (length(cr) == 1) {
      r <- cr[[1]][[slot]]
      rownames(r) <- NULL
    } else {
      old_r <- cr
      r <- lapply(cr, `[[`, slot)
      if (slot == "SummaryPlot") {
        if (util_ensure_suggested("plotly",
            goal = "plot interactive figures",
            err = FALSE
          )) {
          as_plotlys <- unique(lapply(old_r, util_attr, "as_plotly"))
          as_plotly <- NULL
          if (length(as_plotlys) == 1) {
            if (!is.null(as_plotlys[[1]]) &&
                exists(as_plotlys[[1]], mode = "function")) {
              as_plotly <- get(as_plotlys[[1]], mode = "function")
            }
          }
          if (is.function(as_plotly)) {
            my_plots <- lapply(old_r, as_plotly)
          } else {
            my_plots <- lapply(r, util_plot_figure_plotly)
          }
          my_plots <- mapply(
            SIMPLIFY = FALSE,
            p = my_plots,
            nm = names(my_plots), function(p, nm) {
              title <- paste0("", nm)
              title <- util_sub_string_right_from_.(nm)
              if (inherits(p, "plotly")) {
                p <- plotly::config(
                  p,
                  responsive = TRUE,
                  displaylogo = FALSE
                )
              }
              react_size <- "
                  window.dq_rs2_script_init = false;
                  window.dq_rs2_script_init_fkt = function() {
                    if (window.dq_rs2_script_init) {
                      return;
                    }
                    window.dq_rs2_script_init = true;
                    var pys = $(document).find('.js-plotly-plot')
                    for (var i = 0; i < pys.length; i++) {
                      var py = pys[i]
                      Plotly.relayout(py, {
                                    width: py.getBoundingClientRect().width,
                                    height: py.getBoundingClientRect().height
                                  })
                    }
                  }
                  $(window.dq_rs2_script_init_fkt)
                "
              htmltools::div(
                style = "float:left;width:30%;height:33%;overflow-y:hidden;",
                htmltools::h2(
                  style = paste(
                    "position:relative;top:0;height:0;margin:0;padding:0;",
                    "z-index:9;"
                  ),
                  title
                ),
                p,
                htmltools::tags$script(
                  type = "text/javascript",
                  htmltools::HTML(react_size)
                )
              )
            }
          )

          my_plots <- htmltools::tagList(c(
            my_plots,
            list(htmltools::br(
              style =
                "clear: both;"
            ))
          ))
          r <- my_plots
        } else {
          r <- patchwork::wrap_plots(r, ncol = 2)
        }
      }
      class(r) <- union("dataquieR_result", class(r))
    }
    r
  })

  if (!is.null(mini_function_name)) {
    attr(x, "function_name") <- mini_function_name
  }
  if (!is.null(mini_cn)) {
    attr(x, "cn") <- mini_cn
  }
  if (!is.null(mini_call)) {
    attr(x, "call") <- mini_call
  }
  class(x) <- c("master_result")
  x
}

#' Internal helper: sectioned master result from result list
#'
#' @noRd
util_sectioned_master_result_from_result_list <- function(x,
  title_mode = c("function", "ssi")) {
  title_mode <- util_match_arg(title_mode)
  names(x) <- names(x)
  result_titles <- util_result_list_display_titles(x,
    title_mode = title_mode
  )
  function_names <- unique(unlist(lapply(
    x, util_attr,
    "function_name",
    exact = TRUE
  )))
  if (length(function_names) != 1) {
    function_names <- NULL
  }

  result <- list()
  attr(result, "dq_result_list") <- x
  attr(result, "dq_result_titles") <- result_titles
  attr(result, "function_name") <- function_names
  attr(result, "cn") <- "dq_questionnaire"
  class(result) <- "master_result"
  result
}

#' Internal helper: result list display titles
#'
#' @noRd
util_result_list_display_titles <- function(x, title_mode) {
  if (identical(title_mode, "ssi")) {
    return(util_result_list_entity_titles(x))
  }

  titles <- unname(vapply(x, function(result) {
    util_result_function_caption(
      util_attr(result, "function_name", exact = TRUE)
    )
  }, FUN.VALUE = character(1)))

  util_disambiguate_result_titles(x, titles)
}

#' Internal helper: result list entity titles
#'
#' @noRd
util_result_list_entity_titles <- function(x) {
  unname(mapply(
    SIMPLIFY = TRUE,
    USE.NAMES = FALSE,
    result = x,
    result_name = names(x),
    FUN = util_result_entity_title
  ))
}

#' Internal helper: result entity title
#'
#' @noRd
util_result_entity_title <- function(result, result_name) {
  result_title <- util_attr(result, "dq_result_title", exact = TRUE)
  if (length(result_title) == 1 &&
      is.character(result_title) &&
      !is.na(result_title) &&
      nzchar(result_title)) {
    return(result_title)
  }

  call <- util_attr(result, "call", exact = TRUE)
  entity_name <- util_attr(call, "entity_name", exact = TRUE)
  if (length(entity_name) == 1 &&
      is.character(entity_name) &&
      !is.na(entity_name) &&
      nzchar(entity_name)) {
    return(entity_name)
  }

  function_name <- util_attr(result, "function_name", exact = TRUE)
  function_prefix <- paste0(function_name, ".")
  if (length(function_name) == 1 &&
      startsWith(result_name, function_prefix)) {
    return(substring(result_name, nchar(function_prefix) + 1))
  }

  result_name
}

#' Internal helper: disambiguate result titles
#'
#' @noRd
util_disambiguate_result_titles <- function(x, titles) {
  duplicate_title <- duplicated(titles) | duplicated(titles, fromLast = TRUE)
  if (!any(duplicate_title)) {
    return(titles)
  }

  titles[duplicate_title] <- mapply(
    SIMPLIFY = TRUE,
    USE.NAMES = FALSE,
    result = x[duplicate_title],
    result_name = names(x)[duplicate_title],
    title = titles[duplicate_title],
    FUN = function(result, result_name, title) {
      detail <- util_result_entity_title(result, result_name)
      if (identical(detail, title)) {
        detail <- result_name
      }
      paste(
        title,
        detail,
        sep = ": "
      )
    }
  )
  titles
}

#' Internal helper: result function caption
#'
#' @noRd
util_result_function_caption <- function(function_name) {
  caption <- unname(util_alias2caption(function_name, long = TRUE))
  if (util_result_caption_empty(caption)) {
    caption <- util_function_caption_from_metadata(function_name,
      to = c(
        "menu_title_report",
        "dq_report2_short_title",
        "Implementationform"
      )
    )
  }
  if (util_result_caption_empty(caption)) {
    caption <- .manual$titles[[util_function_name_from_alias(function_name)]]
  }
  if (util_result_caption_empty(caption)) {
    caption <- function_name
  }
  unname(caption)
}

#' Internal helper: result caption empty
#'
#' @noRd
util_result_caption_empty <- function(caption) {
  length(caption) != 1 ||
    is.na(caption) ||
    util_empty(caption) ||
    identical(trimws(unname(caption)), "|")
}

#' Internal helper: function caption from metadata
#'
#' @noRd
util_function_caption_from_metadata <- function(alias, to) {
  function_name <- util_function_name_from_alias(alias)
  if (length(function_name) != 1 || is.na(function_name)) {
    return(NA_character_)
  }

  for (column in to) {
    caption <- util_map_labels(function_name,
      util_get_concept_info("implementations"),
      to = column,
      from = "function_R",
      ifnotfound = NA_character_
    )
    if (length(caption) == 1 &&
        !is.na(caption) &&
        !util_result_caption_empty(caption)) {
      return(unname(caption))
    }
  }

  NA_character_
}

#' Internal helper: function name from alias
#'
#' @noRd
util_function_name_from_alias <- function(alias) {
  function_name <- util_map_by_largest_prefix(
    alias,
    haystack = names(.manual$titles)
  )
  if (length(function_name) != 1 || is.na(function_name)) {
    return(alias)
  }
  function_name
}
