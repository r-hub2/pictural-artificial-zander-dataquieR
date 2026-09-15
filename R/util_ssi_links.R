#' Create SSI cross-item page titles
#'
#' @param row one row from cross-item metadata
#'
#' @return named character vector with `short_title` and `long_title`
#' @keywords internal
#' @noRd
util_generate_pages_ssi_cross_item_titles <- function(row) {
  value <- function(name) {
    if (name %in% names(row)) {
      row[[name]]
    } else {
      NA_character_
    }
  }

  check_id <- value(CHECK_ID)
  check_label <- value(CHECK_LABEL)
  scale_name <- value(SCALE_NAME)
  scale_acronym <- value(SCALE_ACRONYM)
  short_title <- util_first_non_empty_character(c(
    check_label,
    scale_acronym,
    scale_name,
    check_id
  ))
  long_title <- util_first_non_empty_character(c(
    check_label,
    scale_name,
    scale_acronym,
    check_id
  ))
  scale_details <- c(scale_name, scale_acronym)
  scale_details <- unique(scale_details[!vapply(
    scale_details,
    util_result_caption_empty,
    FUN.VALUE = logical(1)
  )])
  if (!util_result_caption_empty(check_label) && length(scale_details)) {
    long_title <- paste0(
      check_label,
      " (",
      paste(scale_details, collapse = " -- "),
      ")"
    )
  } else if (!util_result_caption_empty(scale_name) &&
      !util_result_caption_empty(scale_acronym)) {
    long_title <- paste0(scale_name, " (", scale_acronym, ")")
  }

  c(short_title = short_title, long_title = long_title)
}

#' Create an SSI cross-item page href
#'
#' @param row one row from cross-item metadata
#'
#' @return scalar character href or `NA_character_`
#' @keywords internal
#' @noRd
util_ssi_cross_item_href <- function(row) {
  titles <- util_generate_pages_ssi_cross_item_titles(row)
  short_title <- titles[["short_title"]]
  if (util_empty(short_title)) {
    return(NA_character_)
  }
  paste0(
    prep_link_escape(short_title, html = TRUE),
    ".html#",
    short_title
  )
}

#' Identify groups represented only by REDCap contradiction results
#'
#' @param summary_rows Long-format rows from a `dataquieR_summary` object.
#'
#' @return Character vector of stable `CHECK_ID` values.
#' @noRd
util_contradiction_only_group_ids <- function(summary_rows) {
  required <- c(CHECK_ID, "function_name")
  if (!is.data.frame(summary_rows) ||
      !all(required %in% colnames(summary_rows))) {
    return(character())
  }
  group_ids <- as.character(summary_rows[[CHECK_ID]])
  functions <- as.character(summary_rows[["function_name"]])
  keep <- !util_empty(group_ids) & !util_empty(functions)
  functions_by_group <- split(functions[keep], group_ids[keep])
  names(Filter(function(group_functions) {
    group_functions <- unique(group_functions)
    identical(group_functions, "con_contradictions_redcap")
  }, functions_by_group))
}

#' Attach navigation information to normalized cross-item metadata
#'
#' @param meta_data_cross_item Normalized cross-item metadata.
#' @param summary_rows Long-format rows from a `dataquieR_summary` object.
#'
#' @return `meta_data_cross_item` with internal navigation metadata.
#' @noRd
util_mark_contradiction_only_groups <- function(
  meta_data_cross_item,
  summary_rows
) {
  attr(meta_data_cross_item, "contradiction_only_check_ids") <-
    util_contradiction_only_group_ids(summary_rows)
  meta_data_cross_item
}

#' Create the href of the combined contradictions result
#'
#' @return Scalar character href.
#' @noRd
util_contradictions_overview_href <- function() {
  "dim_con.html#Contradictions"
}

#' Create the result-only popup href of the combined contradictions result
#'
#' @return Scalar character href.
#' @noRd
util_contradictions_overview_popup_href <- function() {
  "dim_con.html#nm=con_contradictions_redcap.[ALL]"
}

#' Create cross-item page hrefs for check identifiers
#'
#' @param check_ids check identifiers
#' @param meta_data_cross_item cross-item-level metadata
#'
#' @return character vector of hrefs, `NA_character_` if no target exists
#' @keywords internal
#' @noRd
util_cross_item_hrefs <- function(check_ids, meta_data_cross_item) {
  hrefs <- rep(NA_character_, length(check_ids))
  if (!length(check_ids) ||
      is.null(meta_data_cross_item) ||
      !CHECK_ID %in% colnames(meta_data_cross_item)) {
    return(hrefs)
  }

  cross_item_ids <- as.character(meta_data_cross_item[[CHECK_ID]])
  contradiction_only_ids <- util_attr(
    meta_data_cross_item,
    "contradiction_only_check_ids",
    exact = TRUE
  )
  for (i in seq_along(check_ids)) {
    check_id <- as.character(check_ids[[i]])
    row_index <- match(check_id, cross_item_ids)
    if (!is.na(row_index)) {
      hrefs[[i]] <- if (check_id %in% contradiction_only_ids) {
        util_contradictions_overview_href()
      } else {
        util_ssi_cross_item_href(
          meta_data_cross_item[row_index, , drop = FALSE]
        )
      }
    }
  }
  hrefs
}

#' Create the shared single-result-view identifier for a variable group
#'
#' @param check_ids cross-item `CHECK_ID` values
#'
#' @return character vector of popup identifiers
#' @keywords internal
#' @noRd
util_variable_group_popup_id <- function(check_ids) {
  paste0("variable_group.", as.character(check_ids))
}

#' Relink variable-group result URLs to generated report pages
#'
#' @noRd
util_relink_variable_group_hrefs <- function(
  hrefs,
  check_ids,
  meta_data_cross_item
) {
  group_hrefs <- util_cross_item_hrefs(check_ids, meta_data_cross_item)
  mapply(
    function(href, group_href) {
      if (is.na(group_href) || util_empty(group_href) || is.na(href)) {
        return(href)
      }
      folder_prefix <- sub(
        "^(.*/\\.report/).+$",
        "\\1",
        href
      )
      if (identical(folder_prefix, href)) {
        folder_prefix <- ""
      }
      paste0(folder_prefix, group_href)
    },
    hrefs,
    group_hrefs,
    USE.NAMES = FALSE
  )
}

#' Match explicit variable-group identifiers to summary row names
#'
#' @param var_names Summary row identifiers.
#' @param result Long-format summary rows containing `VAR_NAMES` and `CHECK_ID`.
#'
#' @return Character vector of explicit `CHECK_ID` values. Missing or ambiguous
#'   identities remain `NA_character_`.
#' @noRd
util_result_variable_group_ids <- function(var_names, result) {
  group_ids <- rep(NA_character_, length(var_names))
  if (!is.data.frame(result) ||
      !all(c(VAR_NAMES, CHECK_ID) %in% colnames(result))) {
    return(group_ids)
  }
  result_var_names <- as.character(result[[VAR_NAMES]])
  result_check_ids <- as.character(result[[CHECK_ID]])
  for (i in seq_along(var_names)) {
    if (util_empty(var_names[[i]])) {
      next
    }
    matching_rows <- !is.na(result_var_names) &
      result_var_names == var_names[[i]]
    ids <- unique(result_check_ids[matching_rows])
    ids <- ids[!util_empty(ids)]
    if (length(ids) == 1L) {
      group_ids[[i]] <- ids
    }
  }
  group_ids
}

#' Relink variable-group total cells to generated report pages
#'
#' @noRd
util_relink_variable_group_total_cells <- function(
  cells,
  check_ids,
  meta_data_cross_item
) {
  cells <- vapply(cells, function(cell) {
    if (!length(cell)) {
      return(NA_character_)
    }
    as.character(cell[[1]])
  }, character(1))
  hrefs <- vapply(cells, function(cell) {
    if (is.na(cell)) {
      return(NA_character_)
    }
    sub("^.*href=\"([^\"]*)\".*$", "\\1", cell)
  }, character(1))
  hrefs <- util_relink_variable_group_hrefs(
    hrefs,
    check_ids,
    meta_data_cross_item
  )
  mapply(
    function(cell, href) {
      if (is.na(href) || util_empty(href) || is.na(cell)) {
        return(cell)
      }
      sub('href="[^"]*"', sprintf('href="%s"', href), cell)
    },
    cells,
    hrefs,
    USE.NAMES = FALSE
  )
}

#' Create an SSI group page href
#'
#' @param computed_role computed variable role
#'
#' @return scalar character href or `NA_character_`
#' @keywords internal
#' @noRd
util_ssi_group_href <- function(computed_role) {
  if (util_empty(computed_role)) {
    return(NA_character_)
  }
  paste0(
    "SSIGROUP_",
    htmltools::urlEncodePath(as.character(computed_role)),
    ".html#",
    htmltools::urlEncodePath(as.character(computed_role))
  )
}

#' Create SSI cross-item page hrefs for computed variables
#'
#' @param var_names computed variable names
#' @param meta_data item-level metadata including `CHECK_ID`
#' @param meta_data_cross_item cross-item-level metadata
#'
#' @return character vector of hrefs, `NA_character_` if no SSI target exists
#' @keywords internal
#' @noRd
util_ssi_computed_variable_hrefs <- function(var_names, meta_data,
  meta_data_cross_item) {
  hrefs <- rep(NA_character_, length(var_names))

  if (!length(var_names) ||
      is.null(meta_data_cross_item) ||
      !CHECK_ID %in% colnames(meta_data) ||
      !COMPUTED_VARIABLE_ROLE %in% colnames(meta_data) ||
      !CHECK_ID %in% colnames(meta_data_cross_item)) {
    return(hrefs)
  }

  ssi_metrics <- util_get_concept_info("ssi")[["SSI_METRICS"]]
  computed_roles <- util_map_labels(
    var_names,
    meta_data = meta_data,
    from = VAR_NAMES,
    to = COMPUTED_VARIABLE_ROLE,
    ifnotfound = NA_character_,
    warn_ambiguous = FALSE
  )
  check_ids <- util_map_labels(
    var_names,
    meta_data = meta_data,
    from = VAR_NAMES,
    to = CHECK_ID,
    ifnotfound = NA_character_,
    warn_ambiguous = FALSE
  )
  ssi_rows <- !is.na(computed_roles) & computed_roles %in% ssi_metrics &
    !is.na(check_ids)

  if (!any(ssi_rows)) {
    return(hrefs)
  }

  role_hrefs <- vapply(
    computed_roles[ssi_rows],
    util_ssi_group_href,
    FUN.VALUE = character(1)
  )
  has_role_href <- !is.na(role_hrefs) & nzchar(role_hrefs)
  hrefs[which(ssi_rows)[has_role_href]] <- role_hrefs[has_role_href]
  hrefs
}

#' Create SSI cross-item links for computed variables
#'
#' @param var_names computed variable names
#' @param meta_data item-level metadata including `CHECK_ID`
#' @param meta_data_cross_item cross-item-level metadata
#'
#' @return data frame with `href` and `label`
#' @keywords internal
#' @noRd
util_ssi_computed_variable_cross_item_links <- function(var_names, meta_data,
  meta_data_cross_item) {
  links <- data.frame(
    href = rep(NA_character_, length(var_names)),
    label = rep(NA_character_, length(var_names)),
    stringsAsFactors = FALSE
  )

  if (!length(var_names) ||
      is.null(meta_data_cross_item) ||
      !CHECK_ID %in% colnames(meta_data) ||
      !COMPUTED_VARIABLE_ROLE %in% colnames(meta_data) ||
      !CHECK_ID %in% colnames(meta_data_cross_item)) {
    return(links)
  }

  ssi_metrics <- util_get_concept_info("ssi")[["SSI_METRICS"]]
  computed_roles <- util_map_labels(
    var_names,
    meta_data = meta_data,
    from = VAR_NAMES,
    to = COMPUTED_VARIABLE_ROLE,
    ifnotfound = NA_character_,
    warn_ambiguous = FALSE
  )
  check_ids <- util_map_labels(
    var_names,
    meta_data = meta_data,
    from = VAR_NAMES,
    to = CHECK_ID,
    ifnotfound = NA_character_,
    warn_ambiguous = FALSE
  )
  ssi_rows <- !is.na(computed_roles) & computed_roles %in% ssi_metrics &
    !is.na(check_ids)

  if (!any(ssi_rows)) {
    return(links)
  }

  for (idx in which(ssi_rows)) {
    cross_item_row <- meta_data_cross_item[
      trimws(meta_data_cross_item[[CHECK_ID]]) == trimws(check_ids[[idx]]),
      ,
      drop = FALSE
    ]
    if (!nrow(cross_item_row)) {
      next
    }
    cross_item_row <- cross_item_row[1, , drop = FALSE]
    titles <- util_generate_pages_ssi_cross_item_titles(cross_item_row)
    links$href[[idx]] <- util_cross_item_hrefs(
      cross_item_row[[CHECK_ID]],
      meta_data_cross_item
    )
    links$label[[idx]] <- titles[["short_title"]]
  }

  links
}
