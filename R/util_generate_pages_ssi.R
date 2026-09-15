#' Internal helper: generate pages partition study variables
#'
#' @noRd
util_generate_pages_partition_study_variables <- function(
  study_variables,
  meta_data
) {
  computed_variables <- character(0)
  required_columns <- c(VAR_NAMES, COMPUTED_VARIABLE_ROLE)
  if (is.data.frame(meta_data) &&
      all(required_columns %in% colnames(meta_data))) {
    is_computed <- !util_empty(meta_data[[COMPUTED_VARIABLE_ROLE]])
    computed_variables <- intersect(
      study_variables,
      meta_data[[VAR_NAMES]][is_computed]
    )
  }
  list(
    original = study_variables[!study_variables %in% computed_variables],
    computed = computed_variables
  )
}

#' Internal helper: generate pages ssi result data
#'
#' @noRd
util_generate_pages_ssi_result_data <- function(
  tb, curr_roles,
  mapping_names_labels_ssi
) {
  if (!is.data.frame(tb) ||
      !("Variables" %in% colnames(tb)) ||
      nrow(tb) == 0) {
    return(NULL)
  }

  vgs <- curr_roles[tb$Variables]
  # avoid to auto-expand to var_names/label-column pair
  colnames(tb)[colnames(tb) == "Variables"] <- "Metrics"
  rownames(tb) <- NULL
  tb[["Metrics"]] <- mapping_names_labels_ssi[vgs]
  tb
}

#' Internal helper: generate pages restore data types
#'
#' @noRd
util_generate_pages_restore_data_types <- function(tb, source_tables) {
  data_types <- vapply(
    setNames(nm = colnames(tb)),
    function(cl) {
      source_data_types <- lapply(source_tables, function(source_tb) {
        util_attr(source_tb[[cl]], DATA_TYPE, exact = TRUE)
      })
      source_data_types <- Filter(Negate(is.null), source_data_types)
      source_data_types <- unique(unlist(source_data_types))
      source_data_types <- source_data_types[!is.na(source_data_types)]
      if (length(source_data_types) == 1) {
        source_data_types[[1]]
      } else {
        NA_character_
      }
    },
    FUN.VALUE = character(1)
  )
  for (cl in colnames(tb)) {
    attr(tb[[cl]], DATA_TYPE) <- data_types[[cl]]
  }
  tb
}

#' Internal helper: generate pages compact ssi summary
#'
#' @noRd
util_generate_pages_compact_ssi_summary <- function(tb) {
  if (!is.data.frame(tb) || nrow(tb) == 0) {
    return(tb)
  }

  removed_title <- "Observational units removed"
  tb <- util_apply_summary_table_rules(
    tb,
    rules = util_ssi_summary_table_rules()
  )

  preferred_order <- intersect(c(
    "Metrics",
    "Labels",
    "Variables",
    "Admissible",
    "Below range N (%)",
    "Within range N (%)",
    "Above range N (%)",
    "All outside range N (%)",
    "N",
    removed_title
  ), colnames(tb))
  tb <- tb[, c(preferred_order, setdiff(colnames(tb), preferred_order)),
    drop = FALSE
  ]
  attr(tb, "description") <-
    util_get_hovertext("[ssi_summary_hover]")[colnames(tb)]
  tb
}

#' Internal helper: generate pages compact ssi metric summary
#'
#' @noRd
util_generate_pages_compact_ssi_metric_summary <- function(tb) {
  tb <- util_generate_pages_compact_ssi_summary(tb)
  if (is.data.frame(tb)) {
    attr(tb, "hideCols") <- union(
      util_attr(tb, "hideCols", exact = TRUE),
      "Variables"
    )
  }
  tb
}

#' Internal helper: generate pages link ssi summary metrics
#'
#' @noRd
util_generate_pages_link_ssi_summary_metrics <- function(
  tb,
  metric_roles,
  section_prefix
) {
  util_expect_scalar(section_prefix, check_type = is.character)
  if (!is.data.frame(tb) ||
      !("Metrics" %in% colnames(tb)) ||
      length(metric_roles) != nrow(tb)) {
    return(tb)
  }

  metric_labels <- tb[["Metrics"]]
  tb <- util_df_escape(tb)
  metric_links <- mapply(
    label = metric_labels,
    role = metric_roles,
    SIMPLIFY = FALSE,
    FUN = function(label, role) {
      if (is.na(role) || !nzchar(role)) {
        return(htmltools::HTML(htmltools::htmlEscape(label)))
      }
      htmltools::a(
        href = paste0(
          "#",
          htmltools::urlEncodePath(paste0(section_prefix, ".", role))
        ),
        label
      )
    }
  )
  tb[["Metrics"]] <- vapply(
    metric_links,
    as.character,
    FUN.VALUE = character(1)
  )
  attr(tb[["Metrics"]], DATA_TYPE) <- DATA_TYPES$STRING
  attr(tb, "is_html_escaped") <- TRUE
  tb
}

#' Internal helper: generate pages ssi section
#'
#' @noRd
util_generate_pages_ssi_section <- function(id,
  title,
  content,
  description = NULL) {
  util_expect_scalar(id, check_type = is.character)
  util_expect_scalar(title, check_type = is.character)
  id <- htmltools::urlEncodePath(id)
  description <- if (is.null(description) || util_empty(description)) {
    NULL
  } else {
    htmltools::div(
      class = "infobutton",
      htmltools::HTML(description)
    )
  }
  htmltools::tagList(
    anchor = htmltools::a(id = id),
    link = htmltools::a(href = paste0("#", id), title),
    htmltools::h5(title),
    description,
    content
  )
}

#' Internal helper: generate pages ssi sections
#'
#' @noRd
util_generate_pages_ssi_sections <- function(sections) {
  sections <- Filter(Negate(util_is_empty_html), sections)
  if (!length(sections)) {
    return(htmltools::tagList())
  }
  links <- lapply(sections, function(section) section[["link"]])
  links <- Filter(Negate(util_is_empty_html), links)
  links <- lapply(links, htmltools::tags$li)
  sections <- lapply(sections, function(section) {
    section[["link"]] <- NULL
    section
  })
  htmltools::tagList(
    util_float_index_menu(object = do.call(htmltools::tagList, links)),
    do.call(htmltools::tagList, sections)
  )
}
