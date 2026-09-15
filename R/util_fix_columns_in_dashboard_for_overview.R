#' Internal helper: fix columns in dashboard for overview
#'
#' @noRd
util_fix_columns_in_dashboard_for_overview <- function(
  dashboard_table,
  image_dir
) {
  util_ensure_suggested("jsonlite",
    goal = "dashboard views in overall-overviews",
    err = TRUE
  )

  if (nrow(dashboard_table) == 0) {
    return(dashboard_table)
  }

  dashboard_table <-
    util_extract_datauri_pngs(dashboard_table, image_dir = image_dir)

  if (nrow(dashboard_table) == 0) {
    return(dashboard_table)
  }

  orig_cn <- colnames(dashboard_table)

  trnsl <- util_translate(c("href", "value", "popup_href", "title"),
    as_this_translation = colnames(dashboard_table)
  )

  make_vnlb_link <- function(to_show, fallback_column = to_show) {
    if (!to_show %in% colnames(dashboard_table)) {
      if (fallback_column %in% colnames(dashboard_table)) {
        return(as.character(dashboard_table[[fallback_column]]))
      }
      return(rep("", nrow(dashboard_table)))
    }

    if (any(.cempt <- is.na(dashboard_table[[to_show]]))) {
      dashboard_table[[to_show]][.cempt] <- ""
    }

    vapply(apply(dashboard_table, 1, function(x) {
      res <- as.character(x[[to_show]])
      try({
        cnt <- x[[to_show]]
        if (!util_summary_link_available(
          x[[trnsl[["href"]]]],
          x[[trnsl[["popup_href"]]]]
        )) {
          return(res)
        }
        res <- htmltools::a(
          href = x[[trnsl[["href"]]]],
          title = htmltools::HTML(as.character(
            x[[trnsl[["value"]]]]
          )),
          onclick = util_summary_popup_handler(
            url = paste0(x[[trnsl[["popup_href"]]]]),
            link_url = paste0(x[[trnsl[["href"]]]]),
            title = x[[trnsl[["title"]]]],
            escape = FALSE
          ),
          htmltools::HTML(as.character(cnt))
        )
      })
      res
    }), as.character, FUN.VALUE = character(1))
  }

  dashboard_colnames <- colnames(dashboard_table)

  nm <- util_translate(VAR_NAMES, as_this_translation = dashboard_colnames)
  lb <- util_translate(LABEL, as_this_translation = colnames(dashboard_table))
  fig <- util_translate(
    "Figure", as_this_translation = colnames(dashboard_table)
  )
  gra <- util_translate(
    "Graph", as_this_translation = colnames(dashboard_table)
  )
  fqvn <- util_translate(
    "fq_VARNAME", as_this_translation = colnames(dashboard_table)
  )

  if (lb %in% colnames(dashboard_table)) {
    dashboard_table[[lb]] <- make_vnlb_link(lb)
  }
  if (nm %in% colnames(dashboard_table)) {
    dashboard_table[[nm]] <- make_vnlb_link(fqvn, fallback_column = nm)
  }
  if (fig %in% colnames(dashboard_table)) {
    dashboard_table[[fig]] <- make_vnlb_link(fig)
  }
  if (gra %in% colnames(dashboard_table)) {
    dashboard_table[[gra]] <- make_vnlb_link(gra)
  }

  # Historical Classification-based row filtering removed here.
  rows_to_take <- rep(TRUE, nrow(dashboard_table))

  rows_to_take[is.na(rows_to_take)] <- FALSE

  # Historical automatic long-column detection removed here.
  columns_to_shorten <- rep(FALSE, ncol(dashboard_table))


  if (any(columns_to_shorten)) {
    util_warning("Removed long columns from overall dashboard")
    dashboard_table[, columns_to_shorten] <-
      NA_character_
  }


  dashboard_table <-
    dashboard_table[rows_to_take, , FALSE]

  dashboard_table$title <- NULL
  dashboard_table$href <- NULL
  dashboard_table$popup_href <- NULL

  util_translated_colnames(dashboard_table) <-
    util_translate(
      util_translate(colnames(dashboard_table),
        as_this_translation = orig_cn,
        reverse = TRUE
      ),
      as_this_translation = orig_cn
    )

  dashboard_table
}

#' Internal helper: extract datauri pngs
#'
#' @noRd
util_extract_datauri_pngs <- function(dashboard_table, image_dir) {
  util_stop_if_not(is.data.frame(dashboard_table))
  util_stop_if_not(
    length(image_dir) == 1L,
    is.character(image_dir),
    nzchar(image_dir)
  )

  util_ensure_suggested(
    "jsonlite",
    goal = "dashboard views in overall-overviews",
    err = TRUE
  )

  image_dir_fs <- path.expand(image_dir)

  if (!dir.exists(image_dir_fs)) {
    dir.create(image_dir_fs, recursive = TRUE, showWarnings = FALSE)
  }

  image_dir_href <- basename(normalizePath(
    image_dir_fs, winslash = "/", mustWork = FALSE
  ))

  normalize_data_uri <- function(uri) {
    paste0(
      "data:image/png;base64,",
      gsub(
        "\\s+",
        "",
        sub("^data:image/png;base64,", "", uri, perl = TRUE),
        perl = TRUE
      )
    )
  }

  replace_src_attr <- function(attr_text, new_path) {
    quote_chr <- sub(
      '^src\\s*=\\s*(["\\\']).*$',
      "\\1",
      attr_text,
      perl = TRUE
    )
    paste0("src=", quote_chr, new_path, quote_chr)
  }

  process_cell <- function(x, dedup_env, file_map_env, next_id_env) {
    if (is.na(x) || !grepl("data:image/png;base64", x, fixed = TRUE)) {
      return(x)
    }

    m <- gregexpr(
      paste0(
        'src\\s*=\\s*["\\\']data:image/png;base64,',
        '[A-Za-z0-9+/=[:space:]]+["\\\']'
      ),
      x,
      perl = TRUE
    )[[1L]]

    if (identical(m, -1L)) {
      return(x)
    }

    attrs <- regmatches(x, list(m))[[1L]]
    new_attrs <- attrs

    for (i in seq_along(attrs)) {
      attr_text <- attrs[[i]]

      uri <- sub(
        paste0(
          '^src\\s*=\\s*["\\\'](data:image/png;base64,',
          '[A-Za-z0-9+/=[:space:]]+)["\\\']$'
        ),
        "\\1",
        attr_text,
        perl = TRUE
      )

      uri_norm <- normalize_data_uri(uri)
      payload <- sub("^data:image/png;base64,", "", uri_norm, perl = TRUE)
      dedup_key <- paste0("k_", rlang::hash(payload))

      if (exists(dedup_key, envir = dedup_env, inherits = FALSE)) {
        rel_file <- get(dedup_key, envir = dedup_env, inherits = FALSE)
      } else {
        img_raw <- jsonlite::base64_dec(payload)

        rel_file <- sprintf("img_%04d.png", get("next_id", envir = next_id_env))
        assign(
          "next_id",
          get("next_id", envir = next_id_env) + 1L,
          envir = next_id_env
        )

        writeBin(img_raw, file.path(image_dir_fs, rel_file))
        assign(dedup_key, rel_file, envir = dedup_env)

        file_map <- get("file_map", envir = file_map_env)
        file_map[[length(file_map) + 1L]] <- list(
          file = file.path(image_dir_fs, rel_file),
          uri = uri_norm
        )
        assign("file_map", file_map, envir = file_map_env)
      }

      new_attrs[[i]] <- replace_src_attr(
        attr_text,
        file.path(image_dir_href, rel_file)
      )
    }

    regmatches(x, list(m)) <- list(new_attrs)
    x
  }

  out <- dashboard_table
  dedup_env <- new.env(parent = emptyenv())
  file_map_env <- new.env(parent = emptyenv())
  next_id_env <- new.env(parent = emptyenv())

  assign("file_map", list(), envir = file_map_env)
  assign("next_id", 1L, envir = next_id_env)

  hits <- matrix(
    FALSE,
    nrow = nrow(out),
    ncol = ncol(out),
    dimnames = list(rownames(out), names(out))
  )

  for (j in seq_along(out)) {
    if (is.factor(out[[j]])) {
      out[[j]] <- as.character(out[[j]])
    }

    if (!is.character(out[[j]])) {
      next
    }

    col_hits <- !is.na(out[[j]]) & grepl(
      "data:image/png;base64", out[[j]], fixed = TRUE
    )
    hits[, j] <- col_hits

    if (any(col_hits)) {
      out[[j]][col_hits] <- vapply(
        out[[j]][col_hits],
        process_cell,
        character(1L),
        dedup_env = dedup_env,
        file_map_env = file_map_env,
        next_id_env = next_id_env
      )
    }
  }

  file_map_list <- get("file_map", envir = file_map_env)
  file_map <- if (length(file_map_list)) {
    do.call(
      rbind,
      lapply(file_map_list, function(x) {
        data.frame(
          file = x$file,
          uri = x$uri,
          stringsAsFactors = FALSE
        )
      })
    )
  } else {
    data.frame(
      file = character(),
      uri = character(),
      stringsAsFactors = FALSE
    )
  }

  out
}
