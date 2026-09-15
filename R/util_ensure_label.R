#' Utility function ensuring valid labels and variable names
#'
#' Valid labels should not be empty, be unique and do not exceed a certain
#' length.
#'
#' @inheritParams .template_function_developer
#'
#' @return a list containing the study data, possibly with adapted column names,
#'         the metadata, possibly with adapted labels, and a string and a
#'         table informing about the changes
#'
#' @noRd
util_ensure_label <- function(meta_data, label_col) {
  util_stop_if_not(label_col %in% colnames(meta_data))
  if (grepl("^LONG_", label_col)) {
    max_label_len <- MAX_LONG_LABEL_LEN
  } else {
    max_label_len <- MAX_LABEL_LEN
  }
  res <- .util_ensure_label(
    meta_data = meta_data,
    label_col = label_col,
    max_label_len = max_label_len
  )
  for (lc in setdiff(unique(intersect(colnames(meta_data), c(
    LABEL,
    LONG_LABEL,
    grep("^LABEL_", colnames(meta_data), value = TRUE),
    grep("^LONG_LABEL_", colnames(meta_data), value = TRUE)
  ))), label_col)) {
    if (grepl("^LONG_", lc)) {
      max_label_len <- MAX_LONG_LABEL_LEN
    } else {
      max_label_len <- MAX_LABEL_LEN
    }
    cur_res <- .util_ensure_label(
      meta_data = meta_data,
      label_col = lc,
      max_label_len = max_label_len
    )
    # nocov start
    if (lc == VAR_NAMES) {
      lc <- cur_res$label_col
      if (label_col == VAR_NAMES) {
        res$label_col <- cur_res$label_col
      }
    }
    # nocov end
    res$meta_data[[lc]] <-
      cur_res$meta_data[[lc]]
    if (!grepl(cur_res$label_modification_text,
        res$label_modification_text,
        fixed = TRUE
      )) {
      res$label_modification_text <- paste0(
        res$label_modification_text,
        cur_res$label_modification_text,
        collapse = "\n"
      )
      res$label_modification_table <-
        rbind(
          res$label_modification_table,
          cur_res$label_modification_table
        )
    }
  }
  res
}

#' Utility function ensuring valid labels and variable names -- internal version
#'
#' really shortens only label-col
#'
#' @param max_label_len [integer] maximum length for the labels, defaults to
#'                                `r MAX_LABEL_LEN`.
#' @return a list containing the study data, possibly with adapted column names,
#'         the metadata, possibly with adapted labels, and a string and a
#'         table informing about the changes
#'
#' @noRd
#' @inheritParams util_ensure_label
.util_ensure_label <- function(meta_data, label_col,
  max_label_len = MAX_LABEL_LEN) {
  util_expect_scalar(label_col, check_type = is.character)
  util_expect_data_frame(meta_data, col_names = c(label_col, VAR_NAMES))
  max_label_len <- .util_validate_label_length_limit(max_label_len,
    argument = "max_label_len"
  )

  if (any(util_empty(meta_data[[VAR_NAMES]]))) {
    util_warning(
      c(
        "Need %s in %s, for all variables, some are missing.",
        "I'll discard all %s without %s."
      ),
      sQuote(VAR_NAMES),
      sQuote("meta_data"),
      sQuote("meta_data"),
      sQuote(VAR_NAMES),
      applicability_problem = TRUE
    )
    meta_data <- meta_data[!util_empty(meta_data[[VAR_NAMES]]), , drop = FALSE]
  }
  original <- list(
    meta_data = meta_data,
    label_col = label_col
  )
  if (label_col == VAR_NAMES) {
    i <- 0
    new_label_col <- label_col
    while (new_label_col %in% colnames(meta_data)) {
      i <- i + 1
      new_label_col <- paste0(label_col, "_", i)
    }
    meta_data[[new_label_col]] <-
      meta_data[[label_col]]
    label_col <- new_label_col
  }
  n_label <- length(original$meta_data[[original$label_col]])
  modified_label <- original$meta_data[[original$label_col]]
  label_modification_text <- NULL
  label_modification_table <- data.frame(
    "Label-Column" = rep(original$label_col, n_label),
    "Original label" = original$meta_data[[original$label_col]],
    "Modified label" = vector(mode = "character", length = n_label),
    "Reason" = vector(mode = "character", length = n_label),
    check.names = FALSE
  )

  # check for missing labels ---------------------------------------------------
  if (any(ind_no_label <- util_empty(modified_label))) {
    if (sum(ind_no_label) > 0) {
      util_warning(
        c(
          "Some variables have no label in %s in %s.",
          "Labels are required to create a report,",
          "that's why missing labels will be replaced",
          "provisionally. Please add missing labels in",
          "your metadata."
        ),
        dQuote(label_col),
        sQuote("meta_data"),
        applicability_problem = TRUE
      )
      new_label <- meta_data[[VAR_NAMES]][ind_no_label]
    } else {
      new_label <- character(0)
    }

    # check for possible duplication
    while (any(new_label %in% modified_label)) {
      # estimate the required number of digits:
      nn <- ceiling(log10(n_label))
      new_label <- paste("NO LABEL", sprintf(
        paste0("%0", nn + 2, "d"),
        sample(1:10^(nn + 1), # sample a random number
          # the set from which we sample is larger than the number of
          # variables, so even if there are already similar labels in
          # the study data, there are most likely enough unassigned
          # numbers left
          size = sum(ind_no_label),
          replace = FALSE
        )
      ))
    }

    # apply new labels
    if (sum(ind_no_label) > 0) {
      modified_label[ind_no_label] <- new_label
      label_modification_text <-
        paste0(
          paste(sQuote(new_label), collapse = ", "),
          ifelse(sum(ind_no_label) > 1,
            paste(
              " were introduced for",
              sum(ind_no_label),
              "missing labels."
            ),
            " was introduced for one missing label."
          )
        )
      label_modification_table$Reason[ind_no_label] <- "No label specified."
    }
  }

  # check for duplicated labels ------------------------------------------------

  if (any(duplicated(modified_label))) {
    dupl_lab <- unique(modified_label[which(duplicated(modified_label))])
    if (length(dupl_lab) > 0) {
      util_warning(
        c(
          "Some variables have duplicated labels in %s in %s.",
          "Unique labels are required to create a report,",
          "that's why duplicated labels will be replaced",
          "provisionally. Please modify the labels in",
          "your metadata or select a suitable column."
        ),
        dQuote(label_col),
        sQuote("meta_data"),
        applicability_problem = TRUE
      )
    }
    mod_lab_before <- modified_label

    dps <- util_duplicated_inclding_first(modified_label)
    if (any(dps)) {
      # Fall back from original label values to stable variable names.
      modified_label[dps] <- paste0(
        original$meta_data[[VAR_NAMES]][dps],
        ": ",
        modified_label[dps]
      )
    }

    dupl_lab <- unique(modified_label[which(duplicated(modified_label))])

    for (ll in dupl_lab) {
      ii <- which(modified_label == ll)
      ndupl_ll <- length(ii)
      if (ndupl_ll > 2) {
        # label is duplicated more than once, so we need to add numbers
        label_suffix <- c(
          "", # the first occurrence does not get a suffix
          paste("DUPLICATE", sprintf(
            paste0(
              "%0",
              # estimate the required number of digits:
              ceiling(log10(ndupl_ll - 1)),
              "d"
            ),
            seq_len(ndupl_ll - 1)
          ))
        )
      } else {
        label_suffix <- c("", "DUPLICATE")
      }
      new_label <- trimws(paste(ll, label_suffix))
      # check for possible duplication
      while (any(new_label[-1] %in% modified_label)) {
        jj <- which(new_label %in% modified_label)[-1]
        # estimate the required number of digits:
        nn <- ceiling(log10(n_label))
        new_label[jj] <- paste(new_label[jj], sprintf(
          paste0("%0", nn + 2, "d"),
          sample(1:10^(nn + 1), # sample a random number
            # the set from which we sample is larger than the number of
            # variables, so even if there are already similar labels in
            # the study data, there are most likely enough unassigned
            # numbers left
            size = length(jj),
            replace = FALSE
          )
        ))
      }
      modified_label[ii] <- new_label
    }

    ind_dupl <- which(mod_lab_before != modified_label)
    if (length(ind_dupl) > 0) {
      label_modification_text <- c(
        label_modification_text,
        paste0(
          paste(sQuote(modified_label[ind_dupl]), collapse = ", "),
          ifelse(length(ind_dupl) > 1,
            " were introduced for duplicated labels.",
            " was introduced for one duplicated label."
          )
        )
      )
      label_modification_table$Reason[ind_dupl] <- trimws(
        paste(label_modification_table$Reason[ind_dupl], "Duplicated label.")
      )
    }
  }

  # abbreviate long labels -----------------------------------------------------
  if (any(nchar(modified_label) > max_label_len)) {
    ind_long_label <- which(nchar(modified_label) > max_label_len)
    if (length(ind_long_label) > 0) {
      util_warning(
        c(
          "Some variables have labels with more than %d characters",
          "in %s in %s.",
          "This will cause suboptimal outputs and possibly also",
          "failures when rendering the report,",
          "due to issues with the maximum length of file names",
          "in your operating system or file system.",
          "This will be fixed provisionally. Please shorten",
          "your labels or choose another label column."
        ),
        max_label_len,
        dQuote(label_col),
        sQuote("meta_data"),
        applicability_problem = TRUE,
        additional_classes = LONG_LABEL_EXCEPTION
      )
    }

    new_label <- .util_make_readable_short_labels(
      label = modified_label[ind_long_label],
      var_names = original$meta_data[[VAR_NAMES]][ind_long_label],
      max_label_len = max_label_len,
      other_labels = modified_label[-ind_long_label]
    )
    label_modification_table$Reason[ind_long_label] <- trimws(
      paste(
        label_modification_table$Reason[ind_long_label],
        "The label is too long."
      )
    )
    # check for possible duplication between abbreviated labels and the
    # other, unchanged labels
    temp_label <- modified_label
    temp_label[ind_long_label] <- new_label
    guard <- 0
    while (any(duplicated(temp_label))) {
      # add the duplicated labels to the selection of labels that should
      # be adapted by 'abbreviate'
      ind_long_label <- c(
        ind_long_label,
        which(modified_label %in% new_label)
      )
      ind_long_label <- sort(unique(ind_long_label))
      new_label <- .util_make_readable_short_labels(
        label = modified_label[ind_long_label],
        var_names = original$meta_data[[VAR_NAMES]][ind_long_label],
        max_label_len = max_label_len,
        other_labels = modified_label[-ind_long_label]
      )
      if (any(duplicated(new_label))) {
        util_error(
          c(
            "Could not shorten labels to at most %d",
            "characters preserving uniqueness. Try to",
            "allow more characters setting the %s option()",
            "or amend your labels."
          ),
          max_label_len, sQuote("dataquieR.MAX_LABEL_LEN"),
          applicability_problem = TRUE
        )
      }
      temp_label[ind_long_label] <- new_label
      guard <- guard + 1
      if (guard > 1000) {
        util_error(
          c(
            "Internal error: Could not shorten labels to at most %d",
            "characters preserving uniqueness. Try to",
            "allow more characters setting the %s option()",
            "or amend your labels. I gave up after %d tries (!!).",
            "This should not happen, sorry, please report."
          ),
          max_label_len, sQuote("dataquieR.MAX_LABEL_LEN"), 1000,
          applicability_problem = TRUE
        )
      }
    }

    modified_label <- temp_label

    if (any(util_empty(label_modification_table$Reason[ind_long_label]))) {
      ind_long_label2 <- intersect(
        ind_long_label,
        which(util_empty(
          label_modification_table$Reason
        ))
      )
      label_modification_table$Reason[ind_long_label2] <- trimws(
        paste(
          label_modification_table$Reason[ind_long_label2],
          "The label was identical to an abbreviated label."
        )
      )
    }

    if (any(ind_no_label)) {
      # If we store the original labels or variable names (only needed if they
      # are abbreviated), then we have to replace empty entries, otherwise we
      # will run into errors later with 'util_find_var_by_meta'
      # running 'util_map_labels' for 'ORIGINAL_LABEL'.
      original$meta_data[[original$label_col]][ind_no_label] <- "(missing)"
    }
    meta_data[["ORIGINAL_LABEL"]] <- original$meta_data[[original$label_col]]

    if (length(ind_long_label) > 0) {
      label_modification_text <- c(
        label_modification_text,
        paste0(
          paste(sQuote(modified_label[ind_long_label]), collapse = ", "),
          ifelse(length(ind_long_label) > 1,
            paste0(
              " were introduced as abbreviated labels for ",
              paste(sQuote(original$meta_data[[original$label_col]][ind_long_label]), # nolint: line_length_linter.
                collapse = ", "
              ), "."
            ),
            paste0(
              " was introduced as an abbreviation for the label ",
              sQuote(original$meta_data[[original$label_col]][ind_long_label]), "." # nolint: line_length_linter.
            )
          )
        )
      )
    }
  }

  # apply modified labels now that all checks are done -------------------------

  if (is.data.frame(meta_data)) {
    meta_data[[label_col]] <- modified_label
  }

  label_modification_table[, "Modified label"] <- modified_label
  # only show entries with modifications
  label_modification_table <- label_modification_table[which(
    label_modification_table[, "Original label", drop = TRUE] !=
      label_modification_table[, "Modified label", drop = TRUE] |
      !util_empty(label_modification_table[, "Reason", drop = TRUE])
  ), , drop = FALSE]

  return(list(
    meta_data = meta_data,
    label_modification_text = trimws(paste(label_modification_text,
        collapse = " "
      )),
    label_modification_table = label_modification_table,
    label_col = label_col
  ))
}

#' Internal helper: util make readable short labels
#'
#' @noRd
.util_make_readable_short_labels <- function(label, var_names, max_label_len,
  other_labels = character(0)) {
  util_stop_if_not(length(label) == length(var_names),
    label = "label and var_names must have the same length"
  )
  max_label_limit <-
    .util_validate_label_length_limit(max_label_len,
      argument = "max_label_len"
    )
  max_label_len <- max_label_limit
  other_labels <- .util_squish_label(other_labels)
  labels <- .util_squish_label(label)
  variable_names <- .util_squish_label(var_names)
  # A unique human-readable label needs no technical variable-name prefix.
  # Add that anchor only where shortening would otherwise create ambiguity.
  new_label <- unname(vapply(
    labels,
    .util_shorten_label_at_word,
    max_label_len = max_label_len,
    FUN.VALUE = character(1)
  ))
  collides <- new_label %in% other_labels |
    duplicated(new_label) |
    duplicated(new_label, fromLast = TRUE)
  needs_variable_anchor <- util_empty(labels) |
    labels == variable_names |
    collides
  new_label[needs_variable_anchor] <- vapply(
    which(needs_variable_anchor),
    function(i) {
      .util_make_readable_short_label(
        label = labels[[i]],
        var_name = variable_names[[i]],
        max_label_len = max_label_len
      )
    },
    FUN.VALUE = character(1)
  )

  seen_labels <- other_labels[!util_empty(other_labels)]
  for (i in seq_along(new_label)) {
    new_label[[i]] <- .util_make_unique_short_label(
      x = new_label[[i]],
      label = label[[i]],
      var_name = var_names[[i]],
      seen_labels = seen_labels,
      max_label_len = max_label_len
    )
    seen_labels <- c(seen_labels, new_label[[i]])
  }
  if (any(new_label %in% other_labels) || any(duplicated(new_label)) ||
      any(nchar(new_label) > max_label_len) || any(util_empty(new_label))) {
    util_error(
      c(
        "Internal error: Could not shorten labels to at most %d",
        "characters preserving uniqueness. This should not happen,",
        "sorry, please report."
      ),
      max_label_limit,
      applicability_problem = TRUE
    )
  }
  new_label
}

#' Internal helper: util make readable short label
#'
#' @noRd
.util_make_readable_short_label <- function(label, var_name, max_label_len) {
  label <- .util_squish_label(label)
  var_name <- .util_squish_label(var_name)
  if (util_empty(label)) {
    label <- var_name
  }
  if (util_empty(var_name)) {
    return(.util_shorten_label_at_word(label, max_label_len))
  }

  min_label_len <- min(36L, max(8L, floor(max_label_len * 0.65)))
  max_prefix_len <- max_label_len - nchar(": ") - min_label_len
  max_prefix_len <- min(20L, max(0L, max_prefix_len))
  if (max_prefix_len >= 4L) {
    prefix <- .util_shorten_var_name(var_name, max_prefix_len)
    label_len <- max_label_len - nchar(prefix) - nchar(": ")
    if (label_len >= 4L) {
      return(paste0(
        prefix, ": ",
        .util_shorten_label_at_word(label, label_len)
      ))
    }
  }
  .util_shorten_label_at_word(label, max_label_len)
}

#' Internal helper: util make unique short label
#'
#' @noRd
.util_make_unique_short_label <- function(x, label, var_name, seen_labels,
  max_label_len) {
  seen_labels <- seen_labels[!util_empty(seen_labels)]
  if (!util_empty(x) && !(x %in% seen_labels)) {
    return(x)
  }

  for (guard in seq_len(1000L) - 1L) {
    suffix <- paste(guard + 2L)
    candidate <- .util_add_short_label_suffix(
      x = x,
      suffix = suffix,
      max_label_len = max_label_len,
      min_stem_len = 4L
    )
    if (!util_empty(candidate) && !(candidate %in% seen_labels)) {
      return(candidate)
    }
  }

  candidate <- .util_shorten_var_name(var_name, max_label_len)
  if (!util_empty(candidate) && nchar(candidate) >= 4L &&
      !(candidate %in% seen_labels)) {
    return(candidate)
  }

  if (max_label_len >= 8L) {
    for (guard in seq_len(1000L) - 1L) {
      suffix <- paste0(
        " [",
        substr(
          rlang::hash(list(var_name, label, guard)),
          1, 6
        ),
        "]"
      )
      candidate <- .util_add_short_label_suffix(
        x = x,
        suffix = suffix,
        max_label_len = max_label_len,
        min_stem_len = 4L
      )
      if (!util_empty(candidate) && !(candidate %in% seen_labels)) {
        return(candidate)
      }
    }
  }

  util_error(
    c(
      "Could not create recognizable, unique labels with at most",
      "%d characters. Please increase the %s option()",
      "or amend your labels."
    ),
    max_label_len, sQuote("dataquieR.MAX_LABEL_LEN"),
    applicability_problem = TRUE
  )
}

#' Internal helper: util squish label
#'
#' @noRd
.util_squish_label <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- gsub("[/\\\\]+", " ", x)
  trimws(gsub("[[:space:]]+", " ", x))
}

#' Internal helper: util shorten var name
#'
#' @noRd
.util_shorten_var_name <- function(var_name, max_label_len) {
  max_label_len <- as.integer(max_label_len)
  if (max_label_len < 1L) {
    return("")
  }
  var_name <- .util_squish_label(var_name)
  if (util_empty(var_name)) {
    var_name <- "var"
  }
  if (nchar(var_name) <= max_label_len) {
    return(var_name)
  }
  hash_len <- min(6L, max(1L, max_label_len - 2L))
  hash <- substr(rlang::hash(var_name), 1, hash_len)
  sep <- "~"
  keep_len <- max_label_len - nchar(sep) - nchar(hash)
  if (keep_len < 1L) {
    return(substr(hash, 1, max_label_len))
  }
  paste0(substr(var_name, 1, keep_len), sep, hash)
}

#' Internal helper: util shorten label at word
#'
#' @noRd
.util_shorten_label_at_word <- function(label, max_label_len,
  omission = "...") {
  max_label_len <- as.integer(max_label_len)
  if (max_label_len < 1L) {
    return("")
  }
  label <- .util_squish_label(label)
  if (nchar(label) <= max_label_len) {
    return(label)
  }
  omission_len <- nchar(omission)
  if (max_label_len <= omission_len) {
    return(substr(label, 1, max_label_len))
  }
  keep_len <- max_label_len - omission_len
  prefix <- substr(label, 1, keep_len)
  word_prefix <- sub("[[:space:]][^[:space:]]*$", "", prefix)
  min_keep <- min(keep_len, max(10L, floor(keep_len * 0.6)))
  if (nchar(word_prefix) >= min_keep) {
    prefix <- word_prefix
  }
  prefix <- trimws(gsub("[[:space:][:punct:]]+$", "", prefix))
  if (!nzchar(prefix)) {
    prefix <- trimws(substr(label, 1, keep_len))
  }
  paste0(prefix, omission)
}

#' Internal helper: util add short label suffix
#'
#' @noRd
.util_add_short_label_suffix <- function(x, suffix, max_label_len,
  min_stem_len = 0L) {
  suffix <- .util_squish_label(suffix)
  suffix_sep <- " "
  suffix_len <- nchar(suffix_sep) + nchar(suffix)
  if (suffix_len >= max_label_len) {
    return("")
  }
  stem <- .util_shorten_label_at_word(
    label = x,
    max_label_len - suffix_len,
    omission = ""
  )
  if (nchar(stem) < min_stem_len) {
    return("")
  }
  paste0(stem, suffix_sep, suffix)
}
