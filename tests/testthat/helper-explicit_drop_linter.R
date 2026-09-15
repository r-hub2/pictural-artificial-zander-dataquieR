find_missing_drop_arguments <- function(search_path) {
  files <- unlist(lapply(search_path, function(path) {
    if (dir.exists(path)) {
      list.files(path, pattern = "[.]R$", full.names = TRUE, recursive = TRUE)
    } else {
      path
    }
  }), use.names = FALSE)

  bracket_token <- "'['"
  comma_token <- "','"
  findings <- list()

  is_replacement_subset <- function(parse_data, expr_id) {
    parent_id <- parse_data$parent[parse_data$id == expr_id]
    if (length(parent_id) != 1L || is.na(parent_id)) {
      return(FALSE)
    }

    siblings <- parse_data[parse_data$parent == parent_id, , drop = FALSE]
    expr_row <- siblings$id == expr_id
    assign_row <- siblings$token %in% c("LEFT_ASSIGN", "EQ_ASSIGN")
    any(expr_row) && any(assign_row) &&
      siblings$col1[expr_row][1L] < siblings$col1[assign_row][1L]
  }

  for (file in files) {
    parse_data <- utils::getParseData(
      parse(file, keep.source = TRUE),
      includeText = TRUE
    )

    for (id in unique(parse_data$id[parse_data$token == "expr"])) {
      children <- parse_data[parse_data$parent == id, , drop = FALSE]
      has_drop <- any(
        children$token == "SYMBOL_SUB" & children$text == "drop"
      )
      has_report_raw_flag <- any(
        children$token == "SYMBOL_SUB" & children$text == "as_raw"
      )

      if (any(children$token == bracket_token) &&
          sum(children$token == comma_token) >= 1L && !has_drop &&
          !has_report_raw_flag &&
          !is_replacement_subset(parse_data, id)) {
        bracket_rows <- children$token == bracket_token
        bracket <- children[bracket_rows, , drop = FALSE][1L, , drop = FALSE]
        findings[[length(findings) + 1L]] <- data.frame(
          file = file,
          line = bracket$line1,
          column = bracket$col1,
          code = parse_data$text[match(id, parse_data$id)],
          stringsAsFactors = FALSE
        )
      }
    }
  }

  if (!length(findings)) {
    return(data.frame(
      file = character(),
      line = integer(),
      column = integer(),
      code = character(),
      stringsAsFactors = FALSE
    ))
  }

  do.call(rbind, findings)
}
