#' Add a stable variable-group identity to a result
#'
#' @param result A result or result-like list.
#' @param check_id Scalar `CHECK_ID` value.
#' @param check_label Scalar `CHECK_LABEL` value.
#'
#' @return `result` with the group identity stored as attributes and in
#'   group-level table components.
#' @noRd
util_add_variable_group_identity <- function(result, check_id, check_label) {
  if (length(check_id) != 1L || util_empty(check_id)) {
    return(result)
  }
  check_id <- as.character(check_id)
  check_label <- as.character(check_label)
  attr(result, CHECK_ID) <- check_id
  attr(result, CHECK_LABEL) <- check_label

  group_components <- c(
    "VariableGroupTable",
    "VariableGroupData",
    "OtherTable"
  )
  for (component in intersect(group_components, names(result))) {
    if (is.data.frame(result[[component]])) {
      result[[component]][[CHECK_ID]] <- rep(
        check_id,
        nrow(result[[component]])
      )
      result[[component]][[CHECK_LABEL]] <- rep(
        check_label,
        nrow(result[[component]])
      )
    }
  }
  result
}
