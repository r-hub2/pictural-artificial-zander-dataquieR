#' Get description for a call
#'
#' @param cn the call name
#' @param report [dataquieR_resultset2] the report
#' @param function_alias_map [data.frame] the `function_alias_map` of a report,
#'                           alternative for passing the report
#'
#' @return the description
#'
#' @noRd
util_col_description <- function(cn, report, function_alias_map) {
  if (missing(report) && missing(function_alias_map)) {
    fname <- cn
  } else if (missing(function_alias_map)) {
    fname <- util_cll_nm2fkt_nm(cn, report = report)
  } else {
    fname <- util_cll_nm2fkt_nm(cn, function_alias_map = function_alias_map)
  }
  fname <- unname(fname)

  mapped_fname <- util_map_by_largest_prefix(
    fname,
    haystack = names(.manual$titles)
  )
  if (!is.na(mapped_fname)) {
    fname <- unname(mapped_fname)
  }

  return(util_function_description(fname))
}
