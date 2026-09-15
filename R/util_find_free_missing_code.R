#' Check, if `x` contains valid missing codes
#'
#' @param x a vector of missing codes
#'
#' @return a missing code not in `x`
#' @family metadata_management
#' @noRd
util_find_free_missing_code <- function(x) {
  x <- lapply(util_as_valid_missing_codes(x), as.character)
  parsed_dates <- suppressWarnings(lapply(x, util_parse_date))
  parsed_times <- suppressWarnings(lapply(x, util_parse_time))
  missing_x <- vapply(x, function(v) all(is.na(v)), logical(1))
  valid_dates <- vapply(parsed_dates, function(v) !all(is.na(v)), logical(1))
  valid_times <- vapply(parsed_times, function(v) !all(is.na(v)), logical(1))
  if (all(missing_x == !valid_dates) && any(valid_dates)) {
    return(as.character(
      suppressWarnings(do.call("max", c(parsed_dates, list(na.rm = TRUE)))) +
        lubridate::days(1)
    ))
  } else if (all(missing_x == !valid_times) && any(valid_times)) {
    return(util_as_character(
      hms::as_hms(
        suppressWarnings(do.call("max", c(parsed_times, list(na.rm = TRUE)))) +
          hms::hms(hours = 1)
      )
    ))
  } else if (all(suppressWarnings(missing_x == is.na(as.numeric(x)))) &&
      any(suppressWarnings(!is.na(as.numeric(x))))) {
    return(as.character(suppressWarnings(max(as.numeric(x), na.rm = TRUE)) + 1))
  } else if (all(missing_x)) {
    util_error("Cannot find a free missing code without any valid codes.")
  } else { # fallback to datetime
    return(suppressWarnings(do.call("max", c(
      lapply(x, util_parse_date),
      list(na.rm = TRUE)
    ))) +
      lubridate::days(1))
  }
}
