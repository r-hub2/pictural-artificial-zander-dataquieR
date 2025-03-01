% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/util_validate_known_meta.R
\name{util_abbreviate_unique}
\alias{util_abbreviate_unique}
\title{Abbreviate a vector of strings}
\usage{
util_abbreviate_unique(initial, max_value_label_len)
}
\arguments{
\item{initial}{\link{character} vector with stuff to abbreviate}

\item{max_value_label_len}{\link{integer} maximum length (may not strictly
be met, if not possible keeping a maybe
detected uniqueness of \code{initial})}
}
\value{
\link{character} uniquely abbreviated \code{initial}
}
\description{
Abbreviate a vector of strings
}
\seealso{
Other string_functions: 
\code{\link{util_filter_names_by_regexps}()},
\code{\link{util_pretty_vector_string}()},
\code{\link{util_set_dQuoteString}()},
\code{\link{util_set_sQuoteString}()},
\code{\link{util_sub_string_left_from_.}()},
\code{\link{util_sub_string_right_from_.}()},
\code{\link{util_translate}()}
}
\concept{string}
\concept{string_functions}
\keyword{internal}
