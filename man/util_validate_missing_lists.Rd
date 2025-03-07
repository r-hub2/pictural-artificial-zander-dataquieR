% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/util_validate_missing_lists.R
\name{util_validate_missing_lists}
\alias{util_validate_missing_lists}
\title{Validate code lists for missing and/or jump codes}
\usage{
util_validate_missing_lists(
  meta_data,
  cause_label_df,
  assume_consistent_codes = FALSE,
  expand_codes = FALSE,
  suppressWarnings = FALSE,
  label_col
)
}
\arguments{
\item{meta_data}{\link{data.frame} the data frame that contains metadata
attributes of study data}

\item{cause_label_df}{\link{data.frame} missing code table. If missing codes have
labels the respective data frame can be
specified here, see \link{cause_label_df}}

\item{assume_consistent_codes}{\link{logical} if TRUE and no labels are given and
the same missing/jump code is used
for more than one variable, the
labels assigned for this code will
be the same for all variables.}

\item{expand_codes}{\link{logical} if TRUE, code labels are copied from other
variables, if the code is the same and the
label is set somewhere}

\item{suppressWarnings}{\link{logical} warn about consistency issues with missing
and jump lists}

\item{label_col}{\link{variable attribute} the name of the column in the metadata
with labels of variables}
}
\value{
\link{list} with entries:
\itemize{
\item \code{cause_label_df} updated data frame with labels for missing codes
}
}
\description{
will warn/stop on problems
}
\seealso{
Other metadata_management: 
\code{\link{util_dist_selection}()},
\code{\link{util_find_free_missing_code}()},
\code{\link{util_find_var_by_meta}()},
\code{\link{util_get_var_att_names_of_level}()},
\code{\link{util_get_vars_in_segment}()},
\code{\link{util_looks_like_missing}()},
\code{\link{util_no_value_labels}()},
\code{\link{util_validate_known_meta}()}
}
\concept{metadata_management}
\concept{robustness}
\keyword{internal}
