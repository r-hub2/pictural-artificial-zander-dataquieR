% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/util_dichotomize.R
\name{util_dichotomize}
\alias{util_dichotomize}
\title{Utility function to dichotomize variables}
\usage{
util_dichotomize(study_data, meta_data, label_col = VAR_NAMES)
}
\arguments{
\item{study_data}{study data without jump/missing codes as specified in the
code conventions}

\item{meta_data}{metadata as specified in the code conventions}

\item{label_col}{\link{variable attribute} the name of the column in the metadata
with labels of variables}
}
\description{
This function uses the metadata attributes \code{RECODE_CASES} and/or
\code{RECODE_CONTROL} to dichotomize the data. 'Cases' will be recoded to 1,
'controls' to 0. The recoding can be specified by an interval (for metric
variables) or by a list of categories separated by the 'SPLIT_CHAR'. Recoding
will be used for data quality checks that include a regression model.
}
\seealso{
Other data_management: 
\code{\link{util_assign_levlabs}()},
\code{\link{util_check_data_type}()},
\code{\link{util_check_group_levels}()},
\code{\link{util_compare_meta_with_study}()},
\code{\link{util_fix_merge_dups}()},
\code{\link{util_merge_data_frame_list}()},
\code{\link{util_rbind}()},
\code{\link{util_remove_na_records}()},
\code{\link{util_replace_hard_limit_violations}()},
\code{\link{util_round_to_decimal_places}()},
\code{\link{util_study_var2factor}()},
\code{\link{util_table_of_vct}()}
}
\concept{data_management}
\keyword{internal}
