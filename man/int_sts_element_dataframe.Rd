% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/int_sts_element_dataframe.R
\name{int_sts_element_dataframe}
\alias{int_sts_element_dataframe}
\title{Determine missing and/or superfluous data elements}
\usage{
int_sts_element_dataframe(
  item_level = "item_level",
  meta_data_dataframe = "dataframe_level",
  meta_data = item_level,
  meta_data_v2,
  check_type = getOption("dataquieR.ELEMENT_MISSMATCH_CHECKTYPE",
    dataquieR.ELEMENT_MISSMATCH_CHECKTYPE_default),
  dataframe_level
)
}
\arguments{
\item{item_level}{\link{data.frame} the data frame that contains metadata
attributes of study data}

\item{meta_data_dataframe}{\link{data.frame} the data frame that contains the
metadata for the data frame level}

\item{meta_data}{\link{data.frame} old name for \code{item_level}}

\item{meta_data_v2}{\link{character} path to workbook like metadata file, see
\code{\link{prep_load_workbook_like_file}} for details.
\strong{ALL LOADED DATAFRAMES WILL BE PURGED},
using \code{\link{prep_purge_data_frame_cache}},
if you specify \code{meta_data_v2}.}

\item{check_type}{\link{enum} none | exact | subset_u | subset_m. See
\link{dataquieR.ELEMENT_MISSMATCH_CHECKTYPE}}

\item{dataframe_level}{\link{data.frame} alias for \code{meta_data_dataframe}}
}
\value{
\link{list} with names lots:
\itemize{
\item \code{DataframeData}: data frame with the unexpected elements check results.
\item \code{DataframeTable}: \link{data.frame} table with all errors, used for the data quality report:
- \code{PCT_int_sts_element}: Percentage of element
mismatches
- \code{NUM_int_sts_element}: Number of element
mismatches
- \code{resp_vars}: affected element names
}
}
\description{
Depends on \link{dataquieR.ELEMENT_MISSMATCH_CHECKTYPE} option,
see there
}
\details{
\link{Indicator}
}
\examples{
\dontrun{
prep_load_workbook_like_file("~/tmp/df_level_test.xlsx")
meta_data_dataframe <- "dataframe_level"
meta_data <- "item_level"
}
}
