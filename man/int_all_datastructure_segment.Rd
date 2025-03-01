% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/int_all_datastructure_segment.R
\name{int_all_datastructure_segment}
\alias{int_all_datastructure_segment}
\title{Wrapper function to check for segment data structure}
\usage{
int_all_datastructure_segment(
  study_data,
  label_col,
  item_level = "item_level",
  meta_data = item_level,
  meta_data_v2,
  segment_level,
  meta_data_segment = "segment_level"
)
}
\arguments{
\item{study_data}{\link{data.frame} the data frame that contains the measurements}

\item{label_col}{\link{variable attribute} the name of the column in the metadata
with labels of variables}

\item{item_level}{\link{data.frame} the data frame that contains metadata
attributes of study data}

\item{meta_data}{\link{data.frame} old name for \code{item_level}}

\item{meta_data_v2}{\link{character} path to workbook like metadata file, see
\code{\link{prep_load_workbook_like_file}} for details.
\strong{ALL LOADED DATAFRAMES WILL BE PURGED},
using \code{\link{prep_purge_data_frame_cache}},
if you specify \code{meta_data_v2}.}

\item{segment_level}{\link{data.frame} alias for \code{meta_data_segment}}

\item{meta_data_segment}{\link{data.frame} the data frame that contains the metadata for the segment level, mandatory}
}
\value{
a \link{list} with
\itemize{
\item \code{SegmentTable}: data frame with selected check results, used for the data quality report.
}
}
\description{
This function tests for unexpected elements and records, as well as duplicated identifiers and content.
The unexpected element record check can be conducted by providing the number of expected records or
an additional table with the expected records.
It is possible to conduct the checks by study segments or to consider only selected
segments.

\link{Indicator}
}
\examples{
\dontrun{
out_segment <- int_all_datastructure_segment(
  meta_data_segment = "meta_data_segment",
  study_data = "ship",
  meta_data = "ship_meta"
)

study_data <- cars
meta_data <- dataquieR::prep_create_meta(VAR_NAMES = c("speedx", "distx"),
  DATA_TYPE = c("integer", "integer"), MISSING_LIST = "|", JUMP_LIST = "|",
  STUDY_SEGMENT = c("Intro", "Ex"))

out_segment <- int_all_datastructure_segment(
  meta_data_segment = "meta_data_segment",
  study_data = study_data,
  meta_data = meta_data
)
}
}
