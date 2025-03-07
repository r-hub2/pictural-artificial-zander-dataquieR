% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prep_extract_cause_label_df.R
\name{prep_extract_cause_label_df}
\alias{prep_extract_cause_label_df}
\title{Extract all missing/jump codes from metadata and export a
cause-label-data-frame}
\usage{
prep_extract_cause_label_df(
  item_level = "item_level",
  label_col = VAR_NAMES,
  meta_data_v2,
  meta_data = item_level
)
}
\arguments{
\item{item_level}{\link{data.frame} the data frame that contains metadata
attributes of study data}

\item{label_col}{\link{variable attribute} the name of the column in the metadata
with labels of variables}

\item{meta_data_v2}{\link{character} path to workbook like metadata file, see
\code{\link{prep_load_workbook_like_file}} for details.
\strong{ALL LOADED DATAFRAMES WILL BE PURGED},
using \code{\link{prep_purge_data_frame_cache}},
if you specify \code{meta_data_v2}.}

\item{meta_data}{\link{data.frame} old name for \code{item_level}}
}
\value{
\link{list} with the entries
\itemize{
\item \code{meta_data} \link{data.frame} a data frame that contains updated metadata --
you still need to add a column
\link{MISSING_LIST_TABLE} and add the
\code{cause_label_df} as such to the metadata
cache using \code{\link[=prep_add_data_frames]{prep_add_data_frames()}}, manually.
\item \code{cause_label_df} \link{data.frame} missing code table. If missing codes have
labels the respective data frame are
specified here, see \link{cause_label_df}.
}
}
\description{
Extract all missing/jump codes from metadata and export a
cause-label-data-frame
}
\seealso{
\link{prep_add_cause_label_df}
}
