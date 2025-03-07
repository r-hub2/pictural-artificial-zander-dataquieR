% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prep_add_to_meta.R
\name{prep_add_to_meta}
\alias{prep_add_to_meta}
\title{Support function to augment metadata during data quality reporting}
\usage{
prep_add_to_meta(
  VAR_NAMES,
  DATA_TYPE,
  LABEL,
  VALUE_LABELS,
  item_level = "item_level",
  meta_data = item_level,
  meta_data_v2,
  ...
)
}
\arguments{
\item{VAR_NAMES}{\link{character} Names of the Variables to add}

\item{DATA_TYPE}{\link{character} Data type for the added variables}

\item{LABEL}{\link{character} Labels for these variables}

\item{VALUE_LABELS}{\link{character} Value labels for the values of the variables
as usually pipe separated and assigned with
\code{=}: \code{1 = male | 2 = female}}

\item{item_level}{\link{data.frame} the metadata to extend}

\item{meta_data}{\link{data.frame} old name for \code{item_level}}

\item{meta_data_v2}{\link{character} path to workbook like metadata file, see
\code{\link{prep_load_workbook_like_file}} for details.
\strong{ALL LOADED DATAFRAMES WILL BE PURGED},
using \code{\link{prep_purge_data_frame_cache}},
if you specify \code{meta_data_v2}.}

\item{...}{Further defined variable attributes, see
\link{prep_create_meta}}
}
\value{
a data frame with amended metadata.
}
\description{
adds an annotation to static metadata
}
\details{
Add metadata e.g. of transformed/new variable
This function is not yet considered stable, but we already export it,
because it could help. Therefore, we have some inconsistencies in the
formals still.
}
