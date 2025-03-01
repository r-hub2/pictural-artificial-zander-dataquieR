% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/con_limit_deviations.R
\name{con_limit_deviations}
\alias{con_limit_deviations}
\title{Detects variable values exceeding limits defined in metadata}
\usage{
con_limit_deviations(
  resp_vars = NULL,
  study_data,
  label_col,
  item_level = "item_level",
  limits = NULL,
  flip_mode = "noflip",
  return_flagged_study_data = FALSE,
  return_limit_categorical = TRUE,
  meta_data = item_level,
  meta_data_v2,
  show_obs = TRUE
)
}
\arguments{
\item{resp_vars}{\link{variable list} the name of the measurement variables}

\item{study_data}{\link{data.frame} the data frame that contains the measurements}

\item{label_col}{\link{variable attribute} the name of the column in the metadata
with labels of variables}

\item{item_level}{\link{data.frame} the data frame that contains metadata
attributes of study data}

\item{limits}{\link{enum} HARD_LIMITS | SOFT_LIMITS | DETECTION_LIMITS. what
limits from metadata to check for}

\item{flip_mode}{\link{enum} default | flip | noflip | auto. Should the plot be
in default orientation, flipped, not flipped or
auto-flipped. Not all options are always supported.
In general, this con be controlled by
setting the \code{roptions(dataquieR.flip_mode = ...)}. If
called from \code{dq_report}, you can also pass
\code{flip_mode} to all function calls or set them
specifically using \code{specific_args}.}

\item{return_flagged_study_data}{\link{logical} return \code{FlaggedStudyData} in the
result}

\item{return_limit_categorical}{\link{logical} if TRUE return limit deviations also
for categorical variables}

\item{meta_data}{\link{data.frame} old name for \code{item_level}}

\item{meta_data_v2}{\link{character} path to workbook like metadata file, see
\code{\link{prep_load_workbook_like_file}} for details.
\strong{ALL LOADED DATAFRAMES WILL BE PURGED},
using \code{\link{prep_purge_data_frame_cache}},
if you specify \code{meta_data_v2}.}

\item{show_obs}{\link{logical} Should (selected) individual observations be marked
in the figure for continuous variables?}
}
\value{
a list with:
\itemize{
\item \code{FlaggedStudyData} \link{data.frame} related to the study data by a 1:1
relationship, i.e. for each observation is
checked whether the value is below or above
the limits. Optional, see
\code{return_flagged_study_data}.
\item \code{SummaryTable} \link{data.frame} summarizing limit deviations for each
variable.
\item \code{SummaryData} \link{data.frame} summarizing limit deviations for each
variable for a report.
\item \code{SummaryPlotList} \link{list} of \link[ggplot2:ggplot]{ggplot2::ggplot}s The plots for each variable are
either a histogram (continuous) or a
barplot (discrete).
\item \code{ReportSummaryTable}: heatmap-like data frame about limit violations
}
}
\description{
Inadmissible numerical values can be of type integer or float. This
implementation requires the definition of intervals in the metadata to
examine the admissibility of numerical study data.

This helps identify inadmissible measurements according to
hard limits (for multiple variables).

\link{Indicator}
}
\details{
\subsection{Algorithm of this implementation:}{
\itemize{
\item Remove missing codes from the study data (if defined in the metadata)
\item Interpretation of variable specific intervals as supplied in the metadata.
\item Identification of measurements outside defined limits. Therefore two
output data frames are generated:
\itemize{
\item on the level of observation to flag each deviation, and
\item a summary table for each variable.
}
\item A list of plots is generated for each variable examined for limit
deviations. The histogram-like plots indicate respective limits as well
as deviations.
\item Values exceeding limits are removed in a data frame of modified study data
}
}
}
\seealso{
\itemize{
\item \href{https://dataquality.qihs.uni-greifswald.de/VIN_con_impl_limit_deviations.html}{Online Documentation}
}
}
