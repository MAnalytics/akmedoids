% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/outlier_detect.R
\name{outlier_detect}
\alias{outlier_detect}
\title{Outlier detection and replacement}
\usage{
outlier_detect(traj, id_field = FALSE, method = 1, threshold = 0.95,
count = 1, replace_with = 1, verbose=TRUE)
}
\arguments{
\item{traj}{[matrix (numeric)]: longitudinal data. Each row
represents an individual trajectory (of observations). The columns
show the observations at consecutive time points.}

\item{id_field}{[numeric or character] Whether the first column
of the \code{traj} is a unique (\code{id}) field.
Default: \code{FALSE}. If \code{TRUE} the function recognizes
the second column as the first time step.}

\item{method}{[integer (numeric)] indicating the method for
identifying the outlier. Options are: \code{'1'}: quantile method
(\code{default}), and \code{'2'}: manual method. The \code{manual}
method requires a user-defined value.}

\item{threshold}{[numeric] A cut-off value for outliers. If the
\code{method} parameter is set as \code{'1'}:quantile, the \code{threshold}
should be a numeric vector of probability between \code{[0,1]}, whilst if
the \code{method} is set as \code{'2'}: \code{manual}, the
\code{threshold} could be any numeric vector.}

\item{count}{[integer (numeric)] indicating the number of observations
(in a trajectory) that must exceed the \code{threshold} in order for the
trajectory to be considered an \code{outlier}. Default is \code{1}.}

\item{replace_with}{[integer (numeric)] indicating the technique to
use for calculating a replacement for an outlier observation. The remaining
observations on the row or the column in which the outlier observation is
located are used to calculate the replacement.
The replacement options are: \code{'1'}: Mean value of the column,
\code{'2'}: Mean value of the row and \code{'3'}: remove the row
(trajectory) completely from the data. Default value is the
\code{'1'} option.}

\item{verbose}{to suppress output messages (to the console).
Default: \code{TRUE}.}
}
\value{
A dataframe with outlier observations replaced or removed.
}
\description{
This function identifies outlier observations
in the trajectories, and allows users to replace the observations
or remove trajectories entirely.
}
\details{
Given a matrix, this function identifies outliers that
exceed the threshold and replaces the outliers with an estimate
calculated using the other observations either the rows or the columns
in which the outlier observation is located. Option is also provided to
remove the trajectories (containing the outlier) from the data.
}
\examples{

data(traj)

trajectry <- data_imputation(traj, id_field=TRUE, method = 1,
   replace_with = 1, verbose=FALSE)

trajectry <- props(trajectry$CompleteData, id_field=TRUE)

outp <- outlier_detect(trajectry, id_field = TRUE, method = 1,
threshold = 0.95, count = 1, replace_with = 1, verbose=TRUE)

outp <- outlier_detect(trajectry, id_field = TRUE, method = 2, threshold = 15,
  count = 4, replace_with = 3, verbose=TRUE)

}
