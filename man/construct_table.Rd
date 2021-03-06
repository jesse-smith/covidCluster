% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/table.R
\name{construct_table}
\alias{construct_table}
\title{Create One-Way Table}
\usage{
construct_table(
  .data,
  .by,
  infreq = NULL,
  to_na = c("unknown", "missing", "NA", "N/A", "<NA>", "^$"),
  show_missing_levels = FALSE
)
}
\arguments{
\item{.data}{A \link[base:data.frame]{data frame}, data frame
extension (e.g. a \link[tibble:tbl_df]{tibble}), or a lazy data frame (e.g.
from dbplyr or dtplyr)}

\item{.by}{The variable in \code{.data} to analyze; can be specified as a
normal variable or as a string}

\item{infreq}{Should the output be ordered by frequency? The default depends
on the input type; see details.}

\item{show_missing_levels}{Should all levels be shown, even if empty?}
}
\value{
A tibble holding the summary table
}
\description{
\code{construct_table()} summarizes a given variable in a one-way table with
percentages. It is mostly a wrapper around \code{\link[janitor]{tabyl}()}
that allows more flexibility in ordering the output table.
}
\details{
By default, \code{construct_table()} will order factor inputs by their level
and all other input by frequency. If \code{infreq = TRUE}, it will all input by
frequency; if \code{infreq = FALSE}, it will order all input alpha-numerically.
Note that the \code{.by} variable will be converted to a factor with levels
ordered according to the output table, regardless of input type or ordering.
}
\keyword{internal}
