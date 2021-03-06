% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dates.R
\name{convert_date}
\alias{convert_date}
\title{Parse a character vector in a particular format and convert to a date or datetime}
\usage{
convert_date(x)
}
\arguments{
\item{x}{A character vector to be converted.}
}
\description{
Reads text vectors which are either in dmy or dmy:time format. If all of the times are
zeroes then the output is a date. Fully  \code{NA} vectors are returned as dates.
}
