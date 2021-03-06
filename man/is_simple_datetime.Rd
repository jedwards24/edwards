% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dates.R
\name{is_simple_datetime}
\alias{is_simple_datetime}
\title{Test if a datetime object has times that are all zero}
\usage{
is_simple_datetime(x)
}
\arguments{
\item{x}{An object to test.}
}
\description{
Returns \code{TRUE} if the input is a datetime object where the time part of every element is zero.
}
\examples{
  x <- as.POSIXct("2009-08-03")
  y <- as.Date(x)
  is_simple_datetime(x) # TRUE
  is_simple_datetime(x + 1) # FALSE
  is_simple_datetime(y) # FALSE

}
