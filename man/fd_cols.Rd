% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/db_tools.R
\name{fd_cols}
\alias{fd_cols}
\title{Find all size one candidate functional dependencies of a set of columns.}
\usage{
fd_cols(df, ...)
}
\arguments{
\item{df}{A data frame.}

\item{...}{Columns in determinant set. Given by either by name (quoted or unquoted) or integer positions.}
}
\description{
Returns names of columns in a data frame that may be individually functionally determined a supplied set
of columns (the determinant set). This means that each returned column takes a single value for each unique
combination in the determinant set of columns.
}
\details{
Any \code{NA}s are treated as a distinct value and a warning is given.

This is far from optimised and can be slow with large data frames. The run time is approximately of order
(number of columns not in the determinant set) x (number of unique rows (groups) in the determinant set).
I found that 10,000 groups takes about 0.5 seconds per column.
}
