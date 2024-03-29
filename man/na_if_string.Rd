% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/na_if_string.R
\name{na_if_string}
\alias{na_if_string}
\title{Convert character elements in a data frame to NA}
\usage{
na_if_string(data, na_strings = c(""))
}
\arguments{
\item{data}{A data frame.}

\item{na_strings}{Character vector of strings to convert to missing values.}
}
\value{
A modified version of \code{data} with the same rows and columns.
}
\description{
In all character columns in \code{data} any elements matching any of \code{na_strings} are replaced with \code{NA}.
}
\examples{
df <- tibble::tibble(x1 = c("a", "", "1"),
                 x2 = c("-", "", "b"),
                 x3 = 1:3)
na_if_string(df, c("-", "", "1"))
}
