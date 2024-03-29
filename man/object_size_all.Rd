% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{object_size_all}
\alias{object_size_all}
\title{Get sizes, in Mb, of all objects in global environment}
\usage{
object_size_all()
}
\value{
A tibble with two columns: object name and size in Mb.
}
\description{
Returns table of sizes of all objects in the global environment in Mb (1024^2 B), in
decreasing order of size.
}
