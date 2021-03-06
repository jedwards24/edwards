% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/file_tools.R
\name{dir_files}
\alias{dir_files}
\title{Simple summary of files in a directory}
\usage{
dir_files(dir = ".", ...)
}
\arguments{
\item{dir}{A character vector of one or more directory paths.}

\item{...}{Additional arguments passed to \code{dir_info()}.}
}
\value{
A data frame with columns as follows.
\item{path}{The path of the file, as a \code{\link[=fs_path]{fs_path()}} character vector.}
\item{file_name}{The name of the file, as a character vector.}
\item{ext}{The extension of the file, as a character vector.}
\item{mod_date}{The date of last data modification.}
}
\description{
Similar to \code{fs::dir_info()} but with reduced output. Only files, not sub-directories are included.
}
