#' Simple summary of files in a directory
#'
#' Similar to `fs::dir_info()` but only files, not sub-directories are included, and with fewer
#' columns in the output. Unlike `fs::dir_info()`, `dir` must be length one.
#'
#' @return A data frame with columns as follows.
#'  \item{file_name}{The name of the file, as a character vector.}
#'  \item{ext}{The extension of the file, as a character vector.}
#'  \item{size}{The file size as an [fs_bytes] vector.}
#'  \item{mod_time}{The date and time when the file was last modified.}
#'  \item{directory}{The directory part of the file path, as a `fs::path` vector.}
#'  \item{level}{The level of the directory relative to `dir`}.
#' @param dir Character. A single directory path.
#' @param ... Additional arguments passed to `dir_info()`.
#' @param shorten If `TRUE` (default), the directory column gives the path relative to `dir` (a "."
#' is given when the directory is `dir`). If `FALSE`, the full path is given.
#' @export
dir_files <- function(dir = ".", ..., shorten = TRUE) {
  if (!is.character(dir) || length(dir) > 1){
    stop("`dir` must be a length one character vector.", call. = FALSE)
  }
  dir <- fs::path_abs(dir)
  root_level <- stringr::str_count(dir, "/")
  tbl <- fs::dir_info(path = dir, ...)
  tbl <- tbl %>%
    dplyr::filter(.data$type == "file") %>%
    dplyr::rename(mod_time = "modification_time") %>%
    dplyr::mutate(file = fs::path_file(.data$path)) %>%
    dplyr::mutate(ext = fs::path_ext(.data$path)) %>%
    dplyr::mutate(directory = fs::path(fs::path_dir(.data$path))) %>%
    dplyr::mutate(level = stringr::str_count(.data$directory, "/") - root_level) %>%
    dplyr::select("file", "ext", "size", "mod_time", "directory", "level")
  if (shorten) tbl <- dplyr::mutate(tbl, directory = fs::path_rel(.data$directory, dir))
  attr(tbl, "call") <- match.call()
  tbl
}

#' Summarise the contents of a directory
#'
#' @description
#' These functions give information about the size of directories as well as the number of
#' files and sub-directories they contain (both in the immediate directory and recursively over
#' sub-directories).
#'
#' `dir_contents()` returns a data frame with rows for each directory or sub-directories.
#'
#' `dir_size()`, `dir_count_files()` and `dir_count_dirs()` return atomic vectors of, respectively,
#' the size, the number of files, and the number of directories in `dir`. All information
#' returned by these functions is included in the output of `dir_contents()`, but they may be useful
#' for simplicity or speed.
#'
#' @param dir A character vector of directory paths. For `dir_contents()` this must be length one.
#' @param recurse Passed to `dir_info()`. Defaults to `TRUE` so that all sub-directory contents are
#' included. For `dir_contents()` this only effects which rows are included in the output, not
#' how the counts or totals are calculated.
#' @param ... Additional arguments passed to `dir_info()`.
#' @param shorten If `TRUE` (default), the directory column gives the path relative to `dir` (the
#' `dir` entry is displayed as "."). If `FALSE`, the full path is given.
#' @return `dir_size()` returns an [fs_bytes] vector.
#' @return `dir_count_files()` and `dir_count_dirs()` each return integer vectors.
#' @return `dir_contents()` returns a data frame with columns:
#'  \item{directory}{The path of the directory as an `fs::path`.}
#'  \item{level}{The level of the directory relative to `dir`.}
#'  \item{total_size}{The total size of all files in the directory, including sub-directories.}
#'  \item{total_files}{The total number of all files in the directory, including sub-directories.}
#'  \item{total_dirs}{The total number of all directories in the directory (fully recursive).}
#'  \item{files_size}{The total size of all files in the immediate directory (not sub-directories).}
#'  \item{num_files}{The number of files in the immediate directory.}
#'  \item{num_dirs}{The number of immediate sub-directories of the directory.}
#'  All size columns are [fs_bytes] numeric vectors.
#' @export
dir_contents <- function(dir = ".", recurse = TRUE, ..., shorten = TRUE) {
  if (!is.character(dir) || length(dir) > 1){
    stop("`dir` must be a length one character vector.", call. = FALSE)
  }
  dir <- fs::path_abs(dir)
  tbl <- fs::dir_info(dir, recurse = recurse, ...) %>%
    dplyr::group_by(directory = fs::path(fs::path_dir(.data$path))) %>%
    dplyr::summarise(files_size = sum(.data$size),
                     num_files = sum(.data$type == "file"),
                     num_dirs = sum(.data$type == "directory")) %>%
    dplyr::mutate(total_size = dir_size(.data$directory, recurse = TRUE)) %>%
    dplyr::mutate(total_files = dir_count_files(.data$directory, recurse = TRUE)) %>%
    dplyr::mutate(total_dirs = dir_count_dirs(.data$directory, recurse = TRUE))
  root_level <- stringr::str_count(dir, "/")
  tbl <- dplyr::mutate(tbl, level = stringr::str_count(.data$directory, "/") - root_level) %>%
    dplyr::select("directory", "level", dplyr::contains("total"), dplyr::everything())
  if (shorten) tbl <- dplyr::mutate(tbl, directory = fs::path_rel(.data$directory, dir))
  attr(tbl, "call") <- match.call()
  tbl
}

#' @export
#' @rdname dir_contents
# Total size of a directory.
# This is vectorised to fit in with the fs package functions.
# Note that using `file_size()` is not faster because it calls `dir_info()` anyway.
dir_size <- function(dir = ".", recurse = TRUE, ...) {
  fn <- function(x, ...) sum(fs::dir_info(x, ...)$size)
  sz <- purrr::map_dbl(dir, fn, recurse = recurse, ...)
  fs::fs_bytes(sz)
}

#' @export
#' @rdname dir_contents
# Total number of files in a directory.
dir_count_files <- function(dir = ".", recurse = TRUE, ...) {
  purrr::map_int(dir, count_files, recurse = recurse, ...)
}

#' @export
#' @rdname dir_contents
# Total number of files in a directory.
dir_count_dirs <- function(dir = ".", recurse = TRUE, ...) {
  purrr::map_int(dir, count_dirs, recurse = recurse, ...)
}

#' @noRd
# Helper for `dir_count_files()` (non-vectorised version).
count_files <- function(dir, recurse, ...) {
  fs::dir_ls(dir, recurse = recurse, ...) %>%
    fs::is_file() %>%
    sum()
}

#' @noRd
# Helper for `dir_count_dirs()` (non-vectorised version).
count_dirs <- function(dir, recurse, ...) {
  fs::dir_ls(dir, recurse = recurse, ...) %>%
    fs::is_dir() %>%
    sum()
}
