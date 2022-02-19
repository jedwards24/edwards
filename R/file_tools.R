#' Get paths of files using file name ordering
#'
#' @description
#' Looks for files in a directory `path` with file name matching `pattern`. The path of the `n`th
#' file, ordered alphabetically by file name is returned. "File name" here refers to the part of
#' the path without any directory names i.e. the part following the final "/".
#'
#' The intended use is with files names with matching structure with well-ordered numeric endings,
#' such as "yyyy-mm-dd" or fixed length numeric ids. Output may not be as intended if
#' comparing files with different structures.
#'
#' @param path A string path name for the directory to search in.
#' @param pattern A string pattern to filter file names by. The pattern is only applied to the part
#'   of the path following the final "/".
#' @param ext An optional string giving a file extension which the returned file name must end in.
#'   The supplied extension is checked for an exact match (ignoring case) to the ".xxx" part of the
#'   file name.
#' @param n Integer. Return the nth file path. This can have length > 1, in which case multiple file
#'   paths are returned. An error will be thrown if there are insufficient matches.
#' @param decreasing Logical passed to `order()`. By default returns the (nth) largest value.
#' @param ... Other arguments passed to `fs:dir_ls()`. The argument `type` is always set to `"file"`
#'   and so must not be supplied. Arguments `regexp` or `glob` are applied to the full file path and
#'   so affect output differently than `pattern`.
#' @export
latest_file <- function(path = ".", pattern = NULL, ext = NULL, n = 1L, decreasing = TRUE, ...) {
  if (!is.numeric(n)){
    stop("`n` must be numeric.", call. = FALSE)
  }
  if (!rlang::is_character(path, 1L)){
    stop("`path` must be a length one string", call. = FALSE)
  }
  if (!is.null(pattern) && !rlang::is_character(pattern, 1L)){
    stop("`pattern` must be a length one string", call. = FALSE)
  }
  fpaths <- fs::dir_ls(path, type = "file", ...)
  if (!is.null(pattern)){
    fnames <- fs::path_file(fpaths)
    fpaths <- fpaths[grep(pattern, fnames)]
  }
  if (!is.null(ext)){
    fpaths <- fpaths[grep(ext, fs::path_ext(fpaths), ignore.case = TRUE)]
  }
  if (length(fpaths) < max(n)){
    msg <- "There are fewer than `n` matching files.\n"
    if (length(fpaths) == 0){
      msg <- paste0(msg, "No files match.")
    }else{
      msg <- paste0(msg, "Matching file(s):\n", paste0(fpaths, collapse = "\n"))
    }
    stop(msg, call. = FALSE)
  }
  name_order <- order(fs::path_file(fpaths), decreasing = decreasing)
  fpaths[name_order][n]
}

#' Conditionally save file to RDS.
#'
#' Wrapper around `saveRDS()` but checks if file already exists first. If it does
#' then it will only save if `overwrite = TRUE`. A message is given whether the object is
#' saved or not.
#'
#' @param object Object to save.
#' @param file The name of the file where object is to be saved.
#' @param overwrite Will only overwrite existing file if set to `TRUE`.
#' @param ... Other arguments passed to `saveRDS()`
#' @return (invisible) The `file` argument.
#' @export
save_check <- function(object, file, overwrite = FALSE, ...) {
  if (overwrite | !file.exists(file)){
    saveRDS(object, file, ...)
    cli::cli_alert_success("Saved {file}.")
  }else{
    cli::cli_alert_danger("Output not saved. {file} already exists. Set `overwrite = TRUE` to overwrite.")
  }
  invisible(file)
}

#' Simple summary of files in a directory
#'
#' Similar to `fs::dir_info()` but with reduced output. Only files, not sub-directories are included.
#'
#' @return A data frame with columns as follows.
#'  \item{path}{The path of the file, as a [fs_path()] character vector.}
#'  \item{file_name}{The name of the file, as a character vector.}
#'  \item{ext}{The extension of the file, as a character vector.}
#'  \item{mod_date}{The date of last data modification.}
#' @param dir A character vector of one or more directory paths.
#' @param ... Additional arguments passed to `dir_info()`.
#' @export
dir_files <- function(dir = ".", ...) {
  fs::dir_info(dir, ...) %>%
    dplyr::filter(.data$type == "file") %>%
    dplyr::mutate(mod_date = lubridate::as_date(.data$modification_time)) %>%
    dplyr::mutate(file_name = fs::path_file(.data$path)) %>%
    dplyr::mutate(ext = fs::path_ext(.data$path)) %>%
    dplyr::select(dplyr::all_of(c("file_name", "ext", "size", "mod_date")))
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
#' @param dir A character vector of one or more directory paths.
#' @param recurse Passed to `dir_info()`. Defaults to `TRUE` so that all sub-directory contents are
#' included. For `dir_contents()` this only effects which rows are included in the output, not
#' how the counts or totals are calculated.
#' @param ... Additional arguments passed to `dir_info()`.
#' @param shorten If `TRUE` (default), replace `dir` in directory column of the output with `"."`.
#' @return `dir_size()` returns a `fs::fs_bytes` vector.
#' @return `dir_count_files()` and `dir_count_dirs()` each return integer vectors.
#' @return `dir_contents()` returns a data frame with columns:
#'  \item{directory}{The path of the directory.}
#'  \item{files_size}{The total size of all files the directory, as a `fs::fs_bytes()` numeric vector.}
#'  \item{num_files}{The number of files in the immediate directory.}
#'  \item{num_dirs}{The number of immediate sub-directories of the directory.}
#'  \item{total_size}{The total size of all files in the directory, including sub-directories.}
#'  \item{total_files}{The total number of all files in the directory, including sub-directories.}
#'  \item{total_dirs}{The total number of all directories in the directory (fully recursive).}
#'  \item{level}{The level of the directory relative to `dir`.}
#' @export
dir_contents <- function(dir = ".", recurse = TRUE, ..., shorten = TRUE) {
  if(dir == ".") dir <- as.character(fs::path_real(dir)) # to fix `path` output from `dir_info()`
  tbl <- fs::dir_info(dir, recurse = recurse, ...) %>%
    dplyr::group_by(directory = fs::path_dir(.data$path)) %>%
    dplyr::summarise(files_size = sum(.data$size),
                     num_files = sum(.data$type == "file"),
                     num_dirs = sum(.data$type == "directory")) %>%
    dplyr::mutate(total_size = dir_size(.data$directory, recurse = TRUE)) %>%
    dplyr::mutate(total_files = dir_count_files(.data$directory, recurse = TRUE)) %>%
    dplyr::mutate(total_dirs = dir_count_dirs(.data$directory, recurse = TRUE))
  short_directory <- stringr::str_replace(tbl$directory, stringr::fixed(dir), ".")
  tbl <- dplyr::mutate(tbl, level = stringr::str_count(short_directory, "/"))
  if (shorten) tbl <- dplyr::mutate(tbl, directory = short_directory)
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
