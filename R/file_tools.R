
#########################################################################################
# latest_file: Get file name of most recent file in a directory
#########################################################################################
#'
#' Get file name of most recent file in a directory
#'
#' Looks for files in a directory `path` with names beginning `"root_name_dd"`, where
#' dd are digits. Returns the full path of the file with the maximum numeric part of its name.
#' This maximum is decided alphabetically so the numeric part of the file names should be structured
#' similarly across the compared files e.g. a two digit version number or date in yyyy-mm-dd form.
#'
#' The string `root_name` can be a regex pattern. Lower and upper case versions of `file_ext`
#' will also be matched.
#'
#' @param path A string path name for the directory to search in.
#' @param root_name A string giving the part of the file name before the date.
#' @param file_ext An optional string giving a file extension which the returned file name must end in.
#' @param verbose If `TRUE` will give detail about all matched files and other files containing `root_name`.
#' @param silent If `TRUE` will suppress all messages.
#' @param n Integer. Option to return the nth latest file.
#'
#' @export
latest_file <- function(path = ".", root_name=".*", file_ext = NULL, verbose = FALSE, silent = FALSE, n = 1L) {
  if (!is.numeric(n)) stop("`n` must be numeric.", call. = FALSE)
  if (!dir.exists(path)){
    stop(paste0("Directory ", path, " does not exist."), call. = FALSE)
  }
  files_match <- list.files(path, pattern = paste0("^", root_name, "_\\d{2}"))
  if (!is.null(file_ext)){
    patt <- paste0(file_ext, "$|", stringr::str_to_lower(file_ext), "$|", stringr::str_to_upper(file_ext), "$")
    files_match <- stringr::str_subset(files_match, pattern = patt)
  }
  if(length(files_match) == 0){
    stop("No matching files found in that directory.", call. = FALSE)
  }
  if (verbose){
    message('Filenames found containing "', root_name, '_dd":\n', paste0(sort(files_match), sep = "\n"))
    other_matches <- setdiff(stringr::str_subset(list.files(path), root_name), files_match)
    if (length(other_matches) > 0){
      warning('There are other filenames containing "', root_name, '" that are ignored for latest file:\n',
              paste0(sort(other_matches), sep = "\n"), call. = FALSE)
    }
  }
  chosen <- max_n(files_match, n)
  if(!silent) message("Matched file is ", chosen)
  if (stringr::str_detect(path, "/$")){
    paste0(path, chosen)
  }else{
    paste0(path, "/", chosen)
  }
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
#' The main purpose of these functions is to give information about the size of directories.
#'  `dir_contents()` returns a data frame with rows for each directory or sub-directories.
#'  `dir_size()` returns a `fs::fs_bytes()` numeric vector of the total size the directories, including
#' the contents of sub-directories.
#'
#' @param dir A character vector of one or more directory paths.
#' @param ... Additional arguments passed to `dir_info()`.
#' @return The data frame returned by `dir_contents()` has columns as follows.
#'  \item{directory}{The path of the directory.}
#'  \item{files_size}{The total size of all files the directory, as a `fs::fs_bytes()` numeric vector.}
#'  \item{num_files}{The number of files the immediate directory.}
#'  \item{num_dirs}{The number of immediate sub-directories of the directory.}
#'  \item{total_size}{The total size of all files the directory, including sub-directories.}
#' @export
dir_contents <- function(dir = ".", ...) {
  fs::dir_info(dir, recurse = TRUE) %>%
    dplyr::group_by(directory = fs::path_dir(.data$path)) %>%
    dplyr::summarise(files_size = sum(.data$size),
                     num_files = sum(.data$type == "file"),
                     num_dirs = sum(.data$type == "directory")) %>%
    dplyr::mutate(total_size = dir_size(.data$directory))
}

#' @export
#' @rdname dir_contents
# Total size of a directory, including the contents of sub-directories
# This is vectorised to fit in with the fs package functions.
# Note that using file_size() is not faster because it calls dir_info() anyway.
dir_size <- function(dir = ".") {
  fs::fs_bytes(purrr::map_dbl(dir, ~sum(fs::dir_info(., recurse = TRUE)$size)))
}
