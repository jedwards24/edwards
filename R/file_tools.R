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
#'   The supplied extension is checked for an exact match (ignoring case) to the last extension in
#'   the file name.
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
   ext <- stringr::str_remove(ext, "^\\.")
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
