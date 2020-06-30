
#########################################################################################
# latest_file: Get file name of most recent file in a directory
#########################################################################################
#'
#' Get file name of most recent file in a directory
#'
#' Looks for files in a directory \code{path} with names beginning \code{"root_name_dd"}, where
#' dd are digits. Returns the full path of the file with the maximum numeric part of its name.
#' This maximum is decided alphabetically so the numeric part of the file names should be structured
#' similarly across the compared files e.g. a two digit version number or date in yyyy-mm-dd form.
#'
#' The string \code{root_name} can be a regex pattern. Lower and upper case versions of `\code{file_ext}
#' will also be matched.
#'
#' @param path A string path name for the directory to search in.
#' @param root_name A string giving the part of the file name before the date.
#' @param file_ext An optional string giving a file extension which the returned file name must end in.
#' @param verbose If \code{TRUE} will give detail about all matched files and other files containing \code{root_name}.
#' @param n Integer. Option to return the nth latest file.
#'
#' @export
latest_file <- function(path = ".", root_name=".*", file_ext = NULL, verbose = FALSE, n = 1L) {
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
  chosen <- edwards::max_n(files_match, n)
  message("Matched file is ", chosen)
  if (stringr::str_detect(path, "/$")){
    paste0(path, chosen)
  }else{
    paste0(path, "/", chosen)
  }
}
