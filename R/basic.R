#########################################################################################
# mode_stat: Calculate mode of a vector.
#########################################################################################
#'
#' Calculate the statistical mode of an atomic vector
#'
#' Returns the statistical mode of an atomic vector (can be numeric, character or factor).
#'
#' From https://stackoverflow.com/questions/2547402/is-there-a-built-in-function-for-finding-the-mode.
#'
#' @param x An atomic vector.
#' @param multiple Boolean. If TRUE returns a vector of modes, or just the first otherwise.
#' @param na.rm Boolean. Indicates whether to ignore NAs.
#'
#' @export
mode_stat <- function(x, multiple = TRUE, na.rm = TRUE) {
  if (!is.atomic(x)) {
    stop("`x` must be atomic.", call. = FALSE)
  }
  if(na.rm){
    x <- na.omit(x)
  }
  ux <- unique(x)
  freq <- tabulate(match(x, ux))
  mode_loc <- if(return_multiple) which(freq == max(freq)) else which.max(freq)
  ux[mode_loc]
}

#########################################################################################
# factor_to_numeric: Convert a numeric factor to a numeric vector.
#########################################################################################
#'
#' Convert a numeric factor to a numeric vector
#'
#' The obvious \code{as.numeric()} is incorrect.
#'
#' From  https://stackoverflow.com/questions/3418128/how-to-convert-a-factor-to-integer-numeric-without-loss-of-information
#'
#' @param x A numeric factor.
#'
#' @export
factor_to_numeric <- function(x) {
    as.numeric(levels(x))[x]
}

#########################################################################################
# ilogit: Vectorised inverse logit function.
#########################################################################################
#'
#' Vectorised inverse logit function
#'
#' Calculate the inverse logit function \eqn{exp(x) / (1 + exp(x))}. Modified from Faraway package.
#' The original allowed for some elements to be \code{NA} which this does not. I added a check
#' on \code{x} to avoid \code{NaN}. This occurs when \code{x} is greater than about
#' 750 but, since \eqn{exp(x) / (1 + exp(x)) = 1} for \eqn{x>=20}, I only check for x > 20 and
#' output 1 for these cases.
#'
#' @param x A numeric vector.
#'
#' @export
ilogit <- function (x){
  if (any(big <- (x > 20))) {
    lv <- x
    lv[big] <- 1
    if (any(!big))
      lv[!big] <- Recall(x[!big])
    return(lv)
  }
  exp(x) / (1 + exp(x))
}

#########################################################################################
# prinf: Shorthand for print(n = Inf)
#########################################################################################
#'
#' Print all rows of a tibble
#'
#' Shorthand for \code{print_tbl(x, n = Inf)}. Ok, I'm lazy.
#'
#' @param x A tibble.
#'
#' @export
prinf <- function(x) {
  if (tibble::is_tibble(x)){
    print(x, n = Inf)
  }else{
    print(x)
  }
  invisible(x)
}

#########################################################################################
# latest_file: Get file name of most recent file in a directory
#########################################################################################
#'
#' Get file name of most recent file in a directory
#'
#' Looks for files in a directory \code{path} with names beginning \code{"root_name_dd"}, where
#' dd are digits, then returns the full path of the file with the maximum numeric part of its name.
#' This maximum is decided alphabetically so the numeric part of the file names should be structured
#' similarly across the compared filed e.g. a two digit version number or date in yyyy-mm-dd form.
#'
#' @param path A string path name for the directory to search in.
#' @param root_name A string giving the part of the file name before the date.
#'
#' @export

latest_file <- function(path, root_name) {
  if (!dir.exists(path)){
    stop(paste0("Directory ", path, " does not exist."), call. = FALSE)
  }
  files_match <- stringr::str_subset(list.files(path), paste0(root_name, "_[\\d]{2}"))
  if(length(files_match) == 0){
    stop("No matching files found in that directory.", call. = FALSE)
  }
  message("Files found matching root_name:\n", paste0(files_match, sep = "\n"))
  paste0(path, max(files_match))
}
