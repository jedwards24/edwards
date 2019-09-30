#########################################################################################
# count_nas: counts NAs in a data frame by column.
#########################################################################################
#'
#' Counts NAs in a data frame by column.
#'
#' Returns a vector of the number or proportion of NAs of each variable in a data frame.
#'
#' @param df A data frame.
#'
#' @param prop By default the count os NAs is returned. Set to TRUE instead return a proportion.
#' @param all By default variables with no NAs are omitted from the output. Set to TRUE to show all.
#'
#' @export
count_nas <- function(df, prop = FALSE, all = FALSE) {
  if (!is.list(df)) {
    stop("`df` must be a list.", call. = FALSE)
  }
  vals <- vapply(df, function(x) sum(is.na(x)), integer(1))
  if (prop) {
    vals <- vals / dim(df)[1]
  }
  if(max(vals) == 0){
    cat("There are no NAs in the data.\n")
  }else{
    vals[vals > 0 | all]
  }
}

#########################################################################################
# count_unique: counts the number of unique values in a data frame by column.
#########################################################################################
#'
#' Counts the number of unique values in a data frame by column.
#'
#' Returns a vector of the number of unique values of each variable in a data frame. Any NA entries are included as
#'   a unique value.
#'
#' @param df A data frame.
#'
#' @export
count_unique <- function(df) {
  if (!is.list(df)) {
    stop("`df` must be a list.", call. = FALSE)
  }
  vapply(df, function(x) length(unique(x)), integer(1))
}

#########################################################################################
# count_levels: counts the total number of levels in a data frame by column.
#########################################################################################
#'
#' Counts the number of levels in a data frame by column.
#'
#' Returns a vector of the number of levels of each variable in a data frame.
#'
#' @param df A data frame.
#' @param all By default variables with no levels are omitted from the output. Set all=T to show all.
#'
#' @export
count_levels <- function(df, all = FALSE) {
  if (!is.list(df)) {
    stop("`df` must be a list.", call. = FALSE)
  }
  vals <- vapply(df, nlevels, integer(1))
  vals[vals > 0 | all]
}

#########################################################################################
# count_nas2: counts NAs in a data frame by column. Returns a tbl.
#########################################################################################
#'
#' Counts NAs in a data frame by column.
#'
#' @description
#' Unlike `count_nas()` this returns the results in a tibble with columns giving:
#'
#' * The number of NAs for each variable in the data.
#' * The number of NAs as a proportion of rows.
#' * The class of each variable.
#'
#' @param df A data frame.
#'
#' @param all By default variables with no NAs are omitted from the output. Set to TRUE to show all.
#' @param sort By default the output table is sorted by descending number of NAs. Set to FALSE to keep
#'   variable ordering as in the data.
#'
#' @export
count_nas2 <- function(df, all = FALSE, sort = TRUE) {
  if (!is.data.frame(df)) {
    stop("`df` must be a data frame.", call. = FALSE)
  }
  nas <- vapply(df, function(x) sum(is.na(x)), integer(1))
  if (max(nas) == 0) {
    cat("There are no NAs in the data.\n")
  } else{
    tibble::tibble(
      variable = names(df),
      nas = nas,
      prop = nas / nrow(df),
      class = vapply(dt, function(x) class(x)[1], character(1))
    ) %>%
      dplyr::filter((nas > 0) | all) %>%
      `if`(sort, arrange(., desc(nas)), .)
  }
}

#########################################################################################
# count_string: Counts the total number of string pattern matches in a data frame by column.
#########################################################################################
#'
#' Counts the total number, by column, of entries in a data frame that match a string pattern.
#'
#' Returns a named integer vector with elements that give the number of entries in the corresponding
#' column of \code{df} that contain a match to the string pattern \code{pattern}.
#'
#' Note that repeated occurences of \code{pattern} in a single string are only counted once (see examples).
#'
#' @param df A data frame.
#' @param pattern A string pattern (regular expression).
#' @param all By default variables with no matches are omitted from the output. Set all=T to show all.
#' @examples
#' x <- data.frame(a = c("an", "banana", "candy"), b = c("on", "bon", "bonbon"))
#' count_string(x, "an", all = T)
#' count_string(x, "an")
#' count_string(x, "b")
#'
#' @export
count_string <- function(df, pattern, all = FALSE){
  if (!is.list(df)) {
    stop("`df` must be a list.", call. = FALSE)
  }
  vals <- vapply(df, function(x) sum(stringr::str_detect(x, pattern), na.rm = T), integer(1))
  if(length(vals) == 0){
    cat("String not found in data.\n")
  }else{
    vals[vals > 0 | all]
  }
}

#########################################################################################
# var_summary: Simple summary of the variables in a data frame.
#########################################################################################
#'
#' Simple summary of the variables in a data frame.
#'
#' Returns a tibble with the names, class, number of NAs, number of unique values, and the number of
#'   levels for each variable in the data. Any NA entries are included as a unique value.
#'
#' @param df A data frame.
#'
#' @export
var_summary <- function(df) {
  if (!is.data.frame(df)) {
    stop("`df` must be a data frame.", call. = FALSE)
  }
  tibble::tibble(var = names(df),
                 index = 1:ncol(df),
                 class = vapply(df, function(x) class(x)[1], character(1)),
                 missing = vapply(df, function(x) sum(is.na(x)), integer(1)),
                 unique = vapply(df, function(x) length(unique(x)), integer(1)),
                 levels = vapply(df, nlevels, integer(1))
  )
}

#########################################################################################
# count_at: Performs dplyr::count for a range of variables in a data frame.
#########################################################################################
#'
#' Performs dplyr::count for a range of variables in a data frame.
#'
#' Prints output from dplyr::count for each variable index given by argument `cols` (an integer vector).
#'
#' @param df A data frame.
#' @param cols Vector of integer indices. If not given counts for all columns are given.
#' @param sort Boolean passed to `count()` to say whether results are sorted by descending number of observation..
#'   Defaults to TRUE (unlike `count()`).
#' @param n Integer passed to `print()` which gives the maximum number of rows printed in each count summary.
#'
#' @export
count_at <- function(df, cols = NULL, sort = TRUE, n = 10) {
  if (is.null(cols)) cols <- 1 : ncol(df)
  if (!is.data.frame(df)) {
    stop("`df` must be a data frame.", call. = FALSE)
  }
  if (!all(cols %in% 1 : ncol(df))) {
    stop("Values in `cols` must match column numbers in `df`", call. = FALSE)
  }
  for(name in names(df)[cols]){
    print(count(df, !!as.name(name), sort = sort), n = n)
  }
  invisible(df)
}

