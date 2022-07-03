#' Simple summary of the variables in a data frame
#'
#' A summary tibble of the columns of a data frame. Each row in the output corresponds to a column
#' in `df`.
#'
#' @param df A data frame.
#' @param na_strings Character vector to match for the `str_missing` column.
#' @return A tibble with columns as follows.
#'  \item{var}{The name of the column in `df`.}
#'  \item{index}{The position of the column in `df`.}
#'  \item{class}{The first element of the class of the column.}
#'  \item{unique}{The number of unique values in the column. Note that `NA` and `NaN`
#'    are counted as distinct value in this.}
#'  \item{missing}{The number of missing values in the column (`NA` or `NaN`).}
#'  \item{pct_missing}{The percentage of values in the column that are missing.}
#'  \item{str_missing}{The number of matches to strings in the `na_strings` argument. See
#'    `count_matches()` for details of how the count is done.}
#' @export
var_summary <- function(df, na_strings = string_missing()) {
  if (!is.data.frame(df)) {
    stop("Argument `df` must be a data frame.", call. = FALSE)
  }
  if (ncol(df) == 0) {
    message("The data frame has zero columns.")
  }
  if (!is.character(na_strings)) {
    stop("Argument `na_strings` must be a character vector", call. = FALSE)
  }
  tibble(var = names(df),
         index = seq_along(df),
         class = vapply(df, function(x) class(x)[1], character(1), USE.NAMES = FALSE),
         unique = vapply(df, dplyr::n_distinct, integer(1), USE.NAMES = FALSE),
         missing = vapply(df, function(x) sum(is.na(x)), integer(1), USE.NAMES = FALSE),
         pct_miss = 100 * missing / nrow(df),
         str_missing = vapply(df, total_matches_vec, integer(1), value = na_strings, USE.NAMES = FALSE)
  )
}

#' Count NAs in a data frame by column
#'
#' Returns a vector of the number of NAs of each variable in a data frame.
#'
#' @param df A data frame.
#' @param all By default variables with no NAs are omitted from the output. Set to `TRUE` to show all.
#'
#' @export
count_nas <- function(df, all = FALSE) {
  if (!is.list(df)) {
    stop("Argument `df` must be a list.", call. = FALSE)
  }
  vals <- vapply(df, function(x) sum(is.na(x)), integer(1))
  vals <- vals[vals > 0 | all]
  if(length(vals) == 0){
    message("There are no NAs in the data.")
    invisible(vals)
  }else{
    vals
  }
}

#' Count the number of unique values in a data frame by column
#'
#' Returns a vector of the number of unique values of each variable in a data frame. Any NA entries are included as
#' a unique value.
#'
#' @param df A data frame.
#'
#' @export
count_unique <- function(df) {
  if (!is.list(df)) {
    stop("Argument `df` must be a list.", call. = FALSE)
  }
  vapply(df, function(x) length(unique(x)), integer(1))
}

#' Count the number of levels in a data frame by column
#'
#' Returns a vector of the number of levels of each variable in a data frame.
#'
#' @param df A data frame.
#' @param all By default variables with no levels are omitted from the output. Set `all=TRUE` to show all.
#'
#' @export
count_levels <- function(df, all = FALSE) {
  if (!is.list(df)) {
    stop("Argument `df` must be a list.", call. = FALSE)
  }
  vals <- vapply(df, nlevels, integer(1))
  vals <- vals[vals > 0 | all]
  if(length(vals) == 0){
    message("There are no factors in the data.")
    invisible(vals)
  }else{
    vals
  }
}
