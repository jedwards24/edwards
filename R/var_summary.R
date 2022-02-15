#' Simple summary of the variables in a data frame
#'
#' Returns a tibble with the names, class, number of unique values, and the number and percent of
#' `NA`s for each variable in the data. If there are `NA` values then they are included as
#' a unique value.
#'
#' @param df A data frame.
#'
#' @export
var_summary <- function(df) {
  if (!is.data.frame(df)) {
    stop("Argument \"df\" must be a data frame.", call. = FALSE)
  }
  if (ncol(df) == 0) {
    message("The data frame has zero columns.")
  }
  tibble::tibble(var = names(df),
                 index = seq_along(df),
                 class = vapply(df, function(x) class(x)[1], character(1)),
                 unique = vapply(df, function(x) length(unique(x)), integer(1)),
                 missing = vapply(df, function(x) sum(is.na(x)), integer(1)),
                 pct_miss = 100 * missing / nrow(df)
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
    stop("`df` must be a list.", call. = FALSE)
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
    stop("Argument \"df\" must be a list.", call. = FALSE)
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
    stop("Argument \"df\" must be a list.", call. = FALSE)
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
