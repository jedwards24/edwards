#########################################################################################
# count_nas: counts NAs in a data frame by column.
#########################################################################################
#'
#' count_nas: counts NAs in a data frame by column.
#'
#' Returns a vector of the number or proportion of NAs of each variable in a data frame.
#'
#' @param df a data frame.
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
#' Returns a vector of the number of unique values of each variable in a data frame.
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
