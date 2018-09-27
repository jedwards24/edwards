#########################################################################################
# count_nas: counts NAs in a tibble by column.
#########################################################################################
#'
#' count_nas: counts NAs in a tibble by column.
#'
#' Returns a vector of the number or proportion of NAs of each variable in a tibble.
#'
#' @param tbl a tibble.
#'
#' @param prop=F Set to true to return the number of NAs as a proportion of rows, and count otherwise.
#'
#' @export
count_nas <- function(tbl, prop=F) {
  summarise_all(tbl, funs(sum(is.na(.)))) %>%
    unlist %>%
    `/`(., if(prop){dim(tbl)[1]}else{1})
}

#########################################################################################
# count_unique: counts the number of unique values in a tibble by column.
#########################################################################################
#'
#' Counts NAs in a tibble by column.
#'
#' Returns a vector of the number of unique values of each variable in a tibble.
#'
#' @param tbl a tibble.
#'
#' @export
count_unique <- function(tbl) {
  summarise_all(tbl, funs(n_distinct(.))) %>% unlist
}

#########################################################################################
# count_unique: counts the number of levels in a tibble by column.
#########################################################################################
#'
#' count_nas
#' Counts NAs in a tibble by column.
#'
#' Returns a vector of the number of levels of each variable in a tibble.
#'
#' @param tbl a tibble.
#'
#' @export
count_levels <- function(tbl) {
  summarise_all(tbl, funs(nlevels(.))) %>% unlist
}
