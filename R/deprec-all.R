
#########################################################################################
# count_nas2: counts NAs in a data frame by column. Returns a tbl.
#########################################################################################
#'
#' Count NAs in a data frame by column (depreciated)
#'
#' @description
#' Unlike `count_nas()` this returns the results in a tibble with columns giving:
#' \itemize{
#' \item{The number of NAs for each variable in the data.}
#' \item{The number of NAs as a proportion of rows.}
#' \item{The class of each variable.}
#' }
#'
#' @param df A data frame.
#'
#' @param all By default variables with no NAs are omitted from the output. Set to `TRUE` to show all.
#' @param sort By default the output table is sorted by descending number of NAs. Set to `FALSE` to keep
#'   variable ordering as in the data.
#'
#' @export
count_nas2 <- function(df, all = FALSE, sort = TRUE) {
  warning("`count_nas2() is depreciated. Use `var_summary().", call. = FALSE)
  if (!is.data.frame(df)) {
    stop("Argument \"df\" must be a data frame.", call. = FALSE)
  }
  nas <- vapply(df, function(x) sum(is.na(x)), integer(1))
  if (max(nas) == 0) {
    message("There are no NAs in the data")
  } else{
    tb <- tibble::tibble(
      variable = names(df),
      nas = nas,
      prop = nas / nrow(df),
      class = vapply(df, function(x) class(x)[1], character(1))
    ) %>%
      dplyr::filter((nas > 0) | all)
    if (sort) dplyr::arrange(tb, dplyr::desc(nas))
  }
}


#########################################################################################
# count_at: Performs dplyr::count for a range of variables in a data frame.
#########################################################################################
#'
#' Perform  `dplyr::count` for a range of variables in a data frame (DEPRECIATED - use `count_over()`)
#'
#' Prints output from `dplyr::count()` for each variable index given by argument `cols` (an integer vector).
#'
#' @param df A data frame.
#' @param cols Vector of integer indices. If missing, all columns are included.
#' @param sort Logical passed to `count()` to say whether results are sorted by descending number of observation.
#'   Unlike in `count()`, this defaults to `TRUE`.
#' @param n Integer passed to `print()` which gives the maximum number of rows printed in each count summary.
#'
#' @export
count_at <- function(df, cols = NULL, sort = TRUE, n = 10) {
  if (is.null(cols)) cols <- 1 : ncol(df)
  if (!is.data.frame(df)) {
    stop("Argument \"df\" must be a data frame.", call. = FALSE)
  }
  if (!all(cols %in% 1 : ncol(df))) {
    stop("Values in \"cols\" must match column numbers in \"df\"", call. = FALSE)
  }
  for(name in names(df)[cols]){
    print(dplyr::count(df, !!as.name(name), sort = sort), n = n)
  }
  warning("`count_at() is depreciated. Use `count_over()` instead.", call. = FALSE)
  invisible(df)
}


#' Depreciated. Use `bin_integer()` instead.
#'
#' @inherit bin_integer
#'
bin_numeric <- function(x, cuts, na_level = NULL, na_at_end = FALSE){
  warning("`bin_numeric()` has been depreciated. Use `bin_integer()` instead.", call. = FALSE)
  bin_integer(x = x, cuts = cuts, na_level = na_level, na_at_end = na_at_end)
}
