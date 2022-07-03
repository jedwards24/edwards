#' Perform  `count2()` for a range of variables in a data frame
#'
#' Prints output from `count2()` for multiple columns in `df`.
#'
#' @param df A data frame.
#' @param ... Columns to count over. Accepts tidyselect inputs. If omitted then count is applied to every column.
#' @param sort Logical passed to `count()` to say whether results are sorted by descending number of observation.
#' @param n Integer passed to `print()` which gives the maximum number of rows printed in each count summary.
#'
#' @export
count_over <- function(df, ..., sort = TRUE, n = 10L) {
  if (!is.data.frame(df)) stop("Argument \"df\" must be a data frame.", call. = FALSE)
  if (!is.numeric(n)) stop("Argument \"n\" must be an numeric.", call. = FALSE)
  if (!is.logical(sort)) stop("Argument \"sort\" must be a logical", call. = FALSE)
  cols <- names(dplyr::select(df, ...))
  if (length(cols) == 0) cols <- names(df)
  for(name in cols){
    print(count2(df, !!as.name(name), sort = sort), n = n)
  }
  invisible(df)
}

#' Count group sizes by grouped variables
#'
#' Shorthand for `count(count(df, ...), n)`.
#'
#' @param df A data frame.
#' @param ... Variables to group by.
#'
#' @examples
#' count_n(mtcars, disp)
#'
#' @export
count_n <- function(df, ...) {
  dplyr::count(dplyr::count(df, ...), .data$n, name = "freq")
}

#' `dplyr::count()` with proportion column and default `sort = TRUE`.
#'
#' Adds a column "prop" which gives the proportion of total rows in that group.
#'
#' @param df A data frame.
#' @param ... Variables to group by. Passed to `count()`.
#' @param sort Passed to `count`, but defaults to `TRUE`.
#' @param wt,name Optional, passed to `count`.
#'
#' @examples
#' count2(mtcars, cyl)
#' count2(mtcars, cyl, sort = FALSE)
#'
#' @export
count2 <- function(df, ..., sort = TRUE, wt = NULL, name = NULL) {
  x <- dplyr::count(df, ..., wt = {{wt}}, sort = sort, name = name)
  count_name <- names(x)[ncol(x)]
  dplyr::mutate(x, prop = .data[[count_name]] / sum(.data[[count_name]]))
}

#' Frequency count for values of a vector with tibble output.
#'
#' This behaves similarly to `count2()` but with a vector input.
#'
#' @param x An atomic vector.
#' @param sort Logical. Sort output by descending frequency. If `FALSE` sort by value.
#' @param name Name of count column in output. If omitted `n` will be used (as in `dplyr::count`).
#' @param value_name Name of value column.
#'
#' @export
vcount <- function(x, sort = TRUE, name = NULL, value_name = "value") {
  if (!is.null(name) && (value_name == name)){
    stop("`name` and `value_name` must not be the same.", call. = FALSE)
  }
  res <- tibble::as_tibble_col(x, value_name) %>%
    dplyr::count(.data[[value_name]], sort = sort, name = name)
  count_name <- names(res)[ncol(res)]
  dplyr::mutate(res, prop = .data[[count_name]] / sum(.data[[count_name]]))
}
