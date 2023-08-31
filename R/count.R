#' Perform  `count2()` for a range of variables in a data frame
#'
#' Prints output from `count2()` for multiple columns in `df`.
#'
#' @param df A data frame.
#' @param ... Columns to count over. Accepts tidyselect inputs. If omitted then count is applied to every column.
#' @param sort Logical, passed to `count()`. If `TRUE` each result is sorted by group size.
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
#' Adds a column `pname` which gives the proportion of total rows in that group.
#' `countp()` is an alternative name.
#'
#' @param df A data frame.
#' @param ... Variables to group by. Passed to `count()`.
#' @param sort Passed to `count`, but defaults to `TRUE`.
#' @param wt,name Optional, passed to `count`.
#' @param pname Name for the proportion column.
#'
#' @examples
#' count2(mtcars, cyl)
#' count2(mtcars, cyl, sort = FALSE)
#'
#' @export
count2 <- function(df, ..., sort = TRUE, wt = NULL, name = NULL, pname = "prop") {
  if (pname %in% names(df)){
    stop('A column called "', pname, '" already exists in `df`. Choose a different `pname`.',
         call. = FALSE)
  }
  x <- dplyr::count(df, ..., wt = {{wt}}, sort = sort, name = name)
  count_name <- names(x)[ncol(x)]
  dplyr::mutate(x, !!pname := .data[[count_name]] / sum(.data[[count_name]]))
}

#' @rdname count2
#' @export
countp <- count2

#' Frequency count for values of a vector with tibble output.
#'
#' This behaves similarly to `count2()` but with a vector input. `countv()` is an alternative name.
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

#' @rdname vcount
#' @export
countv <- vcount
