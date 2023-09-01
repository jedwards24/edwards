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

#' `dplyr::count()` with proportion column and default `sort = TRUE`
#'
#' Adds a column which gives the proportion of total rows in that group.
#' * `vcount()` is similar but works with a vector input.
#' * `countp()` and `countv()` are alternative names for, respectively, `count2()` and `vcount()`.
#'
#' @param df A data frame.
#' @param ... Variables to group by. Passed to `count()`.
#' @param sort Passed to `count`, but defaults to `TRUE`.
#' @param wt,name Optional, passed to `count`.
#' @param pname  Name for the proportion column. If omitted, it will default to p. If there's
#' already a column called p, it will use pp. If there's a column called p and pp, it'll use ppp,
#' and so on, adding ps until it gets a new name.
#'
#' @examples
#' count2(mtcars, cyl)
#' count2(mtcars, cyl, sort = FALSE)
#' vcount(c(1, 2, 2, 3))
#'
#' @export
count2 <- function(df, ..., sort = TRUE, wt = NULL, name = NULL, pname = NULL) {
  x <- dplyr::count(df, ..., wt = {{wt}}, sort = sort, name = name)
  count_name <- names(x)[ncol(x)]
  pname <- check_p_name(pname, names(x))
  dplyr::mutate(x, !!pname := .data[[count_name]] / sum(.data[[count_name]]))
}

#' @rdname count2
#' @export
countp <- count2

#' @param x A vector (including a data frame).
#' @seealso [vctrs::vec_count()]
#' @rdname count2
#' @export
vcount <- function(x, sort = TRUE, name = NULL, pname = NULL) {
  dat <- tibble::as_tibble_col(x, "value")
  count2(dat, .data$value, sort = sort, name = name, pname = pname)
}

#' @rdname count2
#' @export
countv <- vcount

# helpers ---------
# Similar to functions check_n_name() and n_name() in dplyr. See dplyr/R/count-tally.R
check_p_name <- function(name, vars) {
  if (is.null(name)) {
    name <- p_name(vars)

    if (name != "p") {
      rlang::inform(c(paste0("Storing counts in `", name, "`, as `p` already present in input"),
        i = "Use `name = \"new_name\"` to pick a new name."))
    }
  }
  name
}

p_name <- function(x) {
  name <- "p"
  while (name %in% x) {
    name <- paste0("p", name)
  }
  name
}
