

#########################################################################################
# my_kable: Version of kable with standard styling to save typing.
#########################################################################################
#'
#' my_kable: Version of kable with standard styling to save typing.
#'
#' Returns a table for R Markdown with kableExtra::kable_styling options `bootstrap_options = c("striped", "condensed")`
#'   and `full_width = F`. The number of digits is set using the argument `digits` (defaults to 3).
#'
#' @param df A data frame.
#' @param digits=3 Sets the number of digits (via the `kable()` `digits` argument).
#'
#' @export
my_kable <- function(df, digits = 3){
  if (!is.data.frame(df)) {
    stop("`df` must be a data frame.", call. = FALSE)
  }
  tb <- knitr::kable(df, digits = digits)
  tb <- kableExtra::kable_styling(tb, bootstrap_options = c("striped", "condensed"), full_width = F)
  return(tb)
}

#########################################################################################
# split_kable: Split a table in into `n` parts of balanced size.
#########################################################################################
#'
#' split_kable: Split a table in into parts of balanced size.
#'
#' Splits a table into a `n_tb` parts of similar sizing using standard styling in `my_kable()`. This is
#'   used when I want to split a long thin table across multiple columns to reduce a document's size. Returns
#'   a single table part. Which part given by argument `index`.
#'
#' Printing all table parts in one function call didn't work since I couldn't get
#'   `kable()` to print from within a function.
#'
#' @param df A data frame.
#' @param index Which part of the split table to return.
#' @param n_tb=2 The number of parts to split the table into.
#' @param digits=3 Sets the number of digits (via the `kable()` `digits` argument).
#'
#' @export
split_kable <- function(df, index, n_tb = 2, digits = 3) {
  nn <- dim(df)[1]
  end <- 0
  for (i in 1 : index){
    start <- end + 1
    rem <- nn - start + 1
    end <- start + ceiling(rem / (n_tb - i + 1)) - 1
  }
  tb <- slice(df, start : end)
  return(my_kable(tb, digits = digits))
}
