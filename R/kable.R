#' Version of `kable()` with specific default kableExtra styling
#'
#' Shorthand to apply both `knitr::kable()` and `kableExtra::kable_styling()` to a table,
#' using styling options `bootstrap_options = c("striped", "condensed")` and `full_width = FALSE`.
#' The default number of digits is set to 3 but can be changed.
#'
#' Only one argument (`position`) from `kableExtra::kable_styling()` can be changed here.
#'
#' @param df A data frame.
#' @param digits Sets the number of digits (via the `kable()` `digits` argument).
#' @param position Passed to `kableExtra::kable_styling()`.
#' @param ... Optional arguments passed to `knitr::kable()`.
#'
#' @export
my_kable <- function(df, digits = 3, position = "center", ...){
  if (!is.data.frame(df)) {
    stop("`df` must be a data frame.", call. = FALSE)
  }
  tb <- knitr::kable(df, digits = digits, ...)
  tb <- kableExtra::kable_styling(tb,
                                  bootstrap_options = c("striped", "condensed"),
                                  full_width = FALSE,
                                  position = position)
  return(tb)
}

#' `edwards::my_kable()` with a two way contingency table input.
#'
#' @param ct A two way table object.
#' @param digits Sets the number of digits (passed to `my_kable()`).
#'
#' @export
kbl_ctable <- function(ct, digits = 3) {
  if (length(dim(ct)) != 2 | !is.table(ct)){
    stop("`ct` must be a two way contingency table.", .call = FALSE)
  }
  dfmat <- as.data.frame.matrix(ct)
  var1 <- names(dimnames(ct)[1])
  var2 <- names(dimnames(ct)[2])
  rnames <- dimnames(ct)[[2]]
  main <- paste0(var1, " / ", var2)
  df <- dplyr::bind_cols(tibble::tibble(!!as.name(main) := rnames), dfmat)
  my_kable(df, digits = digits)
}

#' Split a table in into parts of balanced size
#'
#' Splits a table into a  `n_tb` parts of similar sizing using standard styling in  `my_kable()`. This is
#'   used when I want to split a long thin table across multiple columns to reduce a document's size. Returns
#'   a single table part. Which part given by argument  `index`.
#'
#' Printing all table parts in one function call didn't work since I couldn't get
#'    `kable()` to print from within a function.
#'
#' @param df A data frame.
#' @param index Which part of the split table to return.
#' @param n_tb The number of parts to split the table into.
#' @param digits Sets the number of digits (via the  `kable()`  `digits` argument).
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
  tb <- dplyr::slice(df, start : end)
  return(my_kable(tb, digits = digits))
}
