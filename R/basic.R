#########################################################################################
# mode_stat: Calculate mode of a vector.
#########################################################################################
#'
#' Calculate the statistical mode of an atomic vector
#'
#' Returns the statistical mode of an atomic vector (can be numeric, character or factor).
#'
#' From <https://stackoverflow.com/questions/2547402/is-there-a-built-in-function-for-finding-the-mode>.
#'
#' @param x An atomic vector.
#' @param multiple Logical. If TRUE returns a vector of modes, or just the first otherwise.
#' @param na.rm Logical. Indicates whether to ignore NAs.
#'
#' @export
mode_stat <- function(x, multiple = TRUE, na.rm = TRUE) {
  if (!is.atomic(x)) {
    stop("`x` must be atomic.", call. = FALSE)
  }
  if(na.rm){
    x <- stats::na.omit(x)
  }
  ux <- unique(x)
  freq <- tabulate(match(x, ux))
  mode_loc <- if(multiple) which(freq == max(freq)) else which.max(freq)
  ux[mode_loc]
}

#########################################################################################
# factor_to_numeric: Convert a numeric factor to a numeric vector.
#########################################################################################
#'
#' Convert a factor with numeric or logical levels to a numeric vector
#'
#' The obvious `as.numeric()` is incorrect. No checks are made on the level contents except to check if levels are
#' `c("TRUE", "FALSE")`, in which case they will be converted to a integer vector (0 for FALSE, 1 for TRUE).
#'
#' Numeric part is from <https://stackoverflow.com/questions/3418128/how-to-convert-a-factor-to-integer-numeric-without-loss-of-information>.
#'
#' @param x A factor.
#'
#' @export
factor_to_numeric <- function(x) {
  if (!is.factor(x)) stop("`x` must be a factor.", call. = FALSE)
  if (all(levels(x) %in% c("TRUE", "FALSE"))){
    return(as.integer(as.logical(x)))
  }
  as.numeric(levels(x))[x]
}

#########################################################################################
# ilogit: Vectorised inverse logit function.
#########################################################################################
#'
#' Vectorised inverse logit function
#'
#' Calculate the inverse logit function \eqn{exp(x) / (1 + exp(x))}. Modified from Faraway package.
#' The original allowed for some elements to be `NA` which this does not. I added a check
#' on `x` to avoid `NaN`. This occurs when `x` is greater than about
#' 750 but, since \eqn{exp(x) / (1 + exp(x)) = 1} for \eqn{x>=20}, I only check for x > 20 and
#' output 1 for these cases.
#'
#' @param x A numeric vector.
#'
#' @export
ilogit <- function (x){
  if (any(big <- (x > 20))) {
    lv <- x
    lv[big] <- 1
    if (any(!big))
      lv[!big] <- Recall(x[!big])
    return(lv)
  }
  exp(x) / (1 + exp(x))
}

#########################################################################################
# prinf: Shorthand for print(n = Inf)
#########################################################################################
#'
#' Print all rows of a tibble
#'
#' Shorthand for `print_tbl(x, n = Inf)`. Ok, I'm lazy.
#'
#' @param x A tibble.
#'
#' @export
prinf <- function(x) {
  if (tibble::is_tibble(x)){
    print(x, n = Inf)
  }else{
    print(x)
  }
  invisible(x)
}

#########################################################################################
# max_n: nth largest element in a vector
#########################################################################################
#'
#' nth largest element in a vector
#'
#' @param x A vector.
#' @param n Numeric vector giving the ranks of elements to be returned.
#'
#' @export
max_n <- function(x, n = 2L){
  if (!is.numeric(n)) stop('`n` must be numeric.', call. = FALSE)
  len <- length(x)
  if ((max(n) > len) | (min(n) <= 0L)) stop('All elements of `n` must be between 1 and `length(x)`.', call. = FALSE)
  sort(x, partial = len - n + 1)[len - n + 1]
}

#########################################################################################
# min_n: nth smallest element in a vector
#########################################################################################
#'
#' nth smallest element in a vector
#'
#' @param x A vector.
#' @param n Numeric vector giving the ranks of elements to be returned.
#'
#' @export
min_n <- function(x, n = 2L){
  if (!is.numeric(n)) stop('`n` must be numeric.', call. = FALSE)
  len <- length(x)
  if ((max(n) > len) | (min(n) <= 0L)) stop('All elements of `n` must be between 1 and `length(x)`.', call. = FALSE)
  sort(x, partial = n)[n]
}

#########################################################################################
# %notin%: negation of %in%
#########################################################################################
#'
#' Negation of `%in%`. Binary operator, which returns a logical vector with `TRUE` element where there is
#'  no match in the left operand and `FALSE` where there is a match.
#'
#' @param a,b Vectors passed to `%in%`.
#'
#' @usage a \%notin\% b
#' @rdname notin
#' @export
`%notin%` <- function(a, b) ! a %in% b
