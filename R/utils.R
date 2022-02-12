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

#' Convert a factor with numeric or logical-type levels to a numeric vector
#'
#' The obvious `as.numeric()` is incorrect. No checks are made on the level contents except
#' to check if levels are `c("TRUE", "FALSE")`, in which case they will be converted to a
#' integer vector (0 for FALSE, 1 for TRUE).
#'
#' Numeric part is from
#' <https://stackoverflow.com/questions/3418128/how-to-convert-a-factor-to-integer-numeric-without-loss-of-information>.
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
  warning("`ilogit() is depreciated. Use `jemodel::ilogit()` instead.", call. = FALSE)
  if (any(big <- (x > 20))) {
    lv <- x
    lv[big] <- 1
    if (any(!big))
      lv[!big] <- Recall(x[!big])
    return(lv)
  }
  exp(x) / (1 + exp(x))
}

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

#' nth maximum element in a vector
#'
#' @param x A vector.
#' @param n Numeric vector giving the ranks of elements to be returned.
#' @param na.rm A logical indicating whether missing values should be ignored.
#'   If `FALSE` an `NA` value in any of the arguments will cause a value of `NA`
#'   to be returned.
#'
#' @export
max_n <- function(x, n = 2L, na.rm = FALSE){
  if (!is.numeric(n)) stop("`n` must be numeric.", call. = FALSE)
  if ((max(n) > length(x)) | (min(n) < 1L))
    stop('All elements of `n` must be between 1 and `length(x)`.', call. = FALSE)
  if (!na.rm && any(is.na(x))) return (x[is.na(x)][1]) # match class of x
  if (na.rm) x <- x[!is.na(x)]
  len <- length(x)
  if (max(n) >= len + 1)
    stop("All elements of `n` must be no greater than the number of non-missing values
         in `x`.", call. = FALSE)
  sort(x, partial = len - n + 1)[len - n + 1]
}

#' nth minimum element in a vector
#'
#' @param x A vector.
#' @param n Numeric vector giving the ranks of elements to be returned.
#' @param na.rm A logical indicating whether missing values should be ignored.
#'   If `FALSE` an `NA` value in any of the arguments will cause a value of `NA`
#'   to be returned.
#'
#' @export
min_n <- function(x, n = 2L, na.rm = FALSE){
  if (!is.numeric(n)) stop("`n` must be numeric.", call. = FALSE)
  if ((max(n) > length(x)) | (min(n) < 1L))
    stop('All elements of `n` must be between 1 and `length(x)`.', call. = FALSE)
  if (!na.rm && any(is.na(x))) return (x[is.na(x)][1]) # match class of x
  if (na.rm) x <- x[!is.na(x)]
  if (max(n) >= length(x) + 1)
    stop("All elements of `n` must be no greater than the number of non-missing values
         in `x`.", call. = FALSE)
  sort(x, partial = n)[n]
}

#' Negation of `%in%`
#'
#' Binary operator, which returns a logical vector with `TRUE` element where there is
#' no match in the left operand and `FALSE` where there is a match.
#'
#' @param a,b Vectors passed to `%in%`.
#'
#' @usage a \%notin\% b
#' @rdname notin
#' @export
`%notin%` <- function(a, b) ! a %in% b

#' Check if a package is installed and stop if not
#'
#' If the package is not installed then the function will error and give an appropriate message.
#'
#' Designed to be used in scripts or functions where a function from the named package is used with `::`
#' without attaching the package. It should be used alongside `library()` calls at the top
#' of scripts where it also serves the purpose of indicating to the user that a package is needed.
#'
#' The function uses `requireNamespace()` which will load the package if available but not attach it.
#' This is what happens when `::` is used.
#'
#' @param package a package name as a character string.
#'
#' @export
need <- function(package) {
  if (!requireNamespace(package, quietly = TRUE)) {
    stop("Package \"", package, "\" needed for this script/function to work. Please install it.",
         call. = FALSE)
  }
}

#' Get sizes, in Mb, of all objects in global environment
#'
#' Returns table of sizes of all objects in the global environment in Mb (1024^2 B), in
#' decreasing order of size.
#'
#' @return A tibble with two columns: object name and size in Mb.
#' @export
object_size_all <- function(){
  names <- ls(envir = .GlobalEnv)
  mb <- vapply(names, function(x) utils::object.size(get(x, envir = .GlobalEnv)) / 1024^2, numeric(1))
  mb <- sort(mb, decreasing = TRUE)
  tibble::tibble(object = names(mb), Mb = mb)
}
