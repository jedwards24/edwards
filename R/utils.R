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

#' Convert a factor to a numeric vector using its levels
#'
#' Converts to a numeric vector factors with levels that could be appropriately converted to
#' numeric values (either number or logical-type strings).
#'
#' The output from the obvious `as.numeric()` is unlikely to be what is desired, since it acts
#' on the underlying integers, rather than the levels. This function instead uses
#' `as.integer(as.logical(x))` to attempt to coerce the character levels. If all levels are
#' strings that would be coercible by `as.logical()`, that is in
#' `c("T", "TRUE", "True", "true", "F", "FALSE", "False", "false", NA)`, then they
#' are converted appropriately to a numeric vector instead. The two methods are never mixed, so
#' `factor_to_numeric(factor(c("T", "1")))` will attempt numeric conversion and produce `NA`
#' for the `"T"`.
#'
#' @param x A factor.
#' @examples
#' x <- factor(3:5)
#' as.numeric(x)
#' factor_to_numeric(x)
#' factor_to_numeric(factor(c("T", "false", NA)))
#' @export
factor_to_numeric <- function(x) {
  if (!is.factor(x)) stop("`x` must be a factor.", call. = FALSE)
  lgl_str <- c("T", "TRUE", "True", "true", "F", "FALSE", "False", "false", NA_character_)
  if (all(levels(x) %in% lgl_str)){
    return(as.numeric(as.logical(x)))
  }
  as.numeric(levels(x))[x]
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

#' Select vector elements by sorted position
#'
#' Returns elements of `x` by position `n` after sorting by, decreasing for `max_n()` or increasing for `min_n()`.
#' Any `NA` elements of `x` are ranked last for either function.
#' @examples
#' x <- 5:8
#' max_n(x, 2)
#' min_n(x, 1:2)
#' max_n(c(5:7, NA), 1:4)
#' min_n(c(5:7, NA), 1:4)
#' @param x A vector.
#' @param n Numeric vector giving the ranks of elements to be returned.
#' @return A vector of length equal to `n`.
#' @export
max_n <- function(x, n = 2L){
  if (!is.numeric(n)){
    stop("`n` must be numeric.", call. = FALSE)
  }
  if ((max(n) > length(x)) | (min(n) < 1L))
    stop('All elements of `n` must be between 1 and `length(x)`.', call. = FALSE)
  len <- length(x)
  if (any(is.na(x))){
    return(sort(x, decreasing = TRUE, na.last = TRUE)[n])
  }
  sort(x, partial = len - n + 1)[len - n + 1]
}

#' @rdname max_n
#' @export
min_n <- function(x, n = 2L){
  if (!is.numeric(n)){
    stop("`n` must be numeric.", call. = FALSE)
  }
  if ((max(n) > length(x)) | (min(n) < 1L))
    stop('All elements of `n` must be between 1 and `length(x)`.', call. = FALSE)
  if (any(is.na(x))){
    return(sort(x, na.last = TRUE)[n])
  }
  sort(x, partial = n)[n]
}

#' Extend vector to given length
#'
#' Appends `NA` values to `x` so that its length is `n`.
#' @param x An atomic vector.
#' @param n Length one numeric.
#' @return A vector of length `n`.
#' @examples
#' extend_vector(2:5, 7)
#' extend_vector(2:5, 2)
#' @export
extend_vector <- function(x, n) {
  if (!is.numeric(n) || length(n) != 1){
    stop("`n` must be a length 1 numeric.", call. = FALSE)
  }
  if (!is.atomic(x)){
    stop("`x` must be atomic.", call. = FALSE)
  }
  if (length(x) >= n){
    return(x)
  }
  extra <- rep(NA, n - length(x))
  c(x, extra)
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
