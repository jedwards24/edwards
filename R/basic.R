#########################################################################################
# mode_stat: Calculate mode of a vector.
#########################################################################################
#'
#' Calculate the statistical mode of an atomic vector.
#'
#' Returns the statistical mode of an atomic vector (can be numeric, character or factor).
#'
#' From https://stackoverflow.com/questions/2547402/is-there-a-built-in-function-for-finding-the-mode.
#'
#' @param x An atomic vector.
#' @param multiple Boolean. If TRUE returns a vector of modes, or just the first otherwise.
#' @param na.rm Boolean. Indicates whether to ignore NAs.
#'
#' @export
mode_stat <- function(x, multiple = TRUE, na.rm = TRUE) {
  if (!is.atomic(x)) {
    stop("`x` must be atomic.", call. = FALSE)
  }
  if(na.rm){
    x <- na.omit(x)
  }
  ux <- unique(x)
  freq <- tabulate(match(x, ux))
  mode_loc <- if(return_multiple) which(freq == max(freq)) else which.max(freq)
  ux[mode_loc]
}

#########################################################################################
# factor_to_numeric: Convert a numeric factor to a numeric vector.
#########################################################################################
#'
#' Convert a numeric factor to a numeric vector.
#'
#' The obvious `as.numeric()` is incorrect.
#'
#' From  https://stackoverflow.com/questions/3418128/how-to-convert-a-factor-to-integer-numeric-without-loss-of-information
#'
#' @param x A numeric factor.
#'
#' @export
factor_to_numeric <- function(x) {
    as.numeric(levels(x))[x]
}

#########################################################################################
# ilogit: Vectorised inverse logit function.
#########################################################################################
#'
#' Vectorised inverse logit function: \eqn{exp(x) / (1 + exp(x))}.
#'
#' Modified inverse logit function from Faraway package. The original allowed for some elements to be \code{NA} which
#'   this does not. I added a check on \code{x} to avoid \code{NaN}. This occurs when \code{x} is greater than about
#'   750 but, since \eqn{exp(x) / (1 + exp(x)) = 1} for \eqn{x>=20}, I only check for x > 20 and
#'   output 1 for these cases.
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
