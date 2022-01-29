#' Convert integer vector into binned factor with integer cuts and appropriately named levels
#'
#' Takes a numeric input vector, groups into bins based on the argument `cuts`, and
#' outputs an ordered factor with appropriate names. Any NAs in the input are optionally grouped
#' into an explicit NA level with name `na_level`
#'
#' @param x Integer vector to be binned.
#' @param cuts An integer vector giving bin cuts. Don't add `-Inf` or `Inf` end cuts as these are handled automatically.
#' @param na_level A name for the explicit NA level (if any). If left as the default NULL then no explicit NA
#'   level will be formed.
#' @param na_at_end Logical indicating whether to put any explicit NA level at the end of the levels. The
#'   default `na_at_end = FALSE` will put the level at the start.
#'
#' @export
bin_integer <- function(x, cuts, na_level = NULL, na_at_end = FALSE){
  cuts <- c(-Inf, cuts, Inf)
  x <- cut(x, cuts, ordered_result = T)
  n_cuts <- length(cuts)
  levels(x)[1] <- paste0(cuts[2], " or less")
  levels(x)[(n_cuts - 1)] <- paste0("more than ", cuts[n_cuts - 1])
  for (i in 2 : (n_cuts - 2)){
    levels(x)[i] <- if((cuts[i] + 1) == cuts[i + 1]){
      cuts[i + 1]
    }else{
      paste0(cuts[i] + 1, " to ", cuts[i + 1])
    }
  }
  if(is.null(na_level) | all(!is.na(x))){
    return(x)
  }
  x <- forcats::fct_explicit_na(x, na_level = na_level)
  if(na_at_end){
    return(forcats::fct_relevel(na_level, after = Inf))
  }
  forcats::fct_relevel(na_level)
}

#' Bin integer levels in a factor
#'
#' Input is a vector with levels that, all except one, are naturally integers e.g. "60". The other level is a
#' string e.g. "missing".  An ordered factor is returned with the "integer" levels grouped into bins
#' based on the argument `cuts` with appropriate names.
#'
#' FACTOR ORDERING HAS NOT BEEN CHECKED.
#'
#' @param ff Factor vector to be binned.
#' @param cuts A numeric vector giving bin cuts. Don't add `-Inf` or `Inf` end cuts as these are handled automatically.
#'
#' @export
bin_integer_fct <- function(ff, cuts){
  if (!is.factor(ff)) {
    ff <- as.factor(ff)
  }
  chr_level <- stringr::str_subset(levels(ff), "^\\D*$")
  if(length(chr_level) != 1){
    mes <- "There must be exactly one level which is not a number."
    mes <- if(length(chr_level) == 0){
      paste(mes, "The input has no such levels.")
    }else{
      paste(mes, "The input has multiple non-number levels:", paste(chr_level, collapse = " "))
    }
    stop(mes, call. = FALSE)
  }
  chr_ind <- which(levels(ff) == chr_level)
  if(any(stringr::str_detect(levels(ff)[-chr_ind], "\\."))) {
    stop("All numeric levels must be integers.")
  }

  cuts <- c(-Inf, -1E10, cuts, Inf)
  levels(ff)[chr_ind] <- -9E10
  ff <- as.numeric(levels(ff))[ff]
  ff <- cut(ff, cuts, ordered_result = T)
  n_cuts <- length(cuts)
  levels(ff)[1] <- chr_level
  levels(ff)[2] <- paste0(cuts[3], " or less")
  levels(ff)[(n_cuts - 1)] <- paste0("more than ", cuts[n_cuts - 1])
  for (i in 3 : (n_cuts - 2)){
    levels(ff)[i] <- if(cuts[i + 1] == cuts[i] + 1) {
      cuts[i + 1]
    }else{
      paste0(cuts[i] + 1, " to ", cuts[i + 1])
    }
  }
  ff
}
