#########################################################################################
# convert_date: Parse a string in a particular format and convert to a date or datetime.
#########################################################################################
#'
#' Parse a character vector in a particular format and convert to a date or datetime
#'
#' Reads text vectors which are either in dmy or dmy:time format. If all of the times are
#' zeroes then the output is a date. Fully  \code{NA} vectors are returned as dates.
#'
#' @param x A character vector to be converted.
#'
#' @export
convert_date <- function(x) {
  if (!is.character(x)){
    stop("Input must be a character vector.")
  }
  if (all(is.na(x))) {
    message("Converted to a date (vector is all NA).")
    return(as_date(x))
  }
  xstr <- x[which.max(str_length(x))]
  if (stringr::str_detect(xstr, ":")) {
    xsplit <- stringr::str_split_fixed(xstr, ":", 2)[, , drop = T]
    if (any(stringr::str_detect(xsplit[2], "[1-9]"))) {
      message("Converted to a datetime.")
      return(lubridate::dmy_hms(x))
    } else{
      x <- stringr::str_split_fixed(x, ":", 2)[, 1, drop = T]
    }
  }
  message("Cnverted to a date.")
  lubridate::dmy(x)
}

#########################################################################################
# is_simple_datetime: Test if a datetime object has times that are all zero.
#########################################################################################
#'
#' Test if a datetime object has times that are all zero
#'
#' Returns \code{TRUE} if the input is a datetime object where the time part of every element is zero.
#'
#' @param x An object to test.
#'
#' @examples
#'   x <- as.POSIXct("2009-08-03")
#'   y <- as.Date(x)
#'   is_simple_datetime(x) # TRUE
#'   is_simple_datetime(x + 1) # FALSE
#'   is_simple_datetime(y) # FALSE
#'
#' @export
is_simple_datetime <- function(x) {
  if (!lubridate::is.POSIXt(x)){
    return(FALSE)
  }
  x <- na.omit(x)
  if(all(lubridate::hour(x) == 0) &
     all(lubridate::minute(x) == 0) &
     all(lubridate::second(x) == 0)){
    return(TRUE)
  }
  FALSE
}

#########################################################################################
# diff_days: Difference in days of two dates as a numeric
#########################################################################################
#'
#' Difference in days of two dates as a numeric
#'
#' Finds \code{difftime(date1, date2)} using \code{units = "days"}, then converts to a numeric. If either
#' input is a datetime then only the date part is used (the datetime is first coerced to a date using
#' \code{lubridate::as_date()}).
#'
#' @param date1 A date or datetime.
#' @param date2 A date or datetime.
#'
#' @export
diff_days <- function(date1, date2) {
  as.numeric(difftime(lubridate::as_date(date1), lubridate::as_date(date2), units = "days"))
}
