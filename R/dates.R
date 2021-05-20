#########################################################################################
# convert_date: Parse a string in a particular format and convert to a date or datetime.
#########################################################################################
#'
#' Parse a character vector in a particular format and convert to a date or datetime
#'
#' Reads text vectors which are either in dmy or dmy:time format. If all of the times are
#' zeroes then the output is a date. Fully  `NA` vectors are returned as dates.
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
    return(lubridate::as_date(x))
  }
  xstr <- x[which.max(stringr::str_length(x))]
  if (stringr::str_detect(xstr, ":")) {
    xsplit <- stringr::str_split_fixed(xstr, ":", 2)[, , drop = T]
    if (any(stringr::str_detect(xsplit[2], "[1-9]"))) {
      message("Converted to a datetime.")
      return(lubridate::dmy_hms(x))
    } else{
      x <- stringr::str_split_fixed(x, ":", 2)[, 1, drop = T]
    }
  }
  message("Converted to a date.")
  lubridate::dmy(x)
}

#########################################################################################
# is_simple_datetime: Test if a datetime object has times that are all zero.
#########################################################################################
#'
#' Test if a datetime object has times that are all zero
#'
#' Returns `TRUE` if the input is a datetime object where the time part of every element is zero.
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
  x <- stats::na.omit(x)
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
#' Finds `difftime(date1, date2)` (i.e. `date1 - date2`) using `units = "days"`, then converts
#' to a numeric.
#'
#' By default, only the date part of any datetime inputs are used (the datetime is
#' first coerced to a date using `lubridate::as_date()`). If `keep_times = TRUE` and both
#' `date1` and `date2` are datetimes then the times will be used. See examples.
#'
#' @param date1,date2 A date or datetime.
#' @param keep_times Logical. If `FALSE` any times will be dropped before difference is calculated.
#' @examples
#' d1 <- lubridate::ymd_hms("2020-02-01 08:00:00")
#' d2 <- lubridate::ymd_hms("2020-01-01 00:00:00")
#' d3 <- lubridate::ymd("2020-01-01")
#' diff_days(d1, d2, keep_times = FALSE)
#' diff_days(d1, d2, keep_times = TRUE)
#' diff_days(d1, d3, keep_times = TRUE) # time in d1 not used since d3 is a date
#'
#' @export
diff_days <- function(date1, date2, keep_times = FALSE) {
  if (!lubridate::is.POSIXt(date1) & !lubridate::is.Date(date1)){
    stop("`date1` must be a date or datetime.", call. = FALSE)
  }
  if (!lubridate::is.POSIXt(date2) & !lubridate::is.Date(date2)){
    stop("`date2` must be a date or datetime.", call. = FALSE)
  }
  if (lubridate::is.POSIXt(date1) & lubridate::is.POSIXt(date2) & keep_times){
    return(as.numeric(difftime(date1, date2, units = "days")))
  }
  as.numeric(difftime(lubridate::as_date(date1), lubridate::as_date(date2), units = "days"))
}

