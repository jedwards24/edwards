#########################################################################################
# convert_date: Parse a string in a particular format and convert to a date or datetime.
#########################################################################################
#'
#' Parse a string in a particular format and convert to a date or datetime.
#'
#' Reads text vectors which are either in dmy or dmy:time format. If all of the times are
#' zeroes then the output is a date. Fully `NA` vectors are returned as dates.
#'
#' @param x A character vector to be converted.
#'
#' @export
convert_date <- function(x) {
  if (!is.character(x)){
    stop("Input must be a character vector.")
  }
    if (all(is.na(x))) {
      return(as_date(x))
    }
  xstr <- x[which.max(str_length(x))]
  if (stringr::str_detect(xstr, ":")) {
    xsplit <- stringr::str_split_fixed(xstr, ":", 2)[, , drop = T]
    if (any(stringr::str_detect(xsplit[2], "[1-9]"))) {
      return(lubridate::dmy_hms(x))
    } else{
      x <- stringr::str_split_fixed(x, ":", 2)[, 1, drop = T]
    }
  }
  lubridate::dmy(x)
}
