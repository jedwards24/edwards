#' Convert character elements in a data frame to NA
#'
#' In `data` any character columns, any elements matching any of `na_strings` are replaced with `NA`.
#' @return A modified version of `data` with the same rows and columns.
#' @param data A data frame.
#' @param na_strings Character vector of strings to convert missing values.
#' @export
#' @examples
#' df <- tibble::tibble(x1 = c("a", "", "1"),
#'                  x2 = c("-", "", "b"),
#'                  x3 = 1:3)
#' na_if_string(df, c("-", "", "1"))
na_if_string <- function(data, na_strings = c("")) {
  if (!is.data.frame(data)) {
    stop("Argument `data` must be a data frame.", call. = FALSE)
  }
  if (!is.character(na_strings)) {
    stop("Argument `na_strings` must be a character vector.", call. = FALSE)
  }
  if (length(na_strings) == 1){ # for speed in this case
    return(dplyr::mutate_if(data, is.character, ~dplyr::na_if(., na_strings)))
  }
  dplyr::mutate_if(data, is.character, ~na_if_any(., na_strings))
}

#' Convert values to NA
#'
#' Convert values in `x` to `NA` if they match any elements of `y`.
#' @seealso This is similar to `dplyr::is_na()` but checks each element of `x` for a match with
#' any elements of `y`.
#' @param x Vector to modify.
#' @param y Values to replace with NA.
#' @return A modified version of `x` that replaces any values that are equal to any element of y with NA.
#' @examples
#' na_if_any(1:5, c(2, 4))
#' @export
na_if_any <- function(x, y) {
  x[x %in% y] <- NA
  x
}
