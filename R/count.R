#########################################################################################
# count_nas: counts NAs in a data frame by column.
#########################################################################################
#'
#' Count NAs in a data frame by column
#'
#' Returns a vector of the number of NAs of each variable in a data frame.
#'
#' @param df A data frame.
#' @param all By default variables with no NAs are omitted from the output. Set to \code{TRUE} to show all.
#'
#' @export
count_nas <- function(df, all = FALSE) {
  if (!is.list(df)) {
    stop("`df` must be a list.", call. = FALSE)
  }
  vals <- vapply(df, function(x) sum(is.na(x)), integer(1))
  vals <- vals[vals > 0 | all]
  if(length(vals) == 0){
    message("There are no NAs in the data.")
    invisible(vals)
  }else{
    vals
  }
}

#########################################################################################
# count_unique: counts the number of unique values in a data frame by column.
#########################################################################################
#'
#' Count the number of unique values in a data frame by column
#'
#' Returns a vector of the number of unique values of each variable in a data frame. Any NA entries are included as
#'   a unique value.
#'
#' @param df A data frame.
#'
#' @export
count_unique <- function(df) {
  if (!is.list(df)) {
    stop("Argument \"df\" must be a list.", call. = FALSE)
  }
  vapply(df, function(x) length(unique(x)), integer(1))
}

#########################################################################################
# count_levels: counts the total number of levels in a data frame by column.
#########################################################################################
#'
#' Count the number of levels in a data frame by column
#'
#' Returns a vector of the number of levels of each variable in a data frame.
#'
#' @param df A data frame.
#' @param all By default variables with no levels are omitted from the output. Set \code{all=TRUE} to show all.
#'
#' @export
count_levels <- function(df, all = FALSE) {
  if (!is.list(df)) {
    stop("Argument \"df\" must be a list.", call. = FALSE)
  }
  vals <- vapply(df, nlevels, integer(1))
  vals <- vals[vals > 0 | all]
  if(length(vals) == 0){
    message("There are no factors in the data.")
    invisible(vals)
  }else{
    vals
  }
}

#########################################################################################
# count_nas2: counts NAs in a data frame by column. Returns a tbl.
#########################################################################################
#'
#' Count NAs in a data frame by column (depreciated)
#'
#' @description
#' Unlike \code{count_nas()} this returns the results in a tibble with columns giving:
#' \itemize{
#' \item{The number of NAs for each variable in the data.}
#' \item{The number of NAs as a proportion of rows.}
#' \item{The class of each variable.}
#' }
#'
#' @param df A data frame.
#'
#' @param all By default variables with no NAs are omitted from the output. Set to \code{TRUE} to show all.
#' @param sort By default the output table is sorted by descending number of NAs. Set to \code{FALSE} to keep
#'   variable ordering as in the data.
#'
#' @export
count_nas2 <- function(df, all = FALSE, sort = TRUE) {
  warning("`count_nas2() is depreciated. Use `var_summary().", call. = FALSE)
  if (!is.data.frame(df)) {
    stop("Argument \"df\" must be a data frame.", call. = FALSE)
  }
  nas <- vapply(df, function(x) sum(is.na(x)), integer(1))
  if (max(nas) == 0) {
    message("There are no NAs in the data")
  } else{
    tibble::tibble(
      variable = names(df),
      nas = nas,
      prop = nas / nrow(df),
      class = vapply(df, function(x) class(x)[1], character(1))
    ) %>%
      dplyr::filter((nas > 0) | all) %>%
      `if`(sort, dplyr::arrange(., dplyr::desc(nas)), .)
  }
}

#########################################################################################
# count_string: Counts the total number of string pattern matches in a data frame by column.
#########################################################################################
#'
#' Count the total number, by column, of entries in a data frame that match a string pattern
#'
#' Returns a named integer vector with elements that give the number of entries in the corresponding
#' column of \code{df} that contain a match to the string pattern \code{pattern}. No coercion is used
#' so only characters or factors are m'atched (see examples).
#'
#' Note that repeated occurences of \code{pattern} in a single string are only counted once (see examples).
#'
#' @param df A data frame.
#' @param pattern A string pattern (regular expression).
#' @param all By default variables with no matches are omitted from the output. Set all=T to show all.
#' @examples
#' x <- data.frame(a = c("an", "banana", "candy"), b = c("on", "bon", "bonbon"), d = 1:3)
#' count_string(x, "an", all = TRUE)
#' count_string(x, "an")
#' count_string(x, "b")
#' count_string(x, "1") # not matched to integers
#'
#' @export
count_string <- function(df, pattern, all = FALSE){
  if (!is.list(df)) {
    stop("Argument \"df\" must be a list.", call. = FALSE)
  }
  f <- function(x){
    if (is.character(x) | is.factor(x)) sum(stringr::str_detect(x, pattern), na.rm = T) else 0L
  }
  vals <- vapply(df, f, integer(1))
  vals <- vals[vals > 0 | all]
  if(length(vals) == 0){
    message("String not found in the data.")
    invisible(vals)
  }else{
    vals
  }
}

#########################################################################################
# count_matches: Counts the total number of exact matches to a value in a data frame by column.
#########################################################################################
#'
#' Count the total number of exact matches to a value in a data frame by column
#'
#' Returns a named integer vector with elements that give the number of entries in the corresponding
#' column of \code{df} that match to the argument \code{value}. No coercion is used so type must also match.
#'
#' @param df A data frame.
#' @param value A length one vector.
#' @param all By default variables with no matches are omitted from the output. Set all=T to show all.
#' @examples
#' x <- data.frame(a = c("an", "banana", "candy"), b = c("on", "bon", "bonbon"), c = 1:3)
#' count_matches(x, "an", all = TRUE)
#' count_matches(x, "an")
#' count_matches(x, 1L)
#' count_matches(x, 1) # type must match
#' count_matches(x, "1") # type must match
#'
#' @export
#'
count_matches <- function(df, value, all = FALSE){
  if (!is.list(df)) {
    stop("Argument \"df\" must be a list.", call. = FALSE)
  }
  if (length(value) != 1){
    stop("Argument \"value\" must be length 1.", call. = FALSE)
  }
  type <- typeof(value)
  f <- function(x){
    if (typeof(x) == type | (is.factor(x) & type == "character")){
      sum(x == value, na.rm = TRUE)
    }else{
      0L
    }
  }
  vals <- vapply(df, f, integer(1))
  vals <- vals[vals > 0 | all]
  if(length(vals) == 0){
    message("No matches in the data.")
    invisible(vals)
  }else{
    vals
  }
}

#########################################################################################
# count_matches2: Count exact string matches in a data frame by column.
#########################################################################################
#'
#' Count exact string matches in a data frame by column.
#'
#' Similar to \code{count_matches()} but counts matches for multiple strings rather than just one. The
#' output is a tibble with a row for each column in \code{df}. Unlike \code{count_matches}, non-string
#' matching is not enabled.
#'
#' @param df A data frame.
#' @param strings A character vector.
#' @param all Logical. If \code{FALSE} (default) then rows/columns with no non-zero entry are not shown.
#'
#' @examples
#' df <- tibble::tibble(col1 = c("a", ".", ".", "a"),
#'                      col2 = c("-", "-", "b", "b"),
#'                      col3 = rep("z", 4),
#'                      col4 = c("n/a", "f", "f", ""))
#' strs <- c(".", "-", "n/a", "na", "")
#' count_matches2(df, strs, all = TRUE)
#' count_matches2(df, strs)
#'
#' @export
count_matches2 <- function(df, strings, all = FALSE) {
  if (!is.list(df)) {
    stop("Argument \"df\" must be a list.", call. = FALSE)
  }
  if (!is.character(strings)||!is.vector(strings)){
    stop("Argument \"strings\" must be a character vector.", call. = FALSE)
  }
  tb <- lapply(strings,
               FUN = edwards::count_matches,
               df = df,
               all = TRUE) %>%
    dplyr::bind_cols(col_names = names(df), .)
  names(tb) <- c("col_names", strings)
  if (all){
    return(tb)
  }
  sum_rows <- rowSums(tb[, -1])
  sum_cols <- c(1, colSums(tb[, -1]))
  tb <- tb[sum_rows > 0, sum_cols > 0]
  if (sum(sum_rows) == 0){
    message("No matches in the data.")
    return(invisible(tb))
  }
  tb
}

#########################################################################################
# var_summary: Simple summary of the variables in a data frame.
#########################################################################################
#'
#' Simple summary of the variables in a data frame
#'
#' Returns a tibble with the names, class, number of unique values, and the number and percent of
#' \code{NA}s for each variable in the data. If there are \code{NA} values then they are included as
#' a unique value.
#'
#' @param df A data frame.
#'
#' @export
var_summary <- function(df) {
  if (!is.data.frame(df)) {
    stop("Argument \"df\" must be a data frame.", call. = FALSE)
  }
   if (ncol(df) == 0) {
     message("The data frame has zero columns.")
   }
  tibble::tibble(var = names(df),
                 index = seq_along(df),
                 class = vapply(df, function(x) class(x)[1], character(1)),
                 unique = vapply(df, function(x) length(unique(x)), integer(1)),
                 missing = vapply(df, function(x) sum(is.na(x)), integer(1)),
                 pct_miss = 100 * missing / nrow(df)
  )
}

#########################################################################################
# count_at: Performs dplyr::count for a range of variables in a data frame.
#########################################################################################
#'
#' Perform  \code{dplyr::count} for a range of variables in a data frame (DEPRECIATED - use \code{count_over()})
#'
#' Prints output from \code{dplyr::count()} for each variable index given by argument \code{cols} (an integer vector).
#'
#' @param df A data frame.
#' @param cols Vector of integer indices. If missing, all columns are included.
#' @param sort Logical passed to \code{count()} to say whether results are sorted by descending number of observation.
#'   Unlike in \code{count()}, this defaults to \code{TRUE}.
#' @param n Integer passed to \code{print()} which gives the maximum number of rows printed in each count summary.
#'
#' @export
count_at <- function(df, cols = NULL, sort = TRUE, n = 10) {
  if (is.null(cols)) cols <- 1 : ncol(df)
  if (!is.data.frame(df)) {
    stop("Argument \"df\" must be a data frame.", call. = FALSE)
  }
  if (!all(cols %in% 1 : ncol(df))) {
    stop("Values in \"cols\" must match column numbers in \"df\"", call. = FALSE)
  }
  for(name in names(df)[cols]){
    print(dplyr::count(df, !!as.name(name), sort = sort), n = n)
  }
  warning("`count_at() is depreciated. Use `count_over()` instead.", call. = FALSE)
  invisible(df)
}

#########################################################################################
# count_over: Performs dplyr::count for a range of variables in a data frame.
#########################################################################################
#'
#' Perform  \code{dplyr::count} for a range of variables in a data frame
#'
#' Prints output from \code{dplyr::count()} for each variable index given by argument \code{cols} (an integer vector).
#'
#' @param df A data frame.
#' @param ... Columns to count over. Accepts tidyselect inputs. If omitted then count is applied to every column.
#' @param sort Logical passed to \code{count()} to say whether results are sorted by descending number of observation.
#'   Unlike in \code{count()}, this defaults to \code{TRUE}.
#' @param n Integer passed to \code{print()} which gives the maximum number of rows printed in each count summary.
#'
#' @export
count_over <- function(df, ..., sort = TRUE, n = 10L) {
  if (!is.data.frame(df)) stop("Argument \"df\" must be a data frame.", call. = FALSE)
  if (!is.numeric(n)) stop("Argument \"n\" must be an numeric.", call. = FALSE)
  if (!is.logical(sort)) stop("Argument \"sort\" must be a logical", call. = FALSE)
  cols <- names(dplyr::select(df, ...))
  if (length(cols) == 0) cols <- names(df)
  for(name in cols){
    print(dplyr::count(df, !!as.name(name), sort = sort), n = n)
  }
  invisible(df)
}

#########################################################################################
# count_n: Shorthand for `count(df, ...) %>% count(n)`
#########################################################################################
#'
#' Count group sizes by grouped variables
#'
#' Shorthand for \code{count(count(df, ...), n)}.
#'
#' @param df A data frame.
#' @param ... Variables to group by.
#'
#' @examples
#' count_n(mtcars, disp)
#'
#' @export
count_n <- function(df, ...) {
  dplyr::count(dplyr::count(df, ...), n, name = "freq")
}

#########################################################################################
# count2: dplyr::count() with proportion column and default sort = T
#########################################################################################
#'
#' \code{dplyr::count()} with proportion column and default \code{sort = T}.
#'
#' Adds a column "prop" which gives the proportion of total rows in that group.
#'
#' @param df A data frame.
#' @param ... Arguments passed to count including variables to group by.
#'
#' @examples
#' count2(mtcars, gear)
#'
#' @export
count2 <- function(df, ...) {
  dplyr::count(df, ..., sort = TRUE) %>%
    dplyr::mutate(prop = n / sum(n))
}
