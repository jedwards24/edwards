#########################################################################################
# count_nas: counts NAs in a data frame by column.
#########################################################################################
#'
#' Count NAs in a data frame by column
#'
#' Returns a vector of the number of NAs of each variable in a data frame.
#'
#' @param df A data frame.
#' @param all By default variables with no NAs are omitted from the output. Set to `TRUE` to show all.
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
#' a unique value.
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
#' @param all By default variables with no levels are omitted from the output. Set `all=TRUE` to show all.
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
# count_string: Counts the total number of string pattern matches in a data frame by column.
#########################################################################################
#'
#' Count the total number, by column, of entries in a data frame that match a string pattern
#'
#' Returns a named integer vector with elements that give the number of entries in the corresponding
#' column of `df` that contain a match to the string pattern `pattern`. No coercion is used
#' so only characters or factors are matched (see examples).
#'
#' Note that repeated occurrences of `pattern` in a single string are only counted once (see examples).
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

#' Count elements by column in a data frame, which exactly match a value
#'
#' @description
#' Counts the number of entries of each column in a data frame that exactly match the supplied values.
#' No coercion is used so the type must also match.
#'
#' * `count_matches()` count smatches to a single `value` of any type and returns a named integer vector
#' with elements corresponding to columns of `df`.
#' * `count_matches2()` counts matches to each element of a character vector `strings`. The
#'   output is a tibble with a row for each element of `strings` (see examples).
#'
#' @returns A named integer vector for `count_matches()`. A tibble for `count_matches2()`.
#'
#' @param df A data frame.
#' @param value A length one vector.
#' @param all By default variables with no matches are omitted from the output. Set `all=T` to show all.
#' @param prop Default is to return counts of matches. Set `prop=TRUE` to return proportions of the number of rows.
#'
#' @examples
#' x <- data.frame(a = c("an", "banana", "candy"), b = c("on", "bon", "bonbon"), c = 1:3)
#' count_matches(x, "an", all = TRUE)
#' count_matches(x, "an")
#' count_matches(x, 1L)
#' count_matches(x, 1) # type must match
#' count_matches(x, "1") # type must match
#'
#' @export
count_matches <- function(df, value, all = FALSE, prop = FALSE){
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
  if (prop) vals <- vals / nrow(df)
  if(length(vals) == 0){
    message("No matches in the data.")
    invisible(vals)
  }else{
    vals
  }
}

#' @param strings A character vector. Defaults to `string_missing()`.
#' @examples
#' df <- data.frame(col1 = c("a", ".", ".", "a"),
#'                  col2 = c("-", "-", "b", "b"),
#'                  col3 = rep("z", 4),
#'                  col4 = c("n/a", "f", "f", ""))
#' strs <- c(".", "-", "n/a", "na", "")
#' count_matches2(df, strs, all = TRUE)
#' count_matches2(df, strs)
#'
#' @rdname count_matches
#' @export
count_matches2 <- function(df, strings = string_missing(), all = FALSE, prop = FALSE) {
  if (!is.list(df)) {
    stop("Argument \"df\" must be a list.", call. = FALSE)
  }
  if (!is.character(strings)||!is.vector(strings)){
    stop("Argument \"strings\" must be a character vector.", call. = FALSE)
  }
  tb <- lapply(strings,
               FUN = count_matches,
               df = df,
               all = TRUE,
               prop = prop) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(string = strings) %>%
    dplyr::select(.data$string, dplyr::everything())

  if (all){
    return(tb)
  }
  sum_rows <- rowSums(tb[, -1])
  sum_cols <- c(1L, colSums(tb[, -1]))
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
#' `NA`s for each variable in the data. If there are `NA` values then they are included as
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
# count_over: Performs dplyr::count for a range of variables in a data frame.
#########################################################################################
#'
#' Perform  `count2()` for a range of variables in a data frame
#'
#' Prints output from `count2()` for multiple columns in `df`.
#'
#' @param df A data frame.
#' @param ... Columns to count over. Accepts tidyselect inputs. If omitted then count is applied to every column.
#' @param sort Logical passed to `count()` to say whether results are sorted by descending number of observation.
#' @param n Integer passed to `print()` which gives the maximum number of rows printed in each count summary.
#'
#' @export
count_over <- function(df, ..., sort = TRUE, n = 10L) {
  if (!is.data.frame(df)) stop("Argument \"df\" must be a data frame.", call. = FALSE)
  if (!is.numeric(n)) stop("Argument \"n\" must be an numeric.", call. = FALSE)
  if (!is.logical(sort)) stop("Argument \"sort\" must be a logical", call. = FALSE)
  cols <- names(dplyr::select(df, ...))
  if (length(cols) == 0) cols <- names(df)
  for(name in cols){
    print(count2(df, !!as.name(name), sort = sort), n = n)
  }
  invisible(df)
}

#########################################################################################
# count_n: Shorthand for `count(df, ...) %>% count(n)`
#########################################################################################
#'
#' Count group sizes by grouped variables
#'
#' Shorthand for `count(count(df, ...), n)`.
#'
#' @param df A data frame.
#' @param ... Variables to group by.
#'
#' @examples
#' count_n(mtcars, disp)
#'
#' @export
count_n <- function(df, ...) {
  dplyr::count(dplyr::count(df, ...), .data$n, name = "freq")
}

#########################################################################################
# count2: dplyr::count() with proportion column and default sort = T
#########################################################################################
#'
#' `dplyr::count()` with proportion column and default `sort = TRUE`.
#'
#' Adds a column "prop" which gives the proportion of total rows in that group.
#'
#' @param df A data frame.
#' @param ... Variables to group by. Passed to `count()`.
#' @param sort Passed to `count`, but defaults to `TRUE`.
#' @param wt,name Optional, passed to `count`.
#'
#' @examples
#' count2(mtcars, cyl)
#' count2(mtcars, cyl, sort = FALSE)
#'
#' @export
count2 <- function(df, ..., sort = TRUE, wt = NULL, name = NULL) {
  x <- dplyr::count(df, ..., wt = {{wt}}, sort = sort, name = name)
  if (!is.null(name)){
    count_name <- name
  }else{
    count_name <- dplyr::group_by(df, ...) %>%
      dplyr::group_vars() %>%
      n_name()
  }
  dplyr::mutate(x, prop = !!rlang::sym(count_name) / sum(!!rlang::sym(count_name)))
}

#' Copy of `dplyr:::n_name()`
#' Used in `count2()`
#' @noRd
n_name <- function (x) {
  name <- "n"
  while (name %in% x) {
    name <- paste0("n", name)
  }
  name
}

#' Strings commonly indicating missing values
#'
#' @return A character vector `c("", " ", ".", "-", "NA", "na", "n/a", "N/A", "(missing)")`.
#'
#' @export
string_missing <- function(){
  c("", " ", ".", "-", "NA", "na", "n/a", "N/A", "(missing)")
}
