#' Count elements, by column in a data frame, which exactly match a value
#'
#' @description
#' Counts the number of entries of each column in a data frame that exactly match the supplied values.
#' No coercion is used so the type must also match (unless the column is a factor in which case
#' character values will be matched to levels).
#'
#' @returns A named integer vector or tibble depending on the `detail` argument.
#'
#' @param df A data frame.
#' @param values Vector of values to match.
#' @param all By default, columns or values with no matches are omitted from the output.
#'   Set `all=TRUE` to show all.
#' @param prop Default is to return counts of matches. Set `prop=TRUE` to return proportions of the number of rows.
#' @param detail Logical. If `TRUE` returns a tibble with counts for each element in `values`. If
#'   `FALSE` then a named vector of total matches per column is returned.
#' @examples
#' x <- data.frame(a = c("an", "banana", "candy"), b = c("on", "bon", "bonbon"), c = 1:3)
#' count_matches(x, "an", all = TRUE)
#' count_matches(x, "an")
#' count_matches(x, 1L)
#' count_matches(x, 1) # type must match
#' count_matches(x, "1") # type must match
#'
#' df <- data.frame(x1 = c("a", ".", ".", "a"),
#'                  x2 = c("-", "-", "b", "b"),
#'                  x3 = rep("z", 4),
#'                  x4 = c("n/a", "f", "f", ""))
#' strs <- c(".", "-", "n/a", "na", "")
#' count_matches(df, strs, all = TRUE)
#' count_matches(df, strs, all = TRUE, detail = TRUE)
#' count_matches(df, strs, detail = TRUE)
#' @export
count_matches <- function(df, values = string_missing(), all = FALSE, prop = FALSE, detail = FALSE) {
  if (!is.list(df)) {
    stop("Argument \"df\" must be a list.", call. = FALSE)
  }
  if (!detail){
    return(count_matches_simple(df, values, all = all, prop = prop))
  }
  tb <- lapply(values,
               FUN = count_matches_simple,
               df = df,
               all = TRUE,
               prop = FALSE) %>%
    dplyr::bind_rows()
  if (prop) tb <- tb / nrow(df)
  tb <- tb %>%
    dplyr::mutate(value = values) %>%
    dplyr::select(.data$value, dplyr::everything())

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

#' Count matches to a value in a vector
#'
#' Helper for `count_matches()`. Counts occurrences of `value` in `x`. The type must also match.
#' @param x Vector to search.
#' @param value Value to match.
#' @noRd
total_matches_vec <- function(x, value){
  if (typeof(x) == typeof(value) | (is.factor(x) & typeof(value) == "character")){
    sum(x %in% value, na.rm = TRUE)
  }else{
    0L
  }
}

#' Count elements, by column in a data frame, which exactly match a value
#'
#' Used in `count_matches()` when `detail = FALSE`.
#' @param df A data frame.
#' @param value A length one vector.
#' @param all By default variables with no matches are omitted from the output. Set `all=T` to show all.
#' @param prop Default is to return counts of matches. Set `prop=TRUE` to return proportions of the number of rows.
#' @noRd
count_matches_simple <- function(df, value, all = FALSE, prop = FALSE){
  vals <- vapply(df, total_matches_vec, integer(1), value = value)
  vals <- vals[vals > 0 | all]
  if (prop) vals <- vals / nrow(df)
  if(length(vals) == 0){
    message("No matches in the data.")
    invisible(vals)
  }else{
    vals
  }
}

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
#' count_pattern(x, "an", all = TRUE)
#' count_pattern(x, "an")
#' count_pattern(x, "b")
#' count_pattern(x, "1") # not matched to integers
#'
#' @export
count_pattern <- function(df, pattern, all = FALSE){
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
