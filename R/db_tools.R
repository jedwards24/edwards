
#########################################################################################
# is_one2one: Check if specified columns in a dataframe have one-to-one relationship.
#########################################################################################
#'
#' Check data frame columns for one-to-one relationship.
#'
#' Returns `TRUE` if all chosen columns are one-to-one and `FALSE` otherwise.
#' A message gives the first column found with a many-to relationship with another column.
#'
#' @param df A data frame.
#' @param ... Two or more columns to be compared either by name (quoted or unquoted) or integer positions.
#'
#' @export
is_one2one <- function(df, ...) {
  if (!is.data.frame(df)) {
    stop("`df` must be a data frame.", call. = FALSE)
  }
  counts_all <- dplyr::select(df, ...) %>%
    dplyr::group_by_all() %>%
    dplyr::tally() %>%
    dplyr::ungroup()
  for (col in 1 : (ncol(counts_all) - 1)){
    max_count <- dplyr::group_by_at(counts_all, {{col}}) %>%
      dplyr::tally() %>%
      dplyr::pull(n) %>%
      max()
    if (max_count > 1){
      message("Column ", names(counts_all)[col], " has a many-to relationship.")
      return(FALSE)
    }
  }
  TRUE
}


#########################################################################################
# fd_cols: Find candidate functional dependencies of a set of columns
#########################################################################################
#'
#' Find all size one candidate functional dependencies of a set of columns.
#'
#' Returns names of columns in a data frame that may be individually functionally determined a supplied set
#' of columns (the determinant set). This means that each returned column takes a single value for each unique
#' combination in the determinant set of columns.
#'
#' Any `NA`s are treated as a distinct value and a warning is given.
#'
#' This is far from optimised and can be slow with large data frames. The run time is approximately of order
#' (number of columns not in the determinant set) x (number of unique rows (groups) in the determinant set).
#' I found that 10,000 groups takes about 0.5 seconds per column.
#'
#' @param df A data frame.
#' @param ... Columns in determinant set. Given by either by name (quoted or unquoted) or integer positions.
#'
#' @export
fd_cols <- function(df, ...) {
  if (!is.data.frame(df)) {
    stop("`df` must be a data frame.", call. = FALSE)
  }
  if (any(is.na(df))) warning("`df` contains missing values.", call. = FALSE)
  group_cols <- names(dplyr::select(df, ...))
  same <- df %>%
    dplyr::group_by(!!!rlang::syms(group_cols)) %>%
    dplyr::summarise_all(function(x)  isTRUE(all(x==x[1]) | all(is.na(x)))) %>%
    dplyr::ungroup() %>%
    dplyr::select(-tidyselect::all_of(group_cols))
  all_same <- purrr::map_int(same, all)
  c(group_cols, names(all_same[all_same == TRUE]))
}

