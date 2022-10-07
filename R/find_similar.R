#' Count pairwise matches between columns of two data frames
#'
#' This is used to identify columns in the two data frames that might be the same. This will only be meaningfull
#' if the rows of the two data frames correspond to each other in some way i.e. they are sorted appropriately.
#'
#' The returned table summarises results with a row for each pair of columns with matching classes. There are
#' counts for: matches, both zero, one or both is `NA`, and differences. The proportion of non-zero matches
#' is also given. This is the number of non-zero matches divided by the number of element pairs that don't
#' contain an `NA` and are not both zero. Excluding matches which are both zeroes makes it easier to see
#' genuinely similar columns in data that contains lots of zeroes or missing values.
#'
#' @param df1,df2 Two data frames with matching number of rows. If the argument `df2` is missing
#' then only columns within `df1` will be compared.
#'
#' @export
find_similar <- function(df1, df2 = NULL){
  if (!is.data.frame(df1)){
    stop("`df1` must be a data frame.", call. = FALSE)
  }
  if (is.null(df2)) return(find_similar_single(df1))
  if (!is.data.frame(df2)){
    stop("`df2` must be a data frame.", call. = FALSE)
  }
  if (nrow(df1) != nrow(df2)){
    stop("The inputs df1 and df2 must have the same number of rows.", call. = FALSE)
  }
  classes_1 <- unname(purrr::map(df1, class))
  classes_2 <- unname(purrr::map(df2, class))
  niter <- ncol(df1)
  res_list <- vector("list", niter)
  for (i in seq_along(res_list)){
    inds_y <- which(purrr::map_lgl(classes_2, ~identical(., classes_1[[i]])))
    if (length(inds_y) == 0) next()
    x <- df1[[i]]
    df_y <- dplyr::select(df2, dplyr::all_of(inds_y))
    res_list[[i]] <- compare_cols_to_vector(x, df_y, names(df1)[i])
  }
  dplyr::bind_rows(res_list, blank_find_similar()) %>%
    dplyr::mutate_all(unname) %>%
    dplyr::mutate(diff = nrow(df1) - match - both_na - na_1 - na_2) %>%
    dplyr::mutate(prop_match_nz = (match - match_zero) / (nrow(df1) - match_zero - both_na - na_1 - na_2))
}

#' Count pairwise matches between columns in a single data frame
#'
#' Called from `find_similar()` when the second data frame argument is omitted.
#' See `find_similar()` documentation for details.
#' @param df Data frame.
#' @noRd
find_similar_single <- function(df){
  classes <- unname(purrr::map(df, class))
  niter <- max(ncol(df) - 1, 0)
  res_list <- vector("list", niter)
  for (i in seq_along(res_list)){
    inds_y <- which(purrr::map_lgl(classes, ~identical(., classes[[i]]))) %>% .[. > i]
    if (length(inds_y) == 0) next()
    x <- df[[i]]
    df_y <- dplyr::select(df, dplyr::all_of(inds_y))
    res_list[[i]] <- compare_cols_to_vector(x, df_y, names(df)[i])
  }
  dplyr::bind_rows(res_list, blank_find_similar()) %>%
    dplyr::mutate_all(unname) %>%
    dplyr::mutate(diff = nrow(df) - match - both_na - na_1 - na_2) %>%
    dplyr::mutate(prop_match_nz = (match - match_zero) / (nrow(df) - match_zero - both_na - na_1 - na_2))
}

#' Helper for `find_similar()`.
#'
#' Gets comparison tibble for similarity of vector `x` to each column in data frame `df_y`.
#' @param x Atomic vector of length equal not number of rows in `df_y`.
#' @param df_y Data frame.
#' @param name_x Length one character. Name to be used for `x` in output.
#' @noRd
compare_cols_to_vector <- function(x, df_y, name_x) {
  tibble(var1 = name_x,
         var2 = names(df_y),
         class = class(x),
         match = purrr::map_int(df_y, ~sum(. == x, na.rm = TRUE)),
         match_zero = purrr::map_int(df_y, ~sum(x == 0 & . == 0, na.rm = TRUE)),
         both_na = purrr::map_int(df_y, ~sum(is.na(x) & is.na(.))),
         na_1 = purrr::map_int(df_y, ~sum(is.na(x) & !is.na(.))),
         na_2 = purrr::map_int(df_y, ~sum(!is.na(x) & is.na(.))))
}

# Used in find_similar() and find_similar_single().
#' @noRd
blank_find_similar <- function() {
  tibble(var1 = character(0),
         var2 = character(0),
         class = character(0),
         match = integer(0),
         match_zero = integer(0),
         both_na = integer(0),
         na_1 = integer(0),
         na_2 = integer(0))
}
