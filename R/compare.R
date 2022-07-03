#' Pairwise element comparison of two variables in a dataframe
#'
#' Returns a table summarising the ordering of corresponding elements of two columns in a data frame. Columns
#' can be of different type or class but will then depend on coercion.
#'
#' @param df A data frame.
#' @param ... The two columns to be compared either by name (quoted or unquoted) or integer positions.
#' @param simple Logical. If TRUE then only checks for pairwise differences, not ordering.
#' @param tol When comparing numeric vectors, equality and inequality comparisons use this tolerance.
#' @param na.rm If `TRUE`, will ignore rows where either element is `NA`. The main reason to use
#'   this is if you want to see the proportion of non-missing entries that match/differ.
#'
#' @export
compare_vars <- function(df, ..., simple = FALSE, tol = 1E-6, na.rm = FALSE) {
  if (!is.data.frame(df)) {
    stop("`df` must be a data frame.", call. = FALSE)
  }
  df <- dplyr::select(df, ...)
  if (ncol(df) != 2){
    stop("Must select exactly two columns to compare.", call. = FALSE)
  }
  vec1 <- df[, 1, drop=T]
  vec2 <- df[, 2, drop=T]
  name1 <- names(df)[1]
  name2 <- names(df)[2]
  compare_vecs(vec1, vec2, names = c(name1, name2), simple = simple, tol = tol, na.rm = na.rm)
}

#' Pairwise element comparison of two vectors
#'
#' Returns a table summarising the ordering of corresponding elements of the two supplied vectors. Inputs can be
#' factors but must be atomic and the same length. If the vectors are of different classes or types then the
#' comparison will depend on coercion.
#'
#' @param x,y The two vectors to be compared. Both vectors must be atomic and of the same length.
#' @param names Optional length 2 string vector of names for the input vectors, to be used in the output
#'   table. Otherwise, "x" and "y" are used.
#' @param simple Logical. If TRUE then only checks for pairwise differences, not ordering.
#' @param tol When comparing numeric vectors, equality and inequality comparisons use this tolerance.
#' @param na.rm If `TRUE`, will ignore indices where either vector is `NA`. The main reason to use
#'   this is if you want to see the proportion of non-missing entries that match/differ.
#'
#' @export
compare_vecs <- function(x, y, names = NULL, simple = FALSE, tol = 1E-6, na.rm = FALSE){
  if (!(is.atomic(x) & is.atomic(y))){
    stop("Both vectors must be atomic.", call. = FALSE)
  }
  if (length(x) != length(y)){
    stop("x and y must be the same length.", call. = FALSE)
  }
  if(is.factor(x)) x <- levels(x)[x]
  if(is.factor(y)) y <- levels(y)[y]
  name1 <- "x"
  name2 <- "y"
  if (!is.null(names)) {
    if(length(names) == 2 & is.character(names)){
      name1 <- names[1]
      name2 <- names[2]
    }else{
      warning('Argument "names" must be a length 2 character vector. Using "x" and "y" instead.', call. = FALSE)
    }
  }
  if (class(x)[1] != class(y)[1]){
    warning("Vectors have different classes: ", class(x)[1], " and ", class(y)[1], ".", call. = FALSE)
  }
  if (na.rm){
    either_na <- is.na(x) | is.na(y)
    x <- x[!either_na]
    y <- y[!either_na]
  }
  both_num <- is.numeric(x) && is.numeric(y)
  both_na <- is.na(x) & is.na(y)
  if (both_num){
    equal <- sum(abs(x - y) <= tol, na.rm = T)
    hi <- sum(x - y > tol, na.rm = T)
    lo <- sum(y - x > tol, na.rm = T)
  }else{
    equal <- sum(x == y, na.rm = T)
    hi <- sum(x > y, na.rm = T)
    lo <- sum(x < y, na.rm = T)
  }
  if(simple){
    same = sum(both_na) + equal
    return(tibble(comparison = c("Match", "Different"),
                  count = c(same, length(x) - same),
                  prop = .data$count / length(x)))
  }
  comparison_names <- c(paste(name1, c("==", ">", "<"), name2),
                        "Both NA",
                        paste(c(name1, name2), "NA only"))
  count_vals <- c(equal, hi, lo, sum(both_na), sum(!both_na & is.na(x)), sum(!both_na & is.na(y)))
  tbl <- tibble(comparison = comparison_names,
                count = count_vals,
                prop = .data$count / length(x))
  if (!na.rm) return(tbl)
  tbl[1:3, ]
}

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

#' Compare two sets for overlap and differences.
#'
#' Compares two vectors and returns information on their overlap and differences, ignoring
#' ordering and duplicated elements.
#'
#' A tibble is returned with contents dependent on the argument `summary`. If `TRUE`,
#' counts of elements in both sets and only one set is given. If `FALSE`, the returned tibble
#' will have a row for each element in the union of `x` and `y`, together with which set(s)
#' it belongs to.
#'
#' @param x,y Vectors to compare.
#' @param summary Logical, controls what information is returned. See details.
#'
#' @export
compare_sets <- function(x, y, summary = TRUE) {
  if (!is.atomic(x) && !is.atomic(x)){
    stop("Both `x` and `y` must be atomic vectors.", call. = FALSE)
  }
  both <- intersect(x, y)
  just_x <- setdiff(x, y)
  just_y <- setdiff(y, x)
  union <- union(x, y)
  if(summary){
    return(tibble(set = c("both", "just x", "just y"),
                  count = c(length(both), length(just_x), length(just_y)),
                  prop = .data$count / length(union)))
  }
  tibble(element = union,
         both = union %in% both,
         just_x = union %in% just_x,
         just_y = union %in% just_y)
}
