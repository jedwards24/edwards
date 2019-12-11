#########################################################################################
# compare_vars: Summarise pairwise element comparison of two variables in a dataframe.
#########################################################################################
#'
#' Pairwise element comparison of two variables in a dataframe
#'
#' Returns a table summarising the ordering of corresponding elements of two columns in a data frame. Columns
#' can be of different type or class but will then depend on coercion.
#'
#' @param df A data frame.
#' @param var1,var2 The two columns to be compared, either string names or integer positions.
#' @param simple Boolean. If TRUE then only checks for pairwise differences, not ordering.
#' @param tol When comparing numeric vectors, equality and inequality comparisons use this tolerance.
#'
#' @export
compare_vars <- function(df, var1, var2, simple = F, tol = 1E-6) {
  vec1 <- df[, var1, drop=T]
  vec2 <- df[, var2, drop=T]
  name1 <- if (is.numeric(var1)) names(df)[var1] else var1
  name2 <- if (is.numeric(var2)) names(df)[var2] else var2
  compare_vecs(vec1, vec2, names = c(name1, name2), simple = simple, tol = tol)
}

#########################################################################################
# compare_vecs: Summarise pairwise element comparison of two vectors.
#########################################################################################
#'
#' Pairwise element comparison of two vectors
#'
#' Returns a table summarising the ordering of corresponding elements of the two supplied vectors. Inputs can be
#' factors but must be atomic and the same length. If the vectors are of different classes or types then the
#' comparison will depend on coercion.
#'
#' @param x,y The two vectors to be compared. Both vectors must be atomic and of the same length.
#' @param names Optional length 2 string vector of names for each vector. Otherwise, "x" and "y" are used.
#' @param simple Boolean. If TRUE then only checks for pairwise differences, not ordering.
#' @param tol When comparing numeric vectors, equality and inequality comparisons use this tolerance.
#'
#' @export
compare_vecs <- function(x, y, names = NULL, simple = F, tol = 1E-6){
  if (!(is.atomic(x) & is.atomic(y))){
    stop("Both vectors must be atomic.")
  }
  if (length(x) != length(y)){
    stop("x and y must be the same length.")
  }
  if(is.factor(x)) x <- levels(x)[x]
  if(is.factor(y)) y <- levels(y)[y]
  if (is.null(names)) {
    name1 <- "x"
    name2 <- "y"
  }else{
    name1 <- names[1]
    name2 <- names[2]
  }
  if (class(x) != class(y)){
    warning("Vectors have different classes: ", class(x), " and ", class(y), ".", call. = FALSE)
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
    return(tibble::tibble(comparison = c("Match", "Different"),
                          count = c(same, length(x) - same),
                          prop = count / length(x)
    ))
  }
  tibble::tibble(comparison = c(paste(name1, "==", name2),
                                paste(name1, ">", name2),
                                paste(name1, "<", name2),
                                "Both NA",
                                paste(name1, "NA only"),
                                paste(name2, "NA only")
  ),
  count = c(equal,
            hi,
            lo,
            sum(both_na),
            sum(!both_na & is.na(x)),
            sum(!both_na & is.na(y))
  ),
  prop = count / length(x)
  )
}

#########################################################################################
# find_similar: Counts pairwise matches between columns between two data frames or within a single
# data frame.
#########################################################################################
#'
#' Returns a table of counts of pairwise matches between columns of two data frames
#'
#' This is used to identify columns in the two data frames that might be the same. This will only be meaningfull
#' if the rows of the two data frames correspond to each other in some way i.e. they are sorted appropriately.
#'
#' The returned table summarises results with a row for each pair of columns with matching classes. There are
#' counts for: matches, both zero, one or both is \code{NA}, and differences. The proportion of non-zero matches
#' is also given. This is the number of non-zero matches divided by the number of element pairs that don't
#' contain an \code{NA} and are not both zero. Excluding matches which are both zeroes makes it easier to see
#' genuinely similar columns in data that contains lots of zeroes or missing values.
#'
#' @param df1,df2 Two data frames with matching number of rows. If the argument \code{df2} is missing
#' then only columns within \code{df1} will be compared.
#'
#' @export
find_similar <- function(df1, df2 = NULL){
  if (is.null(df2)) return(find_similar_single(df1))
  if (nrow(df1) != nrow(df2)) stop("The inputs df1 and df2 must have the same number of rows.", call. = FALSE)
  classes1 <- sapply(df1, function(x) class(x)[1])
  classes2 <- sapply(df2, function(x) class(x)[1])
  counts1 <- table(classes1)
  counts2 <- table(classes2)
  counts1 <- counts1[names(counts1) %in% names(counts2)]
  counts2 <- counts2[names(counts2) %in% names(counts1)]
  sz <- sum(counts1 * counts2)
  res <- tibble(var1 = NA_character_,
                var2 = NA_character_,
                class = NA_character_,
                match = NA_integer_,
                match_zero = NA_integer_,
                both_na = NA_integer_,
                na_1 = NA_integer_,
                na_2 = NA_integer_,
                .rows = sz
  )
  nm1 <- names(df1)
  nm2 <- names(df2)
  nvar <- length(nm1)
  k <- 1
  for (i in 1 : nvar){
    x <- pull(df1, nm1[i])
    indsj <- which(classes1[i] == classes2)
    for (j in indsj){
      y <- pull(df2, nm2[j])
      res[k, 1] <- nm1[i]
      res[k, 2] <- nm2[j]
      res[k, 3] <- classes1[i]
      res[k, 4] <- sum(x == y, na.rm = T)
      res[k, 5] <- sum(x == 0 & y == 0, na.rm = T)
      res[k, 6] <- sum(is.na(x) & is.na(y))
      res[k, 7] <- sum(is.na(x) & !is.na(y))
      res[k, 8] <- sum(!is.na(x) & is.na(y))
      k <- k + 1
    }
  }
  res %>%
    mutate(diff = nrow(df1) - match - both_na - na_1 - na_2) %>%
    mutate(prop_match_nz = (match - match_zero) / (nrow(df1) - match_zero - both_na - na_1 - na_2))
}


#########################################################################################
# find_similar_single: Counts pairwise matches between columns in a data frame.
#########################################################################################
#'
#' Count pairwise matches between columns in a single data frame
#'
#' Is called from `find_similar()` when the second data frame argument is omitted.
#' See `find_similar()` documentation for details.
#'
#' @keywords internal
find_similar_single <- function(df){
  classes <- sapply(df, function(x) class(x)[1])
  counts <- table(classes)
  sz <- sum(counts * (counts - 1) / 2)
  res <- tibble(var1 = NA_character_,
                var2 = NA_character_,
                class = NA_character_,
                match = NA_integer_,
                match_zero = NA_integer_,
                both_na = NA_integer_,
                na_1 = NA_integer_,
                na_2 = NA_integer_,
                .rows = sz
  )
  nm <- names(df)
  nvar <- length(nm)
  k <- 1
  for (i in 1 : (nvar - 1)){
    x <- pull(df, nm[i])
    indsj <- which(classes[i] == classes) %>% .[. > i]
    for (j in indsj){
      y <- pull(df, nm[j])
      res[k, 1] <- nm[i]
      res[k, 2] <- nm[j]
      res[k, 3] <- classes[i]
      res[k, 4] <- sum(x == y, na.rm = T)
      res[k, 5] <- sum(x == 0 & y == 0, na.rm = T)
      res[k, 6] <- sum(is.na(x) & is.na(y))
      res[k, 7] <- sum(is.na(x) & !is.na(y))
      res[k, 8] <- sum(!is.na(x) & is.na(y))
      k <- k + 1
    }
  }
  res %>%
    mutate(diff = nrow(df) - match - both_na - na_1 - na_2) %>%
    mutate(prop_match_nz = (match - match_zero) / (nrow(df) - match_zero - both_na - na_1 - na_2))
}
