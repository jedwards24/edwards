#########################################################################################
# compare_vars: Summarise pairwise element comparison of two variables in a dataframe.
#########################################################################################
#'
#' Summarise pairwise element comparison of two variables in a dataframe.
#'
#' Returns a table summarising the ordering of corresponding elements of two columns in a data frame. Columns
#' can be of different type or class but will then depend on coercion.
#'
#' @param df A data frame.
#' @param var1,var2 The two columns to be compared, either string names or integer positions.
#' @param simple Boolean. If TRUE then only checks for pairwise differences, not ordering.
#'
#' @export
compare_vars <- function(df, var1, var2, simple = F) {
  vec1 <- df[, var1, drop=T]
  vec2 <- df[, var2, drop=T]
  name1 <- if (is.numeric(var1)) names(df)[var1] else var1
  name2 <- if (is.numeric(var2)) names(df)[var2] else var2
  compare_vecs(vec1, vec2, names = c(name1, name2), simple = simple)
}

#########################################################################################
# compare_vecs: Summarise pairwise element comparison of two vectors.
#########################################################################################
#'
#' Summarise pairwise element comparison of two vectors.
#'
#' Returns a table summarising the ordering of corresponding elements of the two supplied vectors. Inputs can be
#' factors but must be atomic and the same length. If the vectors are of different classes or types then the
#' comparison will depend on coercion.
#'
#' @param x,y The two vectors to be compared. Both vectors must be atomic and of the same length.
#' @param names Optional length 2 string vector of names for each vector. Otherwise, "x" and "y" are used.
#' @param simple Boolean. If TRUE then only checks for pairwise differences, not ordering.
#'
#' @export
compare_vecs <- function(x, y, names = NULL, simple = F){
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
  both_na <- is.na(x) & is.na(y)
  equal <- sum(x == y, na.rm = T)
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
  count = c(sum(equal),
            sum(x > y, na.rm = T),
            sum(x < y, na.rm = T),
            sum(both_na),
            sum(!both_na & is.na(x)),
            sum(!both_na & is.na(y))
  ),
  prop = count / length(x)
  )
}
