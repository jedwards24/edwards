#' @keywords internal
#' @importFrom ggplot2 aes
#' @importFrom dplyr if_else
#' @importFrom rlang .data
#' @importFrom stats coef
#' @importFrom rlang :=
#' @importFrom tibble tibble
"_PACKAGE"

globalVariables(".")
globalVariables(c("both_na", "na_1", "na_2", "match_zero")) #used in find_similar() and find_similar_single()

# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
## usethis namespace: end
NULL
