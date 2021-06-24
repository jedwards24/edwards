#' Summarise coefficients from glmnet in a table
#'
#' Returns a tibble of coefficients for glmnet model `fit` with parameter `s`. Only
#' coefficients with absolute value greater than `min_coef` are included.
#'
#' This is a simplified version of `jemodel::glmnet_to_table()` kept here to provide some backwards
#' compatibility. Longer term it may be removed from this package.
#'
#' @param fit A fitted glmnet model.
#' @param min_coef Coefficients with smaller absolute value than this are excluded from the table.
#' @param ... Arguments passed to `coef()`. For `glmnet`, most commonly used will be `s` (see `predict.cv.glmnet()`).
#'
#' @export
glmnet_to_table <- function(fit, ..., min_coef=1E-10) {
  ce <- coef(fit, ...)
  coef_mat <- as.matrix(ce)
  level_names <- rownames(ce)
  tibble::tibble(name = rownames(coef_mat), coef = coef_mat[, 1]) %>%
    dplyr::filter(abs(coef) >= min_coef) %>%
    dplyr::arrange(dplyr::desc(coef))
}
