# Temporary function since renaming.
#' @noRd
count_string <- function(...){
  warning("`count_string() is depreciated. Use count_pattern() instead (renamed).", call. = FALSE)
  count_pattern(...)
}

#########################################################################################
# prop_ci: Confidence intervals for binary target variable by values of a discrete predictor.
#########################################################################################
#'
#' Confidence intervals for binary target variable by values of a discrete predictor
#'
#' For a data frame input, one variable is a binary target (`target_name`) and another is selected to
#' be a predictor variable (`var_name`). Mean response and a confidence interval is calculated for the
#' target variable for each level or value of the predictor. The results are plotted and returned as a
#' table. The function most appropriate for factor predictors but will work with other variable types also.
#'
#' The target variable must be binary. Top compute confidence intervals this is converted to 0 and 1 values.
#' If it is not obvious which value corresponds to 1 and which to 0 then it will be based on level order
#' if a factor and the first observation otherwise. Giving the value of corresponding to 1 in the argument
#' `pos_class` will override this.
#'
#' Use the `plot` and `return_plot` arguments to control output. By default (designed to be
#'  used interactively) returns a table and prints a plot.  If `return_plot = TRUE` then just the
#'  plot is returned. If  `return_plot = FALSE` and
#'  `plot = FALSE` then the table is returned and no plot is generated. The default
#'
#' @param dt A data frame.
#' @param target_name Column to use as target variable. Column name (quoted or unquoted) or position.
#' @param var_name Column to use as predictor variable. Column name (quoted or unquoted) or position.
#' @param min_n Integer >= 1. Predictor levels with less than `min_n` observations are not displayed.
#' @param show_all Logical. Defaults to `TRUE`. If `FALSE` will not show levels whose confidence interval
#'   overlaps the mean response of all observations.
#' @param order_n Logical. Whether to force plot and table to order by number of observations of the
#'   predictior. The default setting `NULL` retains ordering if predictor is numeric or ordered factor
#'   and orders by number of observations otherwise.
#' @param conf_level Numeric in (0,1). Confidence level used for confidence intervals.
#' @param prop_lim Optional x axis limits passed to `ggplot()` e.g. `c(0,1)`.
#' @param pos_class Optional. Specify value in target to associate with class 1.
#' @param plot Optional logical. Output a plot or not.
#' @param return_plot Optional logical. If `TRUE` the plot is returned instead of the table (this overrides
#'   `plot` argument).
#'
#' @importFrom ggplot2 aes
#' @export
prop_ci <- function(dt, target_name, var_name, min_n = 1, show_all = TRUE, order_n = NULL,
                    conf_level = 0.95, prop_lim = NULL, pos_class = NULL, plot = TRUE,
                    return_plot = FALSE) {
  lifecycle::deprecate_warn("0.3.1", "prop_ci()", "response::response()")
  if (!is.data.frame(dt)) stop("`dt` must be a data frame.", call. = FALSE)
  y_label <- names(dplyr::select(dt, {{var_name}})) #for plot
  dt <- dplyr::rename(dt,
                      target = {{target_name}},
                      var = {{var_name}})
  if (any(is.na(dt$target))){
    dt <- dplyr::filter(dt, !is.na(.data$target))
    message("There are NA values in target variable. These rows will be excluded from any calculations.")
  }
  targ_vec <- dplyr::pull(dt, .data$target)
  # target vector checks
  if (length(unique(targ_vec)) > 2L){
    stop("Target variable must be binary.", call. = FALSE)
  }
  if (is.factor(targ_vec) && all(targ_vec %in% c("TRUE", "FALSE"))) targ_vec <- as.logical(targ_vec) #use expected pos class if factor
  if (!all(targ_vec %in% c(0, 1))){
    if (is.null(pos_class)) pos_class <- as.vector(targ_vec)[1]
    neg_class <- setdiff(as.vector(targ_vec), pos_class)
    targ_vec <- ifelse(targ_vec == pos_class, 1L, 0L)
    message("Target not binary or logical. Treating ",
            pos_class,
            " as 1 and ",
            neg_class,
            " as 0.\nUse 'pos_class' argument to set different value for class 1.")
  }
  if(is.factor(targ_vec)){
    targ_vec <- as.integer(levels(targ_vec))[targ_vec]
  }
  dt <- dplyr::mutate(dt, target = targ_vec)
  if (is.factor(dt$var) && !("(Missing)" %in% dt$var)){
    dt <- dplyr::mutate(dt, var = forcats::fct_explicit_na(.data$var))
  }
  if (is.null(order_n)){
    order_n <- if (is.numeric(dt$var) || is.ordered(dt$var)) FALSE else TRUE
  }

  mean_all <- mean(targ_vec)

  dt_summ <- dt %>%
    dplyr::group_by(.data$var) %>%
    dplyr::summarise(n = dplyr::n(),
                     n_pos = sum(.data$target),
                     prop = mean(.data$target)) %>%
    dplyr::rename(value = .data$var) %>%
    dplyr::arrange(dplyr::desc(.data$n)) %>%
    dplyr::mutate(lo = binom::binom.wilson(.data$prop * .data$n, .data$n, conf.level = conf_level)[['lower']],
                  hi = binom::binom.wilson(.data$prop * .data$n, .data$n, conf.level = conf_level)[['upper']],
                  sig = dplyr::case_when(
                    .data$lo > mean_all ~ 3,
                    .data$hi < mean_all ~ 1,
                    T ~ 2),
                  sig = factor(.data$sig, levels = c(1, 2, 3), labels = c("lo", "none", "hi"))
    ) %>%
    dplyr::filter(.data$n >= min_n) %>%
    purrr::when(!show_all ~ filter(., .data$sig != "none"), ~.) %>%
    purrr::when(order_n ~ dplyr::arrange(., dplyr::desc(.data$n)), ~dplyr::arrange(., .data$value))
  if (order_n){
    dt_plot <- dplyr::mutate(dt_summ, value = stats::reorder(.data$value, .data$n))
  }else{
    dt_plot <- dt_summ
  }
  if (plot || return_plot){
    cols <- c("#F8766D", "#00BA38", "#619CFF")
    gg <- dt_plot %>%
      ggplot2::ggplot(aes(x = .data$value, y = .data$prop, color = .data$sig)) +
      ggplot2::geom_point() +
      ggplot2::geom_errorbar(aes(ymin = .data$lo, ymax = .data$hi)) +
      ggplot2::coord_flip() +
      ggplot2::geom_hline(yintercept = mean_all, linetype = 2) +
      ggplot2::ylab("Mean Proportion Target") +
      ggplot2::xlab(y_label) +
      ggplot2::theme(legend.position = "none") +
      ggplot2::scale_colour_manual(values = c("lo" = cols[1], "none" = cols[3], "hi" = cols[2])) +
      {if(all(!is.null(prop_lim))) ggplot2::ylim(prop_lim[1], prop_lim[2])}
    if (!return_plot) print(gg)
  }
  if (return_plot) gg else dt_summ
}

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
  lifecycle::deprecate_warn("0.3.1", "glmnet_to_table()", "jemodel::coef_to_table()")
  ce <- coef(fit, ...)
  coef_mat <- as.matrix(ce)
  tibble::tibble(name = rownames(coef_mat), coef = coef_mat[, 1]) %>%
    dplyr::filter(abs(coef) >= min_coef) %>%
    dplyr::arrange(dplyr::desc(coef))
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
               FUN = count_matches_simple,
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
