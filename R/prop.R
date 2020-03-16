#########################################################################################
# prop_ci: Confidence intervals for binary target variable by values of a discrete predictor.
#########################################################################################
#'
#' Confidence intervals for binary target variable by values of a discrete predictor
#'
#' For a data frame input, one variable is a binary target (\code{target_name}) and another is selected to
#' be a predictor variable (\code{var_name}). Mean response and a confidence interval is calculated for the
#' target variable for each level or value of the predictor. The results are plotted and returned as a
#' table. The function most appropriate for factor predictors but will work with other variable types also.
#'
#' The target variable must be binary. Top compute confidence intervals this is converted to 0 and 1 values.
#' If it is not obvious which value corresponds to 1 and which to 0 then it will be based on level order
#' if a factor and the first observation otherwise. Giving the value of corresponding to 1 in the argument
#' \code{pos_class} will override this.
#'
#' @param dt A data frame.
#' @param target_name String. Name of target variable.
#' @param var_name String. Name of predictor variable.
#' @param min_n Integer >= 1. Predictor levels with less than \code{min_n} observations are not displayed.
#' @param show_all Logical. Defaults to \code{TRUE}. If \code{FALSE} will not show levels whose confidence interval
#'   overlaps the mean response of all observations.
#' @param order_n Logical. Whether to force plot and table to order by number of observations of the
#'   predictior. The default setting \code{NULL} retains ordering if predictor is numeric or ordered factor
#'   and orders by number of observations otherwise.
#' @param conf.level Numeric in (0,1). Confidence level used for confidence intervals.
#' @param prop_lim Optional x axis limits passed to \code{ggplot()} e.g. \code{c(0,1)}.
#' @param pos_class Optional. Specify value in target to associate with class 1.
#' @param plot Optional logical. Output a plot or not.
#'
#' @export
prop_ci <- function(dt, target_name, var_name, min_n = 1, show_all = TRUE, order_n = NULL,
                     conf_level = 0.95, prop_lim = NULL, pos_class = NULL, plot = TRUE) {
  dt <- rename(dt,
               target = !!as.name(target_name),
               var = !!as.name(var_name))
  if (any(is.na(dt$target))){
    dt <- filter(dt, !is.na(target))
    message("There are NA values in target variable. These rows will be excluded from any calculations.")
  }
  targ_vec <- pull(dt, target)

  if (length(unique(targ_vec)) > 2L){
    stop("Target variable must be binary.", call. = FALSE)
  }
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
  dt <- mutate(dt, target = targ_vec)
  if (is.factor(dt$var) && !("(Missing)" %in% dt$var)){
    dt <- mutate(dt, var = fct_explicit_na(var))
  }
  if (is.null(order_n)){
    order_n <- if (is.numeric(dt$var) || is.ordered(dt$var)) FALSE else TRUE
  }

  mean_all <- mean(targ_vec)

  dt_summ <- dt %>%
    group_by(var) %>%
    summarise(n = n(),
              n_pos = sum(target),
              prop = mean(target)) %>%
    rename(value = var) %>%
    arrange(desc(n)) %>%
    mutate(lo = binom::binom.wilson(prop * n, n, conf.level = conf_level)[['lower']],
           hi = binom::binom.wilson(prop * n, n, conf.level = conf_level)[['upper']],
           sig = case_when(
             lo > mean_all ~ 3,
             hi < mean_all ~ 1,
             T ~ 2),
           sig = factor(sig, levels = c(1, 2, 3), labels = c("lo", "none", "hi"))
    ) %>%
    filter(n >= min_n) %>%
    purrr::when(!show_all ~ filter(., sig != "none"), ~.) %>%
    purrr::when(order_n ~ arrange(., desc(n)), ~arrange(., value))
  if (order_n){
    dt_plot <- mutate(dt_summ, value = reorder(value, n))
  }else{
    dt_plot <- dt_summ
  }
  if (plot){
    cols <- c("#F8766D", "#00BA38", "#619CFF")
    g <- dt_plot %>%
      ggplot(aes(x = value, y = prop, color = sig)) +
      geom_point() +
      geom_errorbar(aes(ymin = lo, ymax = hi)) +
      coord_flip() +
      geom_hline(yintercept = mean_all, linetype = 2) +
      ylab("Mean Proportion Target") +
      xlab(var_name) +
      theme(legend.position = "none") +
      scale_colour_manual(values = c("lo" = cols[1], "none" = cols[3], "hi" = cols[2])) +
      {if(all(!is.null(prop_lim))) ylim(prop_lim[1], prop_lim[2])}
    print(g)
  }
  dt_summ
}
