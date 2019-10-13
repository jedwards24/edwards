#########################################################################################
# prop_ci: Confidence intervals for binary target variable by values of a discrete predictor.
#########################################################################################
#'
#' Confidence intervals for binary target variable by values of a discrete predictor.
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
#' Currently requires various functions from various tidyverse packages. Some known issues:
#'
#' \itemize{
#'   \item Different colours are used depending on whether the interval is above/below/overlapping the population
#'     mean. Currently sometimes inconsistent.
#'   \item Predictor levels are sorted by n but good to add option to keep usual ordering. Detect ordered factors?
#'   \item Doesn't handle NAs currently.
#' }
#'
#' @param dt A data frame.
#' @param target_name String. Name of target variable.
#' @param var_name String. Name of predictor variable.
#' @param min_n Integer >= 1. Predictor levels with less than \code{min_n} observations are not displayed.
#' @param show_all Boolean. Defaults to \code{TRUE}. If \code{FALSE} will not show levels whose confidence interval
#'   overlaps the mean response of all observations.
#' @param conf.level Numeric in (0,1). Confidence level used for confidence intervals.
#' @param prop_lim Optional x axis limits passed to \code{ggplot()} e.g. \code{c(0,1)}.
#' @param pos_class Optional. Specify value in target to associate with class 1.
#'
#' @export
prop_ci <- function(dt, target_name, var_name, min_n = 1, show_all = T,
                     conf_level = 0.95, prop_lim = NULL, pos_class = NULL) {
  dt <- rename(dt,
               target = !!as.name(target_name),
               var = !!as.name(var_name))
  targ_vec <- pull(dt, target)

  if (length(unique(targ_vec)) > 2L){
    stop("Target variable must be binary.", call. = FALSE)
  }
  if (!all(targ_vec %in% c(0, 1))){
    if (is.null(pos_class)) pos_class <- as.vector(targ_vec)[1]
    neg_class <- setdiff(as.vector(targ_vec), pos_class)
    targ_vec <- ifelse(targ_vec == pos_class, 1, 0)
    message("Target not binary or logical. Treating ",
            pos_class,
            " as 1 and ",
            neg_class,
            " as 0.\nUse 'pos_class' argument to set different value for class 1.")
  }
  if(is.factor(targ_vec)){
    targ_vec <- as.numeric(levels(targ_vec))[targ_vec]
  }
  mean_all <- mean(targ_vec)
  dt <- mutate(dt, target = targ_vec)

  dt_summ <- dt %>%
    group_by(var) %>%
    summarise(n = n(), prop = mean(target)) %>%
    rename(value = var) %>%
    arrange(desc(n)) %>%
    mutate(lo = binom::binom.wilson(prop * n, n, conf.level = conf_level)[['lower']],
           hi = binom::binom.wilson(prop * n, n, conf.level = conf_level)[['upper']],
           sig = case_when(
             lo > mean_all ~ 2,
             hi < mean_all ~ 1,
             T ~ 3),
           sig = factor(sig, levels = c(1, 2, 3), labels = c("lo", "hi", "none"))
    ) %>%
    filter(n >= min_n) %>%
    purrr::when(!show_all ~ filter(., sig != "none"), ~.) %>%
    arrange(desc(n))

  g <- dt_summ %>%
    ggplot(aes(x = reorder(value, n), y = prop, color = sig)) +
    geom_point() +
    geom_errorbar(aes(ymin = lo, ymax = hi)) +
    coord_flip() +
    geom_hline(yintercept = mean_all, linetype = 2) +
    ylab("Mean Proportion Target") +
    xlab(var_name) +
    theme(legend.position = "none") +
    {if(all(!is.null(prop_lim))) ylim(prop_lim[1], prop_lim[2])}

  print(g)
  dt_summ
}

