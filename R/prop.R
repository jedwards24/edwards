#########################################################################################
# prop_ci: Confidence intervals for binary target variable by values of a discrete predictor.
#########################################################################################
#'
#' Confidence intervals for binary target variable by values of a discrete predictor.
#'
#' For a data frame input, one variable is a binary target (`target_name`) and another is selected to
#' be a predictor variable (`var_name`). Mean response and a confidence interval is calculated for the
#' target variable for each level or value of the predictor. The results are plotted and returned as a
#' table. The function most appropriate for factor predictors but will work with other variable types also.
#'
#' Currently requires various functions from various tidyverse packages.
#' * Different colours are used depending on whether the interval is above/below/overlapping the population
#'   mean. Currently sometimes inconsistent.
#' * Predictor levels are sorted by n but good to add option to keep usual ordering. Detect ordered factors?
#' * Doesn't handle NAs currently.
#' * Target must currently be a non-factor. Need to handle this.
#'
#' @param dt A data frame.
#' @param target_name String. Name of target variable. Must be binary (either TRUE/FALSE or 0/1).
#' @param var_name String. Name of predictor variable.
#' @param min_n Integer >= 1. Predictor levels with less than `min_n` observations are not displayed.
#' @param show_all Boolean. Defaults to TRUE. If FALSE will not show levels whose confidence interval
#'   overlaps the mean response of all observations.
#' @param conf.level Numeric in (0,1). Confidence level used for confidence intervals.
#' @param prop_lim Optional x axis limits passed to `ggplot()` e.g. `c(0,1)`.
#'
#' @export
prop_ci <- function(dt, target_name, var_name, min_n = 1, show_all = T, conf.level = 0.95, prop_lim = NULL) {
  mean_all <- mean(dt[[target_name]])
  dt_summ <- dt %>%
    group_by(!!as.name(var_name)) %>%
    summarise(n = n(), prop = mean(!!as.name(target_name))) %>%
    rename(value = !!as.name(var_name)) %>%
    arrange(desc(n)) %>%
    mutate(hi = binom::binom.wilson(prop * n, n, conf.level = conf.level)[['upper']],
           lo = binom::binom.wilson(prop * n, n, conf.level = conf.level)[['lower']],
           sig = case_when(
             lo > mean_all ~ 2,
             hi < mean_all ~ 1,
             T ~ 3),
           sig = factor(sig, levels = c(1, 2, 3), labels = c("lo", "hi", "none"))
    ) %>%
    filter(n >= min_n) %>%
    purrr::when(!show_all ~ filter(., sig != "none"), ~.) %>%
    arrange(desc(n))

  print(dt_summ, n = Inf)

  dt_summ %>%
    ggplot(aes(x = reorder(value, n), y = prop, color = sig)) +
    geom_point() +
    geom_errorbar(aes(ymin = lo, ymax = hi)) +
    coord_flip() +
    geom_hline(yintercept = mean_all, linetype = 2) +
    ylab("Mean Proportion Target") +
    xlab(var_name) +
    theme(legend.position = "none") +
    {if(all(!is.null(prop_lim))) ylim(prop_lim[1], prop_lim[2])}
}

