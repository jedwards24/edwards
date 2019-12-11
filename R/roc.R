#########################################################################################
# roc_cut: Optimal cut for ROC curve.
#########################################################################################
#'
#' Optimal cut for ROC curve
#'
#' Calculates the "optimal" cutoff for a classifier from a ROC curve. This is the cutoff that minimises the
#' distance from the point (FPR = 0, TPR = 1). Also returns sensitivity and specitivity for this cutoff and
#' optionally plots the curve together with the distance measure.
#'
#' Adapted from https://www.r-bloggers.com/a-small-introduction-to-the-rocr-package/.
#'
#' @param perf A ROCR performance object.
#' @param pred A ROCR prediction object.
#' @param plot Boolean indicating whether to plot a ROC curve together with distance from the optimal corner.
#'
#' @export
roc_cut = function(perf, pred, plot = T) {
  x <- perf@x.values[[1]]
  y <- perf@y.values[[1]]
  p <- pred@cutoffs[[1]]
  d = x^2 + (y - 1)^2
  ind = which(d == min(d))

  # Plot
  if(plot){
    nn <- length(y)
    roc_dt <- tibble::tibble(x = rep(x, 2), key = c(rep("tpr", nn), rep("d", nn)), val = c(y, d))
    gg <- ggplot(roc_dt, aes(x = x, y = val)) +
      geom_line(aes(col = key)) +
      geom_abline(slope = 1, intercept = 0, linetype = 2) +
      xlab("False Positive Rate") +
      ylab("") +
      scale_color_discrete(name = "",
                           breaks=c("tpr", "d"),
                           labels=c("TPR", "Distance")) +
      theme_bw()
    print(gg)
  }
  c(sensitivity = y[[ind]],
    specificity = 1 - x[[ind]],
    cutoff = p[[ind]]
  )
}

###################
# Plots ROC curves for both training and test data (in ggplot).
# Inputs: pred - vector of prediction probabilities,
#         target - vector of dependent variables outcomes corresponding to pred.
#         train - indices of the target data set
#         test (optional) - indices of test data set (otherwise complement of train).
# A ggplot with both curves is returned. AUC values for each are printed.
###################
#########################################################################################
# roc_plot: Plots ROC curves for training and test data.
#########################################################################################
#'
#' Plots ROC curves for training and test data.
#'
#' Returns a ggplot of a ROC curve from prediction and target vectors. These are partitioned by the argument
#' `train` into two separate curves so that differences between train and test data can be seen. AUC for each
#' is printed. Functions from ROCR are used.
#'
#' @param pred A vector of prediction probabilities.
#' @param target A vector of outcome classes corresponding to `pred`.
#' @param train A vector of indices indicating which predictions are "training" data.
#' @param test Optional vector of indices indicating which predictions are "test" data. Defaults to `NULL`, in
#'   which case `test` will be set to the data not in `train`.
#'
#' @export
roc_plot <- function(pred, target, train, test = NULL) {
  if (is.null(test)) {
    test <- setdiff(1 : length(target), train)
  }
  roc_pred_train <- ROCR::prediction(pred[train], target[train])
  roc_pred_test <- ROCR::prediction(pred[test], target[test])

  perf_train <- ROCR::performance(roc_pred_train, measure = "tpr", x.measure = "fpr")
  perf_test <- ROCR::performance(roc_pred_test, measure = "tpr", x.measure = "fpr")

  #auc
  cat("AUC:",
      unlist(ROCR::performance(roc_pred_train, measure = "auc")@y.values),
      "for the training set and",
      unlist(ROCR::performance(roc_pred_test, measure = "auc")@y.values),
      "for the test set\n"
  )

  # Plot
  n_train <- length(perf_train@x.values[[1]])
  n_test <- length(perf_test@x.values[[1]])
  roc_dt <- tibble::tibble(x = c(perf_train@x.values[[1]], perf_test@x.values[[1]]),
                           y = c(perf_train@y.values[[1]], perf_test@y.values[[1]]),
                           set = c(rep("train", n_train), rep("test", n_test))
  )
  ggplot(roc_dt, aes(x = x, y = y)) +
    geom_line(aes(col = set)) +
    geom_abline(slope = 1, intercept = 0, linetype = 2) +
    xlab("False Positive Rate") +
    ylab("True Positive Rate") +
    theme_bw()
}
