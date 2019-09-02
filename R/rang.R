#########################################################################################
# rang_roc_cut: ROC curve optimal cut for a ranger object
#########################################################################################
#'
#' ROC curve optimal cut for a ranger object
#'
#' Calls `roc_cut()` for a fitted `ranger()` model. See `roc_cut()`` for details.
#'
#' @param rf A ranger fitted model.
#' @param target A binary class target vector matching `rf`.
#' @param plot Produce a plot. Defaults to `TRUE`.
#'
#' @export
rang_roc_cut <- function(rf, target, plot = TRUE) {
  pred <- rf$predictions[, 1]
  roc_pred <- ROCR::prediction(pred, target)
  perf <- ROCR::performance(roc_pred, measure = "tpr", x.measure = "fpr")
  roc_cut(perf, roc_pred, plot = plot)
}
