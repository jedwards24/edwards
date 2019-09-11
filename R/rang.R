#########################################################################################
# rang_roc_cut: ROC curve optimal cut for a ranger object.
#########################################################################################
#'
#' ROC curve optimal cut for a ranger object.
#'
#' Calls `roc_cut()` for a fitted ranger model. See `roc_cut()` for details. Assumes `probability = TRUE` was
#' used when the ranger model was fitted.
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

# Fits a series of random forest models using the ranger package with different values of mtry, as given in m_vec.
# Returns a table of oob and test errors.

#########################################################################################
# rang_mtry: Tune ranger models for mtry.
#########################################################################################
#'
#' Tune ranger models for mtry.
#'
#' Fits `ranger()` models for a given range of values of mtry. Output is a table and graph giving
#' errors for OOB training data and optional validation data.
#'
#' @param data A data frame.
#' @param fmla A formula for the model.
#' @param mvec Integer vector of values of tuning parameter `mtry`.
#' @param train Optional integer vector giving rows indices to be used in training set. Remaining rows are used for
#'   validation. If default `train = NULL` is used then all data is used for training and there is no validation.
#' @param seed Integer. Random number seed used for fitting each model.
#' @param importance,num.trees,respect.unordered.factors Optional arguments passed to `ranger()`. Defaults are
#'   `"impurity`, `500`, and `TRUE` respectively.
#'
#' @export
rang_mtry <- function(data, fmla, m_vec, train = NULL, seed = 1, importance = "impurity", num.trees = 500,
                      respect.unordered.factors = T) {
  if (is.null(train)) train <- 1 : nrow(data)
  target_name <- fmla[[2]]
  valid <- pull(data, !!target_name)[-train]
  target <- pull(data, !!target_name)[train]
  nn <- length(m_vec)
  oob_err <- double(nn)
  valid_err <- double(nn)
  time <- double(nn)
  cat("mtry completed: ")
  for(i in 1 : nn) {
    mtry <- m_vec[i]
    set.seed(seed)
    time[i] <- system.time(rf <- ranger(fmla, data = data[train, ],
                                        importance = importance, num.trees = num.trees, mtry = mtry,
                                        respect.unordered.factors = respect.unordered.factors))[3]
    oob_err[i] <- rf$prediction.error
    if (length(valid) > 0) {
      pred <- predict(rf, data[-train, ])$predictions
      if(is.factor(target)){
        valid_err[i] <- mean(valid != pred)
      }else{
        valid_err[i] <- mean((valid - pred) ^ 2)
      }
    }
    cat(mtry," ")
  }
  cat("\n")
  res <- tibble(mtry = m_vec, valid_err, oob_err, time) %>%
    {if (length(valid) == 0) select(., -valid_err) else .}
  reslong <- gather(res, key = "metric", value = "error", -time, -mtry)
  g <- ggplot(reslong, aes(x = mtry, y = error, colour = metric)) +
    geom_line() +
    geom_point()
  print(g)
  res
}

#########################################################################################
# oob_pred: Helper for rang_oob_err.
#########################################################################################
#'
#' Helper for rang_oob_err.
#'
#' Returns vector of class predictions chosen by majority vote.
#'
#' Note predictions may include NaNs if there are no out-of-bag predictions for a data point.
#'
#' @param pred_mat Matrix of out-of-bag individual tree class predicitions with a zero where the prediciton
#'   was in bag. Rows are observation, columns are trees.
#' @param n_trees Positive integer. Gives the number of trees to be used in the prediction (columns `1:n_trees`
#'   of `pred_mat` will be used).
#'
oob_pred <- function(pred_mat, n_trees) {
  oob_mat <- pred_mat[, 1:n_trees, drop = F]
  zeroes <- apply(pred_mat, 1, FUN = function(x) sum(x > 0))
  score <- rowSums(pred_mat) / zeroes
  round(score)
}

#########################################################################################
# err_by_class: Error rate by class prediction.
#########################################################################################
#'
#' Error rate by class prediction.
#'
#' Returns the rate of mismatches between entries of `pred_vec` that equal `class` and the
#' corresponding elements of `target`.
#'
#' @param pred_vec Integer vector of class predictions.
#' @param target Integer vector of true classes.
#' @param class Integer indicating which class predictions to measure.
#'
#' @export
err_by_class <- function(pred_vec, target, class = 1) {
  inds <- which(pred_vec == class)
  mean(pred_vec[inds] != target[inds], na.rm = T)
}

#########################################################################################
# rang_oob_err: Out-of-bag error rates by number of trees for a ranger random forest.
#########################################################################################
#'
#' Out-of-bag error rates by number of trees for a ranger random forest.
#'
#' Returns a table of out-of-bag error rates for a ranger randomw forest using number of trees
#' from 10 to all trees in steps of 10.
#'
#' @param rf A ranger random forest object.
#' @param data A data frame used to fit `rf`.
#'
#' @export
rang_oob_err <- function(rf, data) {
  nn <- nrow(dt)
  ntr <- rf$num.trees
  # Convert inbag counts (list) to matrix
  inbag_mat <- matrix(0, nrow = nn, ncol = ntr)
  for (i in 1 : ntr){
    inbag_mat[, i] <- rf$inbag.counts[[i]]
  }
  oob_mat <- if_else(inbag_mat > 0, 0, predict(rf, data, predict.all = T)$predictions) %>%
    matrix(., nrow = nrow(inbag_mat))

  ntrees_vec <- seq(10, ntr, by = 10)
  preds_n_mat <- matrix(0L, nrow = nn, ncol = length(ntrees_vec))
  for (i in 1:length(ntrees_vec)){
    preds_n_mat[, i] <- oob_pred(oob_mat, ntrees_vec[i])
  }

  errs <- apply(preds_n_mat, 2, FUN = function(x, top) mean(x != top, na.rm = T), top = top)
  res <- tibble(num.trees = ntrees_vec,
                total = errs,
                class_1 = apply(preds_n_mat, 2, FUN = err_by_class, target = top, class = 1),
                class_2 = apply(preds_n_mat, 2, FUN = err_by_class, target = top, class = 2))
  res_long <- gather(res, key = "pred", value = "error_rate", -num.trees)
  g <- ggplot(res_long, aes(x = num.trees, y = error_rate, color = pred)) +
    geom_line() +
    ylab("OOB Error Rate")
  print(g)
  res
}
