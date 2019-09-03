#########################################################################################
# rang_roc_cut: ROC curve optimal cut for a ranger object.
#########################################################################################
#'
#' ROC curve optimal cut for a ranger object.
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
