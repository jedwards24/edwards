#########################################################################################
# rang_roc_cut: ROC curve optimal cut for a ranger object.
#########################################################################################
#'
#' ROC curve optimal cut for a ranger object
#'
#' Calls \code{roc_cut()} for a fitted ranger model. See \code{roc_cut()} for details. Assumes
#'  \code{probability = TRUE} was used when the ranger model was fitted.
#'
#' @param rf A ranger fitted model.
#' @param target A binary class target vector matching \code{rf}.
#' @param plot Produce a plot. Defaults to \code{TRUE}.
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
#' Tune ranger models for mtry
#'
#' Fits \code{ranger()} models for a given range of values of mtry. Output is a table and graph giving
#' errors for OOB training data and optional validation data.
#'
#' @param data A data frame.
#' @param fmla A formula for the model.
#' @param mvec Integer vector of values of tuning parameter \code{mtry}.
#' @param train Optional integer vector giving rows indices to be used in training set. Remaining rows are used for
#'   validation. If default \code{train = NULL} is used then all data is used for training and there is no validation.
#' @param seed Integer. Random number seed used for fitting each model.
#' @param importance,num.trees,respect.unordered.factors Optional arguments passed to \code{ranger()}. Defaults are
#'   \code{"impurity"}, \code{500} and \code{TRUE} respectively.
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
# oob_errors: Helper for rang_oob_err().
#########################################################################################
#'
#' Helper for \code{rang_oob_err()}
#'
#' Returns vector of error indicators chosen by majority vote. Where the vote is split the entry wil be 0.5.
#' The entry will be \code{NaN} Where there are no out-of-bag predictions for a data point.
#'
#' @param oob_mat Matrix of out-of-bag individual tree class predicitions with an \code{NA} where the prediciton
#'   was in bag. Rows are observation, columns are trees.
#' @param n_trees Positive integer. Gives the number of trees to be used in the prediction (columns \code{1:n_trees}
#'   of \code{oob_mat} will be used).
#' @param target Vector of true classes (either 1 or 2).
#'
#' @keywords internal
oob_errors <- function(oob_mat, n_trees, target) {
  oob_mat2 <- oob_mat[, 1:n_trees, drop = F]
  err_mat <- sweep(oob_mat2, 1, target, FUN = `!=`)
  err_vec <- rowMeans(err_mat, na.rm = TRUE)
  splits <- err_vec == 0.5
  err_vec <- round(err_vec)
  err_vec[splits] <- 0.5
  err_vec
}

#########################################################################################
# rang_oob_err: Out-of-bag error rates by number of trees for a ranger random forest.
#########################################################################################
#'
#' Out-of-bag error rates by number of trees for a ranger random forest
#'
#' Returns a table of out-of-bag error rates for a ranger randomw forest using number of trees
#' from 10 to all trees in steps of 10.
#'
#' @param rf A ranger random forest object.
#' @param data A data frame used to fit \code{rf}.
#' @param n_trees_vec Optional vector of number of trees to get results for. Defaults to \code{1:ntrees}.
#'
#' @export
rang_oob_err <- function(rf, data, n_trees_vec = NULL) {
  nn <- nrow(data)
  ntr <- rf$num.trees
  if (is.null(n_trees_vec)){
    n_trees_vec <- seq(1, ntr, by = 10)
  }
  fmla <- as.character(rf$call)[2]
  target_name <- str_split(fmla, " ~")[[1]][1]
  target <- data[[target_name]] %>% as.numeric() #use classes in {1,2}

  # Convert inbag counts (list) to matrix
  inbag_mat <- matrix(0, nrow = nn, ncol = ntr)
  for (i in 1 : ntr){
    inbag_mat[, i] <- rf$inbag.counts[[i]]
  }
  oob_mat <- if_else(inbag_mat > 0, NA_real_, predict(rf, data, predict.all = T)$predictions) %>%
    matrix(., nrow = nrow(inbag_mat))

  len_ntv <- length(n_trees_vec)
  errs <- matrix(0L, nrow = nn, ncol = len_ntv)
  for (i in 1: len_ntv){
    errs[, i] <- oob_errors(oob_mat, n_trees = n_trees_vec[i], target = target)
  }
  res <- tibble(num.trees = n_trees_vec,
                total = colMeans(errs, na.rm = TRUE),
                class_1 = colMeans(errs[target == 1, ], na.rm = T),
                class_2 = colMeans(errs[target == 2, ], na.rm = T)
  )
  res_long <- gather(res, key = "pred", value = "error_rate", -num.trees)
  g <- ggplot(res_long, aes(x = num.trees, y = error_rate, color = pred)) +
    geom_line() +
    ylab("OOB Error Rate")
  print(g)
  res
}

