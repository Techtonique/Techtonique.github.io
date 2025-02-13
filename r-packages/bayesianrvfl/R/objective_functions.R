# scaled branin function for testing ( --> [0, 1]^2 )
braninsc <- function(xx)
{
  x1_bar <- 15*xx[1] - 5
  x2_bar <- 15*xx[2]

  term1 <- (x2_bar - (5.1/(4*pi^2)) * x1_bar^2 + (5/pi)*x1_bar - 6)^2
  term2 <- 10*(1-1/(8*pi))*cos(x1_bar)
  z <- (term1 + term2 - 44.81) / 51.95
  return(z)
}
braninsc <- compiler::cmpfun(braninsc)

# Hartmann 6 ( --> [0, 1]^6 )
hart6sc <- function(xx)
{
  ##########################################################################
  #
  # HARTMANN 6-DIMENSIONAL FUNCTION, RESCALED
  #
  # Authors: Sonja Surjanovic, Simon Fraser University
  #          Derek Bingham, Simon Fraser University
  # Questions/Comments: Please email Derek Bingham at dbingham@stat.sfu.ca.
  #
  # Copyright 2013. Derek Bingham, Simon Fraser University.
  #
  # THERE IS NO WARRANTY, EXPRESS OR IMPLIED. WE DO NOT ASSUME ANY LIABILITY
  # FOR THE USE OF THIS SOFTWARE.  If software is modified to produce
  # derivative works, such modified software should be clearly marked.
  # Additionally, this program is free software; you can redistribute it
  # and/or modify it under the terms of the GNU General Public License as
  # published by the Free Software Foundation; version 2.0 of the License.
  # Accordingly, this program is distributed in the hope that it will be
  # useful, but WITHOUT ANY WARRANTY; without even the implied warranty
  # of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
  # General Public License for more details.
  #
  # For function details and reference information, see:
  # http://www.sfu.ca/~ssurjano/
  #
  ##########################################################################
  #
  # INPUT:
  #
  # xx = c(x1, x2, x3, x4, x5, x6)
  #
  ##########################################################################

  alpha <- c(1.0, 1.2, 3.0, 3.2)
  A <- c(10, 3, 17, 3.5, 1.7, 8,
         0.05, 10, 17, 0.1, 8, 14,
         3, 3.5, 1.7, 10, 17, 8,
         17, 8, 0.05, 10, 0.1, 14)
  A <- matrix(A, 4, 6, byrow=TRUE)
  P <- 10^(-4) * c(1312, 1696, 5569, 124, 8283, 5886,
                   2329, 4135, 8307, 3736, 1004, 9991,
                   2348, 1451, 3522, 2883, 3047, 6650,
                   4047, 8828, 8732, 5743, 1091, 381)
  P <- matrix(P, 4, 6, byrow=TRUE)

  xxmat <- matrix(rep(xx,times=4), 4, 6, byrow=TRUE)
  inner <- rowSums(A[,1:6]*(xxmat-P[,1:6])^2)
  outer <- sum(alpha * exp(-inner))

  y <- -outer
  return(y)
}
hart6sc <- compiler::cmpfun(hart6sc)

# Alpine 01 ( --> [-10, 10]^4 )
alpine01 <- function(x)
{
  sum(abs(x * sin(x) + 0.1 * x))
}
alpine01 <- compiler::cmpfun(alpine01)

# 1D function
test1Dfunction <- function(x)
{
  -(exp(-(x - 2)^2) + exp(-(x - 6)^2/10) + 1/(x^2 + 1))
}
test1Dfunction <- compiler::cmpfun(test1Dfunction)

# f_approx finds an approximation of the objective (cv error) for stratified
# subsets of dataset (x, y), and we have f_approx(x, s = 1) = f(x) (we want to avoid s = 1)
# x: covariates (typically, parameters of the objective function)
# y: response (objective function value at x)
# tune_grid: named hyperparams vector for method `method` (tuneGrid)
# s: fraction of data in x to be trained (tune_grid x s)
# method: fitting model to be optimized (from the caret list)
# number: number of folds for the cross-validation
# repeats: number of repeats for the cross-validation procedure
# seed: random seed for selection the fraction of data to be trained
# ...: additional parameters to be passed to caret::train or caret::trainControl
f_approx <- function(x, y, s = 0.5,
                     tune_grid = data.frame(sigma = 1e03, C = 1e-03),
                     method = "svmRadial",
                     metric = ifelse(is.factor(y), "Accuracy", "RMSE"),
                     seed = 123, ...)
{
  stopifnot(0 < s && s <= 1)
  stopifnot(nrow(x) == length(y))

  if (s < 1)
  {
    # fraction (s) of the dataset to be trained
    set.seed(seed)
    training_index <- drop(caret::createDataPartition(y = 1:nrow(x),
                                                      p = s,
                                                      list = FALSE))
    # subset the dataset
    x_train <- x[training_index, ]
    colnames(x_train) <- 1:ncol(x_train)
    y_train <- y[training_index]

  } else { # s == 1
    # subset the dataset
    x_train <- x
    colnames(x_train) <- 1:ncol(x_train)
    y_train <- y
  }

  # caret train parameters
  trControl <- caret::trainControl(method = "repeatedcv",
                                   number = 5L,
                                   repeats = 3L,
                                   allowParallel = FALSE,
                                   ...)

  set.seed(123) # doesn't change
  res <- suppressWarnings(caret::train(x = x_train, y = y_train,
                                       method = method, trControl = trControl,
                                       tuneGrid = tune_grid, metric = metric,
                                       verbose = FALSE, ...))

  # return
  if (is.factor(y)){
    return(list(results_details = res,
                results_metric = -res$results$Accuracy))
  } else {
    return(list(results_details = res,
                results_metric = res$results$RMSE))
  }
}

f_approx_parallel <- function(x, y, s = 0.5,
                              tune_grid = data.frame(nrounds = 100L, max_depth = 1L, eta = 0.01,
                                                     gamma = 0.01, colsample_bytree = 0.5,
                                                     min_child_weight = 1, subsample = 0.5),
                              method = "xgbTree",
                              metric = ifelse(is.factor(y), "Accuracy", "RMSE"),
                              allowParallel = TRUE,
                              seed = 123, ...)
{
  stopifnot(0 < s && s <= 1)
  stopifnot(nrow(x) == length(y))

  if (s < 1)
  {
    # fraction (s) of the dataset to be trained
    set.seed(seed)
    training_index <- drop(caret::createDataPartition(y = 1:nrow(x),
                                                      p = s,
                                                      list = FALSE))
    # subset the dataset
    x_train <- x[training_index, ]
    colnames(x_train) <- 1:ncol(x_train)
    y_train <- y[training_index]

  } else { # s == 1
    # subset the dataset
    x_train <- x
    colnames(x_train) <- 1:ncol(x_train)
    y_train <- y
  }

  # caret train parameters
  trControl <- caret::trainControl(#method = "repeatedcv",
                                   method = "cv",
                                   number = 5L,
                                   #repeats = 3L,
                                   allowParallel = allowParallel,
                                   ...)

  if (allowParallel)
  {
    cl <- 4
    cl_SOCK <- parallel::makeCluster(cl, type = "SOCK")
    doSNOW::registerDoSNOW(cl_SOCK)
  }

  set.seed(123) # doesn't change
  res <- suppressWarnings(caret::train(x = x_train, y = y_train,
                                       method = method, trControl = trControl,
                                       tuneGrid = tune_grid, metric = metric,
                                       verbose = FALSE, ...))

  if (allowParallel) stopCluster(cl_SOCK)

  # return
  if (is.factor(y)){
    return(list(results_details = res,
                results_metric = -res$results$Accuracy))
  } else {
    return(list(results_details = res,
                results_metric = res$results$RMSE))
  }
}

# cross-validation objective on usps dataset
cv_usps <- function(xx)
{
  bayesianrvfl::f_approx_parallel(x = ElemStatLearn::zip.train[,-1],
                                  y = as.factor(ElemStatLearn::zip.train[,1]), s = 1,
                                  # tune_grid = data.frame(nrounds = floor(xx[1]),
                                  #                        max_depth = floor(xx[2]),
                                  #                        eta = xx[3], gamma = xx[4],
                                  #                        colsample_bytree = xx[5],
                                  #                        min_child_weight = xx[6],
                                  #                        subsample = xx[7]),
                                  tune_grid = data.frame(max_depth = floor(xx[1]),
                                                         eta = xx[2], subsample = xx[3],
                                                         nrounds = 150,
                                                         gamma = 0.01,
                                                         colsample_bytree = 0.5,
                                                         min_child_weight = 1),
                                  method = "xgbTree", allowParallel = FALSE,
                                  seed = 123)
}

