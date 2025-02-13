# 0 - functions -----------------------------------------------------------

compute_RMSE <- function(x, y, nb_hidden = 5, n_clusters = 2,
                             nodes_sim = c("sobol", "halton", "unif"),
                             activ = c("relu", "sigmoid", "tanh",
                                       "leakyrelu", "elu", "linear"),
                             lambda = 10^seq(-4, 5, length.out = 100),
                             k = 5, repeats = 1, seed = 1)
{
  stopifnot(is.wholenumber(nb_hidden))
  nodes_sim <- match.arg(nodes_sim)
  activ <- match.arg(activ)

  if(round(nrow(x)/k) < 3)
    warnings("Risk of having empty folds, pick a higher k")

  `%op%` <-  foreach::`%do%`

  if(is.wholenumber(repeats) && repeats > 1)
  {
    set.seed(seed)
    list_folds <- lapply(1:repeats,
                         function (i) caret::createFolds(y = y, k = k))

    i <- j <- NULL
    res <- foreach::foreach(j = 1:repeats, .combine = 'rbind',
                            .errorhandling = "stop",
                            verbose = TRUE)%op%{
      temp <- foreach::foreach(i = 1:k, .combine = 'rbind',
                               .errorhandling = "stop",
                               verbose = TRUE)%op%{
        train_index <- list_folds[[j]][[i]]

        test_index <- -train_index
        fit_obj <- fit_rvfl(x = x[train_index, ], y = y[train_index],
                            nodes_sim = nodes_sim, activ = activ,
                            nb_hidden = nb_hidden, n_clusters = n_clusters,
                            lambda = lambda, compute_Sigma = FALSE)


        predict_rvfl(fit_obj, newx = x[test_index, ]) - y[test_index]
      }
    }

    #return(res)
    return(sqrt(colMeans(res^2)))
  } else {

    set.seed(seed)
    folds <- caret::createFolds(y = y, k = k)

    res <- foreach::foreach(i = 1:k, .combine = rbind,
                            .errorhandling = "stop",
                            verbose = TRUE)%op%{
      train_index <- folds[[i]]

      test_index <- -train_index
      fit_obj <- fit_rvfl(x = x[train_index, ], y = y[train_index],
                          nodes_sim = nodes_sim, activ = activ,
                          nb_hidden = nb_hidden, n_clusters = n_clusters,
                          lambda = lambda, compute_Sigma = FALSE)


      predict_rvfl(fit_obj, newx = x[test_index, ]) - y[test_index]
    }

    #return(res)
    return(sqrt(colMeans(res^2)))
  }
}
compute_RMSE <- compiler::cmpfun(compute_RMSE)

# 1 - GCV -----------------------------------------------------------

# find regularization parameter and number of nodes with GCV
find_lam_nbhidden <- function(x, y, vec_nb_hidden = 1:100, # was 1:100
                              lams = 10^seq(-2, 10, length.out = 100),
                              activ = c("relu", "sigmoid", "tanh"))
{
  activ <- match.arg(activ)

  mat_GCV <- sapply(vec_nb_hidden,
                    function(i) bayesianrvfl::fit_rvfl(x = x, y = y,
                                                       nb_hidden = i, n_clusters = 0,
                                                       lambda = lams, activ = activ)$GCV)

  best_coords <- which(mat_GCV == min(mat_GCV), arr.ind = TRUE)

  return(list(best_lambda = lams[best_coords[1]],
              best_nb_hidden = vec_nb_hidden[best_coords[2]]))
}

# find regularization parameter and number of nodes with GCV
find_lam_nbhidden_nclusters <- function(x, y,
                                        nodes_sim = c("sobol", "halton", "unif"),
                                        activ = c("relu", "sigmoid", "tanh"))
{
  nodes_sim <- match.arg(nodes_sim)
  activ <- match.arg(activ)

  OF <- function(xx)
  {return(bayesianrvfl::fit_rvfl(x = x, y = y,
                                lambda = xx[1],
                                nb_hidden = floor(xx[2]),
                                n_clusters = floor(xx[3]),
                                nodes_sim = nodes_sim,
                                activ = activ)$GCV)}
  OF <- compiler::cmpfun(OF)

  minOF <- bayesianrvfl::msnlminb(objective = OF, nb_iter = 100,
                                  lower = c(0, 2, 2),
                                  upper = c(1e05, 100, 10))

  return(list(best_lambda = minOF$par[1],
              best_nb_hidden = as.integer(minOF$par[2]),
              best_n_clusters = as.integer(minOF$par[3]),
              objective = minOF$objective,
              convergence = minOF$convergence))
}


# 2 - CV -----------------------------------------------------------

cv_rvfl <- function(x, y, k = 5, repeats = 3,
                    nodes_sim = c("sobol", "halton", "unif"),
                    activ = c("relu", "sigmoid", "tanh",
                              "leakyrelu", "elu", "linear"),
                    vec_nb_hidden = sort(c(c(3, 4, 5, 10, 25, 40, 50),
                                           floor(1000*randtoolbox::sobol(15)))),
                    vec_n_clusters = seq(2, 10, by = 1),
                    lams = 10^seq(-4, 5, length.out = 10), seed = 1,
                    cl = NULL)
{
  x <- as.matrix(x)
  y <- as.vector(y)

  nodes_sim <- match.arg(nodes_sim)
  activ <- match.arg(activ)
  nodes_clusters_df <- expand_grid_df(vec_nb_hidden,
                                      vec_n_clusters)
  nb_iter <- nrow(nodes_clusters_df)

  allowParallel <- !is.null(cl) && cl > 0
  if(allowParallel)
  {
    cl_SOCK <- parallel::makeCluster(cl, type = "SOCK")
    doSNOW::registerDoSNOW(cl_SOCK)
    `%op%` <-  foreach::`%dopar%`

    pb <- txtProgressBar(min = 0, max = nb_iter, style = 3)
    progress <- function(n) utils::setTxtProgressBar(pb, n)
    opts <- list(progress = progress)

    i <- NULL
    res <- foreach::foreach(i = 1:nb_iter, .packages = c("doSNOW", "Rcpp"),
                            .combine = rbind, .errorhandling = "remove",
                            .options.snow = opts, .verbose = TRUE,
                            .export = c("compute_RMSE",
                                        "is.wholenumber",
                                        "createFolds",
                                        "fit_rvfl",
                                        "predict_rvfl",
                                        "create_new_predictors",
                                        "my_scale",
                                        "remove_zero_cols",
                                        "my_sd"))%op%
                                        {
                                          as.vector(compute_RMSE(x = x, y = y,
                                                                     nb_hidden = nodes_clusters_df[i, 1],
                                                                     n_clusters = nodes_clusters_df[i, 2],
                                                                     nodes_sim = nodes_sim, activ = activ,
                                                                     k = k, repeats = repeats,
                                                                     lambda = lams, seed = seed))
                                        }
    close(pb)
    snow::stopCluster(cl_SOCK)
  }  else {
    `%op%` <-  foreach::`%do%`

    pb <- txtProgressBar(min = 0, max = nb_iter, style = 3)

    i <- NULL
    res <- foreach::foreach(i = 1:nb_iter, .packages = "Rcpp",
                            .combine = rbind, .errorhandling = "remove",
                            .verbose = TRUE,
                            .export = c("compute_RMSE",
                                        "is.wholenumber",
                                        "createFolds",
                                        "fit_rvfl",
                                        "predict_rvfl",
                                        "create_new_predictors",
                                        "my_scale",
                                        "remove_zero_cols",
                                        "my_sd"))%op%
                                        {
                                          setTxtProgressBar(pb, i)
                                          as.vector(compute_RMSE(x = x, y = y,
                                                                     nb_hidden = nodes_clusters_df[i, 1],
                                                                     n_clusters = nodes_clusters_df[i, 2],
                                                                     nodes_sim = nodes_sim, activ = activ,
                                                                     k = k, repeats = repeats,
                                                                     lambda = lams, seed = seed))
                                        }
    close(pb)
  }





  #colnames(res) <- lams
  #rownames(res) <- vec_nb_hidden
  best_index <- which(res == min(res),
                      arr.ind = TRUE)

  return(list(best_lam = lams[best_index[2]],
              best_nb_hidden = nodes_clusters_df[best_index[1], 1],
              best_n_clusters = nodes_clusters_df[best_index[1], 2],
              objective = res[best_index[1], best_index[2]]))
}
