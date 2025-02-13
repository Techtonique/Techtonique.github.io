
#library(bayesianrvfl)

#  # on random dataset
   # n <- 1000 ; p <- 5
   # X <- matrix(rnorm(n * p), n, p) # no intercept!
   # y <- rnorm(n)
   #   (res <- f_approx(x = X, y = y))
#
#  # on iris dataset
#   data(iris)
#   TrainData <- iris[,1:4]
#   TrainClasses <- iris[,5]
#   (res <- f_approx(x = TrainData, y = TrainClasses))
#
#  # initial design
  # (df <- initial_design(f_approx = f_approx, x = TrainData, y = TrainClasses,
  #                     lower_df = data.frame(sigma = 1e-05, C = 1e-05),
  #                     upper_df = data.frame(sigma = 1e05, C = 1e05),
  #                     nb_init = 5, nb_s = 4, seed = 12))
  #  dplyr::filter(df$init_design, s == 0.375)
  #  dplyr::filter(df$init_design, s == 0.25)
#
# # use other activ
#  (best_params <- bayesianrvfl::find_lam_nbhidden(x = df$init_design[, -ncol(df$init_design)],
#                    y = df$init_design[, "cv_error"], activ = "tanh"))
#
# # use other activ
# fit_obj <- bayesianrvfl::fit_rvfl(x = df$init_design[, -ncol(df$init_design)],
#                        y = df$init_design[, "cv_error"],
#                        nb_hidden = best_params$best_nb_hidden,
#                        lambda = best_params$best_lambda,
#                        activ = "tanh",
#                        compute_Sigma = TRUE)
#
# OF <- function(x) bayesianrvfl::predict_rvfl(fit_obj, newx = cbind(matrix(x, nrow = 1), 1))$mean
#
# # # projected cv error on whole dataset (high variance...)
#  index_best <- which.min(apply(df$hyper_params, 1, OF))
#  (x_best <- df$hyper_params[index_best, ])
#  (f_best <- OF(x_best))

bayes_opt_largedata <- function(x, y, df_init_design = NULL, # x = covariates training set, y = response training set
                                 lower_df = data.frame(sigma = 1e-05, C = 1e-05), # hyperparams bounds for 'fit_method'
                                 upper_df = data.frame(sigma = 1e06, C = 1e06), # hyperparams bounds for 'fit_method'
                                # lower_df = data.frame(nrounds = 50L, max_depth = 1L,
                                #                       eta = 0.01, gamma = 0.01,
                                #                       colsample_bytree = 0.5,
                                #                       min_child_weight = 1, subsample = 0.5),
                                # upper_df = data.frame(nrounds = 250L, max_depth = 6L,
                                #                       eta = 0.5, gamma = 0.5,
                                #                       colsample_bytree = 1,
                                #                       min_child_weight = 2, subsample = 1),
                                nb_init = 10, s_max = 0.5, nb_s = 4L, # params for constructing the initial design
                                seed = 123, # for initial design
                                fit_method = "svmRadial", # caret training methods on training set (x, y)
                                #fit_method = "xgbTree", # caret training methods on training set (x, y)
                                metric = ifelse(is.factor(y), "Accuracy", "RMSE"), # metric for cv error on training set (x, y) (on caret training methods)
                                type_acq = c("ei", "ucb"), kappa = 1.96, # acquistion function
                                method = c("standard", "direct_online"), # '*_online' available for rvfl only
                                surrogate_model = c("rvfl", "matern52"), optim_surr = c("GCV", "loglik"),
                                activation_function = c("relu", "tanh", "sigmoid"), # activation function for surrogate_model == "rvfl"
                                nb_iter = 25,
                                verbose = TRUE,
                                show_progress = TRUE,
                                record_points = FALSE, cl = 4, ...)
{
  stopifnot(0 < s_max && s_max <= 1)
  nb_is_found <- 0
  dim_xx <- length(lower)
  stopifnot(dim_xx == length(upper))
  type_acq <- match.arg(type_acq)
  method <- match.arg(method)
  activation_function <- match.arg(activation_function)
  optim_surr <- match.arg(optim_surr)
  surrogate_model <- match.arg(surrogate_model)
  lower <- as.numeric(lower_df)
  upper <- as.numeric(upper_df)
  next_param_cv_error <- rep(0, nb_s)
  is_integer_lower <- is_integer_params(lower_df)
  is_integer_upper <- is_integer_params(upper_df)
  if(!all.equal(is_integer_lower, is_integer_upper))
  {
    stop("integer upper and lower bounds required")
  }

  # define initial design -----
    if(is.null(df_init_design)) {
        cat("\n", "----- define initial design...", "\n")
        df_init_design <- initial_design(x = x, y = y,
                                         method = fit_method, metric = metric,
                                         lower_df = lower_df, upper_df = upper_df,
                                         nb_init = nb_init, s_max = s_max, nb_s = nb_s,
                                         seed = seed, cl = cl)
    }

    if (verbose == TRUE)
    {
      print(df_init_design)
      cat("\n", "----- initial design", "\n")
      print(df_init_design$init_design)
      cat("\n")
    }

      # initial design for hyperparams ((hyperparams x s))
      x_design <- df_init_design$init_design[, -ncol(df_init_design$init_design)]

      # cv error associated to (hyperparams x s) on the initial design
      y_design <- df_init_design$init_design[, "cv_error"]
  # end define initial design

  # define surrogate and surrogate objective function  -----
    if (optim_surr == "GCV")
    {
      # optimal params for the surrogate
      best_params <- bayesianrvfl::find_lam_nbhidden(x = x_design,
                                                     y = y_design,
                                                     activ = activation_function)
      best_lam <- best_params$best_lambda
      best_nb_hidden <- best_params$best_nb_hidden

      # objective function = approx of f = f_approx(x, s = 1)
      fit_obj_surr <- bayesianrvfl::fit_rvfl(x = x_design,
                                             y = y_design,
                                             nb_hidden = best_nb_hidden,
                                             lambda = best_lam,
                                             activ = activation_function,
                                             compute_Sigma = TRUE)

      if (is.factor(y))
      {
        OF <- function(x, ...) max(-1, suppressWarnings(bayesianrvfl::predict_rvfl(fit_obj_surr,
                                                          newx = cbind(matrix(x, nrow = 1), 1))$mean))
        OF <- compiler::cmpfun(OF)
      } else {
        OF <- function(x, ...) suppressWarnings(bayesianrvfl::predict_rvfl(fit_obj_surr,
                                                          newx = cbind(matrix(x, nrow = 1), 1))$mean)
        OF <- compiler::cmpfun(OF)
      }

      # end define surrogate and surrogate objective function

      parameters <- df_init_design$hyper_params
      scores <- apply(parameters, 1, OF) # scores calculated with f_approx_hat(x, s = 1)
      parameters <- as.matrix(parameters[!is.na(scores),])
      scores <- scores[!is.na(scores)]
    }

    if (optim_surr == "loglik")
    {
      if (surrogate_model == "rvfl")
      {
        # optimal params for the surrogate
        best_params <- bayesianrvfl::min_loglik(x = x_design,
                                                y = y_design,
                                                nodes_sim = "sobol",
                                                activ = activation_function)
        best_lam <- best_params$par[2]
        best_nb_hidden <- floor(best_params$par[1])

        if (verbose == TRUE)
        {
          cat("\n")
          if (optim_surr == "GCV") cat("----- GCV parameters", "\n")
          if (optim_surr == "loglik") cat("----- loglik parameters", "\n")
          cat("\n")
          cat("selected regularization parameter", "\n")
          print(best_lam)
          cat("\n")
          cat("selected number of hidden nodes", "\n")
          print(best_nb_hidden)
          cat("\n")
        }

        # objective function = approx of f = f_approx(x, s = 1)
        fit_obj_surr <- bayesianrvfl::fit_rvfl(x = x_design,
                                               y = y_design,
                                               nb_hidden = best_nb_hidden,
                                               lambda = best_lam,
                                               activ = activation_function,
                                               compute_Sigma = TRUE)
        if (is.factor(y))
        {
          OF <- function(x, ...) max(-1, suppressWarnings(bayesianrvfl::predict_rvfl(fit_obj_surr,
                                                                    newx = cbind(matrix(x, nrow = 1), 1))$mean))
          OF <- compiler::cmpfun(OF)
        } else {
          OF <- function(x, ...) suppressWarnings(bayesianrvfl::predict_rvfl(fit_obj_surr,
                                                            newx = cbind(matrix(x, nrow = 1), 1))$mean)
          OF <- compiler::cmpfun(OF)
        }
        # end define objective function

        parameters <- df_init_design$hyper_params
        scores <- apply(parameters, 1, OF) # scores calculated with f_approx_hat(x, s = 1)
        parameters <- as.matrix(parameters[!is.na(scores),])
        scores <- scores[!is.na(scores)]
      }

      if (surrogate_model == "matern52")
      {
        # optimal params for the surrogate
        best_params <- bayesianrvfl::min_loglik(x = as.matrix(x_design),
                                                y = as.vector(y_design),
                                                surrogate_model = "matern52")
        best_sigma <- best_params$par[1]
        best_l <- best_params$par[2]
        best_lam <- best_params$par[3]

        if (verbose == TRUE)
        {
          if (optim_surr == "loglik") cat("----- loglik parameters", "\n")
          cat("\n")
          cat("selected sigma", "\n")
          print(best_sigma)
          cat("\n")
          cat("selected lengthscale", "\n")
          print(best_l)
          cat("\n")
          cat("selected regularization parameter", "\n")
          print(best_lam)
          cat("\n")
        }

        # objective function = approx of f = f_approx(x, s = 1)
        fit_obj_surr <- bayesianrvfl::fit_matern52(x = x_design, y = y_design,
                                                   sigma = best_sigma, l = best_l,
                                                   lambda_krls = best_lam,
                                                   inv_method = "chol",
                                                   compute_Sigma = TRUE)

        if (is.factor(y))
        {
          OF <- function(x, ...) max(-1, suppressWarnings(bayesianrvfl::predict_matern52(fit_obj_surr,
                                                                                     newx = cbind(matrix(x, nrow = 1), 1))$mean))
          OF <- compiler::cmpfun(OF)
        } else {
          OF <- function(x, ...) suppressWarnings(bayesianrvfl::predict_matern52(fit_obj_surr,
                                                                             newx = cbind(matrix(x, nrow = 1), 1))$mean)
          OF <- compiler::cmpfun(OF)
        }

        parameters <- df_init_design$hyper_params
        scores <- apply(parameters, 1, OF) # scores calculated with f_approx_hat(x, s = 1)
        parameters <- as.matrix(parameters[!is.na(scores),])
        scores <- scores[!is.na(scores)]
      }
    }

    if (verbose == TRUE)
    {
      cat("scores", "\n")
      print(cbind.data.frame(parameters, scores))
      cat("\n")
    }

  # optimization method == "standard" ----------------------------------------------------

  if (method == "standard")
  {
      if (surrogate_model == "rvfl")
      {
        find_next_param_by_ei <- function(x)
        {
          pred_obj <- suppressWarnings(bayesianrvfl::predict_rvfl(fit_obj_surr,
                                                 newx = cbind(matrix(x, nrow = 1), 1)))

          mu_hat <- pred_obj$mean
          sigma_hat <- pred_obj$sd
          gamma_hat <- (min(scores) - mu_hat)/sigma_hat
          res <- -sigma_hat*(gamma_hat*pnorm(gamma_hat) + dnorm(gamma_hat))
          return (ifelse(is.na(res), 100, res))
        }
        find_next_param_by_ei <- compiler::cmpfun(find_next_param_by_ei)

        find_next_param_by_ucb <- function(x)
        {
          pred_obj <- suppressWarnings(bayesianrvfl::predict_rvfl(fit_obj_surr,
                                                 newx = cbind(matrix(x, nrow = 1), 1)))

          return (-(pred_obj$mean - kappa*pred_obj$sd))
        }
        find_next_param_by_ucb <- compiler::cmpfun(find_next_param_by_ucb)

        find_next_param <- switch(type_acq,
                                  "ei" = find_next_param_by_ei,
                                  "ucb" = find_next_param_by_ucb)

        if (verbose == FALSE && show_progress == TRUE)
        {
          pb <- txtProgressBar(min = 1, max = nb_iter, style = 3)
        }

        # main optimization loop for method == "standard" ----------------------------------------------------
        for (iter in 1:nb_iter)
        {
            if (verbose == TRUE)
            {
              cat("\n")
              cat("----- iteration #", iter, "\n")
              cat("\n")
            }

            # finding next param
              set.seed(iter + 1)
              next_param <- suppressWarnings(stats::nlminb(start = lower + (upper-lower)*runif(length(lower)),
                                                           objective = find_next_param,
                                                           lower = lower, upper = upper)$par)

            if (param_is_found(parameters, next_param) == TRUE)
            {
              nb_is_found <- nb_is_found + 1
              set.seed(iter + 1)
              next_param <- lower + (upper - lower)*runif(dim_xx)
            }

            if (verbose == TRUE)
            {
              cat("next_param", "\n")
              print(next_param)
              cat("\n")
            }

            df_next_param <- data.frame(matrix(next_param, nrow = 1))
            colnames(df_next_param) <- colnames(df_init_design$hyper_params)
            df_next_param <- transform_cols_integer(df_next_param, is_integer_bool = is_integer_upper)

            for(j in 1:length(df_init_design$s_vec)){
                next_param_cv_error[j] <- f_approx(x = x, y = y, tune_grid = df_next_param,
                                      s = df_init_design$s_vec[j], seed = j*iter*100,
                                      method = fit_method, metric = metric)$results_metric
            }
            next_param_design <- as.matrix(expand_grid_df(df_next_param,
                                           data.frame(df_init_design$s_vec)))
            colnames(next_param_design) <- colnames(x_design)

            # update x_design
            x_design <- rbind.data.frame(x_design, next_param_design)
            y_design <- c(y_design, next_param_cv_error)

            #update objective function = approx of f = f_approx(x, s = 1)
            fit_obj_surr <- bayesianrvfl::fit_rvfl(x = x_design,
                                                   y = y_design,
                                                   nb_hidden = best_nb_hidden,
                                                   lambda = best_lam,
                                                   activ = activation_function,
                                                   compute_Sigma = TRUE)

            if (is.factor(y))
            {
              OF <- function(x, ...) max(-1, suppressWarnings(bayesianrvfl::predict_rvfl(fit_obj_surr,
                                                                        newx = cbind(matrix(x, nrow = 1), 1))$mean))
              OF <- compiler::cmpfun(OF)
            } else {
              OF <- function(x, ...) suppressWarnings(bayesianrvfl::predict_rvfl(fit_obj_surr,
                                                                newx = cbind(matrix(x, nrow = 1), 1))$mean)
              OF <- compiler::cmpfun(OF)
            }
            current_score <- OF(next_param)

            if (verbose == TRUE)
            {
              cat("score", "\n")
              print(current_score)
              cat("\n")
            }

            # update parameters and scores
            parameters <- rbind(parameters, next_param)
            scores <- c(scores, current_score)

            if (verbose == TRUE)
            {
              index_min <- which.min(scores)
              best_param <- parameters[index_min,]
              cat("current best param", "\n")
              print(best_param)
              cat("\n")
              cat("current best score", "\n")
              print(scores[index_min])
              cat("\n")
            }

            if (verbose == FALSE && show_progress == TRUE) setTxtProgressBar(pb, iter)
        }
        if (verbose == FALSE && show_progress == TRUE) close(pb)

      } # end: if (surrogate_model == "rvfl")

      if (surrogate_model == "matern52")
      {
        find_next_param_by_ei <- function(x)
        {
          x <- matrix(x, nrow = 1)
          pred_obj <- bayesianrvfl::predict_matern52(fit_obj_surr,
                                                     newx = cbind(matrix(x, nrow = 1), 1))
          mu_hat <- pred_obj$mean
          sigma_hat <- pred_obj$sd
          gamma_hat <- (min(scores) - mu_hat)/sigma_hat
          res <- -sigma_hat*(gamma_hat*pnorm(gamma_hat) + dnorm(gamma_hat))
          return (ifelse(is.na(res), 100, res))
        }
        find_next_param_by_ei <- compiler::cmpfun(find_next_param_by_ei)

        find_next_param_by_ucb <- function(x)
        {
          x <- matrix(x, nrow = 1)
          pred_obj <- bayesianrvfl::predict_matern52(bayesianrvfl::fit_matern52(x = parameters, y = scores,
                                                                                sigma = best_sigma,
                                                                                l = best_l,
                                                                                lambda_krls = best_lam,
                                                                                compute_Sigma = TRUE),
                                                     newx = cbind(matrix(x, nrow = 1), 1))
          return (-(pred_obj$mean - kappa*pred_obj$sd))
        }
        find_next_param_by_ucb <- compiler::cmpfun(find_next_param_by_ucb)

        find_next_param <- switch(type_acq,
                                  "ei" = find_next_param_by_ei,
                                  "ucb" = find_next_param_by_ucb)

        if (verbose == FALSE && show_progress == TRUE)
        {
          pb <- txtProgressBar(min = 1, max = nb_iter, style = 3)
        }

        for (iter in 1:nb_iter)
        {
          if (verbose == TRUE)
          {
            cat("\n")
            cat("----- iteration #", iter, "\n")
            cat("\n")
          }

            set.seed(iter + 1)
            next_param <- suppressWarnings(stats::nlminb(start = lower + (upper-lower)*runif(length(lower)),
                                                         objective = find_next_param,
                                                         lower = lower, upper = upper)$par)

          if (param_is_found(parameters, next_param) == TRUE)
          {
            nb_is_found <- nb_is_found + 1
            set.seed(iter + 1)
            next_param <- lower + (upper - lower)*runif(dim_xx)
          }

          if (verbose == TRUE)
          {
            cat("next_param", "\n")
            print(next_param)
            cat("\n")
          }

          # NOT CORRECT?
          current_score <- OF(next_param) # consider also replacing this with evaluation of f_approx for s < s_max
          if (verbose == TRUE)
          {
            cat("score", "\n")
            print(current_score)
            cat("\n")
          }

          # update parameters and scores
          parameters <- rbind(parameters, next_param)
          scores <- c(scores, current_score)

          if (verbose == TRUE)
          {
            index_min <- which.min(scores)
            best_param <- parameters[index_min,]
            cat("current best param", "\n")
            print(best_param)
            cat("\n")
            cat("current best score", "\n")
            print(scores[index_min])
            cat("\n")
          }

          df_next_param <- data.frame(matrix(next_param, nrow = 1))
          colnames(df_next_param) <- colnames(df_init_design$hyper_params)
          df_next_param <- transform_cols_integer(df_next_param, is_integer_bool = is_integer_upper)

          for(j in 1:length(df_init_design$s_vec)){
            next_param_cv_error[j] <- f_approx(x = x, y = y, tune_grid = df_next_param,
                                               s = df_init_design$s_vec[j], seed = j*iter*100,
                                               method = fit_method, metric = metric)$results_metric
          }
          next_param_design <- as.matrix(expand_grid_df(df_next_param,
                                                        data.frame(df_init_design$s_vec)))
          colnames(next_param_design) <- colnames(x_design)

          # update x_design
          x_design <- rbind.data.frame(x_design, next_param_design)
          y_design <- c(y_design, next_param_cv_error)

          #update objective function = approx of f = f_approx(x, s = 1)
          fit_obj_surr <- bayesianrvfl::fit_matern52(x = x_design,
                                                     y = y_design,
                                                     sigma = best_sigma,
                                                     l = best_l,
                                                     lambda_krls = best_lam,
                                                     inv_method = "chol",
                                                     compute_Sigma = TRUE)

          if (is.factor(y))
          {
            OF <- function(x, ...) max(-1, suppressWarnings(bayesianrvfl::predict_matern52(fit_obj_surr,
                                                                                           newx = cbind(matrix(x, nrow = 1), 1))$mean))
            OF <- compiler::cmpfun(OF)
          } else {
            OF <- function(x, ...) suppressWarnings(bayesianrvfl::predict_matern52(fit_obj_surr,
                                                                                   newx = cbind(matrix(x, nrow = 1), 1))$mean)
            OF <- compiler::cmpfun(OF)
          }

          if (verbose == FALSE && show_progress == TRUE) setTxtProgressBar(pb, iter)
        }
        if (verbose == FALSE && show_progress == TRUE) close(pb)

      } # end: if (surrogate_model == "matern52")
  }

  # optimization method == "direct_online" ----------------------------------------------------

  if (method == "direct_online")
  {
    if (surrogate_model == "rvfl")
    {
      fit_obj_surr <- bayesianrvfl::fit_rvfl(x = x_design,
                                             y = y_design,
                                             nb_hidden = best_nb_hidden,
                                             lambda = best_lam,
                                             activ = activation_function,
                                             method = "chol",
                                             compute_Sigma = TRUE)
      if (is.factor(y))
      {
        OF <- function(x, ...) max(-1, suppressWarnings(bayesianrvfl::predict_rvfl(fit_obj_surr,
                                                                  newx = cbind(matrix(x, nrow = 1), 1))$mean))
        OF <- compiler::cmpfun(OF)
      } else {
        OF <- function(x, ...) suppressWarnings(bayesianrvfl::predict_rvfl(fit_obj_surr,
                                                          newx = cbind(matrix(x, nrow = 1), 1))$mean)
        OF <- compiler::cmpfun(OF)
      }

      find_next_param_by_ei <- function(x)
      {
        pred_obj <- suppressWarnings(bayesianrvfl::predict_rvfl(fit_obj_surr,
                                               newx = cbind(matrix(x, nrow = 1), 1)))
        mu_hat <- pred_obj$mean
        sigma_hat <- pred_obj$sd
        gamma_hat <- (min(scores) - mu_hat)/sigma_hat
        res <- -sigma_hat*(gamma_hat*pnorm(gamma_hat) + dnorm(gamma_hat))
        return (ifelse(is.na(res), 100, res))
      }
      find_next_param_by_ei <- compiler::cmpfun(find_next_param_by_ei)

      find_next_param_by_ucb <- function(x)
      {
        pred_obj <- suppressWarnings(bayesianrvfl::predict_rvfl(fit_obj_surr,
                                               newx = cbind(matrix(x, nrow = 1), 1)))
        return (-(pred_obj$mean - kappa*pred_obj$sd))
      }
      find_next_param_by_ucb <- compiler::cmpfun(find_next_param_by_ucb)

      find_next_param <- switch(type_acq,
                                "ei" = find_next_param_by_ei,
                                "ucb" = find_next_param_by_ucb)

      if (verbose == FALSE && show_progress == TRUE)
      {
        pb <- txtProgressBar(min = 1, max = nb_iter, style = 3)
      }

      for (iter in 1:nb_iter)
      {
          if (verbose == TRUE)
          {
            cat("\n")
            cat("----- iteration #", iter, "\n")
            cat("\n")
          }

            set.seed(iter + 1)
            next_param <- suppressWarnings(stats::nlminb(start = lower + (upper-lower)*runif(length(lower)),
                                                         objective = find_next_param,
                                                         lower = lower, upper = upper)$par)

          if (param_is_found(parameters, next_param) == TRUE)
          {
            nb_is_found <- nb_is_found + 1
            set.seed(iter + 1)
            next_param <- lower + (upper - lower)*runif(dim_xx)
          }

          if (verbose == TRUE)
          {
            cat("next_param", "\n")
            print(next_param)
            cat("\n")
          }

          current_score <- OF(next_param)
          if (verbose == TRUE)
          {
            cat("score", "\n")
            print(current_score)
            cat("\n")
          }

          parameters <- rbind(parameters, next_param)
          scores <- c(scores, current_score)
          if (verbose == TRUE)
          {
            index_min <- which.min(scores)
            best_param <- parameters[index_min,]
            cat("current best param", "\n")
            print(best_param)
            cat("\n")
            cat("current best score", "\n")
            print(scores[index_min])
            cat("\n")
          }

          #update objective function = approx of f = f_approx(x, s = 1)
          df_next_param <- data.frame(matrix(next_param, nrow = 1))
          colnames(df_next_param) <- colnames(df_init_design$hyper_params)
          df_next_param <- transform_cols_integer(df_next_param,
                                                  is_integer_bool = is_integer_upper)

          for(i in 1:nb_s)
          {
            next_param_cv_error <- f_approx(x = x, y = y, tune_grid = df_next_param,
                                            s = df_init_design$s_vec[i], seed = i*iter*100,
                                            method = fit_method, metric = metric)$results_metric
            fit_obj_surr <- bayesianrvfl::update_params(fit_obj = fit_obj_surr,
                                                        newx = c(next_param, df_init_design$s_vec[i]),
                                                        newy = next_param_cv_error,
                                                        method = "direct")
          }
          if (is.factor(y))
          {
            OF <- function(x, ...) max(-1, suppressWarnings(bayesianrvfl::predict_rvfl(fit_obj_surr,
                                                                      newx = cbind(matrix(x, nrow = 1), 1))$mean))
            OF <- compiler::cmpfun(OF)
          } else {
            OF <- function(x, ...) suppressWarnings(bayesianrvfl::predict_rvfl(fit_obj_surr,
                                                              newx = cbind(matrix(x, nrow = 1), 1))$mean)
            OF <- compiler::cmpfun(OF)
          }

          if (verbose == FALSE && show_progress == TRUE) setTxtProgressBar(pb, iter)
        }
      if (verbose == FALSE && show_progress == TRUE) close(pb)
    }

    if (surrogate_model == "matern52")
    {stop("not implemented")}
  }

  # final best params
  index_min <- which.min(scores)
  best_param <- parameters[index_min,]

  if (record_points == FALSE){
    return(list(index_min = index_min,
                nb_is_found = nb_is_found,
                best_param = best_param,
                best_value = scores[index_min]))
  } else {

    final_scores <- apply(parameters, 1, OF)
    cat("calculate real scores...", "\n")
    cat("\n")
    cl_SOCK <- parallel::makeCluster(cl, type = "SOCK")
    doSNOW::registerDoSNOW(cl_SOCK)
    pb <- txtProgressBar(min = 0, max = nrow(parameters), style = 3)
    progress <- function(n) utils::setTxtProgressBar(pb, n)
    opts <- list(progress = progress)
    real_scores <- foreach::foreach(i = 1:nrow(parameters),
                                    .packages = "doSNOW",
                                    .export = c("f_approx",
                                                "transform_cols_integer"),
                                    .options.snow = opts
                                    )%dopar%{
      df_params <- data.frame(matrix(parameters[i, ], nrow = 1));
      colnames(df_params) <- names(lower_df);
      df_params <- transform_cols_integer(df_params,
                             is_integer_bool = is_integer_upper)
      f_approx(x = x, y = y, s = 1, tune_grid = df_params,
               method = fit_method, metric = metric)$results_metric
                                    }
    parallel::stopCluster(cl_SOCK)

    points_found <- cbind(parameters, scores)
    n_params <- ncol(parameters)
    colnames(points_found) <- c(paste0("param", 1:n_params), "score")

    real_scores_ <- as.vector(unlist(res$points_found[,"real_scores"]))
    final_scores_ <- as.vector(unlist(res$points_found[,"final_scores"]))
    diff_real_final <- real_scores_ - final_scores_
    index_min_final_scores <- which.min(final_scores)
    index_min_real_scores <- which.min(real_scores)

    plot(real_scores_, ylim = c(-1.05, -0.7), type = 'l',
         main = "real vs est. scores")
    lines(final_scores_, ylim = c(-1.05, -0.7), col = 'red')

    return(list(df_init_design = df_init_design,
                index_min = index_min,
                index_min_final_scores = index_min_final_scores,
                index_min_real_scores = index_min_real_scores,
                nb_is_found = nb_is_found,
                best_param = best_param,
                best_value = scores[index_min],
                points_found = cbind(parameters, scores, final_scores, real_scores),
                diff_real_final = diff_real_final,
                cor_real_final = cor(real_scores_, final_scores_),
                t_test = t.test(diff_real_final),
                Box_test = Box.test(diff_real_final),
                shapiro_test = shapiro.test(diff_real_final)))
  }

}
