# abs(1-x(i)/x(i-1)) < relTol (1e-10) ||  abs(x(i)-x(i-1)) < absTol (1e-20)
# Bayesian optimization

#' Title
#'
#' @param objective
#' @param lower
#' @param upper
#' @param type_acq
#' @param nb_init
#' @param nb_iter
#' @param kappa
#' @param method
#' @param surrogate_model
#' @param optim_surr
#' @param activation_function
#' @param type_optim
#' @param early_stopping
#' @param abs_tol
#' @param rel_tol
#' @param seed
#' @param verbose
#' @param show_progress
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
bayes_opt <- function(objective, # objective function
                      lower, # lower bound for search
                      upper, # upper bound for search
                      type_acq = c("ei", "ucb"), # type of acquisition function
                      nb_init = 10, # number of points in initial design
                      nb_iter = 25, # number of iterations of the algo
                      kappa = 1.96, # quantile for ucb
                      method = c("standard", "direct_online", "polyak_online"), # fit all, or online # '*_online' available for rvfl only
                      surrogate_model = c("rvfl", "matern52", "rvfl_emcee", "rf"), # surrogate model
                      optim_surr = c("GCV", "loglik", "cv"), # surrogate hyperparams fitting
                      activation_function = c("relu", "tanh", "sigmoid"), # activation for bayesian rvfl
                      type_optim = c("nlminb", "DEoptim", "msnlminb", "randsearch", "none"), # optim for acquisition
                      early_stopping = FALSE, abs_tol = 1e-07, rel_tol = 1e-03, # currently only for method == 'direct_online'
                      seed = 123, verbose = TRUE, show_progress = TRUE, ...)
{
  OF <- function(y, ...) {return(objective(y, ...))}

  nb_is_found <- 0
  dim_xx <- length(lower)
  stopifnot(dim_xx == length(upper))
  type_acq <- match.arg(type_acq)
  type_optim <- match.arg(type_optim)
  method <- match.arg(method)
  activation_function <- match.arg(activation_function)
  optim_surr <- match.arg(optim_surr)
  surrogate_model <- match.arg(surrogate_model)

  if (surrogate_model == "matern52")
    optim_surr <- "loglik" # to me MODIFIED

  if (verbose == TRUE)
    cat("\n", "----- define initial design...", "\n")


  rep_1_nb_init <- rep(1, nb_init)
  lower_mat_init <- tcrossprod(rep_1_nb_init, lower)
  upper_mat_init <- tcrossprod(rep_1_nb_init, upper)

  set.seed(seed)
  parameters <- lower_mat_init + (upper_mat_init - lower_mat_init) * matrix(runif(nb_init *
                                                                                    dim_xx), nrow = nb_init,
                                                                            ncol = dim_xx)
  scores <- apply(parameters, 1, OF)
  parameters <- as.matrix(parameters[!is.na(scores),])
  scores <- scores[!is.na(scores)]

  if (identical(type_optim, "none"))
  {
    n_choices <- 1000
    lower_matrix <- t(replicate(n_choices, lower))
    upper_matrix <- t(replicate(n_choices, upper))
    sobol_seq_choices <- lower_matrix + (upper_matrix - lower_matrix) * randtoolbox::sobol(n=n_choices,
                                                                      dim=dim_xx)
  }

  if (early_stopping == TRUE)
  {
    # calculating the integrated acquisition function
    vec_acq <- rep(NA, nb_iter)

    if (type_acq != "ei") {
      type_acq <- "ei"
      warning("type_acq set to 'ei'")
    }

    n_points_in_domain <- 1000 # for the calculation of integrated acquisition
    rep_1_n_points_in_domain <- rep(1, n_points_in_domain)
    lower_mat_points_in_domain <- tcrossprod(rep_1_n_points_in_domain, lower)
    upper_mat_points_in_domain <- tcrossprod(rep_1_n_points_in_domain, upper)
    points_in_domain <- lower_mat_points_in_domain + (upper_mat_points_in_domain -
                                                        lower_mat_points_in_domain)*randtoolbox::sobol(n = n_points_in_domain,
                                                                                                       dim = dim_xx) # for the calculation of integrated acquisition
  }

  if (verbose == TRUE)
  {
    cat("initial design", "\n")
    print(cbind.data.frame(parameters, scores))
    cat("\n")
  }

  if (optim_surr == "GCV")
  {
    cat("\n")
    cat("finding hyperparams for surrogate...", "\n")
    cat("\n")
    best_params <- bayesianrvfl::find_lam_nbhidden_nclusters(x = as.matrix(parameters),
                                                             y = scores)
    best_lam <- best_params$best_lambda
    best_nb_hidden <- best_params$best_nb_hidden
    best_n_clusters <- best_params$best_n_clusters
  }

  if (optim_surr == "loglik")
  {
    if (surrogate_model == "rvfl")
    {
      best_params <- bayesianrvfl::min_loglik(
        x = parameters, y = scores,
        surrogate_model = "rvfl", nodes_sim = "sobol",
        activ = activation_function)

      best_nb_hidden <- floor(best_params$par[1])
      best_lam <- best_params$par[2]

      if (verbose == TRUE)
      {
        cat("\n")
        if (optim_surr == "GCV")
          cat("----- GCV parameters", "\n")
        if (optim_surr == "loglik")
          cat("----- loglik parameters", "\n")
        cat("\n")
        cat("selected regularization parameter", "\n")
        print(best_lam)
        cat("\n")
        cat("selected number of hidden nodes", "\n")
        print(best_nb_hidden)
        cat("\n")
      }
    }

    if (surrogate_model == "matern52")
    {
      best_params <- bayesianrvfl::min_loglik(x = parameters,
                                              y = scores,
                                              surrogate_model = "matern52")
      best_sigma <- best_params$par[1]
      best_l <- best_params$par[2]
      best_lam <- best_params$par[3]

      if (verbose == TRUE)
      {
        if (optim_surr == "loglik")
          cat("----- loglik parameters", "\n")
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
    }
  }

  if (optim_surr == "cv")
  {
    cat("\n")
    cat("finding hyperparams for surrogate...", "\n")
    cat("\n")
    best_params <- bayesianrvfl::cv_rvfl(x = parameters, y = scores)
    best_lam <- best_params$best_lambda
    best_nb_hidden <- best_params$best_nb_hidden
    best_n_clusters <- best_params$best_n_clusters
  }

  # method == "standard" - optimization ----------------------------------------------------

  if (method == "standard")
  {
    if (surrogate_model == "rvfl")
    {
      find_next_param_by_ei <- function(x)
      {
        if (is.vector(x))
          x <- matrix(x, nrow = 1)

        pred_obj <- bayesianrvfl::predict_rvfl(
          bayesianrvfl::fit_rvfl(x = parameters, y = scores,
                                 method = "solve",
                                 activ = activation_function,
                                 nb_hidden = best_nb_hidden,
                                 lambda = best_lam,
                                 n_clusters = best_n_clusters,
                                 compute_Sigma = TRUE), newx = x)

        mu_hat <- pred_obj$mean
        sigma_hat <- pred_obj$sd
        gamma_hat <- (min(scores) - mu_hat) / sigma_hat
        res <- -sigma_hat * (gamma_hat * pnorm(gamma_hat) + dnorm(gamma_hat))

        return (ifelse(!is.na(res), res, 1000))

      }
      find_next_param_by_ei <- compiler::cmpfun(find_next_param_by_ei)

      find_next_param_by_ucb <- function(x)
      {
        x <- matrix(x, nrow = 1)
        pred_obj <- bayesianrvfl::predict_rvfl(
          bayesianrvfl::fit_rvfl(x = parameters, y = scores,
                                 method = "solve",
                                 activ = activation_function,
                                 nb_hidden = best_nb_hidden,
                                 lambda = best_lam,
                                 n_clusters = best_n_clusters,
                                 compute_Sigma = TRUE), newx = x)

        return (-(pred_obj$mean - kappa * pred_obj$sd))
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

        # find next point to evaluate ----
        if (type_optim == "none")
        {
          next_params_scores <- sapply(seq_len(nrow(sobol_seq_choices)),
                 function (i) find_next_param(sobol_seq_choices[i, ]))
          cat("next_params_scores", next_params_scores, "\n")
          next_param <- sobol_seq_choices[which.min(next_params_scores),]
        }


        if (type_optim == "nlminb")
        {
          set.seed(iter + 1)
          next_param <- suppressWarnings(
            stats::nlminb(start = lower + (upper - lower) * runif(length(lower)),
                          objective = find_next_param,
                          lower = lower, upper = upper)$par)

        }

        if (type_optim == "msnlminb")
        {
          next_param <- suppressWarnings(
            bayesianrvfl::msnlminb(objective = find_next_param,
                                   lower = lower, upper = upper,
                                   nb_iter = 10)$par)

        }

        if (type_optim == "DEoptim")
        {
          next_param <-
            suppressWarnings(
              DEoptim::DEoptim(fn = find_next_param,
                               lower = lower, upper = upper,
                               control = DEoptim::DEoptim.control(
                                 trace = FALSE, parallelType = 0,
                                 itermax = 25))$optim$bestmem)

          if (any(is.na(next_param)))
            next_param <- lower + (upper - lower)*runif(dim_xx)
        }

        if (type_optim == "randsearch")
        {
          cat(" randsearch...", "\n")
          next_param <- suppressWarnings(
            bayesianrvfl::random_search_opt(objective = find_next_param,
                                            nb_iter = 100, sim = "sobol",
                                            lower = lower, upper = upper)$par)

        }

        # if already found before, search randomly for another one
        if (param_is_found(parameters, next_param) == TRUE)
        {
          nb_is_found <- nb_is_found + 1
          set.seed(iter + 1)
          next_param <- lower + (upper - lower) * runif(dim_xx)
        }

        current_score <- OF(next_param)
        if (verbose == TRUE)
        {
          cat("next_param", "\n")
          print(next_param)
          cat("\n")
          cat("score", "\n")
          print(current_score)
          cat("\n")
        }

        # update points found and scores
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

        if (verbose == FALSE && show_progress == TRUE)
          setTxtProgressBar(pb, iter)
      }
      if (verbose == FALSE && show_progress == TRUE)
        close(pb)
    } # end: if (surrogate_model == "rvfl")

    if (surrogate_model == "rvfl_emcee")
    {
      stopifnot(type_optim == "randsearch") # otherwise the optimization will never end

      find_next_param_by_ei <- function(x)
      {
        if (is.vector(x))
          x <- matrix(x, nrow = 1)

        pred_obj <- bayesianrvfl::predict_rvfl_mcmc(
          bayesianrvfl::fit_rvfl_mcmc(x = parameters, y = scores,
                                      activ = activation_function,
                                      compute_Sigma = TRUE), newx = x)

        mu_hat <- pred_obj$mean
        sigma_hat <- pred_obj$sd
        gamma_hat <- (min(scores) - mu_hat) / sigma_hat
        res <- -sigma_hat * (gamma_hat * pnorm(gamma_hat) + dnorm(gamma_hat))

        return (ifelse(is.na(res), 1000, res))
      }
      find_next_param_by_ei <- compiler::cmpfun(find_next_param_by_ei)

      find_next_param_by_ucb <- function(x)
      {
        x <- matrix(x, nrow = 1)
        pred_obj <- bayesianrvfl::predict_rvfl_mcmc(
          bayesianrvfl::fit_rvfl_mcmc(x = parameters, y = scores,
                                      activ = activation_function,
                                      compute_Sigma = TRUE), newx = x)

        return (-(pred_obj$mean - kappa * pred_obj$sd))
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

        next_param <- suppressWarnings(
          bayesianrvfl::random_search_opt(objective = find_next_param,
                                          nb_iter = 100, sim = "sobol",
                                          lower = lower, upper = upper)$par)

        # if already found before, search randomly for another one
        if (param_is_found(parameters, next_param) == TRUE)
        {
          nb_is_found <- nb_is_found + 1
          set.seed(iter + 1)
          next_param <- lower + (upper - lower) * runif(dim_xx)
        }

        current_score <- OF(next_param)
        if (verbose == TRUE)
        {
          cat("next_param", "\n")
          print(next_param)
          cat("\n")
          cat("score", "\n")
          print(current_score)
          cat("\n")
        }

        # update points found and scores
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

        if (verbose == FALSE && show_progress == TRUE)
          setTxtProgressBar(pb, iter)
      }
      if (verbose == FALSE && show_progress == TRUE)
        close(pb)
    } # end: if (surrogate_model == "rvfl")

    if (surrogate_model == "matern52")
    {

      find_next_param_by_ei <- function(x)
      {
        if (is.vector(x))
          x <- matrix(x, nrow = 1)

        pred_obj <- bayesianrvfl::predict_matern52(
          bayesianrvfl::fit_matern52( x = parameters, y = scores,
                                      sigma = best_sigma,
                                      l = best_l,
                                      lambda_krls = best_lam,
                                      compute_Sigma = TRUE), newx = x)

        mu_hat <- pred_obj$mean
        sigma_hat <- pred_obj$sd
        gamma_hat <- (min(scores) - mu_hat) / sigma_hat
        res <- -sigma_hat * (gamma_hat * pnorm(gamma_hat) + dnorm(gamma_hat))

        return (ifelse(!is.na(res), res, 1000))

      }
      find_next_param_by_ei <- compiler::cmpfun(find_next_param_by_ei)

      find_next_param_by_ucb <- function(x)
      {
        x <- matrix(x, nrow = 1)
        pred_obj <- bayesianrvfl::predict_matern52(
          bayesianrvfl::fit_matern52( x = parameters, y = scores,
                                      sigma = best_sigma,
                                      l = best_l,
                                      lambda_krls = best_lam,
                                      compute_Sigma = TRUE), newx = x)

        return (-(pred_obj$mean - kappa * pred_obj$sd))
      }
      find_next_param_by_ucb <- compiler::cmpfun(find_next_param_by_ucb)

      find_next_param <- switch(type_acq,
                                "ei" = find_next_param_by_ei,
                                "ucb" = find_next_param_by_ucb)

      if (verbose == FALSE && show_progress == TRUE)
        pb <- txtProgressBar(min = 1, max = nb_iter, style = 3)

      # optimization loop -----
      for (iter in 1:nb_iter)
      {
        if (verbose == TRUE)
        {
          cat("\n")
          cat("----- iteration #", iter, "\n")
          cat("\n")
        }

        if (type_optim == "nlminb")
        {
          set.seed(iter + 1)
          next_param <-
            suppressWarnings(
              stats::nlminb(start = lower + (upper - lower) * runif(length(lower)),
                            objective = find_next_param, lower = lower, upper = upper)$par)
        }

        if (type_optim == "msnlminb")
        {
          next_param <-
            suppressWarnings(
              bayesianrvfl::msnlminb(
                objective = find_next_param,
                lower = lower,
                upper = upper,
                nb_iter = 10
              )$par
            )
        }

        if (type_optim == "DEoptim")
        {
          next_param <-
            suppressWarnings(
              DEoptim::DEoptim(
                fn = find_next_param,
                lower = lower,
                upper = upper,
                control = DEoptim::DEoptim.control(
                  trace = FALSE,
                  parallelType = 0,
                  itermax = 25
                )
              )$optim$bestmem
            )
        }

        if (type_optim == "randsearch")
        {
          next_param <- suppressWarnings(
            bayesianrvfl::random_search_opt(objective = find_next_param,
                                            nb_iter = 100, sim = "sobol",
                                            lower = lower, upper = upper)$par)

        }

        if (param_is_found(parameters, next_param) == TRUE)
        {
          nb_is_found <- nb_is_found + 1
          set.seed(iter + 1)
          next_param <- lower + (upper - lower) * runif(dim_xx)
        }

        if (verbose == TRUE)
        {
          cat("next_param", "\n")
          print(next_param)
          cat("\n")
          if (type_acq == "ei")
            cat("Expected Improvement", "\n")
          else
            cat("Lower confidence bound", "\n")
          print(find_next_param(next_param))
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

        if (verbose == FALSE &&
            show_progress == TRUE)
          setTxtProgressBar(pb, iter)
      }
      if (verbose == FALSE && show_progress == TRUE)
        close(pb)

    } # end: if (surrogate_model == "matern52")
  }

  # method == "direct_online" - optimization ----------------------------------------------------

  if (method == "direct_online")
  {
    if (surrogate_model == "rvfl")
    {
      fit_obj <- bayesianrvfl::fit_rvfl(x = parameters, y = scores,
                                        nb_hidden = best_nb_hidden,
                                        activ = activation_function,
                                        lambda = best_lam,
                                        n_clusters = best_n_clusters,
                                        method = "solve",
                                        compute_Sigma = TRUE)

      find_next_param_by_ei <- function(x)
      {
        if (is.vector(x))
          x <- matrix(x, nrow = 1)

        pred_obj <- suppressWarnings(bayesianrvfl::predict_rvfl(fit_obj,
                                               newx = x))
        mu_hat <- pred_obj$mean
        sigma_hat <- pred_obj$sd
        gamma_hat <- (min(scores) - mu_hat) / sigma_hat
        res <- -sigma_hat * (gamma_hat * pnorm(gamma_hat) + dnorm(gamma_hat))

        return (ifelse(!is.na(res), res, 1000))
      }
      find_next_param_by_ei <- compiler::cmpfun(find_next_param_by_ei)

      find_next_param_by_ucb <- function(x)
      {
        x <- matrix(x, nrow = 1)
        pred_obj <- bayesianrvfl::predict_rvfl(fit_obj,
                                               newx = x)
        return (-(pred_obj$mean - kappa * pred_obj$sd))
      }
      find_next_param_by_ucb <- compiler::cmpfun(find_next_param_by_ucb)

      find_next_param <- switch(type_acq,
                                "ei" = find_next_param_by_ei,
                                "ucb" = find_next_param_by_ucb)

      if (verbose == FALSE && show_progress == TRUE)
        pb <- txtProgressBar(min = 1, max = nb_iter, style = 3)

      if (early_stopping && verbose)
      {
        cat("initial ei acquisition", "\n")
        print(cbind(
          points_in_domain,
          find_next_param_by_ei(points_in_domain)
        ))
        cat("\n")
      }

      for (iter in 1:nb_iter)
      {
        if (verbose == TRUE)
        {
          cat("\n")
          cat("----- iteration #", iter, "\n")
          cat("\n")
        }

        # find next point to evaluate ----
        if (type_optim == "none")
        {
          next_params_scores <- sapply(seq_len(nrow(sobol_seq_choices)),
                                       function (i) find_next_param(sobol_seq_choices[i, ]))
          next_param <- sobol_seq_choices[which.min(next_params_scores),]
        }

        if (type_optim == "nlminb")
        {
          set.seed(iter + 1)
          next_param <- suppressWarnings(
            stats::nlminb(start = lower + (upper - lower) * runif(length(lower)),
                          objective = find_next_param,
                          lower = lower, upper = upper)$par)

        }

        if (type_optim == "msnlminb")
        {
          next_param <- suppressWarnings(
            bayesianrvfl::msnlminb(objective = find_next_param,
                                   lower = lower, upper = upper,
                                   nb_iter = 10)$par)

        }

        if (type_optim == "DEoptim")
        {
          next_param <-
            suppressWarnings(
              DEoptim::DEoptim(fn = find_next_param,
                               lower = lower, upper = upper,
                               control = DEoptim::DEoptim.control(
                                 trace = FALSE, parallelType = 0,
                                 itermax = 25))$optim$bestmem)

          if (any(is.na(next_param)))
            next_param <- lower + (upper - lower)*runif(dim_xx)
        }

        if (type_optim == "randsearch")
        {
          next_param <- suppressWarnings(
            bayesianrvfl::random_search_opt(objective = find_next_param,
                                            nb_iter = 100, sim = "sobol",
                                            lower = lower, upper = upper)$par)
        }

        # if already found before, search randomly for another one
        if (param_is_found(parameters, next_param) == TRUE)
        {
          nb_is_found <- nb_is_found + 1
          set.seed(iter + 1)
          next_param <- lower + (upper - lower) * runif(dim_xx)
        }

        current_score <- OF(next_param)
        if (verbose == TRUE)
        {
          cat("next_param", "\n")
          print(next_param)
          cat("\n")
          cat("score", "\n")
          print(current_score)
          cat("\n")
        }

        # update points found and scores
        parameters <- rbind(parameters, next_param)
        scores <- c(scores, current_score)

        # update stuff
        fit_obj <- bayesianrvfl::update_params(fit_obj = fit_obj,
                                               newx = next_param,
                                               newy = current_score,
                                               method = "direct",
                                               re_clust = FALSE)


        if (early_stopping)
        {
          cat("ei acquisition", "\n")
          print(cbind(
            points_in_domain,
            find_next_param_by_ei(points_in_domain)
          ))
          cat("\n")
        }

        if (type_acq == "ei" && early_stopping == TRUE)
          vec_acq[iter] <- mean(find_next_param_by_ei(points_in_domain)) #numerical integration of a_{EI}(x; \theta)

        if (type_acq == "ei" && early_stopping == TRUE && iter >= 3)
        {
          opt_tols <- bayesianrvfl::calc_tol(vec_acq)

          if (verbose == TRUE)
          {
            cat("a_mcei", "\n")
            print(vec_acq[iter])
            cat("\n")
            cat("abs_tol", "\n")
            print(opt_tols$abs_tol)
            cat("\n")
            cat("rel_tol", "\n")
            print(opt_tols$rel_tol)
            cat("\n")
          }
          # early stopping
          if (opt_tols$abs_tol <= abs_tol ||opt_tols$rel_tol <= rel_tol)
            break
        }

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

        if (verbose == FALSE && show_progress == TRUE)
          setTxtProgressBar(pb, iter)
      } # end for loop

      if (verbose == FALSE && show_progress == TRUE)
        close(pb)
    }

    if (surrogate_model == "matern52")
    {
      stop("not implemented")
    }
  }

  # method == "polyak_online" ----------------------------------------------------

  if (method == "polyak_online")
  {
    if (surrogate_model == "rvfl")
    {
      fit_obj <- bayesianrvfl::fit_rvfl(x = parameters, y = scores,
                                        nb_hidden = best_nb_hidden,
                                        activ = activation_function,
                                        lambda = best_lam,
                                        n_clusters = best_n_clusters,
                                        method = "solve",
                                        compute_Sigma = TRUE
      )
      # with averaged coeffs
      fit_obj2 <- fit_obj
      mat_coefs <- matrix(0, ncol = nb_iter + 1,
                          nrow = length(fit_obj$coef))
      colnames(mat_coefs) <- 1:(nb_iter + 1)
      mat_coefs[, 1] <- fit_obj$coef

      find_next_param_by_ei <- function(x)
      {
        if (is.vector(x))
          x <- matrix(x, nrow = 1)
        pred_obj <- bayesianrvfl::predict_rvfl(fit_obj2,
                                               newx = x)
        gamma_hat <- (min(scores) - pred_obj$mean) / pred_obj$sd
        res <- -pred_obj$sd * (gamma_hat * pnorm(gamma_hat) + dnorm(gamma_hat))
        return (ifelse(is.na(res), 10000, res))
      }
      find_next_param_by_ei <- compiler::cmpfun(find_next_param_by_ei)

      find_next_param_by_ucb <- function(x)
      {
        x <- matrix(x, nrow = 1)
        pred_obj <- bayesianrvfl::predict_rvfl(fit_obj2,
                                               newx = x)
        return (-(pred_obj$mean - kappa * pred_obj$sd))
      }
      find_next_param_by_ucb <- compiler::cmpfun(find_next_param_by_ucb)

      find_next_param <- switch(type_acq,
                                "ei" = find_next_param_by_ei,
                                "ucb" = find_next_param_by_ucb)

      if (verbose == FALSE && show_progress == TRUE)
      {
        pb <- txtProgressBar(min = 1,
                             max = nb_iter,
                             style = 3)
      }


      for (iter in 1:nb_iter)
      {
        if (verbose == TRUE)
        {
          cat("\n")
          cat("----- iteration #", iter, "\n")
          cat("\n")
        }

        if (type_optim == "nlminb")
        {
          set.seed(iter + 1)
          next_param <-
            suppressWarnings(
              stats::nlminb(
                start = lower + (upper - lower) * runif(length(lower)),
                objective = find_next_param,
                lower = lower,
                upper = upper
              )$par
            )
        }

        if (type_optim == "msnlminb")
        {
          next_param <-
            suppressWarnings(
              bayesianrvfl::msnlminb(
                objective = find_next_param,
                lower = lower,
                upper = upper,
                nb_iter = 10
              )$par
            )
        }

        if (type_optim == "DEoptim")
        {
          next_param <-
            suppressWarnings(
              DEoptim::DEoptim(
                fn = find_next_param,
                lower = lower,
                upper = upper,
                control = DEoptim::DEoptim.control(
                  trace = FALSE,
                  parallelType = 0,
                  itermax = 25
                )
              )$optim$bestmem
            )
        }

        if (type_optim == "randsearch")
        {
          next_param <- suppressWarnings(
            bayesianrvfl::random_search_opt(objective = find_next_param,
                                            nb_iter = 100, sim = "sobol",
                                            lower = lower, upper = upper)$par)

        }

        if (param_is_found(parameters, next_param) == TRUE)
        {
          nb_is_found <- nb_is_found + 1
          set.seed(iter + 1)
          next_param <- lower + (upper - lower) * runif(dim_xx)
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

        fit_obj <- bayesianrvfl::update_params(
          fit_obj = fit_obj,
          newx = next_param,
          newy = current_score,
          method = "polyak"
        )

        fit_obj2 <- fit_obj
        mat_coefs[, (iter + 1)] <- fit_obj$coef
        fit_obj2$coef <- rowSums(mat_coefs) / (iter + 1)

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

        if (verbose == FALSE &&
            show_progress == TRUE)
          setTxtProgressBar(pb, iter)
      }
      if (verbose == FALSE && show_progress == TRUE)
        close(pb)
    }

    if (surrogate_model == "matern52")
    {
      stop("not implemented")
    }
  }

  # final best params
  index_min <- which.min(scores)
  best_param <- parameters[index_min,]

  points_found <- cbind(parameters, scores)
  n_params <- ncol(parameters)
  colnames(points_found) <- c(paste0("param", 1:n_params),
                              "score")

  if (early_stopping == FALSE)
  {
    return(
      list(
        index_min = index_min,
        nb_is_found = nb_is_found,
        best_param = best_param,
        best_value = scores[index_min],
        points_found = points_found
      )
    )

  } else {

    return(list(index_min = index_min,
                nb_is_found = nb_is_found,
                vec_acq = vec_acq,
                best_param = best_param,
                best_value = scores[index_min],
                points_found = points_found))

  }
}
