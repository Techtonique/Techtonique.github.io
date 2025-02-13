# predict from an rvfl ----

#' Predict from an rvfl
#'
#' @param fit_obj
#' @param newx
#' @param ci
#' @param graph
#'
#' @return
#' @export
#'
#' @examples
predict_rvfl <- function(fit_obj, newx,
                         ci = NULL, graph = FALSE)
{
  if (is.vector(newx))
    newx <- t(newx)

  if (is.null(fit_obj$clust_obj))
  {
    newx <- create_new_predictors(
      x = newx,
      nb_hidden = fit_obj$nb_hidden,
      nn_xm = fit_obj$nn_xm,
      nn_scales = fit_obj$nn_scales,
      activ = fit_obj$activ,
      nodes_sim = fit_obj$nodes_sim
    )$predictors

  } else { # !is.null(fit_obj$clust_obj)

    # scaled_x <- bayesianrvfl::my_scale(fit_obj$x,
    #                                    xm = fit_obj$clusters_scales$means,
    #                                    xsd = fit_obj$clusters_scales$sds)

    scaled_newx <- bayesianrvfl::my_scale(newx,
                             xm = fit_obj$clusters_scales$means,
                             xsd = fit_obj$clusters_scales$sds)

    pred_clusters <- predict(fit_obj$clust_obj, scaled_newx)

    newX_clust <- one_hot(pred_clusters$cluster,
                                   fit_obj$n_clusters)

    newx <- create_new_predictors(
      x = cbind(newx, newX_clust),
      nb_hidden = fit_obj$nb_hidden,
      nn_xm = fit_obj$nn_xm,
      nn_scales = fit_obj$nn_scales,
      activ = fit_obj$activ,
      nodes_sim = fit_obj$nodes_sim
    )$predictors
  }

  xm <- as.vector(fit_obj$xm)
  scales <- as.vector(fit_obj$scales)
  scaled_newx <- my_scale(x = as.matrix(newx),
                          xm = xm,
                          xsd = scales)
  n <- nrow(scaled_newx)
  p <- ncol(scaled_newx)

  res <- drop(scaled_newx %*% as.matrix(fit_obj$coef) + fit_obj$ym)

  lambda <- fit_obj$lambda
  nlambda <- length(lambda)
  compute_Sigma <- fit_obj$compute_Sigma

  if (is.matrix(res))
    colnames(res) <- lambda

  if (nlambda == 1)
  {
    if (compute_Sigma == TRUE)
    {
      I_p <- diag(p)
      Sigma <-
        I_p - solve(crossprod(scaled_newx) + lambda * I_p) %*% crossprod(scaled_newx)
      Sigma_newx <- tcrossprod(scaled_newx %*% Sigma,
                               scaled_newx) + lambda * diag(n)
      return (list(mean = res,
                   sd = sqrt(diag(
                     Sigma_newx
                   )),
                   simulate = function(n) MASS::mvrnorm(n = n, mu = res, Sigma = Sigma_newx)))
    } else {

      return (res)

    }

  } else {
    # nlambda > 1

    if (compute_Sigma == TRUE)
    {
      i <- NULL
      `%op%` <-  foreach::`%do%`
      std <-
        foreach::foreach(i = 1:nlambda, .combine = cbind) %op% {
          I_p <- diag(p)
          Sigma <-
            I_p - solve(crossprod(scaled_newx) + lambda[i] * I_p) %*% crossprod(scaled_newx)
          sqrt(diag(
            tcrossprod(scaled_newx %*% Sigma,
                       scaled_newx) + lambda * diag(n)
          ))
        }
      colnames(std) <- lambda
      return (list(mean = res,
                   sd = std))
    } else {

      return (res)

    }
  }
}

# predict from an rvfl mcmc ----

predict_rvfl_mcmc <- function(fit_obj,
                              newx,
                              ci = NULL,
                              graph = FALSE)
{
  if (is.vector(newx))
    newx <- t(newx)

  lambda <- fit_obj$obj[[1]]$lambda
  nlambda <- length(lambda)
  nb_iters <- length(fit_obj$obj)

  res <- foreach::foreach(i = 1:nb_iters,
                          .verbose = FALSE,
                          .combine = cbind,
                          .errorhandling = "stop",
                          .packages='cclust')%do%{

                            fit_obj_i <- fit_obj$obj[[i]]

                            if (is.null(fit_obj_i$clust_obj))
                            {
                              augmented_newx <- create_new_predictors(
                                  x = newx,
                                  nb_hidden = fit_obj_i$nb_hidden,
                                  nn_xm = fit_obj_i$nn_xm,
                                  nn_scales = fit_obj_i$nn_scales,
                                  activ = fit_obj_i$activ,
                                  nodes_sim = fit_obj_i$nodes_sim)$predictors

                            } else {

                              pred_clusters <- predict(
                                fit_obj_i$clust_obj,
                                bayesianrvfl::my_scale(
                                  newx,
                                  xm = fit_obj_i$clusters_scales$means,
                                  xsd = fit_obj_i$clusters_scales$sds
                                )
                              )

                              newX_clust <- one_hot(pred_clusters$cluster,
                                                             fit_obj_i$n_clusters)

                              augmented_newx <- create_new_predictors(
                                x = cbind(newx, newX_clust),
                                nb_hidden = fit_obj_i$nb_hidden,
                                nn_xm = fit_obj_i$nn_xm,
                                nn_scales = fit_obj_i$nn_scales,
                                activ = fit_obj_i$activ,
                                nodes_sim = fit_obj_i$nodes_sim
                                )$predictors
                            }

                            xm <- as.vector(fit_obj_i$xm)
                            scales <- as.vector(fit_obj_i$scales)
                            scaled_newx <- my_scale(x = as.matrix(augmented_newx),
                                                    xm = xm,
                                                    xsd = scales)

                            drop(scaled_newx %*% as.matrix(fit_obj_i$coef) + fit_obj_i$ym)
                          }

  if (fit_obj$compute_Sigma)
  {
    return(list(mean = rowMeans(res),
           sd = apply(res, 1, sd)))
  } else {
    return(rowMeans(res))
  }

}

# predict from Matérn 5/2 model ----
predict_matern52 <- function(fit_obj, newx, ci = NULL)
{
  if (is.vector(newx))
    newx <- t(newx)

  y <- fit_obj$y
  xm <- fit_obj$xm
  ym <- fit_obj$ym
  xsd <- fit_obj$xsd
  X <- fit_obj$scaled_x
  sigma <- fit_obj$sigma
  l <- fit_obj$l
  lambda_krls <- fit_obj$lambda_krls
  K <- fit_obj$K
  mat_coefs <- fit_obj$mat_coefs
  compute_Sigma <- fit_obj$compute_Sigma
  scaled_newx <- my_scale(newx, xm = xm, xsd = xsd)
  inv_method <- fit_obj$inv_method
  n_newx <- nrow(newx)

  if (is.vector(scaled_newx)) {
    K_star <- matern52_kxy_cpp(
      x = X,
      y = scaled_newx,
      sigma = sigma,
      l = l
    )
  } else {
    K_star <-
      sapply(1:n_newx, function (i)
        matern52_kxy_cpp(
          x = X,
          y = scaled_newx[i, ],
          sigma = sigma,
          l = l
        ))
  }

  preds <- drop(crossprod(K_star, mat_coefs)) + ym

  if (compute_Sigma == TRUE)
  {
    K_star2 <- matern52_kxx_cpp(x = scaled_newx,
                                sigma = sigma, l = l)
    shrinked_K <- switch(
      inv_method,
      "chol" = chol2inv(chol(K + lambda_krls * diag(nrow(
        K
      )))),
      "ginv" = bayesianrvfl::my_ginv(K + lambda_krls * diag(nrow(K)))
    )
    Sigma <-
      sqrt(diag(K_star2 - crossprod(K_star, shrinked_K) %*% K_star))
    return (list(mean = preds,
                 sd = Sigma))
  } else {
    return(preds)
  }
}

# predicting from elastic net ----
predict_glmnet <- function(fit_obj, newx, s = 0.1)
{
  if (is.vector(newx))
    newx <- t(newx)

  if (fit_obj$compute_Sigma == FALSE)
  {
    newx <- create_new_predictors(
      x = newx,
      nb_hidden = fit_obj$nb_hidden,
      nn_xm = fit_obj$nn_xm,
      nn_scales = fit_obj$nn_scales,
      activ = fit_obj$activ,
      nodes_sim = fit_obj$nodes_sim
    )$predictors

    scaled_newx <- my_scale(
      x = as.matrix(newx),
      xm = as.vector(fit_obj$xm),
      xsd = as.vector(fit_obj$scales)
    )

    return(predict(fit_obj$fit_obj, newx = scaled_newx, s = s))

  } else {
    #fit_obj$compute_Sigma != FALSE

    n_resamples <- length(fit_obj$fit_obj_list)

    boot_preds <-
      foreach::foreach(i = 1:n_resamples, .combine = cbind) %do% {
        newxpreds <- create_new_predictors(
          x = newx,
          nb_hidden = fit_obj$nb_hidden,
          nodes_sim = fit_obj$nodes_sim,
          activ = fit_obj$activ
        )$predictors

        predict(
          fit_obj$fit_obj_list[[i]],
          newx = my_scale(
            x = as.matrix(newxpreds),
            xm = as.vector(fit_obj$xm[[i]]),
            xsd = as.vector(fit_obj$scales[[i]])
          ),
          s = s
        )
      }

    return(list(
      mean = rowMeans(boot_preds),
      sd = apply(boot_preds, 1, sd)
    ))
  }

}
