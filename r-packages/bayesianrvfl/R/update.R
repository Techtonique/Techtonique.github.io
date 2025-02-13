# 2 - update function -----

#' Update function
#'
#' @param fit_obj
#' @param newx
#' @param newy
#' @param re_clust
#' @param method
#' @param alpha
#'
#' @return
#' @export
#'
#' @examples
update_params <- function(fit_obj,
                          newx,
                          newy,
                          re_clust = TRUE,
                          method = c("direct", "polyak"),
                          alpha = 0.5)
{
  stopifnot(length(newx) >= 1) # newx is a vector
  stopifnot(is.null(dim(newy)) && length(newy) == 1) # newy is a scalar
  if (is.null(fit_obj$Dn))
    stop("for argument 'fit_obj', you should have 'method == chol' or 'method == solve' in function 'fit_rvfl'")
  method <- match.arg(method)

  # new information arriving in the system
  mat_newx <- matrix(newx, nrow = 1, byrow = TRUE) # along with newy
  # initial number of covariates
  p <- ncol(fit_obj$x)
  # number of observations at step n
  n <- nrow(fit_obj$x) # /!\ NOT NECESSARY TO STORE x (think about it)
  # number of regularization parameters
  nlambda <- length(fit_obj$lambda)

  #parameters at step n
  if (!is.null(fit_obj$Dn))
  {

    Dn <- fit_obj$Dn # Cn^{-1}

  }
  ym <- fit_obj$ym
  xm <- as.vector(fit_obj$xm)
  scales <- as.vector(fit_obj$scales)

  # regression parameters for step n + 1
  centered_newy <- newy - ym

  #print("here")

  if (is.null(fit_obj$clust_obj))
  {
    augmented_newx <- create_new_predictors(
      x = mat_newx,
      nb_hidden = fit_obj$nb_hidden,
      nodes_sim = fit_obj$nodes_sim,
      activ = fit_obj$activ,
      nn_xm = fit_obj$nn_xm,
      nn_scales = fit_obj$nn_scales
    )$predictors
  } else {

    pred_clusters <- predict(
      fit_obj$clust_obj,
      bayesianrvfl::my_scale(
        mat_newx,
        xm = fit_obj$clusters_scales$means,
        xsd = fit_obj$clusters_scales$sds
      )
    )

    newX_clust <- one_hot(pred_clusters$cluster,
                                   fit_obj$n_clusters)

    augmented_newx <-
      create_new_predictors(
        x = cbind(mat_newx, newX_clust),
        nb_hidden = fit_obj$nb_hidden,
        nodes_sim = fit_obj$nodes_sim,
        activ = fit_obj$activ,
        nn_xm = fit_obj$nn_xm,
        nn_scales = fit_obj$nn_scales
      )$predictors
  }

  scaled_augmented_newx <- my_scale(x = augmented_newx, xm = xm,
                                    xsd = scales)

  # ----- update parameters

  if (fit_obj$compute_Sigma)
  {
    ncol_Sigma <- ifelse(nlambda > 1,
                         ncol(fit_obj$Sigma[[1]]),
                         ncol(fit_obj$Sigma))
  }    

  if (method == "direct") 
  {
    if (nlambda > 1)
    {
      for (i in 1:nlambda) {

        # update factor
        temp <- tcrossprod(Dn[[i]], scaled_augmented_newx) # Cn^{-1}%*%t(mat_newx)
        update_factor <- Dn[[i]] - tcrossprod(temp) / (1 + drop(scaled_augmented_newx %*% temp))
        resids <- centered_newy - scaled_augmented_newx %*% fit_obj$coef

        # update regression coefficients and covariance with update factor
        gradients <- drop(sapply(seq_len(length(resids)),
                                 function (i) scaled_augmented_newx * resids[i]))

        if (is.null(dim(gradients)))
        {
          gradients <- matrix(gradients, ncol = 1,
                              byrow = FALSE)
        }           

        fit_obj$coef[, i] <- fit_obj$coef[, i] + update_factor %*% gradients[, i]

        if (fit_obj$compute_Sigma)
          fit_obj$Sigma[[i]] <-
            (diag(ncol_Sigma) - update_factor %*% crossprod(scaled_augmented_newx)) %*%
            fit_obj$Sigma[[i]]

      }
    } else {
      # nlambda == 1
      # update factor
      temp <- tcrossprod(Dn, scaled_augmented_newx) # Cn^{-1}%*%(t(mat_newx))
      update_factor <- Dn - tcrossprod(temp) / (1 + drop(scaled_augmented_newx %*% temp))
      resids <- drop(centered_newy - scaled_augmented_newx %*% fit_obj$coef)

      # update regression coefficients and covariance with update factor
      gradients <- as.vector(scaled_augmented_newx * resids)
      fit_obj$coef <- fit_obj$coef + update_factor %*% gradients

      if (fit_obj$compute_Sigma)
        fit_obj$Sigma <-
          (diag(ncol_Sigma) - update_factor %*% crossprod(scaled_augmented_newx)) %*%
          fit_obj$Sigma
    }
  } else {
    # else method == polyak avg
    # update factor
    stopifnot(alpha >= 0.5 && alpha < 1)
    update_factor <- n ^ (-alpha)

    if (nlambda > 1)
    {
      for (i in 1:nlambda) {
        # update factor
        temp <-
          tcrossprod(Dn[[i]], scaled_augmented_newx) # Cn^{-1}%*%t(mat_newx)
        resids <-
          centered_newy - scaled_augmented_newx %*% fit_obj$coef

        # update regression coefficients and covariance with update factor
        gradients <- drop(sapply(seq_len(length(resids)),
                                 function (ix) scaled_augmented_newx * resids[ix]))
        if (is.null(dim(gradients)))
          gradients <- matrix(gradients, ncol = 1,
                              byrow = FALSE)
          previous_coef <-  fit_obj$coef[, i]
          new_coef <- previous_coef + update_factor * gradients[, i]                            
          fit_obj$avg_coefs[, i] <- ((fit_obj$n_updates + 1)*fit_obj$avg_coefs[, i] +
           new_coef)/(fit_obj$n_updates + 2)
          fit_obj$coef[, i] <- fit_obj$avg_coefs[, i]
          fit_obj$n_updates <-  fit_obj$n_updates + 1
        if (fit_obj$compute_Sigma)
          fit_obj$Sigma[[i]] <- (diag(ncol_Sigma) - update_factor * crossprod(scaled_augmented_newx)) %*%fit_obj$Sigma[[i]]
      }
    } else {
      # nlambda == 1
      # update factor is a scalar
      temp <-
        tcrossprod(Dn, scaled_augmented_newx) # Cn^{-1}%*%(t(mat_newx))
      resids <-
        drop(centered_newy - scaled_augmented_newx %*% fit_obj$coef)

      # update regression coefficients and covariance with update factor
      gradients <- as.vector(scaled_augmented_newx * resids)
      previous_coef <- fit_obj$coef 
      new_coef <- previous_coef + update_factor * gradients
      fit_obj$avg_coefs <- ((fit_obj$n_updates + 1)*fit_obj$avg_coefs + new_coef)/(fit_obj$n_updates + 2)
      fit_obj$coef <- fit_obj$avg_coefs
      fit_obj$n_updates <-  fit_obj$n_updates + 1

      if (fit_obj$compute_Sigma)
        fit_obj$Sigma <- (diag(ncol_Sigma) - update_factor * crossprod(scaled_augmented_newx)) %*% fit_obj$Sigma
    }
  }

  # update response and covariates with new observations
  fit_obj$x <- rbind(as.matrix(fit_obj$x), newx)
  fit_obj$y <- c(fit_obj$y, newy)

  # update ym
  fit_obj$ym <- mean(fit_obj$y)

  # update xm
  if (is.null(fit_obj$clust_obj))
  {
    list_xreg <- create_new_predictors(
      x = fit_obj$x,
      nb_hidden = fit_obj$nb_hidden,
      nodes_sim = fit_obj$nodes_sim,
      activ = fit_obj$activ
    )
  } else {
    if (re_clust)
    {
      fit_obj$clust_obj <- cclust::cclust(fit_obj$x, fit_obj$n_clusters)
    }

    X_clust_obj  <- predict(
      fit_obj$clust_obj,
      bayesianrvfl::my_scale(
        fit_obj$x,
        xm = fit_obj$clusters_scales$means,
        xsd = fit_obj$clusters_scales$sds
      )
    )

    X_clust <- one_hot(X_clust_obj$cluster,
                                            fit_obj$n_clusters)

    list_xreg <-
      create_new_predictors(
        x = cbind(fit_obj$x, X_clust),
        nb_hidden = fit_obj$nb_hidden,
        nodes_sim = fit_obj$nodes_sim,
        activ = fit_obj$activ
      )
  }
  x_scaled <- my_scale(list_xreg$predictors)
  fit_obj$xm <- x_scaled$xm

  # update scales
  fit_obj$scales <- x_scaled$xsd

  # update nn xm and nn scales
  fit_obj$nn_xm <- list_xreg$nn_xm
  fit_obj$nn_scales <- list_xreg$nn_scales

  # update fitted values
  X <- x_scaled$res
  fit_obj$fitted_values <- drop(fit_obj$ym +  X %*% fit_obj$coef)

  # update Dn
  if (method == "direct")
  {
    XTX <- crossprod(X)
    if (nlambda > 1)
    {
      Dn <- vector("list", length = nlambda)
      names(Dn) <- fit_obj$lambda
      Dn <- lapply(1:nlambda, function (i)
        solve(XTX + diag(x = fit_obj$lambda[i],
                     nrow = ncol(XTX))
        ))
      fit_obj$Dn <- Dn
    } else {
      Dn <- solve(XTX + diag(
        x = fit_obj$lambda,
        nrow = ncol(XTX)
      ))
      fit_obj$Dn <- Dn
    }
  }

  # return a new fit_obj with updated data.
  return (fit_obj)
}


update_params_mcmc <- function(fit_obj,
                               newx,
                               newy,
                               re_clust = TRUE,
                               method = c("direct", "polyak"),
                               alpha = 0.5)
{
  if (is.vector(newx))
    newx <- t(newx)

  lambda <- fit_obj$obj[[1]]$lambda
  nlambda <- length(lambda)
  nb_iters <- length(fit_obj$obj)
  compute_Sigma <- fit_obj$obj[[1]]$compute_Sigma
  method <- match.arg(method)

  res <- foreach::foreach(i = 1:nb_iters,
                          .verbose = FALSE,
                          .errorhandling = "stop",
                          .packages='cclust')%do%{

                            fit_obj$obj[[i]] <- update_params(fit_obj$obj[[i]],
                                                              newx = newx,
                                                              newy = newy,
                                                              re_clust = re_clust,
                                                              method = method,
                                                              alpha = alpha)
                          }

 return(list(obj = res,
             compute_Sigma = compute_Sigma))
}
