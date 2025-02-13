
# fitting base rvfl ----
#' Fitting base rvfl
#'
#' @param x
#' @param y
#' @param nb_hidden
#' @param n_clusters
#' @param nodes_sim
#' @param activ
#' @param lambda
#' @param method
#' @param compute_Sigma
#' @param seed
#'
#' @return
#' @export
#'
#' @examples
fit_rvfl <- function(x,
                     y,
                     nb_hidden = 5,
                     n_clusters = 0,
                     nodes_sim = c("sobol", "halton", "unif"),
                     activ = c("relu", "sigmoid", "tanh",
                               "leakyrelu", "elu", "linear"),
                     lambda = 10 ^ seq(from = -10,
                                       to = 10,
                                       length.out = 100),
                     method = c("svd", "solve", "chol"),
                     compute_Sigma = FALSE,
                     seed = 123)
{
  if (!is.null(dim(y)))
    stop("'y' must be a vector") # otherwise y - ym is not working  
  
  stopifnot(n_clusters == 0 || n_clusters > 1)
  
  ## regression ----
  stopifnot(nb_hidden > 0)
  x <- as.matrix(x)
  y <- as.vector(y)
  nlambda <- length(lambda)
  method <- match.arg(method)
  
  ym <- mean(y)
  centered_y <- y - ym
  
  nodes_sim <- match.arg(nodes_sim)
  activ <- match.arg(activ)
  init_x_scaled <- bayesianrvfl::my_scale(x)
  X_clust_obj <- NULL
  
  if (n_clusters > 0)
  {
    set.seed(seed)
    X_clust_obj <- cclust::cclust(x = init_x_scaled$res,
                                  centers = n_clusters)
    X_clust <- one_hot(X_clust_obj$cluster,
                       n_clusters)
    list_xreg <- create_new_predictors(
      x = cbind(x, X_clust),
      nb_hidden = nb_hidden,
      nodes_sim = nodes_sim,
      activ = activ
    )
    x_scaled <- my_scale(list_xreg$predictors)
  } else {
    # if (n_clusters <= 0)
    list_xreg <- create_new_predictors(
      x = x,
      nb_hidden = nb_hidden,
      nodes_sim = nodes_sim,
      activ = activ
    )
    x_scaled <- my_scale(list_xreg$predictors)
  }
  
  X <- x_scaled$res  
  XTX <- crossprod(X)
  
  if (method == "svd")
  {
    # inspired from MASS::lm.ridge
    Xs <- La.svd(X)
    rhs <- crossprod(Xs$u, centered_y)
    d <- Xs$d
    nb_di <- length(d)
    div <- d ^ 2 + rep(lambda, rep(nb_di, nlambda))
    a <- drop(d * rhs) / div
    dim(a) <- c(nb_di, nlambda)
    n <- nrow(X)
    
    if (nlambda == 1)
    {
      vt <- Xs$vt
      coef <- crossprod(vt, a)
      centered_y_hat <- X %*% coef
      GCV <-
        colSums((centered_y - centered_y_hat) ^ 2) / (n - colSums(matrix(d ^ 2 /
                                                                           div, nb_di))) ^ 2
      
      if (compute_Sigma == TRUE)
      {
        rhsX <- crossprod(Xs$u, X)
        aX <- drop(d * rhsX) / div
        Sigma <- diag(ncol(X)) - crossprod(vt, aX)
        rownames(Sigma) <- colnames(X)
        
        return(
          list(
            coef = drop(coef),
            #Dn = Dn,
            scales = x_scaled$xsd,
            Sigma = Sigma,
            lambda = lambda,
            ym = ym,
            xm = x_scaled$xm,
            n_clusters = n_clusters,
            clusters_scales = list(means = init_x_scaled$xm,
                                   sds = init_x_scaled$xsd),
            clust_obj = X_clust_obj,
            nb_hidden = nb_hidden,
            nn_xm = list_xreg$nn_xm,
            nn_scales = list_xreg$nn_scales,
            nodes_sim = nodes_sim,
            activ = activ,
            fitted_values = drop(ym +  centered_y_hat),
            GCV = GCV,
            compute_Sigma = compute_Sigma,
            x = x,
            y = y
          )
        )
      } else {
        #else: compute_Sigma == FALSE && nlambda == 1
        
        return(
          list(
            coef = drop(coef),
            #Dn = Dn,
            scales = x_scaled$xsd,
            lambda = lambda,
            ym = ym,
            xm = x_scaled$xm,
            n_clusters = n_clusters,
            clusters_scales = list(means = init_x_scaled$xm,
                                   sds = init_x_scaled$xsd),
            clust_obj = X_clust_obj,
            nb_hidden = nb_hidden,
            nn_xm = list_xreg$nn_xm,
            nn_scales = list_xreg$nn_scales,
            nodes_sim = nodes_sim,
            activ = activ,
            fitted_values = drop(ym +  centered_y_hat),
            GCV = GCV,
            compute_Sigma = compute_Sigma,
            x = x,
            y = y
          )
        )
      }
    } else {
      #else: nlambda > 1
      
      coef <- crossprod(Xs$vt, a)
      colnames(coef) <- lambda
      centered_y_hat <- X %*% coef
      fitted_values <- drop(ym +  centered_y_hat)
      colnames(fitted_values) <- lambda
      GCV <-
        colSums((centered_y - centered_y_hat) ^ 2) / (n - colSums(matrix(d ^ 2 /
                                                                           div,
                                                                         nb_di))) ^
        2
      
      if (compute_Sigma == TRUE)
      {
        #compute_Sigma == TRUE && nlambda > 1
        
        rhsX <- crossprod(Xs$u, X)
        `%op%` <-  foreach::`%do%`
        i <- NULL
        
        Sigma <- foreach::foreach(i = 1:nlambda) %op% {
          div_i <- d ^ 2 + rep(lambda[i], rep(nb_di, 1))
          aX_i <- drop(d * rhsX) / div_i
          Sigma <- diag(ncol(X)) - crossprod(Xs$vt, aX_i)
          rownames(Sigma) <- colnames(X)
          Sigma
        }
        names(Sigma) <- lambda
        
        return(
          list(
            coef = drop(coef),
            scales = x_scaled$xsd,
            Sigma = Sigma,
            lambda = lambda,
            ym = ym,
            xm = x_scaled$xm,
            n_clusters = n_clusters,
            clusters_scales = list(means = init_x_scaled$xm,
                                   sds = init_x_scaled$xsd),
            clust_obj = X_clust_obj,
            nb_hidden = nb_hidden,
            nn_xm = list_xreg$nn_xm,
            nn_scales = list_xreg$nn_scales,
            nodes_sim = nodes_sim,
            activ = activ,
            fitted_values = fitted_values,
            GCV = GCV,
            compute_Sigma = compute_Sigma,
            x = x,
            y = y
          )
        )
      } else {
        #else: compute_Sigma == FALSE && length(lambda) == 1
        
        return(
          list(
            coef = drop(coef),
            scales = x_scaled$xsd,
            lambda = lambda,
            ym = ym,
            xm = x_scaled$xm,
            n_clusters = n_clusters,
            clusters_scales = list(means = init_x_scaled$xm,
                                   sds = init_x_scaled$xsd),
            clust_obj = X_clust_obj,
            nb_hidden = nb_hidden,
            nn_xm = list_xreg$nn_xm,
            nn_scales = list_xreg$nn_scales,
            nodes_sim = nodes_sim,
            activ = activ,
            fitted_values = drop(ym +  centered_y_hat),
            GCV = GCV,
            compute_Sigma = compute_Sigma,
            x = x,
            y = y
          )
        )
      }
    }
  }
  
  if (method %in% c("solve", "chol"))
  {
    Sigma <- NULL
    
    if (nlambda > 1)
    { 

      Dn <- vector("list", length = nlambda)
      names(Dn) <- lambda

      if (compute_Sigma == TRUE)
      {
        Sigma <- vector("list", length = nlambda)
        names(Sigma) <- lambda
      }

      if (identical(method, "solve"))
      {
        if (compute_Sigma == TRUE)
        {
          coef <- foreach::foreach(i = 1:nlambda,
                                   .combine = cbind) %do% {
                                     Dn[[i]] <- solve(XTX + diag(x = lambda[i],
                                                                 nrow = nrow(XTX))) # Cn^{-1}
                                     Sigma[[i]] <- diag(ncol(X)) - Dn[[i]] %*% XTX # Sigma_n
                                     Dn[[i]] %*% crossprod(X, centered_y) # beta_n
                                   }
        } else { # identical(method, "chol")
          coef <- foreach::foreach(i = 1:nlambda,
                                   .combine = cbind) %do% {
                                     Dn[[i]] <- solve(XTX + diag(x = lambda[i],
                                                                 nrow = nrow(XTX))) # Cn^{-1}
                                     Dn[[i]] %*% crossprod(X, centered_y) # beta_n
                                   }
        }
      }

    } else { # if (nlambda == 1)
      Sigma <- NULL 
        if (identical(method, "solve"))
      {        
        Dn <- solve(XTX + diag(x = lambda, nrow = nrow(XTX))) # Cn^{-1}                  
      } else { # identical(method, "chol")
        Dn <- chol2inv(chol(XTX + diag(x = lambda, nrow = nrow(XTX)))) # Cn^{-1}                  
      }          
      coef <- Dn %*% crossprod(X, centered_y) # beta_n                        
      if (compute_Sigma == TRUE)        
          Sigma <- diag(ncol(X)) - Dn %*% XTX # Sigma_n                           
    }

      return(
        list(
          coef = coef,
          Dn = Dn,
          Sigma = Sigma,
          scales = x_scaled$xsd,
          lambda = lambda,
          ym = ym,
          xm = x_scaled$xm,
          n_clusters = n_clusters,
          clusters_scales = list(means = init_x_scaled$xm,
                                 sds = init_x_scaled$xsd),
          clust_obj = X_clust_obj,
          nb_hidden = nb_hidden,
          nodes_sim = nodes_sim,
          activ = activ,
          nn_xm = list_xreg$nn_xm,
          nn_scales = list_xreg$nn_scales,
          fitted_values = drop(ym + X %*% coef),
          compute_Sigma = compute_Sigma,
          x = x,
          y = y,
          n_updates = 0,
          avg_coefs = coef
        )
      )
    }    
  }
  
  
  # fitting rvfl mcmc ----
  fit_rvfl_mcmc <- function(x,
                            y,
                            cl = NULL,
                            nodes_sim = c("sobol", "halton", "unif"),
                            activ = c("relu", "sigmoid", "tanh",
                                      "leakyrelu", "elu", "linear"),
                            compute_Sigma = FALSE)
  {
    lambda <- 10 ^ seq(from = -5,
                       to = 4,
                       length.out = 50)
    vec_nb_hidden <- c(c(5, 10, 25, 40, 50),
                       floor(1000 * randtoolbox::sobol(15)))
    vec_n_clusters <- c(2, 3, 4, 5, 6)
    nodes_clusters_df <-
      expand_grid_df(vec_nb_hidden, vec_n_clusters)
    nb_iters <- nrow(nodes_clusters_df)
    nodes_sim <- match.arg(nodes_sim)
    activ <- match.arg(activ)
    
    allowParallel <- !is.null(cl) && cl > 0
    
    if (allowParallel)
    {
      cl_SOCK <- parallel::makeCluster(cl, type = "SOCK")
      doSNOW::registerDoSNOW(cl_SOCK)
      #`%op%` <-  foreach::`%dopar%`
      
      #pb <- txtProgressBar(min = 0, max = nb_iters, style = 3)
      #progress <- function(n) utils::setTxtProgressBar(pb, n)
      #opts <- list(progress = progress)
      
      res <- foreach::foreach(
        i = 1:nb_iters,
        .packages = c("doSNOW", "Rcpp"),
        #.options.snow = opts,
        .errorhandling = "remove",
        .verbose = FALSE
      ) %dopar% {
        bayesianrvfl::fit_rvfl(
          x = x,
          y = y,
          nb_hidden = nodes_clusters_df[i, 1],
          n_clusters = nodes_clusters_df[i, 2],
          nodes_sim = nodes_sim,
          activ = activ,
          lambda = lambda,
          method = "svd",
          compute_Sigma = FALSE
        )
      }
      #close(pb)
    }  else {
      #`%op%` <-  foreach::`%do%`
      #pb <- txtProgressBar(min = 0, max = nb_iters, style = 3)
      res <- foreach::foreach(
        i = 1:nb_iters,
        .verbose = FALSE,
        .errorhandling = "remove"
      ) %do% {
        #setTxtProgressBar(pb, i)
        bayesianrvfl::fit_rvfl(
          x = x,
          y = y,
          nb_hidden = nodes_clusters_df[i, 1],
          n_clusters = nodes_clusters_df[i, 2],
          nodes_sim = nodes_sim,
          activ = activ,
          lambda = lambda,
          method = "svd",
          compute_Sigma = FALSE
        )
      }
      #close(pb)
    }
    
    return(list(obj = res,
                compute_Sigma = compute_Sigma))
  }
  
  
  # fitting Matérn 5/2 model ----
  fit_matern52 <- function(x,
                           y,
                           sigma = 2,
                           l = 0.1,
                           lambda_krls = 0.1,
                           inv_method = c("chol", "ginv"),
                           compute_Sigma = FALSE)
  {
    if (!is.vector(y))
      stop("'y' must be a vector") # otherwise y - ym is not working
    x <- as.matrix(x)
    y <- as.vector(y)
    inv_method <- match.arg(inv_method)
    
    ym <- mean(y)
    centered_y <- y - ym
    
    x_scaled <- my_scale(x)
    X <- x_scaled$res
    xm <- x_scaled$xm
    xsd <- x_scaled$xsd
    
    K <- bayesianrvfl::matern52_kxx_cpp(x = X,
                                        sigma = sigma,
                                        l = l)
    mat_coefs <- switch(
      inv_method,
      "chol" = chol2inv(chol(K + lambda_krls * diag(nrow(
        K
      )))) %*% centered_y,
      "ginv" = my_ginv(K + lambda_krls * diag(nrow(K))) %*% centered_y
    )
    
    lsfit <- drop(crossprod(K, mat_coefs))
    fitted_values <- lsfit  + rep(ym, length(lsfit))
    resid <- y - fitted_values
    
    return(
      list(
        y = y,
        sigma = sigma,
        l = l,
        K = K,
        lambda_krls = lambda_krls,
        inv_method = inv_method,
        mat_coefs = mat_coefs,
        fitted_values = fitted_values,
        ym = ym,
        xm = xm,
        xsd = xsd,
        scaled_x = X,
        resid = resid,
        compute_Sigma = compute_Sigma
      )
    )
    
  }
  
  
  # fitting elastic net ----
  fit_glmnet <- function(x,
                         y,
                         nb_hidden = 5,
                         nodes_sim = c("sobol", "halton", "unif"),
                         activ = c("relu", "sigmoid", "tanh",
                                   "leakyrelu", "elu", "linear"),
                         compute_Sigma = FALSE,
                         ...)
  {
    if (!is.vector(y))
      stop("'y' must be a vector") # otherwise y - ym is not working
    x <- as.matrix(x)
    y <- as.vector(y)
    ym <- mean(y)
    centered_y <- y - ym
    
    nodes_sim <- match.arg(nodes_sim)
    activ <- match.arg(activ)
    
    if (compute_Sigma == FALSE)
    {
      list_xreg <-
        bayesianrvfl::create_new_predictors(
          x = x,
          nb_hidden = nb_hidden,
          nodes_sim = nodes_sim,
          activ = activ
        )
      
      x_scaled <- my_scale(list_xreg$predictors)
      X <- x_scaled$res
      
      fit_obj <- glmnet::glmnet(x = X, y = y,
                                ...)
      
      # obtain fitted values
      
      fitted_values <- predict(fit_obj, newx = X)
      
      return(
        list(
          fit_obj = fit_obj,
          scales = x_scaled$xsd,
          ym = ym,
          xm = x_scaled$xm,
          nb_hidden = nb_hidden,
          nn_xm = list_xreg$nn_xm,
          nn_scales = list_xreg$nn_scales,
          nodes_sim = nodes_sim,
          activ = activ,
          y = y,
          fitted_values = fitted_values,
          resid = y - fitted_values,
          compute_Sigma = compute_Sigma
        )
      )
    } else {
      x_scaled_list <- xreg_list <- X_list <- vector("list", 100)
      fit_obj_list <-
        foreach::foreach(i = 1:100, .errorhandling = "pass") %do% {
          xreg_list[[i]] <-
            bayesianrvfl::create_new_predictors(
              x = bayesianrvfl::resample(x, seed = i),
              nb_hidden = nb_hidden,
              nodes_sim = nodes_sim,
              activ = activ
            )
          x_scaled_list[[i]] <- my_scale(xreg_list[[i]]$predictors)
          X_list[[i]] <- x_scaled_list[[i]]$res
          suppressWarnings(glmnet::glmnet(x = X_list[[i]], y = y,
                                          ...))
        }
      
      return(
        list(
          fit_obj_list = fit_obj_list,
          scales = lapply(1:length(fit_obj_list), function(i)
            x_scaled_list[[i]]$xsd),
          xm = lapply(1:length(fit_obj_list), function(i)
            x_scaled_list[[i]]$xm),
          nb_hidden = nb_hidden,
          nn_xm = lapply(1:length(fit_obj_list), function(i)
            xreg_list[[i]]$nn_xm),
          nn_scales = lapply(1:length(fit_obj_list), function(i)
            xreg_list[[i]]$nn_scales),
          nodes_sim = nodes_sim,
          activ = activ,
          y = y,
          compute_Sigma = compute_Sigma
          #resid = y - fitted_values
        )
      )
    }
  }