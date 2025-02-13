# Data transformation -----------------------------------------------------

# calculate std's of columns
my_sd <- function(x)
{
  n <- dim(x)[1]
  return(drop(rep(1/n, n) %*% (x - tcrossprod(rep.int(1, n), colMeans(x)))^2)^0.5)
}
my_sd <- compiler::cmpfun(my_sd)

# scaling matrixes
my_scale <- function(x, xm = NULL, xsd = NULL)
{
  rep_1_n <- rep.int(1, dim(x)[1])

  # centering and scaling, returning the means and sd's
  if(is.null(xm) && is.null(xsd))
  {
    xm <- colMeans(x)
    xsd <- my_sd(x)
    return(list(res = (x - tcrossprod(rep_1_n, xm))/tcrossprod(rep_1_n, xsd),
                xm = xm,
                xsd = xsd))
  }

  # centering and scaling
  if(is.numeric(xm) && is.numeric(xsd))
  {
    return((x - tcrossprod(rep_1_n, xm))/tcrossprod(rep_1_n, xsd))
  }

  # centering only
  if(is.numeric(xm) && is.null(xsd))
  {
    return(x - tcrossprod(rep_1_n, xm))
  }

  # scaling only
  if(is.null(xm) && is.numeric(xsd))
  {
    return(x/tcrossprod(rep_1_n, xsd))
  }
}
my_scale <- compiler::cmpfun(my_scale)


# create new predictors ---------------------------------------------------

remove_zero_cols <- function(x)
{
  x[, colSums(x == 0) != nrow(x)]
}

is.wholenumber <-
  function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol

# create new predictors
create_new_predictors <- function(x, nb_hidden = 5,
                                  nodes_sim = c("sobol", "halton", "unif"),
                                  activ = c("relu", "sigmoid", "tanh",
                                            "leakyrelu", "elu", "linear"),
                                  nn_xm = NULL, nn_scales = NULL)
{
    x <- as.matrix(x)
    g <- switch(match.arg(activ),
              "relu" = function(x) x*(x>0),
              "sigmoid" = function(x) 1/(1 + exp(-x)),
              "tanh" = function(x) tanh(x),
              "leakyrelu" = function(x) x*(x > 0) + 0.01*x*(x <= 0),
              "elu" = function(x) x*(x >= 0) + 0.01*(exp(x)-1)*(x < 0),
              "linear" = function(x) x)

    p <- ncol(x)
    set.seed(1)
    w <- remove_zero_cols(switch(match.arg(nodes_sim),
                                 "sobol" = 2*t(randtoolbox::sobol(nb_hidden + 1, p)) - 1,
                                 "halton" = 2*t(randtoolbox::halton(nb_hidden, p)) - 1,
                                 "unif" = matrix(runif(nb_hidden*p, min = -1, max = 1),
                                                         nrow = p, ncol = nb_hidden)))

    if((!is.null(nn_xm) && is.null(nn_scales)) || (is.null(nn_xm) && !is.null(nn_scales)))
      stop("either nn_xm and nn_scales provided, or both left to NULL")

    if (is.null(nn_xm) && is.null(nn_scales))
    {
        scaled_x <- my_scale(x)
        hidden <- g(scaled_x$res%*%w)
        res <- cbind(x, hidden)

        if (length(colnames(x)) > 0){
          colnames(res) <- c(colnames(x),
                             paste0("h", 1:ncol(hidden)))
        } else {
          colnames(res) <- c(paste0("x", 1:ncol(x)),
                             paste0("h", 1:ncol(hidden)))
        }

        return(list(nn_xm = scaled_x$xm,
                    nn_scales = scaled_x$xsd,
                    w = w, predictors = res))
    }

    if (!is.null(nn_xm) && !is.null(nn_scales))
    {
        stopifnot(length(nn_xm) == ncol(x) || length(nn_scales) == ncol(x))
        scaled_x <- my_scale(as.matrix(x),
                             xm = as.vector(nn_xm),
                             xsd = as.vector(nn_scales))
        hidden <- g(as.matrix(scaled_x)%*%w)
        res <- cbind(x, hidden)

        if (length(colnames(x)) > 0){
          colnames(res) <- c(colnames(x),
                             paste0("h", 1:ncol(hidden)))
        } else {
          colnames(res) <- c(paste0("x", 1:ncol(x)),
                             paste0("h", 1:ncol(hidden)))
        }

        return(list(w = w, predictors = res))
    }


}


# sort data frame by column -----------------------------------------------

sort_df <- function(df, by_col)
{
  df[with(df, order(by_col)), ]
}


# generalized inverse -----------------------------------------------------

# From MASS::ginv
my_ginv <- function(X, tol = sqrt(.Machine$double.eps))
{
  if (length(dim(X)) > 2L || !(is.numeric(X) || is.complex(X)))
  {
    stop("'X' must be a numeric or complex matrix")
  }

  Xsvd <- La.svd(X)
  Positive <- Xsvd$d > max(tol * Xsvd$d[1L], 0)
  if (all(Positive))
  {
    return(crossprod(Xsvd$vt, (1/Xsvd$d * t(Xsvd$u))))
  }
  else if(!any(Positive))
  {
    return(array(0, dim(X)[2L:1L]))
  }
  else {
    return(crossprod(Xsvd$vt[, Positive, drop = FALSE], ((1/Xsvd$d[Positive]) *
                                                           t(Xsvd$u[, Positive, drop = FALSE]))))
  }
}
my_ginv <- compiler::cmpfun(my_ginv)


# one-hot-encoding of vector x_clusters on n_clusters -----------------------------------------------------

# to be used, in case on clusters is not represented (e.g in predict)
# keep n_clusters!
one_hot_encode <- function(x_clusters, n_clusters)
{
  stopifnot(max(x_clusters) <= n_clusters)
  n_obs <- length(x_clusters)

  # matrix of results
  res <- matrix(0, nrow = n_obs,
                ncol = n_clusters)

  for (i in 1:n_obs){
    res[i, x_clusters[i]] <- 1
  }

  return (res)
}


# convert a factor to a matrix of responses -----------------------------------------------------

# to be used on a training set
factor_to_matrix <- function(x)
{
  if (!is.factor(x)) x <- as.factor(x)

  levels_x <- sort(unique(x))
  n_levels_x <- length(levels_x)
  n <- length(x)
  res <- matrix(NA, nrow = n,
                ncol = n_levels_x)

    for (i in 1:n)
    {
      res[i, ] <- as.numeric(x[i] == levels_x)
    }

  colnames(res) <- levels(x)
  rownames(res) <- 1:n

  return(res)
}
