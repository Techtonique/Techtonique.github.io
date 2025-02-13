# likelihood minimization
min_loglik <- function(x, y,
                       surrogate_model = c("rvfl", "matern52"),
                       nodes_sim = c("sobol", "halton", "unif"),
                       activ = c("relu", "sigmoid", "tanh",
                                 "leakyrelu","elu", "linear"))
{
  n <- nrow(x)
  stopifnot(n == length(y))
  nodes_sim <- match.arg(nodes_sim)
  activ <- match.arg(activ)
  surrogate_model <- match.arg(surrogate_model)

  if (surrogate_model == "rvfl")
  {
    OF <- function(xx)
    {
      x_augmented <- create_new_predictors(x = x,
                                           nb_hidden = floor(xx[1]),
                                           nodes_sim = nodes_sim,
                                           activ = activ)$predictors
      Sigma <- tcrossprod(x_augmented, x_augmented) + xx[2]*diag(n)
      res <- try(0.5*(n*log(2*pi) + log(det(Sigma)) +
                        drop(crossprod(y, chol2inv(chol(Sigma)) )%*%y)),
                 silent = TRUE)

      ifelse(class(res) == "try-error", -1e06, res)
    }

    return(msnlminb(objective = OF, nb_iter = 50,
                    lower =  c(1, 0.01),
                    upper = c(100, 1e04)))
  }

  if (surrogate_model == "matern52")
  {
    OF <- function(xx)
    {
      Sigma <- matern52_kxx_cpp(x = x,
                                sigma = xx[1],
                                l = xx[2]) + xx[3]*diag(n)
      res <- try(0.5*(n*log(2*pi) + log(det(Sigma)) +
                        drop(crossprod(y, chol2inv(chol(Sigma)) )%*%y)),
                 silent = TRUE)

      ifelse(class(res) == "try-error", -1e06, res)
    }

    return(msnlminb(objective = OF, nb_iter = 50,
                    lower =  c(1e-04, 1e-04, 1e-04),
                    upper = c(1e05, 1e05, 1e04)))
  }
}

