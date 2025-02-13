# 1 - multistart nlminb ---------------------------------------------------------------

msnlminb <- function(objective, nb_iter = 100,
                     lower, upper, cl = NULL,
                     ...)
{
  ldots <- list(...)
  OF <- function(u, ...)
    {return(objective(u, ...))}

  rep_1_nb_iter <- rep(1, nb_iter)
  lower_mat <- tcrossprod(rep_1_nb_iter, lower)
  upper_mat <- tcrossprod(rep_1_nb_iter, upper)

  starting_points <- lower_mat + (upper_mat - lower_mat)*randtoolbox::sobol(n = nb_iter,
                                                                            dim = length(lower))
  nb_iter <- nrow(starting_points)

  pb <- txtProgressBar(min = 0, max = nb_iter, style = 3)

  if (is.null(cl)) {

    res <- foreach::foreach(i = 1:nb_iter,
                            .export = "ldots",
                            .verbose = FALSE,
                            .errorhandling = "remove")%do%{
                              setTxtProgressBar(pb, i)
                              stats::nlminb(
                                start = starting_points[i, ],
                                objective = OF,
                                lower = lower,
                                upper = upper,
                                ...
                              )
                            }
    close(pb)

  } else {

    cl_SOCK <- parallel::makeCluster(cl, type = "SOCK")
    doSNOW::registerDoSNOW(cl_SOCK)

    pb <- txtProgressBar(min = 0, max = nb_iter,
                         style = 3)

    progress <- function(n) utils::setTxtProgressBar(pb, n)
    opts <- list(progress = progress)

    i <- j <- NULL
    res <- foreach::foreach(i = 1:nb_iter,
                            .packages = "doSNOW",
                            .options.snow = opts,
                            .verbose = FALSE,
                            .errorhandling = "remove")%dopar%{ # fix this

                              stats::nlminb(
                                start = starting_points[i, ],
                                objective = OF,
                                lower = lower,
                                upper = upper,
                                ...
                              )

                            }

    stopCluster(cl_SOCK)
  }

  index_opt <- which.min(sapply(1:length(res),
                                function (i)
                                  res[[i]]$objective))

  return(res[[index_opt]])
}

# 2 - random search ---------------------------------------------------------------

random_search_opt <- function(objective, nb_iter = 100,
                              lower, upper, sim = c("sobol", "unif"),
                              seed = 123, cl = NULL, ...)
{
  OF <- function(y, ...) {return(objective(y, ...))}
  rep_1_nb_iter <- rep(1, nb_iter)
  lower_mat <- tcrossprod(rep_1_nb_iter, lower)
  upper_mat <- tcrossprod(rep_1_nb_iter, upper)
  sim <- match.arg(sim)

  if (sim == "sobol")
  {
    searched_points <- lower_mat + (upper_mat -
                                      lower_mat) * randtoolbox::sobol(n = nb_iter,
                                                                      dim = length(lower))
  }

  if (sim == "unif")
  {
    set.seed(seed)
    searched_points <- lower_mat + (upper_mat -
                                      lower_mat) * matrix(runif(nb_iter *
                                                                  length(lower)),
                                                          nrow = nrow(lower_mat))
  }

  if (is.null(cl)) {
    pb <- txtProgressBar(min = 1,
                         max = nb_iter,
                         style = 3)

    `%op%` <-  foreach::`%do%`

    res <- foreach::foreach(
      i = 1:nb_iter,
      .combine = c,
      .verbose = FALSE,
      .errorhandling = "remove"
    ) %op% {
      setTxtProgressBar(pb, i)
      res <- try(OF(searched_points[i, ], ...),
                 silent = TRUE)
      ifelse(class(res) == "try-error", 1e06, res)
    }

    close(pb)

  } else {
    cl_SOCK <- parallel::makeCluster(cl, type = "SOCK")
    doSNOW::registerDoSNOW(cl_SOCK)

    pb <- txtProgressBar(min = 0,
                         max = nb_iter,
                         style = 3)
    progress <- function(n)
      utils::setTxtProgressBar(pb, n)
    opts <- list(progress = progress)

    i <- j <- NULL
    res <-
      suppressWarnings(
        foreach::foreach(
          i = 1:nb_iter,
          .combine = c,
          .packages = "doSNOW",
          .options.snow = opts,
          .export = ...,
          .verbose = FALSE,
          .errorhandling = "remove"
        ) %dopar% {
          res <- try(OF(searched_points[i, ], ...),
                     silent = TRUE)
          ifelse(class(res) == "try-error", 1e06, res)
        }
      )
    snow::stopCluster(cl_SOCK)
  }

  index_opt <- which.min(res)

  return(list(par = searched_points[index_opt, ],
              objective = res[index_opt]))
}
random_search_opt <- memoise::memoise(f = random_search_opt)



# 3 - multistart nmkb ---------------------------------------------------------------

msnmkb <- function(objective, nb_iter = 100,
                     lower, upper, cl = NULL,
                     ...)
{
  OF <- function(u, ...)
  {return(objective(u, ...))}

  rep_1_nb_iter <- rep(1, nb_iter)
  lower_mat <- tcrossprod(rep_1_nb_iter, lower)
  upper_mat <- tcrossprod(rep_1_nb_iter, upper)

  starting_points <- lower_mat + (upper_mat - lower_mat)*randtoolbox::sobol(n = nb_iter,
                                                                            dim = length(lower))
  nb_iter <- nrow(starting_points)

  pb <- txtProgressBar(min = 0, max = nb_iter, style = 3)

  if (is.null(cl)) {

    res <- foreach::foreach(i = 1:nb_iter,
                            .export = "ldots",
                            .verbose = FALSE,
                            .packages = "dfoptim",
                            .errorhandling = "remove")%do%{
                              setTxtProgressBar(pb, i)
                              dfoptim::nmkb(par = starting_points[i, ],
                                            fn = OF,
                                            lower = lower,
                                            upper = upper,
                                            ...)
                            }
    close(pb)

  } else {

    cl_SOCK <- parallel::makeCluster(cl, type = "SOCK")
    doSNOW::registerDoSNOW(cl_SOCK)

    pb <- txtProgressBar(min = 0, max = nb_iter,
                         style = 3)

    progress <- function(n) utils::setTxtProgressBar(pb, n)
    opts <- list(progress = progress)

    i <- j <- NULL
    res <- foreach::foreach(i = 1:nb_iter,
                            .packages = c("doSNOW", "dfoptim"),
                            .options.snow = opts,
                            .verbose = FALSE,
                            .errorhandling = "remove")%dopar%{ # fix this

                              dfoptim::nmkb(par = starting_points[i, ],
                                            fn = OF,
                                            lower = lower,
                                            upper = upper,
                                            ...)

                            }

    parallel::stopCluster(cl_SOCK)
  }

  index_opt <- which.min(sapply(1:length(res),
                                function (i)
                                  res[[i]]$value))

  return(res[[index_opt]])
}
