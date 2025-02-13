#' Sequential or parallel for loop.
#' 
#' @param what A function.
#' @param args A list of arguments.
#' @param cl Number of cores to use. If \code{NULL}, the loop will be sequential. It -1, the number of cores will be detected automatically.
#' @param combine A function to combine the results.
#' @param errorhandling A character string specifying how to handle errors. Possible values are \code{"stop"}, \code{"remove"}, and \code{"pass"}.
#' @param verbose A logical indicating whether to print progress.
#' @param show_progress A logical indicating whether to show a progress bar.
#' @param export A list of objects to export to the workers.
#' @param ... Additional arguments to pass to \code{what} for \code{\link{foreach::foreach}} (excludind \code{.combine}, \code{.errorhandling}, \code{.options.snow}, \code{.verbose}, and \code{.export}).
#' @export 
#' @returns A list of results.
#' @examples
#' 
#' # Sequential
#' print(misc::parfor(function(x) x^2, 1:10))
#' 
#' # Parallel
#' print(misc::parfor(function(x) x^2, 1:10, cl = 2))
#' 
parfor <- function(what,
                   args,
                   cl = NULL,
                   combine = c,
                   errorhandling = c("stop",
                                     "remove",
                                     "pass"),
                   verbose = FALSE,
                   show_progress = TRUE,
                   export = NULL,
                   ...)
{
  errorhandling <- match.arg(errorhandling)  
  
  n_iter <- length(args)
  
  if (!is.null(cl)) {
    # parallel
    stopifnot((floor(cl) == cl) && cl > -2)
    if (cl == -1)
    {
      cl_SOCK <-
        parallel::makeCluster(parallel::detectCores(), type = "SOCK")
    } else {
      cl_SOCK <- parallel::makeCluster(cl, type = "SOCK")
    }
    doSNOW::registerDoSNOW(cl_SOCK)
    `%op%` <- foreach::`%dopar%`
  } else {
    # sequential
    `%op%` <- foreach::`%do%`
  }
  
  if (show_progress)
  {
    pb <- utils::txtProgressBar(min = 0,
                                max = n_iter,
                                style = 3)
    progress <- function(n) {
      utils::setTxtProgressBar(pb, n)
    }
    opts <- list(progress = progress)
  } else {
    opts <- NULL
  }
  
  i <- NULL
  res <- foreach::foreach(
    i = 1:n_iter,
    .combine = combine,
    .errorhandling = errorhandling,
    .options.snow = opts,
    .verbose = verbose,
    .export = export,
    ... 
  ) %op% {
    if (identical(show_progress, TRUE))
    {
      utils::setTxtProgressBar(pb, i)
    }
    
    base::do.call(what = what,
                   args = c(list(args[i]), ...))
  }
  
  if (show_progress)
  {
    close(pb)
  }
  
  if (!is.null(cl))
  {
    snow::stopCluster(cl_SOCK)
  }
  
  return(unlist(res))
}