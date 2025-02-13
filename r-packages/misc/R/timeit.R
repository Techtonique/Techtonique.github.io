#' Timing an expression
#' 
#' @param expr an R expression
#' @param times number of repetitions
#' @param ... additional arguments passed to \code{\link{base::eval}}
#' @return the elapsed time in seconds
#' @examples
#' 
#' timeit(1 + 1)
#' timeit(1 + 1, times = 10)
#' 
#' @export 
timeit <- function(expr, times = 1, ...) {
  expr <- substitute(expr)
  if (times == 1) {
    system.time(eval(expr, ...))[3]
  } else {
    times_ <- numeric(times)
    for (i in seq_along(times_)) {
      times_[i] <- system.time(eval(expr, ...))[3]
    }
    summary(times_)
  }
}