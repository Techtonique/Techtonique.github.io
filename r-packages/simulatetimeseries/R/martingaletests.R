#' Martingale Test
#'
#' @param x A numeric vector.
#' @return A list of p-values for the martingale tests.
#' @export
martingale_test <- function(x)
{

 foo <- function(j)
  {
    res <- try(summary(lm(y ~ trend - 1, 
     data = cbind.data.frame(y=diff_x, 
                             trend=as.numeric(trends[, j]))))$coefficients[4])
    if (inherits(res, "try-error")) {
      return(NA)
    }
    return(res)
  } 

if (is.null(dim(x))){
      n <- length(x)
      trend <- seq_len(n-1)
 trends <- cbind.data.frame(y = diff_x,
                             trend1 = trend, 
                             trend2 = trend**2,
                             trend3 = trend**3,
                             trend4 = 1/trend,
                             trend4 = 1/(trend**2),
                             trend5 = 1/(trend**3),
                             trend6 = sqrt(trend), 
                             trend7 = log(trend))
  diff_x <- diff(x)
  

  results <- lapply(2:ncol(trends), 
                    function(j) foo(j))
  names(results) <- c("degree1", "degree2", "degree3", 
  "1_over_trend", "1_over_trend2", "1_over_trend3", 
  "sqrt_trend", "log_trend")
  return(results)
} else {
  n <- ncol(x)
  diff_x <- colMeans(apply(x, 1, diff))
  trend <- seq_len(n-1)
  trends <- cbind.data.frame(y = diff_x,
                             trend1 = trend, 
                             trend2 = trend**2,
                             trend3 = trend**3,
                             trend4 = 1/trend,
                             trend4 = 1/(trend**2),
                             trend5 = 1/(trend**3),
                             trend6 = sqrt(trend), 
                             trend7 = log(trend))
  results <- lapply(2:ncol(trends), 
                    function(j) foo(j))
  names(results) <- c("degree1", "degree2", "degree3", 
  "1_over_trend", "1_over_trend2", "1_over_trend3", 
  "sqrt_trend", "log_trend")
  return(results)
}

}