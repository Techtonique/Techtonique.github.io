#' Scale matrix
#' 
#' @param X A matrix
#' @param X_mean Mean of each column
#' @param X_sd Standard deviation of each column
#' @return A list containing the scaled matrix, mean of each column, and standard deviation of each column
#' @export
#' @examples
#' 
#' X <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
#' (X_scaled <- misc::scale_matrix(X))
#' (X_scaled <- misc::scale_matrix(X, X_mean = colMeans(X), X_sd = apply(X, 2, stats::sd)))
#' print(colMeans(X_scaled$X))
#' print(apply(X_scaled$X, 2, stats::sd))
#' 
scale_matrix <- function(X, X_mean=NULL, X_sd=NULL)
{
  if (is.null(X_mean))
  {
    X_mean <- colMeans(X)    
  }
  if (is.null(X_sd))
  {
    X_sd <- apply(X, 2, stats::sd)
  }
  X <- sweep(X, 2, X_mean, "-")
  X <- sweep(X, 2, X_sd, "/")
  return(list(X = X, X_mean = X_mean, X_sd = X_sd))
}