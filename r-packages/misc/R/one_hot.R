#' One-hot encoding
#' 
#' @param y A vector of class labels
#' @param n_classes The number of classes
#' @return A matrix of one-hot encoded labels
#' @export
#' @examples
#' 
#' y <- as.factor(c(1, 2, 1, 1, 2))
#' misc::one_hot_encode(y)
#' 
one_hot_encode <- function(y)
{
  # y must me `numeric`
  n_obs <- length(y)
  n_classes <- length(unique(y))
  res <- matrix(0, nrow=n_obs, ncol=n_classes)
  if (min(as.numeric(y) == 0)) # input index starting at 0 (index in R start at 1)
  {
    y_ <- y + 1L
    for (i in 1:n_obs){
      res[i, y_[i]] <- 1
    }
  } else { # input index not starting at 0
    for (i in 1:n_obs){
      res[i, y[i]] <- 1
    }
  }
  return(res)
}