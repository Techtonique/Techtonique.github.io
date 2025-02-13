#' Function to calculate KL divergence for continuous distributions using histograms
#' 
#' @param P Numeric vector representing the empirical distribution
#' @param Q Numeric vector representing the theoretical distribution
#' @return KL divergence between P and Q
#' @export
#' @examples
#' 
#' P <- c(0.2, 0.3, 0.5)
#' Q <- c(0.1, 0.4, 0.5)
#' misc::KL_divergence_hist(P, Q)
#' 
KL_divergence_hist <- function(P, Q) {
  # Ensure P and Q sum to 1 (normalize)
  P <- P / sum(P)
  Q <- Q / sum(Q)
  
  # Avoid log(0) and division by zero with small epsilon value
  epsilon <- 1e-10
  P <- P + epsilon
  Q <- Q + epsilon
  
  # Compute KL divergence
  kl_div <- sum(P * log(P / Q))
  
  return(kl_div)
}
