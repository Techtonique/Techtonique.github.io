#' Title
#'
#' @param n
#' @param cor_matrix
#'
#' @return
#' @export
#'
#' @examples
#'
#' cor_matrix <- matrix(c(1, 0.6, 0.6, 1), nrow = 2, byrow = TRUE)
#' correlated_gaussian <- simulate_correlated_gaussian(n=100,
#' cor_matrix=cor_matrix)
#' print(cor(correlated_gaussian))
#'
simulate_correlated_gaussian <- function(n=100L, cor_matrix=diag(3)) {

  # Perform Cholesky decomposition
  chol_matrix <- chol(cor_matrix)

  # Number of variables
  num_vars <- ncol(cor_matrix)

  # Generate uncorrelated Gaussian random variables
  uncorrelated_data <- matrix(rnorm(n * num_vars), ncol = num_vars)

  # Generate correlated data
  correlated_data <- uncorrelated_data %*% chol_matrix

  return(correlated_data)
}


# # Define correlation matrix
# cor_matrix <- matrix(c(1, 0.6, 0.6, 1), nrow = 2, byrow = TRUE)
#
# # Number of simulations
# num_simulations <- 1000
#
# # Simulate correlated Gaussian random variables
# simulated_data <- simulate_correlated_gaussian(num_simulations, cor_matrix)
#
# # Check the first few rows of simulated data
# head(simulated_data)
