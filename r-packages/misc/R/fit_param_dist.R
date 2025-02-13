#' Fit multiple parametric distributions, compute KL divergence, simulate best fit
#' 
#' @param vector Numeric vector of data to fit
#' @param num_bins Number of bins for the empirical histogram
#' @param verbose Logical indicating whether to print results
#' @return Function to simulate data from the best-fitting distribution
#' @export
#' @examples
#' 
#' set.seed(123)
#' n <- 1000
#' vector <- rnorm(n)
#' 
#' start <- proc.time()[3]
#' simulate_function <- fit_param_dist(vector)
#' end <- proc.time()[3]
#' print(paste("Time taken:", end - start))
#' simulated_data <- simulate_function(n)  # Generate 100 samples from the best-fit distribution
#' par(mfrow = c(1, 2))
#' hist(vector, main = "Original Data", xlab = "Value", ylab = "Frequency")
#' hist(simulated_data, main = "Simulated Data", xlab = "Value", ylab = "Frequency")
#' 
fit_param_dist <- function(vector, num_bins = 30, verbose = TRUE) {
  # Step 1: Create empirical histogram
  hist_empirical <- hist(vector, breaks = num_bins, plot = FALSE)
  P_empirical <- hist_empirical$counts / sum(hist_empirical$counts)  # Normalized counts
  bin_midpoints <- hist_empirical$mids  # Midpoints of histogram bins
  
  # List of distributions to try
  distributions <- c("beta", "cauchy", "chi-squared", "exponential", "gamma", "geometric", 
                     "log-normal", "logistic", "negative binomial", "normal", "t", "weibull")
  
  # Storage for KL divergences and fitted PDFs
  kl_divergences <- rep(Inf, length(distributions))  # Use Inf for initialization
  pdf_list <- vector("list", length(distributions))
  names(pdf_list) <- distributions
  best_params <- vector("list", length(distributions))
  names(best_params) <- distributions
  
  # Step 2: Loop over distributions
  for (dist in distributions) {
    fit <- suppressWarnings(try(
      # Fitting the distribution
      switch(dist,
            "log-normal" = MASS::fitdistr(vector, "lognormal"),
            "normal" = MASS::fitdistr(vector, "normal"),
            "exponential" = MASS::fitdistr(vector[vector >= 0], "exponential"),
            "weibull" = MASS::fitdistr(vector, "weibull"),
            "gamma" = MASS::fitdistr(vector[vector >= 0], "gamma"),
            "beta" = MASS::fitdistr(vector[vector >= 0 & vector <= 1], "beta", start = list(shape1 = 1, shape2 = 1)),
            "cauchy" = MASS::fitdistr(vector, "cauchy"),
            "chi-squared" = MASS::fitdistr(vector[vector >= 0], "chi-squared", start = list(df = 2)),
            "logistic" = MASS::fitdistr(vector, "logistic"),
            "negative binomial" = MASS::fitdistr(vector[vector >= 0], "negative binomial"),
            "t" = MASS::fitdistr(vector, "t"),
            "geometric" = MASS::fitdistr(vector[vector >= 0], "geometric"),
            stop("Unknown distribution")), silent = TRUE))
      
      # Step 3: Compute the probability density function for the given distribution
      pdf <- suppressWarnings(try(switch(dist,
                "log-normal" = dlnorm(bin_midpoints, meanlog = fit$estimate[1], sdlog = fit$estimate[2]),
                "normal" = dnorm(bin_midpoints, mean = fit$estimate[1], sd = fit$estimate[2]),
                "exponential" = dexp(bin_midpoints, rate = 1 / fit$estimate[1]),
                "weibull" = dweibull(bin_midpoints, shape = fit$estimate[1], scale = fit$estimate[2]),
                "gamma" = dgamma(bin_midpoints, shape = fit$estimate[1], rate = fit$estimate[2]),
                "beta" = dbeta(bin_midpoints, shape1 = fit$estimate[1], shape2 = fit$estimate[2]),
                "cauchy" = dcauchy(bin_midpoints, location = fit$estimate[1], scale = fit$estimate[2]),
                "chi-squared" = dchisq(bin_midpoints, df = fit$estimate[1]),
                "logistic" = dlogis(bin_midpoints, location = fit$estimate[1], scale = fit$estimate[2]),
                "negative binomial" = dnbinom(bin_midpoints, size = fit$estimate[1], mu = fit$estimate[2]),
                "t" = dt(bin_midpoints, df = fit$estimate[1]),
                "geometric" = dgeom(bin_midpoints, prob = fit$estimate[1]),
                stop("Unknown distribution")), silent = TRUE))                              

    # Store KL divergence, PDF, and parameters for simulation
    if (!inherits(fit, "try-error")){
      # Step 4: Compute KL divergence
      kl_div <- KL_divergence_hist(P_empirical, pdf)
      kl_divergences[which(distributions == dist)] <- kl_div
      pdf_list[[dist]] <- pdf
      best_params[[dist]] <- fit$estimate  # Save distribution parameters
    } else {
        next
    }      
  }
  
  min_best <- min(kl_divergences[!is.na(kl_divergences)])
  best_idx <- which(kl_divergences == min_best)
  best_distribution <- names(best_params)[best_idx]
  best_fit_params <- best_params[[best_distribution]]  # Parameters of the best model
  
  if (verbose) {
    print(kl_divergences)
    print(paste("Best distribution based on KL divergence:", best_distribution))   
  }
  
  simulate_best_fit <- function(n) {
    switch(best_distribution,
           "log-normal" = rlnorm(n, meanlog = best_fit_params[1], sdlog = best_fit_params[2]),
           "normal" = rnorm(n, mean = best_fit_params[1], sd = best_fit_params[2]),
           "exponential" = rexp(n, rate = 1 / best_fit_params[1]),
           "weibull" = rweibull(n, shape = best_fit_params[1], scale = best_fit_params[2]),
           "gamma" = rgamma(n, shape = best_fit_params[1], rate = best_fit_params[2]),
           "beta" = rbeta(n, shape1 = best_fit_params[1], shape2 = best_fit_params[2]),
           "cauchy" = rcauchy(n, location = best_fit_params[1], scale = best_fit_params[2]),
           "chi-squared" = rchisq(n, df = best_fit_params[1]),
           "logistic" = rlogis(n, location = best_fit_params[1], scale = best_fit_params[2]),
           "negative binomial" = rnbinom(n, size = best_fit_params[1], mu = best_fit_params[2]),
           "t" = rt(n, df = best_fit_params[1]),
           "geometric" = rgeom(n, prob = best_fit_params[1]),
           stop("Unknown distribution for simulation")
    )
  }
  
  return(simulate_best_fit)
}
