

#' Simulate Black-Scholes-G2++ model
#'
#' @param n number of simulations
#' @param t0 valuation date (must be an integer > 1)
#' @param T maturity
#'
#' @return a list with the simulated short rate,
#' asset price and discount bond price
#'
#' @export
#'
#' @examples
#'
#' require(esgtoolkit)
#'
#' sims <- simulatetimeseries::simulate_bsg2plus(t0=1, n=10L,
#' horizon = 5L)
#'
#' par(mfrow=c(2,2))
#' esgtoolkit::esgplotbands(sims$discount_bond_prices, main="Discount bond prices", xlab="Maturity", ylab="Prices")
#' esgtoolkit::esgplotbands(window(sims$discount_rates, start=1), main="Discount rates", xlab="Maturity", ylab="Rates")
#' esgtoolkit::esgplotbands(sims$asset_price, main="Asset prices", xlab="Horizon", ylab="Prices")
#' esgtoolkit::esgplotbands(sims$short_rate, main="Short rates", xlab="Horizon", ylab="Rates")
#'
simulate_bsg2plus <- function(n = 100L,
                              t0 = 1L,
                              horizon = 5L,
                              K = 95)
{
  # Parameters of the BSG2++
  a_opt <- 0.50000000
  b_opt <- 0.35412030
  sigma_opt <- 0.09416266
  rho_opt <-
    -0.99855687 # correlation between x (eps[[1]]) and y (eps[[2]]) (G2++ model's factors)
  eta_opt <- 0.08439934
  eta_opT <- 0.05
  # initial value for the risky asset
  S0 <- 100
  # volatility mean-reversion speed
  kappa <- 0.003
  # volatility of volatility
  volvol <- 0.009
  # Correlation between stoch. vol and risky asset's prices
  rho <-
    -0.5 # Correlation between stoch. vol (eps[[3]]) and risky asset's prices (eps[[4]])
  # Initial variance
  V0 <- 0.04
  # long-term variance
  theta <- 0.04
  freq <- "annual"
  n_factors <- 4L
  
  # Observed time-to-maturities
  t0 <- t0 + 1
  stopifnot(t0 >= 1)
  stopifnot(horizon >= 1)
  stopifnot(is.numeric(t0) && (floor(t0) == t0))
  u <- 1:30
  T <- t0 + horizon
  
  if (!(t0 %in% u) || !(T %in% u))
  {
    stop("t0 and T must be in 1:30")
  }
  
  # Forecasting horizon
  delta_t <- 1
  (t_out <- seq(from = 0, to = horizon,
                by = delta_t))
  
  # Yield to maturities
  # convert to `ts` object
  txZC <- c(
    0.01422,
    # maturity = 1
    0.01309,
    0.01380,
    0.01549,
    0.01747,
    0.01940,
    0.02104,
    0.02236,
    0.02348,
    0.02446,
    0.02535,
    0.02614,
    0.02679,
    0.02727,
    0.02760,
    0.02779,
    0.02787,
    0.02786,
    0.02776,
    0.02762,
    0.02745,
    0.02727,
    0.02707,
    0.02686,
    0.02663,
    0.02640,
    0.02618,
    0.02597,
    0.02578,
    0.02563
  )
  
  # discount bonds
  # convert to `ts` object
  p <- c(
    0.9859794,
    # maturity = 1
    0.9744879,
    0.9602458,
    0.9416551,
    0.9196671,
    0.8957363,
    0.8716268,
    0.8482628,
    0.8255457,
    0.8034710,
    0.7819525,
    0.7612204,
    0.7416912,
    0.7237042,
    0.7072136,
    0.6922140,
    0.6785227,
    0.6660095,
    0.6546902,
    0.6441639,
    0.6343366,
    0.6250234,
    0.6162910,
    0.6080358,
    0.6003302,
    0.5929791,
    0.5858711,
    0.5789852,
    0.5722068,
    0.5653231
  )
  
  # 2 - simulation of gaussian correlated shocks ------
  # Shocks
  sigma <- diag(n_factors)
  sigma[1, 2] <- sigma[2, 1] <- rho_opt
  sigma[3, 4] <- sigma[4, 3] <- rho
  sigma[1, 3] <- sigma[3, 1] <- -0.01
  sigma[1, 4] <- sigma[4, 1] <- -0.01
  sigma[2, 4] <- sigma[4, 2] <- -0.01
  # Get eigenvalues and eigenvectors
  eigen_decomp <- eigen(sigma)
  eigenvalues <- eigen_decomp$values
  eigenvectors <- eigen_decomp$vectors
  # Add epsilon to negative eigenvalues
  epsilon <- .Machine$double.eps  # Adjust epsilon value as needed
  eigenvalues <- pmax(eigenvalues, epsilon)
  # Reconstruct the matrix with adjusted eigenvalues
  adjusted_sigma <-
    eigenvectors %*% diag(eigenvalues) %*% t(eigenvectors)
  
  # simulate shocks
  n_horizon <- n * horizon
  if (n_horizon%%2 == 0)
  {
    n_sims_half <- 0.5* n_horizon
  } else {
    n_sims_half <- floor(0.5*(n_horizon + 1))
  }
  eps_temp_half <- mvtnorm::rmvnorm(n = n_sims_half,
                               sigma = adjusted_sigma)
  eps_temp <- rbind(eps_temp_half, -eps_temp_half)
  if (nrow(eps_temp) != floor(0.5* n_horizon))
  {
    eps_temp[1:n_horizon, ]
  }
  eps <- lapply(1:n_factors, 
                function(i) ts(matrix(eps_temp[, i], ncol = n),
                                            deltat = delta_t))

  # Simulation of factor x
  x <- esgtoolkit::simdiff(
    n = n,
    horizon = horizon,
    frequency = freq,
    model = "OU",
    x0 = 0,
    theta1 = 0,
    theta2 = a_opt,
    theta3 = sigma_opt,
    eps = eps[[1]]
  )
  
  # Simulation of factor y
  y <- esgtoolkit::simdiff(
    n = n,
    horizon = horizon,
    frequency = freq,
    model = "OU",
    x0 = 0,
    theta1 = 0,
    theta2 = b_opt,
    theta3 = eta_opt,
    eps = eps[[2]]
  )
  
  # Instantaneous forward rates, with spline interpolation
  fwdrates_ <- esgtoolkit::esgfwdrates(
    n = n,
    horizon = horizon,
    out.frequency = freq,
    in.maturities = u,
    in.zerorates = txZC,
    method = "fmm"
  )
  fwdrates <- window(fwdrates_, end = horizon)
  
  # phi
  param.phi_ <-
    0.5 * (sigma_opt ^ 2) * ((1 - exp(-a_opt * t_out)) / a_opt) ^ 2
  param.phi_ <-
    param.phi_ + 0.5 * (eta_opt ^ 2) * ((1 - exp(-b_opt * t_out)) / b_opt) ^ 2
  param.phi_ <-
    param.phi_ + (rho_opt * (sigma_opt * eta_opt) / (a_opt * b_opt)) * (1 - exp(-a_opt * t_out)) * (1 - exp(-b_opt * t_out))
  param.phi <- ts(replicate(n, param.phi_),
                  start = start(x),
                  deltat = deltat(x))
  phi <- fwdrates + param.phi
  colnames(phi) <- c(paste0("Series ", 1:n))
  
  # short rate
  r_G2 <- x + y + phi
  colnames(r_G2) <- c(paste0("Series ", 1:n))
  
  # discount bonds
  P <- ts(t(sapply(t0:T, function(mat)
    discount_bond(
      t0 = t0,
      T = mat,
      p = p,
      maturities = u,
      sigma = sigma_opt,
      eta = eta_opt,
      rho = rho_opt,
      a = a_opt,
      b = b_opt,
      x = x,
      y = y
    )$discount_bonds)),
    frequency = 1)
  
  R <- ts(t(sapply(t0:T, function(mat)
    discount_bond(
      t0 = t0,
      T = mat,
      p = p,
      maturities = u,
      sigma = sigma_opt,
      eta = eta_opt,
      rho = rho_opt,
      a = a_opt,
      b = b_opt,
      x = x,
      y = y
    )$discount_rates)),
    frequency = 1)
  
  #  Stochastic volatility  simulation
  sim.vol <- esgtoolkit::simdiff(
    n =  n,
    horizon =  horizon,
    frequency =  freq,
    model = "CIR",
    x0 =  V0,
    theta1 =  kappa * theta,
    theta2 =  kappa,
    theta3 =  volvol,
    eps =  eps[[3]]
  )
  
  # Risky asset's prices simulation
  S <- esgtoolkit::simdiff(
    n = n,
    horizon = horizon,
    frequency = freq,
    model = "GBM",
    x0 = S0,
    theta1 = r_G2,
    theta2 = sqrt(sim.vol),
    eps = eps[[4]]
  )
  
  initial_call_payoffs <- (S - K) * (S > K)
  colnames(initial_call_payoffs) <- paste0("Series ", 1:n)
  call_payoffs <-
    drop(tail((S - K) * (S > K), 1)) # for maturity = horizon
  call_prices <- rep(0, length(time(r_G2)))
  i <- 1
  for (future_date in rev(time(r_G2)))
  {
    r_G2_ <- window(r_G2, end = future_date)
    Df <- sapply(1:n, function(j) exp(-simpson_integral_cpp(x = time(r_G2_),
                                                            y = r_G2_[, j])))
    call_prices[i] <- crossprod(Df, call_payoffs) / n
    i <- i + 1
  }
  names(call_prices) <- rev(time(r_G2))
  
  # # initial value for the risky asset
  # S0 <- 100
  # # volatility mean-reversion speed
  # kappa <- 0.003
  # # volatility of volatility
  # volvol <- 0.009
  # # Correlation between stoch. vol and risky asset's prices
  # rho <- -0.5 # Correlation between stoch. vol (eps[[3]]) and risky asset's prices (eps[[4]])
  # # Initial variance
  # V0 <- 0.04
  # # long-term variance
  # theta <- 0.04
  return(
    list(
      initial_discount_bonds = p,
      maturities = u,
      initial_zero_rates = txZC,
      short_rates = r_G2,
      asset_prices = S,
      call_prices = call_prices,
      call_heston = NMOF::callHestoncf(
          S = S0,
          X = K,
          r = txZC[which(u == horizon)],
          tau = horizon,
          q = 0,
          v0 = V0,
          vT = theta,
          rho = rho,
          k = kappa,
          sigma = volvol
        ),
      discount_bond_prices = P,
      discount_rates = R
    )
  )
}




# functions -------------
factor_1_exp_x <- function(t0, T, maturities, x)
{
  stopifnot(t0 <= T)
  stopifnot(T <= max(maturities))
  T_t0 <- T - t0
  return((1 - exp(-x * T_t0)) / x)
}

# V(t0, T)
factorV <- function(t0 = t0,
                    T = T,
                    maturities = maturities,
                    sigma = sigma_opt,
                    eta = eta_opt,
                    rho = rho_opt,
                    a = a_opt,
                    b = b_opt) {
  T_t0 <- T - t0
  exp_a_T_t0 <- exp(-a * T_t0)
  exp_b_T_t0 <- exp(-b * T_t0)
  exp_ab_T_t0 <- exp(-(a + b) * T_t0)
  sigma_a <- sigma / a
  eta_b <- eta / b
  res <-
    (sigma_a ** 2) * (T_t0 + (2 * exp_a_T_t0 - 0.5 * (exp_a_T_t0 ** 2) - 1.5) /
                        a)
  res <-
    res + (eta_b ** 2) * (T_t0 + (2 * exp_b_T_t0 - 0.5 * (exp_b_T_t0 ** 2) - 1.5) /
                            b)
  term <-
    T_t0 - factor_1_exp_x(t0, T, maturities, a) - factor_1_exp_x(t0, T, maturities, b) + factor_1_exp_x(t0, T, maturities, a + b)
  term <- term * (2 * rho * sigma_a * eta_b)
  return(res + term)
}

# P(t0, T)
discount_bond <-
  function(t0,
           T,
           p,
           maturities,
           sigma,
           eta,
           rho,
           a,
           b,
           x,
           y)
  {
    # stopifnot(t0 <= T)
    # stopifnot(T <= max(maturities))
    idx_t0 <- match(t0, maturities)
    idx_T <- match(T, maturities)
    p_up <- p[idx_T]
    p_down <- p[idx_t0]
    x_ <- x[idx_t0, ]
    y_ <- y[idx_t0, ]
    term3 <- 0.5 * factorV(t0, T, maturities,
                           sigma, eta, rho, a, b)
    term4 <- 0.5 * factorV(0, T, maturities,
                           sigma, eta, rho, a, b)
    term5 <- 0.5 * factorV(0, t0, maturities,
                           sigma, eta, rho, a, b)
    term6 <- factor_1_exp_x(t0, T, maturities, a)
    term7 <- factor_1_exp_x(t0, T, maturities, b)
    A <- term3 - term4 + term5 - term6 * x_ - term7 * y_
    res <- list()
    res$discount_bonds <- p_up / p_down * exp(A)
    res$discount_rates <-
      -log(res$discount_bonds) / max((T - t0), .Machine$double.eps)
    return(res)
  }
discount_bond <- memoise::memoize(discount_bond)