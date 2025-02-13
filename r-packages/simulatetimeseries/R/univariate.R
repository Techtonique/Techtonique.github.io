#' Simulate a univariate time series dataset 1
#'
#' @param n numerical, number of data points
#' @param trend string, "linear" or "quadratic"
#' @param seasonality string, "none" or "sinusoidal"
#' @param distribution string, "normal" and "student"
#' @param noise_sd numerical, standard deviation of noise
#' @param seed int, reproducibility seed
#'
#' @return a native time series object
#' @export
#'
#' @examples
#'
#' ts_data <-
#' simulate_time_series_1(
#'   n = 100L,
#'   trend = "quadratic",
#'   seasonality = "sinusoidal",
#'   noise_sd = 2500,
#'   distribution = "normal"
#' )
#' plot(ts_data, type = "l", main = "Simulated Time Series")
#'
simulate_time_series_1 <- function(n,
                                   trend = c("linear",
                                             "quadratic"),
                                   seasonality = c("none",
                                                   "sinusoidal"),
                                   distribution = c("normal",
                                                    "student"),
                                   noise_sd = 10,
                                   seed = 123) {
  trend <- match.arg(trend)
  seasonality <- match.arg(seasonality)
  distribution <- match.arg(distribution)
  # Generate time index
  times <- 1:n
  # Generate trend component
  if (!is.null(trend)) {
    if (trend == "linear") {
      trend_component <- seq(1, n, length.out = n)
    } else if (trend == "quadratic") {
      trend_component <- seq(1, n, length.out = n) ^ 2
    }
  } else {
    trend_component <- rep(0, n)
  }

  # Generate seasonal component
  if (!identical(seasonality, "none")) {
    if (identical(seasonality, "sinusoidal")) {
      if (trend == "linear")
      {
        seasonal_component <- sin(2 * pi * times / 12)
      } else {
        seasonal_component <- sin(2 * pi * (times ** 2) / 12)
      }
    } else {
      stop("Invalid seasonality type. Supported types: sinusoidal.")
    }
  } else {
    seasonal_component <- rep(0, n)
  }

  # Generate noise component
  if (distribution == "normal") {
    noise <- rnorm(n, mean = 0, sd = noise_sd)
  } else if (distribution == "student") {
    noise <- noise_sd * rt(n = 100, df = 3)
  }

  # Combine components to generate time series
  time_series <- trend_component + seasonal_component + noise

  return(ts(time_series))
}

#' Simulate a univariate time series dataset 2
#'
#' @param n numerical, number of data points
#' @param trend string, "linear" or "sinusoidal"
#' @param seasonality string, "none" or "sinusoidal"
#' @param noise_sd numerical, standard deviation of noise
#' @param ar autoregressive order
#' @param ma moving average order
#' @param seed int, reproducibility seed
#'
#' @return a native time series object
#' @export
#'
#' @examples
#'
#' ts_data <-
#' simulate_time_series_2(
#'   n = 100L,
#'   trend = "sinusoidal",
#'   seasonality = TRUE,
#'   noise_sd = runif(n = 1, min = 20, max=50)
#' )
#' plot(ts_data, type = "l", main = "Simulated Time Series")
#'
simulate_time_series_2 <- function(n,
                                   trend = c("linear",
                                             "sinusoidal"),
                                   seasonality = FALSE,
                                   noise_sd = 0.1,
                                   ar = 0,
                                   ma = 0,
                                   seed = 123) {

  trend <- match.arg(trend)
  # Generate base series
  series <- rnorm(n, mean = 0, sd = noise_sd)

  # Add trend
  times <- (1:n)
  if (trend == "linear") {
    series <- series + times * -2.9  # Adjust slope as desired
  } else if (trend == "sinusoidal") {
    series <- series + sin(2 * pi * times**2 / n)
  }

  # Add seasonality
  if (seasonality) {
    seasonal <- stats::rnorm(n, mean = 0, sd = noise_sd / 2)
    seasonal <-
      seasonal[seq(1, n, by = length(seasonal))]  # Repeat for length of series
    series <- series + seasonal
  }

  # Introduce autoregression (AR)
  if (ar > 0) {
    ar.coef <- stats::arima.sim(model = list(ar = ar),
                                n = n,
                                innov = noise_sd)
    series <- series + ar.coef * series[-1]  # Shift for causality
  }

  # Introduce moving average (MA)
  if (ma > 0) {
    ma.coef <- stats::arima.sim(model = list(ma = ma),
                                n = n,
                                innov = noise_sd)
    series <- series + ma.coef * rnorm(n, mean = 0, sd = noise_sd)
  }
  return(series)
}



#' Simulate a univariate time series dataset 3
#'
#' @param n numerical, number of data points
#' @param seed int, reproducibility seed
#'
#' @return a native time series object
#' @export
#'
#' @examples
#'
#' print(simulate_time_series_3(10))
#'
simulate_time_series_3 <- function(n=100,
                                   seed = 123)
{
  xi <- rnorm(n, mean = 0, sd = sqrt(0.01))
  eps <- rep(0, 100)
  for (i in 2:100) {
    eps[i] <- 0.99 * eps[i - 1] + xi[i]
  }
  trend <- seq_along(100)
  season_term <- 2 * pi * trend / 180
  return(ts(cos(season_term) + sin(season_term) + 0.01 * trend + eps))
}





#' Simulate a univariate time series dataset 4
#'
#' @param n numerical, number of data points
#' @param psi 1st parameter for innovation variance (in [0, 1])
#' @param theta 2nd parameter for innovation variance (in [0, 1])
#' @param seed int, reproducibility seed
#'
#' @return a native time series object
#' @export
#'
#' @examples
#'
#' plot(simulate_time_series_4())
#'
simulate_time_series_4 <- function(n = 600,
                                   psi = 0.1,
                                   theta = 0.1,
                                   seed = 123) {
  set.seed(seed)
  s <- 10
  innov_scale <- sqrt(s * (1 - psi**2) / (1 + 2 * psi * theta + theta**2))
  X <- matrix(runif(6 * n), ncol = 6, nrow = n)
  colnames(X) <- paste0("X", 1:6)
  epsilon <- arima.sim(n = n, model = list(ar = psi, ma = theta), sd = innov_scale)
  mu <- 10 * sin(pi * X[,1] * X[,2]) + 20 * (X[,3] - 0.5)**2 + 10 * X[,4] + 5 * X[,5]
  return(mu + epsilon)
}


#' Get data 1
#'
#' Data from Task Views + synthetic
#'
#' @param diffs return the differentiated series or not? (lag = 1)
#'
#' @return a list of time series objects
#' @export
#'
#' @examples
get_data_1 <- function(diffs = TRUE)
{
  pkg_names <- c("astsa",
                 "datasets",
                 "expsmooth",
                 "fma",
                 "forecast",
                 "fpp",
                 "fpp2",
                 "MASS",
                 "tswge")

  require("ahead")
  require("astsa")
  require("datasets")
  require("expsmooth")
  require("fma")
  require("forecast")
  require("fpp")
  require("fpp2")
  require("MASS")
  require("tswge")

  # real-world data
  suppressWarnings(utils::install.packages(pkg_names,
                                           repos="https://cran.r-project.com"))
  dataset_objs <- vector("list", 250L)
  for (pkg in pkg_names)
  {
    exported_objects <- ls(paste0("package:", pkg),
                           all.names = TRUE)
    if (identical(diffs, FALSE))
    {
      for (obj_name in exported_objects) {
        obj <- get(obj_name,
                   envir = asNamespace(pkg))
        if (is.ts(obj) && is.null(dim(obj))) { # univariate ts
          if (!check_list_contains(dataset_objs, obj)) # no duplicates
          {
            dataset_objs[[obj_name]] <-  removenas(obj)
          }
        }
      }
    } else {
      for (obj_name in exported_objects) {
        obj <- get(obj_name,
                   envir = asNamespace(pkg))
        if (is.ts(obj) && is.null(dim(obj))) { # univariate ts
          if (!check_list_contains(dataset_objs, obj)) # no duplicates
          {
            dataset_objs[[obj_name]] <-  diff(removenas(obj))
          }
        }
      }
    }
  }
  dataset_objs[sapply(dataset_objs, is.null)] <- NULL
  # synthetic data
  n_synthetic <- 250 - length(dataset_objs)
  synthetic <- vector("list", n_synthetic)
  if (identical(diffs, FALSE))
  {
    for (j in 1:n_synthetic)
    {
      set.seed(123+j*100)
      xi <- rnorm(100, mean = 0, sd = sqrt(0.01))
      eps <- rep(0, 100)
      for (i in 2:100) {
        eps[i] <- 0.99 * eps[i - 1] + xi[i]
      }
      trend <- seq_along(100)
      season_term <- 2 * pi * trend / 180
      synthetic[[j]] <- ts(cos(season_term) + sin(season_term) + 0.01 * trend + eps)
    }
  } else {
    for (j in 1:n_synthetic)
    {
      set.seed(123+j*100)
      xi <- rnorm(100, mean = 0, sd = sqrt(0.01))
      eps <- rep(0, 100)
      for (i in 2:100) {
        eps[i] <- 0.99 * eps[i - 1] + xi[i]
      }
      trend <- seq_along(100)
      season_term <- 2 * pi * trend / 180
      synthetic[[j]] <- diff(ts(cos(season_term) + sin(season_term) + 0.01 * trend + eps))
    }
  }
  names(synthetic) <- paste0("synth", 1:n_synthetic)

  return(c(dataset_objs, synthetic))
}
