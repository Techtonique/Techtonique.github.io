
#' check if list already contains an element
#' @export
check_list_contains <- function(list_elts, new_elt)
{
  for (elt in list_elts)
  {
    cond1 <- try(base::suppressWarnings(all(as.numeric(elt) == as.numeric(new_elt))),
                 silent = TRUE)
    if (inherits(cond1, "try-error"))
    {
      cond1 <- FALSE
    }
    cond2 <- try(base::suppressWarnings(all(start(elt) == start(new_elt))),
                 silent = TRUE)
    if (inherits(cond2, "try-error"))
    {
      cond2 <- FALSE
    }
    cond3 <- try(base::suppressWarnings(frequency(elt) == frequency(new_elt)),
                 silent = TRUE)
    if (inherits(cond3, "try-error"))
    {
      cond3 <- FALSE
    }
    if (cond1 && cond2 && cond3)
    {
      return(TRUE)
    }
  }
  return(FALSE)
}
check_list_contains <-  compiler::cmpfun(check_list_contains)

# debug print -----
#' @export
debug_print <- function(x) {
  cat("\n")
  print(paste0(deparse(substitute(x)), "'s value:"))
  print(x)
  cat("\n")
}


#' create correlation matrix
#' @export
create_correlation_matrix <- function(cor_values) {
  n <- length(cor_values)
  cor_matrix <- diag(n)  # Initialize correlation matrix as diagonal

  for (i in 1:n) {
    for (j in (i+1):(n-1)) {
      if (j == (n+1)) {
        break
      }
      cor_matrix[i, j] <- cor_values[i]  # Upper triangular part
      cor_matrix[j, i] <- cor_values[i]  # Lower triangular part
    }
  }

  return(cor_matrix)
}


#' remove NAs by linear interpolation
#' @export
removenas <- function(y) {
  # Check if input is a time series object
  if (!is.ts(y)) {
    stop("Input must be a time series object (ts).")
  }

  # Check for NA values
  if (!anyNA(y)) {
    return(y)
  }

  # Extract time and values
  time <- time(y)
  values <- as.numeric(y)

  # Perform linear interpolation to replace NAs
  interpolated_values <- approx(x = time[!is.na(values)], y = values[!is.na(values)], xout = time)$y

  # Create a new time series object
  new_ts <- ts(interpolated_values, start = start(y), frequency = frequency(y))

  return(new_ts)
}

#' split time series sequentially
#' @export
splitts <- function(y, split_prob = 0.5, return_indices = FALSE, ...)
{
  n_y <- base::ifelse(test = is.null(dim(y)),
                      yes = length(y),
                      no = dim(y)[1])

  index_train <- 1:floor(split_prob*n_y)
  if (return_indices)
    return(index_train)

  start_y <- stats::start(y)
  frequency_y <- stats::frequency(y)

  if(is.null(ncol(y))) # univariate case
  {
    training <- ts(y[index_train],
                   start = start_y,
                   frequency = frequency_y)
    start_testing <- tsp(training )[2] + 1 / frequency_y
    return(list(training = training,
                testing = ts(y[-index_train],
                             start = start_testing,
                             frequency = frequency_y)))
  } else { # multivariate case
    training <- ts(y[index_train, ],
                   start = start_y,
                   frequency = frequency_y)
    start_testing <- tsp(training)[2] + 1 / frequency_y
    return(list(training = training,
                testing = ts(y[-index_train, ],
                             start = start_testing,
                             frequency = frequency_y)))
  }
}
splitts  <- compiler::cmpfun(splitts)
