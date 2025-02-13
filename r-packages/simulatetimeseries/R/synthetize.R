# Simulate using bootstrap -----


#' Simulate using bootstrap
#'
#' @export
#'
rbootstrap <- function(x,
                       n = length(x),
                       p = 1,
                       seed = 123) {
  if (p <= 1)
  {
    set.seed(seed)
    return(sample(x, size = n, replace = TRUE))
  } else {
    return(sapply(1:p,
                  function(i) {
                    set.seed(seed + i - 1)
                    sample(x, size = n, replace = TRUE)
                  }))
  }
}

# Simulate Gaussian kernel density -----

#' Simulate Gaussian kernel density
#'
#' @export
#'
rgaussiandens <- function(x,
                          n = length(x),
                          p = 1,
                          seed = 123,
                          method = c("antithetic",
                                     "traditional")) {
  z <- try(stats::density(x, bw = "sj", kernel = "gaussian"),
           silent = TRUE)

  if (inherits(z, "try-error"))
    z <- stats::density(x, kernel = "gaussian")

  width <- z$bw # Kernel width

  method <- match.arg(method)

  rkernel <- function(n, seed) {
    set.seed(seed)
    if (!identical(method, "antithetic"))
    {
      return(stats::rnorm(n, sd = width))
    } else {
      half_n <- n %/% 2
      eps <- stats::rnorm(half_n, sd = width)
      if (2 * length(eps) < n)
      {
        return(c(eps,-eps, stats::rnorm(1, sd = width)))
      }
      return(sample(c(eps,-eps),
                    replace = FALSE))
    }
  }  # Kernel sampler

  if (p <= 1)
  {
    set.seed(seed)
    return(sample(x, n, replace = TRUE) + rkernel(n, seed))    # Here's the entire algorithm
  } else {
    return(sapply(1:p,
                  function(i) {
                    set.seed(seed + i - 1)
                    sample(x, n, replace = TRUE) + rkernel(n, seed + i - 1)
                  }))
  }
}

# Simulate using surrogate -----

#' Simulate using surrogate data
#'
#' @export
#'
rsurrogate <- function(x,
                       n = length(x),
                       p = 1,
                       seed = 123) {
  if (n > length(x))
  {
    stop("For surrogates, must have number of predictions < number of training observations")
  }
  if (p <= 1)
  {
    set.seed(seed)
    res <- tseries::surrogate(x, ns = p,
                              fft = TRUE)
    return(res[seq_len(n), ])
  } else {
    res <- sapply(1:p,
                  function(i) {
                    set.seed(seed + i - 1)
                    tseries::surrogate(x, ns = p,
                                       fft = TRUE)
                  })
    return(res[seq_len(n), ])
  }
}
