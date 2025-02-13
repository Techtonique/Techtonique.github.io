#' Winkler score for probabilistic forecasts
#' 
#' @param actual numeric vector of actual values
#' @param lower numeric vector of lower bounds
#' @param upper numeric vector of upper bounds
#' @param level numeric level of confidence
#' @param scale logical, if TRUE, the score is scaled by the range of the bounds
#' @return numeric score
#' @export
#' @examples
#' 
#' actual <- c(1, 2, 3, 4, 5)
#' lower <- c(0, 1, 2, 3, 4)
#' upper <- c(2, 3, 4, 5, 6)
#' winkler_score(actual, lower, upper)
#' winkler_score(actual, lower, upper, scale = TRUE)
#' winkler_score(actual, lower, upper, level = 99)
#' winkler_score(actual, lower, upper, level = 99, scale = TRUE)
#' 
winkler_score <- function(actual, lower, upper, level = 95, scale = FALSE) {
  alpha <- 1 - level / 100
  diff_bounds <- upper - lower
  score <-
    diff_bounds + (2 / alpha) * (pmax(lower - actual, 0) + pmax(actual - upper, 0))
  if (!scale)
  {
    return(mean(score))
  } else {
    return(mean(1 - score/diff_bounds))
  }
}