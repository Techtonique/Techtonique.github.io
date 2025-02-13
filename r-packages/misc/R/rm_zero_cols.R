#' Removing columns containing only zeros
#' 
#' @param X A matrix or data frame
#' @return A matrix or data frame
#' @export
#' @examples
#' 
#' X <- matrix(c(1, 0, 3, 0, 5, 0, 0, 0), nrow = 2)
#' print(misc::rm_zero_cols(X))
#' 
rm_zero_cols <- function(X)
{
  X[, sapply(1:ncol(X),
             function(j) !all(X[,j] %in% 0))]
}
