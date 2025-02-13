#' Check if a number is a whole number
#' 
#' @param x A number
#' @param tol A tolerance level
#' @return A logical value
#' @export
#' @examples
#' 
#' is_wholenumber(1)
#' is_wholenumber(1.1)
#' is_wholenumber(1L)
#' 
is_wholenumber <- function(x, tol = .Machine$double.eps^0.5)
{
  all(abs(x - round(as.numeric(x))) < tol)
}


