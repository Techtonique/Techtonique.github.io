#' Sort data frame
#' 
#' @param df data frame
#' @param by column to sort by
#' @param decreasing logical. Should sorting be decreasing?
#' @return A sorted data frame
#' @export
#' @examples
#' 
#' df <- data.frame(a = c(2, 4, 3), b = c(3, 5, 1))
#' misc::sort_df(df, "a")
#' misc::sort_df(df, "b", decreasing = TRUE)
#' 
sort_df <- function(df, by, decreasing = FALSE) {
  return(df[order(df[[by]], decreasing = decreasing), ])
}