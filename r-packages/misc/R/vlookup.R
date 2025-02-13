
#' VLOOKUP
#' 
#' A simple implementation similar to the VLOOKUP function in Excel.
#' 
#' @param this The value to look up
#' @param df A data frame
#' @param key The column to look up
#' @param value The column to return
#' @return The value in the `value` column corresponding to the `key` column
#' @export
#' @examples
#' 
#' df <- data.frame(key = c("a", "b", "c"), value = c(1, 2, 3))
#' print(misc::vlookup("b", df, "key", "value"))
#'  
vlookup <- function(this, df, key, value) {
  m <- match(this, df[[key]])
  return(df[[value]][m])
}