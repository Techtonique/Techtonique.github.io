#' Debug print
#' 
#' @param x An object to be printed
#' @export
#' @examples
#' 
#' misc::debug_print(1:10)
#' misc::debug_print("Hello, world!")
#' 
debug_print <- function(x) {
  call_info <- sys.calls()
  frame_info <- sys.frames()
  
  # Get the most recent call (excluding debug_print itself)
  call_str <- deparse(call_info[[length(call_info) - 1]])
  
  # Extract line number if available
  line_number <- if (!is.null(frame_info[[length(frame_info) - 1]]$srcref)) {
    frame_info[[length(frame_info) - 1]]$srcref[[1]]
  } else {
    "unknown"
  }
  
  cat("\n")
  cat(sprintf("Line %s: %s's value:\n", line_number, deparse(substitute(x))))
  print(x)
  cat("\n")
}

#debug_print <- function(x) {
#  cat("\n")
#  print(paste0(deparse(substitute(x)), "'s value:"))
#  print(x)
#  cat("\n")
#}