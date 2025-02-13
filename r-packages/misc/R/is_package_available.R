#' Check if a package is available
#' 
#' @param pkg_name A package name
#' @return A logical value
#' @export
#' @examples
#' 
#' misc::is_package_available("dplyr")
#' 
is_package_available <- function(pkg_name) {
  if (!is.null(pkg_name))
  {
    return(pkg_name %in% rownames(utils::installed.packages()))
  } else {
    return(TRUE) 
  }
}