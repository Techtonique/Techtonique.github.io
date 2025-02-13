.onLoad <- function(libname, pkgname) {
    #.BASE_URL <<- "http://127.0.0.1:8000"
    .BASE_URL <<- "https://www.techtonique.net"
}

# In R/another_file.R
use_global_var <- function() {
    return(.BASE_URL)
}
