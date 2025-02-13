

#' Obtain forecasts from a csv file
#'
#' @param path_to_file a string; path to csv file containing time series data (examples:
#' https://github.com/Techtonique/datasets/tree/main/time_series/univariate)
#' @param base_model a string; Forecasting method to use (default is "RidgeCV"); for now scikit-learn model names. 
#' @param n_hidden_features an integer; Number of hidden features for the model (default is 5).
#' @param lags an integer; Number of lags to use in the model (default is 25).
#' @param type_pi a string; Type of prediction interval to use (default is 'gaussian').
#' @param replications an integer; Number of replications for certain methods (default is None).
#' @param h an integer; Forecast horizon (default is 10).
#' 
#' @examples 
#' 
#' @export
#'
get_forecast <- function(path_to_file,
    base_model="RidgeCV",
    n_hidden_features=5L,
    lags=25L,
    type_pi="gaussian",
    replications=NULL,
    h=10L)
{
  token <- base::readline(prompt = "Please enter your token (from https://www.techtonique.net/token): ")

  if (grepl("^(http|https)://", path_to_file)) {
    temp_file <- tempfile()    
    httr::GET(path_to_file, httr::write_disk(temp_file, 
    overwrite = TRUE))
    if (!file.exists(temp_file)) {
      stop("Failed to download file")
    }
    path_to_file <- temp_file    
  }

  files = list(
    file = httr::upload_file(path_to_file)
  )

headers = c(
  Authorization = paste0("Bearer ", token)
)

params = list(
  base_model = as.character(base_model),
  n_hidden_features = as.character(n_hidden_features),
  lags = as.character(lags),
  type_pi = as.character(type_pi),
  replications = as.character(replications),
  h = as.character(h)
)

res <- httr::POST(url = paste0(.BASE_URL, "/forecasting"), 
httr::add_headers(.headers=headers), 
query = params, body = files, encode = "multipart")

# Parse the response directly to a list
parsed_response <- jsonlite::fromJSON(httr::content(res, "text", encoding = "UTF-8"), 
simplifyDataFrame = TRUE, simplifyVector = TRUE)

# Convert to numeric vectors directly
mean_vector <- jsonlite::fromJSON(parsed_response$mean)
lower_vector <- jsonlite::fromJSON(parsed_response$lower)
upper_vector <- jsonlite::fromJSON(parsed_response$upper)

# For sims, which are nested lists, you can directly convert them to a list of numeric vectors
if (!is.null(parsed_response$sims))
{
  sims_list <- lapply(parsed_response$sims, jsonlite::fromJSON)
  return(list(mean = mean_vector, 
  lower=lower_vector, 
  upper=upper_vector, 
  sims=t(sims_list[[1]])))
}
  

return(list(mean = mean_vector, 
  lower=lower_vector, 
  upper=upper_vector))
}
