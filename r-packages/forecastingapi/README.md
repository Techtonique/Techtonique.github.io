
# forecastingAPI

High level R functions for interacting with [Techtonique forecasting API](https://www.techtonique.net/docs)

## Installation

```R
remotes::install_github("Techtonique/forecastingAPI_r")
```

## Usage 

- File examples: [https://github.com/Techtonique/datasets/tree/main/time_series](https://github.com/Techtonique/datasets/tree/main/time_series)
- Get a token: [https://www.techtonique.net/token](https://www.techtonique.net/token)


```R
library(forecastingapi)

# Example usage 1
path_to_file <- "/Users/t/Documents/datasets/time_series/univariate/AirPassengers.csv"
forecastingapi::get_forecast(path_to_file)

# Example usage 2
forecastingapi::get_forecast(path_to_file, type_pi='scp2-kde', h=5L, replications=10L)
```

## License

MIT 

