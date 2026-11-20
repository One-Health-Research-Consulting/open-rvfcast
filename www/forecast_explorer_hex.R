####
## Forecast explorer: forward forecasts by H3 hex (the model's native units), with each hex's
## forecast quality and outbreak history. Shared code: forecast_explorer_common.R
####

source("forecast_explorer_common.R")

## Built by build_forecast_app_data() (R/build_app_components.R)
forecast_explorer_app("forecast_app_hex.rds")
