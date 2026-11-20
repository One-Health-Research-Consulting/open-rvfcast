####
## Forecast explorer: forward forecasts up-scaled to aggregated ADM2 regions (probability of at
## least one outbreak in the region, Gaussian copula), with each region's forecast quality and
## outbreak history. Shared code: forecast_explorer_common.R
####

source("forecast_explorer_common.R")

## Built by build_forecast_app_data() (R/build_app_components.R)
forecast_explorer_app("forecast_app_regions.rds")
