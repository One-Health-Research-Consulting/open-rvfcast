library(ecmwfr)
library(tidyverse)
library(lubridate)
user_id <- "173186"# user ID (for authentication), see ecmwf
# https://github.com/bluegreen-labs/ecmwfr#file-based-keychains
# ^ linux need to unlock keyring

# Copernicus Climate Change Service #

#TODO convert to markdown
#TODO postprocessing
#TODO key handling in noninteractive mode
#TODO error metrics
#TODO diagnostic report -- bounding box by region - size and time to download
#TODO other models besides ECMWF?

# example papers
# https://link.springer.com/article/10.1007/s00382-021-05681-4

# Seasonal forecast monthly statistics on single levels -------------------------------------------------------------------
## Overview
## https://cds.climate.copernicus.eu/cdsapp#!/dataset/seasonal-monthly-single-levels?tab=overview

### Data type = Gridded
### Projection = 	Regular latitude-longitude grid
### Coverage = Global
### Resolution = 1x1 deg
### Temporal coverage = 1993 to 2016 (hindcasts); 2017 to present (forecasts)
### Temporal resolution = Monthly
### File format = GRIB
### Update frequency = Real-time forecasts are released once per month on the 13th at 12UTC

### 2m dewpoint temperature units = K
### 2m temperature units = K
### Total precipitation = m / s

## Download
## https://cds.climate.copernicus.eu/cdsapp#!/dataset/10.24381/cds.68dd14c3?tab=form
## instructions: https://confluence.ecmwf.int/display/CKB/How+to+use+the+CDS+interactive+forms+for+seasonal+forecast+datasets

### Originating Center = Select the name of the institution the forecasting system of your interest originates from.
### System = Select the version of the forecasting system. This is a numeric label and the available values are different for the different "originating centres". Note that for a given start date you could find more than one system available for a single "originating centre".
###          Please note that you must pair up your forecasts with the relevant hindcasts by using the same "system" for both of them.
### Product type = Select Ensemble mean or Hindcast climate mean, can also select individual members values
###                monthly statistics: monthly average, minimum, maximum and standard deviation are available for all initialization dates.
###                Additionally, for real-time forecast initialization dates you will find the ingredients to calculate the forecast monthly anomalies: the forecast ensemble mean and the hindcast climate mean.
### Year = Select the year of the initialization date of the model run(s) you are interested in.
###         Note that there could be differences in the options available depending on your selection of forecast or hindcast years.
###         Please note that you must use the hindcast data in order to make a meaningful use of the forecast data. And remember you must pair forecasts and hindcasts up by using the same "system" for both of them.
###        *??? Why must you use the hindcast data in order to make a meaningful use of the forecast data
###         Maybe they are referring to the modelers? as explained here https://confluence.ecmwf.int/display/CKB/Seasonal+forecasts+and+the+Copernicus+Climate+Change+Service
###         Models must be calibrated on performance of past forecasts relevant to actual data
###         Also important for interpretation: Information on forecast skill is important to avoid overconfident decision making.
### Month = Select the month of the initialization date of the model run(s) you are interested in.
###       Note that in the current setup all monthly products are encoded using as nominal start date the 1st of each month, regardless of the real initialization date
### Lead time = Select the lead time(s) you are interested in. This is the time, in months, from the initialization date.
###            Note that the convention used for labelling the data implies that leadtime_month=1 is the first complete calendar month after the initialization date. In the current setup of all datasets that means that for a forecast initialised on the 1st of November, leadtime_month=1 is November.

### Note on fixed versus on the fly forecasts
### fixed hindcasts. Some systems are designed so their expected lifetime will be around 4-5 years. Once the system has been designed and tested, ensemble hindcasts for the whole reference period are run. The advantage is that this reference dataset is available well in advance of real-time forecasts being issued, and its properties (biases, skill) can be quantified once for repeated use. As this is a very expensive exercise, it cannot be repeated too often and thus the system remains fixed for a long period of time.
### on-the-fly hindcasts. Some systems prioritise more frequent upgrades, which means that the hindcast sets have to be run more frequently. To achieve this in practice, the full hindcast set is run every time a new real-time forecast is produced, slightly in advance (a few weeks) of the real-time forecast and using exactly the same version of the forecasting system. This also offers the advantage of balancing the requirement for c
### ie on the fly hindcasts are updated when there is a model change. fixed hindcasts are static.
### all sources are fixed except UKMO

## Documentation
## https://cds.climate.copernicus.eu/cdsapp#!/dataset/10.24381/cds.68dd14c3?tab=doc

## Summary of available data
## https://confluence.ecmwf.int/display/CKB/Summary+of+available+data
### Real time forecasts with start date Nov 2022 to present available from several sources
### Goes back to Sept 2017 with differing coverage by model, but every date should have some coverage
### Hindcasts are for some reason shown here by month (assume coverage for all years?)

## Recommendations for efficiency
### https://confluence.ecmwf.int/display/CKB/Recommendations+and+efficiency+tips+for+C3S+seasonal+forecast+datasets
### The C3S seasonal forecast dataset is currently based on GRIB files archived at ECMWF's MARS archive. This implies, in some situations, accessing data not available online (on disk) but archived in the tape library.
### Guidance for batch downloads is to download same month over multiple years (rather than all months per year)
### ^ this may just apply to daily

# Agrometeorological indicators from 1979 to present --------------------------------------------------------
# Overview
## https://cds.climate.copernicus.eu/cdsapp#!/dataset/sis-agrometeorological-indicators?tab=overview

### Data type = Gridded
### Projection = 	Regular latitude-longitude grid
### Coverage = Global
### Horizontal Resolution = 0.1x0.1 deg
### Temporal coverage = 1979 to present
### Temporal resolution = daily
### File format = NetCDF-4
### Update frequency = Monthly

### 2m dewpoint temperature units = K
### 2m temperature units = K
### DOES NOT HAVE Total precipitation (but I think they can be converted based on time and area, m/s to mm3)
### Precipitation flux (total volume of liquid water (mm3) precipitated over the period 00h-24h local time per unit of area (mm2), per day.)
### relative humidity %

# Define seasonal forecast functions --------------------------------------------------------
# API request can be generated with download link above

download_seasonal_forecasts <- function(user_id,
                                        system,
                                        year,
                                        month,
                                        center = c("ecmwf"),
                                        variable = c("2m_dewpoint_temperature", "2m_temperature", "total_precipitation"), # others can be added https://cds.climate.copernicus.eu/cdsapp#!/dataset/10.24381/cds.68dd14c3?tab=form
                                        product_type = c("monthly_mean", "monthly_maximum", "monthly_minimum", "monthly_standard_deviation",
                                                         "ensemble_mean", "hindcast_climate_mean"), # ensemble and hindcast are for calculating monthly anamolies, only available from 2017-present
                                        leadtime_month = c("1", "2", "3", "4", "5", "6"),
                                        spatial_bound = c(90, -180, -90, 180), # N, W, S, E
                                        format = c("grib"),
                                        filename,
                                        dir){

  center <- match.arg(center)
  format <- match.arg(format)

  assertthat::assert_that(all(spatial_bound[c(1,3)] <= 90, spatial_bound[c(1,3)] >= -90))
  assertthat::assert_that(all(spatial_bound[c(2,4)] <= 180 & spatial_bound[c(1,3)] >= -180))

  assertthat::assert_that(!missing(user_id), !missing(system), !missing(year), !missing(month))
  if(missing(dir)) dir <- here::here("downloads"); fs::dir_create(dir)
  if(missing(filename)) filename <- paste0("cds_download.", format)

  message(paste("downloading data to", paste0(dir, "/", filename)))

  request <- list(
    originating_centre = center,
    system = system,
    variable = variable,
    product_type = product_type,
    year = year,
    month = month,
    leadtime_month = leadtime_month,
    area = spatial_bound,
    format = format,
    dataset_short_name = "seasonal-monthly-single-levels",
    target = filename
  )

  time_elapsed <- system.time(
    wf_request(user = user_id, request = request, transfer = TRUE, path = dir)
  )

  print(time_elapsed)
  return(list(time_elapsed = time_elapsed, filename = filename, dir = dir, file.info = file.info))
}

safe_download_seasonal_forecasts <- safely(download_seasonal_forecasts)


# Map out and download Seasonal Forecast  -------------------------------------------------------------------
# https://confluence.ecmwf.int/display/CKB/Summary+of+available+data

# system 4 covers just sept/oct 2017
# sys4 <- list(system = 4, year = 2017,month = 9:10)
sys4 <- tibble(system = 4, year = list(2017), month = list(9:10))

# System 51 covers nov 2022 through present
# Real-time forecasts are released once per month on the 13th at 12UTC
sys51_dates <- seq(ymd("2022-11-01"), Sys.Date(), by = "month")

current_time <- as.POSIXlt(Sys.time(), tz = "UTC")
current_year <- year(current_time)
current_month <- month(current_time)

update_date <- ymd_hms(paste0(current_year,"-", current_month, "-13 12:00:00"))
if(current_time < update_date) sys51_dates <- sys51_dates[-length(sys51_dates)]

sys51_years <- unique(year(sys51_dates))
if(length(sys51_years) > 5){
  sys51_years <- split(sys51_years, ceiling(sys51_years/5))
}else{
  sys51_years <- list(sys51_years)
}
sys51 <- tibble(system = 51,
                year = sys51_years,
                month = list(unique(month(sys51_dates))))

# System 5 covers everything else. Split into batches of 5 for download limites
sys5_years <- 1993:2022
sys5 <- tibble(system = 5, year = split(sys5_years, ceiling(sys5_years/ 5)), month = list(1:12))

# all systems together
seasonal_forecast_parameters <- bind_rows(sys4, sys51, sys5)

# download
seasonal_forecast_download <- pmap(seasonal_forecast_parameters, function(system, year, month){
  dir <- here::here("downloads")
  filename <- paste("ecmwf", "seasonal_forecast", "rsa", system, min(year), "to", max(year), sep = "_")
  filename <- paste0(filename, ".grib")
  if(file.exists(paste0(dir, "/", filename))) return()
  safe_download_seasonal_forecasts(user_id = user_id,
                                   system = system,
                                   year = year,
                                   month = month,
                                   center = "ecmwf",
                                   variable = c("2m_dewpoint_temperature", "2m_temperature", "total_precipitation"), # others can be added https://cds.climate.copernicus.eu/cdsapp#!/dataset/10.24381/cds.68dd14c3?tab=form
                                   product_type = c("monthly_mean", "monthly_maximum", "monthly_minimum", "monthly_standard_deviation"),
                                   leadtime_month = c("1", "2", "3", "4", "5", "6"),
                                   spatial_bound = c(-21, 15, -35, 37), # N, W, S, E
                                   format = "grib",
                                   filename = filename,
                                   dir = dir)
})


# Define climate_data functions --------------------------------------------------------
# API request can be generated with download link above
download_agrometeo <- function(user_id,
                               year,
                               month,
                               day,
                               variable = c("2m_dewpoint_temperature", "2m_temperature", "precipitation_flux"), # others can be added https://cds.climate.copernicus.eu/cdsapp#!/dataset/10.24381/cds.68dd14c3?tab=form
                               statistic = "24_hour_mean",
                               spatial_bound = c(90, -180, -90, 180), # N, W, S, E
                               format = c("tgz"),
                               filename,
                               dir){

  format <- match.arg(format)

  assertthat::assert_that(all(spatial_bound[c(1,3)] <= 90, spatial_bound[c(1,3)] >= -90))
  assertthat::assert_that(all(spatial_bound[c(2,4)] <= 180 & spatial_bound[c(1,3)] >= -180))

  assertthat::assert_that(!missing(user_id), !missing(year), !missing(month), !missing(day))
  if(missing(dir)) dir <- here::here("downloads"); fs::dir_create(dir)
  if(missing(filename)) filename <- paste0("cds_download.tar.gz")

  message(paste("downloading data to", paste0(dir, "/", filename)))

  request <- list(
    variable = variable,
    year = year,
    month = month,
    day = day,
    area = spatial_bound,
    format = format,
    statistic = statistic,
    dataset_short_name = "sis-agrometeorological-indicators",
    target = filename
  )

  time_elapsed <- system.time(
    wf_request(user = user_id, request = request, transfer = TRUE, path = dir)
  )

  print(time_elapsed)
  return(list(time_elapsed = time_elapsed, filename = filename, dir = dir, file.info = file.info))
}

safe_download_agrometeo <- safely(download_agrometeo)

# Map out Agrometeorological indicators download -------------------------------------------------------------------
### Guidance for batch downloads is to download same month over multiple years (rather than all months per year)
### Indicators have to be separate
days <- sprintf("%02d", 1:30)
months <- sprintf("%02d", 1:12)
#years <- 1979:2023
years <- 2013:2023
years_split <- split(years, ceiling(years/ 5)) |> set_names(NULL) # for batching
variables <- c("2m_temperature", "2m_relative_humidity", "precipitation_flux")
# c("06_00", "09_00", "12_00", "15_00", "18_00") # time

agrometeo_parameters <- expand_grid(variable = variables, month = months, year = years_split)

# download
seasonal_forecast_download <- pmap(agrometeo_parameters, function(variable, month, year){
  dir <- here::here("downloads")
  filename <- paste("ecmwf", "agrometeo", "rsa", variable, month, min(year), "to", max(year), sep = "_")
  filename <- paste0(filename, ".tar.gz")
  if(file.exists(paste0(dir, "/", filename))) return()
  safe_download_agrometeo(user_id = user_id,
                          year = year,
                          month = month,
                          day = sprintf("%02d", 1:31),
                          variable = variable,
                          statistic = "24_hour_mean",
                          spatial_bound = c(-21, 15, -35, 37), # N, W, S, E
                          format = c("tgz"),
                          filename = filename,
                          dir = dir)
})
