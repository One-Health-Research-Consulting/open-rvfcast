Climatic Data Download Routine
================

## Copernicus Climate Change Service

We are downloading two data sources from Copernicus: historical and
current seasonal forecasts and agrometeorological data.

### <u>Seasonal forecast monthly statistics on single levels</u>

<https://cds.climate.copernicus.eu/cdsapp#!/dataset/seasonal-monthly-single-levels?tab=overview>

- Data type = Gridded
- Projection = Regular latitude-longitude grid
- Coverage = Global
- Resolution = 1x1 deg
- Temporal coverage = 1993 to 2016 (hindcasts); 2017 to present
  (forecasts)
- Temporal resolution = Monthly
- File format = GRIB
- Update frequency = Real-time forecasts are released once per month on
  the 13th at 12UTC
- Relevant Endpoints: 2m dewpoint temperature (K), 2m temperature (K),
  Total precipitation (m/s)

#### Download Interface & Instructions

<https://cds.climate.copernicus.eu/cdsapp#!/dataset/10.24381/cds.68dd14c3?tab=form>
<https://confluence.ecmwf.int/display/CKB/How+to+use+the+CDS+interactive+forms+for+seasonal+forecast+datasets>

- Originating Center = Select the name of the institution the
  forecasting system of your interest originates from.
- System = Select the version of the forecasting system. This is a
  numeric label and the available values are different for the different
  “originating centres”. Note that for a given start date you could find
  more than one system available for a single “originating centre”.
  Please note that you must pair up your forecasts with the relevant
  hindcasts by using the same “system” for both of them.
- Product type = Select Ensemble mean or Hindcast climate mean, can also
  select individual members values Monthly Statistics: monthly average,
  minimum, maximum and standard deviation are available for all
  initialization dates. Additionally, for real-time forecast
  initialization dates you will find the ingredients to calculate the
  forecast monthly anomalies: the forecast ensemble mean and the
  hindcast climate mean
- Year = Select the year of the initialization date of the model run(s)
  you are interested in. Note that there could be differences in the
  options available depending on your selection of forecast or hindcast
  years. Please note that you must use the hindcast data in order to
  make a meaningful use of the forecast data. And remember you must pair
  forecasts and hindcasts up by using the same “system” for both of
  them. ??? Why must you use the hindcast data in order to make a
  meaningful use of the forecast data. Maybe they are referring to the
  modelers? as explained here
  <https://confluence.ecmwf.int/display/CKB/Seasonal+forecasts+and+the+Copernicus+Climate+Change+Service>
  Models must be calibrated on performance of past forecasts relevant to
  actual data. Also important for interpretation: Information on
  forecast skill is important to avoid overconfident decision making.
- Month = Select the month of the initialization date of the model
  run(s) you are interested in. Note that in the current setup all
  monthly products are encoded using as nominal start date the 1st of
  each month, regardless of the real initialization date
- Lead time = Select the lead time(s) you are interested in. This is the
  time, in months, from the initialization date. Note that the
  convention used for labelling the data implies that leadtime_month=1
  is the first complete calendar month after the initialization date. In
  the current setup of all datasets that means that for a forecast
  initialised on the 1st of November, leadtime_month=1 is November.

Note on fixed versus on the fly forecasts:

- fixed hindcasts. Some systems are designed so their expected lifetime
  will be around 4-5 years. Once the system has been designed and
  tested, ensemble hindcasts for the whole reference period are run. The
  advantage is that this reference dataset is available well in advance
  of real-time forecasts being issued, and its properties (biases,
  skill) can be quantified once for repeated use. As this is a very
  expensive exercise, it cannot be repeated too often and thus the
  system remains fixed for a long period of time.
- on-the-fly hindcasts. Some systems prioritise more frequent upgrades,
  which means that the hindcast sets have to be run more frequently. To
  achieve this in practice, the full hindcast set is run every time a
  new real-time forecast is produced, slightly in advance (a few weeks)
  of the real-time forecast and using exactly the same version of the
  forecasting system. ie on the fly hindcasts are updated when there is
  a model change. fixed hindcasts are static. all sources are fixed
  except UKMO.

#### Additional Documentation

<https://cds.climate.copernicus.eu/cdsapp#!/dataset/10.24381/cds.68dd14c3?tab=doc>

#### Summary of Available Data

<https://confluence.ecmwf.int/display/CKB/Summary+of+available+data>

#### Recommendations for Efficiency

<https://confluence.ecmwf.int/display/CKB/Recommendations+and+efficiency+tips+for+C3S+seasonal+forecast+datasets>

Note The C3S seasonal forecast dataset is currently based on GRIB files
archived at ECMWF’s MARS archive. This implies, in some situations,
accessing data not available online (on disk) but archived in the tape
library.

Guidance for batch downloads is to download same month over multiple
years (rather than all months per year). I believe this applies only to
to daily data

### <u>Agrometeorological indicators from 1979 to present</u>

#### Overview

<https://cds.climate.copernicus.eu/cdsapp#!/dataset/sis-agrometeorological-indicators?tab=overview>

- Data type = Gridded
- Projection = Regular latitude-longitude grid
- Coverage = Global
- Horizontal Resolution = 0.1x0.1 deg
- Temporal coverage = 1979 to present
- Temporal resolution = daily
- File format = NetCDF-4
- Update frequency = Monthly
- Relevant Endpoints: 2m dewpoint temperature units (K), 2m temperature
  units (K), Precipitation flux (total volume of liquid water (mm3)
  precipitated over the period 00h-24h local time per unit of area
  (mm2), per day.), relative humidity %,
- DOES NOT HAVE Total precipitation (but can be converted from flux
  based on time and area, m/s to mm3)

#### Packages and Auth

``` r
quiet_library <- function(pck) suppressPackageStartupMessages(library(pck, character.only = TRUE))
quiet_library("ecmwfr")
quiet_library("tidyverse")
quiet_library("lubridate")

# user ID (for authentication), see ecmwf package
user_id <- "173186"

# https://github.com/bluegreen-labs/ecmwfr#file-based-keychains
# ^ linux need to unlock keyring
```

#### Define Seasonal Forecast Function

``` r
download_seasonal_forecasts <- function(user_id,
                                        system,
                                        year,
                                        month,
                                        center = c("ecmwf"),
                                        variable = c("2m_dewpoint_temperature", "2m_temperature", "total_precipitation"), 
                                        product_type = c("monthly_mean", "monthly_maximum", "monthly_minimum", "monthly_standard_deviation"), 
                                        leadtime_month = c("1", "2", "3", "4", "5", "6"),
                                        spatial_bound = c(90, -180, -90, 180), # N, W, S, E
                                        format = "grib",
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
```

#### Map out Seasonal Forecast API Parameters

Once initial download is complete, we only need to download the current
month on a regular schedule

``` r
# System 4 covers just sept/oct 2017
sys4 <- tibble(system = 4, year = list(2017), month = list(9:10))

# System 51 covers nov 2022 through present
sys51_dates <- seq(ymd("2022-11-01"), Sys.Date(), by = "month")

## Real-time forecasts are released once per month on the 13th at 12UTC
## Check if new forecast has been released this month
current_time <- as.POSIXlt(Sys.time(), tz = "UTC")
current_year <- year(current_time)
current_month <- month(current_time)
update_date <- ymd_hms(paste0(current_year,"-", current_month, "-13 12:00:00"))
if(current_time < update_date) sys51_dates <- sys51_dates[-length(sys51_dates)]

## Split into batches of 5 years for download limits (only will be applicable in 2027 😄)
sys51_years <- unique(year(sys51_dates))
if(length(sys51_years) > 5){
  sys51_years <- split(sys51_years, ceiling(sys51_years/5))
}else{
  sys51_years <- list(sys51_years)
}
sys51 <- tibble(system = 51,
                year = sys51_years,
                month = list(unique(month(sys51_dates))))

# System 5 covers everything else. 
sys5_years <- 1993:2022
sys5 <- tibble(system = 5, year = split(sys5_years, ceiling(sys5_years/ 5)), month = list(1:12))

# Tibble to interate over rowwise for download
seasonal_forecast_parameters <- bind_rows(sys4, sys51, sys5)
head(seasonal_forecast_parameters)
```

    ## # A tibble: 6 × 3
    ##   system year         month     
    ##    <dbl> <named list> <list>    
    ## 1      4 <dbl [1]>    <int [2]> 
    ## 2     51 <dbl [2]>    <dbl [4]> 
    ## 3      5 <int [3]>    <int [12]>
    ## 4      5 <int [5]>    <int [12]>
    ## 5      5 <int [5]>    <int [12]>
    ## 6      5 <int [5]>    <int [12]>

#### Download Seasonal Forecast

``` r
dir <- here::here("downloads")

#TODO debug system 51 failing - may need to split by year? it's looking for nov-dec in 2023 and jan-feb in 2022?
seasonal_forecast_parameters <- seasonal_forecast_parameters |> filter(system != 51) 

# 8 files
# ~ 5 hrs
# ~ 0.6 GB

nrow(seasonal_forecast_parameters)
```

    ## [1] 8

``` r
seasonal_forecast_download <- pmap(seasonal_forecast_parameters, function(system, year, month){
  filename <- paste("ecmwf", "seasonal_forecast", "rsa", system, min(year), "to", max(year), sep = "_")
  filename <- paste0(filename, ".grib")
  if(file.exists(paste0(dir, "/", filename))) return() # for now skip if file exists
  safe_download_seasonal_forecasts(user_id = user_id,
                                   system = system,
                                   year = year,
                                   month = month,
                                   center = "ecmwf",
                                   variable = c("2m_dewpoint_temperature", "2m_temperature", "total_precipitation"),
                                   product_type = c("monthly_mean", "monthly_maximum", "monthly_minimum", "monthly_standard_deviation"),
                                   leadtime_month = c("1", "2", "3", "4", "5", "6"),
                                   spatial_bound = c(-21, 15, -35, 37), # N, W, S, E
                                   format = "grib",
                                   filename = filename,
                                   dir = dir)
})

pluck(seasonal_forecast_download, "error")
```

    ## NULL

#### Define Agrometeorological Function

``` r
download_agrometeo <- function(user_id,
                               year,
                               month,
                               day,
                               variable = c("2m_dewpoint_temperature", "2m_temperature", "precipitation_flux"), 
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
```

#### Map out Agrometeorological API Parameters

Once initial download is complete, we only need to download the current
month on a regular schedule

``` r
### Guidance for batch downloads is to download same month over multiple years (rather than all months per year)
### Indicators have to be separate (cannot combine temp, precip, etc)

# All months
months <- sprintf("%02d", 1:12)

# Years go back to 1979, for now start in 2013
#years <- 1979:2023 
years <- 2013:2023 

# Split into batches of 5 years for download limits 
years_split <- split(years, ceiling(years/ 5)) |> set_names(NULL) # for batching

# Variables
variables <- c("2m_temperature", "precipitation_flux")#, "2m_dewpoint_temperature",)

# If we need time...
# time <- c("06_00", "09_00", "12_00", "15_00", "18_00") 

agrometeo_parameters <- expand_grid(variable = variables, month = months, year = years_split)

head(agrometeo_parameters)
```

    ## # A tibble: 6 × 3
    ##   variable       month year     
    ##   <chr>          <chr> <list>   
    ## 1 2m_temperature 01    <int [3]>
    ## 2 2m_temperature 01    <int [5]>
    ## 3 2m_temperature 01    <int [3]>
    ## 4 2m_temperature 02    <int [3]>
    ## 5 2m_temperature 02    <int [5]>
    ## 6 2m_temperature 02    <int [3]>

#### Download Agrometeorological

``` r
dir <- here::here("downloads")

# Each file is 1-10 MB
# ~30 sec/file (but they may slow you down with mutliple requests, and not sure they allow parallel requests)
# 360 files
# back of envelope approx 3 hrs and 3.6gb
agrometeo_forecast_download <- pmap(agrometeo_parameters, function(variable, month, year){
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
pluck(seasonal_forecast_download, "error")
```

    ## NULL

TODO

- Debug sys 51 failing
- Finish download of humidity / dewpoint temp data
- Other forecasts besides ECMWF?
- Read in files
- Error/Uncertainty
