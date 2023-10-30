suppressPackageStartupMessages(source("packages.R"))
for (f in list.files(here::here("R"), full.names = TRUE)) source (f)


tar_load(ecmwf_forecasts_transformed)

# get forecast anomalies relative to actual (NASA POWER) data

ecmwf <- open_dataset(ecmwf_forecasts_transformed[[9]])

data_types <- ecmwf |> distinct(data_type) |> collect()
vars <- ecmwf |> distinct(short_name) |> collect()


# NASA POWER WEATHER
#  RH2M            MERRA-2 Relative Humidity at 2 Meters (%) ;
#  T2M             MERRA-2 Temperature at 2 Meters (C) ;
#  PRECTOTCORR     MERRA-2 Precipitation Corrected (mm/day)  

# ECMWF https://www.ecmwf.int/en/forecasts/datasets/set-i
# 2d = "2m_dewpoint_temperature" = 	2 metre dewpoint temperature K
# 2t = "2m_temperature" = 	2 metre temperature K
# tprate = "total_precipitation" =  Total precipitation rate kg m**-2 s**-1
# trprate is equivalent to mm per second https://codes.ecmwf.int/grib/param-db/?id=260048


# TODO Units
# 1. covert temperatures to C
# 2. calculate relative humidity 
# 3. calculate convert precip to daily

# TODO anomalies
# 1. filter fcmean
# 2. calculate weighted means for 1, 2, 3, 4, 5, 6 months
# 3. compare to actual recorded historical means for DOYs


nasa <- open_dataset(weather_anomalies[[9]])
