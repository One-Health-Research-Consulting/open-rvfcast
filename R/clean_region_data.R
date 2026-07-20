#' Clean up raw data for use in modeling
#'
#'
#' @title clean_region_data

#' @param dat Complete region_data
#' @param map_dat hex info 
#' @return Tibble of cleaned region data
#' @author Morgan Kain
#' @export

clean_region_data <- function(dat, map_dat) {

  ## Drop all columns that already have a scaled counterpart
  scaled_cols <- names(dat)[str_detect(names(dat), "_scaled_")]
  base_cols   <- str_replace(scaled_cols, "_scaled_", "_")
  dat         <- dat |> dplyr::select(-any_of(base_cols))
  
  ## Drop the sero colums we are not using
  dat <- dat |> dplyr::select(-contains("anomaly_scaled_sero"))

  ## Not ideal as this will need to be manually adjusted, but ok for now. Scale
   ## all of these
  vars_to_scale <- c(
    "glw_cattle", "glw_sheep", "glw_goats", "wc2.1_30s_elev", "Annual_Mean_Temperature"
  , "Mean_Diurnal_Range", "Isothermality", "Temperature_Seasonality", "Max_Temperature_of_Warmest_Month"
  , "Min_Temperature_of_Coldest_Month", "Temperature_Annual_Range", "Mean_Temperature_of_Wettest_Quarter"
  , "Mean_Temperature_of_Driest_Quarter", "Mean_Temperature_of_Warmest_Quarter"
  , "Mean_Temperature_of_Coldest_Quarter", "Annual_Precipitation"
  , "Precipitation_of_Wettest_Month", "Precipitation_of_Driest_Month"
  , "Precipitation_Seasonality", "Precipitation_of_Wettest_Quarter"
  , "Precipitation_of_Driest_Quarter", "Precipitation_of_Warmest_Quarter", "Precipitation_of_Coldest_Quarter"
  )

  dat <- dat |> mutate(across(all_of(vars_to_scale), ~ as.numeric(scale(.x)[, 1])))

  ## dropping extremely rare land types that are causing model fitting to behave somewhat strangely
  dat <- dat |> dplyr::select(-c(built, snow, mangroves, moss))

  ## Add in x, y and doy
  lat_lon_centers <- st_centroid(map_dat) |>
    st_coordinates() |> 
    as.data.frame() %>%
    rename(lat = Y, lon = X) |>
    bind_cols(map_dat |> as.data.frame() |> dplyr::select(-geometry))
  
  dat <- dat |> 
    left_join(lat_lon_centers) |>
    relocate(lat, lon, .after = outbreak) |>
    mutate(doy = yday(date), .after = date)
  
  dat

}
