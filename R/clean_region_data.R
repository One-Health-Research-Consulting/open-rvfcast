#' Clean up raw data for use in modeling
#'
#'
#' @title clean_region_data

#' @param dat Complete region_data
#' @return Tibble of cleaned region data
#' @author Morgan Kain
#' @export

clean_region_data <- function(dat) {

  ## Drop all columns that already have a scaled counterpart
  scaled_cols <- names(dat)[str_detect(names(dat), "_scaled_")]
  base_cols   <- str_replace(scaled_cols, "_scaled_", "_")
  dat         <- dat |> select(-any_of(base_cols))

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
  , "pred_sero"
  )

  dat <- dat |> mutate(across(all_of(vars_to_scale), ~ as.numeric(scale(.x)[, 1])))

  dat

}
