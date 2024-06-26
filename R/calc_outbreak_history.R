# Notes from conversation with Noam.
# We need this for every day as a part of the dynamic data in the model. 
# Exposure to outbreaks falls of with both time and space.
# Split into recent vs old outbreaks. They could have different effects
# Old outbreaks may represent lingering immunity
# Recent outbreaks may act as sparks to ignite more outbreaks

calc_daily_outbreak_history <- function(dates,
                                        wahis_rvf_outbreaks_preprocessed,
                                        continent_raster_template,
                                        continent_polygon,
                                        country_polygons,
                                        beta_dist = .01,
                                        beta_time = 0.5,
                                        beta_cases = 1,
                                        within_km = 500,
                                        max_years = 10,
                                        recent = 1/6) {
  
}

#' Calculate the proximity in recent history and space of RVF outbreaks
#' 
#' Two components: Within season (defined as in the current year),
#' and in previous season
#' 
#' @param date The current date
#' @param season_cutoff_date
#' @param wahis_rvf_outbreaks_preprocessed
#' @param continent_raster_template
#' @return
#' @author 'Noam Ross'
#' @export
calc_outbreak_history <- function(wahis_rvf_outbreaks_preprocessed,
                                  continent_raster_template,
                                  continent_polygon,
                                  country_polygons,
                                  beta_dist = .01,
                                  beta_time = 0.5,
                                  beta_cases = 1,
                                  current_date = Sys.time(),
                                  within_km = 500,
                                  max_years = 10,
                                  recent = 1/6) {

  recent_outbreaks <- wahis_rvf_outbreaks_preprocessed |> 
    mutate(end_date = coalesce(min(current_date, outbreak_end_date, na.rm = T), outbreak_start_date),
           years_since = as.numeric(as.duration(current_date - end_date), "years"),
           weight = ifelse(is.na(cases) | cases == 1, 1,log10(cases + 1))*exp(-beta_time*years_since)) |>
   filter(years_since < recent & years_since > 0) |> 
    sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
  
  old_outbreaks <- wahis_rvf_outbreaks_preprocessed |> 
    mutate(end_date = coalesce(min(current_date, outbreak_end_date, na.rm = T), outbreak_start_date),
           years_since = as.numeric(as.duration(current_date - end_date), "years"),
           weight = ifelse(is.na(cases) | cases == 1, 1,log10(cases + 1))*exp(-beta_time*years_since)) |>
    filter(years_since >= recent & years_since < max_years) |> 
    sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326) 
  
  raster_template <- terra::rast(continent_raster_template)
  raster_template[] <- 0
  raster_template <- crop(raster_template, vect(continent_polygon$geometry), mask = TRUE) 
  
  recent_outbreak_weights <- get_outbreak_weights(recent_outbreaks, raster_template, )
  old_outbreaks_weights <- get_outbreak_weights(old_outbreaks, raster_template, )
  
  # Amplification vs interference
  # Within previous season it might amplify - for current year
  # All previous years.
  # Everything prior to current wet season vs everything in current season
  # floor date current year filter only things before that. 
  # max_years = 10
  
  
  # Generate for every date
  
  # Move to parquet
  # 1. Should be a parquet for each data (match weather anomaly) 
  # of tibble with lat, long, recent_outbreak_weight, old_outbreak_weight
}


get_outbreak_weights <- function(outbreaks, raster_template, ) {
  
  if(nrow(outbreaks) == 0) {
    raster[] <- 0
    return(raster)
  }
  
  outbreak_circles <- vect(outbreaks$geometry) |> 
    terra::buffer(within_km * 1000)
  # Unpack the raster that we want to calculate

  # raster <- crop(raster_template, outbreak_circles, mask = TRUE)
  raster <- terra::mask(raster_template, outbreak_circles)
  # For each pixel in the raster, calculate the most recent outbreak within `within_km` km
  xy <- s2::as_s2_lnglat(terra::crds(raster))
  idx <- which(!is.nan(raster[]))
  
  #Get the unique outbreaks$geometry values while maintaining the sf object characteristics
  
  
  # Create an index for the unique values of outbreaks$geometry and a column of that index for each outbreak
  locations <- unique(outbreaks$geometry)
  attributes(locations) <- attributes(outbreaks$geometry)
  outbreaks$idx <- match(outbreaks$geometry, locations)
  
  xy_o <- s2::as_s2_lnglat(locations)
  
  # For each pixel identify the outbreaks within `within_km` km
  #matches <- s2::s2_dwithin_matrix(xy,  xy_o, within_km * 1000)
  matches_dist <- s2::s2_distance_matrix(xy, xy_o)
  weights <- rowSums(exp(-beta_dist*matches_dist/1000))
  raster[idx] <- weights
  test <- terra::cover(raster, raster_template)
  
  as.data.frame(test, xy = T)

  # Convert raster into lat-long tibble. Needs all pixels 
  
}