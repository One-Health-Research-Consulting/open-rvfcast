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
                                  max_years = 10) {

  
  outbreaks <- wahis_rvf_outbreaks_preprocessed |> 
    mutate(end_date = coalesce(outbreak_end_date, outbreak_start_date),
           years_since = as.numeric(as.duration(current_date - end_date), "years"),
           weight = ifelse(is.na(cases) | cases == 1, 1,log10(cases + 1))*exp(-beta_time*years_since)) |>
   # filter(years_since < max_years) |>
    sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
  
  
  outbreak_circles <- vect(outbreaks$geometry) |> 
    terra::buffer(within_km * 1000)
  # Unpack the raster that we want to calculate
  raster <- terra::rast(continent_raster_template)
  raster[] <- 1
  #raster <- crop(raster, outbreak_circles, mask = TRUE) 
  raster <- crop(raster, vect(continent_polygon$geometry), mask = TRUE) 
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
  matches_dist <- s2::s2_distance_matrix(xy,  xy_o)
  weights <- rowSums(exp(-beta_dist*matches_dist/1000))
  raster[idx] <- weights
  
  
}


