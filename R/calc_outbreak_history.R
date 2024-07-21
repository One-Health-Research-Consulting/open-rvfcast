
# This is going to be dynamic branching over list of dates. Then a target to convert to raster stacks, one for parquet, and one for animation
get_daily_outbreak_history <- function(dates,
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
  
  daily_outbreak_history <- map_dfr(dates, function(d) calc_outbreak_history(date = d,
                                                                             wahis_rvf_outbreaks_preprocessed,
                                                                             continent_raster_template,
                                                                             continent_polygon,
                                                                             country_polygons,
                                                                             beta_dist = .01,
                                                                             beta_time = 0.5,
                                                                             beta_cases = 1,
                                                                             within_km = 500,
                                                                             max_years = 10,
                                                                             recent = 1/6))
  
  daily_old_outbreak_history <- terra::rast(daily_outbreak_history$old_outbreaks_rast)
  daily_recent_outbreak_history <- terra::rast(daily_outbreak_history$recent_outbreaks_rast)
  
  list(daily_old_outbreak_history = daily_old_outbreak_history,
       daily_recent_outbreak_history = daily_recent_outbreak_history)
  
}

test <- get_daily_outbreak_history(dates = dates,
                                   wahis_rvf_outbreaks_preprocessed = wahis_rvf_outbreaks_preprocessed,
                                   continent_raster_template = continent_raster_template,
                                   continent_polygon = continent_polygon,
                                   country_polygons = country_polygons)

get_outbreak_history_animation(daily_old_outbreak_history)

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
#' @author 'Noam Ross and Nathan Layman'
#' @export
calc_outbreak_history <- function(date, 
                                  wahis_rvf_outbreaks_preprocessed,
                                  continent_raster_template,
                                  continent_polygon,
                                  country_polygons,
                                  beta_dist = .01,
                                  beta_time = 0.5,
                                  beta_cases = 1,
                                  within_km = 500,
                                  max_years = 10,
                                  recent = 1/6) { # two months

  message(paste("Extracting outbreak history for", as.Date(date)))
  # Identify time in years since outbreak.
  # Establish a weighting factor that captures how recently the outbreak occurred tapering off exponentially
  # A history of outbreaks can have either a interference effect (previous exposure leads to resistance) 
  # or amplifying effect (previous outbreaks ignite new ones nearby)
  outbreak_history <- wahis_rvf_outbreaks_preprocessed |> 
    mutate(end_date = pmin(date, coalesce(outbreak_end_date, outbreak_start_date), na.rm = T),
           years_since = as.numeric(as.duration(date - end_date), "years"),
           weight = ifelse(is.na(cases) | cases == 1, 1, log10(cases + 1))*exp(-beta_time*years_since)) |>
    filter(years_since < max_years & years_since > 0) 
  
  if(!nrow(outbreak_history)) return(NULL)
  
  outbreak_history <- outbreak_history |> sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

  old_outbreaks <- outbreak_history |> filter(years_since >= recent)
  recent_outbreaks <- outbreak_history |> filter(years_since < recent)
  
  raster_template <- terra::unwrap(continent_raster_template)
  raster_template[] <- 1
  raster_template <- terra::crop(raster_template, terra::vect(continent_polygon$geometry), mask = TRUE) 
  names(raster_template) <- as.Date(date)
  
  recent_outbreak_rast <- get_outbreak_distance_weights(recent_outbreaks, raster_template, within_km)
  old_outbreaks_rast <- get_outbreak_distance_weights(old_outbreaks, raster_template, within_km)
  
  # Integrate space and time
  # Multiply the time weight by the distance weights
  tibble(recent_outbreaks_rast = list(recent_outbreak_rast),
         old_outbreaks_rast = list(old_outbreaks_rast))
  
  # Move to parquets
  # 1. Should be a parquet for each data (match weather anomaly) 
  # of tibble with lat, long, recent_outbreak_weight, old_outbreak_weight
}

# This needs to happen for each outbreak separately otherwise when we calculate weights
# They can blend together. What I mean is that the distance matrix contains distances between
# cells within each circle and every origin not just the origin of that circle.
get_outbreak_distance_weights <- function(outbreaks, raster_template, within_km = 500, beta_dist = 0.01) {
  
  if(!nrow(outbreaks)) {
    raster_template[] <- 0
    return(raster_template)
  }
  
  # Identify unique events
  outbreaks <- outbreaks |> select(cases, years_since, weight) |> distinct()

  # raster <- crop(raster_template, outbreak_circles, mask = TRUE)
  # raster <- terra::mask(raster_template, outbreak_circles, updatevalue = 0)
  # For each pixel in the raster, calculate the most recent outbreak within `within_km` km
  xy <- s2::as_s2_lnglat(terra::crds(raster_template))
  idx <- which(!is.nan(raster_template[]))
  
  #Get the unique outbreaks$geometry values while maintaining the sf object characteristics
  
  # We don't want unique LOCATIONS. We want unique outbreaks. An outbreak at a farm 6 months ago and another at the
  # same place 12 months ago should both contribute to the weight.
  # # Create an index for the unique values of outbreaks$geometry and a column of that index for each outbreak
  # locations <- unique(outbreaks$geometry)
  # attributes(locations) <- attributes(outbreaks$geometry) # Make into geometry set
  # outbreaks$idx <- match(outbreaks$geometry, locations)
  
  xy_o <- s2::as_s2_lnglat(outbreaks)
  
  # For each pixel identify the outbreaks within `within_km` km
  # matches <- s2::s2_dwithin_matrix(xy,  xy_o, within_km * 1000)
  # 883 origins (xy_o) x 133491 cells (xy) = 117872553 distances.
  # but some of those distances cross the buffer. It's every cell to every origin.
  # We don't need to calculate that just within the circles around each origin
  # Also this doesn't connect to log cases at all. It's just a strict spatial
  # distance weight at this point.
  # s2::s2_distance_matrix units are in METERS
  matches_dist <- s2::s2_distance_matrix(xy, xy_o)
  
  # Drop all distances greater than within_km
  # Not sure why we need to do this given choice of beta_dist
  # Enforcing prior?
  matches_dist[matches_dist > (within_km * 1000)] <- NA
  
  # Calculate a weighting factor based on distance. Note we haven't included log10 cases yet.
  # This is negative exponential decay - points closer to the origin will be 1 and those farther
  # away will be closer to zero mediated by beta_dist.
  weights <- exp(-beta_dist*matches_dist/1000)
  
  # Incorporate time and log10(cases + 1)
  weights <- sweep(weights, 2, outbreaks$weight, "*")
  
  # Combine contributions from all outbreaks
  weights <- rowSums(weights, na.rm = T)
  
  raster_template[idx] <- weights
  
  raster_template
}

get_outbreak_history_animation <- function(outbreak_raster, 
                                           output_dir = "outputs",
                                           output_filename = "outbreak_history.gif") {
 
  df <- as.data.frame(outbreak_raster, xy=TRUE)
  
  df_long <- df %>%
    pivot_longer(-c(x, y), names_to="date", values_to="value") |>
    mutate(date = as.Date(date),
           display_date = format(date, "%B %Y"))
  
  # Left off here:
  # labs(title = '{gsub(pattern = "[0-9]+-", replacement = "", closest_state)}')
  p <- ggplot(df_long, aes(x=x, y=y, fill=value)) +
    geom_raster() +
    scale_fill_viridis_c(limits=c(min(df_long$value, na.rm=T), max(df_long$value, na.rm=T))) +
    labs(title = '{closest_state}', x = "Longitude", y = "Latitude", fill = "Weight") +
    theme_minimal() +
    theme(text=element_text(size = 14))
  
  # I can't get anim_save to work on my mac. Switching to ImageMagick rather than bother fixing it
  # gifs save and render fine but can't be opened once saved. I tried re-installing gifski
  # on brew and building the package from source and nothing worked so I gave up.
  gganim <- gganimate::animate(p + gganimate::transition_states(states = date, 
                                                                transition_length = 12, 
                                                                state_length = 1, 
                                                                wrap = FALSE),
                               nframes = 400, 
                               fps = 8.1, 
                               width = 550, 
                               height = 350,
                               end_pause = 10, 
                               start_pause =  20,
                               renderer = gganimate::magick_renderer())
  
  # animate(anim, nframes = 400,fps = 8.1,  width = 550, height = 350,
  #         renderer = gifski_renderer("car_companies_2.gif"), end_pause = 15, start_pause =  25)
  
  magick::image_write(gganim, path=paste(output_dir, output_filename, sep = "/"))
  
  }
