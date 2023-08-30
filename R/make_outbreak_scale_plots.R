make_south_africa_outbreak_scale_map <- function(wahis_rvf_outbreaks_raw) {

  south_africa_map <- rnaturalearth::ne_states(country = "South Africa", returnclass = "sf") |> 
    sf::st_crop(ymin = -35, ymax = -22, xmin = 16, xmax = 33)
  
  plot_points <- wahis_rvf_outbreaks_raw |> 
    group_by(epi_event_id) |> 
    mutate(outbreak_start_date = lubridate::ymd_hms(outbreak_start_date)) |> 
    mutate(days = outbreak_start_date - min(outbreak_start_date, na.rm = TRUE),
           origin_loc = days == 0,
           single_outbreak = if_else(n() == 1, "Single Outbreak", "Outbreak Origin"),
           label = format(min(outbreak_start_date), "%Y-%m-%d")) |> 
    arrange(desc(days)) |> 
    ungroup() |> 
    select(country, epi_event_id, report_id, longitude, latitude, days, origin_loc, label, single_outbreak, outbreak_start_date) |> 
    mutate(across(c(longitude, latitude), as.numeric)) |> 
    filter(!is.na(longitude), !is.na(latitude))

  plot_points_sa <- plot_points |> 
    dplyr::filter(country == "south africa")

  ggplot()+
    geom_sf(data = south_africa_map) +
    geom_point(data = plot_points_sa, aes(x=longitude, y=latitude, color = origin_loc, shape = single_outbreak), size = 4) +
    scale_shape_manual(values = c(20,1), name = "") +
    scale_color_manual(values = c("grey40", "red"), guide = guide_none()) +
   scale_fill_discrete(guide = guide_none()) +
    ggforce::geom_mark_hull(data = plot_points_sa,
                           aes(x=longitude, y=latitude, group = label, fill = label), radius = 0.01, expand = 0.01, concavity = 3) +
    ggrepel::geom_label_repel(data = plot_points_sa |> dplyr::filter(origin_loc) |> distinct(outbreak_start_date, .keep_all = TRUE), 
                             mapping = aes(x=longitude, y=latitude, label = label), alpha = 0.8) +
    theme_void()
  }

make_africa_outbreak_scale_map <- function(wahis_rvf_outbreaks_raw) {
  
  plot_points <- wahis_rvf_outbreaks_raw |> 
    group_by(epi_event_id) |> 
    mutate(outbreak_start_date = lubridate::ymd_hms(outbreak_start_date)) |> 
    mutate(days = outbreak_start_date - min(outbreak_start_date, na.rm = TRUE),
           origin_loc = days == 0,
           single_outbreak = if_else(n() == 1, "Single Outbreak", "Outbreak Origin"),
           label = format(min(outbreak_start_date), "%Y-%m-%d")) |> 
    arrange(desc(days)) |> 
    ungroup() |> 
    select(country, epi_event_id, report_id, longitude, latitude, days, origin_loc, label, single_outbreak, outbreak_start_date) |> 
    mutate(across(c(longitude, latitude), as.numeric)) |> 
    filter(!is.na(longitude), !is.na(latitude))
  
  africa_map <- rnaturalearth::ne_countries(continent = "Africa", returnclass = "sf")
  
  ggplot()+
    geom_sf(data = africa_map) +
    geom_point(data = plot_points, aes(x=longitude, y=latitude, color = origin_loc, shape = single_outbreak)) +
    scale_shape_manual(values = c(20,1), name = "") +
    scale_color_manual(values = c("grey40", "red"), guide = guide_none()) +
    scale_fill_discrete(guide = guide_none()) +
    ggforce::geom_mark_hull(data = plot_points,
                            aes(x=longitude, y=latitude, fill = label), radius = 0.01, expand = 0.01, concavity = 3) +
     ggrepel::geom_label_repel(data = plot_points |> dplyr::filter(origin_loc) |> distinct(outbreak_start_date, .keep_all = TRUE), 
                               mapping = aes(x=longitude, y=latitude, label = label), alpha = 0.8) +
    theme_void()
  
}

make_south_africa_outbreaks_timeline <- function(wahis_rvf_outbreaks_raw) {
  
  plot_points <- wahis_rvf_outbreaks_raw |> 
    group_by(epi_event_id) |> 
    mutate(outbreak_start_date = lubridate::ymd_hms(outbreak_start_date)) |> 
    mutate(days = outbreak_start_date - min(outbreak_start_date, na.rm = TRUE),
           origin_loc = days == 0,
           single_outbreak = if_else(n() == 1, "Single Outbreak", "Outbreak Origin"),
           label = format(min(outbreak_start_date), "%Y-%m-%d")) |> 
    arrange(desc(days)) |> 
    ungroup() |> 
    select(country, epi_event_id, report_id, longitude, latitude, days, origin_loc, label, single_outbreak, outbreak_start_date) |> 
    mutate(across(c(longitude, latitude), as.numeric)) |> 
    filter(!is.na(longitude), !is.na(latitude))
  
  plot_points_sa <- plot_points |> 
    dplyr::filter(country == "south africa")
  
  ggplot(plot_points_sa) +
    geom_density(mapping = aes(x = outbreak_start_date)) +
    geom_point(data = filter(plot_points_sa, origin_loc), mapping = aes(x = outbreak_start_date, y = 0), col = "red")
  
}
