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

#' Map the geographic progression of space-time linked outbreak chains
#'
#' One overview map of Africa: outbreaks in the largest chains are colored by chain
#' identity and connected chronologically with a path so the geographic progression of
#' each chain is visible; all other (small/singleton) chains are folded into a single
#' neutral grey category. Point shape distinguishes the epidemiological index case
#' (first case of the whole chain) from a country-level index case (first case of the
#' chain to land in a given country, i.e. the case that country's own forecast most
#' needed to catch).
#'
#' @author Morgan Kain
#'
#' @param outbreaks_indexed Output of identify_index_outbreaks
#' @param country_polygons Country-level sf polygons used as the basemap; continent_polygon target
#' @param top_n_chains How many of the largest (by case count) multi-case chains to
#'   color individually; all others are grouped into "Other / single outbreak"
#'
#' @return A ggplot object
#'
#' @export
make_chain_progression_map <- function(outbreaks_indexed, country_polygons, top_n_chains = 8) {

  stopifnot(all(c("outbreak_chain_id", "index_case", "country_iso3c", "country_index_case") %in% names(outbreaks_indexed)))

  highlighted_chains <- outbreaks_indexed |>
    filter(!is.na(outbreak_chain_id)) |>
    count(outbreak_chain_id, name = "n_cases") |>
    filter(n_cases > 1) |>
    arrange(desc(n_cases)) |>
    slice_head(n = top_n_chains) |>
    pull(outbreak_chain_id)

  chain_pal <- chain_color_palette(highlighted_chains)

  plot_dat <- outbreaks_indexed |>
    filter(!is.na(outbreak_chain_id)) |>
    mutate(
      chain_group = if_else(outbreak_chain_id %in% highlighted_chains, as.character(outbreak_chain_id), "Other / single outbreak")
    , point_type  = case_when(
        index_case == 1                                ~ "Index case"
      , country_index_case == 1                        ~ "New-country entry"
      , TRUE                                           ~ "Chain member"
      )
    ) |>
    arrange(outbreak_chain_id, start_date)

  path_dat <- plot_dat |> filter(chain_group != "Other / single outbreak")

  ggplot() +
    geom_sf(data = country_polygons, fill = "grey97", color = "grey70", linewidth = 0.2) +
    geom_point(
      data = plot_dat |> filter(chain_group == "Other / single outbreak")
    , mapping = aes(x = longitude, y = latitude)
    , color = "grey80", size = 1, alpha = 0.6
    ) +
    geom_path(
      data = path_dat
    , mapping = aes(x = longitude, y = latitude, group = outbreak_chain_id, color = chain_group)
    , linewidth = 0.5, alpha = 0.7
    ) +
    geom_point(
      data = path_dat
    , mapping = aes(x = longitude, y = latitude, color = chain_group, shape = point_type)
    , size = 2.2
    ) +
    scale_color_manual(values = chain_pal, breaks = as.character(highlighted_chains), name = "Chain (top by size)") +
    scale_shape_manual(
      values = c("Chain member" = 16, "New-country entry" = 17, "Index case" = 8)
    , name   = "Case type"
    ) +
    coord_sf(datum = NA) +
    theme_void() +
    labs(
      title    = "Outbreak chain progression across Africa"
    , subtitle = paste0("Top ", length(highlighted_chains), " chains by case count highlighted; grey = singleton/small chains")
    )

}

#' Small multiples of the largest outbreak chains, colored by time and marked by
#' country-of-first-entry
#'
#' For the largest chains individually, plots each chain's cases in chronological
#' order (color = date, a sequential scale) connected by a path, with the case that
#' first brought the chain into a new country labeled by date. This is meant to make
#' cross-border spread within a single chain visually obvious: a path segment that
#' crosses into a new polygon on the basemap and is immediately followed by a labeled
#' point is a border crossing.
#'
#' @author Morgan Kain
#'
#' @param outbreaks_indexed Output of identify_index_outbreaks 
#' @param country_polygons Country-level sf polygons used as the basemap
#' @param top_n_chains How many of the largest multi-case chains to facet over
#'
#' @return A ggplot object faceted by chain
#'
#' @export
make_chain_progression_facets <- function(outbreaks_indexed, country_polygons, top_n_chains = 6) {

  stopifnot(all(c("outbreak_chain_id", "country_iso3c", "country_index_case") %in% names(outbreaks_indexed)))

  chain_sizes <- outbreaks_indexed |>
    filter(!is.na(outbreak_chain_id)) |>
    count(outbreak_chain_id, name = "n_cases") |>
    filter(n_cases > 1) |>
    arrange(desc(n_cases)) |>
    slice_head(n = top_n_chains)

  facet_dat <- outbreaks_indexed |>
    inner_join(chain_sizes, by = "outbreak_chain_id") |>
    arrange(outbreak_chain_id, start_date) |>
    mutate(
      n_countries_in_chain = n_distinct(country_iso3c)
    , country_word         = if_else(n_countries_in_chain == 1, "country", "countries")
    , chain_label           = paste0("Chain ", outbreak_chain_id, " (n=", n_cases, ", ", n_countries_in_chain, " ", country_word, ")")
      ## Days since the chain's own first case, not the calendar date -- each chain only
       ## spans weeks-to-months, so a shared 2006-2025 color scale would wash every facet
       ## out to nearly one flat color and is not comparable across chains from different eras
    , days_since_start      = as.numeric(start_date - min(start_date))
    , .by                   = outbreak_chain_id
    , entry_label           = if_else(country_index_case == 1, format(start_date, "%Y-%m-%d"), NA_character_)
    )

  ## Country borders as plain x/y paths rather than an sf layer: coord_sf() (which
   ## geom_sf would force) does not support facet_wrap(scales = "free"), and each chain
   ## needs its own zoom level here. L1 = ring/part, L2 = country feature index
  border_lines <- sf::st_coordinates(sf::st_boundary(country_polygons)) |>
    as.data.frame() |>
    dplyr::mutate(ring_id = paste(L1, L2, sep = "_"))

  ggplot(facet_dat, aes(x = longitude, y = latitude)) +
    geom_path(data = border_lines, mapping = aes(x = X, y = Y, group = ring_id), inherit.aes = FALSE, color = "grey70", linewidth = 0.2) +
    geom_path(aes(group = outbreak_chain_id), color = "grey40", linewidth = 0.4, alpha = 0.6) +
    geom_point(aes(color = days_since_start, shape = factor(country_index_case)), size = 2) +
    ggrepel::geom_label_repel(aes(label = entry_label), size = 2.3, na.rm = TRUE, max.overlaps = 20, label.padding = 0.15) +
    scale_color_viridis_c(name = "Days since\nchain start") +
    scale_shape_manual(values = c("0" = 16, "1" = 17), labels = c("No", "Yes"), name = "New-country entry") +
    facet_wrap(~ chain_label, scales = "free") +
    theme_void() +
    theme(legend.position = "bottom", strip.text = element_text(size = 9))

}

#' Bar chart of how many outbreak chains touch how many countries
#' 
#' @author Morgan Kain
#'
#' @param outbreaks_indexed Output of identify_index_outbreaks
#'
#' @return A ggplot object
#'
#' @export
make_border_crossing_summary_plot <- function(outbreaks_indexed) {

  stopifnot(all(c("outbreak_chain_id", "country_iso3c") %in% names(outbreaks_indexed)))

  count_dat <- outbreaks_indexed |>
    filter(!is.na(outbreak_chain_id)) |>
    summarize(n_cases = n(), n_countries = n_distinct(country_iso3c), .by = outbreak_chain_id) |>
    filter(n_cases > 1) |>
    count(n_countries, name = "n_chains")

  ggplot(count_dat, aes(x = factor(n_countries), y = n_chains)) +
    geom_col(fill = "#3B7EA1", width = 0.6) +
    geom_text(aes(label = n_chains), vjust = -0.4, size = 3.5) +
    labs(
      x     = "Countries touched by the chain"
    , y     = "Number of chains (>1 outbreak)"
    , title = "How many outbreak chains cross borders?"
    ) +
    theme_minimal(base_size = 12) +
    theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank())

}

## Categorical palette for the largest chains; anything outside the highlighted set
 ## is expected to be plotted separately as flat grey
chain_color_palette <- function(highlighted_chains) {

  n <- max(length(highlighted_chains), 1)
  pal_n <- max(3, min(n, 8))

  setNames(
    RColorBrewer::brewer.pal(pal_n, "Dark2")[seq_len(n)]
  , as.character(highlighted_chains)
  )

}
