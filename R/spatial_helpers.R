####
## Large series of spatial helper functions to assist with region clustering
## for spatial inner folds and plotting
####

#### Used in / for fold_data.R --------------------------------------------------

## Build a clean tibble of regions from region_districts, cleaning up small problems like crossing edges
build_clean_country  <- function(x) {
  
  ## Harmonize cols & CRS
  x <- st_transform(x, 4326) %>%
    transmute(
      country  = .data$shapeGroup
      , region   = .data$shapeName
      , geometry = .data$geometry
    ) %>% 
    ## Validity pass
    sf::st_make_valid(x) %>% 
    ## keep polygonal parts only
    suppressWarnings(st_collection_extract(x, "POLYGON")) 
  
  ## Drop empties
  x <- x[!st_is_empty(x), , drop = FALSE]
  
  ## Invalid pieces can remain; buffer(0) can help
  bad <- !st_is_valid(x)
  if (any(bad)) x[bad, "geometry"] <- st_buffer(x[bad, "geometry"], 0)
  
  ## Dissolve to one MULTIPOLYGON for each country:region
  x %>%
    sf::st_transform("+proj=cea +lon_0=0 +lat_ts=0 +datum=WGS84 +units=m +no_defs") %>%
    dplyr::group_by(country, region) %>%
    dplyr::summarise(geometry = sf::st_union(geometry), .groups = "drop") %>%
    sf::st_transform(4326)
  
}

## Unify multi-region list
build_regions_sf     <- function(sf_list) {
  
  ## Temporarily disable s2 for robust handling of issue polygons
  old_s2 <- sf::sf_use_s2()
  on.exit(sf::sf_use_s2(old_s2), add = TRUE)
  sf::sf_use_s2(FALSE)
  
  ## Tidy up region shapes
  cleaned <- lapply(sf_list, build_clean_country)
  
  ## Bind all countries; remove accidental duplicates if there are any
  regions <- dplyr::bind_rows(cleaned) %>%
    distinct(country, region, .keep_all = TRUE)
  
  ## For safety conduct a final validity sweep + drop empties
  regions <- sf::st_make_valid(regions)
  regions <- regions[!st_is_empty(regions), , drop = FALSE]
  
  return(regions)
  
}

## Project to equal-area, compute areas (km^2), drop tiny slivers
prep_equal_area      <- function(regions, min_area_km2 = 5) {
  
  ## Equal-area CRS (Cylindrical Equal Area)
  cea        <- "+proj=cea +lon_0=0 +lat_ts=0 +datum=WGS84 +units=m +no_defs"
  regions_eq <- regions %>% sf::st_transform(cea) %>% vfix()
  
  ## Cast, compute area, filter small bits, dissolve back per (country, region)
  parts          <- suppressWarnings(sf::st_cast(regions_eq, "POLYGON"))
  parts$area_km2 <- as.numeric(sf::st_area(parts)) / 1e6
  
  regions_filtered <- parts %>%
    dplyr::filter(.data$area_km2 >= min_area_km2) %>%
    dplyr::group_by(.data$country, .data$region) %>%
    dplyr::summarise(geometry = sf::st_union(.data$geometry), .groups = "drop") %>%
    vfix()
  
  regions_filtered$area_km2 <- as.numeric(sf::st_area(regions_filtered)) / 1e6
  
  return(regions_filtered)
  
}

## Collapse within-country regions so that within-country regions are *vaguely* comparable
 ## in size across countries to speed up the spatial folding across all of Africa
reduce_country_units <- function(sf_cty, target_unit_area_km2 = 15000, min_unit_area_km2 = 8000) {
  
  ## Nothing to do for small countries
  if (nrow(sf_cty) <= 1) return(sf_cty)
  
  ## Compute neighbors
  neighbors <- function(x) sf::st_touches(x)
  
  repeat {
    
    sf_cty$area_km2 <- as.numeric(sf::st_area(sf_cty)) / 1e6
    total_area      <- sum(sf_cty$area_km2)
    k_target        <- max(1L, round(total_area / target_unit_area_km2))
    
    ## Stop if both conditions satisfied
    if (nrow(sf_cty) <= k_target && all(sf_cty$area_km2 >= min_unit_area_km2)) break
    
    nb    <- neighbors(sf_cty)
    areas <- sf_cty$area_km2
    
    ## Pick the smallest region to merge
    i    <- which.min(areas)
    nbrs <- nb[[i]]
    
    ## If isolated (rare), merge with nearest geometry by centroid distance
    if (length(nbrs) == 0) {
      ci   <- sf::st_centroid(sf_cty$geometry[i])
      d    <- sf::st_distance(ci, sf_cty$geometry)[, 1] |> as.numeric()
      d[i] <- Inf
      j    <- which.min(d)
    } else {
      ## Choose the neighbor with largest area (fast + usually compact)
      j    <- nbrs[ which.max(areas[nbrs]) ]
    }
    
    # Merge i into j
    new_geom  <- sf::st_union(sf_cty$geometry[j], sf_cty$geometry[i])
    new_name  <- paste(sf_cty$region[j], sf_cty$region[i], sep = " + ")
    
    sf_cty$geometry[j] <- new_geom
    sf_cty$region[j]   <- new_name
    
    ## Drop i
    sf_cty <- sf_cty[-i, ]
    sf_cty <- vfix(sf_cty) # keep valid
    
  }
  
  ## Dissolve duplicates if any residual overlaps
  sf_cty %>%
    group_by(country, region) %>%
    summarise(geometry = sf::st_union(geometry), .groups = "drop") %>%
    vfix()
  
}

## Apply the reduction per country
reduce_regions_by_country <- function(regions_eq, target_unit_area_km2 = 15000, min_unit_area_km2 = 8000) {
  regions_eq %>%
    group_split(country, .keep = TRUE) %>%
    map_dfr(~reduce_country_units(.x,
                                  target_unit_area_km2 = target_unit_area_km2,
                                  min_unit_area_km2   = min_unit_area_km2))
}

## Area-balanced contiguous clustering with rgeoda functions
area_clusters_rgeoda      <- function(regions_eq, k, tol = 0.3, seed = 10001) {
  
  set.seed(seed)
  
  ## Contiguity weights directly from sf
  w <- rgeoda::queen_weights(regions_eq)
  
  ## Simple centroid matrix for shape spatial closeness 
  xy <- sf::st_coordinates(sf::st_centroid(regions_eq))
  
  ## Do the clustering
  azp <- try(
    rgeoda::azp_tabu(
        p              = k
      , w              = w
      , df             = data.frame(x = xy[, 1], y = xy[, 2])
      , bound_variable = data.frame(area_km2 = regions_eq$area_km2)
      , min_bound      = (sum(regions_eq$area_km2, na.rm = TRUE) / k) * (1 - tol)
      , scale_method   = "raw"
    )
    , silent = TRUE
  )
  
  return(as.integer(azp$Clusters))
  
}

## Build contiguous clusters by total area
make_area_clusters        <- function(
    sf_list
  , path_to_joined_regions    = "data/joined_Africa_regions.Rds"
  , path_to_collapsed_regions = "data/reduced_Africa_regions.Rds"
  , path_to_clustered_regions = "data/clustered_Africa_regions.Rds"
  , k
  , tol                       = 0.30
  , seed                      = 10001
  , min_area_km2              = 5
    ) {
  
  if (!file.exists(path_to_joined_regions)) {
    
    ## Combine list of country regions
    regions      <- build_regions_sf(sf_list)
    
    ## Tidy up regions
    regions_eq   <- prep_equal_area(regions, min_area_km2 = min_area_km2)
    
    saveRDS(regions_eq, path_to_joined_regions)
    
  } else {
    regions_eq <- readRDS(path_to_joined_regions)
  }
  
  if (!file.exists(path_to_joined_regions)) {
    
    ## Collapse within-country regions so that within-country regions are *vaguely* comparable
    regions_eq.r <- reduce_regions_by_country(regions_eq)
    
    saveRDS(regions_eq.r, path_to_collapsed_regions)
    
  } else {
    regions_eq.r <- readRDS(path_to_collapsed_regions)
  }
  
  regions_eq.r <- regions_eq.r %>% 
    mutate(area_km2 = 0, regions = list(rep(character(1), nrow(.))))

  if (!file.exists(path_to_clustered_regions)) {
    
    for (z in 1:nrow(regions_eq.r)) {
      region_chunk      <- strsplit(regions_eq.r[z, ]$region, " [+] ") %>% unlist()
      regions_eq.r[z, ]$regions  <- region_chunk %>% list()
      regions_eq.r[z, ]$area_km2 <- regions_eq %>% 
        filter(
          country %in% regions_eq.r[z, ]$country
          , region %in% region_chunk
        ) %>% pull(area_km2) %>% sum()
    }
    
    ## Split up Africa into k groups with rgeoda
    cluster_id   <- area_clusters_rgeoda(regions_eq.r, k = k, tol = tol, seed = seed)
    
    ## Add the cluster id to the regions
    regions_eq.r$cluster_id <- cluster_id$Clusters
    
    clustered_regions <- regions_eq.r
    
    saveRDS(clustered_regions, path_to_clustered_regions)
    
  } else {
    clustered_regions <- readRDS(path_to_clustered_regions)
  }
    
  clustered_regions <- clustered_regions %>% 
    rowwise() %>% 
    mutate(cluster = sample(seq(1, k), 1)) %>% 
    ungroup()
  
  regions_eq <- regions_eq %>% mutate(cluster = 0)
  
  for (i in 1:nrow(clustered_regions)) {
    for (j in seq_along(clustered_regions$regions[[i]])) {
      regions_eq[regions_eq$region == clustered_regions$regions[[i]][j], ]$cluster <- clustered_regions[i, ]$cluster
    }
  }
  
  return(regions_eq)
  
}


#### Used in examine_fit.R --------------------------------------------------------

combine_africa_sf  <- function(sf_list) {
  
  africa_sf <- do.call(rbind, sf_list)  
  
  africa_sf %<>%
    transmute(
      country = shapeGroup
      , region  = shapeName
      , geometry = geometry
    ) %>%
    sf::st_make_valid()
  
  orig_crs <- sf::st_crs(africa_sf)
  
  africa_sf %<>%
    vfix() %>%
    ## Web Mercator; fine for area ballpark
    sf::st_transform(3857) %>%
    sf::st_cast("POLYGON", warn = FALSE) %>%
    mutate(area_km2 = as.numeric(sf::st_area(geometry)) / 1e6) %>%
    filter(area_km2 >= 5) %>%                 
    group_by(country, region) %>%
    summarise(geometry = sf::st_union(geometry), .groups = "drop") %>%
    vfix()
  
  rmapshaper::ms_simplify(
    africa_sf
    , keep = 0.05
    , keep_shapes = TRUE
  ) %>% sf::st_transform(orig_crs)
  
}
vfix               <- function(x) {
  x <- sf::st_make_valid(x)
  x <- sf::st_collection_extract(x, "POLYGON")
  x
}
norm_key           <- function(x) {
  x %>%
    iconv(to = "ASCII//TRANSLIT") %>%
    str_to_lower() %>%
    str_squish()
}
prep_preds_for_map <- function(preds_all
                               , prob_col = ".pred_1"
                               , country_col = "Country"
                               , region_col = "shapeName"
) {
  preds_all %>%
    mutate(
      country_norm = norm_key(.data[[country_col]])
    , region_norm  = norm_key(.data[[region_col]])
    ) %>%
    group_by(country_norm, region_norm) %>%
    summarize(
      prob_pred = 1 - prod(1 - get(prob_col))
    , true_out  = max(outbreak)
    ) %>%
    ungroup() 
}

