####
## Large series of spatial helper functions to assist with region clustering
## for spatial inner folds and plotting
####

#### Used in make_area_clusters( ) for fold_data.R --------------------------------------------------

## Build contiguous clusters by total area
make_area_clusters        <- function(
    sf_list
  , using_hexes               = FALSE
  , path_to_joined_regions    = "data/joined_Africa_regions.Rds"
  , path_to_collapsed_regions = "data/reduced_Africa_regions.Rds"
  , path_to_region_neighbors  = "data/Africa_region_neighbors.Rds"
  , path_to_clustered_regions = "data/clustered_Africa_regions.Rds"
  , k
  ## Weight placed on distance relative to area. Very finnicy. Need a *tiny* value to
   ## put more weight on area added then distance. The more weight on area the more meandering
   ## the regions. The more weight on distance the more blocky the regions
  , tol                       = 0.30
  ## three options for choosing the next region to add
  ## "balanced" (using both area of the new region -- focus on adding small regions over larger first 
  ## and distance from the entire grown region to that point and the next region)
  ## "distance" (using only distance)
  ## "random" (add a random region next)
  , growth_option             = "distance"
  , seed                      = 10001
  , overwrite                 = FALSE
    ) {
  
  if (!using_hexes) {
    
  ## Build or load single joined feature of all regions in the broadest target area
  if (file.exists(path_to_joined_regions) & !overwrite) {
    ## Load if previously saved
    regions_eq <- readRDS(path_to_joined_regions)
  } else {
    ## Combine list of country regions
    regions      <- build_regions_sf(sf_list)
    ## Tidy up regions
    regions_eq   <- prep_equal_area(regions)
    ## Save for future use
    saveRDS(regions_eq, path_to_joined_regions)
  }
  
  ## Build or load simplified feature set (collapsing/joining small nearby regions)
  if (file.exists(path_to_collapsed_regions) & !overwrite) {
    ## Load or previously saved
    regions_eq.r <- readRDS(path_to_collapsed_regions)
  } else {
    ## Collapse within-country regions so that within-country regions are *vaguely* comparable
    regions_eq.r <- reduce_regions_by_country(regions_eq)
    ## Save for future use
    saveRDS(regions_eq.r, path_to_collapsed_regions)
  }
  
  ## Add row for tidying up the region names that make up the broader region
  regions_eq.r <- regions_eq.r %>% mutate(regions = list(rep(character(1), nrow(.))))
  
  } else {
    
  regions_eq.r <- sf_list[[1]]
    
  }
  
  ## Build or load how these regions have been clustered for inner folds
  if (file.exists(path_to_clustered_regions) & !overwrite) {
    ## Load if already run and saved
    clusts.t <- readRDS(path_to_clustered_regions)
  } else {
    
    ## Do the clustering
    clusts.t <- make_k_contiguous_area_clusters(
      regions_eq.r, K = k, alpha = tol, seed = seed
    , path_to_region_neighbors = path_to_region_neighbors
    , growth_option = growth_option
    )
    
    if (!using_hexes) {
    
    ## Tidy region names
    for (z in 1:nrow(clusts.t)) {
      region_chunk           <- strsplit(clusts.t[z, ]$region, " [+] ") %>% unlist()
      clusts.t[z, ]$regions  <- region_chunk %>% list()
    }
      
    }
    
    ## Save for future use
    saveRDS(clusts.t, path_to_clustered_regions)
    
  }
    
  ## Figure out the cluster of the ungrouped regions
  if (!using_hexes) {
  
  regions_eq <- regions_eq %>% mutate(cluster = 0)
  
  for (i in 1:nrow(clusts.t)) {
    for (j in seq_along(clusts.t$regions[[i]])) {
      regions_eq[regions_eq$region == clusts.t$regions[[i]][j], ]$cluster <- clusts.t[i, ]$cluster
    }
  }
  
  return(regions_eq)
  
  } else {
    
  return(
    clusts.t %>% 
      mutate(
        regions = NA, country = NA
        ## not actually area in km2, but not using this anyway for hexes
         ## so just putting this in here so the rest of the downstream code
         ## works the same as for ADM2
      , area_km2 = log(area)
      , .after = "shapeName"
      ) %>% rename(region = shapeName)
  )
    
  }
  
}

####
## Large series of helpers used in the above function, listed in the approximate order as they
## show up in the above function
####

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

## Project to equal-area, compute areas (km^2)
prep_equal_area      <- function(regions) {
  
  ## Equal-area CRS (Cylindrical Equal Area)
  cea        <- "+proj=cea +lon_0=0 +lat_ts=0 +datum=WGS84 +units=m +no_defs"
  regions_eq <- regions %>% sf::st_transform(cea) %>% vfix()
  
  ## Cast, compute area, filter small bits, dissolve back per (country, region)
  parts          <- suppressWarnings(sf::st_cast(regions_eq, "POLYGON"))
  parts$area_km2 <- as.numeric(sf::st_area(parts)) / 1e6
  
  regions_filtered <- parts %>%
   # dplyr::filter(area_km2 >= min_area_km2) %>%
    dplyr::group_by(country, region) %>%
    dplyr::summarise(geometry = sf::st_union(.data$geometry), .groups = "drop") %>%
    vfix()
  
  regions_filtered$area_km2 <- as.numeric(sf::st_area(regions_filtered)) / 1e6
  
  return(regions_filtered)
  
}

## Collapse within-country regions so that within-country regions are *vaguely* comparable
## in size across countries to speed up the spatial folding across all of Africa
reduce_country_units <- function(sf_cty, target_unit_area_km2, min_unit_area_km2) {
  
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
    sf_cty <- vfix(sf_cty)
    
  }
  
  ## Dissolve duplicates if any residual overlaps
  sf_cty.f <- sf_cty %>%
    group_by(country, region) %>%
    summarise(geometry = sf::st_union(geometry), .groups = "drop") %>%
    vfix()
  
  print(sf_cty.f[1, ]$region)
  
  return(sf_cty.f)
  
}

## Apply the reduction per country
reduce_regions_by_country <- function(regions_eq, target_unit_area_km2, min_unit_area_km2) {
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

## Wrapper for custom function to build regions by selecting seeds across Africa and then growing the regions
 ## by accumulating nearby regions
make_k_contiguous_area_clusters <- function(sf_in, K, alpha, seed, path_to_region_neighbors, growth_option) {
  
  ## Some cleanup of shapes
  eq         <- prep_shapes(sf_in)
  
  ## See note above wrapper function and for this function
  eq$cluster <- partition_by_area(
    sf_eq = eq, K = K
  , alpha = alpha, seed = seed
  , growth_option = growth_option
  , path_to_region_neighbors = path_to_region_neighbors
  )
  
  # return with original attributes + cluster_id in WGS84 for joins/plots
  out <- sf::st_transform(eq, 4326)
  
  return(out)
  
}

## Clean & prep grouped / collapsed region map
prep_shapes <- function(sf_regions, cea = "+proj=cea +lon_0=0 +lat_ts=0 +datum=WGS84 +units=m +no_defs") {
  
  ## Turn of s2 to better deal with some overlapping edges and such, make this slightly less precise but fine
  sf_use_s2(FALSE)
  
  ## Some cleanup of area shapes
  x <- sf_regions %>%
    sf::st_transform(cea) %>%
    sf::st_make_valid() %>%
    suppressWarnings(sf::st_collection_extract("POLYGON")) %>%
    filter(!sf::st_is_empty(geometry))
  x$area <- as.numeric(sf::st_area(x)) # m^2
  xy     <- sf::st_coordinates(sf::st_centroid(x))
  x$cx   <- xy[, 1]
  x$cy   <- xy[, 2]
  
  return(x)
  
}

## Grow areas adhering to contiguity + area balance + compactness (but see growth_options)
partition_by_area <- function(sf_eq, K, alpha, seed, growth_option, path_to_region_neighbors) {
  
  set.seed(seed)
  
  n  <- nrow(sf_eq)
  A  <- sf_eq$area
  Cx <- sf_eq$cx
  Cy <- sf_eq$cy
  
  if(!file.exists(path_to_region_neighbors)) {
    nb_info <- build_neighbors_with_tolerance(sf_eq, tol_m = 500)
    saveRDS(nb_info, path_to_region_neighbors)
  } else {
    nb_info <- readRDS(path_to_region_neighbors)
  }
  
  nb   <- nb_info$nb
  comp <- nb_info$comp
  
  ## precompute centroid distance matrix (Euclidean in meters)
  D <- as.matrix(dist(cbind(Cx, Cy), method = "euclidean"))
  
  totalA <- sum(A)
  target <- totalA / K
  
  ## seed allocation and selection (where the regions will begin) taking a global approach
  seeds_per_comp <- allocate_seeds_per_component(A, comp, K)
  seeds          <- pick_global_seeds(Cx, Cy, K, A)
  seeds          <- enforce_exact_K(seeds, K, Cx, Cy, n)
  
  ## Some cleanup if issues above
  seeds  <- unique(seeds)
  if (length(seeds) != K) {
    extra <- setdiff(seq_len(n), seeds)[seq_len(K - length(seeds))]
    seeds <- c(seeds, extra)
  }
  
  cl    <- rep(NA_integer_, n)
  Acl   <- rep(0, K)
  Ccl_x <- rep(0, K)
  Ccl_y <- rep(0, K)
  
  ## Initialize clusters with seeds
  for (k in seq_len(K)) {
    i        <- seeds[k]
    cl[i]    <- k
    Acl[k]   <- A[i]
    Ccl_x[k] <- Cx[i]
    Ccl_y[k] <- Cy[i]
  }
  
  ## Frontier for cluster k (unassigned neighbors of assigned members)
  frontier <- function(k) {
    mem <- which(cl == k)
    if (!length(mem)) return(integer(0))
    cand <- unique(unlist(nb[mem]))
    cand <- cand[is.na(cl[cand])]
    cand
  }
  
  ## Loop for growth of regions around the seed kernels
  remaining <- which(is.na(cl))
  iter <- 0L
  while (length(remaining) > 0L) {
    progressed <- FALSE
    for (k in seq_len(K)) {
      cand <- frontier(k)
      if (!length(cand)) next
      ## Score: area balance + compactness
      ## Balance term: |(Acl + a) - target|
      ## Compactness term: alpha * distance to cluster centroid
      dc    <- sqrt((Cx[cand] - Ccl_x[k])^2 + (Cy[cand] - Ccl_y[k])^2)
      score <- log(abs((Acl[k] + A[cand]) - target)) + alpha * dc
      if (growth_option == "balanced") {
        j     <- cand[which.min(score)]
      } else if (growth_option == "distance") {
        j     <- cand[which.min(dc)]
      } else if (growth_option == "random") {
        j     <- cand[sample(seq(length(score)), 1)]
      } else {
        stop("choose balanced, distance, or random")
      }
      
      
      ## assign j to cluster k
      cl[j]  <- k
      
      ## update cluster centroid as area-weighted mean (no st_union!)
      Acl[k]   <- Acl[k] + A[j]
      Ccl_x[k] <- (Ccl_x[k] * (Acl[k] - A[j]) + Cx[j] * A[j]) / Acl[k]
      Ccl_y[k] <- (Ccl_y[k] * (Acl[k] - A[j]) + Cy[j] * A[j]) / Acl[k]
      
      progressed <- TRUE
      
    }
    
    ## If no frontier grew (disconnected leftover), attach the leftover node
    if (!progressed) {
      
      ## ensure contiguity: pick a leftover node that touches some cluster,
      ## then attach to the touching cluster with best score
      left     <- which(is.na(cl))
      attached <- FALSE
      
      for (j in left) {
        neigh          <- nb[[j]]
        neigh_assigned <- neigh[!is.na(cl[neigh])]
        
        if (!length(neigh_assigned)) next
        k_neigh <- unique(cl[neigh_assigned])
        
        ## choose cluster among neighbors minimizing score
        sc <- sapply(k_neigh, function(k) {
          dc <- sqrt((Cx[j] - Ccl_x[k])^2 + (Cy[j] - Ccl_y[k])^2)
          abs((Acl[k] + A[j]) - target) + alpha * dc
        })
        
        kbest        <- k_neigh[which.min(sc)]
        cl[j]        <- kbest
        Acl[kbest]   <- Acl[kbest] + A[j]
        Ccl_x[kbest] <- (Ccl_x[kbest] * (Acl[kbest] - A[j]) + Cx[j] * A[j]) / Acl[kbest]
        Ccl_y[kbest] <- (Ccl_y[kbest] * (Acl[kbest] - A[j]) + Cy[j] * A[j]) / Acl[kbest]
        
        attached     <- TRUE
        
      }
      
      if (!attached) {
        
        ## final safety: assign any remaining by nearest cluster (rare; may break contiguity if graph truly disconnected from all seeds)
        for (j in which(is.na(cl))) {
          kbest        <- which.min(sqrt((Cx[j]-Ccl_x)^2 + (Cy[j]-Ccl_y)^2))
          cl[j]        <- kbest
          Acl[kbest]   <- Acl[kbest] + A[j]
          Ccl_x[kbest] <- (Ccl_x[kbest] * (Acl[kbest] - A[j]) + Cx[j] * A[j]) / Acl[kbest]
          Ccl_y[kbest] <- (Ccl_y[kbest] * (Acl[kbest] - A[j]) + Cy[j] * A[j]) / Acl[kbest]
        }
      }
    }
    
    remaining <- which(is.na(cl))
    iter      <- iter + 1L
    if (iter > 1e6) stop("Loop runaway (should never happen).")
    
  }
  
  return(cl)
  
}

## Determine what regions each region borders, allowing for a buffer given slight mismatches at country borders
build_neighbors_with_tolerance <- function(x, tol_m = 500) {
  
  ## Grab strict shared-edge neighbors (works best within-country)
  nb_touch <- sf::st_touches(x) 
  
  ## Distance-based neighbors to bridge micro-gaps that often arise between countries
  ## Loop over actually seems faster than sending in the whole object for some reason.
  ## This is quite slow, so saving it -- NOTE: need to update the pipeline to dynamically look
  ## to make sure this is updated correctly to match the rest of the objects
  nb_near <- vector("list", nrow(x))
  for (i in 1:nrow(x)) {
    nb_near[[i]] <- sf::st_is_within_distance(x[i, ], x, dist = tol_m)[[1]]
    nb_near[[i]] <- nb_near[[i]][nb_near[[i]] != i]
  }
  
  ## combine and deduplicate per focal region
  nb <- Map(function(a, b) sort(unique(c(a, b))), nb_touch, nb_near)
  
  ## Remove self references if any crept in
  nb <- lapply(seq_along(nb), function(i) setdiff(nb[[i]], i))
  
  ## graph + components
  g    <- igraph::graph_from_adj_list(nb, mode = "all")
  comp <- igraph::components(g)$membership
  
  return(
    list(
      nb   = nb
      , comp = comp
      , g    = g
    )
  )
  
}

## Allocate seeds proportional to component area
allocate_seeds_per_component <- function(areas, comp, K) {
  comps <- sort(unique(comp))
  n_i   <- as.integer(tabulate(comp, nbins = max(comps)))      # capacity per comp
  A_i   <- tapply(areas, comp, sum)
  prop  <- A_i / sum(A_i)
  
  # start with floor(prop*K), ensure at least 1 if capacity>0, cap by capacity
  alloc <- pmin(n_i, pmax(1L, floor(prop * K)))
  
  # fix to sum exactly K
  adjust <- function(alloc, targetK) {
    diffK <- targetK - sum(alloc)
    if (diffK == 0) return(alloc)
    
    if (diffK > 0) {
      # need to add seeds: add to comps with spare capacity, highest residual first
      resid <- prop * K - alloc
      can_add <- which(alloc < n_i)
      if (length(can_add) == 0) return(alloc)  # no capacity to add
      ord <- order(resid[can_add], decreasing = TRUE)
      i <- 1
      while (sum(alloc) < targetK && i <= length(ord)) {
        idx <- can_add[ord[i]]
        if (alloc[idx] < n_i[idx]) alloc[idx] <- alloc[idx] + 1L
        i <- i + 1
      }
    } else {
      # need to remove seeds: remove from comps with alloc > 1, lowest residual first
      resid <- prop * K - alloc
      can_drop <- which(alloc > 1L)
      if (length(can_drop) == 0) return(alloc)  # can't drop without killing coverage
      ord <- order(resid[can_drop], decreasing = FALSE)
      i <- 1
      while (sum(alloc) > targetK && i <= length(ord)) {
        idx <- can_drop[ord[i]]
        if (alloc[idx] > 1L) alloc[idx] <- alloc[idx] - 1L
        i <- i + 1
      }
    }
    alloc
  }
  
  alloc <- adjust(alloc, K)
  
  # final safeguard: if still off (rare rounding/cap cases), force-fit
  if (sum(alloc) != K) {
    # add to comps with capacity or drop from comps with alloc>1
    if (sum(alloc) < K) {
      missing <- K - sum(alloc)
      can_add <- which(alloc < n_i)
      add_idx <- rep(can_add, length.out = missing)
      alloc[add_idx] <- alloc[add_idx] + 1L
    } else {
      extra <- sum(alloc) - K
      can_drop <- which(alloc > 1L)
      drop_idx <- rep(can_drop, length.out = extra)
      alloc[drop_idx] <- alloc[drop_idx] - 1L
    }
  }
  
  setNames(alloc, names(A_i))
}

## Global seed selection (no per-component minimum)
pick_global_seeds <- function(cx, cy, K, areas) {
  # farthest-point sampling weighted by area (optional)
  # start at the largest area unit to anchor
  first <- which.max(areas)
  seeds <- first
  while (length(seeds) < K) {
    d2 <- vapply(seq_along(cx), function(i) min((cx[i]-cx[seeds])^2 + (cy[i]-cy[seeds])^2), numeric(1))
    d2[seeds] <- -Inf
    seeds <- c(seeds, which.max(d2))
  }
  unique(seeds)
}

## Ensure exactly K seeds overall (if still not exact, nudge by removing/adding)
enforce_exact_K <- function(seeds, K, cx, cy, n, taken = NULL) {
  seeds <- unique(seeds)
  if (length(seeds) == K) return(seeds)
  
  # helper: nearest-neighbor distance among seeds
  nn_dist <- function(S) {
    if (length(S) < 2) return(rep(Inf, length(S)))
    D <- as.matrix(dist(cbind(cx[S], cy[S])))
    diag(D) <- Inf
    apply(D, 1, min)
  }
  
  if (length(seeds) > K) {
    # drop seeds with smallest separation first (keep dispersion)
    while (length(seeds) > K) {
      nd <- nn_dist(seeds)
      drop_one <- which.min(nd)
      seeds <- seeds[-drop_one]
    }
  } else {
    # add seeds far from current seeds
    pool <- setdiff(seq_len(n), seeds)
    while (length(seeds) < K && length(pool) > 0) {
      dmin <- vapply(pool, function(i) min((cx[i]-cx[seeds])^2 + (cy[i]-cy[seeds])^2), numeric(1))
      add_one <- pool[which.max(dmin)]
      seeds <- c(seeds, add_one)
      pool <- setdiff(pool, add_one)
    }
  }
  seeds
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

