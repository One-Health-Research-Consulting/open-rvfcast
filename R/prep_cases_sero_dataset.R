#' Prepare data for fitting the sero ~ cases kernel model

#' @title prep_cases_sero_dataset

#' @param sero_dat serology data 
#' @param cases_dat cases data
#' @param map_dat spatial groupings
#' @return tibble of data
#' @author Morgan Kain
#' @export

prep_cases_sero_dataset <- function(sero_dat, cases_dat, map_dat) {

  ## Prep sero data (join into H3 hexes)
  sero_df <- sero_dat %>% 
    st_as_sf(., coords = c("lon", "lat"), crs = st_crs(map_dat[[1]])) %>%
    st_join(., map_dat[[1]], left = FALSE) %>%
    left_join(sero_dat) %>% 
    rename(year = Year, h3_id = shapeName, y = N_pos) %>%
    filter(N > 0)

  ## Prep case data (join into H3 hexes)
  cases_sf <- cases_dat %>%
    dplyr::select(start_date, latitude, longitude) %>%
    st_as_sf(., coords = c("longitude", "latitude"), crs = st_crs(map_dat[[1]])) %>%
    st_join(., map_dat[[1]], left = FALSE) %>%
    distinct() %>%
    rename(date = start_date, h3_id = shapeName)
  
  ## Build adjacency graph and determine membership into adjacent clusters
  adj     <- build_h3_edges_h3jsr(map_dat[[1]] %>% rename(h3_id = shapeName) %>% pull(h3_id))
  g       <- igraph::graph_from_edgelist(cbind(adj$edge_u, adj$edge_v), directed = FALSE)
  comp    <- igraph::components(g)
  C       <- comp$no
  comp_id <- comp$membership 
  
  ## NOTE: To be fixed eventually! Issues with fitting with all of the small islands, so dropping
   ## all but the mainland and MDA
  drop_ids <- map_dat[[1]] %>% 
    rename(h3_id = shapeName) %>%
    mutate(hexgroup = comp_id) %>% 
    filter(hexgroup %notin% c(1, 2)) %>%
    pull(h3_id)
  
  sero_df <- sero_df %>% filter(h3_id %notin% drop_ids)
  
  map_dat.reduced <- map_dat[[1]] %>% 
    rename(h3_id = shapeName) %>%
    mutate(hexgroup = comp_id) %>% 
    filter(hexgroup %in% c(1, 2))
  
  ## Redo with smaller data set
  adj     <- build_h3_edges_h3jsr(map_dat.reduced %>% pull(h3_id))
  g       <- igraph::graph_from_edgelist(cbind(adj$edge_u, adj$edge_v), directed = FALSE)
  comp    <- igraph::components(g)
  C       <- comp$no
  comp_id <- comp$membership 
  
  ## Minor bit of cleanup
  cases_dat.c <- cases_dat %>% mutate(year = year(start_date)) %>% 
    rename(lat = latitude, lon = longitude, Cases = cases)
  
  ## Build the stan data
  res <- make_stan_data_from_real_h3_support(
    sero_df        = sero_df
  , outbreak_df    = cases_dat.c
  , support_hex_df = map_dat.reduced
  , stripping_down = TRUE
  , use_magnitude  = FALSE
  , R_km           = 1000
  , L_years        = 5
  )
  
  ## Drop all seroprevalence values with no linked epidemics, with these very loose criteria
  sero_df.r <- sero_df %>% 
    mutate(i = seq(n())) %>%
    left_join(
      .
      , res %>% group_by(i) %>% summarize(n_linked = n())
    ) %>% filter(!is.na(n_linked))
  
  res <- make_stan_data_from_real_h3_support(
      sero_df = sero_df.r
    , outbreak_df = cases_dat.c
    , support_hex_df = map_dat.reduced
    , use_magnitude  = FALSE
    , R_km = 1000
    , L_years = 5
  )
  
  stan_data <- c(res, C = C %>% list(), comp_id = comp_id %>% list())
  
  check_list <- c(
    length(stan_data$comp_id) == stan_data$G
  , max(stan_data$edge_u) <= stan_data$G
  , max(stan_data$edge_v) <= stan_data$G
  , max(stan_data$cell)   <= stan_data$G
  , min(stan_data$cell)   >= 1
  , min(stan_data$comp_id) >= 1
  , max(stan_data$comp_id) == stan_data$C
  , all(stan_data$comp_id[stan_data$edge_u] == stan_data$comp_id[stan_data$edge_v])
  )
  
  if (any(!check_list)) {stop("Some indices are not aligning, debug in and check")}
  
  return(
    tibble(
      stan_data = stan_data %>% list()
    , cases_sf  = cases_sf %>% list()
    , map_data  = map_dat.reduced %>% as.data.frame() %>% dplyr::select(-geometry) %>%
       mutate(idx = seq(n())) %>% list()
    )
  )
    
}
  

#' Series of helper functions

## Get edge adjacencies 
build_h3_edges_h3jsr <- function(h3_ids_support) {
  
  h3_ids_support <- unique(as.character(h3_ids_support))
  
  ## Validate
  ok <- h3jsr::is_valid(h3_ids_support)
  if (!all(ok)) stop("Invalid H3 IDs in support set. Example: ", h3_ids_support[which(!ok)[1]])
  
  ## check clusters and get numeric ids
  G         <- length(h3_ids_support)
  id_to_idx <- setNames(seq_len(G), h3_ids_support)
  
  ## Vectorized disk query: returns a list aligned to h3_ids_support
  disks <- h3jsr::get_disk(h3_address = h3_ids_support, ring_size = 1, simple = TRUE)
  
  edge_u <- integer(0)
  edge_v <- integer(0)
  
  ## Neighbors within a distance
  for (k in seq_along(h3_ids_support)) {
    id <- h3_ids_support[k]
    ## character vector of neighbors incl self
    nb <- disks[[k]]         
    ## drop self
    nb <- nb[nb != id]            
    ## restrict to support 
    nb <- nb[nb %in% h3_ids_support]  
    
    if (length(nb) == 0) next
    
    u <- id_to_idx[[id]]
    v <- unname(id_to_idx[nb])
    
    edge_u <- c(edge_u, rep.int(u, length(v)))
    edge_v <- c(edge_v, v)
  }
  
  ## Keep each undirected edge once
  keep  <- edge_u < edge_v
  edge_u <- edge_u[keep]
  edge_v <- edge_v[keep]
  
  ## Deduplicate
  key    <- paste(edge_u, edge_v, sep = "_")
  dedup  <- !duplicated(key)
  edge_u <- edge_u[dedup]
  edge_v <- edge_v[dedup]
  
  return(
    list(
      G         = G
    , E         = length(edge_u)
    , edge_u    = edge_u
    , edge_v    = edge_v
    , id_to_idx = id_to_idx
    )
  )
  
}

## Build the stan model list
make_stan_data_from_real_h3_support <- function(
    sero_df
  , outbreak_df
  , support_hex_df
  , stripping_down = FALSE
  , h3_col_sero    = "h3_id"
  , h3_col_support = "h3_id"
  , sero_lat       = "lat"
  , sero_lon       = "lon"
  , sero_n         = "N"
  , sero_y         = "y"
  , sero_t         = "year"
  , ob_lat         = "lat"
  , ob_lon         = "lon"
  , ob_mag         = "Cases"
  , ob_t           = "year"
  , R_km           = 500   ## max distance for building case sero pairs
  , L_years        = 8     ## max years for building case sero paris
  , use_magnitude  = FALSE
  , cases_na_to    = 1
  , crs_in         = 4326
  , crs_projected  = 3857
) {
  
  ## Bit of cleanup
  sero <- sero_df %>%
    transmute(
      lat = .data[[sero_lat]]
    , lon = .data[[sero_lon]]
    , n   = as.integer(.data[[sero_n]])
    , y   = as.integer(.data[[sero_y]])
    , t   = as.numeric(.data[[sero_t]])
    , h3  = as.character(.data[[h3_col_sero]]))
  
  ## Some checks
  if (anyNA(sero$h3)) stop("sero_df has NA H3 IDs.")
  if (any(sero$y > sero$n)) stop("Found y > N in serology data.")
  
  ## Outbreaks
  ob <- outbreak_df %>%
    transmute(
      lat     = .data[[ob_lat]]
    , lon     = .data[[ob_lon]]
    , t       = as.numeric(.data[[ob_t]])
    , mag_raw = .data[[ob_mag]])
  
  ## Some extra steps if using size of outbreaks
  if (use_magnitude) {
    mag             <- suppressWarnings(as.numeric(ob$mag_raw))
    mag[is.na(mag)] <- cases_na_to
    mag[mag < 0]    <- cases_na_to
    ob$mag          <- mag
  } else {
    ob$mag          <- 1.0
  }
  
  ## Projected coords for distance in km 
  sero_sf     <- st_as_sf(sero, coords = c("lon", "lat"), crs = crs_in)
  ob_sf       <- st_as_sf(ob,   coords = c("lon", "lat"), crs = crs_in)
  sero_xy     <- st_transform(sero_sf, crs_projected)
  ob_xy       <- st_transform(ob_sf,   crs_projected)
  sero_coords <- st_coordinates(sero_xy)
  ob_coords   <- st_coordinates(ob_xy)
  sero        <- sero %>% mutate(x = sero_coords[, 1] / 1000, yxy = sero_coords[, 2] / 1000)
  ob          <- ob   %>% mutate(x = ob_coords[, 1] / 1000,   yxy = ob_coords[, 2] / 1000)
  
  N <- nrow(sero)
  
  ## Complete set of all H3 hexes
  support_ids <- unique(as.character(support_hex_df[[h3_col_support]]))
  if (length(support_ids) == 0) stop("support_hex_df has zero support H3 IDs.")
  
  ## Build adjacency on full support
  adj <- build_h3_edges_h3jsr(support_ids)
  
  ## Map sero cells to support indices
  sero$cell <- map_sero_cells_to_support(sero$h3, adj$id_to_idx)
  
  ## Sparse outbreak pairs
  ord_t      <- order(ob$t)
  ob_sorted  <- ob[ord_t, ]
  ot_sorted  <- ob_sorted$t
  pairs_list <- vector("list", N)
  
  ## Fill out sparse pairs
  for (i in 1:N) {
    ti <- sero$t[i]
    lo <- ti - L_years
    hi <- ti
    
    idx_lo <- findInterval(lo, ot_sorted) + 1L
    idx_hi <- findInterval(hi, ot_sorted)
    
    if (idx_lo <= idx_hi) {
      cand <- ob_sorted[idx_lo:idx_hi, ]
      dx   <- sero$x[i] - cand$x
      dy   <- sero$yxy[i] - cand$yxy
      d    <- sqrt(dx*dx + dy*dy)
      
      keep <- which(d <= R_km & (ti - cand$t) > 0)
      
      if (length(keep) > 0) {
        
        cand2 <- cand[keep, ]
        pairs_list[[i]] <- data.frame(
          i    = i
        , dist = d[keep]
        , dt   = ti - cand2$t
        , mag  = cand2$mag)
        
      }
    }
  }
  
  ## cleanup
  pairs <- bind_rows(pairs_list)
  if (nrow(pairs) == 0) {
    pairs <- data.frame(i = integer(0), dist = numeric(0), dt = numeric(0), mag = numeric(0))
  } else {
    pairs <- pairs %>% arrange(i)
  }
  if (stripping_down) {return(pairs)}
  K <- nrow(pairs)
  
  ## Build the indexing ragged array vector for speedy computation
  start <- integer(N + 1)
  start[1] <- 1L
  if (K == 0) {
    start[2:(N+1)] <- 1L
  } else {
    counts <- tabulate(pairs$i, nbins = N)
    start[2:(N+1)] <- 1L + cumsum(counts)
  }
  
  return(
    list(
      N      = N
    , y      = sero$y
    , n      = sero$n
    , cell   = sero$cell
    , G      = adj$G
    , E      = adj$E
    , edge_u = adj$edge_u
    , edge_v = adj$edge_v
    , K      = K
    , start  = start
    , dist   = if (K > 0) pairs$dist else numeric(0)
    , dt     = if (K > 0) pairs$dt   else numeric(0)
    , mag    = if (K > 0) pairs$mag  else numeric(0)
    )
  )
  
}

## Clean up cell ids / check for issues
map_sero_cells_to_support <- function(sero_h3_ids, id_to_idx) {
  sero_h3_ids <- as.character(sero_h3_ids)
  miss        <- setdiff(unique(sero_h3_ids), names(id_to_idx))
  if (length(miss) > 0) {
    stop("Some serology H3 IDs are not in the support set. Example: ", miss[1])
  }
  unname(id_to_idx[sero_h3_ids])
}
