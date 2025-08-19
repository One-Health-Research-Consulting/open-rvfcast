#' Generate folds for model tuning. Do so in a nested way so that data folds are
#' created across both time in an expanding window and space considering the proximity
#' of Sub regions (e.g., districts)
#'
#'
#' @title fold_data

#' @param data Data for the region of interest. Final output of rvf_data_processing_targets.R
#' @param type train_data or test_data
#' @param sf_districts Shape file for the sub regions within the region of interest
#' @param assess_time_chunk Total amount of time over which the assessment of model fit occurs
#' @param step_size Over what period to expand the expanding window of temporal folds
#' @param n_spatial_folds How many spatial clusters of sub-regions to generate for sptatial folds
#' @param district_id_col What column from sf_districts to save for ID purposes of sub-regions. Must match what was used to build the data in rvf_data_processing_targets.R 
#' @param seed A seed
#' @return Tibble of folds
#' @author Morgan Kain
#' @export

fold_data <- function(
    data
  , type
  , sf_districts
  , assess_time_chunk = 180
  , step_size         = 90
  , n_spatial_folds   = 10
  , district_id_col   = "shapeName"
  , seed              = 10001
  , ...
  ) {
  
  if (type %notin% c("train_data", "test_data")) {
    stop("Choose train_data or test_data for parameter 'type'")
  }
  
  set.seed(seed)
  
  data <- (data %>% pull(!!type))[[1]]
  
  ## Ensure date column is Date type
  data <- data %>% mutate(date = as.Date(date))
  
  if (type == "train_data") {
  
  ## Beginning date for which CV folds will be generated
  start_date <- min(unique(data$date))
  
  ## Last date of the training data set (all data beyond this date will be set aside for final model evaluation)
  end_date   <- max(unique(data$date))
  
  ## Generate fold start dates
  fold_starts <- seq.Date(
    from = as.Date(start_date)
    , to   = as.Date(end_date)
    , by   = paste(step_size, "days")
  )
  
  outer_folds <- map_df(seq_along(fold_starts), function(i) {
    
    train_end    <- fold_starts[i]
    assess_start <- train_end + 1
    assess_end   <- train_end + assess_time_chunk
    
    
    training  <- data %>% filter(date <= train_end)
    assessing <- data %>% filter(date >= assess_start & date <= assess_end)
    
    tibble(
      outer_fold_id = i
      , train_data    = training %>% list()
      , assess_data   = assessing %>% list()
      , train_range   = paste(min(data$date), train_end)
      , assess_range  = paste(assess_start, assess_end)
    )
    
  })
  
  } else {
    
  start_date <- min(unique(data$date))
  end_date   <- max(unique(data$date)) - step_size
  
  other_parms <- list(...)
  
  ## Generate fold start dates
  fold_ends <- seq.Date(
      from = as.Date(other_parms$holdout_start)
    , to   = as.Date(end_date)
    , by   = paste(step_size, "days")
  )
  
  outer_folds <- map_df(seq_along(fold_ends), function(i) {
    
    train_end    <- fold_ends[i]
    assess_start <- train_end + 1
    assess_end   <- train_end + assess_time_chunk
    
    training  <- data %>% filter(date <= train_end)
    assessing <- data %>% filter(date >= assess_start & date <= assess_end)
    
    tibble(
      outer_fold_id = i
      , train_data    = training %>% list()
      , assess_data   = assessing %>% list()
      , train_range   = paste(min(data$date), train_end)
      , assess_range  = paste(assess_start, assess_end)
    )
    
  })
    
  }
  

  
  if (type == "test_data") {
    return(outer_folds %>% mutate(type = "test_data", .before = 1))
  }
  
  ## Create spatial clusters via k-means on centroids. 
   ## One simple option among many potentially better but more complicated options
   ## for building spatial clusterings of multiple districts
  
  ## Pretty rough strategy here to get clusters of approximate equal size
  
  ## Prepare cluster assignment tracking and such
  coords           <- sf::st_coordinates(sf::st_centroid(sf_districts))
  kmeans_init      <- kmeans(coords, centers = n_spatial_folds)
  n_clusters       <- n_spatial_folds
  n_districts      <- nrow(sf_districts)
  ## Ceiling on cluster size 
  target_size      <- ceiling(n_districts / n_clusters) 
  ## Floor on cluster size
  min_size         <- floor(n_districts / n_clusters)      
  cluster_sizes    <- rep(0, n_clusters)
  district_cluster <- rep(NA_integer_, nrow(sf_districts))
  
  ## Distance matrix: districts × centroids
  dist_matrix <- as.matrix(dist(rbind(coords, kmeans_init$centers)))[
    1:nrow(sf_districts),
    (nrow(sf_districts) + 1):(nrow(sf_districts) + n_clusters)
  ]
  
  ## Order centroids by proximity for each district (force into matrix)
  nearest_order <- t(apply(dist_matrix, 1, function(x) order(x)))
  
  ## PASS 1: Fill each cluster to min_size
  for (cluster in seq_len(n_clusters)) {
    ## Districts not yet assigned, sorted by proximity to this cluster
    unassigned        <- which(is.na(district_cluster))
    order_for_cluster <- order(dist_matrix[unassigned, cluster])
    to_assign         <- head(unassigned[order_for_cluster], min_size)
    
    district_cluster[to_assign] <- cluster
    cluster_sizes[cluster]      <- cluster_sizes[cluster] + length(to_assign)
  }

  ## PASS 2: Assign remaining districts up to target_size
  for (i in which(is.na(district_cluster))) {
    for (j in nearest_order[i, ]) {
      if (cluster_sizes[j]  < target_size) {
        district_cluster[i] <- j
        cluster_sizes[j]    <- cluster_sizes[j] + 1
        break
      }
    }
  }
  
  ## Add cluster assignments back to sf object and strip down the object.
   ## Only need name and what CV fold they will be a part of
  sf_districts <- sf_districts %>% as.data.frame() %>%
    dplyr::select(!!district_id_col) %>%
    mutate(cluster = district_cluster)

  ## Add inner spatial folds
  outer_folds <- outer_folds %>%
    dplyr::mutate(inner_folds = purrr::map(train_data, function(train_df) {
      purrr::map(1:n_spatial_folds, function(clust) {
        
        ## Inner training data: exclude a cluster
        train_inner <- train_df %>%
          dplyr::left_join(sf_districts %>% dplyr::select(!!district_id_col, cluster), by = district_id_col) %>%
          dplyr::filter(cluster != clust) %>%
          relocate(cluster, .after = "date")
        
        ## Inner assess data: only the left-out cluster
        assess_inner <- train_df %>%
          dplyr::left_join(sf_districts %>% dplyr::select(!!district_id_col, cluster), by = district_id_col) %>%
          dplyr::filter(cluster == clust) %>%
          relocate(cluster, .after = "date")
        
        tibble(
          inner_fold_id = clust
        , train_inner   = list(train_inner)
        , assess_inner  = list(assess_inner)
        )
        
      }) %>% dplyr::bind_rows()
      
    }))
  
  return(outer_folds %>% mutate(type = "train_data", .before = 1))
  
}

#' Strip down the folds to folds where we can reasonably expect to "learn" something
#' about model parameters based on data availability
#'
#'
#' @title clean_folded_data

#' @param data Folded data using type = "train_data"
#' @param epidemic_threshold_total threshold number of total epidemics under which we throw out a fold
#' @param epidemic_threshold_space threshold number of the n_spatial_folds with an epidemic under which we throw out a fold
#' @return Tibble of retained folds
#' @author Morgan Kain
#' @export

clean_folded_data <- function (data, epidemic_threshold_total, epidemic_threshold_space) {
  
  ## First, have to elongate the nested tibble of the inner and outer folds
  clean_folded <- lapply(data %>% rowwise() %>% group_split(), FUN = function(x) {
    x$inner_folds[[1]] %>% 
      mutate(
        type            = x$type
      , outer_fold_id   = x$outer_fold_id
      , train_range     = x$train_range
      , assess_range    = x$assess_range
      , .before         = 1
      ) %>% relocate(inner_fold_id, .after = outer_fold_id)
  }) %>% do.call("rbind", .)
  
  reduced_folds <- lapply(clean_folded %>% rowwise() %>% group_split(), FUN = function(x) {
    
    epi_checks <- x$train_inner[[1]] %>% filter(outbreak != 0) %>% 
      group_by(cluster) %>% 
      summarize(n_per_clust = n()) %>% 
      ungroup() %>% 
      summarize(
        total_n = sum(n_per_clust)
      , clust_n = n()
      )
    
    if (epi_checks$total_n > epidemic_threshold_total & epi_checks$clust_n > epidemic_threshold_space) {
      return(x)
    } else {
      return(NULL)
    }
    
  }) %>% do.call("rbind", .)
  
  return(
    list(
      inner_folds = reduced_folds
    , outer_folds = data %>% filter(outer_fold_id %in% unique(reduced_folds$outer_fold_id)) %>% dplyr::select(-contains("inner"))
    )
  )
  
  
}

