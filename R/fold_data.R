#' Generate folds for model tuning. Do so in a nested way so that data folds are
#' created across both time in an expanding window and space considering the proximity
#' of Sub regions (e.g., districts)
#'
#'
#' @title fold_data

#' @param data Data for the region of interest. Final output of rvf_data_processing_targets.R
#' @param sf_districts Shape file for the sub regions within the region of interest
#' @param start_date Beginning date for which CV folds will be generated
#' @param end_date Last date of the training data set (all data beyond this date will be set aside for final model evaluation)
#' @param forecast_horizon Period into the future over which evaluation will occur
#' @param step_size Over what period to expand the expanding window of temporal folds
#' @param n_spatial_folds How many spatial clusters of sub-regions to generate for sptail folds
#' @param district_id_col What column from sf_districts to save for ID purposes of sub-regions. Must match what was used to build the data in rvf_data_processing_targets.R 
#' @param seed A seed
#' @return Tibble of folds
#' @author Morgan Kain
#' @export

fold_data <- function(
    data
  , sf_districts
  , start_date       = "2005-04-07"
  , end_date         = "2020-12-19"
  , forecast_horizon = 90
  , step_size        = 90
  , n_spatial_folds  = 10
  , district_id_col  = "shapeName"
  , seed             = 10001
  ) {
  
  set.seed(seed)
  
  ## Ensure date column is Date type
  data <- data %>% mutate(date = as.Date(date))
  
  ## Generate fold start dates
  fold_starts <- seq.Date(
    from = as.Date(start_date)
  , to   = as.Date(end_date)
  , by   = paste(step_size, "days")
  )
  
  outer_folds <- map_df(seq_along(fold_starts), function(i) {
    
    train_end    <- fold_starts[i]
    assess_start <- train_end + 1
    assess_end   <- train_end + forecast_horizon
    
    tibble(
      fold_id = i
    , train_data = list(data %>% filter(date <= train_end))
    , assess_data = list(data %>% filter(date >= assess_start & date <= assess_end))
    , train_range = paste(min(data$date), train_end)
    , assess_range = paste(assess_start, assess_end)
    )
  })
  
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
          dplyr::select(-cluster)
        
        ## Inner assess data: only the left-out cluster
        assess_inner <- train_df %>%
          dplyr::left_join(sf_districts %>% dplyr::select(!!district_id_col, cluster), by = district_id_col) %>%
          dplyr::filter(cluster == clust) %>%
          dplyr::select(-cluster)
        
        tibble(
          inner_fold_id = clust
        , train_inner = list(train_inner)
        , assess_inner = list(assess_inner)
        )
        
      }) %>% dplyr::bind_rows()
      
    }))
  
  return(outer_folds)
  
}
