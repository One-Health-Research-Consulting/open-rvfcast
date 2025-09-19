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
  , n_spatial_folds   = 15
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
    
    ## Just pull the row index for which entries of the data are used in this fold. The actual data
     ## involved in each fit is subset from the full data at the time of fitting
    training  <- data %>% filter(date <= train_end) %>% pull(index)
    assessing <- data %>% filter(date >= assess_start & date <= assess_end) %>% pull(index)
    
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
    
    training  <- data %>% filter(date <= train_end) %>% pull(index)
    assessing <- data %>% filter(date >= assess_start & date <= assess_end) %>% pull(index)
    
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
  coords           <- lapply(sf_districts, FUN = function(x) {
    sf::sf_use_s2(TRUE)
    try_coords <- try({sf::st_coordinates(sf::st_centroid(x))}, silent = T)
    if (class(try_coords)[1] == "try-error") {
      sf::sf_use_s2(FALSE)
      try_coords <- try({sf::st_coordinates(sf::st_centroid(x))}, silent = T) 
    }
    as.data.frame(x) %>% dplyr::select(shapeGroup, shapeName) %>% 
      cbind(., try_coords)
  }) %>% do.call("rbind", .)
  coords_mat       <- coords %>% dplyr::select(X, Y) %>% as.matrix()
  kmeans_init      <- kmeans(coords_mat, centers = n_spatial_folds)
  n_clusters       <- n_spatial_folds
  n_districts      <- nrow(coords)
  ## Ceiling on cluster size 
  target_size      <- ceiling(n_districts / n_clusters) 
  ## Floor on cluster size
  min_size         <- floor(n_districts / n_clusters)      
  cluster_sizes    <- rep(0, n_clusters)
  district_cluster <- rep(NA_integer_, nrow(coords))
  
  ## Distance matrix: districts × centroids
  dist_matrix <- as.matrix(dist(rbind(coords_mat, kmeans_init$centers)))[
    1:nrow(coords_mat),
    (nrow(coords_mat) + 1):(nrow(coords_mat) + n_clusters)
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
  coords.sorted <- coords %>%
    dplyr::select(-X, -Y) %>%
    mutate(cluster = district_cluster) %>%
    mutate(Country = plyr::mapvalues(shapeGroup, from = unique(shapeGroup), to = unique(data$Country)), .before = 1) %>%
    dplyr::select(-shapeGroup)

  outer_folds <- outer_folds %>% mutate(inner_folds = NA)
  
  ## Add inner spatial folds. Once again, just returning a set of indexes and a column that shows in which of the
   ## inner folds that specific index is actually not included (the one excluded will be the assess data for that
    ## inner fold)
  for (i in 1:nrow(outer_folds)) {
    
    tdat <- data %>% filter(index %in% (outer_folds[i, ] %>% pull(train_data) %>% unlist())) %>% 
      left_join(., coords.sorted, by = c("shapeName", "Country")) %>% 
      relocate(cluster, .after = index)
    
    outer_folds[i, ]$inner_folds <- tdat %>% dplyr::select(index, cluster) %>% list()
    
  }
  
  return(outer_folds %>% mutate(type = "train_data", .before = 1))
  
}


#' Strip down the folds to folds where we can reasonably expect to "learn" something
#' about model parameters based on data availability
#'
#'
#' @title clean_folded_data

#' @param raw_data Full training dataset
#' @param folded_data Folded data using type = "train_data"
#' @param epidemic_threshold_total threshold number of total epidemics under which we throw out a fold
#' @param epidemic_threshold_space threshold number of the n_spatial_folds with an epidemic under which we throw out a fold
#' @return Tibble of retained folds
#' @author Morgan Kain
#' @export

clean_folded_data <- function (raw_data, folded_data, epidemic_threshold_total, epidemic_threshold_space) {
  
  clean_folded <- lapply(folded_data %>% rowwise() %>% group_split(), FUN = function(x) {
    x$inner_folds[[1]] %>% 
      mutate(
        type            = x$type
        , outer_fold_id   = x$outer_fold_id
        , train_range     = x$train_range
        , assess_range    = x$assess_range
        , .before         = 1
      ) %>% left_join(., raw_data[[1]], by = "index") %>%
      group_by(cluster, outer_fold_id) %>%
      summarize(
        has_outbreak = ifelse(any(outbreak == 1), 1, 0)
        , tot_outbreak = sum(outbreak)
      )
  }) %>% do.call("rbind", .)
  
  folded_sub <- clean_folded %>% group_by(outer_fold_id) %>% 
    summarize(
      reg_with_out = sum(has_outbreak)
      , tot_out      = sum(tot_outbreak)
    ) %>%
    filter(
      reg_with_out > epidemic_threshold_space
      , tot_out > epidemic_threshold_total
    )
  
  folded_data %>% filter(outer_fold_id %in% folded_sub$outer_fold_id)
  
}


#' From the cleaned/subset outer folds build all of the individual fit and assess inner folds for all of 
#' the established clusters. Doing this to map over all inner folds
#'
#'
#' @title build_all_inner_folds

#' @param folded_data Folded data using type = "train_data"
#' @return Tibble of all inner folds with outer fold id
#' @author Morgan Kain
#' @export

build_all_inner_folds <- function(folded_data) {

 all_inner_folds <- purrr::map(1:nrow(folded_data), function(fold) {
    
    test_inner <- folded_data[fold, ]
  
    purrr::map(seq_along(unique(test_inner$inner_folds[[1]]$cluster)), function(clust) {
    
    ## Inner training data: exclude a cluster
    train_inner <- test_inner$inner_folds[[1]] %>% dplyr::filter(cluster != clust) 
    
    ## Inner assess data: only the left-out cluster
    assess_inner <- test_inner$inner_folds[[1]] %>% dplyr::filter(cluster == clust) 
    
    tibble(
      outer_fold_id = test_inner$outer_fold_id
      , inner_fold_id = clust
      , train_inner   = list(train_inner)
      , assess_inner  = list(assess_inner)
    )
    
  }) %>% dplyr::bind_rows()
    
 })
  
 return(all_inner_folds)

}



