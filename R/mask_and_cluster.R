#' Mask data to a sub-region of Africa (e.g., country) and then summarize covariates
#' into sub-sub regions of the sub-region (e.g., district)
#'
#'
#' @title mask_and_cluster

#' @param cov_files list of covariate parquet files
#' @param districts_sf sf / dataframe object of sub-sub regions
#' @param district_id_col name of the identifier column for the sub-sub region
#' @param out_dir directory to save output
#' @param overwrite boolean to recalculate and save over a previously saved file or not
#' @return Tibble containing covariate data 
#' @author Morgan Kain
#' @export

mask_and_cluster <- function(
    cov_files
  , districts_sf
  , district_id_col = "shapeName"
  , out_dir
  , overwrite = FALSE
    ) {
  
  ## Check that we're only working on one date at a time
  stopifnot(length(cov_files) == 1)
  
  ## Set filename
  save_filename <- paste(
      out_dir
    , "/"
    , out_dir %>% strsplit("data/") %>% unlist() %>% pluck(length(.))
    , "_"
    , cov_files %>% strsplit("data_") %>% unlist() %>% pluck(length(.))
    , sep = ""
  )
  message(paste0("Processing ", cov_files))
  
  ## Check if file already exists and can be read
  error_safe_read_parquet <- possibly(arrow::open_dataset, NULL)
  
  if (!is.null(error_safe_read_parquet(save_filename)) & !overwrite) {
    message("file already exists and can be loaded, skipping processing")
    return(save_filename)
  }
  
  ## Ensure districts are in projected CRS (ensure valid polygons)
  districts_sf <- sf::st_make_valid(districts_sf) 
  crs_target   <- sf::st_crs(districts_sf)
  
  ## Extract all of the identifier names for the sub-sub regions, retaining just the
   ## name column
  district_identifiers <- names(districts_sf)
  district_name_col    <- district_identifiers[grepl("Name", district_identifiers)]
  district_identifiers <- district_identifiers[!grepl("Name", district_identifiers)]
  
    ## Read one parquet file
    df <- read_parquet(cov_files)
    
    ## Convert to sf
    pts_sf <- sf::st_as_sf(df, coords = c("x", "y"), crs = crs_target)
    
    ## Spatial join: keep only points in the full set of sub-sub regions
    joined <- sf::st_join(pts_sf, districts_sf, left = FALSE)
    
    ## Extract coordinates back into x and y columns
     ## (mostly for plotting and debugging purposes)
    coords <- sf::st_coordinates(joined)
    joined <- joined %>% mutate(x = coords[, 1], y = coords[, 2], .before = 1)
    
    ## Remove the unneeded ID columns
    joined <- joined %>% as_tibble() %>%
      dplyr::select(-all_of(district_identifiers)) %>%
      relocate(!!district_name_col, .after = "y")
    
    ## Write output to a parquet file
    arrow::write_parquet(joined, save_filename, compression = "gzip", compression_level = 5)
    
    return(save_filename)
}

