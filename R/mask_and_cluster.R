#' Two functions to mask data to a sub-region of Africa (e.g., country) and then summarize covariates
#' into sub-sub regions of the sub-region (e.g., district). Because all dates use the same x, y coordinates
#' can just figure out ADM regions for one date and join this with the other dates
#'
#'
#' @title mask_and_cluster_build_template

#' @param cov_files A single covariate parquet file
#' @param districts_sf sf / dataframe object of sub-sub regions
#' @param district_id_col name of the identifier column for the sub-sub region
#' @param out_dir directory to save output
#' @param overwrite boolean to recalculate and save over a previously saved file or not
#' @return Single tibble of the ADM identifiers for all x, y coordinates
#' @author Morgan Kain
#' @export

mask_and_cluster_build_template <- function(
    cov_files
  , districts_sf
  , district_id_col = "shapeName"
  , out_dir
  , overwrite = FALSE
) {
  
  ## Check that we're only working on one date at a time
  stopifnot(length(cov_files) == 1)
  
  ## Read one parquet file
  df <- read_parquet(cov_files) %>% 
    filter(forecast_interval == 30) %>%
    dplyr::select(-forecast_interval)
  
  ## Ensure districts are in projected CRS (ensure valid polygons)
  districts_sf <- lapply(districts_sf, FUN = function(x) {
    sf::st_make_valid(x) |> st_collection_extract("POLYGON")
  }) 
  
  crs_target   <- lapply(districts_sf, FUN = function(x) sf::st_crs(x))
  
  ## Extract all of the identifier names for the sub-sub regions, retaining just the name column
  all_identifiers <- lapply(districts_sf, FUN = function(x) {
    district_identifiers <- names(x)
    district_name_col    <- district_identifiers[grepl("Name", district_identifiers)]
    district_identifiers <- district_identifiers[!grepl("Name", district_identifiers)]
    return(list(district_name_col, district_identifiers))
  })
  
  names(all_identifiers) <- lapply(districts_sf, FUN = function(x) x$shapeGroup[1])
  
  df_sorted <- purrr::pmap(list(crs_target, districts_sf, all_identifiers, names(all_identifiers)), .f = function(x, y, z, q) {
    ## Convert to sf
    pts_sf <- sf::st_as_sf(df, coords = c("x", "y"), crs = x)
    
    old_s2 <- sf_use_s2(TRUE)
    
    ## Spatial join: keep only points in the full set of sub-sub regions
    joined <- try({sf::st_join(pts_sf, y, left = FALSE)}, silent = T)
    
    if (class(joined)[1] == "try-error") {
      old_s2 <- sf_use_s2(FALSE)
      joined <- try({sf::st_join(pts_sf, y, left = FALSE)}, silent = T)
    }
    
    ## Extract coordinates back into x and y columns
    ## (mostly for plotting and debugging purposes)
    coords <- sf::st_coordinates(joined)
    joined <- joined %>% 
      mutate(x = coords[, 1], y = coords[, 2], .before = 1) %>%
      as_tibble() %>%
      dplyr::select(-all_of(z[[2]])) %>%
      relocate(!!z[[1]], .after = "y") %>%
      mutate(Country = q, .before = shapeName)
    
  }) %>% do.call("rbind", .)
  
  return(df_sorted %>% dplyr::select(x, y, Country, shapeName))
  
}


#' @title mask_and_cluster_from_template

#' @param template ADM regions for one date built from mask_and_cluster_build_template
#' @param cov_files all covariate parquet files
#' @param out_dir directory to save output
#' @param overwrite boolean to recalculate and save over a previously saved file or not
#' @return character vector path to parquet files
#' @author Morgan Kain
#' @export

mask_and_cluster_from_template <- function(template, cov_files, out_dir, overwrite = FALSE) {

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
  
  ## Read one parquet file
  df <- read_parquet(cov_files)
  
  ## Join with the template to get the ADM region info
  df_with_adm <- df %>% 
    left_join(., template) %>% 
    relocate(Country, shapeName, .after = y) %>%
    filter(!is.na(Country))
  
  ## Write output to a parquet file
  arrow::write_parquet(df_with_adm, save_filename, compression = "gzip", compression_level = 5)
  
  return(save_filename)
  
}
