#' Build lagged variables, join in cases, and aggregate
#'
#'
#' @title lag_join_aggregate

#' @param file_list names of the parquets saved separately (only fix I could get working for whatever reason)
#' @param processed_dates prepared dates for each parquet for building lagged variables
#' @param cov_files list of covariate parquet files. Here the masked and clustered data
#' @param rvf_response summarized case data
#' @param out_dir Place to save output
#' @param overwrite Boolean to recalculate and save over a previously saved file or not
#' @return character vector path to parquet files
#' @author Morgan Kain
#' @export

lag_join_aggregate <- function (
      file_list
    , processed_dates
    , cov_files
    , rvf_response
    , out_dir
    , overwrite = FALSE
) {
  
  ## 0) Logistics stuff
  
  ## Of all of the preprocessed_dates which is the one needed for this branch.
   ## NOTE: Rather messy way to do this, but was having so many targets issues and this is the
   ## method I could get working
  processed_dates <- processed_dates[[
    which(lapply(processed_dates, FUN = function(x) grepl(file_list, x$filename[1])) %>% unlist())
  ]]
  
  save_filename <- paste(
    out_dir
    , "/"
    , paste(out_dir %>% strsplit("data/") %>% unlist() %>% pluck(length(.)), unique(processed_dates$date), sep = "_")
    , ".parquet"
    , sep = ""
  ) %>% as.character()
  
  ## Check if file already exists and can be read
  error_safe_read_parquet <- possibly(arrow::read_parquet, NULL)
  
  if (!is.null(error_safe_read_parquet(save_filename)) & !overwrite) {
    message("file already exists and can be loaded, skipping processing")
    return(save_filename)
  }
  
  ## Load cases
  cases <- read_parquet(rvf_response)
  
  ## Build a soil mapping key for use within the loop for all dates
  ## see https://cteco.uconn.edu/guides/Soils_Drainage.htm
  soil_drainage_key <- data.frame(
    old = c("E", "SE", "W", "MW", "I", "P", "VP")
    , new = c(1, 2, 3, 4, 5, 6, 7)
  )
  
  ## 2) Build lagged variables, 3) Join cases, and 4) Summarize covariate data and outbreaks to sub regions
  ## one .parquet file at a time
  fdat <- read_parquet(file_list) %>% 
    sf::st_drop_geometry() %>%
    ungroup()
  
  ## Could maybe [?] have done this earlier, but is kinda part of data aggregation
  ## so seems ok for now 
  ## Want soil drainage to be ordinal, which for a tree based method involves just treating it
  ## as numeric. So cant have UNK in soil drainage, so converting UNK to nearest neighbor values
  ## first, then converting to numeric based on the soil drainage then averaging and rounding
  fdat$soil_drainage[fdat$soil_drainage == "UNK"] <- NA
  
  fdat <- sf::st_as_sf(fdat, coords = c("x","y"))
  fdat$soil_drainage[is.na(fdat$soil_drainage)] <- fdat[!is.na(fdat$soil_drainage),]$soil_drainage[
    sf::st_nearest_feature(fdat[is.na(fdat$soil_drainage),], fdat[!is.na(fdat$soil_drainage),])
  ]
  
  fdat$soil_texture[fdat$soil_texture == "UNK"] <- NA
  
  fdat <- sf::st_as_sf(fdat, coords = c("x","y"))
  fdat$soil_texture[is.na(fdat$soil_texture)] <- fdat[!is.na(fdat$soil_texture),]$soil_texture[
    sf::st_nearest_feature(fdat[is.na(fdat$soil_texture),], fdat[!is.na(fdat$soil_texture),])
  ]
  
  coords <- sf::st_coordinates(fdat)
  fdat   <- sf::st_drop_geometry(fdat)
  fdat   <- fdat %>% mutate(x = coords[,1], y = coords[,2], .before = 1)
  
  suppressMessages({
    fdat <- fdat %>% mutate(
      soil_drainage = plyr::mapvalues(
        soil_drainage
        , from = soil_drainage_key$old
        , to   = soil_drainage_key$new
      ) %>% as.numeric()
      , soil_texture = as.numeric(soil_texture)
    )
  })
  
  ## First, find the variables that are static and forecasted -- these do not
  ## need to be lagged
  
  ## Covariates for lagging
  lagging_names <- fdat %>% dplyr::select(
    contains("anomaly"), -contains("forecast")
  ) %>% names()
  
  ## Forecasted and Static covariates
  fdat.f <- fdat %>% dplyr::select(-all_of(lagging_names), -slope, -aspect)
  
  ## process each lag
  all_lags <- lapply(processed_dates %>% rowwise() %>% group_split(), FUN = function(this_set) {
    
    lag_gap <- as.numeric(this_set$date - this_set$lag_floors) 
    
    tdat <- arrow::open_dataset(cov_files[this_set$file_nums %>% unlist()]) %>% 
      collect() %>% ungroup() %>%
      dplyr::select(x, y, shapeName, Country, all_of(lagging_names))
    
    ## Extract out the average of the variables over the parquet files that are needed
    ## for the given lag
    tdat.s <- tdat %>% 
      group_by(x, y, shapeName, Country) %>%
      summarize(across(where(is.numeric), mean), .groups = "keep") %>%
      rename_with(
        ~ paste0(.x, paste("_", lag_gap, sep = "")),           
        .cols = starts_with("anomaly")    
      )
    
  }) %>% 
    reduce(left_join, by = c("x", "y", "shapeName", "Country"))
  
  ## Build the final covariate data frame
  fdat.fc <- fdat.f %>% left_join(., all_lags, by = c("x", "y", "shapeName", "Country"))
  
  ## join cases
  cases.t <- cases %>% filter(date == unique(fdat.fc$date)) %>%
    dplyr::select(-forecast_start, -forecast_end)
  
  fdat.fcc <- fdat.fc %>% 
    left_join(cases.t, by = c("x", "y", "date", "forecast_interval")) %>% 
    relocate(cases, .after = shapeName) %>%
    mutate(cases = ifelse(is.na(cases), 0, cases))
  
  ## Finally, first step in dealing with NAN or infinite: 1) convert to NA so that it does not contribute to the mean
  fdat.fcc <-  fdat.fcc %>%
    mutate(across(starts_with("anomaly"), ~ replace(., is.nan(.), NA))) %>%
    mutate(across(starts_with("anomaly"), ~ replace(., is.infinite(.), NA)))
  
  ## and then reduce
  fdat.final <- fdat.fcc %>% 
    dplyr::select(-c(x, y, doy, month, year)) %>%
    group_by(shapeName, Country, date, forecast_interval) %>% 
    summarize(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)), .groups = "keep") %>%
    mutate(outbreak = ifelse(cases > 0, 1, 0), .after = cases) %>%
    ## Finally, second step in dealing with NAN or infinite: 2) convert (rare) NAN, NA, and inf to 0
    mutate(across(starts_with("anomaly"), ~ replace(., is.nan(.), 0))) %>%
    mutate(across(starts_with("anomaly"), ~ replace(., is.na(.), 0))) %>%
    mutate(across(starts_with("anomaly"), ~ replace(., is.infinite(.), 0))) %>%
    ungroup()
  
  arrow::write_parquet(fdat.final, save_filename, compression = "gzip", compression_level = 5)
  
  return(save_filename)
  
}


#' A setup function for lag_join_aggregate
#'
#'
#' @title prep_dates

#' @param cov_files list of covariate parquet files. Here the masked and clustered data
#' @param rvf_response summarized case data
#' @return List of tibbles that contain which files are needed for each of the lags 
#' @author Morgan Kain
#' @export

prep_dates <- function(cov_files, rvf_response) {
  
  ## Extract dates from the saved files
  dates_for_predictions <- cov_files %>% sapply(., FUN = function(x) {
    strsplit(x, "data_") %>% unlist() %>% pluck(2) %>% strsplit(., ".parquet") %>% unlist() %>% pluck(1)
  }) %>% unname() %>% as.Date()
  processed_dates       <- vector("list", length(dates_for_predictions))
  cases                 <- read_parquet(rvf_response)
  
  ## 1) Determine which parquet files are needed to build the lagged covariates
  for (i in seq_along(cov_files)) {
    
    files_to_avg <- data.frame(
      lag_floors   = dates_for_predictions[i] - c(30, 60, 90)
      , lag_ceilings = dates_for_predictions[i] - c(1, 31, 61)
    ) %>% rowwise() %>%
      mutate(
        file_nums    = which(dates_for_predictions >= lag_floors & dates_for_predictions <= lag_ceilings) %>% list()
        , num_files    = length(file_nums)
        , closest_date = dates_for_predictions[-i][which((dates_for_predictions[-i] - lag_ceilings) < 0)] %>% pluck(length(.)) %>% list()
      )
    
    if (any(sapply(files_to_avg$closest_date, FUN = is.null))) {
      processed_dates[[i]] <- NULL
    } else {
      
      files_to_avg <- files_to_avg %>% mutate(
        day_diff     = dates_for_predictions[i] - closest_date
        , file_nums    = ifelse(num_files == 0, which(dates_for_predictions == unlist(closest_date)) %>% list(), file_nums %>% list())
      ) %>% mutate(
        date       = dates_for_predictions[i]
        , filename   = cov_files[i]
        , .before    = 1
      )
      
      processed_dates[[i]] <- files_to_avg
      
    }
    
  }
  
  processed_dates <- Filter(Negate(is.null), processed_dates)
  
  processed_dates
  
}


#' A follow up function to lag_join_aggregate to combine all of the aggregated parquet files
#'
#'
#' @title combine_lja

#' @param in_dir vector of paths to all of the summarized parquet files from lag_join_aggregate
#' @param out_dir location to save the single final joined file
#' @param overwrite Boolean to recalculate and save over a previously saved file or not
#' @return character vector path to parquet file
#' @author Morgan Kain
#' @export

combine_lja <- function(
    in_dir
  , out_dir
  , overwrite
  ) {
  
  ## 0) Logistics stuff
  
  ## Set filename
  save_filename <- paste(
    out_dir
    , "/"
    , paste(out_dir %>% strsplit("data/") %>% unlist() %>% pluck(length(.)), "final", sep = "_")
    , ".parquet"
    , sep = ""
  )
  
  ## Check if file already exists and can be read
  error_safe_read_parquet <- possibly(arrow::open_dataset, NULL)
  
  if (!is.null(error_safe_read_parquet(save_filename)) & !overwrite) {
    message("file already exists and can be loaded, skipping processing")
    return(save_filename)
  }
  
  all_out <- lapply(in_dir %>% as.list(), FUN = function(x) {
    tf <- read_parquet(x)
  }) %>% do.call("rbind", .)
  
  arrow::write_parquet(all_out, save_filename, compression = "gzip", compression_level = 5)
  
  return(save_filename)
  
}
