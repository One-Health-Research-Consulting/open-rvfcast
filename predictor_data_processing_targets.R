# This repository uses targets projects.
# To switch to the data acquisition adn cleaning pipeline run:
# `Sys.setenv(TAR_PROJECT = "data")`

## Re-record current dependencies for CAPSULE users
if (Sys.getenv("USE_CAPSULE") %in% c("1", "TRUE", "true")) {
  capsule::capshot(c(
    "packages.R"
  , list.files(pattern = "_targets.*\\.(r|R)$", full.names = TRUE)
  , list.files("R", pattern = "\\.(R|r)$", full.names = TRUE)
  ))
}

## Load packages (in packages.R) and load project-specific functions in R folder
suppressPackageStartupMessages(source("packages.R"))
for (f in list.files(here::here("R"), full.names = TRUE)) source(f)

aws_bucket <- Sys.getenv("AWS_BUCKET_ID")

## Get the "purpose" of the current run (full model 'train' or 'forecast')
purpose <- Sys.getenv("PURPOSE")

## Targets options
source("_targets_settings.R")

## Convenience function to format .env flags properly for overwrite parameter and target cues
## For AWS targets if the overwrite flag is 'TRUE' we don't want to download data from AWS
## otherwise we always want to check.
parse_flag <- function(flags, cue = NULL) {
  stopifnot(cue %in% c(NULL, "never", "always"))
  flag <- any(as.logical(Sys.getenv(flags, unset = "FALSE")))
  if (!is.null(cue)) flag <- targets::tar_cue(ifelse(flag, cue, ifelse(cue == "never", "always", "thorough")))
  flag
}

## Every major data target returns a list of parquet file names. Those can then be
## combined and opened using arrow::open_dataset which allows a lot of operations
## to be performed on the data without loading it all into memory. See
## augmented_data target for more details

## Data targets are integrated with but not dependent on AWS. The _AWS and
## _AWS_upload targets fetch and upload parquet files. Before trying
## to download and process the raw data from the primary sources, each data
## target will attempt to fetch the processed parquet file from an AWS S3 bucket.
## If the file can be successfully downloaded and opened with arrow it will move
## on to the next task unless the OVERWRITE_X_DATA environment flag is set to TRUE
## in the .env file. In that case, the target will always download and process
## data directly from the primary source. The pipeline will still run even if
## the AWS targets fail.

## Static Data Download ----------------------------------------------------
## These data sources don't change with time.
static_targets <- tar_plan(

  # Define country bounding boxes and years to set up download ----------------------------------------------------
  # TODO change from rnaturalearth to rgeoboundaries to get ADM2 districts
  tar_target(country_polygons, create_country_polygons(
    countries = c(
      "Libya", "Kenya", "South Africa", "Mauritania", "Niger", "Namibia"
    , "Madagascar", "Eswatini", "Botswana", "Mali", "United Republic of Tanzania"
    , "Chad", "Sudan", "Senegal", "Uganda", "South Sudan", "Burundi")
  , states = tibble(state = "Mayotte", country = "France")))

, tar_target(continent_polygon, create_africa_polygon())
, tar_target(continent_raster_template, wrap(terra::rast(ext(continent_polygon), resolution = 0.1)))
, tar_target(country_bounding_boxes, get_country_bounding_boxes(continent_polygon))

  ## nasa power resolution = 0.5;
  ## ecmwf = 1;
  ## sentinel ndvi = 0.01
  ## modis ndvi = 0.01

  ## SOIL -----------------------------------------------------------
, tar_target(soil_directory, create_data_directory(directory_path = "data/soil_dataset"))

  ## Check if preprocessed soil data already exists on AWS and can be loaded.
  ## If so download from AWS instead of primary source
, tar_target(soil_AWS, AWS_get_folder(
    local_folder     = soil_directory
  , skip_fetch       = Sys.getenv("SKIP_FETCH") == "TRUE"
  , sync_with_remote = TRUE)
  , error            = "null"
  , cue              = tar_cue("always"))

, tar_target(soil_preprocessed, preprocess_soil(
    soil_directory            = soil_directory
  , continent_raster_template = continent_raster_template
  , output_filename            = "soil_preprocessed.parquet"
  , overwrite                 = parse_flag("OVERWRITE_STATIC_DATA")
    ## Enforce dependency
  , soil_AWS)
  , format                    = "file"
  , repository                = "local")

, tar_target(soil_preprocessed_AWS_upload, AWS_put_files(
    transformed_file_list  = soil_preprocessed
  , local_folder          = soil_directory
  , overwrite             = parse_flag("OVERWRITE_STATIC_DATA"))
    ## Continue the pipeline even on error
  , error                 = "null")

  ## ASPECT -------------------------------------------------
, tar_target(aspect_urls, c(
    "aspect_zero"         = "https://www.fao.org/fileadmin/user_upload/soils/HWSD%20Viewer/GloAspectClN_30as.rar"
  , "aspect_fortyfive"     = "https://www.fao.org/fileadmin/user_upload/soils/HWSD%20Viewer/GloAspectClE_30as.rar"
  , "aspect_onethirtyfive" = "https://www.fao.org/fileadmin/user_upload/soils/HWSD%20Viewer/GloAspectClS_30as.rar"
  , "aspect_twotwentyfive" = "https://www.fao.org/fileadmin/user_upload/soils/HWSD%20Viewer/GloAspectClW_30as.rar"
  , "aspect_undef"        = "https://www.fao.org/fileadmin/user_upload/soils/HWSD%20Viewer/GloAspectClU_30as.rar"))

, tar_target(aspect_directory, create_data_directory(directory_path = "data/aspect_dataset"))

  ## Check if preprocessed aspect data already exists on AWS and can be loaded.
  ## If so download from AWS instead of primary source
, tar_target(aspect_AWS, AWS_get_folder(
    local_folder     = aspect_directory
  , skip_fetch       = Sys.getenv("SKIP_FETCH") == "TRUE"
  , sync_with_remote = TRUE)
  , error            = "null"
  , cue              = tar_cue("always"))

, tar_target(aspect_preprocessed, get_remote_rasters(
    urls                      = aspect_urls
  , output_dir                = aspect_directory
  , output_filename            = "aspect.parquet"
  , continent_raster_template = continent_raster_template
    ## What is the dominant aspect for each point?
  , aggregate_method          = "which.max"
    ## What is the dominant aspect at the scale of the template raster?
  , resample_method           = "mode"
  , factorize                 = TRUE
  , overwrite                 = parse_flag("OVERWRITE_STATIC_DATA")
    ## Enforce dependency
  , aspect_AWS)
  , format                    = "file"
  , repository                = "local")

, tar_target(aspect_preprocessed_AWS_upload, AWS_put_files(
    transformed_file_list  = aspect_preprocessed
  , local_folder          = aspect_directory
  , overwrite             = parse_flag("OVERWRITE_STATIC_DATA"))
    ## Continue the pipeline even on error
  , error                 = "null")

  ## SLOPE -------------------------------------------------
, tar_target(slope_urls, c(
    "slope_zero"     = "https://www.fao.org/fileadmin/user_upload/soils/HWSD%20Viewer/GloSlopesCl1_30as.rar"
  , "slope_pointfive" = "https://www.fao.org/fileadmin/user_upload/soils/HWSD%20Viewer/GloSlopesCl2_30as.rar"
  , "slope_two"      = "https://www.fao.org/fileadmin/user_upload/soils/HWSD%20Viewer/GloSlopesCl3_30as.rar"
  , "slope_five"      = "https://www.fao.org/fileadmin/user_upload/soils/HWSD%20Viewer/GloSlopesCl4_30as.rar"
  , "slope_ten"      = "https://www.fao.org/fileadmin/user_upload/soils/HWSD%20Viewer/GloSlopesCl5_30as.rar"
  , "slope_fifteen"   = "https://www.fao.org/fileadmin/user_upload/soils/HWSD%20Viewer/GloSlopesCl6_30as.rar"
  , "slope_thirty"   = "https://www.fao.org/fileadmin/user_upload/soils/HWSD%20Viewer/GloSlopesCl7_30as.rar"
  , "slope_fortyfive" = "https://www.fao.org/fileadmin/user_upload/soils/HWSD%20Viewer/GloSlopesCl8_30as.rar"))

, tar_target(slope_directory, create_data_directory(directory_path = "data/slope_dataset"))

  # Check if preprocessed slope data already exists on AWS and can be loaded.
  # If so download from AWS instead of primary source
, tar_target(slope_AWS, AWS_get_folder(
    local_folder     = slope_directory
  , skip_fetch       = Sys.getenv("SKIP_FETCH") == "TRUE"
  , sync_with_remote = TRUE)
  , error            = "null"
  , cue              = tar_cue("always"))

, tar_target(slope_preprocessed, get_remote_rasters(
    urls             = slope_urls
  , output_dir       = slope_directory
  , output_filename   = "slope.parquet"
  , continent_raster_template
    ## What is the dominant slope for each point?
  , aggregate_method = "which.max"
    ## What is the dominant slope at the scale of the template raster?
  , resample_method  = "mode"
  , factorize        = TRUE
  , overwrite        = parse_flag("OVERWRITE_STATIC_DATA")
    ## Enforce dependency
  , slope_AWS)
  , format           = "file"
  , repository       = "local")

, tar_target(slope_preprocessed_AWS_upload, AWS_put_files(
    transformed_file_list  = slope_preprocessed
  , local_folder          = slope_directory
  , overwrite             = parse_flag("OVERWRITE_STATIC_DATA"))
    ## Continue the pipeline even on error
  , error                 = "null")

  ## Gridded Livestock of the world -----------------------------------------------------------
, tar_target(glw_urls, c(
    "glw_cattle" = "https://dataverse.harvard.edu/api/access/datafile/6769710"
  , "glw_sheep"  = "https://dataverse.harvard.edu/api/access/datafile/6769629"
  , "glw_goats"  = "https://dataverse.harvard.edu/api/access/datafile/6769692"))

, tar_target(glw_directory, create_data_directory(directory_path = "data/glw_dataset"))

  ## Check if preprocessed glw data already exists on AWS and can be loaded.
  ## If so download from AWS instead of primary source
, tar_target(glw_AWS, AWS_get_folder(
    local_folder     = glw_directory
  , skip_fetch       = Sys.getenv("SKIP_FETCH") == "TRUE"
  , sync_with_remote = TRUE)
  , error            = "null"
  , cue              = tar_cue("always"))

, tar_target(glw_preprocessed, preprocess_glw_data(
    glw_directory_dataset     = glw_directory
  , glw_urls                  = glw_urls
  , continent_raster_template = continent_raster_template
  , overwrite                 = parse_flag("OVERWRITE_STATIC_DATA")
    ## Enforce dependency
  , glw_AWS)
  , format                    = "file"
  , repository                = "local")

, tar_target(glw_preprocessed_AWS_upload, AWS_put_files(
    transformed_file_list = glw_preprocessed
  , local_folder          = glw_directory
  , overwrite             = parse_flag("OVERWRITE_STATIC_DATA"))
  , error                 = "null")

  ## ELEVATION -----------------------------------------------------------
, tar_target(elevation_directory, create_data_directory(directory_path = "data/elevation_dataset"))

  ## Check if preprocessed elevation data already exists on AWS and can be loaded.
  ## If so download from AWS instead of primary source
, tar_target(elevation_AWS, AWS_get_folder(
    local_folder     = elevation_directory
  , skip_fetch       = Sys.getenv("SKIP_FETCH") == "TRUE"
  , sync_with_remote = TRUE)
  , error            = "null"
  , cue              = tar_cue("always"))

, tar_target(elevation_preprocessed, get_elevation_data(
    output_dir                = elevation_directory
  , output_filename            = "africa_elevation.parquet"
  , continent_raster_template = continent_raster_template
  , overwrite                 = parse_flag("OVERWRITE_STATIC_DATA")
  , elevation_AWS)
  , format                    = "file"
  , repository                = "local")

, tar_target(elevation_preprocessed_AWS_upload, AWS_put_files(
    transformed_file_list  = elevation_preprocessed
  , local_folder          = elevation_directory
  , overwrite             = parse_flag("OVERWRITE_STATIC_DATA"))
  , error                 = "null")

  ## BIOCLIM -----------------------------------------------------------
, tar_target(bioclim_directory, create_data_directory(directory_path = "data/bioclim_dataset"))

  ## Check if preprocessed bioclim data already exists on AWS and can be loaded.
  ## If so download from AWS instead of primary source
, tar_target(bioclim_AWS, AWS_get_folder(
    local_folder     = bioclim_directory
  , skip_fetch       = Sys.getenv("SKIP_FETCH") == "TRUE"
  , sync_with_remote = TRUE)
  , error            = "null"
  , cue              = tar_cue("always"))

, tar_target(bioclim_preprocessed, get_bioclim_data(
    output_dir                = bioclim_directory
  , output_filename            = "bioclim.parquet"
  , continent_raster_template = continent_raster_template
  , overwrite                 = parse_flag("OVERWRITE_STATIC_DATA")
  , bioclim_AWS)
  , format                    = "file"
  , repository                = "local")

, tar_target(bioclim_preprocessed_AWS_upload, AWS_put_files(
    transformed_file_list  = bioclim_preprocessed
  , local_folder          = bioclim_directory
  , overwrite             = parse_flag("OVERWRITE_STATIC_DATA"))
  , error                 = "null")

  ## LANDCOVER -----------------------------------------------------------
, tar_target(landcover_types, c("trees", "grassland", "shrubs", "cropland", "built", "bare", "snow", "water", "wetland", "mangroves", "moss")),
  tar_target(landcover_directory, create_data_directory(directory_path = "data/landcover_dataset"))

  # Check if preprocessed bioclim data already exists on AWS and can be loaded.
  # If so download from AWS instead of primary source
, tar_target(landcover_AWS, AWS_get_folder(
    local_folder     = landcover_directory
  , skip_fetch       = Sys.getenv("SKIP_FETCH") == "TRUE"
  , sync_with_remote = TRUE)
  , error            = "null"
  , cue              = tar_cue("always"))

, tar_target(landcover_preprocessed, get_landcover_data(
    output_dir                = landcover_directory
  , output_filename            = "landcover.parquet"
  , landcover_types           = landcover_types
  , continent_raster_template = continent_raster_template
  , overwrite                 = parse_flag("OVERWRITE_STATIC_DATA")
  , landcover_AWS)
  , format                    = "file"
  , repository                = "local")

, tar_target(landcover_preprocessed_AWS_upload, AWS_put_files(
    transformed_file_list  = landcover_preprocessed
  , local_folder          = landcover_directory
  , overwrite             = parse_flag("OVERWRITE_STATIC_DATA"))
  , error                 = "null")

)

## Dynamic Data Download -----------------------------------------------------------
dynamic_targets <- tar_plan(

  ## NCL: This function produces a random sampling of n_per_month dates for every month
  ## in every year between start_year and end_year. If a new year is added, the
  ## random draws for the previous years won't change unless the seed is updated.
  ## Ideally we want to make the full dataset for every day and store it then subset
  ## only right before fitting the model.
  tar_target(dates_to_process_all, set_model_dates(
    start_year  = 2005
  , end_year    = lubridate::year(Sys.time())
  , n_per_month = 2
  , seed        = 212)
  , cue = tar_cue("always"))

  ## Pull the names of the full africa parquet files from the bucket and figure out what
   ## files need updating based on which files are missing | the dates in dates_to_process_all
, tar_target(dates_to_process, {

    full_parquet_files <- AWS_get_filenames(africa_full_predictor_data_directory)

    ## Extract the date string embedded in each parquet filename
    processed_dates <- purrr::map_chr(full_parquet_files, .f = function(i) {
       (strsplit(i, "data_")[[1]][2] |> strsplit(".parquet"))[[1]][1]
    }) |>
    na.omit()

    ## Process only the dates_to_process_all dates that have not yet been written.
    ## Using membership rather than max-date comparison avoids skipping unprocessed
    ## historical dates when a more recent forecast anchor parquet already exists.
    narrow_dates <- dates_to_process_all[!as.character(dates_to_process_all) %in% processed_dates]

    if (purpose == "forecast") {

      ## Forecast anchor = last day of the previous complete month.
      ## ERA5T for the previous month is always fully available by the time the
      ## pipeline runs (end-of-month is well past the 5-day ERA5T lag).
      ## Anchoring to floor_date - 1 prevents current-month ERA5T download failures.
      era5t_anchor <- lubridate::floor_date(Sys.Date(), "month") - 1L

      ## Exclude current-month dates: ERA5T is not yet available for the current month.
      ## NASA downloads whatever is available for each month; fetch_and_transform_nasa_weather
      ## handles 404s gracefully, and africa_full_predictor_data_sources_temporal falls
      ## back to ERA5T anomaly files for any date without a NASA anomaly file.
      narrow_dates <- narrow_dates[narrow_dates <= era5t_anchor]

      ## Anchor is always appended so the pipeline always has a fresh forecast date.
      ## unique() removes the duplicate when era5t_anchor is already in narrow_dates.
      narrow_dates <- unique(c(narrow_dates, era5t_anchor))
    }

    narrow_dates

  })

, tar_target(months_to_process, dates_to_process |> format("%Y-%m") |> unique())

  ## SENTINEL NDVI -----------------------------------------------------------
  ## 2018-present
  ## 10 day period
, tar_target(sentinel_ndvi_transformed_directory, create_data_directory(directory_path = "data/sentinel_ndvi_transformed"))

, tar_target(get_sentinel_ndvi_AWS, AWS_get_folder(
    local_folder     = sentinel_ndvi_transformed_directory
  , skip_fetch       = Sys.getenv("SKIP_FETCH") == "TRUE"
  , sync_with_remote = TRUE)
  , error            = "null"
  , cue              = tar_cue("always"))

  ## Should last 10 minutes. If it fails renew the token and try again.
, tar_target(sentinel_ndvi_token_file, get_sentinel_ndvi_token(), cue = tar_cue("always"))

  # get API parameters
, tar_target(sentinel_ndvi_api_parameters, get_sentinel_ndvi_api_parameters())

  ## MAX SESSION = 4! Can't parallel this one due to API restrictions
  ## Sentinel data is weekly so we also expand out so every day has a value
  ## to make it easier to join in. This is a step function see modis NDVI for
  ## more details
, tar_target(sentinel_ndvi_transformed, transform_sentinel_ndvi(
    sentinel_ndvi_api_parameters        = sentinel_ndvi_api_parameters
  , continent_raster_template           = continent_raster_template
  , sentinel_ndvi_transformed_directory = sentinel_ndvi_transformed_directory
  , sentinel_ndvi_token_file            = sentinel_ndvi_token_file
  , basename_template                   = "transformed_sentinel_NDVI_{start_date}_to_{end_date}.parquet"
  , overwrite                           = parse_flag("OVERWRITE_SENTINEL_NDVI"))
  , pattern                             = map(sentinel_ndvi_api_parameters)
  , error                               = "null"
  , format                              = "file"
  , repository                          = "local")

, tar_target(sentinel_ndvi_transformed_AWS_upload, AWS_put_files(
    transformed_file_list = sentinel_ndvi_transformed
  , local_folder          = sentinel_ndvi_transformed_directory
  , overwrite             = parse_flag("OVERWRITE_SENTINEL_NDVI"))
  , error                 = "null")

  ## MODIS NDVI -----------------------------------------------------------
  ## 2005-present
  ## this satellite will be retired soon, so we should use sentinel for present dates
  ## ~10 day period. Note the period of sentinel data does not match modis.
  ## Some interpolation would be useful. Currently using step function.
, tar_target(modis_ndvi_transformed_directory, create_data_directory(directory_path = "data/modis_ndvi_transformed"))

  ## This target reads in an Appears token from the .env file and tests that it
  ## still works. It requests a new token and updates the .env file if not.
, tar_target(modis_ndvi_token, get_modis_ndvi_token(), cue = tar_cue("always"))

  ## The last day of every years we want to request ndvi data.
  ## Ordered by end date so that the current year will request new data ever new
  ## day the pipeline is run.
, tar_target(modis_task_end_dates, c(seq(as.Date("2005-12-31"), Sys.Date(), by = "year"), Sys.Date()) |> unique()),

  ## Set parameters and submit request for full continent
  ## Bundle requests take quite a while to finish processing depending on the size.
  ## Branching by year. This makes each task faster and lets us process new years without having
  ## to re-do previous years. It also ensures that tasks are processed in the order submitted.
  ## Set OVERWRITE_MODIS_NDVI to TRUE in the .env file to force re-download and processing of
  ## previous years. The current year will always re-run regardless of this setting.
  ## If a year isn't complete (i.e there are missing days) it will re-run that year.
  tar_target(modis_ndvi_task_id_continent, submit_modis_ndvi_task_request_continent(
    end_date                         = modis_task_end_dates
  , modis_ndvi_token                 = modis_ndvi_token
    ## Add an extra 10 degrees of width to avoid weird circle that cuts off Somalia. Due to modis's native sinusoidal crs
  , bbox_coords                      = sf::st_bbox(continent_polygon) + c(0, 0, 10, 0)
  , modis_ndvi_transformed_directory = modis_ndvi_transformed_directory
  , overwrite                        = parse_flag("OVERWRITE_MODIS_NDVI"))
  , pattern                          = map(modis_task_end_dates)
  , cue                              = tar_cue("always"))

  ## Set up modis_ndvi data requests
, tar_target(modis_ndvi_bundle_request, submit_modis_ndvi_bundle_request(
    modis_ndvi_token             = modis_ndvi_token
  , modis_ndvi_task_id_continent = modis_ndvi_task_id_continent)
  , pattern                      = map(modis_ndvi_task_id_continent))

  ## Check if modis_ndvi files already exists on AWS and can be loaded
  ## The only important one is the directory. The others are there to enforce dependencies.
, tar_target(modis_ndvi_transformed_AWS, AWS_get_folder(
    local_folder     = modis_ndvi_transformed_directory
  , skip_fetch       = Sys.getenv("SKIP_FETCH") == "TRUE"
  , sync_with_remote = TRUE)
  , error            = "null"
  , cue              = tar_cue("always"))

  ## Collect branches from modis_ndvi_bundle_request and split into branches
  ## where each branch is a batch of 10 requests
  ## MODIS NDVI refers to the highest Normalized Difference
  ## Vegetation Index (NDVI) value recorded within a 16-day period.
  ## We're joining that to daily data. One approach would be spline based interpolation
  ## but then it would be tough to figure out what to do with NDVI
  ## when we go to forecast. Right now we're just using step function
  ## interpolation where the NDVI value is constant for the entire 16-day period,
  ## then it steps up or down to the next interval's NDVI value.
, tarchetypes::tar_group_size(
    name    = modis_ndvi_requests
  , size    = 10
  , command = modis_ndvi_bundle_request |>
      arrange(start_date) |>
      ## Remove duplicate file requests
      group_by(sha256) |>
      slice_max(created, n = 1) |>
      ungroup() |>
      mutate(
        end_date = lead(start_date) - days(1)
      , end_date = case_when(
          is.na(end_date) ~ start_date + 15
        , TRUE ~ end_date)
      , interval = end_date - start_date, 10))

  ## Download data, project to the template and save as parquets
  ## ToDo NAs outside of the continent (though masked anyway so would just save some
  ## space, which is nice but not necessary)
  ## Not Found HTTP 404 means the bundle request hasn't finished processing
  ## transform_modis_ndvi()
, tar_target(modis_ndvi_transformed, map_vec(
    ## This map is batching: multiple requests per branch
      seq_len(nrow(modis_ndvi_requests))
    , ~ transform_modis_ndvi(
          modis_ndvi_token
        , modis_ndvi_requests[.x, ]
        , continent_raster_template
        , modis_ndvi_transformed_directory
        , basename_template = "transformed_modis_NDVI_{start_date}.parquet"
        , overwrite         = parse_flag("OVERWRITE_MODIS_NDVI")
        , modis_ndvi_transformed_AWS))
    , pattern    = map(modis_ndvi_requests)
    , format     = "file"
    ## Repos itory local means it isn't stored on AWS just yet.
    , repository = "local"
    , error      = "null")

  ## Put modis_ndvi_transformed files on AWS
, tar_target(modis_ndvi_transformed_AWS_upload, AWS_put_files(
    transformed_file_list = modis_ndvi_transformed
  , local_folder         = modis_ndvi_transformed_directory
  , overwrite            = parse_flag("OVERWRITE_MODIS_NDVI"))
  , error                = "null")

  ## Combine Sentinel an MODIS ndvi data and interopolate to daily interval
  ## Check if modis_ndvi files already exists on AWS and can be loaded
  ## The only important one is the directory. The others are there to enforce dependencies.
, tar_target(ndvi_transformed_directory, create_data_directory(directory_path = "data/ndvi_transformed"))

, tar_target(ndvi_transformed_AWS, AWS_get_folder(
    local_folder     = ndvi_transformed_directory
  , skip_fetch       = Sys.getenv("SKIP_FETCH") == "TRUE"
  , sync_with_remote = TRUE)
  , error            = "null"
  , cue              = tar_cue("always"))

, tar_target(ndvi_years, lubridate::year(modis_task_end_dates))

  ## Create intermediary target pairing each month with relevant MODIS and Sentinel files
, tar_target(ndvi_transformed_sources, create_ndvi_transformed_sources(
    modis_ndvi_transformed
  , sentinel_ndvi_transformed
  , months_to_process_all) |>
    group_by(month) |>
    tar_group()
  , iteration = "group")

  ## Note: MODIS and Sentinel raw data need to be scaled.
  ## MODIS/10000 and Sentinel/200
, tar_target(ndvi_transformed, transform_ndvi(
    ndvi_transformed_sources
  , ndvi_transformed_directory
  , basename_template = "ndvi_transformed_{.y}_{.m}.parquet"
  , overwrite  = parse_flag(c("OVERWRITE_MODIS_NDVI", "OVERWRITE_SENTINEL_NDVI", "OVERWRITE_NDVI_TRANSFORMED")))
  , pattern    = map(ndvi_transformed_sources)
  , format     = "file"
  , error      = "null"
  , repository = "local")

  ## Put ndvi_transformed files on AWS
, tar_target(ndvi_transformed_AWS_upload, AWS_put_files(
    ndvi_transformed
  , ndvi_transformed_directory
  , overwrite = parse_flag(c("OVERWRITE_MODIS_NDVI", "OVERWRITE_SENTINEL_NDVI", "OVERWRITE_NDVI_TRANSFORMED")))
  , error = "null")

  ## NASA POWER recorded weather -----------------------------------------------------------
  ## Replaced by ERA5T as the primary weather source (ERA5T covers the full historical record
  ## at ~5-day lag vs MERRA-2's ~5-week lag, making the mixed NASA+ERA5T approach unnecessary).
  ## Targets retained as comments to allow rollback if needed.
  ##
  # , tar_target(nasa_weather_transformed_directory, create_data_directory(directory_path = "data/nasa_weather_transformed"))
  # , tar_target(nasa_weather_transformed_AWS, AWS_get_folder(
  #     local_folder     = nasa_weather_transformed_directory
  #   , skip_fetch       = Sys.getenv("SKIP_FETCH") == "TRUE"
  #   , sync_with_remote = TRUE)
  #   , error            = "null"
  #   , cue              = tar_cue("always"))
  # , tar_target(nasa_weather_transformed, fetch_and_transform_nasa_weather(
  #     months_to_process         = months_to_process
  #   , nasa_weather_variables    = c("relative_humidity" = "RH2M", "temperature" = "T2M", "precipitation" = "PRECTOTCORR")
  #   , continent_raster_template = continent_raster_template
  #   , local_folder              = nasa_weather_transformed_directory
  #   , basename_template         = "nasa_weather_transformed_{months_to_process}.parquet"
  #   , endpoint                  = "https://power-datastore.s3.amazonaws.com/v10/daily/{year}/{month}/power_10_daily_{yyyymmdd}_merra2_lst.nc"
  #   , overwrite                 = parse_flag("OVERWRITE_NASA_WEATHER")
  #   , nasa_weather_transformed_AWS)
  #   , pattern                   = map(months_to_process)
  #   , error                     = "null"
  #   , format                    = "file")
  # , tar_target(nasa_weather_transformed_AWS_upload, AWS_put_files(
  #     transformed_file_list  = nasa_weather_transformed
  #   , local_folder          = nasa_weather_transformed_directory
  #   , overwrite             = parse_flag("OVERWRITE_NASA_WEATHER"))
  #   , error                 = "null")

  ## ERA5T Near Real-Time Weather -----------------------------------------------------------
  ## ERA5T (ECMWF ERA5 near real-time) mirrors the same three variables as NASA POWER MERRA-2
  ## (temperature, relative humidity, precipitation) but is available with only a ~5-day lag,
  ## allowing monthly forecast runs that cannot wait for MERRA-2's ~5-week lag.
  ## Uses the same CDS credentials (ECMWF_USERID, ECMWF_TOKEN) as the ECMWF seasonal forecasts.
  ## Output parquets share the schema of nasa_weather_transformed so downstream anomaly and
  ## prediction targets can consume either source without changes.
  ## NOTE: Before first use, accept the ERA5 license at:
  ## https://cds.climate.copernicus.eu/datasets/derived-era5-single-levels-daily-statistics
, tar_target(era5t_weather_transformed_directory, create_data_directory(directory_path = "data/era5t_weather_transformed"))

  ## Check if ERA5T weather files already exist on AWS
, tar_target(era5t_weather_transformed_AWS, AWS_get_folder(
    local_folder     = era5t_weather_transformed_directory
  , skip_fetch       = Sys.getenv("SKIP_FETCH") == "TRUE"
  , sync_with_remote = TRUE)
  , error            = "null"
  , cue              = tar_cue("always"))

  ## months_to_process_era5t was a narrow window for the NASA fallback period; now that ERA5T
  ## is the sole weather source, era5t_weather_transformed uses months_to_process_all directly.
  # , tar_target(months_to_process_era5t,
  #     dates_to_process |> format("%Y-%m") |> unique()
  #   , cue = tar_cue("always"))

, tar_target(months_to_process_all, dates_to_process_all |> format("%Y-%m") |> unique())

  ## Fetch ERA5T daily weather from CDS and transform to continental parquets.
  ## Branches over months_to_process_era5t
   ## For forecasting, don't need many months
, tar_target(era5t_weather_transformed, fetch_and_transform_era5t_weather(
    #months_to_process        = months_to_process_era5t
    months_to_process         = months_to_process_all
  , continent_raster_template = continent_raster_template
  , local_folder              = era5t_weather_transformed_directory
  , basename_template         = "era5t_weather_transformed_{months_to_process}.parquet"
  , overwrite                 = parse_flag("OVERWRITE_ERA5T_WEATHER")
  , era5t_weather_transformed_AWS)
  , #pattern                   = map(months_to_process_era5t)
  , pattern                   = map(months_to_process_all)
  , error                     = "null"
  , format                    = "file")

  ## Upload ERA5T weather parquets to AWS
, tar_target(era5t_weather_transformed_AWS_upload, AWS_put_files(
    transformed_file_list  = era5t_weather_transformed
  , local_folder          = era5t_weather_transformed_directory
  , overwrite             = parse_flag("OVERWRITE_ERA5T_WEATHER"))
  , error                 = "null")

  ## ERA5T Bias Correction -----------------------------------------------------------
  ## Was needed when ERA5T served as a fallback for NASA POWER to correct systematic
  ## ERA5T-vs-MERRA-2 offsets. Now that ERA5T is the sole source and weather_historical_means
  ## is computed from ERA5T data, anomalies are ERA5T vs ERA5T means — no bias offset needed.
  # , tar_target(era5t_bias_correction_file, get_or_compute_era5t_bias_correction(
  #       era5t_weather_dir              = era5t_weather_transformed_directory
  #     , weather_anomalies_dir          = weather_anomalies_directory
  #     , weather_historical_means_dir   = weather_historical_means_directory
  #     , continent_raster_template      = continent_raster_template
  #     , output_file                    = "outputs/anomaly_bias_correction/era5t_bias_correction.parquet"
  #     , n_calibration_months           = 72
  #     , skip_download                  = TRUE
  #     , overwrite                      = parse_flag("OVERWRITE_ERA5T_BIAS_CORRECTION"))
  #   , format                           = "file"
  #   , cue                              = tar_cue("always"))

  ## ERA5T Weather Anomalies -----------------------------------------------------------
  ## calculate_era5t_weather_anomalies was a separate fallback path that applied bias
  ## correction before ERA5T became the primary source. weather_anomalies now uses
  ## calculate_weather_anomalies directly on ERA5T monthly parquets (same schema).
  # , tar_target(era5t_weather_anomalies_directory, create_data_directory(directory_path = "data/weather_anomalies_era5t"))
  # , tar_target(era5t_weather_anomalies_AWS, AWS_get_folder(
  #     local_folder     = era5t_weather_anomalies_directory
  #   , skip_fetch       = Sys.getenv("SKIP_FETCH") == "TRUE"
  #   , sync_with_remote = TRUE)
  #   , error            = "null"
  #   , cue              = tar_cue("always"))
  # , tar_target(era5t_weather_anomalies, calculate_era5t_weather_anomalies(
  #     era5t_weather_transformed_file      = era5t_weather_transformed
  #   , bias_correction_file                = era5t_bias_correction_file
  #   , weather_historical_means_dir        = weather_historical_means_directory
  #   , weather_anomalies_era5t_directory   = era5t_weather_anomalies_directory
  #   , overwrite                           = parse_flag("OVERWRITE_ERA5T_WEATHER_ANOMALIES")
  #   , era5t_weather_anomalies_AWS)
  #   , pattern                             = map(era5t_weather_transformed)
  #   , error                               = "null"
  #   , format                              = "file"
  #   , repository                          = "local")
  # , tar_target(era5t_weather_anomalies_AWS_upload, AWS_put_files(
  #     transformed_file_list    = era5t_weather_anomalies
  #   , local_folder            = era5t_weather_anomalies_directory
  #   , overwrite               = parse_flag("OVERWRITE_ERA5T_WEATHER_ANOMALIES"))
  #   , pattern                 = map(era5t_weather_anomalies)
  #   , error                   = "null")

  ## How many months out are we forecasting?
, tar_target(ecmwf_lead_months, seq(1, 6))

  ## ECMWF Weather Forecast data -----------------------------------------------------------
, tar_target(ecmwf_forecasts_transformed_directory, create_data_directory(directory_path = "data/ecmwf_forecasts_transformed"))

  ## set branching for ecmwf download
  ## Note: Neet to auto update years here.
, tar_target(ecmwf_forecasts_api_parameters, set_ecmwf_api_parameter(
    start_year    = 2005
  , bbox_coords   = sf::st_bbox(terra::rast(continent_raster_template))
  , variables     = c("2m_dewpoint_temperature", "2m_temperature", "total_precipitation")
    ## product_types = c("monthly_mean", "monthly_maximum", "monthly_minimum", "monthly_standard_deviation"),
  , product_types = c("monthly_mean")
  , lead_months   = ecmwf_lead_months)
  , cue           = tar_cue("always"))

  ## Check if ecmwf files already exists on AWS and can be loaded
  ## The only important one is the directory. The others are there to enforce dependencies.
, tar_target(get_ecmwf_forecasts_AWS, AWS_get_folder(
    local_folder     = ecmwf_forecasts_transformed_directory
  , skip_fetch       = Sys.getenv("SKIP_FETCH") == "TRUE"
  , sync_with_remote = TRUE)
  , error            = "null"
  , cue              = tar_cue("always"))

  ## Download ecmwf forecasts, project to the template and save as arrow dataset
  ## Note: This target takes a while (mostly because the ECMWF API is slow)
  ## and may need to be run more than once if rebuilding data from scratch
  ## because it's also prone to random failures. Expected parquet file size
  ## is ~100MB.
  ## If this target fails it could be the API is down. Check status at https://status.ecmwf.int/
  ## NOTE: This can't be joined in with other datasets directly because DATE is
  ## base_date - the date the forecast was made which is once a month.
  ## Most often a 30 day forecast, in example, will overlap multiple base date
  ## forecast ranges.
, tar_target(ecmwf_forecasts_transformed, transform_ecmwf_forecasts(
    ecmwf_forecasts_api_parameters
  , ecmwf_forecasts_transformed_directory
  , continent_raster_template
  , basename_template = "ecmwf_seasonal_forecast_{month}_{year}.parquet"
  , overwrite         = parse_flag("OVERWRITE_ECMWF_FORECASTS")
  , get_ecmwf_forecasts_AWS)
  , pattern           = map(ecmwf_forecasts_api_parameters)
  , error             = "null"
  , format            = "file"
  , repository        = "local")

  ## Next step put ecmwf_forecasts files on AWS.
, tar_target(ecmwf_forecasts_transformed_AWS_upload, AWS_put_files(
    transformed_file_list  = ecmwf_forecasts_transformed
  , local_folder          = ecmwf_forecasts_transformed_directory
  , overwrite = parse_flag("OVERWRITE_ECMWF_FORECASTS"))
  , error = "null")

  )

## Data Processing -----------------------------------------------------------
derived_data_targets <- tar_plan(

  ## How far out are we forecasting?
  ## 0-30, 30-60, 60-90 days out ect...
  ## Right now 5 months foreward
  tar_target(forecast_intervals, c(0, 30, 60, 90, 120, 150))

  ## Recorded weather anomalies --------------------------------------------------
, tar_target(weather_historical_means_directory, create_data_directory(directory_path = "data/weather_historical_means"))

  ## Check if weather_historical_means parquet files already exists on AWS and can be loaded
  ## The only important one is the directory. The others are there to enforce dependencies.
, tar_target(weather_historical_means_AWS, AWS_get_folder(
    local_folder     = weather_historical_means_directory
  , skip_fetch       = Sys.getenv("SKIP_FETCH") == "TRUE"
  , sync_with_remote = TRUE)
  , error            = "null"
  , cue              = tar_cue("always"))

, tar_target(weather_historical_means, calculate_weather_historical_means(
    era5t_weather_transformed_directory
  , weather_historical_means_directory
  , basename_template = "weather_historical_mean_doy_{i}.parquet"
  , overwrite         = parse_flag("OVERWRITE_HISTORICAL_MEANS")
  , weather_historical_means_AWS)
  , format            = "file"
  , repository        = "local"
  , cue               = tar_cue_age(
     name = weather_historical_means
     ## Recalculate every 6 months
   , age  = as.difftime(180, units = "days")))

  ## Next step put weather_historical_means files on AWS.
, tar_target(weather_historical_means_AWS_upload, AWS_put_files(
    transformed_file_list  = weather_historical_means
  , local_folder          = weather_historical_means_directory
  , overwrite             = parse_flag("OVERWRITE_HISTORICAL_MEANS"))
  , error                 = "null")

, tar_target(weather_anomalies_directory, create_data_directory(directory_path = "data/weather_anomalies"))

  # Check if weather_historical_means parquet files already exists on AWS and can be loaded
  # The only important one is the directory. The others are there to enforce dependencies.
, tar_target(weather_anomalies_AWS, AWS_get_folder(
    local_folder     = weather_anomalies_directory
  , skip_fetch       = Sys.getenv("SKIP_FETCH") == "TRUE"
  , sync_with_remote = TRUE)
  , error            = "null"
  , cue              = tar_cue("always"))

  ## Weather anomalies are deviations from the historical mean.
  ## ERA5T and NASA POWER share the same output schema, so calculate_weather_anomalies
  ## works directly on ERA5T monthly parquets. Branch over era5t_weather_transformed
  ## (one branch per month); each branch writes one parquet per date.
, tar_target(weather_anomalies, calculate_weather_anomalies(
    era5t_weather_transformed
  , weather_historical_means
  , weather_anomalies_directory
  , basename_template = "weather_anomaly_{date}.parquet"
  , overwrite         = parse_flag("OVERWRITE_WEATHER_ANOMALIES")
  , weather_anomalies_AWS)
  , pattern           = map(era5t_weather_transformed)
  , error             = "null"
  , format            = "file"
  , repository        = "local")

  ## Next step put weather_historical_means files on AWS.
, tar_target(weather_anomalies_AWS_upload, AWS_put_files(
    weather_anomalies
  , weather_anomalies_directory
  , overwrite = parse_flag("OVERWRITE_WEATHER_ANOMALIES"))
  , pattern   = map(weather_anomalies)
  , error     = "null")

  ## forecast weather anomalies ----------------------------------------------------------------------
, tar_target(forecasts_anomalies_directory, create_data_directory(directory_path = "data/forecast_anomalies"))

  ## Check if forecasts_anomalies parquet files already exists on AWS and can be loaded
  ## The only important one is the directory. The others are there to enforce dependencies.
, tar_target(forecasts_anomalies_AWS, AWS_get_folder(
    local_folder     = forecasts_anomalies_directory
  , skip_fetch       = Sys.getenv("SKIP_FETCH") == "TRUE"
  , sync_with_remote = TRUE)
  , error            = "null"
  , cue              = tar_cue("always"))

  ## Calculate the scaled and unscaled difference between the forecast mean and the
  ## historical mean across different lead intervals. The lead intervals reflect
  ## how far out the forecast is. For example 0-30 days out, 30-60 days out ect.
  ## Expected target size is ~40MB. Each branch takes 1-2 minutes in serial
  ## on an M1 mac. Expect to take a day to regenerate the data if re-building from
  ## scratch.

  ## Create intermediary target pairing each date with most recent forecast file
  ## Only includes dates up to the latest forecast month
, tar_target(forecasts_anomalies_sources, create_forecasts_anomalies_sources(
      ecmwf_forecasts_transformed
    , dates_to_process_all) |>
      group_by(date) |>
      tar_group()
    , iteration = "group")

  ## Forecast anomalies - branch over dates
  ## Each branch gets one row from forecasts_anomalies_sources (date + forecast file)
, tar_target(forecasts_anomalies, calculate_forecast_anomalies(
    forecasts_anomalies_sources
  , weather_historical_means
  , land_pixel_reference = elevation_preprocessed
  , forecasts_anomalies_directory
  , basename_template    = "forecast_anomaly_{date}.parquet"
  , forecast_intervals
  , overwrite            = parse_flag("OVERWRITE_FORECAST_ANOMALIES")
  , forecasts_anomalies_AWS)
  , pattern              = map(forecasts_anomalies_sources)
  , error                = "null"
  , format               = "file"
  , repository           = "local")

  ## Next step put weather_historical_means files on AWS.
, tar_target(forecasts_anomalies_AWS_upload, AWS_put_files(
    transformed_file_list = forecasts_anomalies
  , forecasts_anomalies_directory
  , overwrite            = parse_flag("OVERWRITE_FORECAST_ANOMALIES"))
  , pattern              = map(forecasts_anomalies)
  , error                = "null")

, tar_target(ndvi_historical_means_directory, create_data_directory(directory_path = "data/ndvi_historical_means"))

  ## Check if weather_historical_means parquet files already exists on AWS and can be loaded
  ## The only important one is the directory. The others are there to enforce dependencies.
, tar_target(ndvi_historical_means_AWS, AWS_get_folder(
    local_folder     = ndvi_historical_means_directory
  , skip_fetch       = Sys.getenv("SKIP_FETCH") == "TRUE"
  , sync_with_remote = TRUE)
  , error            = "null"
  , cue              = tar_cue("always"))

, tar_target(ndvi_historical_means, calculate_ndvi_historical_means(
    sentinel_ndvi_transformed
  , modis_ndvi_transformed
  , ndvi_historical_means_directory
  , basename_template = "ndvi_historical_mean_doy_{i}.parquet"
  , overwrite         = parse_flag("OVERWRITE_HISTORICAL_MEANS")
  , ndvi_historical_means_AWS)
  , format            = "file"
  , repository        = "local"
  , cue               = tar_cue_age(
      name            = ndvi_historical_means
      ## Recalculate every 6 months
    , age             = as.difftime(180, units = "days")))

  ## Next step put ndvi_historical_means files on AWS.
, tar_target(ndvi_historical_means_AWS_upload, AWS_put_files(
    transformed_file_list  = ndvi_historical_means
  , local_folder          = ndvi_historical_means_directory
  , overwrite             = parse_flag("OVERWRITE_HISTORICAL_MEANS"))
  , error                 = "null")

, tar_target(ndvi_anomalies_directory, create_data_directory(directory_path = "data/ndvi_anomalies"))

  ## Check if ndvi_anomalies_AWS parquet files already exists on AWS and can be loaded
  ## The only important one is the directory. The others are there to enforce dependencies.
, tar_target(ndvi_anomalies_AWS, AWS_get_folder(
    local_folder     = ndvi_anomalies_directory
  , skip_fetch       = Sys.getenv("SKIP_FETCH") == "TRUE"
  , sync_with_remote = TRUE)
  , error            = "null"
  , cue              = tar_cue("always"))

  ## NDVI anomalies - branch over months (ndvi_transformed) instead of dates
  ## Each branch processes all dates within that month
, tar_target(ndvi_anomalies, calculate_ndvi_anomalies(
    ndvi_transformed
  , ndvi_historical_means
  , ndvi_anomalies_directory
  , basename_template = "ndvi_anomaly_{date}.parquet"
  , overwrite         = parse_flag("OVERWRITE_NDVI_ANOMALIES")
  , ndvi_anomalies_AWS)
  , pattern           = map(ndvi_transformed)
  , error             = "null"
  , format            = "file"
  , repository        = "local")

  ## Next step put ndvi_anomalies files on AWS.
, tar_target(ndvi_anomalies_AWS_upload, AWS_put_files(
    transformed_file_list  = ndvi_anomalies
  , local_folder          = ndvi_anomalies_directory
  , overwrite             = parse_flag("OVERWRITE_NDVI_ANOMALIES"))
  , pattern               = map(ndvi_anomalies)
  , error                 = "null"))

## Join all data sources -----------------------------------------------------------
full_data_targets <- tar_plan(

  tar_target(africa_full_predictor_data_directory, create_data_directory(directory_path = "data/africa_full_predictor_data"))

  ## Assemble Africa Wide Model Data --------------------------------------------------

  ## Check if ndvi_anomalies_AWS parquet files already exists on AWS and can be loaded
  ## The only important one is the directory. The others are there to enforce dependencies.
, tar_target(africa_full_predictor_data_AWS,
    AWS_get_folder(
      local_folder     = africa_full_predictor_data_directory
    , skip_fetch       = Sys.getenv("SKIP_FETCH") == "TRUE"
    , sync_with_remote = TRUE)
    , error            = "null"
    , cue              = tar_cue("always"))

, tar_target(africa_full_predictor_data_sources_static,
    list(
      soil_preprocessed      = soil_preprocessed
    , aspect_preprocessed    = aspect_preprocessed
    , slope_preprocessed     = slope_preprocessed
    , glw_preprocessed       = glw_preprocessed
    , elevation_preprocessed = elevation_preprocessed
    , bioclim_preprocessed   = bioclim_preprocessed
    , landcover_preprocessed = landcover_preprocessed))

  ## Create intermediary target pairing each date with its predictor files.
  ## weather_anomalies is now ERA5T-sourced for all dates; no separate fallback needed.
  ## NDVI anomalies are carried forward up to 21 days when an exact-date file is absent.
, tar_target(africa_full_predictor_data_sources_temporal,
    create_africa_full_predictor_data_sources(
      forecasts_anomalies
    , weather_anomalies
    , ndvi_anomalies
    , dates_to_process_all) |>
      group_by(date) |>
      tar_group()
    , iteration = "group")

  ## Join all explanatory variable data sources using file based partitioning instead of hive
  ## error needs to be null here because some predictors (like wahis_outbreak_sources) aren't
  ## present in all times.
, tar_target(africa_full_predictor_data, file_partition_duckdb(
    temporal_sources  = africa_full_predictor_data_sources_temporal
  , static_sources    = africa_full_predictor_data_sources_static
  , local_folder      = africa_full_predictor_data_directory
  , basename_template = "africa_full_predictor_data_{date}.parquet"
  , overwrite         = parse_flag("OVERWRITE_AFRICA_FULL_PREDICTOR_DATA")
  , africa_full_predictor_data_AWS)
  , pattern           = map(africa_full_predictor_data_sources_temporal)
  , format            = "file"
  , repository        = "local"
  , error             = "null")

  ## Next step put combined_anomalies files on AWS.
, tar_target(africa_full_predictor_data_AWS_upload, AWS_put_files(
    transformed_file_list = africa_full_predictor_data
  , local_folder          = africa_full_predictor_data_directory
  , overwrite             = FALSE #parse_flag("OVERWRITE_AFRICA_FULL_PREDICTOR_DATA")
 # , first_date           = "2025-01-08"
 # , all_dates            = dates_to_process
 )
  , pattern               = map(africa_full_predictor_data)
  , error                 = "null"))

## Separate pipeline for building dataset for REMIT project ---------------------
REMIT_targets <- tar_plan(

  tar_target(REMIT_weather_vars, c(
    ## Theoretically can obtain these as well, but have to use nasapower::get_power and for that
    ## have to provide small regions of the map at a time. So if we want these two variables would
    ## have to jump through quite a few hoops which may not be worth it as these covariates have similar
    ## analogs elsewhere in the dataset
    #   "solar_rad"        = "ALLSKY_SFC_SW_DWN"
    # , "clouds"           = "CLOUD_AMT"
        "evapotrans"       = "EVPTRNS"
      , "soil_moisture"    = "GWETPROF"
      , "precipitation"    = "PRECTOTCORR"
      , "spec_humid_2m"    = "QV2M"
      , "rel_humid_2m"     = "RH2M"
      , "air_temp_avg"     = "T2M"
      , "air_temp_max"     = "T2M_MAX"
      , "air_temp_min"     = "T2M_MIN"
      , "air_temp_range"   = "T2M_RANGE"
      , "dewpoint"         = "T2MDEW"
      , "surface_temp_avg" = "TS"
      , "wind_speed_10m"   = "WS10M"
      , "wind_speed_2m"    = "WS2M"))

, tar_target(REMIT_weather_vars_N, c("evap_land" = "EVLAND", "surface_wetness" = "GWETTOP"))

, tar_target(nasa_weather_transformed_REMIT_N, fetch_and_transform_nasa_weather(
    months_to_process
  , REMIT_weather_vars_N
  , continent_raster_template
  , local_folder = "data/nasa_weather_transformed_REMIT_N"
  , basename_template = "nasa_weather_transformed_{months_to_process}.parquet"
  , endpoint = "https://power-datastore.s3.amazonaws.com/v10/daily/{year}/{month}/power_10_daily_{yyyymmdd}_merra2_lst.nc"
  , overwrite = FALSE
  , NULL
  , dates_to_process)
  , pattern = map(months_to_process)
  , error = "null"
  , format = "file")

, tar_target(nasa_weather_summarized_REMIT, summarize_REMIT_weather_data(
    dat          = nasa_weather_transformed_REMIT
  , weather_vars = REMIT_weather_vars
  , yrs          = seq(2005, 2025)
  , path_to_out  = "data/nasa_weather_summarized_REMIT")
  , pattern      = map(REMIT_weather_vars)
  , error        = "null"
  , format       = "file")

, tar_target(nasa_weather_REMIT_combined, combine_REMIT_weather_data(
    nasa_weather_summarized_REMIT
  , "data/nasa_weather_combined_REMIT"))

, tar_target(nasa_weather_REMIT_cleaned, impute_REMIT_weather_data(
    nasa_weather_REMIT_combined
  , "data/nasa_weather_combined_REMIT"))

, tar_target(africa_full_predictor_data_sources_REMIT, list(
    nasa_weather_REMIT_cleaned = nasa_weather_REMIT_cleaned
  , bioclim_preprocessed = bioclim_preprocessed
  , soil_preprocessed = soil_preprocessed
  , aspect_preprocessed = aspect_preprocessed
  , slope_preprocessed = slope_preprocessed
  , glw_preprocessed = glw_preprocessed
  , elevation_preprocessed = elevation_preprocessed
  , landcover_preprocessed = landcover_preprocessed))

, tar_target(africa_full_predictor_data_REMIT, {
    joined_df <- map(africa_full_predictor_data_sources_REMIT %>% unlist()
                     , .f = function(this_file) {
                       arrow::read_parquet(this_file)
                       }) |>
      reduce(., left_join, by = c("x", "y"))

    joined_df <- joined_df[complete.cases(joined_df), ]

    joined_df |> arrow::write_parquet(
      "data/africa_full_predictor_data_REMIT/REMIT_data.parquet"
    , compression = "gzip", compression_level = 5
    )

    return("data/africa_full_predictor_data_REMIT/REMIT_data.parquet")

  })

)


## List targets -----------------------------------------------------------------
## all_targets() doesn't work with tarchetypes like tar_change().
list(
  static_targets
, dynamic_targets
, derived_data_targets
, full_data_targets
)
