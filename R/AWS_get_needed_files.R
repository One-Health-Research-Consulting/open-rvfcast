#' Download only the S3 files required for specific processing dates.
#'
#' This function: a) lists the S3 folder once; b) filters to files whose 
#' embedded date metadata covers \code{dates} -- internal function logic is differnet
#' depending on the data source as each has its own file needs / file name
#' conventions; c) downloads only those files. 
#' Function is designed to run and "fail gracefully" when credentials are missing 
#' or no matching files are found, allowing downstream targets to attempt 
#' primary-source downloads.
#'
#' @param s3_folder Character. S3 folder prefix (mirrors the local path,
#'   e.g. \code{"data/era5t_weather_transformed"})
#' @param dates Date or character vector of processing dates
#' @param data_type Character. Controls the filename-to-date matching logic
#'   One of: \code{"sentinel_ndvi"}, \code{"modis_ndvi"},
#'   \code{"ndvi_transformed"}, \code{"era5t_weather"},
#'   \code{"ecmwf_forecasts"}, \code{"weather_historical_means"},
#'   \code{"weather_anomalies"}, \code{"ndvi_historical_means"},
#'   \code{"ndvi_anomalies"}, \code{"forecast_anomalies"},
#'   \code{"africa_full_predictor"}
#' @param skip_fetch Logical. If TRUE, skip all downloads and return character(0)
#' @param ... Unused, but present for compatibility with AWS_get_folder calls
#' @return Character vector of local file paths downloaded or already present
#' @author Morgan Kain
#' @export
AWS_get_needed_files <- function(s3_folder, dates, data_type, skip_fetch = FALSE, ...) {

  if (skip_fetch) return(character(0))

  ## Check for credentials and return a message if they are not found. In this case S3 
   ## cant be grabbed, fall back to download of data from source (really slow)
  if (any(Sys.getenv(c("AWS_ACCESS_KEY_ID", "AWS_SECRET_ACCESS_KEY", "AWS_REGION")) == "")) {
    message("AWS credentials not set; skipping targeted download of ", s3_folder)
    return(character(0))
  }

  ## Rare pipeline would be run with no new dates, but add a safety anyway
  if (length(dates) == 0) {
    cat("No dates to process; skipping download of", s3_folder, "\n")
    return(character(0))
  }

  ## Get all of the filenames in the S3 bucket for this data source
  all_s3_files <- AWS_get_filenames(s3_folder)

  ## Return empty vector if the folder doesn't have any files
  if (is.null(all_s3_files) || length(all_s3_files) == 0) {
    cat("No files found on S3 under", s3_folder,
        "; downstream processing will download from source\n")
    return(character(0))
  }

  ## Function that determines what files are needed for this dataset for these dates
  needed_files <- get_needed_s3_filenames(all_s3_files, dates, data_type)

  ## Return empty vector if no relevant files exist in the S3 bucket for this
   ## data type and these dates
  if (length(needed_files) == 0) {
    cat("No S3 files matched", data_type, "for dates [",
        paste(format(as.Date(dates)), collapse = ", "), "]\n")
    return(character(0))
  }

  ## Return
  AWS_get_files(needed_files)
}


#' Filter S3 file paths to those covering specific processing dates
#'
#' Each data_type has a distinct filename convention that encodes date coverage.  
#' This helper function extracts that metadata from each filename and returns
#' only files whose coverage overlaps dates (dates_to_process).
#'
#' File conventions by type:
#' \itemize{
#'   \item \code{sentinel_ndvi}: \code{transformed_sentinel_NDVI_\{start\}_to_\{end\}.parquet} — 10-day spans
#'   \item \code{modis_ndvi}: \code{transformed_modis_NDVI_\{start\}.parquet} — 16-day composites
#'   \item \code{ndvi_transformed}: \code{ndvi_transformed_\{year\}_\{month\}.parquet}
#'   \item \code{era5t_weather}: \code{era5t_weather_transformed_\{year\}-\{month\}.parquet}
#'   \item \code{ecmwf_forecasts}: \code{ecmwf_seasonal_forecast_\{month\}_\{year\}.parquet}
#'   \item \code{weather_historical_means}: \code{weather_historical_mean_doy_\{doy\}.parquet} — only DOYs in \code{dates}
#'   \item \code{weather_anomalies}: \code{weather_anomaly_\{date\}.parquet}
#'   \item \code{ndvi_historical_means}: \code{ndvi_historical_mean_doy_\{doy\}.parquet} — only DOYs in \code{dates}
#'   \item \code{ndvi_anomalies}: \code{ndvi_anomaly_\{date\}.parquet} — 21-day look-back for carry-forward
#'   \item \code{forecast_anomalies}: \code{forecast_anomaly_\{date\}.parquet}
#'   \item \code{africa_full_predictor}: \code{africa_full_predictor_data_\{date\}.parquet}
#' }
#'
#' @param s3_files Character vector of S3 object keys from \code{AWS_get_filenames}
#' @param dates Date or character vector of processing dates
#' @param data_type Character string naming the data type; see AWS_get_needed_files for valid values.
#' @return Filtered subset of s3_files.
#' @author Morgan Kain
#' @export
get_needed_s3_filenames <- function(s3_files, dates, data_type) {

  ## Be extra careful that the dates are dates and not a character
  dates <- as.Date(dates)

  ## Depending on data type, do a different filter of the full list of file names
   ## given dates_to_process
  switch(data_type,

    #### sentinel_ndvi ------------------------------------------------------------
    ## 10-day windows: transformed_sentinel_NDVI_{start}_to_{end}.parquet
    sentinel_ndvi = {
      Filter(function(f) {
        m          <- regmatches(f, regexpr("(\\d{4}-\\d{2}-\\d{2})_to_(\\d{4}-\\d{2}-\\d{2})", f))
        if (length(m) == 0) return(FALSE)
        
        parts      <- strsplit(m, "_to_")[[1]]
        file_start <- as.Date(parts[1])
        file_end   <- as.Date(parts[2])
        
        any(dates >= file_start & dates <= file_end)
      }, s3_files)
    }

    #### modis_ndvi ---------------------------------------------------------------
    ## 16-day composites: transformed_modis_NDVI_{start_date}.parquet
  , modis_ndvi = {
      Filter(function(f) {
        m          <- regmatches(f, regexpr("NDVI_(\\d{4}-\\d{2}-\\d{2})", f))
        if (length(m) == 0) return(FALSE)
        
        file_start <- as.Date(sub("NDVI_", "", m))
        file_end   <- file_start + 15L
        
        any(dates >= file_start & dates <= file_end)
      }, s3_files)
    }

    #### ndvi_transformed ---------------------------------------------------------
    ## Monthly: ndvi_transformed_{year}_{month}.parquet
  , ndvi_transformed = {
      needed <- unique(format(dates, "%Y_%m"))
      
      Filter(function(f) any(vapply(needed, grepl, logical(1L), x = f)), s3_files)
    }

    #### era5t_weather_transformed ------------------------------------------------
    ## Monthly: era5t_weather_transformed_{year}-{month}.parquet
  , era5t_weather = {
      needed <- unique(format(dates, "%Y-%m"))
      
      Filter(function(f) any(vapply(needed, grepl, logical(1L), x = f)), s3_files)
    }

    #### ecmwf_forecasts ----------------------------------------------------------
    ## Monthly (month-first): ecmwf_seasonal_forecast_{month}_{year}.parquet
    ## Filenames use lubridate::month() (unpadded integer), so "1_2026" not "01_2026".
    ## Each file covers ~6 months of lead time, so need to look back 6 months from the 
    ## earliest date. Regex extraction avoids grepl substring false-matches 
    ## ("1_2026" inside "11_2026").
  , ecmwf_forecasts = {
      lookback_start <- lubridate::floor_date(min(dates) - 180L, "month")
      window_end     <- lubridate::floor_date(max(dates), "month")
      all_months     <- seq.Date(lookback_start, window_end, by = "month")
      needed         <- paste0(lubridate::month(all_months), "_", lubridate::year(all_months))
      
      Filter(function(f) {
        m <- regmatches(f, regexpr("forecast_(\\d+)_(\\d{4})\\.parquet", f))
        if (length(m) == 0) return(FALSE)
        
        file_key <- paste0(
          sub("forecast_(\\d+)_(\\d{4})\\.parquet", "\\1", m), "_",
          sub("forecast_(\\d+)_(\\d{4})\\.parquet", "\\2", m)
        )
        file_key %in% needed
      }, s3_files)
    }

    #### weather_historical_means -------------------------------------------------
    ## One per DOY: weather_historical_mean_doy_{i}.parquet (i is unpadded, 1–366)
    ## Must cover dates_to_process AND the full forecast horizon (150 days out):
    ## calculate_forecast_anomalies reads historical means for every DOY spanned by
    ## each of the 5 lead intervals, so DOYs from `date` through `date + 149` are needed.
    ## Regex extraction avoids grepl substring false-matches ("_doy_1." inside "_doy_10.").
  , weather_historical_means = {
      forecast_horizon   <- 150L
      all_forecast_dates <- seq.Date(min(dates), max(dates) + forecast_horizon - 1L, by = "day")
      needed_doys        <- unique(lubridate::yday(all_forecast_dates))
      
      Filter(function(f) {
        m <- regmatches(f, regexpr("_doy_(\\d+)\\.parquet$", f))
        if (length(m) == 0) return(FALSE)
        
        as.integer(sub("_doy_(\\d+)\\.parquet$", "\\1", m)) %in% needed_doys
      }, s3_files)
    }

    #### ndvi_historical_means ----------------------------------------------------
    ## One per DOY: ndvi_historical_mean_doy_{i}.parquet (i is unpadded, 1–366)
    ## Same regex strategy as weather_historical_means to avoid substring false-matches.
  , ndvi_historical_means = {
      needed_doys <- unique(lubridate::yday(dates))
      
      Filter(function(f) {
        m <- regmatches(f, regexpr("_doy_(\\d+)\\.parquet$", f))
        if (length(m) == 0) return(FALSE)
        
        as.integer(sub("_doy_(\\d+)\\.parquet$", "\\1", m)) %in% needed_doys
      }, s3_files)
    }

    #### weather_anomalies --------------------------------------------------------
    ## One per date: weather_anomaly_{date}.parquet
  , weather_anomalies = {
      needed <- as.character(dates)
      
      Filter(function(f) any(vapply(needed, grepl, logical(1L), x = f)), s3_files)
    }

    #### ndvi_anomalies -----------------------------------------------------------
    ## One per date, but ndvi values are carried forward up to 21 days when a date
    ## file is absent; include the prior 21 days so that carry-forward joins work:
    ## ndvi_anomaly_{date}.parquet
  , ndvi_anomalies = {
      date_range <- seq.Date(min(dates) - 21L, max(dates), by = "day")
      needed     <- as.character(date_range)
      
      Filter(function(f) any(vapply(needed, grepl, logical(1L), x = f)), s3_files)
    }

    #### forecast_anomalies -------------------------------------------------------
    ## One per date: forecast_anomaly_{date}.parquet
  , forecast_anomalies = {
      needed <- as.character(dates)
      
      Filter(function(f) any(vapply(needed, grepl, logical(1L), x = f)), s3_files)
    }

    #### africa_full_predictor_data -----------------------------------------------
    ## One per date: africa_full_predictor_data_{date}.parquet
  , africa_full_predictor = {
      needed <- as.character(dates)
      
      Filter(function(f) any(vapply(needed, grepl, logical(1L), x = f)), s3_files)
    }

    #### unknown ------------------------------------------------------------------
  , stop("Unknown data_type: '", data_type, "'. See ?AWS_get_needed_files for valid types.")
  
  )
}


#' Delete all parquet files from intermediate local directories after S3 upload
#'
#' Called after africa_full_predictor_data_AWS_upload completes. Removes
#' all .parquet files from the ten intermediate data directories so that
#' the next monthly run starts with a clean local state and re-downloads only
#' the files it needs via the smart-download AWS_get_needed_files targets.
#'
#' The final africa_full_predictor_data directory is intentionally
#' excluded; these files are used in the next step of the pipeline to generate
#' the model data file which is used in the third step of the pipeline
#' Static-data directories (soil, glw, etc.) are also excluded because they are 
#' small, infrequently updated, and not re-generated each run.
#'
#' Set SKIP_FETCH=TRUE in .env to suppress cleanup during local development runs
#' where re-downloading would be wasteful.
#'
#' @param sentinel_ndvi_transformed_directory Character. Path to sentinel NDVI parquets.
#' @param modis_ndvi_transformed_directory Character. Path to MODIS NDVI parquets.
#' @param ndvi_transformed_directory Character. Path to combined NDVI parquets.
#' @param era5t_weather_transformed_directory Character. Path to ERA5T weather parquets.
#' @param ecmwf_forecasts_transformed_directory Character. Path to ECMWF forecast parquets.
#' @param weather_historical_means_directory Character. Path to weather DOY means.
#' @param weather_anomalies_directory Character. Path to weather anomaly parquets.
#' @param forecasts_anomalies_directory Character. Path to forecast anomaly parquets.
#' @param ndvi_historical_means_directory Character. Path to NDVI DOY means.
#' @param ndvi_anomalies_directory Character. Path to NDVI anomaly parquets.
#' @param ... africa_full_predictor_data_AWS_upload is passed here to enforce dependency ordering; the values are not used.
#' @return Integer. Total number of parquet files deleted across all directories.
#' @author Morgan Kain
#' @export
cleanup_local_intermediate_files <- function(
  sentinel_ndvi_transformed_directory
, modis_ndvi_transformed_directory
, ndvi_transformed_directory
, era5t_weather_transformed_directory
, ecmwf_forecasts_transformed_directory
, weather_historical_means_directory
, weather_anomalies_directory
, forecasts_anomalies_directory
, ndvi_historical_means_directory
, ndvi_anomalies_directory
, ...
) {

  ## Skip the whole thing if SKIP_FETCH == TRUE
  if (Sys.getenv("SKIP_FETCH") == "TRUE") {
    message("SKIP_FETCH is TRUE; skipping local intermediate file cleanup")
    return(0L)
  }

  ## All directories
  dirs <- c(
    sentinel_ndvi_transformed_directory
  , modis_ndvi_transformed_directory
  , ndvi_transformed_directory
  , era5t_weather_transformed_directory
  , ecmwf_forecasts_transformed_directory
  , weather_historical_means_directory
  , weather_anomalies_directory
  , forecasts_anomalies_directory
  , ndvi_historical_means_directory
  , ndvi_anomalies_directory
  )

  ## Track deleted files so we can return something of some usefulness
  n_deleted <- sum(vapply(dirs, function(d) {
    if (!dir.exists(d)) return(0L)
    
    files <- list.files(d, pattern = "\\.parquet$", full.names = TRUE)
    if (length(files) == 0L) return(0L)
    
    file.remove(files)
    length(files)
  }, integer(1L)))

  message(glue::glue("Local cleanup: removed {n_deleted} intermediate parquet files"))
  n_deleted
  
}
