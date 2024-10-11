#' Retrieve Daily Outbreak History
#'
#' This function computes the outbreak history of specific diseases on a daily basis based
#' on the given dates. It ultimately returns the file path of the resulting dataset.
#'
#' @author Nathan C. Layman
#'
#' @param dates_df A dataframe of the dates for which the outbreak history is to be calculated
#' @param wahis_outbreaks The outbreak naming convention
#' @param wahis_distance_matrix The inter locality distance matrix
#' @param wahis_raster_template The template for the raster mapping
#' @param output_dir The directory where the final dataset will be outputted to, default is 'data/outbreak_history_dataset'.
#' @param output_filename The name of the output file, default is 'outbreak_history.parquet'
#' @param beta_time The rate of exponential decline to use for the kernel
#' @param max_years The maximum number of years to consider for the decay function. Default is 10.
#' @param recent The cutoff (in years) to distinguish recent from old outbreaks. Default is 3/12 (3 months).
#' @param overwrite A boolean value indicating to overwrite the file if it already exists. Default is FALSE.
#' @param ... Other ignored parameters for compatibility
#'
#' @return A string containing the filepath of the computed outbreak history file.
#'
#' @note This function computes the outbreak history for the specified dates and saves them
#' into the specified directory. If a file already exists and overwrite parameter is set to
#' FALSE, it simply returns the filepath of the existing file.
#'
#' @examples
#' get_daily_outbreak_history(dates_df = dates,
#'                            wahis_outbreaks = outbreaks,
#'                            wahis_distance_matrix = distance_matrix,
#'                            wahis_raster_template = raster_template,
#'                            output_dir = './data',
#'                            output_filename = 'outbreak_history.parquet',
#'                            beta_time = 0.5, max_years = 10, recent = 3/12, overwrite = FALSE)
#'
#' @export
get_daily_outbreak_history <- function(dates_df,
                                       wahis_outbreaks,
                                       wahis_distance_matrix,
                                       wahis_raster_template,
                                       output_dir = "data/outbreak_history_dataset",
                                       output_filename = "outbreak_history.parquet",
                                       beta_time = 0.5,
                                       max_years = 10,
                                       recent = 3/12,
                                       overwrite = FALSE,
                                       ...) {
  
  # Ensure only one year in dates provided
  year <- unique(dates_df$year)
  stopifnot(length(year) == 1)
  
  # Create directory if it does not yet exist
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Unwrap raster template
  wahis_raster_template <- terra::unwrap(wahis_raster_template)
  
  # Set up safe way to read parquet files
  error_safe_read_parquet <- possibly(arrow::read_parquet, NULL)
  
  # Check if output file already exists and can be loaded
  outbreak_history_filename <- file.path(output_dir, glue::glue("{tools::file_path_sans_ext(output_filename)}_{year}.{tools::file_ext(output_filename)}"))
  
  # Check if outbreak_history file exist and can be read and that we don't want to overwrite them.
  if(!is.null(error_safe_read_parquet(outbreak_history_filename)) & !overwrite & year != year(Sys.time())) {
    message("preprocessed outbreak history parquet file already exists and can be loaded, skipping download and processing")
    return(outbreak_history_filename)
  }
  
  # This is the computationally intensive step get_outbreak_history()
  daily_outbreak_history <- map_dfr(dates_df$date, ~get_outbreak_history(date = .x,
                                                                         wahis_outbreaks,
                                                                         wahis_distance_matrix,
                                                                         wahis_raster_template,
                                                                         beta_time = beta_time,
                                                                         max_years = max_years,
                                                                         recent = recent))

  daily_recent_outbreak_history <- terra::rast(daily_outbreak_history$recent_outbreaks_rast)
  daily_old_outbreak_history <- terra::rast(daily_outbreak_history$old_outbreaks_rast)
  
  recent_xy <- as.data.frame(daily_recent_outbreak_history, xy = TRUE) |> 
    as_tibble() |> 
    pivot_longer(-c(x,y), names_to = "date", values_to = "weight") |> 
    mutate(time_frame = "recent")
  
  old_xy <- as.data.frame(daily_old_outbreak_history, xy = TRUE) |> 
    as_tibble() |> 
    pivot_longer(-c(x,y), names_to = "date", values_to = "weight") |> 
    mutate(time_frame = "old")
  
  arrow::write_parquet(bind_rows(recent_xy, old_xy), outbreak_history_filename, compression = "gzip", compression_level = 5)
  
  return(outbreak_history_filename)
  
}

#' Extracting Outbreak History from WAHIS Data
#'
#' This function extracts outbreak history from World Animal Health Information System (WAHIS). 
#' It uses the end_date of each outbreak, logarithm of the number of cases (if available), and exponential of the years since the end_date to weight the outbreak 
#' and generate rasters for recent and old outbreaks based on the provided recent period.
#'
#' @author Nathan C. Layman
#'
#' @param date The reference date for extracting outbreak history.
#' @param wahis_outbreaks A data frame of WAHIS outbreaks.
#' @param wahis_distance_matrix The WAHIS spatial distance matrix.
#' @param wahis_raster_template A raster template.
#' @param beta_time Exponential decay rate for time weighting of outbreaks, default is 0.5.
#' @param max_years Maximum years to consider an outbreak old, default is 10 years.
#' @param recent Period (in years) to consider an outbreak recent, default is 1/6 year i.e., approx. 2 months.
#'
#' @return A tibble containing the date, and raster stacks for recent and old outbreaks.
#'
#' @note This function transforms outbreak history into a spatial-temporal data that can be analyzed with other covariates.
#'
#' @examples
#' get_outbreak_history(date = "2018-01-01", wahis_outbreaks = wahis_outbreaks,
#'                      wahis_distance_matrix = wahis_distance_matrix,
#'                      wahis_raster_template = wahis_raster_template,
#'                      beta_time = 0.5, max_years = 10, recent = 2/12)
#'
#' @export
get_outbreak_history <- function(date,
                                 wahis_outbreaks, 
                                 wahis_distance_matrix,
                                 wahis_raster_template,
                                 beta_time = 0.5,
                                 max_years = 10,
                                 recent = 1/6) {
  
  message(paste("Extracting outbreak history for", as.Date(date)))
  
  outbreak_history <- wahis_outbreaks |> 
    arrange(outbreak_id) |>
    mutate(end_date = pmin(date, end_date, na.rm = T),
           years_since = as.numeric(as.duration(date - end_date), "years")) |>
    filter(date > end_date, years_since < max_years & years_since >= 0) |>
    mutate(time_weight = ifelse(is.na(cases), 1, log10(cases + 1))*exp(-beta_time*years_since))
  
  old_outbreaks <- outbreak_history |> filter(years_since >= recent) |> 
    combine_weights(wahis_distance_matrix, wahis_raster_template) |> setNames(as.Date(date))
  
  recent_outbreaks <- outbreak_history |> filter(years_since < recent) |> 
    combine_weights(wahis_distance_matrix, wahis_raster_template) |> setNames(as.Date(date))
  
  tibble(date = as.Date(date), 
         recent_outbreaks_rast = list(recent_outbreaks),
         old_outbreaks_rast = list(old_outbreaks))
}

#' Combine Outbreaks, Distance Matrix and Raster Template 
#'
#' This function combines the outbreaks, the corresponding distance matrix and raster template. It computes the weights
#' based on the outbreak time and updates the raster template with these computed weights. If no outbreaks are provided, 
#' it returns the raster template without any modifications.
#'
#' @author Nathan C. Layman
#'
#' @param outbreaks A dataset of outbreaks. 
#' @param wahis_distance_matrix A distance matrix corresponding to the outbreaks.
#' @param wahis_raster_template The raster template which is to be updated based on the weights.
#'
#' @return The updated raster template with newly calculated weights. If no outbreaks are given, it returns the unmodified raster template.
#'
#' @note This function computes the weights using the outbreak time and the distance matrix. After computing the weights,
#' it updates the raster template with these weights. If no outbreaks are given, it simply returns the raster template with no modifications.
#'
#' @examples
#' combine_weights(outbreaks = outbreak_db,
#'                 wahis_distance_matrix = distance_matrix,
#'                 wahis_raster_template = raster_template)
#'
#' @export
combine_weights <- function(outbreaks, 
                            wahis_distance_matrix, 
                            wahis_raster_template) {
 
  if(!nrow(outbreaks)) {
    wahis_raster_template[!is.na(wahis_raster_template)] <- 0
    return(wahis_raster_template)
  }
  # Multiply time weights by distance weights
  
  # Super fast matrix multiplication step. This is the secret sauce.
  # Performs sweep(outbreaks$time_weight, "*") and rowsums() all in once go
  # and indexes the wahis_distance_matrix (which was calculated only once)
  # instead of re-calculating distances every day. These changes
  # sped it up from needing 7 hours to calculate the daily history for
  # 2010 to doing the same thing in 4.3 minutes.
  weights <- wahis_distance_matrix[,outbreaks$outbreak_id] |>
    as.matrix() |> Rfast::mat.mult(as.matrix(outbreaks$time_weight))
  
  idx <- which(!is.nan(wahis_raster_template[]))
  wahis_raster_template[idx] <- weights
  
  wahis_raster_template
}
