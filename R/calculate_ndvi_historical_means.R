#' Calculate historical means for Normalized Difference Vegetation Index (NDVI)
#'
#' This function calculates historical means for NDVI based on provided
#' transformed sentinel and modis NDVI data, then saves the results in a specified directory.
#'
#' @author Nathan Layman
#'
#' @param sentinel_ndvi_transformed The transformed NDVI data from Sentinel.
#' @param modis_ndvi_transformed The transformed NDVI data from MODIS.
#' @param ndvi_historical_means_directory The directory where the results will be saved.
#' @param ndvi_historical_means_AWS The historical NDVI means from AWS.
#' @param ... Further arguments passed to or from other methods.
#'
#' @return A vector of filepaths to saved parquet files of calculated historical NDVI means.
#'
#' @note 
#' This function works by grouping the provided NDVI data by x,y coordinates and day of year (doy),
#' then calculating the mean and standard deviation for each group. The results are then saved
#' into parquet files (one for each day of the year).
#'
#' @examples
#' calculate_ndvi_historical_means(sentinel_ndvi_transformed = "path_to_sentinel_data",
#'                                 modis_ndvi_transformed = "path_to_modis_data",
#'                                 ndvi_historical_means_directory = "path_to_output_directory",
#'                                 ndvi_historical_means_AWS = "path_to_AWS_means")
#'
#' @export
calculate_ndvi_historical_means <- function(sentinel_ndvi_transformed,
                                            modis_ndvi_transformed,
                                            ndvi_historical_means_directory,
                                            ndvi_historical_means_AWS,
                                            ...) {
  
  # Open dataset can handle multi-file datasets larger than can
  # fit in memory
  ndvi_data <- arrow::open_dataset(c(modis_ndvi_transformed, sentinel_ndvi_transformed))
  
  # Fast because we can avoid collecting until write_parquet
  ndvi_historical_means <- map_vec(1:366, .progress = TRUE, function(i) {
      
    filename <- file.path(ndvi_historical_means_directory, 
                          glue::glue("ndvi_historical_mean_doy_{i}.gz.parquet"))
    
    ndvi_data |>
      filter(doy == i) |>
      group_by(x, y, doy) |> 
      summarize(ndvi_sd = sd(ndvi, na.rm = T),
                ndvi = mean(ndvi, na.rm = T),
                .groups = "drop") |>
      filter(!is.na(ndvi_sd)) |> # Drop constant values (ndvi_sd == NA)
      arrow::write_parquet(filename, compression = "gzip", compression_level = 5)
        
      # Check plot
      # ggplot(ndvi_data, aes(x = x, y = y)) +
      #   geom_tile(aes(fill = ndvi), size = 5) +  # Points colored by NDVI
      #   scale_fill_viridis_c() +  # Gradient for NDVI values
      #   labs(
      #     title = glue::glue("Combined NDVI Historical Means, doy: {i}"),
      #     x = "Longitude",
      #     y = "Latitude",
      #     color = "NDVI"
      #   ) +
      #   theme_minimal()
    
    filename
    })
    
  return(ndvi_historical_means)
}
