#' Calculate Outbreak Distance Matrix
#'
#' This function calculates a distance matrix for outbreaks based on given parameters. The function takes
#' outbreak data, a raster template, a distance in kilometers, and a beta distribution values as arguments.
#' It then calculates the distance values, applies conditions, and returns a matrix of distances.
#'
#' @author Nathan C. Layman
#'
#' @param wahis_outbreaks Dataframe containing outbreak data.
#' @param wahis_raster_template A raster template for structuring the data.
#' @param within_km The maximum distance in kilometers to be considered for the matrix. Default is 500.
#' @param beta_dist A value defining beta distribution. Default is 0.01.
#'
#' @return A matrix containing calculated distance values based on outbreak data.
#'
#' @note This function calculates distance values based on vincenty measure, applies conditions, and returns a 
#' matrix of distances. If the distance is larger than a given threshold, it is set as NA. Distance values are then adjusted
#' based on a beta distribution, and NA values are set as 0.
#'
#' @examples
#' get_outbreak_distance_matrix(wahis_outbreaks = outbreak_data,
#'                    wahis_raster_template = raster_template,
#'                    within_km = 100,
#' beta_dist = 0.05)
#'
#' @export
get_outbreak_distance_matrix <- function(wahis_outbreaks, wahis_raster_template, within_km = 500, beta_dist = 0.01) {
  
  wahis_raster_template <- wahis_raster_template |> terra::unwrap()
  
  xy <- as.data.frame(wahis_raster_template, xy = TRUE) |> select(y, x) |> rename(longitude = x, latitude = y)
  
  # For each outbreak origin identify the distance to every other point in Africa within `within_km` km
  dist_mat <- geodist::geodist(xy, wahis_outbreaks |> arrange(outbreak_id), measure = "vincenty") # Good enough for our purposes and _much_ faster than s2
  
  # Drop all distances greater than within_km
  # Not sure why we need to do this given choice of beta_dist
  dist_mat[dist_mat > (within_km * 1000)] <- NA
  
  # Calculate a weighting factor based on distance. Note we haven't included log10 cases yet.
  # This is negative exponential decay - points closer to the origin will be 1 and those farther
  # away will be closer to zero mediated by beta_dist.
  dist_mat <- exp(-beta_dist*dist_mat/1000)
  
  # Facilitate matrix math later
  dist_mat[is.na(dist_mat)] <- 0
  
  dist_mat
}