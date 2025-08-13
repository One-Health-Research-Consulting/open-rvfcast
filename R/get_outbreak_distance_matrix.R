#' Compute the Outbreak Distance Matrix
#'
#' This function computes the geographical distance between outbreak locations and 
#' raster grid cells, transforming these distances into a connectivity matrix.
#'
#' @author Nathan C. Layman
#'
#' @param wahis_outbreaks A dataframe of outbreak locations with columns 'longitude' and 'latitude'.
#' @param wahis_raster_template A raster template on which the function calculates distances.
#' @param within_km Numeric, the radius within which to calculate distances. Defaults to 500.
#' @param beta_dist Numeric, a coefficient used to transform distances into a connectivity matrix. Defaults to 0.01.
#'
#' @return A matrix with rows representing grid cells and columns representing outbreak locations,
#'         where each cell's value is a transformed measure of its distance from the outbreak.
#'
#' @note Distances are calculated with the Vincenty measure, 
#'       transformed using an exponential decay function, and outside of the specified radius are set to zero.
#'
#' @examples
#' get_outbreak_distance_matrix(wahis_outbreaks = wahis_df,
#'                    wahis_raster_template = raster_template,
#'                    within_km = 500,
#'                    beta_dist = 0.01)
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
