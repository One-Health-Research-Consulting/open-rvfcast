#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param bounding_boxes
#' @return
#' @author Whitney Bagge
#' @export

preprocess_soil <- function(unzipped_soil, bounding_boxes) {
  
  library(terra)
  library(sf)
  library(RSQLite)
  
  hwsd <- rast("./data/soil/HWSD2.bil")
  
  hwsd_bounded <- terra::crop(hwsd, bounding_boxes)
  
  print(paste("UTM zone:", utm.zone <-
                floor(((sf::st_bbox(hwsd_bounded)$xmin +
                          sf::st_bbox(hwsd_bounded)$xmax)/2 + 180)/6)
              + 1))
  
  (epsg <- 32600 + utm.zone)
  
  hwsd_bounded.utm <- project(hwsd_bounded, paste0("EPSG:", epsg), method = "near")
  
  
 # m <- dbDriver("SQLite")
 # con <- dbConnect(m, dbname="HWSD2.sqlite")
 # dbListTables(con)
 # 
 # print(dbGetQuery(con, "select * from HWSD2_SMU_METADATA"), width=100)
 # 
}
