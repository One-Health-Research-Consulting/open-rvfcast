# Setup -------------------------------------------------------------------
suppressPackageStartupMessages(source("packages.R"))
library(leaflet)

set.seed(333)

# load raster template 
continent_raster_template <- rast(tar_read(continent_raster_template))
raster_crs <- crs(continent_raster_template) 

# load selected model dates - user will be able to select from these
dates_to_process <- tar_read(dates_to_process)

# random select date
user_date <- sample(dates_to_process, 1)

# leaflet base
leafmap <- leaflet() |>  
  setView(lng = 21, lat = 5, zoom = 2) |> 
  addTiles()

# NDVI --------------------------------------------------------------------
ndvi_date_lookup <- tar_read(ndvi_date_lookup)

# actual data 
## subset for user selected date and read in to memory as raster
r_ndvi <- ndvi_date_lookup |> 
  dplyr::filter(purrr::map_lgl(lookup_dates, ~user_date %in% .)) |> 
  dplyr::pull(filename) |> 
  arrow::open_dataset() |> 
  dplyr::select(x, y, ndvi) |> 
  dplyr::collect() |> 
  terra::rast() |> 
  terra::`crs<-`(raster_crs)

pal_ndvi <- colorNumeric(c(rev(grDevices::terrain.colors(50))), values(r_ndvi),  na.color = "transparent")

# map
leafmap |> 
  addRasterImage(r_ndvi, colors = pal_ndvi) |> 
  addLegend(pal = pal_ndvi, 
            values = values(r_ndvi),
            title = "NDVI")

# anomalies
## subset for user selected date and read in to memory as raster
r_ndvi_anomalies <- arrow::open_dataset(ndvi_anomalies) |> 
  dplyr::filter(date == user_date) |> 
  dplyr::select(x, y, anomaly_ndvi_30) |> 
  dplyr::collect() |> 
  terra::rast() |> 
  terra::`crs<-`(raster_crs)

v_ndvi_anomalies <- c(values(r_ndvi_anomalies))
v_ndvi_anomalies = v_ndvi_anomalies[!is.na(v_ndvi_anomalies)]

pal_ndvi_anomalies <- colorNumeric(palette = c("#783f04", "#f6efee","green"), domain = c(-1, 0 , 1),  na.color = "transparent")

leafmap |> 
  addRasterImage(r_ndvi_anomalies, colors = pal_ndvi_anomalies) |> 
  addLegend(pal = pal_ndvi_anomalies, 
            values = values(r_ndvi_anomalies),
            title = "NDVI")



