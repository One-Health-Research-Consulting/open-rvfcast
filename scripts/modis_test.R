
# Minimal script to download MOD13A2 v061 data for 2010
# Load required packages
library(modisfast)
library(terra)
library(sf)

# 1. Define your template raster and create ROI
template_raster <- tar_read(continent_raster_template) |> terra::unwrap()

# Convert extent to sf polygon (modisfast requirement)
roi <- st_as_sf(st_as_sfc(st_bbox(template_raster)))
roi$id <- "study_area"  # modisfast requires an 'id' column
roi <- st_transform(roi, 4326)  # modisfast expects WGS84

# 2. Define collection, time range, and variables
collection <- "MOD13A2.061"
time_range <- as.Date(c("2010-01-01", "2010-12-31"))

# MOD13A2 NDVI variable:
variables <- "_1_km_16_days_NDVI"

# 3. Login to NASA EarthData (replace with your credentials)
log <- mf_login(credentials = c(Sys.getenv("EARTHDATA_USERNAME"), Sys.getenv("EARTHDATA_PASSWORD")))

# 4. Get URLs for the data
urls <- mf_get_url(
  collection = collection,
  variables = variables,
  roi = roi,
  time_range = time_range
)

# 5. Download the data
# Specify download directory (optional, defaults to temp directory)
download_dir <- "modis_data"  # Change as needed
dir.create(download_dir, showWarnings = FALSE)

res_dl <- mf_download_data(urls, 
                          path = download_dir,
                          parallel = TRUE)

# 6. Import the data as SpatRaster
r <- mf_import_data(
  path = download_dir,
  collection = collection,
  proj_epsg = st_crs(template_raster)$epsg  # Match your template raster CRS
)

# Print summary
print(paste("Downloaded", nlyr(r), "NDVI layers"))
print(paste("Temporal range:", min(time(r)), "to", max(time(r))))
print(paste("CRS:", crs(r)))

# Optional: Save as a single multi-layer raster
writeRaster(r, "MOD13A2_NDVI_2010.tif", overwrite = TRUE)







# MODIS collections, variables and time ranges
collection <- "MOD13A3.061"
variables <- c("_1_km_monthly_NDVI")
time_range <- as.Date(c("2023-01-01", "2023-12-31"))

africa_polygon <- tar_read(continent_polygon) |> st_cast("POLYGON") |> rename(id = country)

urls <- mf_get_url(
  collection = collection,
  variables = variables,
  roi = africa_polygon,
  time_range = time_range
)

res_dl <- mf_download_data(urls, parallel = TRUE)
