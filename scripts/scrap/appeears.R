#  get list of modis layers
library(httr)
library(jsonlite)
params <- list(pretty = TRUE)
product_id <- 'MOD13A2.061' # 1 km NDVI - this is the coarsest
response <- GET(paste("https://appeears.earthdatacloud.nasa.gov/api/product/",product_id, sep = ""), query = params)
product_response <- prettify(toJSON(content(response), auto_unbox = TRUE))
product_response

# authentication

secret <- base64_enc(paste(Sys.getenv("APPEEARS_USERNAME"), Sys.getenv("APPEEARS_PASSWORD"), sep = ":")) #TODO make project auth
response <- POST("https://appeears.earthdatacloud.nasa.gov/api/login", 
                 add_headers("Authorization" = paste("Basic", gsub("\n", "", secret)),
                             "Content-Type" = "application/x-www-form-urlencoded;charset=UTF-8"), 
                 body = "grant_type=client_credentials")
token_response <- prettify(toJSON(content(response), auto_unbox = TRUE))
token_response

# see existing tasks
token <- paste("Bearer", fromJSON(token_response)$token)
response <- GET("https://appeears.earthdatacloud.nasa.gov/api/task", add_headers(Authorization = token))
task_response <- prettify(toJSON(content(response), auto_unbox = TRUE))
task_response

# set parameters
task_name <- "test"
task_type <- "area"
start_date <- "01-01-2005"
end_date <- "12-31-2005"
product <- "MOD13A2.061"
layer <- "_1_km_16_days_NDVI"
file_type <- "geotiff"
projection_name <- "native"
bbox <- paste(country_bounding_boxes$bounding_box[[5]], collapse = ",")  # {min_longitude},{min_latitude},{max_longitude},{max_latitude} 
polygon <- country_polygons$geometry[[5]]

params <- glue::glue("task_name={task_name}&task_type={task_type}&startDate={start_date}&endDate={end_date}&product={product}&layer={layer}&bbox={bbox}&file_type={file_type}&projection_name={projection_name}")


# create the task request
task <- list(task_type = task_type, task_name = task_name, startDate = start_date, endDate = end_date,  layer = paste(product, layer, sep = ","), file_type = file_type, projection_name = projection_name, bbox = bbox)

# submit the task request
token <- paste("Bearer", fromJSON(token_response)$token)
response <- POST("https://appeears.earthdatacloud.nasa.gov/api/task", query = task, add_headers(Authorization = token))

task_response <- prettify(toJSON(content(response), auto_unbox = TRUE))
task_response


token <- paste("Bearer", fromJSON(token_response)$token)
task_id <- fromJSON(task_response)$task_id
response <- GET(paste("https://appeears.earthdatacloud.nasa.gov/api/bundle/", task_id, sep = ""), add_headers(Authorization = token))
bundle_response <- prettify(toJSON(content(response), auto_unbox = TRUE))
bundle_response <- fromJSON(bundle_response)
bundle_response$files$file_id
# identify files to download

# get bundle off of AWS