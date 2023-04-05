library(MODISTools)
library(tidyverse)
library(targets)
tar_load(bounding_boxes)
products <- mt_products()
MOD13Q1 <- products |> filter(product == "MOD13Q1")
bands <- mt_bands(product = "MOD13Q1")
ndvi <- bands |> filter(band == "250m_16_days_NDVI")
dates <- mt_dates(product = "MOD13Q1", lat = bounding_boxes$y_min[1], lon = bounding_boxes$x_max[1])
min(dates$calendar_date)
max(dates$calendar_date)

#### Option 2
library(rstac)
library(magrittr)
library(terra)
library(httr)

s_obj <- stac("https://planetarycomputer.microsoft.com/api/stac/v1/")

collection <- "modis-13Q1-061" # this is 250m (500m is modis-13A1-061)

it_obj <- s_obj %>% 
  stac_search(collections = collection,
              bbox = as.numeric(bounding_boxes[1,-1])) %>%
  get_request() |> 
  items_sign(sign_fn = sign_planetary_computer())
# max 250 returned

url <- paste0("/vsicurl/", it_obj$features[[37]]$assets$`250m_16_days_NDVI`$href)

data <- rast(url)
plot(data)
