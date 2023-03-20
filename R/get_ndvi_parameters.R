
#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title

#' @return
#' @author Emma Mendelsohn
#' @export
get_ndvi_parameters <- function() {
  
  # # This returns full continents - would need to subset for Africa
  # url1 <- "https://catalogue.dataspace.copernicus.eu/resto/api/collections/Sentinel3/search.json?maxRecords=10&productType=SY_2_V10___&platform=S3A" 
  # resp1 <- GET(url1)
  # out1 <- fromJSON(rawToChar(resp1$content))
  # id1 <- out1$features$id[9]
  # title <- tools::file_path_sans_ext(out$features$properties$title[9])
  # 
  # url <- glue::glue("http://catalogue.dataspace.copernicus.eu/odata/v1/Products({id})/$value")
  # filename <- glue::glue("{title}.zip")
  # auth_header <- paste("Bearer", Sys.getenv("KEYCLOAK_TOKEN"), sep = " ")
  # response <- GET(url, add_headers(Authorization = auth_header), write_disk(filename, overwrite = TRUE))
  # unzip(filename)
  # t1 = terra::rast("S3A_SY_2_V10____20180922T111721_20181002T111721_20181012T115623_AFRICA____________LN2_O_NT_002.SEN3/NDVI.nc")

  # Query using our bounding box for Central Africa - returns full Africa files
  # 229 results as of 2023-03-20, so max records of 500 is safe
  url <- "https://catalogue.dataspace.copernicus.eu/resto/api/collections/Sentinel3/search.json?maxRecords=500&productType=SY_2_V10___&platform=S3A&box=13.4,7.46,24.0,23.4" 
  resp <- GET(url)
  out <- fromJSON(rawToChar(resp$content))
  return(out$features)
  # id <- out$features$id
  # title <- tools::file_path_sans_ext(out$features$properties$title[9])
  # 
  # url <- glue::glue("http://catalogue.dataspace.copernicus.eu/odata/v1/Products({id})/$value")
  # filename <- glue::glue("{title}.zip")
  # auth_header <- paste("Bearer", Sys.getenv("KEYCLOAK_TOKEN"), sep = " ")
  # response <- GET(url, add_headers(Authorization = auth_header), write_disk(filename, overwrite = TRUE))
  # unzip(filename)
  # t1 = terra::rast("S3A_SY_2_V10____20180922T111721_20181002T111721_20181012T115623_AFRICA____________LN2_O_NT_002.SEN3/NDVI.nc")
  
  
  
  
  
  
}
