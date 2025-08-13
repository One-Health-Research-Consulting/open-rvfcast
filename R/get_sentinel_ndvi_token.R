get_sentinel_ndvi_token <- function(filename = "sentinel.token") {
  auth <- httr::POST("https://identity.dataspace.copernicus.eu/auth/realms/CDSE/protocol/openid-connect/token", 
                     body = list(
                       grant_type = "password",
                       username = Sys.getenv("COPERNICUS_USERNAME"),
                       password = Sys.getenv("COPERNICUS_PASSWORD"),
                       client_id = "cdse-public"), 
                     encode = "form")
  
  sentinel_ndvi_token <- httr::content(auth)$access_token
  
  # Write to a file
  writeLines(sentinel_ndvi_token, filename)
  
  filename
}
