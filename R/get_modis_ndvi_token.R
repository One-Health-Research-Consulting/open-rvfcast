#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title

#' @return
#' @author Emma Mendelsohn
#' @export
get_modis_ndvi_token <- function() {
  
  token <- Sys.getenv("APPEEARS_TOKEN", unset = "")
  
  # Test that the current token works
  test <- httr::GET("https://appeears.earthdatacloud.nasa.gov/api/task", httr::add_headers(Authorization = paste("Bearer", token)))

  # If it doesn't get a new one and update the .env file
  if(test$status_code == 403) {
    secret <- jsonlite::base64_enc(paste(Sys.getenv("APPEEARS_USERNAME"), Sys.getenv("APPEEARS_PASSWORD"), sep = ":")) #TODO make project auth
    token_response <- httr::POST("https://appeears.earthdatacloud.nasa.gov/api/login", 
                                 httr::add_headers("Authorization" = paste("Basic", gsub("\n", "", secret)),
                                                   "Content-Type" = "application/x-www-form-urlencoded;charset=UTF-8"), 
                                 body = "grant_type=client_credentials")
    token_response <- jsonlite::prettify(jsonlite::toJSON(httr::content(token_response), auto_unbox = TRUE))
    token <- jsonlite::fromJSON(token_response)$token
    update_env_key("APPEEARS_TOKEN", token)
  }
  
  return(paste("Bearer", token))

}
