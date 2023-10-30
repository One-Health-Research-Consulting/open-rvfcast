#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title

#' @return
#' @author Emma Mendelsohn
#' @export
get_modis_ndvi_token <- function() {

  secret <- base64_enc(paste(Sys.getenv("APPEEARS_USERNAME"), Sys.getenv("APPEEARS_PASSWORD"), sep = ":")) #TODO make project auth
  token_response <- POST("https://appeears.earthdatacloud.nasa.gov/api/login", 
                         add_headers("Authorization" = paste("Basic", gsub("\n", "", secret)),
                                     "Content-Type" = "application/x-www-form-urlencoded;charset=UTF-8"), 
                         body = "grant_type=client_credentials")
  token_response <- prettify(toJSON(content(token_response), auto_unbox = TRUE))
  token <- paste("Bearer", fromJSON(token_response)$token)
  
  return(token)

}
