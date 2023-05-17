#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title

#' @return
#' @author Emma Mendelsohn
#' @export
get_nasa_weather_coordinates <- function(country_bounding_boxes) {
  
  country_bounding_boxes$bounding_box <- map(country_bounding_boxes$bounding_box, ~pivot_wider(enframe(unlist(as.list(.)))))
  country_bounding_boxes_with_coords <- country_bounding_boxes |> 
    mutate(coords = map(bounding_box, function(bb){
     x <- c(seq(bb$xmin, bb$xmax, by = 4)) # by 4 instead of 5 to be able to handle adding 2 to the range below
     if(bb$xmax > x[length(x)]) x <- c(x, bb$xmax)
     if(x[length(x)] - x[length(x)-1] <= 2) x[length(x)] <- x[length(x)-1] + 2.01 # API requires at least 2 degree range
     
     y <- c(seq(bb$ymin, bb$ymax, by = 4))
     if(bb$ymax > y[length(y)]) y <- c(y, bb$ymax)
     if(y[length(y)] - y[length(y)-1] <= 2) y[length(y)] <- y[length(y)-1] + 2.01 # API requires at least 2 degree range
     
     crossing(x = rolling_box(x), y = rolling_box(y)) 
   }))
  
  return(country_bounding_boxes_with_coords)

}

rolling_box <- function(x){
  out <- list()
  for(i in 1:(length(x)-1)){
    out[[i]] <- c(x[i], x[i+1])
  }
  return(out)
}
