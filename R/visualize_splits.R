#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param training_data
#' @param holdout_data
#' @return
#' @author Emma Mendelsohn
#' @export
visualize_splits <- function(training_data, holdout_data) {

  train <- training_data |> 
    select(date, shapeName) |> 
    mutate(split = "training")
  
  holdout <- holdout_data |> 
    select(date, shapeName) |> 
    mutate(split = "holdout")
  
  ggplot(bind_rows(train, holdout) |> mutate(shapeName = factor(shapeName)), 
         aes(x = date, y = shapeName, fill = split)) +
    geom_tile(alpha = 1, width = 20) +
    scale_fill_manual(values = c("holdout" = "#009292", "training" = "#ffb6db"), na.value = 'gray85', guide = guide_legend(reverse = TRUE)) +
    coord_cartesian() +
    scale_y_discrete(limits = rev(levels(factor(bind_rows(train, holdout)$shapeName)))) +   
    scale_x_date(expand = c(0,0)) +
    theme_minimal() +
    theme(panel.grid = element_blank())+
    NULL

}
