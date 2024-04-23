#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param training_data
#' @param holdout_data
#' @param holdout_data_masks
#' @return
#' @author Emma Mendelsohn
#' @export
visualize_splits_masked <- function(training_data, holdout_data,
                                    holdout_data_masks) {

  train <- training_data |> 
    select(date, shapeName) |> 
    mutate(split = "training")
  
  holdout <- holdout_data |> 
    select(date, shapeName) |> 
    mutate(split = "holdout")
  
  mask <- holdout_data_masks |> 
    mutate(mask = "mask")
  
  dat <- bind_rows(train, holdout) |> 
    left_join(mask)  |> 
    mutate(split = coalesce(mask, split)) |> 
    select(-mask)
  
  ggplot(dat |> mutate(shapeName = factor(shapeName)), 
         aes(x = date, y = shapeName, fill = split)) +
    geom_tile(alpha = 1, width = 20) +
    scale_fill_manual(values = c("holdout" = "#009292", 
                                 "training" = "#ffb6db",
                                 "mask" = "red"
                                 ),
                      na.value = 'gray85', guide = guide_legend(reverse = TRUE)) +
    coord_cartesian() +
    scale_y_discrete(limits = rev(levels(factor(bind_rows(train, holdout)$shapeName)))) +   
    scale_x_date(expand = c(0,0)) +
    theme_minimal() +
    theme(panel.grid = element_blank())+
    NULL
    

}
