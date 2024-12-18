#' Generate Animated Outbreak History
#'
#' This function reads outbreak history data from the wahis_outbreak_history, creates a temporary
#' directory for data processing, and generates an animated gif of the outbreak history
#' using the weights calculated by the get_daily_outbreak_history() function.
#'
#' @author Nathan C. Layman
#'
#' @param wahis_outbreak_history The file containing historical outbreak data in parquet format.
#' @param wahis_outbreak_history_animations_directory Directory to save the output gif file. Default is "outputs".
#' @param num_cores Number of cores to use for parallel processing. Default is 1.
#' @param ... Additional arguments not used by this function but included for generic function compatibility.
#'
#' @return String specifying the path to the created gif file.
#'
#' @note The function creates a temporary directory for its process, which it deletes after the gif generation.
#' Make sure enough disc space is available for this operation.
#'
#' @examples
#' get_outbreak_history_animation(wahis_outbreak_history = './data/outbreak.parquet',
#'                                wahis_outbreak_history_animations_directory = './outputs',
#'                                num_cores = 4)
#'
#' @export
get_outbreak_history_animation <- function(wahis_outbreak_history,
                                           wahis_outbreak_history_animation_metadata,
                                           wahis_outbreak_history_animations_directory,
                                           num_cores = 1,
                                           overwrite = FALSE,
                                           ...) {
  
  assertthat::are_equal(nrow(wahis_outbreak_history_animation_metadata), 1)
  output_basename = glue::glue("outbreak_history_{wahis_outbreak_history_animation_metadata$year}")
  
  # Load the data
  outbreak_history_dataset <- arrow::open_dataset(wahis_outbreak_history) |> 
    filter(year == wahis_outbreak_history_animation_metadata$year) |>
    collect() |>
    pivot_longer(contains("weight"), 
                 names_to = "time_frame", 
                 values_to = "weight",
                 names_pattern = ".*_(.*)")
  
  min_weight <- wahis_outbreak_history_animation_metadata$min
  max_weight <- wahis_outbreak_history_animation_metadata$max
  
  # Identify limits (used to calibrate the color scale)
  lims <- c(min_weight, max_weight)
  
  # Create temporary directory if it does not yet exist
  tmp_dir <- paste(wahis_outbreak_history_animations_directory, output_basename, sep = "/")
  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
  
  time_frames <- unique(pull(outbreak_history_dataset, time_frame))
  
  # Make animations for both recent and old outbreaks
  output_files <- map_vec(time_frames, function(tf) {
      
    output_filename = file.path(wahis_outbreak_history_animations_directory, glue::glue("{basename(output_basename)}_{tf}.gif"))
    
    # Check if outbreak_history file exist and can be read and that we don't want to overwrite them.
    if(file.exists(output_filename) & !overwrite) {
      message(glue::glue("{basename(output_filename)} already exists. Skipping."))
      return(output_filename)
    }
    
    message(paste("Animating", basename(output_filename))) 
    
    dates <- outbreak_history_dataset |> filter(time_frame == tf) |> select(date) |> distinct() |> pull(date, as_vector = TRUE)
    
    # This function makes a png for each date which will then get stiched together
    # into the animation. Saving each png is faster than trying to do everything
    # in memory.
    png_files <- parallel::mclapply(mc.cores = num_cores, 
                                    dates, 
                                    function(d) plot_outbreak_history(outbreak_history_dataset |> 
                                                                        filter(date == d) |>
                                                                        filter(time_frame == tf) |>
                                                                        collect(),
                                                                      tmp_dir = tmp_dir,
                                                                      write_frame = TRUE,
                                                                      lims = lims)) |> 
      unlist() |> sort()
    
    # Add in a delay at end before looping back to beginning. This is in frames not seconds
    png_files <- c(png_files, rep(png_files |> tail(1), 50))
    
    # Render the animation
    gif_file <- gifski::gifski(png_files, 
                               delay = 0.04,
                               gif_file = output_filename)
    
    # Clean up temporary files
    file.remove(png_files)
    
    # Return the location of the rendered animation
    output_filename
    }
  )
  
  # Clean up temporary files
  unlink(tmp_dir, recursive = T)
  
  return(output_files)
}

#' Plot Outbreak History
#'
#' This function generates a plot of outbreak history based on provided data, saves it 
#' as a png image in the specified directory and returns the filepath of the image.
#'
#' @author Nathan C. Layman
#'
#' @param frame Data frame with outbreak history data.
#' @param tmp_dir Temporary directory where the plot image will be saved.
#' @param write_frame Boolean flag indicating whether to write the frame to a file. Default is TRUE.
#' @param lims Numeric vector specifying the range of the fill color scale. NULL by default. 
#'
#' @return A string path to the saved image if write_frame is TRUE, otherwise a ggplot object.
#'
#' @note This Function creates plot image using ggplot and saves it if write_frame is TRUE.
#'
#' @examples
#' plot_outbreak_history(frame = df,
#'                       tmp_dir = './outputs',
#'                       write_frame = TRUE,
#'                       lims = c(0, 10))
#'
#' @export
plot_outbreak_history <- function(frame,
                                  tmp_dir,
                                  write_frame = TRUE,
                                  lims = NULL) {
  
  date <- frame$date |> unique() |> pluck(1)
  time_frame <- frame$time_frame |> unique() |> pluck(1)
  
  filename <- file.path(tmp_dir, glue::glue("{date}_{time_frame}.png"))
  title <- glue::glue("Outbreak History: {date}")
  
  p <- ggplot(frame, aes(x=x, y=y, fill=weight)) +
    geom_raster() +
    scale_fill_viridis_c(limits = lims,
                         trans = scales::sqrt_trans()) +
    labs(title = title, x = "Longitude", y = "Latitude", fill = "Weight\n") +
    theme_minimal() +
    theme(text = element_text(size = 18),
          legend.title = element_text(vjust = 0.05)) 
  
  if(write_frame) {
    png(filename = filename, width = 600, height = 600)
    print(p)
    dev.off()
    return(filename)
  }
  
  p
}
