#' Preprocess Soil Data
#'
#' This function downloads, processes and transforms soil dataset. If a preprocessed file 
#' already exists in the specified directory and the overwrite argument is FALSE, 
#' the function returns the filepath to the existing preprocessed file.
#' 
#' @author Nathan C. Layman & Whitney Bagge
#'
#' @param soil_directory Directory where the soil dataset and preprocessed files will be stored.
#' @param continent_raster_template Template to be used for projecting the soil raster file.
#' @param overwrite Boolean flag determining whether the processing should be done if a preprocessed file already exists. Default is FALSE.
#' @param ... Additional arguments not used by this function but included for compatibility with generic functions
#'
#' @return A string containing the filepath to the preprocessed soil file.
#'
#' @note The process of downloading, transformation, and saving soil data includes downloading the HWSD2 ZIP file and its key from 
#' online resources, unzipping the downloaded file, and performing various transformations (including raster projection and merging with the key) 
#' before saving the processed file as 'soil_preprocessed.parquet'.
#'
#' @examples
#' preprocess_soil(soil_directory = "./data",
#'                 continent_raster_template = raster_template,
#'                 overwrite = TRUE)
#'
#' @export
preprocess_soil <- function(soil_directory, 
                            continent_raster_template,
                            output_filename = "soil_preprocessed.parquet",
                            overwrite = FALSE,
                            ...) {

  # Harmonized World Soil Database (HWSD2) https://gaez.fao.org/pages/hwsd
  
  # Set up safe way to read parquet files
  error_safe_read_parquet <- possibly(arrow::open_dataset, NULL)
  
  # Parquet filenames
  soil_preprocessed_file <- file.path(soil_directory, output_filename)
  
  # Check if soil files exist and can be read and that we don't want to overwrite them.
  if(!is.null(error_safe_read_parquet(soil_preprocessed_file)) & 
     !overwrite) {
    message("preprocessed soil file already exists and can be loaded, skipping download and processing")
    return(soil_preprocessed_file)
  }
  
  ###### DOWNLOAD HWSD2 DATA #######
  
  HWSD2_raw_file <- file.path(soil_directory, "HWSD2.zip")
  download.file(url="https://s3.eu-west-1.amazonaws.com/data.gaezdev.aws.fao.org/HWSD/HWSD2_RASTER.zip", 
                destfile = HWSD2_raw_file)
  unzip(HWSD2_raw_file, exdir = soil_directory)
  
  # Download HWSD2 SMU key
  HWSD2_SMU_key_file <- file.path(soil_directory, "HWSD2_SMU_key.sqlite")
  download.file(url="https://www.isric.org/sites/default/files/HWSD2.sqlite", 
                destfile = HWSD2_SMU_key_file)
  
  # Query the HWSD2_SMU table and convert it to a tibble
  con <- RSQLite::dbConnect(RSQLite::SQLite(), dbname=HWSD2_SMU_key_file)
  HWSD2_SMU_data <- tbl(con, "HWSD2_SMU") |> collect()
  RSQLite::dbDisconnect(con)
  
  # Unwrap continent_raster template
  continent_raster_template <- terra::unwrap(continent_raster_template)
  
  # Each pixel in the raster corresponds to a Soil Mapping Unit (SMU).
  HWSD2_raster <- terra::rast(file.path(soil_directory, "/HWSD2.bil")) |> 
    terra::project(continent_raster_template, method = "near", mask = T) |>
    setNames("HWSD2_SMU_ID") |>
    as.data.frame(xy = TRUE) |>
    as_tibble()
  
  soil_preprocessed <- HWSD2_raster |> 
    left_join(HWSD2_SMU_data |> select(HWSD2_SMU_ID, DRAINAGE, TEXTURE_USDA), by = join_by(HWSD2_SMU_ID)) |>
    select(-HWSD2_SMU_ID) |>
    rename(soil_drainage = DRAINAGE,
           soil_texture = TEXTURE_USDA) |>
    mutate(soil_drainage = as.factor(soil_drainage))
  
  ###### SOIL TEXTURE ######
  
  # At this point
  # 1 - clay (heavy)
  # 2 - silty clay
  # 3 - clay
  # 4 - silty clay loam
  # 5 - clay loam
  # 6 - silt
  # 7 - silt loam
  # 8 - sandy clay
  # 9 - loam
  # 10 - sandy clay loam
  # 11 - sandy loam
  # 12 - loamy sand
  # 13 - sand
  
  # Re-code factor levels to collapse simplex. 
  # Figure out where key is for the units are in HWSD2
  # NCL: This is confusing but keeping to match previous work
  soil_preprocessed <- soil_preprocessed |> mutate(soil_texture = if_else(soil_texture == 5, 1, # clay (heavy) + clay loam
                                                                          if_else(soil_texture == 7, 2, # silt loam + silty clay
                                                                                  if_else(soil_texture == 8, 3, # sandy clay + clay
                                                                                          if_else(soil_texture == 9, 4, # loam + silty clay loam
                                                                                                  if_else(soil_texture == 10, 5, # sandy clay loam SEE RULE 1!!!
                                                                                                          if_else(soil_texture == 11, 6, # sandy loam + silt
                                                                                                                  if_else(soil_texture == 12, 7, # loamy sand + silt loam
                                                                                                                          0))))))) |>
                                                     as.factor()) # loamy sand + silt loam
                                           

  ###### SOIL DRAINAGE ######
  
  # Soil drainage classes are based on the "Guidelines to estimation of 
  # drainage classes based on soil type, texture, soil phase and terrain 
  # slope" (FAO, 1995). In the HWSD, drainage classes represent reference 
  # drainage conditions assuming flat terrain (i.e., 0.0 - 0.5% slope). 
  
  # https://cteco.uconn.edu/guides/Soils_Drainage.htm
  
  # 1. Excessively drained: water is removed from the soil very rapidly 
  #    Soils are commonly very coarse textured or rocky, shallow or on steep slopes
  # 2. Somewhat excessively drained: water is removed from the soil rapidly. 
  #    Soils are commonly sandy and very pervious  
  # 3. Well drained: water is removed from the soil readily but not rapidly. 
  #    Soils commonly retain optimum amounts of moisture, 
  #    but wetness does not inhibit root growth for significant periods
  # 4. Moderately well drained: Water is removed from the soil somewhat 
  #    slowly during some periods of the year. For a short period, soils are 
  #    wet within the rooting depth, they commonly have an almost impervious layer
  # 5. Imperfectly drained: Water is removed slowly so that soil is wet at a shallow 
  #    depth for significant periods. Soils commonly have an impervious layer, 
  #    a high-water table, or additions of water by seepage
  # 6. Poorly drained: Water is removed so slowly that soils are commonly wet at a 
  #    shallow depth for considerable periods. Soils commonly have a shallow water 
  #    table which is usually the result of an almost impervious layer, or seepage
  # 7. Very poorly drained: Water is removed so slowly that the soils are wet at shallow 
  #    depths for long periods. Soils have a very shallow water table and are commonly 
  #    in level or depressed sites. 

  ## Change NA to UNK so we don't lose cells just because of soil
  soil_preprocessed <- soil_preprocessed %>% 
    mutate(
        soil_drainage = as.character(soil_drainage)
      , soil_drainage = ifelse(is.na(soil_drainage), "UNK", soil_drainage) %>% as.factor()
      , soil_texture  = as.character(soil_texture)
      , soil_texture  = ifelse(is.na(soil_texture), "UNK", soil_texture) %>% as.factor()
    ) 
  
  # Save soil data as parquet files
  arrow::write_parquet(soil_preprocessed, soil_preprocessed_file, compression = "gzip", compression_level = 5)
  
  # Test if soil parquet files can be loaded. If not clean up directory and return NULL
  if(is.null(error_safe_read_parquet(soil_preprocessed_file))) {
    message("Preprocessed soil parquet file couldn't be read after processing. Cleaning up")
    file.remove(list.files(soil_directory, full.names = TRUE))
    return(NULL)
  }
  
  # Clean up all non-parquet files
  file.remove(grep("\\.parquet.*$", list.files(soil_directory, full.names = TRUE), value = TRUE, invert = TRUE))
  
  return(soil_preprocessed_file)
}
