#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param continent_polygon
#' @return
#' @author Whitney Bagge
#' @export
library(DBI)
library(RSQLite)
preprocess_soil <- function(soil_directory_dataset, 
                            continent_raster_template,
                            overwrite = FALSE,
                            ...) {

  # Unwrap continent_raster template
  continent_raster_template <- terra::unwrap(continent_raster_template)
  
  # Set up safe way to read parquet files
  error_safe_read_parquet <- possibly(arrow::read_parquet, NULL)
  
  # Parquet filenames
  soil_texture_file <- file.path(soil_directory_dataset, "soil_texture.parquet")
  soil_drainage_file <- file.path(soil_directory_dataset, "soil_drainage.parquet")
  
  # Check if sile files exist and can be read and that we don't want to overwrite them.
  if(!is.null(error_safe_read_parquet(soil_texture_file)) & 
     !is.null(error_safe_read_parquet(soil_drainage_file)) & 
     !overwrite) {
    message("preprocessed soil files already exist and can be loaded, skipping download and processing")
    return(c(basename(soil_texture_file), 
             basename(soil_drainage_file)))
  }
  
  # Download soil texture data and unzip
  soil_texture_raw_file <- file.path(soil_directory_dataset, "soil_raster.zip")
  download.file(url="https://s3.eu-west-1.amazonaws.com/data.gaezdev.aws.fao.org/HWSD/HWSD2_RASTER.zip", 
                destfile = soil_texture_raw_file)
  unzip(soil_texture_raw_file, exdir = soil_directory_dataset)
  
  # Download soil drainage data
  soil_drainage_raw_file <- file.path(soil_directory_dataset, "soil_drainage.sqlite")
  download.file(url="https://www.isric.org/sites/default/files/HWSD2.sqlite", 
                destfile = soil_drainage_raw_file)
   
  ###### SOIL TEXTURE ######
  transformed_raster <- transform_raster(raw_raster = rast(file.path(soil_directory_dataset, "/HWSD2.bil")),
                                         template = rast(continent_raster_template))
  
  # connect to database and extract values
  m <- dbDriver("SQLite")
  con <- dbConnect(m, dbname="data/soil/soil_database.sqlite")
  dbListTables(con)
  
  #### extract map unit codes in bounded area (WINDOW_ZHNJ) to join with SQL databases###
  dbWriteTable(con, name="WINDOW_ZHNJ",
               value=data.frame(hwsd2_smu = sort(unique(values(transformed_raster)))),
               overwrite=TRUE)
  
  dbExecute(con, "drop table if exists ZHNJ_SMU") # to overwrite
  
  dbListTables(con)
  
  #creates a temp database that combines the map unit codes in the raster window to the desired variable
  dbExecute(con,
            "create TABLE ZHNJ_SMU AS select T.* from HWSD2_SMU as T
              join WINDOW_ZHNJ as U
              on T.HWSD2_SMU_ID=U.HWSD2_SMU
              order by HWSD2_SMU_ID")
  
  #creates a dataframe "records" in R from SQL temp table created above
  records <- dbGetQuery(con, "select * from ZHNJ_SMU")
  
  #remove the temp tables and database connection
  dbRemoveTable(con, "WINDOW_ZHNJ")
  dbRemoveTable(con, "ZHNJ_SMU")
  dbDisconnect(con)
  
  #changes from character to factor for the raster
  for (i in names(records)[c(2:5,7:13,16:17,19:23)]) {
    eval(parse(text=paste0("records$",i," <- as.factor(records$",i,")")))
  }

  #create matrix of map unit ids and the variable of interest - TEXTURE CLASS
  rcl.matrix.texture <- cbind(id = as.numeric(as.character(records$HWSD2_SMU_ID)),
                      texture = as.numeric(records$TEXTURE_USDA))
  
  #classify the raster (transformed_raster) using the matrix of values - TEXTURE CLASS
  # CLASIFFY DOESN'T SEEM TO BE WORKING LEFT OFF HERE
  hwsd.zhnj.texture <- classify(transformed_raster, rcl.matrix.texture)
  hwsd.zhnj.texture <- as.factor(hwsd.zhnj.texture)
  levels(hwsd.zhnj.texture) <- levels(records$TEXTURE_USDA)
  
  # Convert to dataframe
  soil_texture <- as.data.frame(hwsd.zhnj.texture, xy = TRUE) |> 
    as_tibble() 
  
  # At this point:
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
  soil_texture$HWSD2 <- if_else(soil_texture$HWSD2=="5", "1", # clay (heavy) + clay loam
                        if_else(soil_texture$HWSD2=="7", "2", # silty clay + silty loam aka
                        if_else(soil_texture$HWSD2=="8", "3", # clay + sandy clay
                        if_else(soil_texture$HWSD2=="9", "4", # silty clay loam
                        if_else(soil_texture$HWSD2=="10", "5", # clay loam + sandy clay loam BUT SEE RULE 1!!!
                        if_else(soil_texture$HWSD2=="11", "6", # silt sandy + loam
                        if_else(soil_texture$HWSD2=="12", "7", "0"))))))) # loamy sand + silt loam
                                           

  ###### SOIL DRAINAGE ######
  
  #create matrix of map unit ids and the variable of interest - DRAINAGE
  rcl.matrix.drainage <- cbind(id = as.numeric(as.character(records$HWSD2_SMU_ID)),
                      drainage = as.numeric(records$DRAINAGE))
  
  #classify the raster (transformed_raster) using the matrix of values - DRAINAGE
  hwsd.zhnj.drainage <- classify(transformed_raster, rcl.matrix.drainage)
  hwsd.zhnj.drainage <- as.factor(hwsd.zhnj.drainage)
  levels(hwsd.zhnj.drainage) <- levels(records$DRAINAGE)
  
  # Convert to dataframe
  soil_drainage <- as.data.frame(hwsd.zhnj.drainage, xy = TRUE) |> 
    as_tibble() 
  
  soil_drainage$HWSD2 <- if_else(soil_drainage$HWSD2=="MW", "4",
              if_else(soil_drainage$HWSD2=="P", "6",
              if_else(soil_drainage$HWSD2=="SE", "2",
              if_else(soil_drainage$HWSD2=="VP", "7","0"))))
  
  soil_drainage$HWSD2 <- as.numeric(as.character(soil_drainage$HWSD2))
  
  # Save soil data as parquet files
  arrow::write_parquet(soil_texture,  soil_texture_file, compression = "gzip", compression_level = 5)
  arrow::write_parquet(soil_drainage, soil_drainage_file, compression = "gzip", compression_level = 5)
  
  # Test if soil parquet files can be loaded. If not clean up directory and return NULL
  if(is.null(error_safe_read_parquet(soil_texture_file)) || 
     is.null(error_safe_read_parquet(soil_drainage_file))) {
    message("Preprocessed soil parquet files couldn't be read after processing. Cleaning up")
    file.remove(list.files(soil_directory_dataset, full.names = TRUE))
    return(NULL)
  }
  
  # Clean up all non-parquet files
  file.remove(grep("\\.parquet$", list.files(soil_directory_dataset, full.names = TRUE), value = TRUE, invert = TRUE))
  
  return(c(basename(soil_texture_file), 
           basename(soil_drainage_file)))
}
