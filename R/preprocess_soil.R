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
preprocess_soil <- function(soil_directory_dataset, soil_directory_raw, continent_raster_template, soil_downloaded) {

    #read in the raster file

  #crop the raster to the continent
  #hwsd_bounded <- terra::crop(unzipped_soil_raster, terra::unwrap(continent_raster_template))
  
  #reproject the raster
    #print(paste("UTM zone:", utm.zone <-
    #            floor(((sf::st_bbox(hwsd_bounded)$xmin +
    #                      sf::st_bbox(hwsd_bounded)$xmax)/2 + 180)/6)
    #          + 1))
  
  #(epsg <- 32600 + utm.zone)
  
  #hwsd_bounded.utm <- project(hwsd_bounded, paste0("EPSG:", epsg), method = "near")
  
  #terra::resample(hwsd_bounded.utm, method = "near")
   
  transformed_raster <- transform_raster(raw_raster = rast(paste0(soil_downloaded, "/HWSD2.bil")),
                                         template = rast(continent_raster_template))
  
  #connect to database and extract values
  m <- dbDriver("SQLite")
  con <- dbConnect(m, dbname="data/soil/soil_database.sqlite")
  dbListTables(con)
  
  ####extract map unit codes in bounded area (WINDOW_ZHNJ) to join with SQL databases###
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
  
  #create sand and clay tables in R
  #sand.d1 <- dbGetQuery(con,
  #                      "select U.HWSD2_SMU_ID, U.SAND from ZHNJ_SMU as T
  #                      join HWSD2_LAYERS as U on T.HWSD2_SMU_ID=U.HWSD2_SMU_ID
  #                      where U.LAYER='D1'
  #                      order by U.HWSD2_SMU_ID")
  #
  #clay.d1 <- dbGetQuery(con,
  #                      "select U.HWSD2_SMU_ID, U.CLAY from ZHNJ_SMU as T
  #                      join HWSD2_LAYERS as U on T.HWSD2_SMU_ID=U.HWSD2_SMU_ID
  #                      where U.LAYER='D1'
  #                      order by U.HWSD2_SMU_ID")
  
  #remove the temp tables and database connection
  dbRemoveTable(con, "WINDOW_ZHNJ")
  dbRemoveTable(con, "ZHNJ_SMU")
  dbDisconnect(con)
  
  #join sand and clay data frames in r to create a ratio variable
  #full_join (sand.d1, clay.d1)
  
  #changes from character to factor for the raster
  for (i in names(records)[c(2:5,7:13,16:17,19:23)]) {
    eval(parse(text=paste0("records$",i," <- as.factor(records$",i,")")))
  }

  #create matrix of map unit ids and the variable of interest - TEXTURE CLASS
  rcl.matrix.texture <- cbind(id = as.numeric(as.character(records$HWSD2_SMU_ID)),
                      texture = as.numeric(records$TEXTURE_USDA))
  
  #classify the raster (transformed_raster) using the matrix of values - TEXTURE CLASS
  hwsd.zhnj.texture <- classify(transformed_raster, rcl.matrix.texture)
  hwsd.zhnj.texture <- as.factor(hwsd.zhnj.texture)
  levels(hwsd.zhnj.texture) <- levels(records$TEXTURE_USDA)
  
  # Convert to dataframe
  dat_out <- as.data.frame(hwsd.zhnj.texture, xy = TRUE) |> 
    as_tibble() 
  
  # Re-code factor levels to collapse simplex. 
  # Figure out where key is for the units are in HWSD2
  dat_out$HWSD2 <- if_else(dat_out$HWSD2=="5", "1",
                   if_else(dat_out$HWSD2=="7", "2",
                   if_else(dat_out$HWSD2=="8", "3",
                   if_else(dat_out$HWSD2=="9", "4",
                   if_else(dat_out$HWSD2=="10", "5",
                   if_else(dat_out$HWSD2=="11", "6",
                   if_else(dat_out$HWSD2=="12", "7","0")))))))
                                           

  #create matrix of map unit ids and the variable of interest - DRAINAGE
  rcl.matrix.drainage <- cbind(id = as.numeric(as.character(records$HWSD2_SMU_ID)),
                      drainage = as.numeric(records$DRAINAGE))
  
  #classify the raster (transformed_raster) using the matrix of values - DRAINAGE
  hwsd.zhnj.drainage <- classify(transformed_raster, rcl.matrix.drainage)
  hwsd.zhnj.drainage <- as.factor(hwsd.zhnj.drainage)
  levels(hwsd.zhnj.drainage) <- levels(records$DRAINAGE)
  
  # Convert to dataframe
  dat_out2 <- as.data.frame(hwsd.zhnj.drainage, xy = TRUE) |> 
    as_tibble() 
  
  dat_out2$HWSD2 <- if_else(dat_out2$HWSD2=="MW", "4",
              if_else(dat_out2$HWSD2=="P", "6",
              if_else(dat_out2$HWSD2=="SE", "2",
              if_else(dat_out2$HWSD2=="VP", "7","0"))))
  
  dat_out2$HWSD2 <- as.numeric(as.character(dat_out2$HWSD2))
  
  # Save as parquet 
  write_parquet(dat_out,  "data/soil_dataset/soil_texture", compression = "gzip", compression_level = 5)
  write_parquet(dat_out2, "data/soil_dataset/soil_drainage", compression = "gzip", compression_level = 5)
  
  #writeRaster(hwsd.zhnj.drainage, "data/soil/drainage_raster.tif", overwrite=TRUE)
  #writeRaster(hwsd.zhnj.texture, "data/soil/texture_class_raster.tif", overwrite=TRUE)
  #writeRaster(x, sand_clay_raster, overwrite=TRUE)
  
  
  return(soil_directory_dataset)
  
}
