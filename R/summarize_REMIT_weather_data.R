#' Collapse temporal NASA Power data into a single static layer for REMIT
#'
#' @title summarize_REMIT_weather_data

#' @param dat names of parquet files with the nasa data
#' @param weather_vars vars of nasa power data
#' @param yrs years of downloaded nasa power data
#' @param path_to_out place to save parquet files
#' @return names of saved parquet files 
#' @author Morgan Kain
#' @export

summarize_REMIT_weather_data <- function(dat, weather_vars, yrs, path_to_out) {
  
  all_dat <- map(yrs, .f = function(this_year) {
    
    print(this_year)
    
    fileset <- dat[grepl(this_year, dat)]
    
    tdat <- arrow::open_dataset(fileset)
    
      tdat.s <- tdat %>%
        dplyr::select(x, y, date, year, month, day, doy, all_of(names(weather_vars))) %>%
        group_by(x, y) %>%
        summarise(
          across(any_of(names(weather_vars))
                 , list(mean = ~mean(.)
                        , sd = ~sd(.)
                        , min = ~min(.)
                        , max = ~max(.))
                 , .names = "{.fn}")
        ) %>% collect() %>%
        mutate(range = max - min) %>%
        mutate(year = this_year)
      
      tdat.s
    
  })
  
  all_dat <- all_dat %>% do.call("rbind", .) %>%
    group_by(x, y) %>%
    summarize(
      mean  = mean(mean)
    , sd    = mean(sd)
    , min   = mean(min)
    , max   = mean(max)
    , range = mean(range)
    ) %>% rename_with(
      .fn   = ~ paste0(names(weather_vars), "_", .x)
      , .cols = !c(x, y)
    )
  
  all_dat %>% arrow::write_parquet(
    paste(path_to_out, "/summarized_weather_", names(weather_vars), ".parquet", sep = "")
  , compression = "gzip", compression_level = 5)
  
  return(paste(path_to_out, "/summarized_weather_", names(weather_vars), ".parquet", sep = ""))
  
}


#' @title combine_REMIT_weather_data

#' @param dat names of parquet files of summarized nasa power data
#' @param path_to_out place to save parquet files
#' @return single parquet file 
#' @author Morgan Kain
#' @export

combine_REMIT_weather_data   <- function(dat, path_to_out) {
  
  joined_df <- map(dat, .f = function(this_file) { arrow::read_parquet(this_file) }) %>% 
    reduce(., left_join, by = c("x", "y"))
  
  joined_df %>% arrow::write_parquet(
    paste(path_to_out, "/combined_weather", ".parquet", sep = "")
    , compression = "gzip", compression_level = 5)
  
  return(paste(path_to_out, "/combined_weather", ".parquet", sep = ""))
  
}


#' @title impute_REMIT_weather_data

#' @param dat names of parquet files of summarized nasa power data
#' @param path_to_out place to save parquet files
#' @return single parquet file 
#' @author Morgan Kain
#' @export

impute_REMIT_weather_data   <- function(dat, path_to_out) {
  
  df <- arrow::read_parquet(dat)
  
  cols_to_fill <- setdiff(names(df), c("x", "y"))
  
  for (col in cols_to_fill) { df <- fill_na_nn(df, col) }
  
  df %>% arrow::write_parquet(
    paste(path_to_out, "/finalized_weather", ".parquet", sep = "")
    , compression = "gzip", compression_level = 5)
  
  return(paste(path_to_out, "/finalized_weather", ".parquet", sep = ""))
  
}


# Create a helper function to fill NAs using nearest neighbor
fill_na_nn <- function(df, target_col) {
  complete_rows <- df %>% filter(!is.na(.data[[target_col]]))
  missing_rows  <- df %>% filter(is.na(.data[[target_col]]))
  
  if (nrow(complete_rows) == 0 || nrow(missing_rows) == 0) {
    return(df)  # Nothing to fill
  }
  
  nn_index <- get.knnx(
    data = complete_rows[, c("x", "y")],
    query = missing_rows[, c("x", "y")],
    k = 1
  )$nn.index
  
  df[df[[target_col]] %>% is.na(), target_col] <- complete_rows[nn_index, target_col]
  return(df)
}

