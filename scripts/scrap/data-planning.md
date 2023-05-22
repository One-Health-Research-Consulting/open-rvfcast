initial-database-planning
================
2023-03-23

#### Weather data (NASA)

Files are split by year and region

``` r
ex_nasa_recorded_weather_local <- tar_read(nasa_recorded_weather_local, store = h("_targets"))[[1]]
ex_nasa_recorded_weather_local <- read_parquet(h(ex_nasa_recorded_weather_local))
weather <- ex_nasa_recorded_weather_local |>
  select(x = LON, y = LAT, date = YYYYMMDD, relative_humidity = RH2M, temperature = T2M, precipitation = PRECTOTCORR)
head(weather)
```

    ## # A tibble: 6 × 6
    ##       x     y date       relative_humidity temperature precipitation
    ##   <dbl> <dbl> <date>                 <dbl>       <dbl>         <dbl>
    ## 1  13.8  7.75 1993-01-01              58.7        23.9          0.01
    ## 2  14.2  7.75 1993-01-01              58.2        24.4          0.02
    ## 3  14.8  7.75 1993-01-01              58.8        25.1          0.02
    ## 4  15.2  7.75 1993-01-01              58.8        25.8          0.02
    ## 5  15.8  7.75 1993-01-01              57.7        26.3          0.02
    ## 6  16.2  7.75 1993-01-01              57.0        26.9          0.02

### Forecast data

Currently split by year need to split by year, step, variable

``` r
ex_ecmwf_forecasts_preprocessed_local <- tar_read(ecmwf_forecasts_preprocessed_local, store = h("_targets"))[[1]]
ex_ecmwf_forecasts_preprocessed_local <- read_parquet(h(ex_ecmwf_forecasts_preprocessed_local))
combos <- expand.grid(c("mean", "min", "max", "sd"), c("2m_dewpoint_temperature", "2m_temperature", "total_precipitation"), paste0("ahead_month_", 1:6)) 
vars <- paste(combos[, 1], combos[, 2], combos[, 3], sep = "_")
length(vars)
```

    ## [1] 72

``` r
c("x", "y", vars, "days_til_first_forecast") 
```

    ##  [1] "x"                                         
    ##  [2] "y"                                         
    ##  [3] "mean_2m_dewpoint_temperature_ahead_month_1"
    ##  [4] "min_2m_dewpoint_temperature_ahead_month_1" 
    ##  [5] "max_2m_dewpoint_temperature_ahead_month_1" 
    ##  [6] "sd_2m_dewpoint_temperature_ahead_month_1"  
    ##  [7] "mean_2m_temperature_ahead_month_1"         
    ##  [8] "min_2m_temperature_ahead_month_1"          
    ##  [9] "max_2m_temperature_ahead_month_1"          
    ## [10] "sd_2m_temperature_ahead_month_1"           
    ## [11] "mean_total_precipitation_ahead_month_1"    
    ## [12] "min_total_precipitation_ahead_month_1"     
    ## [13] "max_total_precipitation_ahead_month_1"     
    ## [14] "sd_total_precipitation_ahead_month_1"      
    ## [15] "mean_2m_dewpoint_temperature_ahead_month_2"
    ## [16] "min_2m_dewpoint_temperature_ahead_month_2" 
    ## [17] "max_2m_dewpoint_temperature_ahead_month_2" 
    ## [18] "sd_2m_dewpoint_temperature_ahead_month_2"  
    ## [19] "mean_2m_temperature_ahead_month_2"         
    ## [20] "min_2m_temperature_ahead_month_2"          
    ## [21] "max_2m_temperature_ahead_month_2"          
    ## [22] "sd_2m_temperature_ahead_month_2"           
    ## [23] "mean_total_precipitation_ahead_month_2"    
    ## [24] "min_total_precipitation_ahead_month_2"     
    ## [25] "max_total_precipitation_ahead_month_2"     
    ## [26] "sd_total_precipitation_ahead_month_2"      
    ## [27] "mean_2m_dewpoint_temperature_ahead_month_3"
    ## [28] "min_2m_dewpoint_temperature_ahead_month_3" 
    ## [29] "max_2m_dewpoint_temperature_ahead_month_3" 
    ## [30] "sd_2m_dewpoint_temperature_ahead_month_3"  
    ## [31] "mean_2m_temperature_ahead_month_3"         
    ## [32] "min_2m_temperature_ahead_month_3"          
    ## [33] "max_2m_temperature_ahead_month_3"          
    ## [34] "sd_2m_temperature_ahead_month_3"           
    ## [35] "mean_total_precipitation_ahead_month_3"    
    ## [36] "min_total_precipitation_ahead_month_3"     
    ## [37] "max_total_precipitation_ahead_month_3"     
    ## [38] "sd_total_precipitation_ahead_month_3"      
    ## [39] "mean_2m_dewpoint_temperature_ahead_month_4"
    ## [40] "min_2m_dewpoint_temperature_ahead_month_4" 
    ## [41] "max_2m_dewpoint_temperature_ahead_month_4" 
    ## [42] "sd_2m_dewpoint_temperature_ahead_month_4"  
    ## [43] "mean_2m_temperature_ahead_month_4"         
    ## [44] "min_2m_temperature_ahead_month_4"          
    ## [45] "max_2m_temperature_ahead_month_4"          
    ## [46] "sd_2m_temperature_ahead_month_4"           
    ## [47] "mean_total_precipitation_ahead_month_4"    
    ## [48] "min_total_precipitation_ahead_month_4"     
    ## [49] "max_total_precipitation_ahead_month_4"     
    ## [50] "sd_total_precipitation_ahead_month_4"      
    ## [51] "mean_2m_dewpoint_temperature_ahead_month_5"
    ## [52] "min_2m_dewpoint_temperature_ahead_month_5" 
    ## [53] "max_2m_dewpoint_temperature_ahead_month_5" 
    ## [54] "sd_2m_dewpoint_temperature_ahead_month_5"  
    ## [55] "mean_2m_temperature_ahead_month_5"         
    ## [56] "min_2m_temperature_ahead_month_5"          
    ## [57] "max_2m_temperature_ahead_month_5"          
    ## [58] "sd_2m_temperature_ahead_month_5"           
    ## [59] "mean_total_precipitation_ahead_month_5"    
    ## [60] "min_total_precipitation_ahead_month_5"     
    ## [61] "max_total_precipitation_ahead_month_5"     
    ## [62] "sd_total_precipitation_ahead_month_5"      
    ## [63] "mean_2m_dewpoint_temperature_ahead_month_6"
    ## [64] "min_2m_dewpoint_temperature_ahead_month_6" 
    ## [65] "max_2m_dewpoint_temperature_ahead_month_6" 
    ## [66] "sd_2m_dewpoint_temperature_ahead_month_6"  
    ## [67] "mean_2m_temperature_ahead_month_6"         
    ## [68] "min_2m_temperature_ahead_month_6"          
    ## [69] "max_2m_temperature_ahead_month_6"          
    ## [70] "sd_2m_temperature_ahead_month_6"           
    ## [71] "mean_total_precipitation_ahead_month_6"    
    ## [72] "min_total_precipitation_ahead_month_6"     
    ## [73] "max_total_precipitation_ahead_month_6"     
    ## [74] "sd_total_precipitation_ahead_month_6"      
    ## [75] "days_til_first_forecast"

``` r
# is it "how many days until 'month 1' begins?" or "how many days are we into `month 1`"?
# if it's the former, then maybe we are using month 2 from the previous forecast release as our "month 1" value?
```

#### NDVI (sentinel 2018-present)

Files are split by 10 day satellite period (all Africa in file) Looks
like the coverage is continuous and may overlap by a day Note that this
is s3a only. Need to confirm if we need s3b as well.

``` r
ex_sentinel_ndvi <- tar_read(ndvi_local, store = h("_targets"))
ndvi_file_name <- ex_sentinel_ndvi[[1]]
ex_sentinel_ndvi <- terra::rast(h(ndvi_file_name))
ex_sentinel_ndvi <- as.data.frame(ex_sentinel_ndvi, xy = TRUE) |> as_tibble() |> slice(1:100)
start_date <- str_extract(ndvi_file_name, "(?<=_)(\\d{8})(?=T\\d{6})")
end_date <- str_extract(ndvi_file_name, "(?<=_)(\\d{8})(?=T\\d{6}_cache)")

ndvi <- crossing(tibble(date = seq.Date(from = ymd(start_date), to = ymd(end_date), by = "day")), ex_sentinel_ndvi)
head(ndvi)
```

    ## # A tibble: 6 × 4
    ##   date           x     y  NDVI
    ##   <date>     <dbl> <dbl> <dbl>
    ## 1 2018-09-22 -26.0  38.0 0.412
    ## 2 2018-09-22 -26.0  38.0 0.412
    ## 3 2018-09-22 -26.0  38.0 0.412
    ## 4 2018-09-22 -26.0  38.0 0.412
    ## 5 2018-09-22 -26.0  38.0 0.412
    ## 6 2018-09-22 -26.0  38.0 0.412

#### Notes on CV and variable selection

<https://arxiv.org/pdf/2303.07334.pdf> see `spatialsample` package
options: blocking, spatial clustering, leave one disc out

<https://onlinelibrary.wiley.com/doi/10.1111/geb.13635>

- Map accuracy estimates based on the relationship between the
  dissimilarity index (based on similarity of predictors in the holdout
  set to the training set) and the model performance

- The dissimilarity index is the normalized Euclidean distance to the
  nearest training data point in the multivariate predictor space, with
  predictors being scaled and weighted by their respective importance in
  the model (see Meyer & Pebesma, 2021, for more details on the
  calculation of the dissimilarity index). The area of applicability is
  then derived by applying a threshold to the dissimilarity index. The
  threshold is the (outlier-removed) maximum dissimilarity index of the
  training data derived via cross-validation.

- Reduce the number of predictors by spatial variable selection and
  compare the map accuracy and the area of applicability between the
  models using the full predictor set to models using the reduced
  predictor set in order to measure the benefits of spatial variable
  selection
