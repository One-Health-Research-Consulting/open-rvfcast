#' Build lagged variables, join in cases, and aggregate
#'
#'
#' @title lag_join_aggregate

#' @param file_list names of the parquets saved separately (only fix I could get working for whatever reason)
#' @param processed_dates prepared dates for each parquet for building lagged variables
#' @param cov_files list of covariate parquet files. Here the masked and clustered data
#' @param rvf_response summarized case data. When built from country-index-flagged outbreaks
#'   (see get_rvf_response), also carries country_index_outbreak, aggregated up to the hex
#'   level here alongside cases
#' @param sero_layer built seroprevalence for all hexes
#' @param neighbor_outbreaks outbreak history in neighboring hexes
#' @param out_dir Place to save output
#' @param overwrite Boolean to recalculate and save over a previously saved file or not
#' @return character vector path to parquet files
#' @author Morgan Kain
#' @export

lag_join_aggregate <- function (
      file_list
    , processed_dates
    , cov_files
    , rvf_response
    , sero_layer
    , neighbor_outbreaks
    , out_dir
    , all_dates
    , overwrite = FALSE
) {

  ## 0) Logistics stuff

  ## file_list is a placeholder (NA) when there was nothing new to process this cycle, added
   ## purely so the calling target has a non-empty vector to dynamically branch over; bail out
   ## here so this branch gets dropped via error = "null" instead of indexing into an empty list
  if (length(processed_dates) == 0) stop("no new dates to process this cycle")

  ## Of all of the preprocessed_dates which is the one needed for this branch.
   ## NOTE: Rather messy way to do this, but was having so many targets issues and this is the
   ## method I could get working
  processed_dates <- processed_dates[[
    which(lapply(processed_dates, FUN = function(x) grepl(file_list, x$filename[1])) |> unlist())
  ]]

  save_filename <- paste(
    out_dir
    , "/"
    , paste(out_dir |> strsplit("data/") |> unlist() |> tail(1), unique(processed_dates$date), sep = "_")
    , ".parquet"
    , sep = ""
  ) |>
  as.character()

  ## Check if file already exists and can be read
  error_safe_read_parquet <- possibly(arrow::read_parquet, NULL)

  if (!is.null(error_safe_read_parquet(save_filename)) && !overwrite) {
    message("file already exists and can be loaded, skipping processing")
    return(save_filename)
  }
  
  ## Load sero layer
  seroprev <- read_parquet(sero_layer) |> 
    rename(anomaly_not_sero = pred_sero) |>
    dplyr::select(-c(historical_sd, historical_mean))

  ## Load cases
  cases <- read_parquet(rvf_response)

  ## Build a soil mapping key for use within the loop for all dates
  ## see https://cteco.uconn.edu/guides/Soils_Drainage.htm
  soil_drainage_key <- data.frame(
    old = c("E", "SE", "W", "MW", "I", "P", "VP")
  , new = c(1, 2, 3, 4, 5, 6, 7)
  )

  ## 2) Build lagged variables, 3) Join cases, and 4) Summarize covariate data and outbreaks to sub regions
  ## one .parquet file at a time
  fdat <- read_parquet(file_list) |>
    sf::st_drop_geometry() |>
    ungroup()

  ## Could maybe [?] have done this earlier, but is kinda part of data aggregation
  ## so seems ok for now
  ## Want soil drainage to be ordinal, which for a tree based method involves just treating it
  ## as numeric. So cant have UNK in soil drainage, so converting UNK to nearest neighbor values
  ## first, then converting to numeric based on the soil drainage then averaging and rounding
  fdat$soil_drainage[fdat$soil_drainage == "UNK"] <- NA

  fdat <- sf::st_as_sf(fdat, coords = c("x", "y"))
  fdat$soil_drainage[is.na(fdat$soil_drainage)] <- fdat[!is.na(fdat$soil_drainage), ]$soil_drainage[
    sf::st_nearest_feature(fdat[is.na(fdat$soil_drainage), ], fdat[!is.na(fdat$soil_drainage), ])
  ]

  fdat$soil_texture[fdat$soil_texture == "UNK"] <- NA

  fdat <- sf::st_as_sf(fdat, coords = c("x", "y"))
  fdat$soil_texture[is.na(fdat$soil_texture)] <- fdat[!is.na(fdat$soil_texture), ]$soil_texture[
    sf::st_nearest_feature(fdat[is.na(fdat$soil_texture), ], fdat[!is.na(fdat$soil_texture), ])
  ]

  coords <- sf::st_coordinates(fdat)
  fdat   <- sf::st_drop_geometry(fdat)
  fdat   <- fdat |> mutate(x = coords[, 1], y = coords[, 2], .before = 1)

  suppressMessages({
    fdat <- fdat |> mutate(
      soil_drainage = plyr::mapvalues(
        soil_drainage
      , from = soil_drainage_key$old
      , to   = soil_drainage_key$new
      )
    ) |>
    mutate(
      soil_texture  = as.factor(soil_texture)
    , soil_drainage = as.factor(soil_drainage)
    )
  })
  
  ## Attach seroprevalence data
  fdat <- fdat |> left_join(seroprev |> rename(shapeName = h3_id))

  ## First, find the variables that are static and forecasted -- these do not need to be lagged

  ## Covariates for lagging
  near_lagging_names <- fdat |> dplyr::select(
    contains("anomaly"), -contains("forecast")
  ) |>
  names()
  
  far_lagging_names <- fdat |> dplyr::select(contains("sero")) |> names()

  ## Forecasted and Static covariates
  fdat.f <- fdat |> dplyr::select(-all_of(near_lagging_names), -slope, -aspect)

  ## split the needed dates by cov type
  near_dates <- processed_dates |> filter(purpose == "all") |> rowwise() |> group_split()
  far_dates  <- processed_dates |> filter(purpose == "sero") |> rowwise() |> group_split()
  
  ## process each short-term lag for anomaly data
  all_lags_near <- lapply(near_dates, FUN = function(this_set) {

    lag_gap <- as.numeric(this_set$date - this_set$lag_floors)

    file_nums    <- this_set$file_nums[[1]]
    needed_dates <- all_dates[file_nums, ]$dates
    needed_files <- purrr::map(needed_dates, .f = function(x) cov_files[grepl(x, cov_files)]) |> unlist()

    tdat <- arrow::open_dataset(needed_files) |>
      collect() |>
      ungroup() |>
      left_join(seroprev |> rename(shapeName = h3_id)) |>
      dplyr::select(x, y, shapeName, Country, all_of(near_lagging_names))
  
    ## Extract out the average of the variables over the parquet files that are needed
    ## for the given lag
    tdat.s <- tdat |>
      group_by(x, y, shapeName, Country) |>
      summarize(across(where(is.numeric), mean), .groups = "keep") |>
      rename_with(
        ~ paste0(.x, paste("_", lag_gap, sep = "")),
        .cols = starts_with("anomaly")
      )
    
    tdat.s

  }) |>
    reduce(left_join, by = c("x", "y", "shapeName", "Country"))

  ## process each long-term lag for anomaly data
  sero_lags_far <- lapply(far_dates, FUN = function(this_set) {
    
    lag_gap <- as.numeric(this_set$date - this_set$lag_floors)
    
    file_nums    <- this_set$file_nums[[1]]
    needed_dates <- all_dates[file_nums, ]$dates
    needed_files <- purrr::map(needed_dates, .f = function(x) cov_files[grepl(x, cov_files)]) |> unlist()
    
    tdat <- arrow::open_dataset(needed_files) |>
      collect() |>
      ungroup() |>
      left_join(seroprev |> rename(shapeName = h3_id)) |>
      dplyr::select(x, y, shapeName, Country, all_of(far_lagging_names))
    
    ## Extract out the average of the variables over the parquet files that are needed
    ## for the given lag
    tdat.s <- tdat |>
      group_by(x, y, shapeName, Country) |>
      summarize(across(where(is.numeric), mean), .groups = "keep") |>
      rename_with(
        ~ paste0(.x, paste("_", lag_gap, sep = "")),
        .cols = starts_with("anomaly")
      )
    
    tdat.s
    
  }) |>
    reduce(left_join, by = c("x", "y", "shapeName", "Country"))
  
  ## Build the final covariate data frame
  fdat.fc <- fdat.f |> left_join(
    left_join(all_lags_near, sero_lags_far, by = c("x", "y", "shapeName", "Country"))
  , by = c("x", "y", "shapeName", "Country"))

  ## join cases
  cases.t <- cases |>
    filter(date == unique(fdat.fc$date)) |>
    dplyr::select(-forecast_start, -forecast_end) |>
    ## Safety
    dplyr::mutate(x = round(x, 4), y = round(y, 4))

  ## Issue persisting with projection into the template leading to small differences in
  ## x y coordinates. Best I can come up with to remedy this is to snap each case to the nearest
  ## (x, y) pair that actually exists in fdat.fc so we use sf::st_nearest_feature to
  ## find the nearest real covariate cell.
  if (nrow(cases.t) > 0) {
    fdat_xy <- fdat.fc |>
      ## Safety
      mutate(x = round(x, 4), y = round(y, 4)) |>
      distinct(x, y) |>
      sf::st_as_sf(coords = c("x", "y"))

    nearest_idx <- sf::st_nearest_feature(sf::st_as_sf(cases.t, coords = c("x", "y")), fdat_xy)
    snapped_xy  <- sf::st_coordinates(fdat_xy[nearest_idx, ])

    cases.t <- cases.t |>
      mutate(x = snapped_xy[, 1], y = snapped_xy[, 2]) |>
      group_by(x, y, date, forecast_interval) |>
      summarize(
        cases = sum(cases, na.rm = TRUE)
        ## Same snapped (x, y) grouping as cases, so any country-index case keeps landing
         ## on the identical cell as the case it came from
      , country_index_outbreak = max(country_index_outbreak, na.rm = TRUE)
      , .groups = "drop")
  }

  fdat.fcc <- fdat.fc |>
    ## Safety
    mutate(x = round(x, 4), y = round(y, 4)) |>
    left_join(cases.t, by = c("x", "y", "date", "forecast_interval")) |>
    relocate(cases, .after = shapeName) |>
    mutate(cases = ifelse(is.na(cases), 0, cases)) |>
    mutate(country_index_outbreak = ifelse(is.na(country_index_outbreak), 0, country_index_outbreak))

  ## check join issue
  cases_a <- cases.t$cases |> sum()
  cases_b <- fdat.fcc |> filter(cases > 0) |> pull(cases) |> sum()

  if (cases_a != cases_b) stop("join issue with cases")

  ## Finally, first step in dealing with NAN or infinite: 1) convert to NA so that it does not contribute to the mean
  fdat.fcc <-  fdat.fcc |>
    mutate(across(starts_with("anomaly"), ~ replace(., is.nan(.), NA))) |>
    mutate(across(starts_with("anomaly"), ~ replace(., is.infinite(.), NA)))

  ## and then reduce
  fdat.final <- fdat.fcc |>
    dplyr::select(-c(x, y, doy, month, year)) |>
    group_by(shapeName, date, forecast_interval) |>
    summarize(
      cases = sum(cases, na.rm = TRUE)
      ## Excluded from the generic numeric mean below and aggregated by max instead --
       ## it's a 0/1 flag, not a magnitude, so any grid cell in this hex/window flagged
       ## as a country-index case should keep the hex flagged, not get averaged away
    , country_index_outbreak = max(country_index_outbreak, na.rm = TRUE)
    , across(where(is.numeric) & !all_of(c("cases", "country_index_outbreak")), ~ mean(.x, na.rm = TRUE))
    , across(where(is.factor), ~ stat_mode(.x, na.rm = TRUE))
    , .groups = "keep") |>
    mutate(outbreak = ifelse(cases > 0, 1, 0), .after = cases) |>
    ## Finally, second step in dealing with NAN or infinite: 2) convert (rare) NAN, NA, and inf to 0
    mutate(across(starts_with("anomaly"), ~ replace(., is.nan(.), 0))) |>
    mutate(across(starts_with("anomaly"), ~ replace(., is.na(.), 0))) |>
    mutate(across(starts_with("anomaly"), ~ replace(., is.infinite(.), 0))) |>
    ungroup()

  ## Triple check no cases have been lost
  cases_c <- fdat.final |> filter(cases > 0) |> pull(cases) |> sum()
  if (cases_a != cases_c) stop("join issue with cases")

  ## and get country and ADM2 region for each hex and the proportion of that hex in that country and ADM2 region
  dominant_country <- fdat.fcc |>
    dplyr::select(Country, shapeName) |>
    group_by(shapeName, Country) |>
    summarize(n = n()) |>
    ungroup(Country) |>
    mutate(Proportion_Country = n / sum(n)) |>
    arrange(desc(Proportion_Country)) |>
    slice(1) |>
    ungroup() |>
    dplyr::select(-n)

  dominant_ADM2 <- fdat.fcc |>
    dplyr::select(ADM2, shapeName) |>
    group_by(shapeName, ADM2) |>
    summarize(n = n()) |>
    ungroup(ADM2) |>
    mutate(Proportion_ADM2 = n / sum(n)) |>
    arrange(desc(Proportion_ADM2)) |>
    slice(1) |>
    ungroup() |>
    dplyr::select(-n)

  ## Finally, now have the country and ADM2 that has the largest share of the H3 hex
  fdat.final <- fdat.final |>
    left_join(dominant_country) |>
    left_join(dominant_ADM2) |>
    relocate(c(Country, Proportion_Country, ADM2, Proportion_ADM2), .after = shapeName)

  ## Join the neighbor observed-outbreak history term (per hex, per model date). It is known at the
  ## model date so it is constant across forecast intervals; filter to this branch's date, join by
  ## hex, and fill hexes with no recent neighbor activity with 0
  if (!is.null(neighbor_outbreaks)) {
    
    target_date <- as.Date(unique(processed_dates$date))

    neigh_this <- read_parquet(neighbor_outbreaks) |>
      mutate(date = as.Date(date)) |>
      filter(date == target_date) |>
      dplyr::select(-date)

    fdat.final <- fdat.final |>
      left_join(neigh_this, by = "shapeName") |>
      mutate(across(starts_with("neigh_outbreak_wt_"), ~ replace(., is.na(.), 0)))
  }

  arrow::write_parquet(fdat.final, save_filename, compression = "gzip", compression_level = 5)

  save_filename

}


#' A setup function for lag_join_aggregate
#'
#'
#' @title prep_dates

#' @param cov_files list of covariate parquet files. Here the masked and clustered data
#' @param dates_all full list of dates
#' @return List of tibbles that contain which files are needed for each of the lags
#' @author Morgan Kain
#' @export

prep_dates <- function(cov_files, dates_all) {

  ## Extract dates from the saved files
  dates_for_predictions <-  sapply(cov_files, FUN = function(x) {
    strsplit(x, "data_") |> unlist() |> pluck(2) |> strsplit(".parquet") |> unlist() |> head(1)
  }) |>
  unname() |>
  as.Date()

  processed_dates       <- vector("list", length(dates_for_predictions))

  ## 1) Determine which parquet files are needed to build the lagged covariates
  for (i in seq_along(cov_files)) {

    files_to_avg <- data.frame(
      lag_floors   = dates_for_predictions[i] - c(30, 60, 90, 360, 720)
    , lag_ceilings = dates_for_predictions[i] - c(1, 31, 61, 91, 361)
    , purpose      = c("all", "all", "all", "sero", "sero")
    ) |>
    rowwise() |>
      mutate(
        file_nums    = which(dates_all >= lag_floors & dates_all <= lag_ceilings) |> list()
      , num_files    = length(file_nums)
      , closest_date = dates_all[-i][which((dates_all[-i] - lag_ceilings) < 0)] |> tail(1) |> list()
      )

    if (any(sapply(files_to_avg$closest_date, FUN = function(x) length(x)) == 0)) {
      processed_dates[[i]] <- NULL
    } else {

      files_to_avg <- files_to_avg |>
      mutate(
        day_diff = dates_for_predictions[i] - closest_date
      , file_nums = ifelse(num_files == 0, which(dates_for_predictions == unlist(closest_date)) |> list(), file_nums |> list())
      ) |>
      mutate(
        date    = dates_for_predictions[i]
      , filename = cov_files[i]
      , .before = 1
      )

      processed_dates[[i]] <- files_to_avg

    }

  }

  processed_dates <- Filter(Negate(is.null), processed_dates)

  processed_dates

}


#' @title combine_lja_and_append
#' @description Appends new cleaned model data to an existing joined dataset and writes a single _final_with_sero.parquet.
#' @param new_files character vector of paths to new cleaned parquet files (cleaned_region_data)
#' @param save_filename path to the existing _final.parquet produced by combine_lja
#' @param rebuild whether or not all data files are being rebuilt
#' @param out_dir directory to write the output file
#' @param overwrite Boolean to recalculate and save over a previously saved file or not
#' @return character path to the written _final_with_sero.parquet
#' @author Morgan Kain
#' @export
combine_lja_and_append <- function(new_files, save_filename, rebuild, out_dir, overwrite) {

  ## Return existing output file unchanged when there is nothing new to add and no explicit
   ## refresh was requested; overwrite = TRUE forces a rejoin of sero_layer onto the existing
   ## data below even with zero new_files (e.g. to pick up a corrected sero layer)
  if (length(new_files) == 0 && !overwrite) return(save_filename)

  ## Check if file already exists and can be read
  error_safe_read_parquet <- possibly(arrow::open_dataset, NULL)

  if (!is.null(error_safe_read_parquet(save_filename)) && !overwrite) {
    message("No new data, returning saved file")
    return(save_filename)
  }

  ## Read new cleaned files, if any
  new_data <- if (length(new_files) > 0) lapply(as.list(new_files), read_parquet) |> bind_rows() else NULL

  ## Read existing base data; drop pred_sero since existing_dat (save_filename) is itself a _with_sero
   ## file, so a stale pred_sero column would otherwise collide with the freshly joined one below
  safe_read <- possibly(arrow::read_parquet, NULL)
  base_data <- safe_read(save_filename)

  all_data <- if (!is.null(base_data) && !is.null(new_data) && !rebuild) {
    bind_rows(base_data, new_data)
  } else if (!is.null(base_data) && !rebuild) {
    base_data
  } else {
    new_data
  }

  ## Nothing existing and nothing new; no-op
  if (is.null(all_data)) return(save_filename)

  arrow::write_parquet(all_data, save_filename, compression = "gzip", compression_level = 5)

  save_filename

}

