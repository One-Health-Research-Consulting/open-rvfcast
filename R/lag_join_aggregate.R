#' Build lagged variables, join in cases, and aggregate
#'
#'
#' @title lag_join_aggregate

#' @param file_list names of the parquets saved separately (only fix I could get working for whatever reason)
#' @param processed_dates prepared dates for each parquet for building lagged variables
#' @param cov_files list of covariate parquet files. Here the masked and clustered data
#' @param rvf_response summarized case data
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
    , out_dir
    , all_dates
    , overwrite = FALSE
) {

  ## 0) Logistics stuff

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

  ## First, find the variables that are static and forecasted -- these do not need to be lagged

  ## Covariates for lagging
  lagging_names <- fdat |> dplyr::select(
    contains("anomaly"), -contains("forecast")
  ) |>
  names()

  ## Forecasted and Static covariates
  fdat.f <- fdat |> dplyr::select(-all_of(lagging_names), -slope, -aspect)

  ## process each lag
  all_lags <- lapply(processed_dates |> rowwise() |> group_split(), FUN = function(this_set) {

    lag_gap <- as.numeric(this_set$date - this_set$lag_floors)

    file_nums    <- this_set$file_nums[[1]]
    needed_dates <- all_dates[file_nums, ]$dates
    needed_files <- purrr::map(needed_dates, .f = function(x) cov_files[grepl(x, cov_files)]) |> unlist()

    tdat <- arrow::open_dataset(needed_files) |>
      collect() |>
      ungroup() |>
      dplyr::select(x, y, shapeName, Country, all_of(lagging_names))

    ## Extract out the average of the variables over the parquet files that are needed
    ## for the given lag
    tdat.s <- tdat |>
      group_by(x, y, shapeName, Country) |>
      summarize(across(where(is.numeric), mean), .groups = "keep") |>
      rename_with(
        ~ paste0(.x, paste("_", lag_gap, sep = "")),
        .cols = starts_with("anomaly")
      )

  }) |>
    reduce(left_join, by = c("x", "y", "shapeName", "Country"))

  ## Build the final covariate data frame
  fdat.fc <- fdat.f |> left_join(all_lags, by = c("x", "y", "shapeName", "Country"))

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
      summarize(cases = sum(cases, na.rm = TRUE), .groups = "drop")
  }

  fdat.fcc <- fdat.fc |>
    ## Safety
    mutate(x = round(x, 4), y = round(y, 4)) |>
    left_join(cases.t, by = c("x", "y", "date", "forecast_interval")) |>
    relocate(cases, .after = shapeName) |>
    mutate(cases = ifelse(is.na(cases), 0, cases))

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
    , across(where(is.numeric) & !all_of("cases"), ~ mean(.x, na.rm = TRUE))
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
      lag_floors   = dates_for_predictions[i] - c(30, 60, 90)
    , lag_ceilings = dates_for_predictions[i] - c(1, 31, 61)
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


#' A follow up function to lag_join_aggregate to combine all of the aggregated parquet files
#'
#'
#' @title combine_lja

#' @param in_dir vector of paths to all of the summarized parquet files from lag_join_aggregate
#' @param out_dir location to save the single final joined file
#' @param overwrite Boolean to recalculate and save over a previously saved file or not
#' @return character vector path to parquet file
#' @author Morgan Kain
#' @export

combine_lja <- function(in_dir, out_dir, overwrite) {
    ## 0) Logistics stuff

    ## Set filename
    save_filename <- paste(
        out_dir
      , "/"
      , paste(out_dir |> strsplit("data/") |> unlist() |> tail(1), "final", sep = "_")
      , ".parquet"
      , sep = "")

    ## Return existing file unchanged when no new data files are available
    if (length(in_dir) == 0) {
      return(save_filename)
    }

    ## Check if file already exists and can be read
    error_safe_read_parquet <- possibly(arrow::open_dataset, NULL)

    if (!is.null(error_safe_read_parquet(save_filename)) && !overwrite) {
      message("No new data, returning saved file")
      return(save_filename)
    }

    new_data <- lapply(in_dir |> as.list(), FUN = function(x) {
        tf <- read_parquet(x)
    }) |>
        bind_rows()

    ## Append new rows to existing data if present; if _final.parquet is missing (deleted after
    ## append_with_sero ran last cycle), fall back to _final_with_sero.parquet stripping pred_sero
    safe_read     <- possibly(arrow::read_parquet, NULL)
    existing_data <- safe_read(save_filename)
    if (is.null(existing_data)) {
        with_sero_file <- gsub("final.parquet", "final_with_sero.parquet", save_filename)
        existing_data <- safe_read(with_sero_file)

        if (is.null(existing_data)) {
         existing_data <- NULL
        } else {
         existing_data <- existing_data |> dplyr::select(-dplyr::any_of("pred_sero"))
        }
    }
    all_out <- if (!is.null(existing_data)) bind_rows(existing_data, new_data) else new_data

    arrow::write_parquet(all_out, save_filename, compression = "gzip", compression_level = 5)

    save_filename
}


#' @title append_with_sero
#' @description Appends new cleaned model data to an existing joined dataset, joins in the
#'   seroprevalence layer, and writes a single _final_with_sero.parquet.
#' @param new_files character vector of paths to new cleaned parquet files (cleaned_region_data)
#' @param save_filename path to the existing _final.parquet produced by combine_lja
#' @param sero_layer path to the sero layer parquet (finished_sero_layer)
#' @param out_dir directory to write the output file
#' @param overwrite Boolean to recalculate and save over a previously saved file or not
#' @return character path to the written _final_with_sero.parquet
#' @author Morgan Kain
#' @export
combine_lja_and_append_with_sero <- function(new_files, save_filename, sero_layer, out_dir, overwrite) {

  ## Return existing output file unchanged when there is nothing new to add
  if (length(new_files) == 0) return(save_filename)

  ## Check if file already exists and can be read
  error_safe_read_parquet <- possibly(arrow::open_dataset, NULL)

  if (!is.null(error_safe_read_parquet(save_filename)) && !overwrite) {
    message("No new data, returning saved file")
    return(save_filename)
  }

  ## Read new cleaned files
  new_data <- lapply(as.list(new_files), read_parquet) |> bind_rows()

  ## Read existing base data; drop pred_sero in case existing_dat (save_filename) is itself a _with_sero file
  safe_read <- possibly(arrow::read_parquet, NULL)
  base_data <- safe_read(save_filename)
  all_data  <- if (!is.null(base_data)) bind_rows(base_data, new_data) else new_data

  ## Join sero layer predictions to the full combined dataset
  slay <- read_parquet(sero_layer) |> rename(shapeName = h3_id)
  fdat <- left_join(all_data, slay)

  arrow::write_parquet(fdat, save_filename, compression = "gzip", compression_level = 5)

  ## Delete the intermediate files
  # unlink(new_files)

  save_filename

}


join_in_sero_layer <- function(region_dat, sero_layer, overwrite) {

  ## 0) Logistics stuff

  ## Set filename
  save_filename <- gsub("final.parquet", "final_with_sero.parquet", region_dat)

  ## Check if file already exists and can be read
  error_safe_read_parquet <- possibly(arrow::open_dataset, NULL)

  if (!is.null(error_safe_read_parquet(save_filename)) && !overwrite) {
    message("file already exists and can be loaded, skipping processing")
    return(save_filename)
  }

  tdat <- read_parquet(region_dat)
  slay <- read_parquet(sero_layer) |> rename(shapeName = h3_id)
  fdat <- left_join(tdat, slay)

  arrow::write_parquet(fdat, save_filename, compression = "gzip", compression_level = 5)

  save_filename

}
