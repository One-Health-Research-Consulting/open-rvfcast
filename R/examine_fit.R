#' Save the model's hex-level predictions (train) or export the forward forecasts (forecast)
#'
#' PURPOSE = train: saves the hex-level predictions of one fitted model on its assessment data
#' (one row per base date, hex and forecast window with prob_pred and true_out). These files are
#' row-bound by the ex_fits.all_probs_raw target and evaluated by run_comprehensive_performance().
#'
#' PURPOSE = forecast: exports the forecasts for the most recent base date as json, one file for
#' all of Africa and one for the chosen country (hexes with their ADM2 overlap shares).
#'
#' @title examine_fits_within
#' @param model_out target model_out_for_eval
#' @param test_data splitted_data$test_data
#' @param regions Hex grid (the region_map target); used for the country forecast export
#' @param outpath Folder for the saved hex-level predictions (train)
#' @param outpath_for_for Folder for the forecast json files (forecast)
#' @param purpose whether the main purpose of this run is for fitting and evaluation (train) or forecasting (forecast)
#' @param country_code ISO3 code of the country given its own forecast json (forecast)
#' @param overwrite overwrite saved predictions or not (train)
#' @return Path to the saved predictions (train) or paths to the two json files (forecast)
#' @author Morgan Kain
#' @export
examine_fits_within <- function(model_out, test_data, regions, outpath, outpath_for_for
                                , purpose, country_code = "ZAF", overwrite = FALSE) {

  print(model_out$outer_fold_id)

  ## Combine predictions with the data
  dat_with_pred <- model_out$preds[[1]] |>
    mutate(index = model_out$assess_data[[1]], .before = 1) |>
    mutate(outbreak = as.numeric(as.character(outbreak))) |>
    left_join(test_data |> dplyr::filter(index %in% model_out$assess_data[[1]]))

  if (purpose == "train") {

    outname <- paste0(outpath, "/predictions_raw_", model_out$type[1], "_", model_out$outer_fold_id, ".qs")

    if (file.exists(outname) && !overwrite) return(outname)

    ## One row per base date, hex and forecast window (the rows run_comprehensive_performance() scores)
    hex_preds <- dat_with_pred |>
      dplyr::summarize(
        prob_pred = 1 - prod(1 - .pred_1)
      , true_out  = max(outbreak)
      , .by = c(date, shapeName, forecast_interval)) |>
      dplyr::mutate(outer_fold_id = model_out$outer_fold_id, .before = 1)

    qsave(hex_preds, outname)

    outname

  ## forecasting requires less, can always add to later if desired, though can't 
   ## really evaluate given that these are true forecasts without known info
  } else {

    eval_regions <- regions[[1]]

    dat_with_pred <- dat_with_pred |>
      filter(date == max(date)) |>
      dplyr::select(
        shapeName, Country, ADM2, Proportion_ADM2, date
        , forecast_interval, .pred_1
      ) |>
      rename(outbreak_probability = .pred_1, H3 = shapeName)

    country_preds <- dat_with_pred |> filter(Country == country_code)

    country_preds <- eval_regions %>%
      rename(H3 = shapeName) %>%
      left_join(., country_preds) %>%
      filter(!is.na(Proportion_ADM2))

    country_preds.share <- country_preds |>
      as.data.frame() %>%
      dplyr::select(-geometry)

    ## Create folder if it is needed
    create_data_directory(outpath_for_for)

    africa_outpath  <- paste0(outpath_for_for, "/forecasts_for_", "Africa", "_", unique(dat_with_pred$date), ".json")
    country_outpath <- paste0(outpath_for_for, "/forecasts_for_", country_code, "_", unique(dat_with_pred$date), ".json")

    ## Save the two files as JSONs (digits = NA keeps full precision; the default rounds small probabilities to 0)
    write_json(dat_with_pred, africa_outpath, pretty = TRUE, digits = NA)
    write_json(country_preds.share, country_outpath, pretty = TRUE, digits = NA)

    c(africa_outpath, country_outpath)

  }

}
