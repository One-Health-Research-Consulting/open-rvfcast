################################################################################
## Compare predicted H3 hex forecasts with the FAO RVF decision support tool 
## (DST) monthly risk maps
################################################################################

## NOTE: This script does not run as part of the targets pipeline. It can be
 ## run without running any of the preambles in any of the _targets.R files.

## However, also NOTE: 

## This script downloads FAO VAXCRM-MEA months, summarizes them to hexes and
 ## scores the summarized output against hex-month outbreaks

## Extra packages needed for this analysis
library(httr2)

## Load series of needed functions
source("secondary_analyses/compare_to_fao_main_functions.R")
source("secondary_analyses/compare_to_fao_helper_functions.R")

## This script also needs functions stored in the two scripts below, so sourcing
 ## them here even if they would be brought in anyway if any targets pipeline
 ## preamble is also run
## R/link_predictions_to_outbreaks.R -- read_hex_predictions()
source("R/link_predictions_to_outbreaks.R")
## R/run_comprehensive_performance.R -- make_forecast_windows(), read_harvest_predictions()
source("R/run_comprehensive_performance.R")

## Path to FAO predictions
fao_api     <- "https://data.apps.fao.org/gismgr/api/v2/catalog/workspaces/RVF/mapsets"

## Better labels for the FAO classes
fao_classes <- c(
  "0" = "no data / no risk"
, "1" = "risk current month only"
, "2" = "risk last month only"
, "3" = "persistent risk")

## Download FAO months of interest. Could download files for dates extending back into 
 ## the training period, but default will be to download and compare with just the
 ## test period for our model. 
 ## Adjusted easily here
## NOTE: takes a bit, so keeping files on disk is sensible. Function checks for files on 
 ## disk (a given month) before downloading
fao_months <- seq(as.Date("2021-01-01"), as.Date("2026-10-01"), by = "month") |> format("%Y-%m")
fao_files  <- rvf_fetch(
  month    = fao_months
, api_path = fao_api)$path

## Hexes as used by the targets pipeline
region_hexes <- readRDS("data/region_hexes_small.Rds")

## Summarize FAO classes to hexes (output cached in data/fao_rvf_rasters)
fao_hex <- summarize_fao_dst_to_hexes(
  raster_files = fao_files
, region_hexes = region_hexes)

## Test-period predictions; to add the outer-fold (training-period) predictions, bind
 ## read_harvest_predictions("outputs/calibration_harvest") to these and pass the tibble
## NOTE: Takes about 2 min or so (no progress bar)
fao_comparison <- compare_to_fao_dst(
  ## Load a recent saved file of predictions; running outside of the main targets
   ## pipeline, so provide the actual path here
  predictions          = "outputs/fit_evaluation/ex_fits.all_probs_raw_2026-10-03.qs"
  ## NOTE: the default (with this as NULL is to set the same amount of flags in our
   ## fitted model as show up in the FAO tool **overall across the full test period**
   ## -- which causes this comparison to:
   ## A) focus more on how many of the true outbreaks are captured by both tools
   ## B) compare trends in false positives by month over time (possible because 
    ## the same amount flag is across the full test period and FAO varies over time)
  ## The alternative is to set a fixed model probability that defines "high" 
   ## (the probability of an outbreak in the hex within the forecast window). 
   ## FAO "high" is unaffected by this argument: it is set in summarize_fao_dst_to_hexes() 
   ## (≥ min_flag_frac of the hex in risk classes 1–3, with class 1 or 3 dominant). 
   ## With a fixed threshold the two products no longer spend the same alert budget.
, model_high_threshold = 0.005
, fao_hex              = fao_hex
, region_hexes         = region_hexes
  ## See function header for details for these two arguments
, fao_month_offset     = 0
, month_alignment      = "issue_month")

## Lots gets saved internally in compare_to_fao_dst, but this saves the "master"
 ## multi-panel comparison figure.
## NOTE: figure is really tall so won't print well in R console, so saving file to
 ## disk for inspection
ggsave("secondary_analyses/fao_comparison_B.png"
       , plot = fao_comparison$plots$monthly_comparison
       , width = 8, height = 10, units = "in", dpi = 300)
