
<!-- README.md is generated from README.Rmd. Please edit that file -->

# An open-source framework for Rift Valley Fever forecasting

<!-- badges: start -->

[![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![License (for code):
MIT](https://img.shields.io/badge/License%20(for%20code)-MIT-green.svg)](https://opensource.org/licenses/MIT)
[![License:
CC0-1.0](https://img.shields.io/badge/License%20(for%20data)-CC0_1.0-lightgrey.svg)](http://creativecommons.org/publicdomain/zero/1.0/)
[![License:
CC-BY-4.0](https://img.shields.io/badge/License%20(for%20text)-CC_BY_4.0-blue.svg)](http://creativecommons.org/publicdomain/zero/1.0/)
<!-- badges: end -->

### OpenRVFcast

EcoHealth Alliance’s ongoing OpenRVFcast project is developing a
generalizable, open-source modeling framework for predicting Rift Valley
Fever (RVF) outbreaks in Africa, funded by the Wellcome Trust’s
climate-sensitive infectious disease [modeling
initiative](https://wellcome.org/news/digital-tools-climate-sensitive-infectious-disease).
We aim to integrate open data sets of climatic and vegetation data with
internationally-reported outbreak data to build an modeling pipeline
that can be adapted to varying local conditions in RVF-prone regions
across the continent.

### Repository Structure and Reproducibility

- `data/` contains downloaded and transformed data sources. These data
  are .gitignored and non-EHA users will need to download the data.
- `R/` contains functions used in this analysis.
- `reports/` contains literate code for R Markdown reports generated in
  the analysis
- `outputs/` contains compiled reports and figures.
- This project uses the [{renv}](https://rstudio.github.io/renv/)
  framework to record R package dependencies and versions. Packages and
  versions used are recorded in `renv.lock` and code used to manage
  dependencies is in `renv/` and other files in the root project
  directory. On starting an R session in the working directory, run
  `renv::restore()` to install R package dependencies.
- This project uses the
  [{targets}](https://wlandau.github.io/targets-manual/) framework to
  organize build steps for analysis pipeline. The schematic figure below
  summarizes the steps. (The figure is generated using `mermaid.js`
  syntax and should display as a graph on GitHub. It can also be viewed
  by pasting the code into <https://mermaid.live>.)

Warning messages: 1: package ‘targets’ was built under R version 4.3.3
2: package ‘rmarkdown’ was built under R version 4.3.3 3: package ‘paws’
was built under R version 4.3.3 4: package ‘terra’ was built under R
version 4.3.3

``` mermaid
graph LR
subgraph Project Workflow
  subgraph Graph
    direction LR
    x97fc33c6215703a3(["rsa_polygon"]):::completed --> x8367f94bdb991b08(["rsa_polygon_spatial_weights"]):::queued
    xba6244832b5285ba(["continent_raster_template"]):::queued --> x71c93c84792ad529(["glw_AWS"]):::queued
    x5448b80c3909d641(["glw_directory"]):::completed --> x71c93c84792ad529(["glw_AWS"]):::queued
    x27dbf0f2484063f3["wahis_outbreak_history"]:::queued --> x2b50e7687b4412ab(["wahis_outbreak_history_animations_AWS"]):::queued
    x338ce62055c4090f(["wahis_outbreak_history_animations_directory"]):::completed --> x2b50e7687b4412ab(["wahis_outbreak_history_animations_AWS"]):::queued
    x5130788afbe32544["modis_ndvi_transformed"]:::queued --> x42d785b9e0106385(["ndvi_historical_means_AWS"]):::queued
    x7fef416d6ce259f3(["ndvi_historical_means_directory"]):::completed --> x42d785b9e0106385(["ndvi_historical_means_AWS"]):::queued
    xa4eb23442420052a["sentinel_ndvi_transformed"]:::queued --> x42d785b9e0106385(["ndvi_historical_means_AWS"]):::queued
    xba6244832b5285ba(["continent_raster_template"]):::queued --> x4110b36142c7dd5b(["soil_AWS"]):::queued
    x9c14f0532ee1f83c(["soil_directory"]):::completed --> x4110b36142c7dd5b(["soil_AWS"]):::queued
    x680f7450837c9229["forecasts_anomalies"]:::queued --> x72d065c3b2ed1267(["forecasts_anomalies_AWS_upload"]):::queued
    x8ff15aa322c64802(["forecasts_anomalies_directory"]):::completed --> x72d065c3b2ed1267(["forecasts_anomalies_AWS_upload"]):::queued
    xb06c08f4a8f21445(["lag_intervals"]):::completed --> xc61c34839fb8c873(["model_dates_selected"]):::queued
    xba6244832b5285ba(["continent_raster_template"]):::queued --> x680370f9b58b9f6d(["slope_preprocessed"]):::queued
    x890a8fc59a28f6b2(["slope_AWS"]):::queued --> x680370f9b58b9f6d(["slope_preprocessed"]):::queued
    x165085d61327782d(["slope_directory"]):::completed --> x680370f9b58b9f6d(["slope_preprocessed"]):::queued
    x1ef0d1881ff89dbd(["slope_urls"]):::completed --> x680370f9b58b9f6d(["slope_preprocessed"]):::queued
    xba6244832b5285ba(["continent_raster_template"]):::queued --> x82990a83bfa4db45(["glw_preprocessed"]):::queued
    x71c93c84792ad529(["glw_AWS"]):::queued --> x82990a83bfa4db45(["glw_preprocessed"]):::queued
    x5448b80c3909d641(["glw_directory"]):::completed --> x82990a83bfa4db45(["glw_preprocessed"]):::queued
    x4d4a15b2f0f1851f(["glw_urls"]):::completed --> x82990a83bfa4db45(["glw_preprocessed"]):::queued
    xba6244832b5285ba(["continent_raster_template"]):::queued --> x32725338020380f8(["get_ecmwf_forecasts_AWS"]):::queued
    x73599238bfebd1c5(["ecmwf_forecasts_api_parameters"]):::queued --> x32725338020380f8(["get_ecmwf_forecasts_AWS"]):::queued
    x16ce463b7b647c1e(["ecmwf_forecasts_transformed_directory"]):::completed --> x32725338020380f8(["get_ecmwf_forecasts_AWS"]):::queued
    x42a5375a64b48216(["aspect_directory"]):::completed --> x049b29595ee19108(["aspect_AWS"]):::queued
    xba6244832b5285ba(["continent_raster_template"]):::queued --> x049b29595ee19108(["aspect_AWS"]):::queued
    x165085d61327782d(["slope_directory"]):::completed --> x5aa9efa15ecd03d0(["slope_preprocessed_AWS_upload"]):::queued
    x680370f9b58b9f6d(["slope_preprocessed"]):::queued --> x5aa9efa15ecd03d0(["slope_preprocessed_AWS_upload"]):::queued
    x6bae1f342f811d0b(["wahis_rvf_controls_raw"]):::completed --> x2668bdb7843be979(["wahis_rvf_controls_preprocessed"]):::queued
    xba6244832b5285ba(["continent_raster_template"]):::queued --> x3b5d33025a7856bb["ecmwf_forecasts_transformed"]:::queued
    x73599238bfebd1c5(["ecmwf_forecasts_api_parameters"]):::queued --> x3b5d33025a7856bb["ecmwf_forecasts_transformed"]:::queued
    x16ce463b7b647c1e(["ecmwf_forecasts_transformed_directory"]):::completed --> x3b5d33025a7856bb["ecmwf_forecasts_transformed"]:::queued
    x32725338020380f8(["get_ecmwf_forecasts_AWS"]):::queued --> x3b5d33025a7856bb["ecmwf_forecasts_transformed"]:::queued
    xc61c34839fb8c873(["model_dates_selected"]):::queued --> x01b9e03cb52b7b05["weather_anomalies"]:::queued
    x0548e231345702f7["nasa_weather_transformed"]:::queued --> x01b9e03cb52b7b05["weather_anomalies"]:::queued
    x0df1395319c2f010(["weather_anomalies_AWS"]):::queued --> x01b9e03cb52b7b05["weather_anomalies"]:::queued
    xf94f7486eed9869c(["weather_anomalies_directory"]):::completed --> x01b9e03cb52b7b05["weather_anomalies"]:::queued
    xbd6b5d8fe3154d5a(["weather_historical_means"]):::queued --> x01b9e03cb52b7b05["weather_anomalies"]:::queued
    xb06c08f4a8f21445(["lag_intervals"]):::completed --> x0df1395319c2f010(["weather_anomalies_AWS"]):::queued
    xc61c34839fb8c873(["model_dates_selected"]):::queued --> x0df1395319c2f010(["weather_anomalies_AWS"]):::queued
    x0548e231345702f7["nasa_weather_transformed"]:::queued --> x0df1395319c2f010(["weather_anomalies_AWS"]):::queued
    xf94f7486eed9869c(["weather_anomalies_directory"]):::completed --> x0df1395319c2f010(["weather_anomalies_AWS"]):::queued
    xbd6b5d8fe3154d5a(["weather_historical_means"]):::queued --> x0df1395319c2f010(["weather_anomalies_AWS"]):::queued
    x049b29595ee19108(["aspect_AWS"]):::queued --> x155e2f0b29a20e05(["aspect_preprocessed"]):::queued
    x42a5375a64b48216(["aspect_directory"]):::completed --> x155e2f0b29a20e05(["aspect_preprocessed"]):::queued
    x213d1d2657d00cd0(["aspect_urls"]):::completed --> x155e2f0b29a20e05(["aspect_preprocessed"]):::queued
    xba6244832b5285ba(["continent_raster_template"]):::queued --> x155e2f0b29a20e05(["aspect_preprocessed"]):::queued
    x27dbf0f2484063f3["wahis_outbreak_history"]:::queued --> x53890e32519c2cdc(["wahis_outbreak_history_AWS_upload"]):::queued
    x338ce62055c4090f(["wahis_outbreak_history_animations_directory"]):::completed --> x53890e32519c2cdc(["wahis_outbreak_history_AWS_upload"]):::queued
    x659aa62eded9787b(["wahis_outbreaks"]):::queued --> xa33032ce29b67c7f(["wahis_distance_matrix"]):::queued
    x39ef63e4c3553f78(["wahis_raster_template"]):::queued --> xa33032ce29b67c7f(["wahis_distance_matrix"]):::queued
    x2b83f10567783884(["wahis_rvf_outbreaks_preprocessed"]):::queued --> x50043477563454fd(["wahis_outbreak_dates"]):::queued
    xe8b8ca5535fe5f2a(["bioclim_directory"]):::completed --> xe2930fde1049416f(["bioclim_AWS"]):::queued
    xba6244832b5285ba(["continent_raster_template"]):::queued --> xe2930fde1049416f(["bioclim_AWS"]):::queued
    xba6244832b5285ba(["continent_raster_template"]):::queued --> x5130788afbe32544["modis_ndvi_transformed"]:::queued
    xb64343d9bc0ef12e(["modis_ndvi_requests"]):::queued --> x5130788afbe32544["modis_ndvi_transformed"]:::queued
    x3f3ba2f9e89a9591(["modis_ndvi_token"]):::completed --> x5130788afbe32544["modis_ndvi_transformed"]:::queued
    xf36f13d6d1345340(["modis_ndvi_transformed_AWS"]):::queued --> x5130788afbe32544["modis_ndvi_transformed"]:::queued
    xdc843e2504e22144(["modis_ndvi_transformed_directory"]):::skipped --> x5130788afbe32544["modis_ndvi_transformed"]:::queued
    x9c9060069417a49a(["wahis_rvf_outbreaks_raw"]):::completed --> x2b83f10567783884(["wahis_rvf_outbreaks_preprocessed"]):::queued
    x2e80ddc3dafd7312(["augmented_data_directory"]):::completed --> x67bf5d7b4af55c88(["augmented_data_AWS"]):::queued
    x482d364fd551ff53(["augmented_data_sources"]):::queued --> x67bf5d7b4af55c88(["augmented_data_AWS"]):::queued
    x27dbf0f2484063f3["wahis_outbreak_history"]:::queued --> x16e1cc582647deec["wahis_outbreak_history_animations"]:::queued
    x338ce62055c4090f(["wahis_outbreak_history_animations_directory"]):::completed --> x16e1cc582647deec["wahis_outbreak_history_animations"]:::queued
    xa33032ce29b67c7f(["wahis_distance_matrix"]):::queued --> x200daf9f58e96ac5(["wahis_outbreak_history_AWS"]):::queued
    x50043477563454fd(["wahis_outbreak_dates"]):::queued --> x200daf9f58e96ac5(["wahis_outbreak_history_AWS"]):::queued
    xcc02e30ec90a7edd(["wahis_outbreak_history_directory"]):::completed --> x200daf9f58e96ac5(["wahis_outbreak_history_AWS"]):::queued
    x659aa62eded9787b(["wahis_outbreaks"]):::queued --> x200daf9f58e96ac5(["wahis_outbreak_history_AWS"]):::queued
    x39ef63e4c3553f78(["wahis_raster_template"]):::queued --> x200daf9f58e96ac5(["wahis_outbreak_history_AWS"]):::queued
    x16e1cc582647deec["wahis_outbreak_history_animations"]:::queued --> x3a8830b5def8250b(["wahis_outbreak_history_animations_AWS_upload"]):::queued
    x338ce62055c4090f(["wahis_outbreak_history_animations_directory"]):::completed --> x3a8830b5def8250b(["wahis_outbreak_history_animations_AWS_upload"]):::queued
    x01b9e03cb52b7b05["weather_anomalies"]:::queued --> xc567f473073bf453(["weather_anomalies_AWS_upload"]):::queued
    xf94f7486eed9869c(["weather_anomalies_directory"]):::completed --> xc567f473073bf453(["weather_anomalies_AWS_upload"]):::queued
    xba6244832b5285ba(["continent_raster_template"]):::queued --> xdecc37cc7e708cec(["landcover_preprocessed"]):::queued
    x955e49f3e0c22510(["landcover_AWS"]):::queued --> xdecc37cc7e708cec(["landcover_preprocessed"]):::queued
    x8894af119fe2eaa1(["landcover_directory"]):::completed --> xdecc37cc7e708cec(["landcover_preprocessed"]):::queued
    x684d7fe78b0e841d(["landcover_types"]):::completed --> xdecc37cc7e708cec(["landcover_preprocessed"]):::queued
    xe8b8ca5535fe5f2a(["bioclim_directory"]):::completed --> xb22e604d282eb0d4(["ta"]):::queued
    x1c7b6e6a1c101e59(["bioclim_preprocessed"]):::queued --> xb22e604d282eb0d4(["ta"]):::queued
    x4847fdb918188b25(["country_polygons"]):::completed --> x53c4b2fb80542353(["country_bounding_boxes"]):::queued
    x3b5d33025a7856bb["ecmwf_forecasts_transformed"]:::queued --> xe017ffc3bafa162a(["ecmwf_forecasts_transformed_AWS_upload"]):::queued
    x16ce463b7b647c1e(["ecmwf_forecasts_transformed_directory"]):::completed --> xe017ffc3bafa162a(["ecmwf_forecasts_transformed_AWS_upload"]):::queued
    x53c4b2fb80542353(["country_bounding_boxes"]):::queued --> xc54ffbea58c4afd9(["nasa_weather_coordinates"]):::queued
    xba6244832b5285ba(["continent_raster_template"]):::queued --> xd70b16641fa1b4ef(["soil_preprocessed"]):::queued
    x4110b36142c7dd5b(["soil_AWS"]):::queued --> xd70b16641fa1b4ef(["soil_preprocessed"]):::queued
    x9c14f0532ee1f83c(["soil_directory"]):::completed --> xd70b16641fa1b4ef(["soil_preprocessed"]):::queued
    xe3c4533ec81ef618(["continent_polygon"]):::skipped --> x73599238bfebd1c5(["ecmwf_forecasts_api_parameters"]):::queued
    x42a5375a64b48216(["aspect_directory"]):::completed --> xfe5a910dc093a019(["aspect_preprocessed_AWS_upload"]):::queued
    x155e2f0b29a20e05(["aspect_preprocessed"]):::queued --> xfe5a910dc093a019(["aspect_preprocessed_AWS_upload"]):::queued
    xe3c4533ec81ef618(["continent_polygon"]):::skipped --> x39ef63e4c3553f78(["wahis_raster_template"]):::queued
    xba6244832b5285ba(["continent_raster_template"]):::queued --> xf36f13d6d1345340(["modis_ndvi_transformed_AWS"]):::queued
    xcfc776190ac6b73c["modis_ndvi_bundle_request"]:::queued --> xf36f13d6d1345340(["modis_ndvi_transformed_AWS"]):::queued
    x3f3ba2f9e89a9591(["modis_ndvi_token"]):::completed --> xf36f13d6d1345340(["modis_ndvi_transformed_AWS"]):::queued
    xdc843e2504e22144(["modis_ndvi_transformed_directory"]):::skipped --> xf36f13d6d1345340(["modis_ndvi_transformed_AWS"]):::queued
    x42a5375a64b48216(["aspect_directory"]):::completed --> x890a8fc59a28f6b2(["slope_AWS"]):::queued
    xba6244832b5285ba(["continent_raster_template"]):::queued --> x890a8fc59a28f6b2(["slope_AWS"]):::queued
    xba6244832b5285ba(["continent_raster_template"]):::queued --> x955e49f3e0c22510(["landcover_AWS"]):::queued
    x8894af119fe2eaa1(["landcover_directory"]):::completed --> x955e49f3e0c22510(["landcover_AWS"]):::queued
    xa4eb23442420052a["sentinel_ndvi_transformed"]:::queued --> x6db823df8cb78984(["sentinel_ndvi_transformed_AWS_upload"]):::queued
    x3ea733d22e9c32e7(["sentinel_ndvi_transformed_directory"]):::completed --> x6db823df8cb78984(["sentinel_ndvi_transformed_AWS_upload"]):::queued
    x0381132b9136146c(["elevation_directory"]):::completed --> xd9f5e6274aef515b(["elevation_preprocessed_AWS_upload"]):::queued
    x0dffb1605751d1b1(["elevation_preprocessed"]):::queued --> xd9f5e6274aef515b(["elevation_preprocessed_AWS_upload"]):::queued
    x0548e231345702f7["nasa_weather_transformed"]:::queued --> xddb5620937cdbc01(["nasa_weather_transformed_AWS_upload"]):::queued
    x711dc87df29f0a9c(["nasa_weather_transformed_directory"]):::completed --> xddb5620937cdbc01(["nasa_weather_transformed_AWS_upload"]):::queued
    x67bf5d7b4af55c88(["augmented_data_AWS"]):::queued --> xcc8316047ac6de28(["augmented_data"]):::queued
    x2e80ddc3dafd7312(["augmented_data_directory"]):::completed --> xcc8316047ac6de28(["augmented_data"]):::queued
    x482d364fd551ff53(["augmented_data_sources"]):::queued --> xcc8316047ac6de28(["augmented_data"]):::queued
    x1f222a4448edddc4(["days_of_year"]):::completed --> xce069f3121e32dfb(["weather_historical_means_AWS"]):::queued
    xb06c08f4a8f21445(["lag_intervals"]):::completed --> xce069f3121e32dfb(["weather_historical_means_AWS"]):::queued
    x021b0407fd88c849(["lead_intervals"]):::completed --> xce069f3121e32dfb(["weather_historical_means_AWS"]):::queued
    x0548e231345702f7["nasa_weather_transformed"]:::queued --> xce069f3121e32dfb(["weather_historical_means_AWS"]):::queued
    x4407a62768444c3e(["weather_historical_means_directory"]):::completed --> xce069f3121e32dfb(["weather_historical_means_AWS"]):::queued
    x44345ceb9b3d4a81(["ndvi_historical_means"]):::queued --> x1be60916d37ebe0f(["ndvi_historical_means_AWS_upload"]):::queued
    x7fef416d6ce259f3(["ndvi_historical_means_directory"]):::completed --> x1be60916d37ebe0f(["ndvi_historical_means_AWS_upload"]):::queued
    xc61c34839fb8c873(["model_dates_selected"]):::queued --> xf9b79e824823a870["ndvi_anomalies"]:::queued
    x5130788afbe32544["modis_ndvi_transformed"]:::queued --> xf9b79e824823a870["ndvi_anomalies"]:::queued
    x8d58a79e9d066b5d(["ndvi_anomalies_AWS"]):::queued --> xf9b79e824823a870["ndvi_anomalies"]:::queued
    xe2329877730e44b5(["ndvi_anomalies_directory"]):::completed --> xf9b79e824823a870["ndvi_anomalies"]:::queued
    x44345ceb9b3d4a81(["ndvi_historical_means"]):::queued --> xf9b79e824823a870["ndvi_anomalies"]:::queued
    xa4eb23442420052a["sentinel_ndvi_transformed"]:::queued --> xf9b79e824823a870["ndvi_anomalies"]:::queued
    x5130788afbe32544["modis_ndvi_transformed"]:::queued --> x44345ceb9b3d4a81(["ndvi_historical_means"]):::queued
    x42d785b9e0106385(["ndvi_historical_means_AWS"]):::queued --> x44345ceb9b3d4a81(["ndvi_historical_means"]):::queued
    x7fef416d6ce259f3(["ndvi_historical_means_directory"]):::completed --> x44345ceb9b3d4a81(["ndvi_historical_means"]):::queued
    xa4eb23442420052a["sentinel_ndvi_transformed"]:::queued --> x44345ceb9b3d4a81(["ndvi_historical_means"]):::queued
    xcfc776190ac6b73c["modis_ndvi_bundle_request"]:::queued --> xb64343d9bc0ef12e(["modis_ndvi_requests"]):::queued
    xa5bc51cd67d5e6c0["modis_ndvi_task_id_continent"]:::queued --> xcfc776190ac6b73c["modis_ndvi_bundle_request"]:::queued
    x3f3ba2f9e89a9591(["modis_ndvi_token"]):::completed --> xcfc776190ac6b73c["modis_ndvi_bundle_request"]:::queued
    xba6244832b5285ba(["continent_raster_template"]):::queued --> x0dffb1605751d1b1(["elevation_preprocessed"]):::queued
    x98b1351d966647f6(["elevation_AWS"]):::queued --> x0dffb1605751d1b1(["elevation_preprocessed"]):::queued
    x0381132b9136146c(["elevation_directory"]):::completed --> x0dffb1605751d1b1(["elevation_preprocessed"]):::queued
    xe3c4533ec81ef618(["continent_polygon"]):::skipped --> xba6244832b5285ba(["continent_raster_template"]):::queued
    x3b5d33025a7856bb["ecmwf_forecasts_transformed"]:::queued --> x680f7450837c9229["forecasts_anomalies"]:::queued
    x259885b5bdbd7dfc(["forecasts_anomalies_AWS"]):::queued --> x680f7450837c9229["forecasts_anomalies"]:::queued
    x8ff15aa322c64802(["forecasts_anomalies_directory"]):::completed --> x680f7450837c9229["forecasts_anomalies"]:::queued
    x021b0407fd88c849(["lead_intervals"]):::completed --> x680f7450837c9229["forecasts_anomalies"]:::queued
    xc61c34839fb8c873(["model_dates_selected"]):::queued --> x680f7450837c9229["forecasts_anomalies"]:::queued
    xbd6b5d8fe3154d5a(["weather_historical_means"]):::queued --> x680f7450837c9229["forecasts_anomalies"]:::queued
    xba6244832b5285ba(["continent_raster_template"]):::queued --> x98b1351d966647f6(["elevation_AWS"]):::queued
    x0381132b9136146c(["elevation_directory"]):::completed --> x98b1351d966647f6(["elevation_AWS"]):::queued
    x5130788afbe32544["modis_ndvi_transformed"]:::queued --> xe90a7836ba709288(["modis_ndvi_transformed_AWS_upload"]):::queued
    xdc843e2504e22144(["modis_ndvi_transformed_directory"]):::skipped --> xe90a7836ba709288(["modis_ndvi_transformed_AWS_upload"]):::queued
    xba6244832b5285ba(["continent_raster_template"]):::queued --> xa4eb23442420052a["sentinel_ndvi_transformed"]:::queued
    xa9eddcdb0d1f1d02(["get_sentinel_ndvi_AWS"]):::queued --> xa4eb23442420052a["sentinel_ndvi_transformed"]:::queued
    x6e1924e349d8e6e8(["sentinel_ndvi_api_parameters"]):::completed --> xa4eb23442420052a["sentinel_ndvi_transformed"]:::queued
    x3ea733d22e9c32e7(["sentinel_ndvi_transformed_directory"]):::completed --> xa4eb23442420052a["sentinel_ndvi_transformed"]:::queued
    x30a742f54b518a5f(["augmented_data_rsa_directory"]):::completed --> x62efb3d9001cb58b(["augmented_data_rsa_AWS"]):::queued
    xc61c34839fb8c873(["model_dates_selected"]):::queued --> x62efb3d9001cb58b(["augmented_data_rsa_AWS"]):::queued
    xf9b79e824823a870["ndvi_anomalies"]:::queued --> x62efb3d9001cb58b(["augmented_data_rsa_AWS"]):::queued
    x01b9e03cb52b7b05["weather_anomalies"]:::queued --> x62efb3d9001cb58b(["augmented_data_rsa_AWS"]):::queued
    xcc8316047ac6de28(["augmented_data"]):::queued --> xcfd8f1e8f3ea9117["aggregated_data_rsa"]:::queued
    xc61c34839fb8c873(["model_dates_selected"]):::queued --> xcfd8f1e8f3ea9117["aggregated_data_rsa"]:::queued
    x97fc33c6215703a3(["rsa_polygon"]):::completed --> xcfd8f1e8f3ea9117["aggregated_data_rsa"]:::queued
    xa33032ce29b67c7f(["wahis_distance_matrix"]):::queued --> x27dbf0f2484063f3["wahis_outbreak_history"]:::queued
    x50043477563454fd(["wahis_outbreak_dates"]):::queued --> x27dbf0f2484063f3["wahis_outbreak_history"]:::queued
    x200daf9f58e96ac5(["wahis_outbreak_history_AWS"]):::queued --> x27dbf0f2484063f3["wahis_outbreak_history"]:::queued
    xcc02e30ec90a7edd(["wahis_outbreak_history_directory"]):::completed --> x27dbf0f2484063f3["wahis_outbreak_history"]:::queued
    x659aa62eded9787b(["wahis_outbreaks"]):::queued --> x27dbf0f2484063f3["wahis_outbreak_history"]:::queued
    x39ef63e4c3553f78(["wahis_raster_template"]):::queued --> x27dbf0f2484063f3["wahis_outbreak_history"]:::queued
    x2b83f10567783884(["wahis_rvf_outbreaks_preprocessed"]):::queued --> x659aa62eded9787b(["wahis_outbreaks"]):::queued
    x3b5d33025a7856bb["ecmwf_forecasts_transformed"]:::queued --> x259885b5bdbd7dfc(["forecasts_anomalies_AWS"]):::queued
    x8ff15aa322c64802(["forecasts_anomalies_directory"]):::completed --> x259885b5bdbd7dfc(["forecasts_anomalies_AWS"]):::queued
    x021b0407fd88c849(["lead_intervals"]):::completed --> x259885b5bdbd7dfc(["forecasts_anomalies_AWS"]):::queued
    xc61c34839fb8c873(["model_dates_selected"]):::queued --> x259885b5bdbd7dfc(["forecasts_anomalies_AWS"]):::queued
    xbd6b5d8fe3154d5a(["weather_historical_means"]):::queued --> x259885b5bdbd7dfc(["forecasts_anomalies_AWS"]):::queued
    x9c14f0532ee1f83c(["soil_directory"]):::completed --> x7039ba6fde7353f3(["soil_preprocessed_AWS_upload"]):::queued
    xd70b16641fa1b4ef(["soil_preprocessed"]):::queued --> x7039ba6fde7353f3(["soil_preprocessed_AWS_upload"]):::queued
    xe3c4533ec81ef618(["continent_polygon"]):::skipped --> xa5bc51cd67d5e6c0["modis_ndvi_task_id_continent"]:::queued
    x3f3ba2f9e89a9591(["modis_ndvi_token"]):::completed --> xa5bc51cd67d5e6c0["modis_ndvi_task_id_continent"]:::queued
    xdc843e2504e22144(["modis_ndvi_transformed_directory"]):::skipped --> xa5bc51cd67d5e6c0["modis_ndvi_task_id_continent"]:::queued
    xb406dc4c2762194f(["modis_task_end_dates"]):::queued --> xa5bc51cd67d5e6c0["modis_ndvi_task_id_continent"]:::queued
    x155e2f0b29a20e05(["aspect_preprocessed"]):::queued --> x482d364fd551ff53(["augmented_data_sources"]):::queued
    x1c7b6e6a1c101e59(["bioclim_preprocessed"]):::queued --> x482d364fd551ff53(["augmented_data_sources"]):::queued
    x3b5d33025a7856bb["ecmwf_forecasts_transformed"]:::queued --> x482d364fd551ff53(["augmented_data_sources"]):::queued
    x0dffb1605751d1b1(["elevation_preprocessed"]):::queued --> x482d364fd551ff53(["augmented_data_sources"]):::queued
    x680f7450837c9229["forecasts_anomalies"]:::queued --> x482d364fd551ff53(["augmented_data_sources"]):::queued
    x82990a83bfa4db45(["glw_preprocessed"]):::queued --> x482d364fd551ff53(["augmented_data_sources"]):::queued
    xdecc37cc7e708cec(["landcover_preprocessed"]):::queued --> x482d364fd551ff53(["augmented_data_sources"]):::queued
    xf9b79e824823a870["ndvi_anomalies"]:::queued --> x482d364fd551ff53(["augmented_data_sources"]):::queued
    x44345ceb9b3d4a81(["ndvi_historical_means"]):::queued --> x482d364fd551ff53(["augmented_data_sources"]):::queued
    x680370f9b58b9f6d(["slope_preprocessed"]):::queued --> x482d364fd551ff53(["augmented_data_sources"]):::queued
    xd70b16641fa1b4ef(["soil_preprocessed"]):::queued --> x482d364fd551ff53(["augmented_data_sources"]):::queued
    x27dbf0f2484063f3["wahis_outbreak_history"]:::queued --> x482d364fd551ff53(["augmented_data_sources"]):::queued
    x01b9e03cb52b7b05["weather_anomalies"]:::queued --> x482d364fd551ff53(["augmented_data_sources"]):::queued
    xbd6b5d8fe3154d5a(["weather_historical_means"]):::queued --> x482d364fd551ff53(["augmented_data_sources"]):::queued
    xb06c08f4a8f21445(["lag_intervals"]):::completed --> x8d58a79e9d066b5d(["ndvi_anomalies_AWS"]):::queued
    xc61c34839fb8c873(["model_dates_selected"]):::queued --> x8d58a79e9d066b5d(["ndvi_anomalies_AWS"]):::queued
    xe2329877730e44b5(["ndvi_anomalies_directory"]):::completed --> x8d58a79e9d066b5d(["ndvi_anomalies_AWS"]):::queued
    x44345ceb9b3d4a81(["ndvi_historical_means"]):::queued --> x8d58a79e9d066b5d(["ndvi_anomalies_AWS"]):::queued
    x3ea733d22e9c32e7(["sentinel_ndvi_transformed_directory"]):::completed --> xa9eddcdb0d1f1d02(["get_sentinel_ndvi_AWS"]):::queued
    x0548e231345702f7["nasa_weather_transformed"]:::queued --> xbd6b5d8fe3154d5a(["weather_historical_means"]):::queued
    xce069f3121e32dfb(["weather_historical_means_AWS"]):::queued --> xbd6b5d8fe3154d5a(["weather_historical_means"]):::queued
    x4407a62768444c3e(["weather_historical_means_directory"]):::completed --> xbd6b5d8fe3154d5a(["weather_historical_means"]):::queued
    xf9b79e824823a870["ndvi_anomalies"]:::queued --> x144f59a0db036a4b(["ndvi_anomalies_AWS_upload"]):::queued
    xe2329877730e44b5(["ndvi_anomalies_directory"]):::completed --> x144f59a0db036a4b(["ndvi_anomalies_AWS_upload"]):::queued
    x8894af119fe2eaa1(["landcover_directory"]):::completed --> xbc982b2f29054bd9(["landcover_preprocessed_AWS_upload"]):::queued
    xdecc37cc7e708cec(["landcover_preprocessed"]):::queued --> xbc982b2f29054bd9(["landcover_preprocessed_AWS_upload"]):::queued
    xba6244832b5285ba(["continent_raster_template"]):::queued --> x0548e231345702f7["nasa_weather_transformed"]:::queued
    x84fbc80b775022e1(["nasa_weather_AWS"]):::queued --> x0548e231345702f7["nasa_weather_transformed"]:::queued
    xc54ffbea58c4afd9(["nasa_weather_coordinates"]):::queued --> x0548e231345702f7["nasa_weather_transformed"]:::queued
    x711dc87df29f0a9c(["nasa_weather_transformed_directory"]):::completed --> x0548e231345702f7["nasa_weather_transformed"]:::queued
    x0c2748f0f39a3907(["nasa_weather_years"]):::queued --> x0548e231345702f7["nasa_weather_transformed"]:::queued
    xbd6b5d8fe3154d5a(["weather_historical_means"]):::queued --> xf8b72b30842f6a3c(["weather_historical_means_AWS_upload"]):::queued
    x4407a62768444c3e(["weather_historical_means_directory"]):::completed --> xf8b72b30842f6a3c(["weather_historical_means_AWS_upload"]):::queued
    xba6244832b5285ba(["continent_raster_template"]):::queued --> x84fbc80b775022e1(["nasa_weather_AWS"]):::queued
    xc54ffbea58c4afd9(["nasa_weather_coordinates"]):::queued --> x84fbc80b775022e1(["nasa_weather_AWS"]):::queued
    x711dc87df29f0a9c(["nasa_weather_transformed_directory"]):::completed --> x84fbc80b775022e1(["nasa_weather_AWS"]):::queued
    x0c2748f0f39a3907(["nasa_weather_years"]):::queued --> x84fbc80b775022e1(["nasa_weather_AWS"]):::queued
    xcc8316047ac6de28(["augmented_data"]):::queued --> x3130c5a4e0174582(["augmented_data_AWS_upload"]):::queued
    x2e80ddc3dafd7312(["augmented_data_directory"]):::completed --> x3130c5a4e0174582(["augmented_data_AWS_upload"]):::queued
    xe8b8ca5535fe5f2a(["bioclim_directory"]):::completed --> x1c7b6e6a1c101e59(["bioclim_preprocessed"]):::queued
    xba6244832b5285ba(["continent_raster_template"]):::queued --> x1c7b6e6a1c101e59(["bioclim_preprocessed"]):::queued
    x5448b80c3909d641(["glw_directory"]):::completed --> x01e625a4f1dd2c42(["glw_preprocessed_AWS_upload"]):::queued
    x82990a83bfa4db45(["glw_preprocessed"]):::queued --> x01e625a4f1dd2c42(["glw_preprocessed_AWS_upload"]):::queued
    xdc94d22b863438a5(["nasa_weather_variables"]):::queued --> xdc94d22b863438a5(["nasa_weather_variables"]):::queued
  end
linkStyle 0 stroke-width:0px;
```

Many of the computational steps can be time consuming and either depend
on or produce large files. In order to speed up the pipeline,
intermediate files can be stored on the cloud for rapid retrieval and
portability between pipeline instances. We currently use an AWS [S3
bucket](https://aws.amazon.com/s3/) for this purpose. The pipeline will
still run without access to cloud storage but the user can benefit from
adapt the `_targets.R` file to use their own object storage repository.
AWS access keys and bucket ID are stored in the `.env` file.

For handling gridded binary weather data, this pipeline uses the
[ecCodes](https://confluence.ecmwf.int/display/ECC) package which can be
installed on OSX using `homebrew`:

    brew install eccodes

and on Ubuntu using apt-get

    sudo apt update
    sudo apt install eccodes

EHA users: see the `stripts/` repository to be able to directly download
data from AWS outside of the targets workflow.

Follow the links for more information about:

- [`targets`](https://ecohealthalliance.github.io/eha-ma-handbook/3-projects.html#targets)
- [`renv`](https://ecohealthalliance.github.io/eha-ma-handbook/3-projects.html#package-management-with-renv)  
- [git-crypt](https://ecohealthalliance.github.io/eha-ma-handbook/16-encryption.html)
- [Reproducible
  workflows](https://github.com/ecohealthalliance/building-blocks-of-reproducibility)
