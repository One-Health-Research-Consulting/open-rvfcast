
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

``` mermaid
graph LR
subgraph Project Workflow
    direction LR
    x2afa632571070318(["continent_raster_template"]):::queued --> xc55126e36b9ee566["modis_ndvi_transformed"]:::queued
    x60c5d01495f04ee8(["modis_ndvi_downloaded_subset"]):::queued --> xc55126e36b9ee566["modis_ndvi_transformed"]:::queued
    xddbcda901f17e1d1(["modis_ndvi_transformed_directory"]):::queued --> xc55126e36b9ee566["modis_ndvi_transformed"]:::queued
    xd854377155612be3(["wahis_rvf_outbreaks_raw"]):::queued --> xeadd38ebf89ffb3e(["wahis_rvf_outbreaks_preprocessed"]):::queued
    x0ba8074843dd369a(["continent_polygon"]):::queued --> xa1d97300a3b205ca(["continent_bounding_box"]):::queued
    x4ca41505bc9c846a(["modis_ndvi_task_id_continent"]):::queued --> x2c3b1f139ba532ca(["modis_ndvi_bundle_request"]):::queued
    x916defe204c1f69c(["modis_ndvi_token"]):::queued --> x2c3b1f139ba532ca(["modis_ndvi_bundle_request"]):::queued
    xc55126e36b9ee566["modis_ndvi_transformed"]:::queued --> xc337ca407a72f903["modis_ndvi_transformed_upload_aws_s3"]:::queued
    xd52ee303d6021275(["country_polygons"]):::queued --> x39dbcca303e23588(["country_bounding_boxes"]):::queued
    x43cae5645a1256a7["nasa_weather_downloaded"]:::queued --> x77539b5dc772bdfd(["nasa_weather_pre_transformed"]):::queued
    xbcea85d356221687(["nasa_weather_pre_transformed_directory"]):::queued --> x77539b5dc772bdfd(["nasa_weather_pre_transformed"]):::queued
    xa1d97300a3b205ca(["continent_bounding_box"]):::queued --> xf00bd0e6e1fc24a0(["ecmwf_forecasts_api_parameters"]):::queued
    x5752763e075efea5["ecmwf_forecasts_transformed"]:::queued --> x49bcc7244e5eb3cd["ecmwf_forecasts_transformed_upload_aws_s3"]:::queued
    xe1a1cac3045abbc0["weather_anomalies"]:::queued --> xe9b302b56f44da20["weather_anomalies_upload_aws_s3"]:::queued
    x2afa632571070318(["continent_raster_template"]):::queued --> x4ddad172e6ddc3fa["nasa_weather_transformed"]:::queued
    x77539b5dc772bdfd(["nasa_weather_pre_transformed"]):::queued --> x4ddad172e6ddc3fa["nasa_weather_transformed"]:::queued
    xa60264a63420800d(["nasa_weather_transformed_directory"]):::queued --> x4ddad172e6ddc3fa["nasa_weather_transformed"]:::queued
    x2afa632571070318(["continent_raster_template"]):::queued --> x19b225f852b090d9["sentinel_ndvi_transformed"]:::queued
    x26dfd01796db7ec2["sentinel_ndvi_downloaded"]:::queued --> x19b225f852b090d9["sentinel_ndvi_transformed"]:::queued
    xdd7379fb675e7857(["sentinel_ndvi_transformed_directory"]):::queued --> x19b225f852b090d9["sentinel_ndvi_transformed"]:::queued
    xf00bd0e6e1fc24a0(["ecmwf_forecasts_api_parameters"]):::queued --> x9c76f3c92ea2d49d["ecmwf_forecasts_downloaded"]:::queued
    xb288b7c1b8fb2514(["ecmwf_forecasts_raw_directory"]):::queued --> x9c76f3c92ea2d49d["ecmwf_forecasts_downloaded"]:::queued
    x19b225f852b090d9["sentinel_ndvi_transformed"]:::queued --> x65d1896932a802c6["sentinel_ndvi_transformed_upload_aws_s3"]:::queued
    x2afa632571070318(["continent_raster_template"]):::queued --> x5752763e075efea5["ecmwf_forecasts_transformed"]:::queued
    x9c76f3c92ea2d49d["ecmwf_forecasts_downloaded"]:::queued --> x5752763e075efea5["ecmwf_forecasts_transformed"]:::queued
    xfb389ce5466cdf51(["ecmwf_forecasts_transformed_directory"]):::queued --> x5752763e075efea5["ecmwf_forecasts_transformed"]:::queued
    x0ba8074843dd369a(["continent_polygon"]):::queued --> x2afa632571070318(["continent_raster_template"]):::queued
    x39dbcca303e23588(["country_bounding_boxes"]):::queued --> xbe17c5133608677e(["nasa_weather_coordinates"]):::queued
    x26dfd01796db7ec2["sentinel_ndvi_downloaded"]:::queued --> xa42ed0b375131490(["sentinel_ndvi_raw_upload_aws_s3"]):::queued
    x5d90422bc28d73ea(["sentinel_ndvi_raw_directory"]):::queued --> xa42ed0b375131490(["sentinel_ndvi_raw_upload_aws_s3"]):::queued
    x43cae5645a1256a7["nasa_weather_downloaded"]:::queued --> x8d9f04c3ab33c97e(["nasa_weather_raw_upload_aws_s3"]):::queued
    x139cb26a74e33685(["nasa_weather_raw_directory"]):::queued --> x8d9f04c3ab33c97e(["nasa_weather_raw_upload_aws_s3"]):::queued
    xfe05df041c188102(["lag_intervals"]):::queued --> x7fe7e05ff9fedb6e(["model_dates"]):::queued
    xa1d97300a3b205ca(["continent_bounding_box"]):::queued --> x4ca41505bc9c846a(["modis_ndvi_task_id_continent"]):::queued
    x0a00065ff46dd97d(["modis_ndvi_end_year"]):::queued --> x4ca41505bc9c846a(["modis_ndvi_task_id_continent"]):::queued
    xc77054c0c2d4b521(["modis_ndvi_start_year"]):::queued --> x4ca41505bc9c846a(["modis_ndvi_task_id_continent"]):::queued
    x916defe204c1f69c(["modis_ndvi_token"]):::queued --> x4ca41505bc9c846a(["modis_ndvi_task_id_continent"]):::queued
    x2c3b1f139ba532ca(["modis_ndvi_bundle_request"]):::queued --> x77ec1b9f60190564["modis_ndvi_downloaded"]:::queued
    x406ecfd77a739217(["modis_ndvi_raw_directory"]):::queued --> x77ec1b9f60190564["modis_ndvi_downloaded"]:::queued
    x916defe204c1f69c(["modis_ndvi_token"]):::queued --> x77ec1b9f60190564["modis_ndvi_downloaded"]:::queued
    x77ec1b9f60190564["modis_ndvi_downloaded"]:::queued --> x9ed24944fa9bfa94(["modis_ndvi_raw_upload_aws_s3"]):::queued
    x406ecfd77a739217(["modis_ndvi_raw_directory"]):::queued --> x9ed24944fa9bfa94(["modis_ndvi_raw_upload_aws_s3"]):::queued
    x77ec1b9f60190564["modis_ndvi_downloaded"]:::queued --> x60c5d01495f04ee8(["modis_ndvi_downloaded_subset"]):::queued
    x9c76f3c92ea2d49d["ecmwf_forecasts_downloaded"]:::queued --> x7a39ddedb20d3272(["ecmwf_forecasts_raw_upload_aws_s3"]):::queued
    xb288b7c1b8fb2514(["ecmwf_forecasts_raw_directory"]):::queued --> x7a39ddedb20d3272(["ecmwf_forecasts_raw_upload_aws_s3"]):::queued
    x567c2f1aadeaa766(["days_of_year"]):::queued --> x2393c57020f85ab5["weather_historical_means"]:::queued
    x4ddad172e6ddc3fa["nasa_weather_transformed"]:::queued --> x2393c57020f85ab5["weather_historical_means"]:::queued
    xa60264a63420800d(["nasa_weather_transformed_directory"]):::queued --> x2393c57020f85ab5["weather_historical_means"]:::queued
    x1ef3b920160b49c2(["weather_historical_means_directory"]):::queued --> x2393c57020f85ab5["weather_historical_means"]:::queued
    x479365af91c7e266(["sentinel_ndvi_api_parameters"]):::queued --> x26dfd01796db7ec2["sentinel_ndvi_downloaded"]:::queued
    x5d90422bc28d73ea(["sentinel_ndvi_raw_directory"]):::queued --> x26dfd01796db7ec2["sentinel_ndvi_downloaded"]:::queued
    x7fe7e05ff9fedb6e(["model_dates"]):::queued --> x664a03317c592009(["model_dates_selected"]):::queued
    xbe17c5133608677e(["nasa_weather_coordinates"]):::queued --> x43cae5645a1256a7["nasa_weather_downloaded"]:::queued
    x139cb26a74e33685(["nasa_weather_raw_directory"]):::queued --> x43cae5645a1256a7["nasa_weather_downloaded"]:::queued
    x5f34aae31382da1e(["nasa_weather_variables"]):::queued --> x43cae5645a1256a7["nasa_weather_downloaded"]:::queued
    x910c507cd106a733(["nasa_weather_years"]):::queued --> x43cae5645a1256a7["nasa_weather_downloaded"]:::queued
    x4ddad172e6ddc3fa["nasa_weather_transformed"]:::queued --> x043ea23dffe848ec["nasa_weather_transformed_upload_aws_s3"]:::queued
    xfe05df041c188102(["lag_intervals"]):::queued --> xe1a1cac3045abbc0["weather_anomalies"]:::queued
    x7fe7e05ff9fedb6e(["model_dates"]):::queued --> xe1a1cac3045abbc0["weather_anomalies"]:::queued
    x664a03317c592009(["model_dates_selected"]):::queued --> xe1a1cac3045abbc0["weather_anomalies"]:::queued
    x4ddad172e6ddc3fa["nasa_weather_transformed"]:::queued --> xe1a1cac3045abbc0["weather_anomalies"]:::queued
    xa60264a63420800d(["nasa_weather_transformed_directory"]):::queued --> xe1a1cac3045abbc0["weather_anomalies"]:::queued
    x805693de8bc2372c(["weather_anomalies_directory"]):::queued --> xe1a1cac3045abbc0["weather_anomalies"]:::queued
    x2393c57020f85ab5["weather_historical_means"]:::queued --> xe1a1cac3045abbc0["weather_anomalies"]:::queued
  end
linkStyle 0 stroke-width:0px;
```

To run the pipeline, the user will need to adapt the `_targets.R` file
to use their own object storage repository (we use AWS) and will need to
supply keys in an `.env` file.

EHA users: see the `stripts/` repository to be able to directly download
data from AWS outside of the targets workflow.

Follow the links for more information about:

- [`targets`](https://ecohealthalliance.github.io/eha-ma-handbook/3-projects.html#targets)
- [`renv`](https://ecohealthalliance.github.io/eha-ma-handbook/3-projects.html#package-management-with-renv)  
- [git-crypt](https://ecohealthalliance.github.io/eha-ma-handbook/16-encryption.html)
- [Reproducible
  workflows](https://github.com/ecohealthalliance/building-blocks-of-reproducibility)
