
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

# Overview of OpenRVFcast

The goal of EcoHealth Alliance’s ongoing OpenRVFcast project the
development of a generalizable, open-source modeling framework for
predicting Rift Valley Fever (RVF) outbreaks in Africa, funded by the
Wellcome Trust’s climate-sensitive infectious disease [modeling
initiative](https://wellcome.org/news/digital-tools-climate-sensitive-infectious-disease).
We aim to integrate open data sets of climatic and vegetation data with
internationally-reported outbreak data to build an modeling pipeline
that can be adapted to varying local conditions in RVF-prone regions
across the continent.

### Pipeline Structure

The project pipeline is organized into two distinct modules: 1) Data
Acquisition, and 2) the Modeling Framework Module. Both modules are orchestrated using the
`targets` package in R, a powerful tool for creating reproducible and
efficient data analysis workflows. By defining a workflow of
interdependent tasks, known as ‘targets’, this package ensures that each
step in the workflow is only executed when its inputs or code change,
thereby optimizing computational efficiency. A modular, scalable, and
transparent design makes `targets` an ideal choice for managing
pipelines in reproducible research and production environments. An
introduction to workflow management using `targets` can be found
[here](https://books.ropensci.org/targets/). This project also uses the
[{renv}](https://rstudio.github.io/renv/) framework to track R package
dependencies and versions which are recorded in the `renv.lock` file.
Code used to manage dependencies is in `renv/` and other files in the
root project directory. On starting an R session in the working
directory, run \``renv::hydrate()` and `renv::restore()` to install
required R packags and dependencies.

### Repository Structure

Project code is available on the
[open-rvfcast](https://github.com/ecohealthalliance/open-rvfcast) GitHub
repository

- `data/` contains downloaded and transformed data sources. These data
  are .gitignored and are available with access to the EHA open-rvf S3
  bucket or the raw data can be download and processed.
- `R/` contains functions used in this analysis.
- `reports/` contains literate code for R Markdown reports generated in
  the analysis
- `outputs/` contains compiled reports and figures.

### Data Storage

We utilized parquet files and the `arrow` package in R as our primary
method of storing data. Parquet files are optimized for
high-performance, out-of-memory data processing, making it well-suited
for efficiently handling and processing large, complex datasets.
Additionally, `arrow::open_dataset()` supports seamless integration with
cloud storage, enabling direct access to remote datasets, which improves
workflow efficiency and scalability when working with large, distributed
data sources. While the data acquisition module requires the processing
of large datasets, the final cleaned data can be accessed directly from
the cloud simply via:

    dataset <- open_dataset("s3://open-rvfcast/data/explanatory_variables")

Because parquet files are a columnar format with structured metadata
available in each file, some operations, such as filtering, can be
applied directly to remote datasets without having to first download the
full data. The following will only download the model data for a single
day:

    dataset <- open_dataset("s3://open-rvfcast/data/explanatory_variables") |> filter(date == "2023-12-14") |> collect()

Due to the large nature of the data not every day is available - the
dataset has been subsetted to two randomly chosen days per month between
2007 and 2024.

## 1. Data Acquisition Module

### Cloud Storage

Many of the computational steps in the first module can be time
consuming and either depend on or produce large files. In order to speed
up the pipeline, intermediate files can be stored on the cloud for rapid
retrieval and portability between pipeline instances. We currently use
an AWS [S3 bucket](https://aws.amazon.com/s3/) for this purpose. The
pipeline will still run without access to cloud storage but the user can
benefit from adapt the `_targets.R` file to use their own object storage
repository. AWS access keys and bucket ID are stored in the `.env` file.

### Data Access

Gaining access to the source data stores involves obtaining
authentication credentials, such as API keys, tokens, and certificates,
to ensure secure communication and data transfer. There are three
primary sources of data that require access credentials 1.
[ECMWF](https://www.ecmwf.int/): for accessing monthly weather forecasts
from the European Centre for Medium-Range Weather Forecasts (ECMWF). 2.
[COPERNICUS](https://dataspace.copernicus.eu/): for accessing Normalized
Difference Vegetation Index (NDVI) data derived from the European Space
Agency’s Sentinel-3 satellite. 3.
[APPEEARS](https://appeears.earthdatacloud.nasa.gov/api/): for accessing
historical NDVI data prior to the Sentinel-3 mission from NASA MODIS
satellites.

### Data Sources

#### Static Data

The following data sources are static, or time-invariant. Raw static
data was downloaded from the linked sources and joined with dynamic
data, such as temperature, which varied by day.

1.  [Soil types]():
2.  [Aspect]():
3.  [Slope]():
4.  [Gridded Livestock of the World (GLW)]():
5.  [Elevation]():
6.  [Bioclimatic data]():
7.  [Landcover type]():

#### Dynamic Data

Dynamic data sources are those that vary with time. The following
sources make up the dynamic layers

1.  [weather_anomalies]()
2.  [forecasts_anomalies]()
3.  [ndvi_anomalies]()
4.  [wahis_outbreak_history]()

#### Temporal Covaraince

In order to isolate the influence of each dynamic predictor, which can
be highly conflated with each other due to a shared dependence on time
and long-term trends, we used the difference between current values and
historical means instead of raw values for dynamic layers. This approach
helped mitigate the strong correlation with time that naturally exists
in environmental variables like temperature and NDVI. Seasonality was
then accounted for by including year and day-of-year (DOY) as predictors
in the model.

#### Forecast Dynamic Data

#### Lagged Dynamic Data

#### Historical Outbreak Data

### Targets Pipeline

A visualization of the data acquisition module can be found below.
Additional targets not shown are responsible for fetching and storing
intermediate datasets on the cloud. To run the data acquisition module,
download the repository from github and run the following command. Note,
without access to the common S3 bucket store this pipeline will take a
significant amount of time and space to run. In addition, without access
to the remote data store, the data acquisition module must be run before
running the modeling module.

    tar_make(store = "data_aquisition_targets.R")

The schematic figure below summarizes the steps of the data acquisition
module. The figure is generated using `mermaid.js` syntax and should
display as a graph on GitHub. It can also be viewed by pasting the code
into <https://mermaid.live>.)

Warning messages: 1: package ‘targets’ was built under R version 4.3.3
2: package ‘rmarkdown’ was built under R version 4.3.3 3: package ‘paws’
was built under R version 4.3.3 4: package ‘terra’ was built under R
version 4.3.3 5: package ‘arrow’ was built under R version 4.3.3

``` mermaid
graph LR
subgraph Project Workflow
  subgraph Graph
    direction LR
    x2b83f10567783884(["wahis_rvf_outbreaks_preprocessed"]):::queued --> x50043477563454fd(["wahis_outbreak_dates"]):::queued
    x0548e231345702f7["nasa_weather_transformed"]:::queued --> xddb5620937cdbc01(["nasa_weather_transformed_AWS_upload"]):::queued
    x711dc87df29f0a9c(["nasa_weather_transformed_directory"]):::queued --> xddb5620937cdbc01(["nasa_weather_transformed_AWS_upload"]):::queued
    xc61c34839fb8c873(["model_dates_selected"]):::skipped --> x0df1395319c2f010(["weather_anomalies_AWS"]):::queued
    x0548e231345702f7["nasa_weather_transformed"]:::queued --> x0df1395319c2f010(["weather_anomalies_AWS"]):::queued
    xf94f7486eed9869c(["weather_anomalies_directory"]):::queued --> x0df1395319c2f010(["weather_anomalies_AWS"]):::queued
    xbd6b5d8fe3154d5a(["weather_historical_means"]):::queued --> x0df1395319c2f010(["weather_anomalies_AWS"]):::queued
    xba6244832b5285ba(["continent_raster_template"]):::skipped --> x3b5d33025a7856bb["ecmwf_forecasts_transformed"]:::queued
    x73599238bfebd1c5(["ecmwf_forecasts_api_parameters"]):::queued --> x3b5d33025a7856bb["ecmwf_forecasts_transformed"]:::queued
    x16ce463b7b647c1e(["ecmwf_forecasts_transformed_directory"]):::queued --> x3b5d33025a7856bb["ecmwf_forecasts_transformed"]:::queued
    x32725338020380f8(["get_ecmwf_forecasts_AWS"]):::queued --> x3b5d33025a7856bb["ecmwf_forecasts_transformed"]:::queued
    xba6244832b5285ba(["continent_raster_template"]):::skipped --> xf36f13d6d1345340(["modis_ndvi_transformed_AWS"]):::completed
    xcfc776190ac6b73c["modis_ndvi_bundle_request"]:::skipped --> xf36f13d6d1345340(["modis_ndvi_transformed_AWS"]):::completed
    x3f3ba2f9e89a9591(["modis_ndvi_token"]):::completed --> xf36f13d6d1345340(["modis_ndvi_transformed_AWS"]):::completed
    xdc843e2504e22144(["modis_ndvi_transformed_directory"]):::skipped --> xf36f13d6d1345340(["modis_ndvi_transformed_AWS"]):::completed
    x0548e231345702f7["nasa_weather_transformed"]:::queued --> xce069f3121e32dfb(["weather_historical_means_AWS"]):::queued
    x4407a62768444c3e(["weather_historical_means_directory"]):::queued --> xce069f3121e32dfb(["weather_historical_means_AWS"]):::queued
    xb06c08f4a8f21445(["lag_intervals"]):::completed --> x04eda626a40d7d5e["nasa_weather_transformed_lagged"]:::queued
    xc61c34839fb8c873(["model_dates_selected"]):::skipped --> x04eda626a40d7d5e["nasa_weather_transformed_lagged"]:::queued
    x0548e231345702f7["nasa_weather_transformed"]:::queued --> x04eda626a40d7d5e["nasa_weather_transformed_lagged"]:::queued
    x13112224557e242b(["nasa_weather_transformed_lagged_AWS"]):::queued --> x04eda626a40d7d5e["nasa_weather_transformed_lagged"]:::queued
    xd1a19d6808243286(["nasa_weather_transformed_lagged_directory"]):::queued --> x04eda626a40d7d5e["nasa_weather_transformed_lagged"]:::queued
    xba6244832b5285ba(["continent_raster_template"]):::skipped --> xdecc37cc7e708cec(["landcover_preprocessed"]):::queued
    x955e49f3e0c22510(["landcover_AWS"]):::queued --> xdecc37cc7e708cec(["landcover_preprocessed"]):::queued
    x8894af119fe2eaa1(["landcover_directory"]):::queued --> xdecc37cc7e708cec(["landcover_preprocessed"]):::queued
    x684d7fe78b0e841d(["landcover_types"]):::queued --> xdecc37cc7e708cec(["landcover_preprocessed"]):::queued
    x8c316985096325c6["ndvi_anomalies_lagged"]:::dispatched --> x1b6e4d2730b719df(["ndvi_anomalies_lagged_AWS_upload"]):::queued
    x4f0fdf9b3ab89593(["ndvi_anomalies_lagged_directory"]):::skipped --> x1b6e4d2730b719df(["ndvi_anomalies_lagged_AWS_upload"]):::queued
    xba6244832b5285ba(["continent_raster_template"]):::skipped --> x82990a83bfa4db45(["glw_preprocessed"]):::queued
    x71c93c84792ad529(["glw_AWS"]):::queued --> x82990a83bfa4db45(["glw_preprocessed"]):::queued
    x5448b80c3909d641(["glw_directory"]):::queued --> x82990a83bfa4db45(["glw_preprocessed"]):::queued
    x4d4a15b2f0f1851f(["glw_urls"]):::queued --> x82990a83bfa4db45(["glw_preprocessed"]):::queued
    xbd6b5d8fe3154d5a(["weather_historical_means"]):::queued --> xf8b72b30842f6a3c(["weather_historical_means_AWS_upload"]):::queued
    x4407a62768444c3e(["weather_historical_means_directory"]):::queued --> xf8b72b30842f6a3c(["weather_historical_means_AWS_upload"]):::queued
    xc61c34839fb8c873(["model_dates_selected"]):::skipped --> x13112224557e242b(["nasa_weather_transformed_lagged_AWS"]):::queued
    xd1a19d6808243286(["nasa_weather_transformed_lagged_directory"]):::queued --> x13112224557e242b(["nasa_weather_transformed_lagged_AWS"]):::queued
    x3b5d33025a7856bb["ecmwf_forecasts_transformed"]:::queued --> x259885b5bdbd7dfc(["forecasts_anomalies_AWS"]):::queued
    x8ff15aa322c64802(["forecasts_anomalies_directory"]):::queued --> x259885b5bdbd7dfc(["forecasts_anomalies_AWS"]):::queued
    xc61c34839fb8c873(["model_dates_selected"]):::skipped --> x259885b5bdbd7dfc(["forecasts_anomalies_AWS"]):::queued
    xbd6b5d8fe3154d5a(["weather_historical_means"]):::queued --> x259885b5bdbd7dfc(["forecasts_anomalies_AWS"]):::queued
    xef7dbc04c9db3001(["africa_full_model_data_directory"]):::queued --> x25bf0fd7a1b4bba3(["africa_full_model_data_AWS"]):::queued
    x3b5d33025a7856bb["ecmwf_forecasts_transformed"]:::queued --> xe017ffc3bafa162a(["ecmwf_forecasts_transformed_AWS_upload"]):::queued
    x16ce463b7b647c1e(["ecmwf_forecasts_transformed_directory"]):::queued --> xe017ffc3bafa162a(["ecmwf_forecasts_transformed_AWS_upload"]):::queued
    xc61c34839fb8c873(["model_dates_selected"]):::skipped --> x01b9e03cb52b7b05["weather_anomalies"]:::queued
    x0548e231345702f7["nasa_weather_transformed"]:::queued --> x01b9e03cb52b7b05["weather_anomalies"]:::queued
    x0df1395319c2f010(["weather_anomalies_AWS"]):::queued --> x01b9e03cb52b7b05["weather_anomalies"]:::queued
    xf94f7486eed9869c(["weather_anomalies_directory"]):::queued --> x01b9e03cb52b7b05["weather_anomalies"]:::queued
    xbd6b5d8fe3154d5a(["weather_historical_means"]):::queued --> x01b9e03cb52b7b05["weather_anomalies"]:::queued
    xc61c34839fb8c873(["model_dates_selected"]):::skipped --> xf9b79e824823a870["ndvi_anomalies"]:::skipped
    x8d58a79e9d066b5d(["ndvi_anomalies_AWS"]):::completed --> xf9b79e824823a870["ndvi_anomalies"]:::skipped
    xe2329877730e44b5(["ndvi_anomalies_directory"]):::skipped --> xf9b79e824823a870["ndvi_anomalies"]:::skipped
    x44345ceb9b3d4a81(["ndvi_historical_means"]):::skipped --> xf9b79e824823a870["ndvi_anomalies"]:::skipped
    xb8d88361e3190fbf(["ndvi_transformed"]):::skipped --> xf9b79e824823a870["ndvi_anomalies"]:::skipped
    xe3c4533ec81ef618(["continent_polygon"]):::skipped --> xa5bc51cd67d5e6c0["modis_ndvi_task_id_continent"]:::completed
    x3f3ba2f9e89a9591(["modis_ndvi_token"]):::completed --> xa5bc51cd67d5e6c0["modis_ndvi_task_id_continent"]:::completed
    xdc843e2504e22144(["modis_ndvi_transformed_directory"]):::skipped --> xa5bc51cd67d5e6c0["modis_ndvi_task_id_continent"]:::completed
    xb406dc4c2762194f(["modis_task_end_dates"]):::skipped --> xa5bc51cd67d5e6c0["modis_ndvi_task_id_continent"]:::completed
    xe3c4533ec81ef618(["continent_polygon"]):::skipped --> xba6244832b5285ba(["continent_raster_template"]):::skipped
    x3b5d33025a7856bb["ecmwf_forecasts_transformed"]:::queued --> x680f7450837c9229["forecasts_anomalies"]:::queued
    xdac479b8154aa4e0(["forecast_intervals"]):::queued --> x680f7450837c9229["forecasts_anomalies"]:::queued
    x259885b5bdbd7dfc(["forecasts_anomalies_AWS"]):::queued --> x680f7450837c9229["forecasts_anomalies"]:::queued
    x8ff15aa322c64802(["forecasts_anomalies_directory"]):::queued --> x680f7450837c9229["forecasts_anomalies"]:::queued
    xc61c34839fb8c873(["model_dates_selected"]):::skipped --> x680f7450837c9229["forecasts_anomalies"]:::queued
    xbd6b5d8fe3154d5a(["weather_historical_means"]):::queued --> x680f7450837c9229["forecasts_anomalies"]:::queued
    xba6244832b5285ba(["continent_raster_template"]):::skipped --> xa4eb23442420052a["sentinel_ndvi_transformed"]:::skipped
    xa9eddcdb0d1f1d02(["get_sentinel_ndvi_AWS"]):::completed --> xa4eb23442420052a["sentinel_ndvi_transformed"]:::skipped
    x6e1924e349d8e6e8(["sentinel_ndvi_api_parameters"]):::skipped --> xa4eb23442420052a["sentinel_ndvi_transformed"]:::skipped
    x45b75d590706329f(["sentinel_ndvi_token_file"]):::completed --> xa4eb23442420052a["sentinel_ndvi_transformed"]:::skipped
    x3ea733d22e9c32e7(["sentinel_ndvi_transformed_directory"]):::skipped --> xa4eb23442420052a["sentinel_ndvi_transformed"]:::skipped
    xa33032ce29b67c7f(["wahis_distance_matrix"]):::queued --> x27dbf0f2484063f3["wahis_outbreak_history"]:::queued
    x50043477563454fd(["wahis_outbreak_dates"]):::queued --> x27dbf0f2484063f3["wahis_outbreak_history"]:::queued
    x200daf9f58e96ac5(["wahis_outbreak_history_AWS"]):::queued --> x27dbf0f2484063f3["wahis_outbreak_history"]:::queued
    xcc02e30ec90a7edd(["wahis_outbreak_history_directory"]):::queued --> x27dbf0f2484063f3["wahis_outbreak_history"]:::queued
    x659aa62eded9787b(["wahis_outbreaks"]):::queued --> x27dbf0f2484063f3["wahis_outbreak_history"]:::queued
    x39ef63e4c3553f78(["wahis_raster_template"]):::queued --> x27dbf0f2484063f3["wahis_outbreak_history"]:::queued
    x42a5375a64b48216(["aspect_directory"]):::queued --> xfe5a910dc093a019(["aspect_preprocessed_AWS_upload"]):::queued
    x155e2f0b29a20e05(["aspect_preprocessed"]):::queued --> xfe5a910dc093a019(["aspect_preprocessed_AWS_upload"]):::queued
    xc61c34839fb8c873(["model_dates_selected"]):::skipped --> xfac7ed895dc2f9a1(["ndvi_anomalies_lagged_AWS"]):::completed
    xf9b79e824823a870["ndvi_anomalies"]:::skipped --> xfac7ed895dc2f9a1(["ndvi_anomalies_lagged_AWS"]):::completed
    x4f0fdf9b3ab89593(["ndvi_anomalies_lagged_directory"]):::skipped --> xfac7ed895dc2f9a1(["ndvi_anomalies_lagged_AWS"]):::completed
    xba6244832b5285ba(["continent_raster_template"]):::skipped --> x84fbc80b775022e1(["nasa_weather_AWS"]):::queued
    xc54ffbea58c4afd9(["nasa_weather_coordinates"]):::queued --> x84fbc80b775022e1(["nasa_weather_AWS"]):::queued
    x711dc87df29f0a9c(["nasa_weather_transformed_directory"]):::queued --> x84fbc80b775022e1(["nasa_weather_AWS"]):::queued
    x0c2748f0f39a3907(["nasa_weather_years"]):::queued --> x84fbc80b775022e1(["nasa_weather_AWS"]):::queued
    x53c4b2fb80542353(["country_bounding_boxes"]):::queued --> xc54ffbea58c4afd9(["nasa_weather_coordinates"]):::queued
    xa33032ce29b67c7f(["wahis_distance_matrix"]):::queued --> x200daf9f58e96ac5(["wahis_outbreak_history_AWS"]):::queued
    x50043477563454fd(["wahis_outbreak_dates"]):::queued --> x200daf9f58e96ac5(["wahis_outbreak_history_AWS"]):::queued
    xcc02e30ec90a7edd(["wahis_outbreak_history_directory"]):::queued --> x200daf9f58e96ac5(["wahis_outbreak_history_AWS"]):::queued
    x659aa62eded9787b(["wahis_outbreaks"]):::queued --> x200daf9f58e96ac5(["wahis_outbreak_history_AWS"]):::queued
    x39ef63e4c3553f78(["wahis_raster_template"]):::queued --> x200daf9f58e96ac5(["wahis_outbreak_history_AWS"]):::queued
    xba6244832b5285ba(["continent_raster_template"]):::skipped --> x0548e231345702f7["nasa_weather_transformed"]:::queued
    x84fbc80b775022e1(["nasa_weather_AWS"]):::queued --> x0548e231345702f7["nasa_weather_transformed"]:::queued
    xc54ffbea58c4afd9(["nasa_weather_coordinates"]):::queued --> x0548e231345702f7["nasa_weather_transformed"]:::queued
    x711dc87df29f0a9c(["nasa_weather_transformed_directory"]):::queued --> x0548e231345702f7["nasa_weather_transformed"]:::queued
    x0c2748f0f39a3907(["nasa_weather_years"]):::queued --> x0548e231345702f7["nasa_weather_transformed"]:::queued
    x42a5375a64b48216(["aspect_directory"]):::queued --> x890a8fc59a28f6b2(["slope_AWS"]):::queued
    xba6244832b5285ba(["continent_raster_template"]):::skipped --> x890a8fc59a28f6b2(["slope_AWS"]):::queued
    xe8b8ca5535fe5f2a(["bioclim_directory"]):::queued --> xe4be5b46895c0f8c(["bioclim_preprocessed_AWS_upload"]):::queued
    x1c7b6e6a1c101e59(["bioclim_preprocessed"]):::queued --> xe4be5b46895c0f8c(["bioclim_preprocessed_AWS_upload"]):::queued
    xa4eb23442420052a["sentinel_ndvi_transformed"]:::skipped --> x6db823df8cb78984(["sentinel_ndvi_transformed_AWS_upload"]):::queued
    x3ea733d22e9c32e7(["sentinel_ndvi_transformed_directory"]):::skipped --> x6db823df8cb78984(["sentinel_ndvi_transformed_AWS_upload"]):::queued
    xba6244832b5285ba(["continent_raster_template"]):::skipped --> x0dffb1605751d1b1(["elevation_preprocessed"]):::queued
    x98b1351d966647f6(["elevation_AWS"]):::queued --> x0dffb1605751d1b1(["elevation_preprocessed"]):::queued
    x0381132b9136146c(["elevation_directory"]):::queued --> x0dffb1605751d1b1(["elevation_preprocessed"]):::queued
    xba6244832b5285ba(["continent_raster_template"]):::skipped --> x98b1351d966647f6(["elevation_AWS"]):::queued
    x0381132b9136146c(["elevation_directory"]):::queued --> x98b1351d966647f6(["elevation_AWS"]):::queued
    xb406dc4c2762194f(["modis_task_end_dates"]):::skipped --> x5173ee721c44ebc0(["ndvi_years"]):::skipped
    xba6244832b5285ba(["continent_raster_template"]):::skipped --> xd70b16641fa1b4ef(["soil_preprocessed"]):::queued
    x4110b36142c7dd5b(["soil_AWS"]):::queued --> xd70b16641fa1b4ef(["soil_preprocessed"]):::queued
    x9c14f0532ee1f83c(["soil_directory"]):::queued --> xd70b16641fa1b4ef(["soil_preprocessed"]):::queued
    x049b29595ee19108(["aspect_AWS"]):::queued --> x155e2f0b29a20e05(["aspect_preprocessed"]):::queued
    x42a5375a64b48216(["aspect_directory"]):::queued --> x155e2f0b29a20e05(["aspect_preprocessed"]):::queued
    x213d1d2657d00cd0(["aspect_urls"]):::queued --> x155e2f0b29a20e05(["aspect_preprocessed"]):::queued
    xba6244832b5285ba(["continent_raster_template"]):::skipped --> x155e2f0b29a20e05(["aspect_preprocessed"]):::queued
    xba6244832b5285ba(["continent_raster_template"]):::skipped --> x71c93c84792ad529(["glw_AWS"]):::queued
    x5448b80c3909d641(["glw_directory"]):::queued --> x71c93c84792ad529(["glw_AWS"]):::queued
    x8894af119fe2eaa1(["landcover_directory"]):::queued --> xbc982b2f29054bd9(["landcover_preprocessed_AWS_upload"]):::queued
    xdecc37cc7e708cec(["landcover_preprocessed"]):::queued --> xbc982b2f29054bd9(["landcover_preprocessed_AWS_upload"]):::queued
    x165085d61327782d(["slope_directory"]):::queued --> x5aa9efa15ecd03d0(["slope_preprocessed_AWS_upload"]):::queued
    x680370f9b58b9f6d(["slope_preprocessed"]):::queued --> x5aa9efa15ecd03d0(["slope_preprocessed_AWS_upload"]):::queued
    x0548e231345702f7["nasa_weather_transformed"]:::queued --> xbd6b5d8fe3154d5a(["weather_historical_means"]):::queued
    xce069f3121e32dfb(["weather_historical_means_AWS"]):::queued --> xbd6b5d8fe3154d5a(["weather_historical_means"]):::queued
    x4407a62768444c3e(["weather_historical_means_directory"]):::queued --> xbd6b5d8fe3154d5a(["weather_historical_means"]):::queued
    x9c9060069417a49a(["wahis_rvf_outbreaks_raw"]):::queued --> x2b83f10567783884(["wahis_rvf_outbreaks_preprocessed"]):::queued
    x3ea733d22e9c32e7(["sentinel_ndvi_transformed_directory"]):::skipped --> xa9eddcdb0d1f1d02(["get_sentinel_ndvi_AWS"]):::completed
    xa5bc51cd67d5e6c0["modis_ndvi_task_id_continent"]:::completed --> xcfc776190ac6b73c["modis_ndvi_bundle_request"]:::skipped
    x3f3ba2f9e89a9591(["modis_ndvi_token"]):::completed --> xcfc776190ac6b73c["modis_ndvi_bundle_request"]:::skipped
    x680f7450837c9229["forecasts_anomalies"]:::queued --> x72d065c3b2ed1267(["forecasts_anomalies_AWS_upload"]):::queued
    x8ff15aa322c64802(["forecasts_anomalies_directory"]):::queued --> x72d065c3b2ed1267(["forecasts_anomalies_AWS_upload"]):::queued
    x5130788afbe32544["modis_ndvi_transformed"]:::skipped --> xe90a7836ba709288(["modis_ndvi_transformed_AWS_upload"]):::queued
    xdc843e2504e22144(["modis_ndvi_transformed_directory"]):::skipped --> xe90a7836ba709288(["modis_ndvi_transformed_AWS_upload"]):::queued
    xef7dbc04c9db3001(["africa_full_model_data_directory"]):::queued --> x1c52768a2bb44b28["africa_full_model_data"]:::queued
    xc61c34839fb8c873(["model_dates_selected"]):::skipped --> x1c52768a2bb44b28["africa_full_model_data"]:::queued
    x2b83f10567783884(["wahis_rvf_outbreaks_preprocessed"]):::queued --> x659aa62eded9787b(["wahis_outbreaks"]):::queued
    xb8d88361e3190fbf(["ndvi_transformed"]):::skipped --> xe6a0cdc82e337d14(["ndvi_transformed_AWS_upload"]):::queued
    x704a24502f5bfcb5(["ndvi_transformed_directory"]):::skipped --> xe6a0cdc82e337d14(["ndvi_transformed_AWS_upload"]):::queued
    x0381132b9136146c(["elevation_directory"]):::queued --> xd9f5e6274aef515b(["elevation_preprocessed_AWS_upload"]):::queued
    x0dffb1605751d1b1(["elevation_preprocessed"]):::queued --> xd9f5e6274aef515b(["elevation_preprocessed_AWS_upload"]):::queued
    x04eda626a40d7d5e["nasa_weather_transformed_lagged"]:::queued --> xe47d75e1e93db64a(["nasa_weather_transformed_lagged_AWS_upload"]):::queued
    xd1a19d6808243286(["nasa_weather_transformed_lagged_directory"]):::queued --> xe47d75e1e93db64a(["nasa_weather_transformed_lagged_AWS_upload"]):::queued
    x9c14f0532ee1f83c(["soil_directory"]):::queued --> x7039ba6fde7353f3(["soil_preprocessed_AWS_upload"]):::queued
    xd70b16641fa1b4ef(["soil_preprocessed"]):::queued --> x7039ba6fde7353f3(["soil_preprocessed_AWS_upload"]):::queued
    xb06c08f4a8f21445(["lag_intervals"]):::completed --> x8c316985096325c6["ndvi_anomalies_lagged"]:::dispatched
    xc61c34839fb8c873(["model_dates_selected"]):::skipped --> x8c316985096325c6["ndvi_anomalies_lagged"]:::dispatched
    xf9b79e824823a870["ndvi_anomalies"]:::skipped --> x8c316985096325c6["ndvi_anomalies_lagged"]:::dispatched
    xfac7ed895dc2f9a1(["ndvi_anomalies_lagged_AWS"]):::completed --> x8c316985096325c6["ndvi_anomalies_lagged"]:::dispatched
    x4f0fdf9b3ab89593(["ndvi_anomalies_lagged_directory"]):::skipped --> x8c316985096325c6["ndvi_anomalies_lagged"]:::dispatched
    x44345ceb9b3d4a81(["ndvi_historical_means"]):::skipped --> x1be60916d37ebe0f(["ndvi_historical_means_AWS_upload"]):::queued
    x7fef416d6ce259f3(["ndvi_historical_means_directory"]):::skipped --> x1be60916d37ebe0f(["ndvi_historical_means_AWS_upload"]):::queued
    x659aa62eded9787b(["wahis_outbreaks"]):::queued --> xa33032ce29b67c7f(["wahis_distance_matrix"]):::queued
    x39ef63e4c3553f78(["wahis_raster_template"]):::queued --> xa33032ce29b67c7f(["wahis_distance_matrix"]):::queued
    x155e2f0b29a20e05(["aspect_preprocessed"]):::queued --> xa51aa171acb7e103(["africa_full_model_data_sources"]):::queued
    x1c7b6e6a1c101e59(["bioclim_preprocessed"]):::queued --> xa51aa171acb7e103(["africa_full_model_data_sources"]):::queued
    x0dffb1605751d1b1(["elevation_preprocessed"]):::queued --> xa51aa171acb7e103(["africa_full_model_data_sources"]):::queued
    x680f7450837c9229["forecasts_anomalies"]:::queued --> xa51aa171acb7e103(["africa_full_model_data_sources"]):::queued
    x82990a83bfa4db45(["glw_preprocessed"]):::queued --> xa51aa171acb7e103(["africa_full_model_data_sources"]):::queued
    xdecc37cc7e708cec(["landcover_preprocessed"]):::queued --> xa51aa171acb7e103(["africa_full_model_data_sources"]):::queued
    xf9b79e824823a870["ndvi_anomalies"]:::skipped --> xa51aa171acb7e103(["africa_full_model_data_sources"]):::queued
    x8c316985096325c6["ndvi_anomalies_lagged"]:::dispatched --> xa51aa171acb7e103(["africa_full_model_data_sources"]):::queued
    x680370f9b58b9f6d(["slope_preprocessed"]):::queued --> xa51aa171acb7e103(["africa_full_model_data_sources"]):::queued
    xd70b16641fa1b4ef(["soil_preprocessed"]):::queued --> xa51aa171acb7e103(["africa_full_model_data_sources"]):::queued
    x27dbf0f2484063f3["wahis_outbreak_history"]:::queued --> xa51aa171acb7e103(["africa_full_model_data_sources"]):::queued
    x01b9e03cb52b7b05["weather_anomalies"]:::queued --> xa51aa171acb7e103(["africa_full_model_data_sources"]):::queued
    xe8b8ca5535fe5f2a(["bioclim_directory"]):::queued --> x1c7b6e6a1c101e59(["bioclim_preprocessed"]):::queued
    xba6244832b5285ba(["continent_raster_template"]):::skipped --> x1c7b6e6a1c101e59(["bioclim_preprocessed"]):::queued
    xc61c34839fb8c873(["model_dates_selected"]):::skipped --> x8d58a79e9d066b5d(["ndvi_anomalies_AWS"]):::completed
    xe2329877730e44b5(["ndvi_anomalies_directory"]):::skipped --> x8d58a79e9d066b5d(["ndvi_anomalies_AWS"]):::completed
    x44345ceb9b3d4a81(["ndvi_historical_means"]):::skipped --> x8d58a79e9d066b5d(["ndvi_anomalies_AWS"]):::completed
    xba6244832b5285ba(["continent_raster_template"]):::skipped --> x32725338020380f8(["get_ecmwf_forecasts_AWS"]):::queued
    x73599238bfebd1c5(["ecmwf_forecasts_api_parameters"]):::queued --> x32725338020380f8(["get_ecmwf_forecasts_AWS"]):::queued
    x16ce463b7b647c1e(["ecmwf_forecasts_transformed_directory"]):::queued --> x32725338020380f8(["get_ecmwf_forecasts_AWS"]):::queued
    xba6244832b5285ba(["continent_raster_template"]):::skipped --> x680370f9b58b9f6d(["slope_preprocessed"]):::queued
    x890a8fc59a28f6b2(["slope_AWS"]):::queued --> x680370f9b58b9f6d(["slope_preprocessed"]):::queued
    x165085d61327782d(["slope_directory"]):::queued --> x680370f9b58b9f6d(["slope_preprocessed"]):::queued
    x1ef0d1881ff89dbd(["slope_urls"]):::queued --> x680370f9b58b9f6d(["slope_preprocessed"]):::queued
    xc61c34839fb8c873(["model_dates_selected"]):::skipped --> xd8a73eba91d443f8(["ndvi_transformed_AWS"]):::queued
    x5130788afbe32544["modis_ndvi_transformed"]:::skipped --> xd8a73eba91d443f8(["ndvi_transformed_AWS"]):::queued
    x704a24502f5bfcb5(["ndvi_transformed_directory"]):::skipped --> xd8a73eba91d443f8(["ndvi_transformed_AWS"]):::queued
    xa4eb23442420052a["sentinel_ndvi_transformed"]:::skipped --> xd8a73eba91d443f8(["ndvi_transformed_AWS"]):::queued
    xba6244832b5285ba(["continent_raster_template"]):::skipped --> x73599238bfebd1c5(["ecmwf_forecasts_api_parameters"]):::queued
    x8d531cfe4886deda(["ecmwf_lead_months"]):::queued --> x73599238bfebd1c5(["ecmwf_forecasts_api_parameters"]):::queued
    xe3c4533ec81ef618(["continent_polygon"]):::skipped --> x39ef63e4c3553f78(["wahis_raster_template"]):::queued
    xba6244832b5285ba(["continent_raster_template"]):::skipped --> x955e49f3e0c22510(["landcover_AWS"]):::queued
    x8894af119fe2eaa1(["landcover_directory"]):::queued --> x955e49f3e0c22510(["landcover_AWS"]):::queued
    x5130788afbe32544["modis_ndvi_transformed"]:::skipped --> xb8d88361e3190fbf(["ndvi_transformed"]):::skipped
    x704a24502f5bfcb5(["ndvi_transformed_directory"]):::skipped --> xb8d88361e3190fbf(["ndvi_transformed"]):::skipped
    x5173ee721c44ebc0(["ndvi_years"]):::skipped --> xb8d88361e3190fbf(["ndvi_transformed"]):::skipped
    xa4eb23442420052a["sentinel_ndvi_transformed"]:::skipped --> xb8d88361e3190fbf(["ndvi_transformed"]):::skipped
    xe8b8ca5535fe5f2a(["bioclim_directory"]):::queued --> xe2930fde1049416f(["bioclim_AWS"]):::queued
    xba6244832b5285ba(["continent_raster_template"]):::skipped --> xe2930fde1049416f(["bioclim_AWS"]):::queued
    x1c52768a2bb44b28["africa_full_model_data"]:::queued --> xea3d157b452d0e43(["africa_full_model_data_AWS_upload"]):::queued
    xef7dbc04c9db3001(["africa_full_model_data_directory"]):::queued --> xea3d157b452d0e43(["africa_full_model_data_AWS_upload"]):::queued
    x6bae1f342f811d0b(["wahis_rvf_controls_raw"]):::queued --> x2668bdb7843be979(["wahis_rvf_controls_preprocessed"]):::queued
    x5448b80c3909d641(["glw_directory"]):::queued --> x01e625a4f1dd2c42(["glw_preprocessed_AWS_upload"]):::queued
    x82990a83bfa4db45(["glw_preprocessed"]):::queued --> x01e625a4f1dd2c42(["glw_preprocessed_AWS_upload"]):::queued
    x27dbf0f2484063f3["wahis_outbreak_history"]:::queued --> x53890e32519c2cdc(["wahis_outbreak_history_AWS_upload"]):::queued
    x338ce62055c4090f(["wahis_outbreak_history_animations_directory"]):::queued --> x53890e32519c2cdc(["wahis_outbreak_history_AWS_upload"]):::queued
    xba6244832b5285ba(["continent_raster_template"]):::skipped --> x5130788afbe32544["modis_ndvi_transformed"]:::skipped
    xb64343d9bc0ef12e(["modis_ndvi_requests"]):::skipped --> x5130788afbe32544["modis_ndvi_transformed"]:::skipped
    x3f3ba2f9e89a9591(["modis_ndvi_token"]):::completed --> x5130788afbe32544["modis_ndvi_transformed"]:::skipped
    xf36f13d6d1345340(["modis_ndvi_transformed_AWS"]):::completed --> x5130788afbe32544["modis_ndvi_transformed"]:::skipped
    xdc843e2504e22144(["modis_ndvi_transformed_directory"]):::skipped --> x5130788afbe32544["modis_ndvi_transformed"]:::skipped
    x42a5375a64b48216(["aspect_directory"]):::queued --> x049b29595ee19108(["aspect_AWS"]):::queued
    xba6244832b5285ba(["continent_raster_template"]):::skipped --> x049b29595ee19108(["aspect_AWS"]):::queued
    xcfc776190ac6b73c["modis_ndvi_bundle_request"]:::skipped --> xb64343d9bc0ef12e(["modis_ndvi_requests"]):::skipped
    xf9b79e824823a870["ndvi_anomalies"]:::skipped --> x144f59a0db036a4b(["ndvi_anomalies_AWS_upload"]):::queued
    xe2329877730e44b5(["ndvi_anomalies_directory"]):::skipped --> x144f59a0db036a4b(["ndvi_anomalies_AWS_upload"]):::queued
    x4847fdb918188b25(["country_polygons"]):::queued --> x53c4b2fb80542353(["country_bounding_boxes"]):::queued
    x5130788afbe32544["modis_ndvi_transformed"]:::skipped --> x42d785b9e0106385(["ndvi_historical_means_AWS"]):::completed
    x7fef416d6ce259f3(["ndvi_historical_means_directory"]):::skipped --> x42d785b9e0106385(["ndvi_historical_means_AWS"]):::completed
    xa4eb23442420052a["sentinel_ndvi_transformed"]:::skipped --> x42d785b9e0106385(["ndvi_historical_means_AWS"]):::completed
    x01b9e03cb52b7b05["weather_anomalies"]:::queued --> xc567f473073bf453(["weather_anomalies_AWS_upload"]):::queued
    xf94f7486eed9869c(["weather_anomalies_directory"]):::queued --> xc567f473073bf453(["weather_anomalies_AWS_upload"]):::queued
    xba6244832b5285ba(["continent_raster_template"]):::skipped --> x4110b36142c7dd5b(["soil_AWS"]):::queued
    x9c14f0532ee1f83c(["soil_directory"]):::queued --> x4110b36142c7dd5b(["soil_AWS"]):::queued
    x5130788afbe32544["modis_ndvi_transformed"]:::skipped --> x44345ceb9b3d4a81(["ndvi_historical_means"]):::skipped
    x42d785b9e0106385(["ndvi_historical_means_AWS"]):::completed --> x44345ceb9b3d4a81(["ndvi_historical_means"]):::skipped
    x7fef416d6ce259f3(["ndvi_historical_means_directory"]):::skipped --> x44345ceb9b3d4a81(["ndvi_historical_means"]):::skipped
    xa4eb23442420052a["sentinel_ndvi_transformed"]:::skipped --> x44345ceb9b3d4a81(["ndvi_historical_means"]):::skipped
    x30a742f54b518a5f(["augmented_data_rsa_directory"]):::queued --> x30a742f54b518a5f(["augmented_data_rsa_directory"]):::queued
    xdc94d22b863438a5(["nasa_weather_variables"]):::queued --> xdc94d22b863438a5(["nasa_weather_variables"]):::queued
    x97fc33c6215703a3(["rsa_polygon"]):::queued --> x97fc33c6215703a3(["rsa_polygon"]):::queued
  end
linkStyle 0 stroke-width:0px;
```

## 2. Modeling Framework Module

Follow the links for more information about:

- [`targets`](https://ecohealthalliance.github.io/eha-ma-handbook/3-projects.html#targets)
- [`renv`](https://ecohealthalliance.github.io/eha-ma-handbook/3-projects.html#package-management-with-renv)  
- [git-crypt](https://ecohealthalliance.github.io/eha-ma-handbook/16-encryption.html)
- [Reproducible
  workflows](https://github.com/ecohealthalliance/building-blocks-of-reproducibility)
