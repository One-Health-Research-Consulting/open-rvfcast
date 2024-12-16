
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

This project is a collaborative effort between [EcoHealth
Alliance](https://www.ecohealthalliance.org/), \[INSERT PARTNER LINKS\]

### Pipeline Structure

The project pipeline is organized into two distinct modules: 1) the
**Data Acquisition Module** and 2) the **Modeling Framework Module**.
Both modules are orchestrated using the `targets` package in R, a
powerful tool for creating reproducible and efficient data analysis
workflows. By defining a workflow of interdependent tasks, known as
‘targets’, this package ensures that each step in the workflow is only
executed when its inputs or code change, thereby optimizing
computational efficiency. A modular, scalable, and transparent design
makes `targets` an ideal choice for managing pipelines in reproducible
research and production environments. An introduction to workflow
management using `targets` can be found
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
repository which is organized with the following structure:

- `data/` contains downloaded and transformed data sources. These data
  are .gitignored and are available with access to the EHA open-rvf S3
  bucket or the raw data can be download and processed.
- `R/` contains functions used in this analysis.
- `reports/` contains literate code for R Markdown reports generated in
  the analysis.
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
the cloud by opening the following connection:

    dataset <- open_dataset("s3://open-rvfcast/data/africa_full_model_data")
    dataset$schema

As parquet files are a columnar format with structured metadata
available in each file, some operations, such as filtering, summarizing,
and inspecting the data schema can be applied directly to remote
datasets without having to first download the full data. Calling
collect() on the dataset will initiate the download. For example, the
following will filter the data and then download the model data for a
single day:

    dataset <- open_dataset("s3://open-rvfcast/data/africa_full_model_data") |> 
    filter(date == "2023-12-14") |> ß
    collect()

However, due to computational demands of such large data, the model
analysis pipeline will download the data in entirety before analysis. In
addition, the dataset has been subsetted to two randomly chosen days per
month between 2007 and 2024.

## 1. Data Acquisition Module

### Cloud Storage

Many of the computational steps in the first module can be time
consuming and either depend on or produce large files. In order to speed
up the pipeline, intermediate files can be stored on the cloud for
portability. We currently use an AWS [S3
bucket](https://aws.amazon.com/s3/) for this purpose. The pipeline will
still run without access to cloud storage, but users can add their own
AWS access keys and bucket ID to the `.env` file to enable cloud
storage.

Environment variables to add to the .env file:

    AWS_DEFAULT_REGION=
    AWS_REGION=
    AWS_BUCKET_ID=
    AWS_ACCESS_KEY_ID=
    AWS_SECRET_ACCESS_KEY=

### Data Access

Acquiring the raw source data stores involves first obtaining
authentication credentials, such as API keys, tokens, and certificates.
There are three primary sources of data that require access
credentials 1. [ECMWF](https://www.ecmwf.int/): for accessing monthly
weather forecasts from the European Centre for Medium-Range Weather
Forecasts (ECMWF). 2. [COPERNICUS](https://dataspace.copernicus.eu/):
for accessing Normalized Difference Vegetation Index (NDVI) data derived
from the European Space Agency’s Sentinel-3 satellite. 3.
[APPEEARS](https://appeears.earthdatacloud.nasa.gov/api/): for accessing
historical NDVI data prior to the Sentinel-3 mission from NASA MODIS
satellites.

Before running the data acquisition pipeline, credentials for all three
sources must be added to the .env file

Environment variables to add to the .env file:

    ECMWF_USERID=
    ECMWF_TOKEN=
    COPERNICUS_USERNAME=
    COPERNICUS_PASSWORD=
    APPEEARS_USERNAME=
    APPEEARS_PASSWORD=
    APPEEARS_TOKEN=

### Data Sources

All spatial data were interpolated to a resolution of 0.1° across Africa
and standardized to the WGS 84 coordinate reference system. All temporal
data layers were joined by date.

#### The Response Variable

The goal of this project is to evaluate the potential for an outbreak of
Rift Valley fever (RVF) to occur across Africa. The model was trained
against a binary variable representing whether or not an outbreak
occurred at each spatial location 0-30 days, 30-60 days, 60-90 days,
90-120 days, and 120-150 days after every date. RVF outbreak data was
provided by the [World Animal Health Information System
(WOAH)](https://www.woah.org/en/home/) and accessed via a
[database](https://www.dolthub.com/csv/ecohealthalliance/wahisdb/main/wahis_outbreaks)
of cleaned outbreak data managed by EcoHealth Alliance.

1.  RVF_occurance: A binary factor reflecting RVF occurance at each
    location across the 5 forecast intervals.

#### Static Data

The following data sources are static, or time-invariant. Raw static
data was downloaded from the linked sources and joined with dynamic
data, such as temperature, which varied by day.

2.  [Soil
    types](https://www.fao.org/soils-portal/data-hub/soil-maps-and-databases/harmonized-world-soil-database-v20/en/):
    Soil types based on the Food and Agriculture Organization of the
    United Nations ([FAO](https://www.fao.org/home/en)) Harmonized World
    Soil Database v2.0 (HWSD) with soil types aggregated into 8
    categories: clay (heavy) + clay loam (1), silt loam + silty clay
    (2), sandy clay + clay (3), loam + silty clay loam (4), sandy clay
    loam (5), sandy loam + silt (6), loamy sand + silt loam (7), and
    sand (8) based on similarity in the USDA sand-silt-clay ternary
    texture class diagram ([Figure
    2](https://www.fao.org/soils-portal/data-hub/soil-maps-and-databases/harmonized-world-soil-database-v20/en/)).
    Data was aggregated by identifying the most common slope or aspect
    within each 0.1 degree grid cell.
3.  [Slope and Aspect](Global%20Terrain%20Slope%20and%20Aspect%20Data):
    Slope and aspect data from the FAO Global Terrain Slope and Aspect
4.  [Gridded Livestock of the World 3
    (GLW3)](https://www.nature.com/articles/sdata2018227): Global
    distribution data included
    [cattle](https://dataverse.harvard.edu/api/access/datafile/6769710),
    [sheep](https://dataverse.harvard.edu/api/access/datafile/6769629),
    and
    [goats](https://dataverse.harvard.edu/api/access/datafile/6769692)
    censused in 2010 and available at a native resolution of 5
    arc-minutes. Data was accessed via the [Harvard
    dataverse](https://dataverse.harvard.edu/).
5.  [Elevation](https://srtm.csi.cgiar.org/): Elevation data accessed
    via the `elevation_global()` function of the
    [geodata](https://rdrr.io/cran/geodata/man/elevation.html) package
    in R, drawn from the Shuttle Radar Topography Mission (SRTM) at
    resolution of 0.5 minutes of a degree.
6.  [Bioclimatic data\*](https://www.worldclim.org/data/bioclim.html):
    Bioclimactic data from the WorldClim version 2.1 accessed via the
    `worldclim_global()` function of the
    [geodata](https://rdrr.io/cran/geodata/man/worldclim.html) package
    in R and represent the global mean values across the period of
    1970-2000 at a 2.5m resolution.
7.  [Landcover
    type](https://search.r-project.org/CRAN/refmans/geodata/html/landcover.html):
    Landcover data was accessed via the `landcover()` function of
    [geodata](https://rdrr.io/cran/geodata/man/elevation.html) package
    in R, drawn from the ESA WorldCover Database with a spatial
    resolution of 30 arc-seconds. Values for each landcover type (trees,
    grassland, shrubs, cropland, built, bare, snow, water, wetland,
    mangroves, and moss), reflect the fraction of each a landcover class
    at each location.

<small>\* Bioclimactic variables included: Annual_Mean_Temperature,
Mean_Diurnal_Range, Isothermality, Temperature_Seasonality,
Max_Temperature_of_Warmest_Month, Min_Temperature_of_Coldest_Month,
Temperature_Annual_Range, Mean_Temperature_of_Wettest_Quarter,
Mean_Temperature_of_Driest_Quarter, Mean_Temperature_of_Warmest_Quarter,
Mean_Temperature_of_Coldest_Quarter, Annual_Precipitation,
Precipitation_of_Wettest_Month, Precipitation_of_Driest_Month,
Precipitation_Seasonality, Precipitation_of_Wettest_Quarter,
Precipitation_of_Driest_Quarter, Precipitation_of_Warmest_Quarter, and
Precipitation_of_Coldest_Quarter</small>

#### Dynamic Data

Dynamic data sources are those that vary with time. Dynamic predictors
can be highly conflated with each other due to a shared dependence on
time, to account for this shared dependence, we used calculated the
anomaly, or difference between current values and historical means,
instead of using the raw values. Anomalies were calculated by first
determining the difference between the current value and its historical
mean for that day-of-year (DOY) and scaled by dividing by the standard
deviation for that DOY. Focusing on anomalous values helped mitigate the
strong correlation with time that naturally exists in environmental
variables like temperature and NDVI. Seasonality was then accounted for
by including year and day-of-year (DOY) as predictors in the model. The
following sources make up the dynamic layers:

8.  [weather_anomalies](): NASA weather data was acquired across Africa
    using the `get_power()` function of the
    [nasapower](https://docs.ropensci.org/nasapower/) package in R which
    provides access to NASA meteorological data from the
    [NASAPOWER](https://power.larc.nasa.gov/) project. The difference,
    or anomaly value, was then found by subtracting each weather value
    from the average value for that day-of-year (DOY).
9.  ndvi_anomalies: NDVI data was sourced from both the NASA’s Moderate
    Resolution Imaging Spectroradiometer
    ([MODIS](https://modis.gsfc.nasa.gov/data/dataprod/mod13.php)) and
    the European Space Agency’s Copernicus
    [Sentinel-3](https://user.eumetsat.int/catalogue/EO:EUM:DAT:0340)
    missions. MODIS is due to be retired in 2025 while Sentinel-3 NDVI
    data is available from September 2018. MODIS and Sentinel-3 NDVI
    values were interpolated to a daily interval from their native 16
    day (MODIS) and \~10 day (Sentinel-3) intervals using a
    step-function and NDVI averaged when data from both sources were
    available. The difference, or anomaly value, was then found by
    subtracting NDVI from the average value for that day-of-year (DOY).

##### Weather Forecasts

10. [ecmwf_forecasts](https://cds.climate.copernicus.eu/datasets/seasonal-monthly-single-levels?tab=overview)
    We also included long-range projections of future weather provided
    by the European Centre for Medium-Range Weather Forecasts (ECMWF)
    and accessed through the [Copernicus Climate Data Store
    (CDS)](https://cds.climate.copernicus.eu/). The projected data
    represent the mean of a 51-member ensemble and include the expected
    average temperature, precipitation, and relative humidity for each
    location across different forecast intervals. Historical forecasts
    were available through hindcasts, which apply the current
    forecasting methods to historical data to simulate what forecasts
    would have been available at those times based on past conditions.

##### Lagged Dynamic Data

Outbreak occurrence is not always directly influenced by the immediately
preceding conditions. Biological systems often involve delayed
responses. For example, heavy precipitation may promote a mosquito
hatch, which can lead to an outbreak only after a delay. To account for
the influence of past environmental conditions, we included lagged
weather and NDVI data, specifically the average values from 0-30, 30-60,
60-90, 90-120, and 120-150 days prior.

11. weather_anomalies: Average weather anomaly values lagged over the
    previous 1-5 months
12. ndvi_anomalies_lagged: Average NDVI anomaly values lagged over the
    previous 1-5 months

##### Historical Outbreak Data

An important factor in evaluating the potential for a future outbreak is
the history of outbreaks in a region. Recent nearby outbreaks can
amplify the likelihood of an outbreak occurring at a given location,
while older outbreaks might reduce the risk by influencing the
resistance landscape, reflecting a history of prior exposure to the
disease.

To account for the influence of outbreak history, we generated outbreak
exposure weights for both recent and historical outbreaks. These weights
were determined using a function that decreases with distance from the
source, modeling exposure as declining exponentially outward to a
maximum distance of 500 km with an exponential rate of decay of
0.01km<sup>-1</sup>. Similarly, the effects of an outbreak were assumed
to fade over time, with influence declining as time elapsed since the
outbreak increased out to a maximum of 10 years at an exponential rate
of decay of 0.5year<sup>-1</sup>. Outbreaks that occurred within the
last 3 months were classified as ‘recent’ and included as a separate
predictor in the model allowing them to have a different effect on the
model outcome compared to the older outbreak exposures.

13. outbreak_history: Outbreak history was calculated using the data
    provided from same data described in the response section (item 1)
    above. As outbreak history contains information about the state of
    variable being predicted, special care was taken when splitting the
    data into test and training datasets to prevent data leakage
    described further below.

### Targets Pipeline

A visualization of the data acquisition module can be found below.
Additional targets not shown are responsible for fetching and storing
intermediate datasets on the cloud. To run the data acquisition module,
download the repository from github and run the following command. Note,
without access to the common S3 bucket store this pipeline will take a
significant amount of time and space to run. In addition, without access
to the remote data store, the data acquisition module must be run before
running the modeling module.

    tar_make(script = "data_acquisition_targets.R")

The schematic figure below summarizes the steps of the data acquisition
module. The figure is generated using `mermaid.js` syntax and should
display as a graph on GitHub. It can also be viewed by pasting the code
into <https://mermaid.live>.)

<!-- # ```{r, echo=FALSE, message = FALSE, results='asis'} -->
<!-- # mer <- targets::tar_mermaid(targets_only = TRUE,  -->
<!-- #                             outdated = FALSE,  -->
<!-- #                             legend = FALSE,  -->
<!-- #                             color = FALSE,  -->
<!-- #                             script = "data_acquisition_targets.R", -->
<!-- #                             exclude = c("readme", contains("AWS"))) -->
<!-- # cat( -->
<!-- #   "```mermaid", -->
<!-- #   mer[1],  -->
<!-- #   #'Objects([""Objects""]) --- Functions>""Functions""]', -->
<!-- #   'subgraph Project Workflow', -->
<!-- #   mer[3:length(mer)], -->
<!-- #   'linkStyle 0 stroke-width:0px;', -->
<!-- #   "```", -->
<!-- #   sep = "\n" -->
<!-- # ) -->
<!-- # ``` -->

## 2. Rift Valley Fever (RVF) risk model pipeline

### Data Partitioning

Splitting data into training, validation, and test sets is an important
step for building robust and reliable models. The training set is used
to learn model parameters, the validation set helps fine-tune
hyperparameters and prevent overfitting, and the test set provides an
unbiased evaluation of the model’s performance on unseen data. Proper
splitting ensures the model generalizes well to new data, avoiding
issues like data leakage or over-optimistic performance estimates.

However, splitting outbreak data can be particularly challenging due to
spatial and temporal clustering, which can lead to imbalanced or
non-representative splits. Ensuring that all three splits contain
representative data, including both outbreak presence and absence, is
critical for robust model evaluation.

#### Spatial splitting

Spatial splitting was accomplished by [spatial
blocking](https://nsojournals.onlinelibrary.wiley.com/doi/10.1111/ecog.02881)
using the spatial_block_cv() function of the
[spatialsample](https://spatialsample.tidymodels.org/) to create spatial
cross-validation folds. This ensures that each split contains distinct
spatial regions, at the level of municipality that contain representive
information in all three splits.

#### Temporal splitting

In addition to spatial clustering, outbreak data is time-series by
nature, necessitating techniques like expanding window splitting where
the training set grows incrementally over time as more data becomes
available. This approach is particularly suited for scenarios where
temporal dependencies exist, and models must be evaluated on their
ability to generalize to future, unseen data. When outbreaks are rare,
subdividing the limited positive detections can exacerbate the
imbalance, making it harder to accurately assess the model’s performance
and generalizability.

### Model Structure

### Evaluating Model Performance

### Generating Dynamic Documentation and Reports

### Targets Pipeline

A visualization of the data acquisition module can be found below.

    tar_make(script = "model_framework_targets.R")

The schematic figure below summarizes the steps of the data acquisition
module. The figure is generated using `mermaid.js` syntax and should
display as a graph on GitHub. It can also be viewed by pasting the code
into <https://mermaid.live>.)

<!-- # ```{r, echo=FALSE, message = FALSE, results='asis'} -->
<!-- # mer <- targets::tar_mermaid(targets_only = TRUE,  -->
<!-- #                             outdated = FALSE,  -->
<!-- #                             legend = FALSE,  -->
<!-- #                             color = FALSE,  -->
<!-- #                             script = "model_framework_targets.R", -->
<!-- #                             exclude = c("readme", contains("AWS"))) -->
<!-- # cat( -->
<!-- #   "```mermaid", -->
<!-- #   mer[1],  -->
<!-- #   #'Objects([""Objects""]) --- Functions>""Functions""]', -->
<!-- #   'subgraph Project Workflow', -->
<!-- #   mer[3:length(mer)], -->
<!-- #   'linkStyle 0 stroke-width:0px;', -->
<!-- #   "```", -->
<!-- #   sep = "\n" -->
<!-- # ) -->
<!-- # ``` -->

[Waywiser](https://github.com/ropensci/waywiser)

Follow the links for more information about:

- [`targets`](https://ecohealthalliance.github.io/eha-ma-handbook/3-projects.html#targets)
- [`renv`](https://ecohealthalliance.github.io/eha-ma-handbook/3-projects.html#package-management-with-renv)  
- [git-crypt](https://ecohealthalliance.github.io/eha-ma-handbook/16-encryption.html)
- [Reproducible
  workflows](https://github.com/ecohealthalliance/building-blocks-of-reproducibility)
