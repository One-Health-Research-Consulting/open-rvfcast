
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

The goal of this ongoing `OpenRVFcast` project is the development of a
generalizable, open-source modeling framework for predicting Rift Valley
Fever (RVF) outbreaks in Africa, funded by the Wellcome Trust’s
climate-sensitive infectious disease [modeling
initiative](https://wellcome.org/news/digital-tools-climate-sensitive-infectious-disease).
We aim to integrate open data sets of climatic and vegetation data with
internationally-reported outbreak data to build an modeling pipeline
that can be adapted to varying local conditions in RVF-prone regions
across the continent.

### Pipeline Structure

In brief, the project pipeline is organized into three distinct
“modules” that join together sequentially to form the overall
OpenRVFcast pipeline: 1) the **Data Acquisition Module**, 2) the **Data
Preparation Module**, 3) and the **Modeling Framework Module**.

Some brief info on each of these modules are described below, while each
is described in detail in documentation/openRVFcast_Walkthrough.docx.

For further detail on the data and model see
documentation/model_supplement.html (created from
documentation/model_supplement.qmd).

Each module is orchestrated using the `targets` package in R, a powerful
tool for creating reproducible and efficient data analysis workflows. By
defining a workflow of interdependent tasks, known as ‘targets’, this
package ensures that each step in the workflow is only executed when its
inputs or code change, thereby optimizing computational efficiency. A
modular, scalable, and transparent design makes `targets` an ideal
choice for managing pipelines in reproducible research and production
environments. An introduction to workflow management using `targets` can
be found [here](https://books.ropensci.org/targets/).

This project also uses the [{renv}](https://rstudio.github.io/renv/)
framework to track R package dependencies and versions which are
recorded in the `renv.lock` file. Code used to manage dependencies is in
`renv/` and other files in the root project directory. On starting an R
session in the working directory, run \``renv::hydrate()` and
`renv::restore()` to install required R packags and dependencies.

### Repository Structure

Project code is available on the
[open-rvfcast](https://github.com/One-Health-Research-Consulting/open-rvfcast)
GitHub repository which is organized with the following structure:

- `data/` contains downloaded and transformed data sources. These data
  are .gitignored and are available with access to S3-compatible cloud
  storage or the raw data can be downloaded and processed.
- `R/` contains functions used in this analysis.
- `documentation/` contains documentation:
  - `openRVFcast_Walkthrough.docx` - comprehensive details on the full
    modeling pipeline
  - `dynamic_branching_guide.md` - comprehensive guide to the dynamic
    branching implementation
  - `manual.md` - some additional info for running the pipeline
- `docs/` contains shinylive app for visualizing predictions
- `outputs/` contains visualization outputs (maps, animations) and
  stores intermediate saved products created in the modeling module
- `www/` contains R shiny application for visualizing predictions and
  data needed for this application

### Data Storage

We utilized parquet files and the `arrow` package in R as our primary
method of storing data. Parquet files are optimized for
high-performance, out-of-memory data processing, making it well-suited
for efficiently handling and processing large, complex datasets.
Additionally, `arrow::open_dataset()` supports seamless integration with
S3-compatible cloud storage (including Cloudflare R2), enabling direct
access to remote datasets, which improves workflow efficiency and
scalability when working with large, distributed data sources.

#### Accessing the Complete Dataset

If run in full, the first of the modules generates approximately 500
300mb files (meant to be stored in `data/africa_full_predictor_data`).
These data can be accessed from the cloud (see .env for credentials).
However, for the purposes of monthly forecasting, the combined data file
needed for modeling can be appended with data from recent dates without
downloading this full stack. The second of the modules generates a
single ~900mb file; this file is also accessible from the cloud (BUT ATM
email Noam Ross or Morgan Kain); this file should be stored in
`data/pan_hex_joined_response_data` for the third module (modeling).
**Important**: The `africa_full_predictor_data` dataset only contains
dates for which data was successfully retrieved from **all** predictor
sources. If any predictor is missing for a given date, that date will
not be included in the final dataset. This ensures data completeness -
if a file is present in the `africa_full_predictor_data` folder, it is
guaranteed to contain all predictors for that date.

For more details on how the pipeline handles data dependencies and
incremental updates, see
[`documentation/dynamic_branching_guide.md`](documentation/dynamic_branching_guide.md).

While the data acquisition module requires the processing of large
datasets, the final cleaned data can be accessed directly from the
cloud:

    dataset <- arrow::open_dataset(arrow::s3_bucket(
      "rvfcast/data/africa_full_predictor_data/",
      endpoint_override = "https://85b2883dde6f6150134acd170f842c81.r2.cloudflarestorage.com",
      anonymous = TRUE
    ))
    dataset$schema

As parquet files are a columnar format with structured metadata
available in each file, some operations, such as filtering, summarizing,
and inspecting the data schema can be applied directly to remote
datasets without having to first download the full data. Calling
collect() on the dataset will initiate the download. For example, the
following will filter the data and then download the model data for a
single day:

    dataset <- arrow::open_dataset(arrow::s3_bucket(
      "rvfcast/data/africa_full_predictor_data/",
      endpoint_override = "https://85b2883dde6f6150134acd170f842c81.r2.cloudflarestorage.com",
      anonymous = TRUE
    )) |>
      dplyr::filter(date == "2005-01-08") |>
      dplyr::collect()

    dataset

However, due to computational demands of such large data, the model
analysis pipeline will download the data in entirety before analysis.
**Important**: The current modeling pipeline uses two randomly chosen
days per month between 2007 and 2024; however, the entire dataset is
available for every day between 2005 and the current year (though none
of the data processing has been run on this “temporally complete”
dataset).

## 1. Data Acquisition Module

This module is focused on downloading data, doing some minor cleaning,
and packaging it for further manipulation in the next module. The code
is built with a toggle that controls if data is downloaded from a bucket
owned by us or from source. If the toggle is set to download from the
bucket and it isn’t found, it defaults to re-downloading the data from
source. In most cases for model development work, the data has already
been downloaded, cleaned, and packaged and this module can be skipped.

The pipeline is designed so that for auto-updating of data for just new
dates monthly for new forecasts, only files that are needed to update
the data are downloaded (which makes forecasting not very time
intensive). However, if the goal is to not just download recent data
(i.e., to rebuild the full stack, all data can be downloaded – see
targets dates_to_process_all and dates_to_process).

Many of the computational steps in the first module can be time
consuming and either depend on or produce large files. In order to speed
up the pipeline, intermediate files can be stored in S3-compatible cloud
storage for portability. We currently use Cloudflare R2 (S3-compatible
storage). The pipeline will still run without access to cloud storage,
but users can add their own S3-compatible storage credentials to the
`.env` file to enable cloud storage and collaboration with team members.

For complete details see documentation/openRVFcast_Walkthrough.docx and
documentation/model_supplement.html (created from
documentation/model_supplement.qmd).

## 2. Data Preparation Module

In this module the raw (minorly cleaned) data saved to disk in Module 1
is further cleaned and combined into the single master tibble covariate
stack that is used in model fitting in Module 3. The key steps in this
module are combining covariates with cases, building lagged variables,
and collapsing data into w/e spatial aggregation is chosen (currently H3
hex or ADM2).

For complete details see documentation/openRVFcast_Walkthrough.docx and
documentation/model_supplement.html (created from
documentation/model_supplement.qmd).

## 3. Modeling Module

This module contains all of the modeling steps from hyperparameter
tuning through to final model fitting, prediction visualization, and
automated report generation.

For complete details see documentation/openRVFcast_Walkthrough.docx and
documentation/model_supplement.html (created from
documentation/model_supplement.qmd).

A key aspect of the modeling pipeline is our spatial and temporal
splitting strategy for cross validation (see
documentation/openRVFcast_Walkthrough.docx). Splitting data into
training, validation, and test sets is an important step for building
robust and reliable models. The training set is used to learn model
parameters, the validation set helps fine-tune hyperparameters and
prevent overfitting, and the test set provides an unbiased evaluation of
the model’s performance on unseen data. Proper splitting ensures the
model generalizes well to new data, avoiding issues like data leakage or
over-optimistic performance estimates.

### More resources

Follow the links for more information about:

- [`targets`](https://ecohealthalliance.github.io/eha-ma-handbook/3-projects.html#targets)
- [`renv`](https://ecohealthalliance.github.io/eha-ma-handbook/3-projects.html#package-management-with-renv)
- [`git-crypt`](https://ecohealthalliance.github.io/eha-ma-handbook/16-encryption.html)
- [`Reproducible workflows`](https://github.com/ecohealthalliance/building-blocks-of-reproducibility)
