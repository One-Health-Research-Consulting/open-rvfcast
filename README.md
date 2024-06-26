
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

- The project is out-of-sync – use `renv::status()` for details.

``` mermaid
graph LR
subgraph Project Workflow
  subgraph Graph
    direction LR
    x83714660cbce9386(["model_data"]):::queued --> x61133f3a5d97a935(["rolling_n"]):::queued
    xcc8316047ac6de28(["augmented_data"]):::queued --> xcfd8f1e8f3ea9117["aggregated_data_rsa"]:::queued
    xc61c34839fb8c873(["model_dates_selected"]):::queued --> xcfd8f1e8f3ea9117["aggregated_data_rsa"]:::queued
    x97fc33c6215703a3(["rsa_polygon"]):::completed --> xcfd8f1e8f3ea9117["aggregated_data_rsa"]:::queued
    xcfc776190ac6b73c(["modis_ndvi_bundle_request"]):::queued --> x38fbe682c153055a["modis_ndvi_downloaded"]:::queued
    x4654083e75e14da7(["modis_ndvi_raw_directory"]):::completed --> x38fbe682c153055a["modis_ndvi_downloaded"]:::queued
    x3f3ba2f9e89a9591(["modis_ndvi_token"]):::skipped --> x38fbe682c153055a["modis_ndvi_downloaded"]:::queued
    xc54ffbea58c4afd9(["nasa_weather_coordinates"]):::queued --> xee6df18dd4394b6d["nasa_weather_downloaded"]:::queued
    x82934fd0342127f1(["nasa_weather_raw_directory"]):::completed --> xee6df18dd4394b6d["nasa_weather_downloaded"]:::queued
    xdc94d22b863438a5(["nasa_weather_variables"]):::completed --> xee6df18dd4394b6d["nasa_weather_downloaded"]:::queued
    x0c2748f0f39a3907(["nasa_weather_years"]):::completed --> xee6df18dd4394b6d["nasa_weather_downloaded"]:::queued
    xba6244832b5285ba(["continent_raster_template"]):::queued --> xa4eb23442420052a["sentinel_ndvi_transformed"]:::queued
    xdf6af9d980a9adcc["sentinel_ndvi_downloaded"]:::queued --> xa4eb23442420052a["sentinel_ndvi_transformed"]:::queued
    x3ea733d22e9c32e7(["sentinel_ndvi_transformed_directory"]):::completed --> xa4eb23442420052a["sentinel_ndvi_transformed"]:::queued
    x5130788afbe32544["modis_ndvi_transformed"]:::queued --> x7953d879f95da493["modis_ndvi_transformed_upload_aws_s3"]:::queued
    xba6244832b5285ba(["continent_raster_template"]):::queued --> x3b5d33025a7856bb["ecmwf_forecasts_transformed"]:::queued
    x704b33b3c6c3260c["ecmwf_forecasts_downloaded"]:::queued --> x3b5d33025a7856bb["ecmwf_forecasts_transformed"]:::queued
    x16ce463b7b647c1e(["ecmwf_forecasts_transformed_directory"]):::completed --> x3b5d33025a7856bb["ecmwf_forecasts_transformed"]:::queued
    xeb29aee01eadfdee(["rec"]):::queued --> xb405ca1344048da4(["rec_juiced"]):::queued
    x9c9060069417a49a(["wahis_rvf_outbreaks_raw"]):::completed --> x2b83f10567783884(["wahis_rvf_outbreaks_preprocessed"]):::queued
    x6bae1f342f811d0b(["wahis_rvf_controls_raw"]):::completed --> x2668bdb7843be979(["wahis_rvf_controls_preprocessed"]):::queued
    xf01ad9529437fc85(["grid"]):::queued --> x864ebfd4dc9e834b(["tuned"]):::queued
    xf94084fe2fb21595(["splits"]):::queued --> x864ebfd4dc9e834b(["tuned"]):::queued
    x78508f838d845e21(["wf"]):::queued --> x864ebfd4dc9e834b(["tuned"]):::queued
    x01b9e03cb52b7b05["weather_anomalies"]:::queued --> xb0713c55749f1489["weather_anomalies_upload_aws_s3"]:::queued
    xee6df18dd4394b6d["nasa_weather_downloaded"]:::queued --> xbe36c3f119b633d4(["nasa_weather_pre_transformed"]):::queued
    x8371e9beef39aa7f(["nasa_weather_pre_transformed_directory"]):::completed --> xbe36c3f119b633d4(["nasa_weather_pre_transformed"]):::queued
    x83714660cbce9386(["model_data"]):::queued --> x6f9c2b7a946d40a2(["model_data_split"]):::queued
    x2278d345b92b60b8(["split_prop"]):::queued --> x6f9c2b7a946d40a2(["model_data_split"]):::queued
    x5130788afbe32544["modis_ndvi_transformed"]:::queued --> x92b237aaa434cba4(["ndvi_date_lookup"]):::queued
    xdc843e2504e22144(["modis_ndvi_transformed_directory"]):::completed --> x92b237aaa434cba4(["ndvi_date_lookup"]):::queued
    xa4eb23442420052a["sentinel_ndvi_transformed"]:::queued --> x92b237aaa434cba4(["ndvi_date_lookup"]):::queued
    x3ea733d22e9c32e7(["sentinel_ndvi_transformed_directory"]):::completed --> x92b237aaa434cba4(["ndvi_date_lookup"]):::queued
    x704b33b3c6c3260c["ecmwf_forecasts_downloaded"]:::queued --> xba23a761d341369c(["ecmwf_forecasts_raw_upload_aws_s3"]):::queued
    x36db65bcd3aa9f83(["ecmwf_forecasts_raw_directory"]):::completed --> xba23a761d341369c(["ecmwf_forecasts_raw_upload_aws_s3"]):::queued
    xa72a356ab8b0f2e4["forecasts_anomalies_validate"]:::queued --> x9f1762adbbb894ba["forecasts_anomalies_validate_upload_aws_s3"]:::queued
    xb49d77ffc5b097ae(["continent_bounding_box"]):::queued --> xa5bc51cd67d5e6c0(["modis_ndvi_task_id_continent"]):::queued
    xd0a560cea3a0849b(["modis_ndvi_end_year"]):::completed --> xa5bc51cd67d5e6c0(["modis_ndvi_task_id_continent"]):::queued
    xebaacae55fa09931(["modis_ndvi_start_year"]):::completed --> xa5bc51cd67d5e6c0(["modis_ndvi_task_id_continent"]):::queued
    x3f3ba2f9e89a9591(["modis_ndvi_token"]):::skipped --> xa5bc51cd67d5e6c0(["modis_ndvi_task_id_continent"]):::queued
    xe3c4533ec81ef618(["continent_polygon"]):::completed --> xb49d77ffc5b097ae(["continent_bounding_box"]):::queued
    xe3c4533ec81ef618(["continent_polygon"]):::completed --> xba6244832b5285ba(["continent_raster_template"]):::queued
    x0e0af3fadb8c4b95(["base_score"]):::queued --> xc8979a9fdd830ea4(["spec"]):::queued
    xf071aa3ac248a06c(["interaction_constraints"]):::completed --> xc8979a9fdd830ea4(["spec"]):::queued
    x5762fd8bafb8be90(["monotone_constraints"]):::completed --> xc8979a9fdd830ea4(["spec"]):::queued
    x0548e231345702f7["nasa_weather_transformed"]:::queued --> xeb40be29de55c659["nasa_weather_transformed_upload_aws_s3"]:::queued
    x1f222a4448edddc4(["days_of_year"]):::completed --> x44345ceb9b3d4a81["ndvi_historical_means"]:::queued
    xb06c08f4a8f21445(["lag_intervals"]):::completed --> x44345ceb9b3d4a81["ndvi_historical_means"]:::queued
    x92b237aaa434cba4(["ndvi_date_lookup"]):::queued --> x44345ceb9b3d4a81["ndvi_historical_means"]:::queued
    x7fef416d6ce259f3(["ndvi_historical_means_directory"]):::completed --> x44345ceb9b3d4a81["ndvi_historical_means"]:::queued
    xa5bc51cd67d5e6c0(["modis_ndvi_task_id_continent"]):::queued --> xcfc776190ac6b73c(["modis_ndvi_bundle_request"]):::queued
    x3f3ba2f9e89a9591(["modis_ndvi_token"]):::skipped --> xcfc776190ac6b73c(["modis_ndvi_bundle_request"]):::queued
    x3fac2193a6071940(["training_data"]):::queued --> xeb29aee01eadfdee(["rec"]):::queued
    x83714660cbce9386(["model_data"]):::queued --> x2278d345b92b60b8(["split_prop"]):::queued
    xba6244832b5285ba(["continent_raster_template"]):::queued --> x0548e231345702f7["nasa_weather_transformed"]:::queued
    xbe36c3f119b633d4(["nasa_weather_pre_transformed"]):::queued --> x0548e231345702f7["nasa_weather_transformed"]:::queued
    x711dc87df29f0a9c(["nasa_weather_transformed_directory"]):::completed --> x0548e231345702f7["nasa_weather_transformed"]:::queued
    x680f7450837c9229["forecasts_anomalies"]:::queued --> xa72a356ab8b0f2e4["forecasts_anomalies_validate"]:::queued
    x309ea01959a83a5a(["forecasts_validate_directory"]):::completed --> xa72a356ab8b0f2e4["forecasts_anomalies_validate"]:::queued
    x021b0407fd88c849(["lead_intervals"]):::completed --> xa72a356ab8b0f2e4["forecasts_anomalies_validate"]:::queued
    xc61c34839fb8c873(["model_dates_selected"]):::queued --> xa72a356ab8b0f2e4["forecasts_anomalies_validate"]:::queued
    x0548e231345702f7["nasa_weather_transformed"]:::queued --> xa72a356ab8b0f2e4["forecasts_anomalies_validate"]:::queued
    xbd6b5d8fe3154d5a["weather_historical_means"]:::queued --> xa72a356ab8b0f2e4["forecasts_anomalies_validate"]:::queued
    x38fbe682c153055a["modis_ndvi_downloaded"]:::queued --> x466609b51f5cc265(["modis_ndvi_raw_upload_aws_s3"]):::queued
    x4654083e75e14da7(["modis_ndvi_raw_directory"]):::completed --> x466609b51f5cc265(["modis_ndvi_raw_upload_aws_s3"]):::queued
    x3fac2193a6071940(["training_data"]):::queued --> xf01ad9529437fc85(["grid"]):::queued
    xee6df18dd4394b6d["nasa_weather_downloaded"]:::queued --> x90d78478b2c330b4(["nasa_weather_raw_upload_aws_s3"]):::queued
    x82934fd0342127f1(["nasa_weather_raw_directory"]):::completed --> x90d78478b2c330b4(["nasa_weather_raw_upload_aws_s3"]):::queued
    x73599238bfebd1c5(["ecmwf_forecasts_api_parameters"]):::queued --> x704b33b3c6c3260c["ecmwf_forecasts_downloaded"]:::queued
    x36db65bcd3aa9f83(["ecmwf_forecasts_raw_directory"]):::completed --> x704b33b3c6c3260c["ecmwf_forecasts_downloaded"]:::queued
    xb06c08f4a8f21445(["lag_intervals"]):::completed --> xf9b79e824823a870["ndvi_anomalies"]:::queued
    xc61c34839fb8c873(["model_dates_selected"]):::queued --> xf9b79e824823a870["ndvi_anomalies"]:::queued
    xe2329877730e44b5(["ndvi_anomalies_directory"]):::completed --> xf9b79e824823a870["ndvi_anomalies"]:::queued
    x92b237aaa434cba4(["ndvi_date_lookup"]):::queued --> xf9b79e824823a870["ndvi_anomalies"]:::queued
    x44345ceb9b3d4a81["ndvi_historical_means"]:::queued --> xf9b79e824823a870["ndvi_anomalies"]:::queued
    x44345ceb9b3d4a81["ndvi_historical_means"]:::queued --> x2f1bdfda2bc25995["ndvi_historical_means_upload_aws_s3"]:::queued
    x4847fdb918188b25(["country_polygons"]):::completed --> x53c4b2fb80542353(["country_bounding_boxes"]):::queued
    x3b5d33025a7856bb["ecmwf_forecasts_transformed"]:::queued --> x680f7450837c9229["forecasts_anomalies"]:::queued
    x16ce463b7b647c1e(["ecmwf_forecasts_transformed_directory"]):::completed --> x680f7450837c9229["forecasts_anomalies"]:::queued
    x8ff15aa322c64802(["forecasts_anomalies_directory"]):::completed --> x680f7450837c9229["forecasts_anomalies"]:::queued
    x021b0407fd88c849(["lead_intervals"]):::completed --> x680f7450837c9229["forecasts_anomalies"]:::queued
    xc61c34839fb8c873(["model_dates_selected"]):::queued --> x680f7450837c9229["forecasts_anomalies"]:::queued
    xbd6b5d8fe3154d5a["weather_historical_means"]:::queued --> x680f7450837c9229["forecasts_anomalies"]:::queued
    x6e1924e349d8e6e8(["sentinel_ndvi_api_parameters"]):::completed --> xdf6af9d980a9adcc["sentinel_ndvi_downloaded"]:::queued
    x6ec1e9466f1e39de(["sentinel_ndvi_raw_directory"]):::completed --> xdf6af9d980a9adcc["sentinel_ndvi_downloaded"]:::queued
    xdf6af9d980a9adcc["sentinel_ndvi_downloaded"]:::queued --> x18dd83d28c2fff6a(["sentinel_ndvi_raw_upload_aws_s3"]):::queued
    x6ec1e9466f1e39de(["sentinel_ndvi_raw_directory"]):::completed --> x18dd83d28c2fff6a(["sentinel_ndvi_raw_upload_aws_s3"]):::queued
    xbd6b5d8fe3154d5a["weather_historical_means"]:::queued --> x7b0349be57c93e06["weather_historical_means_upload_aws_s3"]:::queued
    x1f222a4448edddc4(["days_of_year"]):::completed --> xbd6b5d8fe3154d5a["weather_historical_means"]:::queued
    xb06c08f4a8f21445(["lag_intervals"]):::completed --> xbd6b5d8fe3154d5a["weather_historical_means"]:::queued
    x021b0407fd88c849(["lead_intervals"]):::completed --> xbd6b5d8fe3154d5a["weather_historical_means"]:::queued
    x0548e231345702f7["nasa_weather_transformed"]:::queued --> xbd6b5d8fe3154d5a["weather_historical_means"]:::queued
    x711dc87df29f0a9c(["nasa_weather_transformed_directory"]):::completed --> xbd6b5d8fe3154d5a["weather_historical_means"]:::queued
    x4407a62768444c3e(["weather_historical_means_directory"]):::completed --> xbd6b5d8fe3154d5a["weather_historical_means"]:::queued
    xb49d77ffc5b097ae(["continent_bounding_box"]):::queued --> x73599238bfebd1c5(["ecmwf_forecasts_api_parameters"]):::queued
    x680f7450837c9229["forecasts_anomalies"]:::queued --> x16a8c65820c9852a["forecasts_anomalies_upload_aws_s3"]:::queued
    xb06c08f4a8f21445(["lag_intervals"]):::completed --> xc61c34839fb8c873(["model_dates_selected"]):::queued
    x38fbe682c153055a["modis_ndvi_downloaded"]:::queued --> x6ed0b56027606605(["modis_ndvi_downloaded_subset"]):::queued
    xeb29aee01eadfdee(["rec"]):::queued --> x78508f838d845e21(["wf"]):::queued
    xc8979a9fdd830ea4(["spec"]):::queued --> x78508f838d845e21(["wf"]):::queued
    x3fac2193a6071940(["training_data"]):::queued --> x0e0af3fadb8c4b95(["base_score"]):::queued
    x6f9c2b7a946d40a2(["model_data_split"]):::queued --> xcc759d34fbe5ec35(["holdout_data"]):::queued
    x2e80ddc3dafd7312(["augmented_data_directory"]):::completed --> xcc8316047ac6de28(["augmented_data"]):::queued
    x680f7450837c9229["forecasts_anomalies"]:::queued --> xcc8316047ac6de28(["augmented_data"]):::queued
    xf9b79e824823a870["ndvi_anomalies"]:::queued --> xcc8316047ac6de28(["augmented_data"]):::queued
    x01b9e03cb52b7b05["weather_anomalies"]:::queued --> xcc8316047ac6de28(["augmented_data"]):::queued
    xba6244832b5285ba(["continent_raster_template"]):::queued --> x5130788afbe32544["modis_ndvi_transformed"]:::queued
    x6ed0b56027606605(["modis_ndvi_downloaded_subset"]):::queued --> x5130788afbe32544["modis_ndvi_transformed"]:::queued
    xdc843e2504e22144(["modis_ndvi_transformed_directory"]):::completed --> x5130788afbe32544["modis_ndvi_transformed"]:::queued
    x3b5d33025a7856bb["ecmwf_forecasts_transformed"]:::queued --> x72337b688b89b9b6["ecmwf_forecasts_transformed_upload_aws_s3"]:::queued
    x6f9c2b7a946d40a2(["model_data_split"]):::queued --> x3fac2193a6071940(["training_data"]):::queued
    xf9b79e824823a870["ndvi_anomalies"]:::queued --> xdfd31ada1a752471["ndvi_anomalies_upload_aws_s3"]:::queued
    xc61c34839fb8c873(["model_dates_selected"]):::queued --> x68c68eb425eb5254(["rvf_outbreaks"]):::queued
    x97fc33c6215703a3(["rsa_polygon"]):::completed --> x68c68eb425eb5254(["rvf_outbreaks"]):::queued
    x2b83f10567783884(["wahis_rvf_outbreaks_preprocessed"]):::queued --> x68c68eb425eb5254(["rvf_outbreaks"]):::queued
    x53c4b2fb80542353(["country_bounding_boxes"]):::queued --> xc54ffbea58c4afd9(["nasa_weather_coordinates"]):::queued
    x97fc33c6215703a3(["rsa_polygon"]):::completed --> x8367f94bdb991b08(["rsa_polygon_spatial_weights"]):::queued
    xcc8316047ac6de28(["augmented_data"]):::queued --> x7cd8d791b750c70d(["augmented_data_upload_aws_s3"]):::queued
    xb06c08f4a8f21445(["lag_intervals"]):::completed --> x01b9e03cb52b7b05["weather_anomalies"]:::queued
    xc61c34839fb8c873(["model_dates_selected"]):::queued --> x01b9e03cb52b7b05["weather_anomalies"]:::queued
    x0548e231345702f7["nasa_weather_transformed"]:::queued --> x01b9e03cb52b7b05["weather_anomalies"]:::queued
    x711dc87df29f0a9c(["nasa_weather_transformed_directory"]):::completed --> x01b9e03cb52b7b05["weather_anomalies"]:::queued
    xf94f7486eed9869c(["weather_anomalies_directory"]):::queued --> x01b9e03cb52b7b05["weather_anomalies"]:::queued
    xbd6b5d8fe3154d5a["weather_historical_means"]:::queued --> x01b9e03cb52b7b05["weather_anomalies"]:::queued
    xa4eb23442420052a["sentinel_ndvi_transformed"]:::queued --> xef2f237c350aec34["sentinel_ndvi_transformed_upload_aws_s3"]:::queued
    x61133f3a5d97a935(["rolling_n"]):::queued --> xf94084fe2fb21595(["splits"]):::queued
    x3fac2193a6071940(["training_data"]):::queued --> xf94084fe2fb21595(["splits"]):::queued
    xcfd8f1e8f3ea9117["aggregated_data_rsa"]:::queued --> x83714660cbce9386(["model_data"]):::queued
    x8367f94bdb991b08(["rsa_polygon_spatial_weights"]):::queued --> x83714660cbce9386(["model_data"]):::queued
    x68c68eb425eb5254(["rvf_outbreaks"]):::queued --> x83714660cbce9386(["model_data"]):::queued
    x30a742f54b518a5f(["augmented_data_rsa_directory"]):::completed --> x30a742f54b518a5f(["augmented_data_rsa_directory"]):::completed
    xc11069275cfeb620(["readme"]):::dispatched --> xc11069275cfeb620(["readme"]):::dispatched
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
