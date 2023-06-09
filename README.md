
<!-- README.md is generated from README.Rmd. Please edit that file -->

# An open-source framework for Rift Valley Fever forecasting

<!-- badges: start -->

[![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)

EcoHealth Alliance's ongoing OpenRVF project is developing an open-source modeling framework for predicting Rift Valley Fever (RVF) outbreaks in Africa. The project is funded by the Wellcome Trust's climate-sensitive infectious disease modeling [initiative](https://wellcome.org/news/digital-tools-climate-sensitive-infectious-disease) and aims to integrate open data sets of climatic and vegetation data with internationally-reported outbreak data to build a pipeline for model fitting, testing, and deployment.

This repository is based on EHA's [template repository](https://github.com/ecohealthalliance/container-template) of a containerised R workflow built on the
`targets` framework, made portable using `renv`, and ran manually or
automatically using `GitHub Actions`. 

To run the pipeline, the user will need to adapt the `_targets.R` file to use their own object storage repository (we use AWS) and will need to supply keys in an `.env` file. 

Follow the links for more information about:

- [`targets`](https://ecohealthalliance.github.io/eha-ma-handbook/3-projects.html#targets)
- [`renv`](https://ecohealthalliance.github.io/eha-ma-handbook/3-projects.html#package-management-with-renv)  
- [git-crypt](https://ecohealthalliance.github.io/eha-ma-handbook/16-encryption.html)
- [Reproducible
  workflows](https://github.com/ecohealthalliance/building-blocks-of-reproducibility)
