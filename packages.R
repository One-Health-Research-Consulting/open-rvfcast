# crew v1.3+ switched worker launch from callr to Rscript; Rscript in batch mode
# does not source the project .Rprofile (only ~/.Rprofile or R_PROFILE_USER).
# Setting R_PROFILE_USER here propagates to crew workers via environment inheritance,
# so they source the project .Rprofile and receive the conflicts_prefer() settings.
Sys.setenv(R_PROFILE_USER = normalizePath(".Rprofile", mustWork = FALSE))

# Pin every native thread pool to a single thread in this process and in all crew
# workers (which inherit these env vars at launch, same mechanism as R_PROFILE_USER
# above). xgboost, data.table and BLAS each initialize their own OpenMP/BLAS thread
# pools; in multi-process crew workers these can intermittently segfault the worker
# (surfacing as a crew "crash"), independent of available RAM. Setting nthread = 1 on
# the xgboost engine does not stop the OpenMP runtime from initializing, so pin it
# here at the runtime level. These must be set before any thread pool initializes.
Sys.setenv(
  OMP_NUM_THREADS          = "1"
, OMP_THREAD_LIMIT         = "1"
, R_DATATABLE_NUM_THREADS  = "1"
, OPENBLAS_NUM_THREADS     = "1"
, VECLIB_MAXIMUM_THREADS   = "1"
, GOTO_NUM_THREADS         = "1"
)

library(targets)
library(tarchetypes)
# conflicted must be loaded before tidyverse so it can intercept all package conflicts at attach time
library(conflicted)
conflicts_prefer(
  dplyr::filter,
  dplyr::count,
  dplyr::select,
  magrittr::set_names,
  utils::View,
  .quiet = TRUE
)
library(tidyverse)
library(here)
library(knitr)
library(rmarkdown)
library(paws)
library(dotenv)
library(usethis)
library(kableExtra)
library(httr)
library(jsonlite)
library(urltools)
library(qs)
library(lubridate)
library(ecmwfr)
library(terra)
library(arrow)
library(visNetwork)
library(hoardr)
library(qs2)
library(sf)
library(rnaturalearth)
library(rnaturalearthhires)
library(rnaturalearthdata)
library(tidymodels)
library(recipes)
library(parsnip)
library(workflows)
library(yardstick)
library(tune)
library(vip)
library(pdp)
library(h3jsr)
library(rstan)
library(quarto)
library(mori)
library(janitor)
