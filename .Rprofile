for (env_file in list.files(all.files = TRUE, pattern = "^\\.env.*")) {
  try(readRenviron(env_file), silent = TRUE)
}


# Put the project library *outside* the project
#Sys.setenv(RENV_PATHS_LIBRARY_ROOT = file.path(normalizePath("~/.renv-project-libraries", mustWork = FALSE)))

if(Sys.getenv("USE_CAPSULE") %in% c("1", "TRUE", "true")) {
  if (interactive() && file.exists("renv.lock")) {
    message("renv library not loaded (found env var USE_CAPSULE=", Sys.getenv("USE_CAPSULE"), "). Use `capsule` functions (see https://github.com/MilesMcBain/capsule)")
    if(require(capsule, quietly = TRUE)) {
    capsule::whinge()
    } else {
      message('Install {capsule} with install.packages("capsule", repos = c(mm = "https://milesmcbain.r-universe.dev", getOption("repos")))')
    }
  }
} else {
  source("renv/activate.R")
}


# Use the local user's .Rprofile when interactive.
# Good for keeping local preferences, but not always reproducible.
user_rprof <- Sys.getenv("R_PROFILE_USER", normalizePath("~/.Rprofile", mustWork = FALSE))
if(interactive() && file.exists(user_rprof)) {
  source(user_rprof)
}

options(
  repos = c(RSPM = "https://packagemanager.rstudio.com/all/latest",
            CRAN = "https://cran.rstudio.com/",
            INLA = "https://inla.r-inla-download.org/R/testing"),
  
  renv.config.auto.snapshot = TRUE, ## Attempt to keep renv.lock updated automatically
  renv.config.rspm.enabled = TRUE, ## Use RStudio Package manager for pre-built package binaries
  renv.config.install.shortcuts = TRUE, ## Use the existing local library to fetch copies of packages for renv
  renv.config.cache.enabled = TRUE,   ## Use the renv build cache to speed up install times
  renv.config.cache.symlinks = TRUE,  ## Keep full copies of packages locally than symlinks to make the project portable in/out of containers
  renv.config.install.transactional = FALSE
  
)

# Set maximum allowed total size (in bytes) of global variables for future package. Used to prevent too large exports.
if (Sys.info()[["nodename"]] %in% c("aegypti-reservoir" , "prospero-reservoir")) {
  options(
    future.globals.maxSize = 4194304000
  )
}

# Since RSPM does not provide Mac binaries, always install packages from CRAN
# on mac or windows, even if renv.lock specifies they came from RSPM
if (Sys.info()[["sysname"]] %in% c("Darwin", "Windows")) {
  options(renv.config.repos.override = c(
    CRAN = "https://cran.rstudio.com/",
    INLA = "https://inla.r-inla-download.org/R/testing"))
} else if (Sys.info()[["sysname"]] == "Linux") {
  options(renv.config.repos.override = c(
    RSPM = "https://packagemanager.rstudio.com/all/latest",
    INLA = "https://inla.r-inla-download.org/R/testing"))
}

# If project packages have conflicts define them here
if(requireNamespace("conflicted", quietly = TRUE)) {
  conflicted::conflict_prefer("filter", "dplyr", quiet = TRUE)
  conflicted::conflict_prefer("count", "dplyr", quiet = TRUE)
  conflicted::conflict_prefer("select", "dplyr", quiet = TRUE)
  conflicted::conflict_prefer("geom_rug", "ggplot2", quiet = TRUE)
  conflicted::conflict_prefer("set_names", "magrittr", quiet = TRUE)
  conflicted::conflict_prefer("View", "utils", quiet = TRUE)
}

# Suppress summarize messages
options(dplyr.summarise.inform = FALSE)
